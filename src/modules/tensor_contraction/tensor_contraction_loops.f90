module tensor_contraction_loops_module
    use, intrinsic :: iso_fortran_env, only : &
            int64, &
            real32, &
            real64

    use :: data_api, only : stream

    use :: tensor_api, only : &
            dt_r32, &
            dt_r64, &
            dt_c64, &
            dt_c128, &
            tensor, &
            scalar, &
            tensor_fortran_converter

    use :: tc_index_helper_module, only : &
            tc_index_helper, &
            max_number_of_indices
    use :: tc_descriptor_module, only : tc_descriptor
    use :: tensor_contraction_module, only : tensor_contraction

    implicit none
    private

    public :: tensor_contraction_loops

    integer, parameter :: max_order = 4

    type, extends(tensor_contraction) :: tensor_contraction_loops
        type(tensor_fortran_converter) :: converter
        type(tc_descriptor) :: tcd
    contains
        procedure :: contract => contract
        procedure :: synchronize => synchronize
        procedure :: cleanup => cleanup

        procedure :: contract_real32 => contract_real32
        procedure :: contract_real64 => contract_real64
        procedure :: contract_complex64 => contract_complex64
        procedure :: contract_complex128 => contract_complex128
        procedure :: get_helper => get_helper
        procedure :: set_descriptor => set_descriptor
        procedure :: is_valid => is_valid
        procedure :: clear => clear
    end type tensor_contraction_loops

    interface tensor_contraction_loops
        module procedure constructor_empty
        module procedure constructor_arrays
        module procedure constructor_tc_descriptor
    end interface tensor_contraction_loops
contains
    function constructor_empty() result(this)
        type(tensor_contraction_loops) :: this

        call this%clear()
        this%converter = tensor_fortran_converter()
    end function constructor_empty

    function constructor_arrays(c_indices, a_indices, b_indices) result(this)
        integer, dimension(:), intent(in) :: c_indices, a_indices, b_indices
        type(tensor_contraction_loops) :: this

        this = tensor_contraction_loops()

        call this%set_descriptor(tc_descriptor(c_indices, a_indices, b_indices))
    end function constructor_arrays

    function constructor_tc_descriptor(tcd_in) result(this)
        type(tc_descriptor), intent(in) :: tcd_in
        type(tensor_contraction_loops) :: this

        this = tensor_contraction_loops()
        call this%set_descriptor(tcd_in)
    end function constructor_tc_descriptor

    subroutine set_descriptor(this, descr)
        class(tensor_contraction_loops), intent(inout) :: this
        type(tc_descriptor), intent(in) :: descr

        if ( .not. this%is_valid(descr%c_indices, descr%a_indices, descr%b_indices) ) &
                error stop "tensor_contraction_loops::set_descriptor:Not valid"

        this%tcd = descr
    end subroutine set_descriptor

    logical function is_valid(this, c_indices, a_indices, b_indices)
        class(tensor_contraction_loops), intent(in) :: this
        integer, dimension(:), intent(in) :: c_indices, a_indices, b_indices

        integer :: number_of_indices

        number_of_indices = maxval([maxval(a_indices), maxval(b_indices), maxval(c_indices)])

        is_valid = number_of_indices <= max_number_of_indices .and. &
                size(c_indices) <= max_order .and. &
                size(a_indices) <= max_order .and. &
                size(b_indices) <= max_order
    end function is_valid

    type(tc_index_helper) function get_helper(this, a, b)
        class(tensor_contraction_loops), intent(in) :: this
        class(tensor), intent(in) :: a, b

        integer(int64), dimension(max_number_of_indices) :: sizes
        integer :: idx

        do idx = 1, a%get_rank()
            sizes(this%tcd%a_indices(idx)) = a%dims(idx)
        end do

        do idx = 1, b%get_rank()
            sizes(this%tcd%b_indices(idx)) = b%dims(idx)
        end do

        get_helper = tc_index_helper(this%tcd, sizes)
    end function get_helper

    subroutine contract(this, c, a, b, alpha, beta, astream)
        class(tensor_contraction_loops), intent(inout) :: this
        class(tensor), intent(inout) :: c
        class(tensor), intent(in) :: a, b
        type(scalar), intent(in), optional :: alpha, beta
        type(stream), intent(in), optional :: astream

        if ( c%datatype == dt_r32 ) then
            call this%contract_real32(this%get_helper(a, b), c, a, b, alpha, beta, astream)
        else if ( c%datatype == dt_r64 ) then
            call this%contract_real64(this%get_helper(a, b), c, a, b, alpha, beta, astream)
        else if ( c%datatype == dt_c64 ) then
            call this%contract_complex64(this%get_helper(a, b), c, a, b, alpha, beta, astream)
        else if ( c%datatype == dt_c128 ) then
            call this%contract_complex128(this%get_helper(a, b), c, a, b, alpha, beta, astream)
        else
            error stop "tensor_contraction_loops::contract:Datatype not supported."
        end if
    end subroutine contract

    subroutine contract_real32(this, helper, c, a, b, alpha, beta, astream)
        class(tensor_contraction_loops), intent(inout) :: this
        type(tc_index_helper), intent(in) :: helper
        class(tensor), intent(inout) :: c
        class(tensor), intent(in) :: a, b
        type(scalar), intent(in), optional :: alpha, beta
        type(stream), intent(in), optional :: astream

        real(real32), dimension(:), pointer, contiguous :: cvec, avec, bvec
        real(real32) :: alpha_val, beta_val
        integer(int64) :: idx
        integer(int64), dimension(3) :: indexes

        alpha_val = 1.0
        if ( present(alpha) ) alpha_val = alpha%as_real32()
        beta_val = 1.0
        if ( present(beta) ) beta_val = beta%as_real32()

        call this%converter%secure_fortran_pointer(cvec, c, astream)
        call this%converter%secure_fortran_pointer(avec, a, astream)
        call this%converter%secure_fortran_pointer(bvec, b, astream)

        if ( beta_val == 0.0 ) then
            do idx = 1, size(cvec)
                cvec(idx) = 0.0
            end do
        else if ( beta_val /= 1.0 ) then
            do idx = 1, size(cvec)
                cvec(idx) = beta_val*cvec(idx)
            end do
        end if

        do idx = 1, helper%number_of_products
            call helper%calculate_abc_indexes(indexes, idx)
            cvec(indexes(1)) = cvec(indexes(1)) + alpha_val*avec(indexes(2))*bvec(indexes(3))
        end do

        call this%converter%update_remote_and_release_pointer(cvec, c, astream)
        call this%converter%release_pointer(avec, a, astream)
        call this%converter%release_pointer(bvec, b, astream)
    end subroutine contract_real32

    subroutine contract_real64(this, helper, c, a, b, alpha, beta, astream)
        class(tensor_contraction_loops), intent(inout) :: this
        type(tc_index_helper), intent(in) :: helper
        class(tensor), intent(inout) :: c
        class(tensor), intent(in) :: a, b
        type(scalar), intent(in), optional :: alpha, beta
        type(stream), intent(in), optional :: astream

        real(real64), dimension(:), pointer, contiguous :: cvec, avec, bvec
        real(real64) :: alpha_val, beta_val
        integer(int64) :: idx
        integer(int64), dimension(3) :: indexes

        alpha_val = 1.0d0
        if ( present(alpha) ) alpha_val = alpha%as_real64()
        beta_val = 1.0d0
        if ( present(beta) ) beta_val = beta%as_real64()

        call this%converter%secure_fortran_pointer(cvec, c, astream)
        call this%converter%secure_fortran_pointer(avec, a, astream)
        call this%converter%secure_fortran_pointer(bvec, b, astream)

        if ( beta_val == 0.0d0 ) then
            do idx = 1, size(cvec)
                cvec(idx) = 0.0d0
            end do
        else if ( beta_val /= 1.0d0 ) then
            do idx = 1, size(cvec)
                cvec(idx) = beta_val*cvec(idx)
            end do
        end if

        do idx = 1, helper%number_of_products
            call helper%calculate_abc_indexes(indexes, idx)
            cvec(indexes(1)) = cvec(indexes(1)) + alpha_val*avec(indexes(2))*bvec(indexes(3))
        end do

        call this%converter%update_remote_and_release_pointer(cvec, c, astream)
        call this%converter%release_pointer(avec, a, astream)
        call this%converter%release_pointer(bvec, b, astream)
    end subroutine contract_real64

    subroutine contract_complex64(this, helper, c, a, b, alpha, beta, astream)
        class(tensor_contraction_loops), intent(inout) :: this
        type(tc_index_helper), intent(in) :: helper
        class(tensor), intent(inout) :: c
        class(tensor), intent(in) :: a, b
        type(scalar), intent(in), optional :: alpha, beta
        type(stream), intent(in), optional :: astream

        complex(real32), dimension(:), pointer, contiguous :: cvec, avec, bvec
        complex(real32) :: alpha_val, beta_val
        integer(int64) :: idx
        integer(int64), dimension(3) :: indexes

        alpha_val = (1.0, 0.0)
        if ( present(alpha) ) alpha_val = alpha%as_complex64()
        beta_val = (1.0, 0.0)
        if ( present(beta) ) beta_val = beta%as_complex64()

        call this%converter%secure_fortran_pointer(cvec, c, astream)
        call this%converter%secure_fortran_pointer(avec, a, astream)
        call this%converter%secure_fortran_pointer(bvec, b, astream)

        if ( beta_val == (0.0, 0.0) ) then
            do idx = 1, size(cvec)
                cvec(idx) = (0.0, 0.0)
            end do
        else if ( beta_val /= (1.0, 0.0) ) then
            do idx = 1, size(cvec)
                cvec(idx) = beta_val*cvec(idx)
            end do
        end if

        do idx = 1, helper%number_of_products
            call helper%calculate_abc_indexes(indexes, idx)
            cvec(indexes(1)) = cvec(indexes(1)) + alpha_val*avec(indexes(2))*bvec(indexes(3))
        end do

        call this%converter%update_remote_and_release_pointer(cvec, c, astream)
        call this%converter%release_pointer(avec, a, astream)
        call this%converter%release_pointer(bvec, b, astream)
    end subroutine contract_complex64

    subroutine contract_complex128(this, helper, c, a, b, alpha, beta, astream)
        class(tensor_contraction_loops), intent(inout) :: this
        type(tc_index_helper), intent(in) :: helper
        class(tensor), intent(inout) :: c
        class(tensor), intent(in) :: a, b
        type(scalar), intent(in), optional :: alpha, beta
        type(stream), intent(in), optional :: astream

        complex(real64), dimension(:), pointer, contiguous :: cvec, avec, bvec
        complex(real64) :: alpha_val, beta_val
        integer(int64) :: idx
        integer(int64), dimension(3) :: indexes

        alpha_val = (1.0d0,0.0d0)
        if ( present(alpha) ) alpha_val = alpha%as_complex64()
        beta_val = (1.0d0,0.0d0)
        if ( present(beta) ) beta_val = beta%as_complex64()

        call this%converter%secure_fortran_pointer(cvec, c, astream)
        call this%converter%secure_fortran_pointer(avec, a, astream)
        call this%converter%secure_fortran_pointer(bvec, b, astream)

        if ( beta_val == (0.0d0, 0.0d0) ) then
            do idx = 1, size(cvec)
                cvec(idx) = (0.0d0, 0.0d0)
            end do
        else if ( beta_val /= (1.0d0, 0.0d0) ) then
            do idx = 1, size(cvec)
                cvec(idx) = beta_val*cvec(idx)
            end do
        end if

        do idx = 1, helper%number_of_products
            call helper%calculate_abc_indexes(indexes, idx)
            cvec(indexes(1)) = cvec(indexes(1)) + alpha_val*avec(indexes(2))*bvec(indexes(3))
        end do

        call this%converter%update_remote_and_release_pointer(cvec, c, astream)
        call this%converter%release_pointer(avec, a, astream)
        call this%converter%release_pointer(bvec, b, astream)
    end subroutine contract_complex128

    subroutine synchronize(this, astream)
        class(tensor_contraction_loops), intent(in) :: this
        type(stream), intent(in), optional :: astream

        continue ! Nothing to do for host
    end subroutine synchronize

    subroutine cleanup(this)
        class(tensor_contraction_loops), intent(inout) :: this

        call this%converter%cleanup()
        call this%tcd%cleanup()
        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(tensor_contraction_loops), intent(inout) :: this

        this%tcd = tc_descriptor()
    end subroutine clear
end module tensor_contraction_loops_module
