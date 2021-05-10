module pointer_data_bmm_driver_module
    use, intrinsic :: iso_fortran_env, only : &
            real64, &
            int64, &
            int32

    use, intrinsic :: iso_c_binding, only : &
            c_int, &
            c_float, &
            c_double, &
            c_ptr

    use :: data_api, only : stream

    use :: tensor_api, only : &
            scalar, &
            vector, &
            matrix, &
            dt_c128, &
            dt_c64, &
            dt_r32, &
            dt_r64, &
            tensor_builder, &
            tensor_c_pointer_converter

    use :: batched_matrix_multiplication_module, only : &
            batched_matrix_multiplication

    implicit none
    private

    public :: pointer_data_bmm_driver

    type, abstract, extends(batched_matrix_multiplication) :: pointer_data_bmm_driver
        type(tensor_c_pointer_converter) :: converter
        type(tensor_builder) :: builder
    contains
        procedure :: bmm_array => bmm_array
        procedure :: bmm_single => bmm_single
        procedure :: is_compatible => is_compatible
        procedure :: is_valid => is_valid
        procedure :: set_and_check_dimensions => set_and_check_dimensions
        procedure :: within_blas_bounds => within_blas_bounds
        procedure :: get_padding_for_sizes => get_padding_for_sizes
        procedure(bmm_r32_single_interface), deferred :: bmm_single_real32
        procedure(bmm_r64_single_interface), deferred :: bmm_single_real64
        procedure(bmm_c64_single_interface), deferred :: bmm_single_complex64
        procedure(bmm_c128_single_interface), deferred :: bmm_single_complex128
    end type pointer_data_bmm_driver

    abstract interface
        subroutine bmm_r32_single_interface(this, batch_count, dst, left, right, m, n, k, transa, transb, alpha, beta, astream)
            import :: pointer_data_bmm_driver
            import :: c_ptr
            import :: c_float
            import :: scalar
            import :: stream

            class(pointer_data_bmm_driver), intent(in) :: this
            integer, intent(in) :: batch_count
            type(c_ptr), intent(inout) :: dst
            type(c_ptr), intent(in) :: left, right, m, n, k
            logical, intent(in) :: transa, transb
            real(c_float), intent(in)  :: alpha, beta
            type(stream), intent(in), optional :: astream
        end subroutine bmm_r32_single_interface

        subroutine bmm_r64_single_interface(this, batch_count, dst, left, right, m, n, k, transa, transb, alpha, beta, astream)
            import :: pointer_data_bmm_driver
            import :: c_ptr
            import :: c_double
            import :: scalar
            import :: stream

            class(pointer_data_bmm_driver), intent(in) :: this
            integer, intent(in) :: batch_count
            type(c_ptr), intent(inout) :: dst
            type(c_ptr), intent(in) :: left, right, m, n, k
            logical, intent(in) :: transa, transb
            real(c_double), intent(in)  :: alpha, beta
            type(stream), intent(in), optional :: astream
        end subroutine bmm_r64_single_interface

        subroutine bmm_c64_single_interface(this, batch_count, dst, left, right, m, n, k, transa, transb, alpha, beta, astream)
            import :: pointer_data_bmm_driver
            import :: c_ptr
            import :: c_float
            import :: scalar
            import :: stream

            class(pointer_data_bmm_driver), intent(in) :: this
            integer, intent(in) :: batch_count
            type(c_ptr), intent(inout) :: dst
            type(c_ptr), intent(in) :: left, right, m, n, k
            logical, intent(in) :: transa, transb
            complex(c_float), intent(in)  :: alpha, beta
            type(stream), intent(in), optional :: astream
        end subroutine bmm_c64_single_interface

        subroutine bmm_c128_single_interface(this, batch_count, dst, left, right, m, n, k, transa, transb, alpha, beta, astream)
            import :: pointer_data_bmm_driver
            import :: c_ptr
            import :: c_double
            import :: scalar
            import :: stream

            class(pointer_data_bmm_driver), intent(in) :: this
            integer, intent(in) :: batch_count
            type(c_ptr), intent(inout) :: dst
            type(c_ptr), intent(in) :: left, right, m, n, k
            logical, intent(in) :: transa, transb
            complex(c_double), intent(in)  :: alpha, beta
            type(stream), intent(in), optional :: astream
        end subroutine bmm_c128_single_interface
    end interface
contains
    subroutine bmm_array(this, dst, left, right, alpha, beta, astream)
        class(pointer_data_bmm_driver), intent(inout) :: this
        type(matrix), dimension(:), intent(inout) :: dst
        type(matrix), dimension(:), intent(in) :: left, right
        type(scalar), dimension(:), intent(in), optional :: alpha, beta
        type(stream), intent(in), optional :: astream

        error stop "pointer_data_bmm_driver::bmm_array:Not implemented."
    end subroutine bmm_array

    subroutine bmm_single(this, dst, left, right, alpha, beta, astream)
        class(pointer_data_bmm_driver), intent(inout) :: this
        type(matrix), dimension(:), intent(inout) :: dst
        type(matrix), dimension(:), intent(in) :: left, right
        type(scalar), intent(in) :: alpha, beta
        type(stream), intent(in), optional :: astream

        type(vector) :: m, n, k
        type(c_ptr) :: d, l, r, mptr, nptr, kptr
        logical :: transa, transb
        integer :: batch_count

        if ( .not. this%is_valid(dst, left, right) ) &
                error stop "pointer_data_bmm_driver::Not a valid set of matrices."

        if ( .not. this%is_compatible(dst, left, right) ) &
                error stop "pointer_data_bmm_driver::bmm:Matrices are not compatible."

        call this%set_and_check_dimensions(dst, left, m, n, k)

        call this%converter%secure_pointer_from_array(dst, d, astream)
        call this%converter%secure_pointer_from_array(left, l, astream)
        call this%converter%secure_pointer_from_array(right, r, astream)
        call this%converter%secure_pointer(m, mptr, astream)
        call this%converter%secure_pointer(n, nptr, astream)
        call this%converter%secure_pointer(k, kptr, astream)

        transa = left(1)%is_transposed
        transb = right(1)%is_transposed
        batch_count = size(dst)

        select case ( dst(1)%get_datatype() )
        case (dt_r32)
            call this%bmm_single_real32(batch_count, d, l, r, mptr, nptr, kptr, transa, transb, &
                    alpha%as_real32(), beta%as_real32(), astream)
        case (dt_r64)
            call this%bmm_single_real64(batch_count, d, l, r, mptr, nptr, kptr, transa, transb, &
                    alpha%as_real64(), beta%as_real64(), astream)
        case (dt_c64)
            call this%bmm_single_complex64(batch_count, d, l, r, mptr, nptr, kptr, transa, transb, &
                    alpha%as_complex64(), beta%as_complex64(), astream)
        case (dt_c128)
            call this%bmm_single_complex128(batch_count, d, l, r, mptr, nptr, kptr, transa, transb, &
                    alpha%as_complex128(), beta%as_complex128(), astream)
        case default
            error stop "pointer_data_bmm_driver::bmm:Not a supported datatype."
        end select

        call this%converter%update_array_and_release(dst, d, astream)
        call this%converter%release_from_array(left, l, astream)
        call this%converter%release_from_array(right, r, astream)
        call this%converter%release(m, mptr, astream)
        call this%converter%release(n, nptr, astream)
        call this%converter%release(k, kptr, astream)

        call m%cleanup(); call n%cleanup(); call k%cleanup()
    end subroutine bmm_single

    integer function get_padding_for_sizes(this)
        class(pointer_data_bmm_driver), intent(in) :: this

        get_padding_for_sizes = 0
    end function get_padding_for_sizes

    logical function is_compatible(this, dst, left, right)
        class(pointer_data_bmm_driver), intent(in) :: this
        type(matrix), dimension(:), intent(in) :: dst, left, right

        logical, dimension(:), allocatable :: compat
        integer :: i, number_of_matrices

        number_of_matrices = size(dst)

        allocate(compat(number_of_matrices))

        do i = 1,number_of_matrices
            if ( left(i)%is_transposed .and. right(i)%is_transposed ) then
                compat(i) = dst(i)%dims(1) == left(i)%dims(2) .and. &
                        dst(i)%dims(2) == right(i)%dims(1) .and. &
                        left(i)%dims(1) == right(i)%dims(2)
            else if ( left(i)%is_transposed ) then
                compat(i) = dst(i)%dims(1) == left(i)%dims(2) .and. &
                        dst(i)%dims(2) == right(i)%dims(2) .and. &
                        left(i)%dims(1) == right(i)%dims(1)
            else if ( right(i)%is_transposed ) then
                compat = dst(i)%dims(1) == left(i)%dims(1) .and. &
                        dst(i)%dims(2) == right(i)%dims(1) .and. &
                        left(i)%dims(2) == right(i)%dims(2)
            else
                compat = dst(i)%dims(1) == left(i)%dims(1) .and. &
                        dst(i)%dims(2) == right(i)%dims(2) .and. &
                        left(i)%dims(2) == right(i)%dims(1)
            end if
        end do
        is_compatible = all(compat)
        deallocate(compat)
    end function is_compatible

    subroutine set_and_check_dimensions(this, dst, left, mv, nv, kv)
        class(pointer_data_bmm_driver), intent(in) :: this
        type(matrix), dimension(:), intent(in) :: dst, left
        type(vector), intent(inout) :: mv, nv, kv

        integer(int64), dimension(:), allocatable :: m, n, k

        integer :: i, number_of_matrices, vector_size

        number_of_matrices = size(dst)
        vector_size = number_of_matrices + this%get_padding_for_sizes()

        allocate(m(vector_size))
        allocate(n(vector_size))
        allocate(k(vector_size))

        m = 0; n = 0; k = 0
        do i = 1, number_of_matrices
            m(i) = dst(i)%dims(1)
            n(i) = dst(i)%dims(2)
            if ( left(i)%is_transposed ) then
                k(i) = left(i)%dims(1)
            else
                k(i) = left(i)%dims(2)
            end if
        end do

        if ( .not. this%within_blas_bounds(m, n, k) ) &
            error stop "pointer_data_bmm_driver::set_and_check_dimensions:"// &
                    "Dimensions outside bmm implementation bounds."

        call this%builder%copy(mv, int(m))
        call this%builder%copy(nv, int(n))
        call this%builder%copy(kv, int(k))
    end subroutine set_and_check_dimensions

    logical function within_blas_bounds(this, m, n, k)
        class(pointer_data_bmm_driver), intent(in) :: this
        integer(int64), intent(in), dimension(:) :: m, n, k

        integer :: idx

        within_blas_bounds = &
                all(m <= huge(int(1, int32))) .and. &
                all(n <= huge(int(1, int32))) .and. &
                all(k <= huge(int(1, int32)))
    end function within_blas_bounds

    logical function is_valid(this, dst, left, right)
        class(pointer_data_bmm_driver), intent(in) :: this
        type(matrix), dimension(:), intent(in) :: dst, left, right

        integer :: batch_count, i

        is_valid = .false.

        batch_count = size(dst)
        if ( .not. (batch_count == size(left) .and. batch_count == size(right)) ) return

        is_valid = .true.
        do i = 1, batch_count
            is_valid = is_valid .and. &
                    (left(i)%is_transposed .eqv. left(1)%is_transposed) .and. &
                    (right(i)%is_transposed .eqv. right(1)%is_transposed)
        end do
    end function is_valid
end module pointer_data_bmm_driver_module
