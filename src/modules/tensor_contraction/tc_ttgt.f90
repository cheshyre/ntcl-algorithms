module tc_ttgt_module
    use, intrinsic :: iso_fortran_env, only : int64

    use :: data_api, only : stream

    use :: tensor_api, only : &
            tensor, &
            matrix, &
            scalar, &
            reshape_tensor, &
            tensor_builder

    use :: tensor_permute_module, only : tensor_permute
    use :: matrix_multiplication_module, only : matrix_multiplication

    use :: tensor_contraction_module, only : tensor_contraction
    use :: ttgt_descriptor_module, only : ttgt_descriptor

    implicit none
    private

    public :: tc_ttgt

    type, abstract, extends(tensor_contraction) :: tc_ttgt
        type(ttgt_descriptor) :: descr
        class(tensor_permute), allocatable :: permuter
        class(matrix_multiplication), allocatable :: mm
        type(tensor_builder) :: builder
    contains
        procedure :: contract => contract
        procedure :: set_permuter => set_permuter
        procedure :: set_matrix_multiplication => set_matrix_multiplication
        procedure :: set_descriptor => set_descriptor
        procedure :: create_permuted_tensors => create_permuted_tensors
        procedure :: create_permuted_tensor => create_permuted_tensor
        procedure(allocate_interface), deferred :: allocate_tensor
        procedure(destroy_tensors), deferred :: destroy_permuted_tensors
    end type tc_ttgt

    abstract interface
        subroutine allocate_interface(this, src, dst, order, initialize)
            import :: tc_ttgt
            import :: tensor

            class(tc_ttgt), intent(inout) :: this
            class(tensor), intent(in) :: src
            class(tensor), allocatable, intent(inout) :: dst
            integer, dimension(:), intent(in) :: order
            logical, intent(in) :: initialize
        end subroutine allocate_interface

        subroutine destroy_tensors(this, c, a, b, astream)
            import :: tc_ttgt
            import :: tensor
            import :: stream

            class(tc_ttgt), intent(inout) :: this
            class(tensor), intent(inout) :: c, a, b
            type(stream), intent(in), optional :: astream
        end subroutine destroy_tensors
    end interface
contains
    subroutine contract(this, c, a, b, alpha, beta, astream)
        class(tc_ttgt), intent(inout) :: this
        class(tensor), intent(inout) :: c
        class(tensor), intent(in) :: a, b
        type(scalar), intent(in), optional :: alpha, beta
        type(stream), intent(in), optional :: astream

        class(tensor), allocatable :: ct, at, bt
        type(matrix) :: cm, am, bm
        integer(int64), dimension(3) :: mdims

        am = matrix(); bm = matrix(); cm = matrix()

        call this%create_permuted_tensors(c, a, b, ct, at, bt, beta, astream)

        mdims = this%descr%get_matrix_dimensions(a, b)
        call reshape_tensor(cm, ct, [mdims(1), mdims(2)])
        if ( this%descr%switch_ab) then
            call reshape_tensor(am, bt, [mdims(1), mdims(3)])
            call reshape_tensor(bm, at, [mdims(3), mdims(2)])
        else
            call reshape_tensor(am, at, [mdims(1), mdims(3)])
            call reshape_tensor(bm, bt, [mdims(3), mdims(2)])
        end if

        call this%mm%mm(cm, am, bm, alpha, beta, astream)
        call cm%release(); call am%release(); call bm%release()

        if ( this%descr%permute_c ) &
            call this%permuter%permute(ct, c, this%descr%cinverseorder, astream)

        call this%destroy_permuted_tensors(ct, at, bt, astream)
    end subroutine contract

    subroutine create_permuted_tensors(this, c, a, b, ct, at, bt, beta, astream)
        class(tc_ttgt), intent(inout) :: this
        class(tensor), intent(in) :: c, a, b
        class(tensor), allocatable, intent(inout) :: ct, at, bt
        type(scalar), intent(in), optional :: beta
        type(stream), intent(in), optional :: astream

        call this%create_permuted_tensor(c, ct, this%descr%corder, present(beta), this%descr%permute_c, astream=astream)
        call this%create_permuted_tensor(a, at, this%descr%aorder, .true., this%descr%permute_a, astream=astream)
        call this%create_permuted_tensor(b, bt, this%descr%border, .true., this%descr%permute_b, astream=astream)
    end subroutine create_permuted_tensors

    subroutine set_permuter(this, permuter)
        class(tc_ttgt), intent(inout) :: this
        class(tensor_permute), intent(in) :: permuter

        this%permuter = permuter
    end subroutine set_permuter

    subroutine set_matrix_multiplication(this, mm)
        class(tc_ttgt), intent(inout) :: this
        class(matrix_multiplication), intent(in) :: mm

        this%mm = mm
    end subroutine set_matrix_multiplication

    subroutine set_descriptor(this, descr)
        class(tc_ttgt), intent(inout) :: this
        type(ttgt_descriptor), intent(in) :: descr

        this%descr = descr
    end subroutine set_descriptor

    subroutine create_permuted_tensor(this, src, dst, order, copy, permute, astream)
        class(tc_ttgt), intent(inout) :: this
        class(tensor), intent(in) :: src
        class(tensor), allocatable, intent(inout) :: dst
        integer, dimension(:), intent(in) :: order
        logical, intent(in) :: copy, permute
        type(stream), intent(in), optional :: astream

        if ( permute ) then
            call this%allocate_tensor(src, dst, order, .not. copy)

            if (copy) &
                call this%permuter%permute(src, dst, order, astream)
        else
            dst = src
        end if
    end subroutine create_permuted_tensor
end module tc_ttgt_module
