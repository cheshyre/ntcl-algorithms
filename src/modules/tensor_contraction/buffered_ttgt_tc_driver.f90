module buffered_ttgt_tc_driver_module
    use, intrinsic :: iso_fortran_env, only : int64
    use :: data_api, only : &
            stream, &
            scratch_buffer

    use :: tensor_api, only : &
            tensor, &
            scalar

    use :: tc_ttgt_module, only : tc_ttgt
    use :: ttgt_descriptor_module, only : ttgt_descriptor
    use :: tensor_permute_module, only : tensor_permute
    use :: matrix_multiplication_module, only : matrix_multiplication

    implicit none
    private

    public :: buffered_ttgt_tc_driver

    type, extends(tc_ttgt) :: buffered_ttgt_tc_driver
        type(scratch_buffer) :: scratch
    contains
        procedure :: synchronize => synchronize
        procedure :: cleanup => cleanup
        procedure :: setup => setup
        procedure :: set_scratch_buffer => set_scratch_buffer
        procedure :: allocate_tensor => allocate_tensor
        procedure :: destroy_permuted_tensor => destroy_permuted_tensor
        procedure :: destroy_permuted_tensors => destroy_permuted_tensors
        procedure :: clear => clear
    end type buffered_ttgt_tc_driver

    interface buffered_ttgt_tc_driver
        module procedure constructor_empty
        module procedure constructor
    end interface buffered_ttgt_tc_driver
contains
    function constructor_empty() result(this)
        type(buffered_ttgt_tc_driver) :: this

        call this%clear()
    end function constructor_empty

    function constructor(descr, permuter, mm, scratch) result(this)
        type(ttgt_descriptor), intent(in) :: descr
        class(tensor_permute), intent(in) :: permuter
        class(matrix_multiplication), intent(in) :: mm
        type(scratch_buffer), intent(in) :: scratch
        type(buffered_ttgt_tc_driver) :: this

        this = buffered_ttgt_tc_driver()

        call this%setup(descr, permuter, mm, scratch)
    end function constructor

    subroutine destroy_permuted_tensors(this, c, a, b, astream)
        class(buffered_ttgt_tc_driver), intent(inout) :: this
        class(tensor), intent(inout) :: c, a, b
        type(stream), intent(in), optional :: astream

        call this%destroy_permuted_tensor(b, this%descr%permute_b)
        call this%destroy_permuted_tensor(a, this%descr%permute_a)
        call this%destroy_permuted_tensor(c, this%descr%permute_c)
        call this%scratch%checkpoint(astream)
    end subroutine destroy_permuted_tensors

    subroutine synchronize(this, astream)
        class(buffered_ttgt_tc_driver), intent(in) :: this
        type(stream), intent(in), optional :: astream

        call this%permuter%synchronize(astream)
    end subroutine synchronize

    subroutine cleanup(this)
        class(buffered_ttgt_tc_driver), intent(inout) :: this

        if (allocated(this%permuter)) then
            call this%permuter%cleanup()
            deallocate(this%permuter)
        end if

        if (allocated(this%mm)) then
            call this%mm%cleanup()
            deallocate(this%mm)
        end if

        call this%builder%cleanup()
        call this%scratch%cleanup()

        call this%clear()
    end subroutine cleanup

    subroutine setup(this, descr, permuter, mm, scratch)
        class(buffered_ttgt_tc_driver), intent(inout) :: this
        type(ttgt_descriptor), intent(in) :: descr
        class(tensor_permute), intent(in) :: permuter
        class(matrix_multiplication), intent(in) :: mm
        type(scratch_buffer), intent(in) :: scratch

        call this%set_descriptor(descr)
        call this%set_permuter(permuter)
        call this%set_matrix_multiplication(mm)
        call this%set_scratch_buffer(scratch)
    end subroutine setup

    subroutine set_scratch_buffer(this, scratch)
        class(buffered_ttgt_tc_driver), intent(inout) :: this
        type(scratch_buffer), intent(in) ::scratch

        this%scratch = scratch
        call this%scratch%initialize()
    end subroutine set_scratch_buffer

    subroutine allocate_tensor(this, src, dst, order, initialize)
        class(buffered_ttgt_tc_driver), intent(inout) :: this
        class(tensor), intent(in) :: src
        class(tensor), allocatable, intent(inout) :: dst
        integer, dimension(:), intent(in) :: order
        logical, intent(in) :: initialize

        call this%builder%allocate_and_create_in_scratch(dst, this%scratch, src%datatype, src%dims(order), initialize)
    end subroutine allocate_tensor

    subroutine destroy_permuted_tensor(this, dst, permute)
        class(buffered_ttgt_tc_driver), intent(inout) :: this
        class(tensor), intent(inout) :: dst
        logical, intent(in) :: permute

        if ( permute ) then
            call this%scratch%destroy(dst%storage)
        end if
        call dst%release()
    end subroutine destroy_permuted_tensor

    subroutine clear(this)
        class(buffered_ttgt_tc_driver), intent(inout) :: this

        this%descr = ttgt_descriptor()
    end subroutine clear
end module buffered_ttgt_tc_driver_module
