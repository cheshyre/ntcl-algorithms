module unbuffered_ttgt_tc_driver_module
    use, intrinsic :: iso_fortran_env, only : int64

    use :: data_api, only : stream

    use :: tensor_api, only : &
            tensor, &
            scalar

    use :: tensor_permute_module, only : tensor_permute
    use :: matrix_multiplication_module, only : matrix_multiplication
    use :: tc_ttgt_module, only : tc_ttgt
    use :: ttgt_descriptor_module, only : ttgt_descriptor

    implicit none
    private

    public :: unbuffered_ttgt_tc_driver

    type, extends(tc_ttgt) :: unbuffered_ttgt_tc_driver
    contains
        procedure :: synchronize => synchronize
        procedure :: cleanup => cleanup
        procedure :: allocate_tensor => allocate_tensor
        procedure :: destroy_permuted_tensors => destroy_permuted_tensors
        procedure :: destroy_permuted_tensor => destroy_permuted_tensor
        procedure :: setup => setup
        procedure :: clear => clear
    end type unbuffered_ttgt_tc_driver

    interface unbuffered_ttgt_tc_driver
        module procedure constructor_empty
        module procedure constructor
    end interface unbuffered_ttgt_tc_driver
contains
    function constructor_empty() result(this)
        type(unbuffered_ttgt_tc_driver) :: this

        call this%clear()
    end function constructor_empty

    function constructor(descr, permuter, mm) result(this)
        type(ttgt_descriptor), intent(in) :: descr
        class(tensor_permute), intent(in) :: permuter
        class(matrix_multiplication), intent(in) :: mm
        type(unbuffered_ttgt_tc_driver) :: this

        this = unbuffered_ttgt_tc_driver()

        call this%setup(descr, permuter, mm)
    end function constructor

    subroutine synchronize(this, astream)
        class(unbuffered_ttgt_tc_driver), intent(in) :: this
        type(stream), intent(in), optional :: astream

        call this%permuter%synchronize(astream)
    end subroutine synchronize

    subroutine cleanup(this)
        class(unbuffered_ttgt_tc_driver), intent(inout) :: this

        if (allocated(this%permuter)) then
            call this%permuter%cleanup()
            deallocate(this%permuter)
        end if

        if (allocated(this%mm)) then
            call this%mm%cleanup()
            deallocate(this%mm)
        end if

        call this%builder%cleanup()
        call this%clear()
    end subroutine cleanup

    subroutine setup(this, descr, permuter, mm)
        class(unbuffered_ttgt_tc_driver), intent(inout) :: this
        type(ttgt_descriptor), intent(in) :: descr
        class(tensor_permute), intent(in) :: permuter
        class(matrix_multiplication), intent(in) :: mm

        call this%set_descriptor(descr)
        call this%set_permuter(permuter)
        call this%set_matrix_multiplication(mm)
    end subroutine setup

    subroutine allocate_tensor(this, src, dst, order, initialize)
        class(unbuffered_ttgt_tc_driver), intent(inout) :: this
        class(tensor), intent(in) :: src
        class(tensor), allocatable, intent(inout) :: dst
        integer, dimension(:), intent(in) :: order
        logical, intent(in) :: initialize

        call this%builder%allocate_and_create(dst, src%datatype, src%dims(order), initialize)
    end subroutine allocate_tensor

    subroutine destroy_permuted_tensors(this, c, a, b, astream)
        class(unbuffered_ttgt_tc_driver), intent(inout) :: this
        class(tensor), intent(inout) :: c, a, b
        type(stream), intent(in), optional :: astream

        call this%destroy_permuted_tensor(b, this%descr%permute_b)
        call this%destroy_permuted_tensor(a, this%descr%permute_a)
        call this%destroy_permuted_tensor(c, this%descr%permute_c)
    end subroutine destroy_permuted_tensors

    subroutine destroy_permuted_tensor(this, dst, permute)
        class(unbuffered_ttgt_tc_driver), intent(inout) :: this
        class(tensor), intent(inout) :: dst
        logical, intent(in) :: permute

        if ( permute ) then
            call dst%cleanup()
        else
            call dst%release()
        end if
    end subroutine destroy_permuted_tensor

    subroutine clear(this)
        class(unbuffered_ttgt_tc_driver), intent(inout) :: this

        this%descr = ttgt_descriptor()
    end subroutine clear
end module unbuffered_ttgt_tc_driver_module
