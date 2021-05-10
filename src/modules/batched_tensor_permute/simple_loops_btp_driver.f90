module simple_loops_btp_driver_module
    use :: data_api, only : stream

    use :: tensor_api, only : &
            tensor_array_element

    use :: batched_tensor_permute_module, only : &
            batched_tensor_permute

    use :: tensor_permute_module, only : tensor_permute

    implicit none
    private

    public :: simple_loops_btp_driver

    type, extends(batched_tensor_permute) :: simple_loops_btp_driver
        class(tensor_permute), allocatable :: driver
    contains
        procedure :: btp => btp
        procedure :: synchronize => synchronize
        procedure :: cleanup => cleanup
    end type simple_loops_btp_driver

    interface simple_loops_btp_driver
        module procedure constructor_empty
        module procedure constructor
    end interface simple_loops_btp_driver

contains
    function constructor_empty() result(this)
        type(simple_loops_btp_driver) :: this

    end function constructor_empty

    function constructor(driver) result(this)
        class(tensor_permute), intent(in) :: driver
        type(simple_loops_btp_driver) :: this

        this%driver = driver
    end function constructor

    subroutine btp(this, src, dst, perm, astream)
        class(simple_loops_btp_driver), target, intent(inout) :: this
        class(tensor_array_element), dimension(:), intent(in) :: src
        class(tensor_array_element), dimension(:), intent(inout) :: dst
        integer, intent(in) :: perm(:)
        type(stream), intent(in), optional :: astream

        integer :: idx

        do idx = 1, size(dst)
            call this%driver%permute(src(idx)%element, dst(idx)%element, perm, astream)
        end do
    end subroutine btp

    subroutine synchronize(this, astream)
        class(simple_loops_btp_driver), intent(in) :: this
        type(stream), intent(in), optional :: astream

        call this%driver%synchronize(astream)
    end subroutine synchronize

    subroutine cleanup(this)
        class(simple_loops_btp_driver), intent(inout) :: this

        call this%driver%cleanup()
        deallocate(this%driver)
    end subroutine cleanup
end module simple_loops_btp_driver_module
