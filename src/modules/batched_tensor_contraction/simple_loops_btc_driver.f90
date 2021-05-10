module simple_loops_btc_driver_module
    use :: data_api, only : stream

    use :: tensor_api, only : &
            tensor_array_element, &
            scalar

    use :: tensor_contraction_api, only : tensor_contraction

    use :: batched_tensor_contraction_module, only : batched_tensor_contraction

    implicit none
    private

    public :: simple_loops_btc_driver

    type, extends(batched_tensor_contraction) :: simple_loops_btc_driver
        class(tensor_contraction), allocatable :: driver
    contains
        procedure :: contract => contract
        procedure :: synchronize => synchronize
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type simple_loops_btc_driver

    interface simple_loops_btc_driver
        module procedure constructor_empty
        module procedure constructor
    end interface simple_loops_btc_driver

contains
    function constructor_empty() result(this)
        type(simple_loops_btc_driver) :: this

        call this%clear()
    end function constructor_empty

    function constructor(driver) result(this)
        class(tensor_contraction), intent(in) :: driver
        type(simple_loops_btc_driver) :: this

        this = simple_loops_btc_driver()

        this%driver = driver
    end function constructor

    subroutine contract(this, c, a, b, alpha, beta, astream)
        class(simple_loops_btc_driver), intent(inout) :: this
        type(tensor_array_element), dimension(:), intent(inout) :: c
        type(tensor_array_element), dimension(:), intent(in) :: a, b
        type(scalar), dimension(:), intent(in), optional :: alpha, beta
        type(stream), intent(in), optional :: astream

        integer :: number_of_tcs, idx

        number_of_tcs = size(c)
        if ( present(alpha) .and. present(beta) ) then
            do idx = 1, number_of_tcs
                call this%driver%contract(c(idx)%element, a(idx)%element, b(idx)%element, alpha(idx), beta(idx), astream)
            end do
        else if (present(alpha) ) then
            do idx = 1, number_of_tcs
                call this%driver%contract(c(idx)%element, a(idx)%element, b(idx)%element, alpha=alpha(idx), astream=astream)
            end do
        else if (present(beta) ) then
            do idx = 1, number_of_tcs
                call this%driver%contract(c(idx)%element, a(idx)%element, b(idx)%element, beta=beta(idx), astream=astream)
            end do
        else
            do idx = 1, number_of_tcs
                call this%driver%contract(c(idx)%element, a(idx)%element, b(idx)%element, astream=astream)
            end do
        end if
    end subroutine contract

    subroutine synchronize(this, astream)
        class(simple_loops_btc_driver), intent(in) :: this
        type(stream), intent(in), optional :: astream

        call this%driver%synchronize(astream)
    end subroutine synchronize

    subroutine cleanup(this)
        class(simple_loops_btc_driver), intent(inout) :: this

        if ( allocated(this%driver) ) then
            call this%driver%cleanup()
            deallocate(this%driver)
        end if
        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(simple_loops_btc_driver), intent(inout) :: this
    end subroutine clear
end module simple_loops_btc_driver_module
