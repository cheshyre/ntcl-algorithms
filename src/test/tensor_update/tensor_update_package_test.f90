! Auto-generated -- DO NOT MODIFY
module tensor_update_package_test_module
    use :: util_api, only : &
            selector, &
            assert

    use :: scalar_inline_update_test_module, only : scalar_inline_update_test

    implicit none
    private

    public :: tensor_update_package_test

    type :: tensor_update_package_test
        type(selector) :: test_selector
    contains
        procedure :: run => run
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type tensor_update_package_test

    interface tensor_update_package_test
        module procedure constructor
    end interface tensor_update_package_test

contains
    function constructor(aselector) result(this)
        type(selector), intent(in) :: aselector
        type(tensor_update_package_test) :: this

        call this%clear()

        this%test_selector = aselector
    end function constructor

    subroutine run(this, assertion)
        class(tensor_update_package_test), intent(in) :: this
        type(assert), intent(inout) :: assertion

        type(scalar_inline_update_test) :: ascalar_inline_update_test

        call assertion%equal("tensor_update::Package test complete", .true.)

        if ( &
                this%test_selector%is_enabled("scalar_inline_update") ) then
            ascalar_inline_update_test = scalar_inline_update_test()
            call ascalar_inline_update_test%run(assertion)
            call ascalar_inline_update_test%cleanup()
        end if

    end subroutine run

    subroutine cleanup(this)
        class(tensor_update_package_test), intent(inout) :: this

        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(tensor_update_package_test), intent(inout) :: this
    end subroutine clear
end module tensor_update_package_test_module
