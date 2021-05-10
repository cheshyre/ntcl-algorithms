! Auto-generated -- DO NOT MODIFY
module tensor_permute_package_test_module
    use :: util_api, only : &
            selector, &
            assert

    use :: tensor_permute_test_module, only : tensor_permute_test

    implicit none
    private

    public :: tensor_permute_package_test

    type :: tensor_permute_package_test
        type(selector) :: test_selector
    contains
        procedure :: run => run
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type tensor_permute_package_test

    interface tensor_permute_package_test
        module procedure constructor
    end interface tensor_permute_package_test

contains
    function constructor(aselector) result(this)
        type(selector), intent(in) :: aselector
        type(tensor_permute_package_test) :: this

        call this%clear()

        this%test_selector = aselector
    end function constructor

    subroutine run(this, assertion)
        class(tensor_permute_package_test), intent(in) :: this
        type(assert), intent(inout) :: assertion

        type(tensor_permute_test) :: atensor_permute_test

        call assertion%equal("tensor_permute::Package test complete", .true.)

        if ( &
                this%test_selector%is_enabled("tensor_permute") ) then
            atensor_permute_test = tensor_permute_test()
            call atensor_permute_test%run(assertion)
            call atensor_permute_test%cleanup()
        end if

    end subroutine run

    subroutine cleanup(this)
        class(tensor_permute_package_test), intent(inout) :: this

        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(tensor_permute_package_test), intent(inout) :: this
    end subroutine clear
end module tensor_permute_package_test_module
