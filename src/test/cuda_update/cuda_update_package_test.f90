! Auto-generated -- DO NOT MODIFY
module cuda_update_package_test_module
    use :: util_api, only : &
            selector, &
            assert

    implicit none
    private

    public :: cuda_update_package_test

    type :: cuda_update_package_test
        type(selector) :: test_selector
    contains
        procedure :: run => run
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type cuda_update_package_test

    interface cuda_update_package_test
        module procedure constructor
    end interface cuda_update_package_test

contains
    function constructor(aselector) result(this)
        type(selector), intent(in) :: aselector
        type(cuda_update_package_test) :: this

        call this%clear()

        this%test_selector = aselector
    end function constructor

    subroutine run(this, assertion)
        class(cuda_update_package_test), intent(in) :: this
        type(assert), intent(inout) :: assertion

        call assertion%equal("cuda_update::Package test complete", .true.)

    end subroutine run

    subroutine cleanup(this)
        class(cuda_update_package_test), intent(inout) :: this

        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(cuda_update_package_test), intent(inout) :: this
    end subroutine clear
end module cuda_update_package_test_module
