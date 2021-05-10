! Auto-generated -- DO NOT MODIFY
module batched_tensor_permute_package_test_module
    use :: util_api, only : &
            selector, &
            assert

    use :: batched_tensor_permute_test_module, only : batched_tensor_permute_test

    implicit none
    private

    public :: batched_tensor_permute_package_test

    type :: batched_tensor_permute_package_test
        type(selector) :: test_selector
    contains
        procedure :: run => run
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type batched_tensor_permute_package_test

    interface batched_tensor_permute_package_test
        module procedure constructor
    end interface batched_tensor_permute_package_test

contains
    function constructor(aselector) result(this)
        type(selector), intent(in) :: aselector
        type(batched_tensor_permute_package_test) :: this

        call this%clear()

        this%test_selector = aselector
    end function constructor

    subroutine run(this, assertion)
        class(batched_tensor_permute_package_test), intent(in) :: this
        type(assert), intent(inout) :: assertion

        type(batched_tensor_permute_test) :: abatched_tensor_permute_test

        call assertion%equal("batched_tensor_permute::Package test complete", .true.)

        if ( &
                this%test_selector%is_enabled("batched_tensor_permute") ) then
            abatched_tensor_permute_test = batched_tensor_permute_test()
            call abatched_tensor_permute_test%run(assertion)
            call abatched_tensor_permute_test%cleanup()
        end if

    end subroutine run

    subroutine cleanup(this)
        class(batched_tensor_permute_package_test), intent(inout) :: this

        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(batched_tensor_permute_package_test), intent(inout) :: this
    end subroutine clear
end module batched_tensor_permute_package_test_module
