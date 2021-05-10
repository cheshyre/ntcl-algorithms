! Auto-generated -- DO NOT MODIFY
module matrix_multiplication_package_test_module
    use :: util_api, only : &
            selector, &
            assert

    use :: matrix_multiplication_test_module, only : matrix_multiplication_test

    implicit none
    private

    public :: matrix_multiplication_package_test

    type :: matrix_multiplication_package_test
        type(selector) :: test_selector
    contains
        procedure :: run => run
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type matrix_multiplication_package_test

    interface matrix_multiplication_package_test
        module procedure constructor
    end interface matrix_multiplication_package_test

contains
    function constructor(aselector) result(this)
        type(selector), intent(in) :: aselector
        type(matrix_multiplication_package_test) :: this

        call this%clear()

        this%test_selector = aselector
    end function constructor

    subroutine run(this, assertion)
        class(matrix_multiplication_package_test), intent(in) :: this
        type(assert), intent(inout) :: assertion

        type(matrix_multiplication_test) :: amatrix_multiplication_test

        call assertion%equal("matrix_multiplication::Package test complete", .true.)

        if ( &
                this%test_selector%is_enabled("matrix_multiplication") ) then
            amatrix_multiplication_test = matrix_multiplication_test()
            call amatrix_multiplication_test%run(assertion)
            call amatrix_multiplication_test%cleanup()
        end if

    end subroutine run

    subroutine cleanup(this)
        class(matrix_multiplication_package_test), intent(inout) :: this

        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(matrix_multiplication_package_test), intent(inout) :: this
    end subroutine clear
end module matrix_multiplication_package_test_module
