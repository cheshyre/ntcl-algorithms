! Auto-generated -- DO NOT MODIFY
module tensor_contraction_package_test_module
    use :: util_api, only : &
            selector, &
            assert

    use :: tc_index_helper_test_module, only : tc_index_helper_test
    use :: tensor_contraction_test_module, only : tensor_contraction_test
    use :: tc_parser_test_module, only : tc_parser_test
    use :: tc_descriptor_test_module, only : tc_descriptor_test
    use :: ttgt_parser_test_module, only : ttgt_parser_test

    implicit none
    private

    public :: tensor_contraction_package_test

    type :: tensor_contraction_package_test
        type(selector) :: test_selector
    contains
        procedure :: run => run
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type tensor_contraction_package_test

    interface tensor_contraction_package_test
        module procedure constructor
    end interface tensor_contraction_package_test

contains
    function constructor(aselector) result(this)
        type(selector), intent(in) :: aselector
        type(tensor_contraction_package_test) :: this

        call this%clear()

        this%test_selector = aselector
    end function constructor

    subroutine run(this, assertion)
        class(tensor_contraction_package_test), intent(in) :: this
        type(assert), intent(inout) :: assertion

        type(tc_index_helper_test) :: atc_index_helper_test
        type(tensor_contraction_test) :: atensor_contraction_test
        type(tc_parser_test) :: atc_parser_test
        type(tc_descriptor_test) :: atc_descriptor_test
        type(ttgt_parser_test) :: attgt_parser_test

        call assertion%equal("tensor_contraction::Package test complete", .true.)

        if ( &
                this%test_selector%is_enabled("tc_index_helper") ) then
            atc_index_helper_test = tc_index_helper_test()
            call atc_index_helper_test%run(assertion)
            call atc_index_helper_test%cleanup()
        end if

        if ( &
                this%test_selector%is_enabled("tensor_contraction") ) then
            atensor_contraction_test = tensor_contraction_test()
            call atensor_contraction_test%run(assertion)
            call atensor_contraction_test%cleanup()
        end if

        if ( &
                this%test_selector%is_enabled("tc_parser") ) then
            atc_parser_test = tc_parser_test()
            call atc_parser_test%run(assertion)
            call atc_parser_test%cleanup()
        end if

        if ( &
                this%test_selector%is_enabled("tc_descriptor") ) then
            atc_descriptor_test = tc_descriptor_test()
            call atc_descriptor_test%run(assertion)
            call atc_descriptor_test%cleanup()
        end if

        if ( &
                this%test_selector%is_enabled("ttgt_parser") ) then
            attgt_parser_test = ttgt_parser_test()
            call attgt_parser_test%run(assertion)
            call attgt_parser_test%cleanup()
        end if

    end subroutine run

    subroutine cleanup(this)
        class(tensor_contraction_package_test), intent(inout) :: this

        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(tensor_contraction_package_test), intent(inout) :: this
    end subroutine clear
end module tensor_contraction_package_test_module
