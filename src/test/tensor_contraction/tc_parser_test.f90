module tc_parser_test_module
    use :: util_api, only : &
            assert, &
            string
    use :: algorithms_dev, only : &
            tc_parser, &
            tc_descriptor

    implicit none
    private

    public :: tc_parser_test

    type :: tc_parser_test
    contains
        procedure :: run => run
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type tc_parser_test

    interface tc_parser_test
        module procedure constructor
    end interface tc_parser_test
contains
    function constructor() result(this)
        type(tc_parser_test) :: this

        call this%clear()
    end function constructor

    subroutine run(this, assertion)
        class(tc_parser_test), intent(in) :: this
        type(assert), intent(inout) :: assertion

        type(tc_parser) :: atc_parser
        type(tc_descriptor) :: atc_descriptor

        atc_descriptor = atc_parser%parse(string("T_new(a,b,i,j) = V(a,b,c,d) * T(c,d,i,j)"))

        call assertion%equal("tc_parser::4x4x4::external_indices::", &
                atc_descriptor%num_external_indices == 4)
        call assertion%equal("tc_parser::4x4x4::internal_indices::", &
                atc_descriptor%num_internal_indices == 2)

        call assertion%equal("tc_parser::4x4x4::c_indices::", &
                all(atc_descriptor%c_indices == [1,2,3,4]))
        call assertion%equal("tc_parser::4x4x4::a_indices::", &
                all(atc_descriptor%a_indices == [1,2,5,6]))
        call assertion%equal("tc_parser::4x4x4::b_indices::", &
                all(atc_descriptor%b_indices == [5,6,3,4]))
        call atc_descriptor%cleanup()

        atc_descriptor = atc_parser%parse(string("T_new(a,b,i,j) : V(a,b,c,d) * T(c,d,i,j)"))

        call assertion%equal("tc_parser::4x4x4::external_indices::", &
                atc_descriptor%num_external_indices == 4)
        call assertion%equal("tc_parser::4x4x4::internal_indices::", &
                atc_descriptor%num_internal_indices == 2)

        call assertion%equal("tc_parser::4x4x4::c_indices::", &
                all(atc_descriptor%c_indices == [1,2,3,4]))
        call assertion%equal("tc_parser::4x4x4::a_indices::", &
                all(atc_descriptor%a_indices == [1,2,5,6]))
        call assertion%equal("tc_parser::4x4x4::b_indices::", &
                all(atc_descriptor%b_indices == [5,6,3,4]))
        call atc_descriptor%cleanup()

        atc_descriptor = atc_parser%parse(string("T_new(a,b,i,j) = V(a,b) * T(i,j)"))
        call assertion%equal("tc_parser::4x2x2::external_indices::", &
                atc_descriptor%num_external_indices == 4)
        call assertion%equal("tc_parser::4x2x2::internal_indices::", &
                atc_descriptor%num_internal_indices == 0)

        call assertion%equal("tc_parser::4x2x2::c_indices::", &
                all(atc_descriptor%c_indices == [1,2,3,4]))
        call assertion%equal("tc_parser::4x2x2::a_indices::", &
                all(atc_descriptor%a_indices == [1,2]))
        call assertion%equal("tc_parser::4x2x2::b_indices::", &
                all(atc_descriptor%b_indices == [3,4]))
        call atc_descriptor%cleanup()

        atc_descriptor = atc_parser%parse(string("E=t(i)*v(i)"))
        call assertion%equal("tc_parser::0x1x1::external_indices::", &
                atc_descriptor%num_external_indices == 0)
        call assertion%equal("tc_parser::0x1x1::internal_indices::", &
                atc_descriptor%num_internal_indices == 1)
        call assertion%equal("tc_parser::0x1x1::size(c_indices)::", &
                size(atc_descriptor%c_indices) == 0)
        call assertion%equal("tc_parser::0x1x1::a_indices::", &
                all(atc_descriptor%a_indices == [1]))
        call assertion%equal("tc_parser::0x1x1::b_indices::", &
                all(atc_descriptor%b_indices == [1]))
        call atc_descriptor%cleanup()
        atc_descriptor = atc_parser%parse(string("E(i)=f*v(i)"))
        call assertion%equal("tc_parser::1x0x1::external_indices::", &
                atc_descriptor%num_external_indices == 1)
        call assertion%equal("tc_parser::1x0x1::internal_indices::", &
                atc_descriptor%num_internal_indices == 0)
        call assertion%equal("tc_parser::1x0x1::c_indices::", &
                all(atc_descriptor%c_indices == [1]))
        call assertion%equal("tc_parser::1x0x1::size(a_indices)::", &
                size(atc_descriptor%a_indices) == 0)
        call assertion%equal("tc_parser::1x0x1::b_indices::", &
                all(atc_descriptor%b_indices == [1]))
        call atc_descriptor%cleanup()
    end subroutine run

    subroutine cleanup(this)
        class(tc_parser_test), intent(inout) :: this

        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(tc_parser_test), intent(inout) :: this
    end subroutine clear
end module tc_parser_test_module
