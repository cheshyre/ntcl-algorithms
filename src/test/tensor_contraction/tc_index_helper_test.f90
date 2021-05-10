module tc_index_helper_test_module
    use, intrinsic :: iso_fortran_env, only : int64

    use :: util_api, only : assert

    use :: tc_index_helper_module, only : tc_index_helper

    implicit none
    private

    public :: tc_index_helper_test

    type :: tc_index_helper_test
    contains
        procedure :: run => run
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type tc_index_helper_test

    interface tc_index_helper_test
        module procedure constructor
    end interface tc_index_helper_test
contains
    function constructor() result(this)
        type(tc_index_helper_test) :: this

        call this%clear()
    end function constructor

    subroutine run(this, assertion)
        class(tc_index_helper_test), intent(in) :: this
        type(assert), intent(inout) :: assertion

        type(tc_index_helper) :: helper
        integer(int64), dimension(3) :: sizes
        integer(int64), dimension(3) :: multipliers
        integer(int64), dimension(3) :: indexes

        call assertion%equal("tc_index_helper::Test complete", .true.)

        helper%number_of_indices = 3
        sizes = int([2,2,2], int64)
        call helper%set_number_of_products(sizes)
        call assertion%equal("tc_index_helper::number_of_products", &
                8 == helper%number_of_products )

        call helper%calculate_all_multipliers(multipliers, sizes)
        call assertion%equal("tc_index_helper::all_multipliers", &
                all(multipliers == int([1,2,4], int64)))

        call helper%calculate_abc_multipliers(multipliers, sizes, [1,2])
        call assertion%equal("tc_index_helper::c_multipliers", &
                all(multipliers == int([1,2,0], int64)))

        call helper%calculate_abc_multipliers(multipliers, sizes, [1,3])
        call assertion%equal("tc_index_helper::a_multipliers", &
                all(multipliers == int([1,0,2], int64)))

        call helper%calculate_abc_multipliers(multipliers, sizes, [3,2])
        call assertion%equal("tc_index_helper::b_multipliers", &
                all(multipliers == int([0,2,1], int64)))

        call helper%calculate_all_multipliers(helper%all_multipliers, sizes)
        call helper%calculate_abc_multipliers(helper%c_multipliers, sizes, [1,2])
        call helper%calculate_abc_multipliers(helper%a_multipliers, sizes, [1,3])
        call helper%calculate_abc_multipliers(helper%b_multipliers, sizes, [3,2])

        call helper%calculate_all_indexes(indexes, int(7, int64))
        call assertion%equal("tc_index_helper::all indexes for idx=7", &
                all(indexes == int([1,2,2], int64)))

        call assertion%equal("tc_index_helper::c for idx=7", &
                helper%calculate_index(int([1,2,2], int64), int([1,2,0], int64)) == int(3, int64))
        call assertion%equal("tc_index_helper::a for idx=7", &
                helper%calculate_index(int([1,2,2], int64), int([1,0,2], int64)) == int(3, int64))
        call assertion%equal("tc_index_helper::b for idx=7", &
                helper%calculate_index(int([1,2,2], int64), int([0,2,1], int64)) == int(4, int64))

        indexes = 0
        call helper%calculate_abc_indexes(indexes, int(1, int64))
        call assertion%equal("tc_index_helper::abc indexes for idx=1", &
                all(indexes == int([1,1,1], int64)))
        call helper%calculate_abc_indexes(indexes, int(6, int64))
        call assertion%equal("tc_index_helper::abc indexes for idx=6", &
                all(indexes == int([2,4,2], int64)))
    end subroutine run

    subroutine cleanup(this)
        class(tc_index_helper_test), intent(inout) :: this

        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(tc_index_helper_test), intent(inout) :: this
    end subroutine clear
end module tc_index_helper_test_module
