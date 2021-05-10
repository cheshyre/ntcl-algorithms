module tc_descriptor_test_module
    use :: util_api, only : &
            assert, &
            string
    use :: tc_descriptor_module, only : tc_descriptor

    implicit none
    private

    public :: tc_descriptor_test

    type :: tc_descriptor_test
    contains
        procedure :: run => run
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type tc_descriptor_test

    interface tc_descriptor_test
        module procedure constructor
    end interface tc_descriptor_test
contains
    function constructor() result(this)
        type(tc_descriptor_test) :: this

        call this%clear()
    end function constructor

    subroutine run(this, assertion)
        class(tc_descriptor_test), intent(in) :: this
        type(assert), intent(inout) :: assertion

        type(tc_descriptor) :: atc_descriptor
        character(len=:), allocatable :: str
        integer, dimension(0) :: empty

        call assertion%equal("tc_descriptor::Test complete", .true.)

        str = "tc_descriptor::[1,2],[1],[2]::"
        atc_descriptor = tc_descriptor([1,2], [1], [2])

        call assertion%equal(str//"is_valid::", &
                atc_descriptor%is_valid([1,2], [1], [2]))
        call assertion%equal(str//"Correct c_indices", &
                all(atc_descriptor%c_indices == [1,2]) )
        call assertion%equal(str//"Correct a_indices", &
                all(atc_descriptor%a_indices == [1]) )
        call assertion%equal(str//"Correct b_indices", &
                all(atc_descriptor%b_indices == [2]) )
        call assertion%equal(str//"Correct number of indices", &
                atc_descriptor%num_unique_indices == 2 )
        call assertion%equal(str//"external_indices::", &
                atc_descriptor%num_external_indices == 2)
        call assertion%equal(str//"internal_indices::", &
                atc_descriptor%num_internal_indices == 0)
        call atc_descriptor%cleanup()

        str = "tc_descriptor::[1,2,3,4],[1,2,5,6],[5,6,3,4]::"
        atc_descriptor = tc_descriptor([1,2,3,4], [1,2,5,6], [5,6,3,4])
        call assertion%equal(str//"is_valid::", &
                atc_descriptor%is_valid([1,2,3,4], [1,2,5,6], [5,6,3,4]))
        call assertion%equal(str//"Correct c_indices", &
                all(atc_descriptor%c_indices == [1,2,3,4]) )
        call assertion%equal(str//"Correct a_indices", &
                all(atc_descriptor%a_indices == [1,2,5,6]) )
        call assertion%equal(str//"Correct b_indices", &
                all(atc_descriptor%b_indices == [5,6,3,4]) )
        call assertion%equal(str//"Correct number of indices", &
                atc_descriptor%num_unique_indices == 6 )
        call assertion%equal(str//"external_indices::", &
                atc_descriptor%num_external_indices == 4)
        call assertion%equal(str//"internal_indices::", &
                atc_descriptor%num_internal_indices == 2)
        call atc_descriptor%cleanup()

        str = "tc_descriptor::[1,2,3,4,7],[1,2,5,6],[5,6,3,4]::"
        call assertion%equal(str//".not.is_valid::", &
                .not. atc_descriptor%is_valid([1,2,3,4,7], [1,2,5,6], [5,6,3,4]))
        call atc_descriptor%cleanup()

        str = "tc_descriptor::[1,2,3,4],[1,2,5,6],[5,6,3,4,6]::"
        call assertion%equal(str//".not.is_valid::", &
                .not. atc_descriptor%is_valid([1,2,3,4], [1,2,5,6], [5,6,3,4,6]))
        call atc_descriptor%cleanup()

        str = "tc_descriptor::[1,2,3,4],[1,2,5,6],[5,6,4]::"
        call assertion%equal(str//".not.is_valid::", &
                .not. atc_descriptor%is_valid([1,2,3,4], [1,2,5,6], [5,6,4,6]))
        call atc_descriptor%cleanup()

        str = "tc_descriptor::[],[1,2],[1,2]::"
        atc_descriptor = tc_descriptor(empty, [1,2], [1,2])

        call assertion%equal(str//"is_valid::", &
                atc_descriptor%is_valid(empty, [1,2], [1,2]))
        call assertion%equal(str//"Correct c_indices", &
                size(atc_descriptor%c_indices) == 0)
        call assertion%equal(str//"Correct a_indices", &
                all(atc_descriptor%a_indices == [1,2]) )
        call assertion%equal(str//"Correct b_indices", &
                all(atc_descriptor%b_indices == [1,2]) )
        call assertion%equal(str//"Correct number of indices", &
                atc_descriptor%num_unique_indices == 2 )
        call assertion%equal(str//"external_indices::", &
                atc_descriptor%num_external_indices == 0)
        call assertion%equal(str//"internal_indices::", &
                atc_descriptor%num_internal_indices == 2)
        call atc_descriptor%cleanup()

        str = "tc_descriptor::[1,2],[],[1,2]::"
        atc_descriptor = tc_descriptor([1,2], empty, [1,2])

        call assertion%equal(str//"is_valid::", &
                atc_descriptor%is_valid([1,2], empty, [1,2]))
        call assertion%equal(str//"Correct c_indices", &
                all(atc_descriptor%c_indices == [1,2]))
        call assertion%equal(str//"Correct a_indices", &
                size(atc_descriptor%a_indices) == 0 )
        call assertion%equal(str//"Correct b_indices", &
                all(atc_descriptor%b_indices == [1,2]) )
        call assertion%equal(str//"Correct number of indices", &
                atc_descriptor%num_unique_indices == 2 )
        call assertion%equal(str//"external_indices::", &
                atc_descriptor%num_external_indices == 2)
        call assertion%equal(str//"internal_indices::", &
                atc_descriptor%num_internal_indices == 0)
        call atc_descriptor%cleanup()

    end subroutine run

    subroutine cleanup(this)
        class(tc_descriptor_test), intent(inout) :: this

        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(tc_descriptor_test), intent(inout) :: this
    end subroutine clear
end module tc_descriptor_test_module
