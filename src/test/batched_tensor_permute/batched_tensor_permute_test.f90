module batched_tensor_permute_test_module
    use :: util_api, only : &
            assert, &
            string, &
            dictionary

    use :: data_api, only : memory_factory

    use :: algorithms_api, only : &
            batched_tensor_permute_factory

    use :: batched_tensor_permute_test_helper_module, only : &
            batched_tensor_permute_test_helper

    implicit none
    private

    public :: batched_tensor_permute_test

    type :: batched_tensor_permute_test
    contains
        procedure :: run => run
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type batched_tensor_permute_test

    interface batched_tensor_permute_test
        module procedure constructor
    end interface batched_tensor_permute_test
contains
    function constructor() result(this)
        type(batched_tensor_permute_test) :: this

        call this%clear()
    end function constructor

    subroutine run(this, assertion)
        class(batched_tensor_permute_test), intent(in) :: this
        type(assert), intent(inout) :: assertion

        type(string), dimension(:), allocatable :: drivers, memory_types
        integer :: idx, midx
        type(string) :: prefix, my_prefix
        type(batched_tensor_permute_test_helper) :: helper
        type(dictionary) :: options
        type(string), dimension(1) :: priorities

        call assertion%equal("batched_tensor_permute::Test complete", .true.)

        drivers = batched_tensor_permute_factory%get_available_btp_drivers()
        memory_types = memory_factory%get_available_memory_types()

        prefix = "batched_tensor_permute::"
        call helper%run(assertion, prefix%char_array//"default:")
        do idx = 1, size(drivers)
            do midx = 1, size(memory_types)
                my_prefix = prefix%char_array//"memory_type="//memory_types(midx)%char_array//","// &
                        "btp_driver="//drivers(idx)%char_array//":"
                call helper%run(assertion, my_prefix%char_array, &
                        drivers(idx)%char_array, memory_types(midx)%char_array)
            end do
        end do

        options = dictionary()
        do idx = 1, size(drivers)
            do midx = 1, size(memory_types)
                call options%set_value("memory_type", memory_types(midx)%char_array)
                call options%set_value("btp_driver", drivers(idx)%char_array)
                my_prefix = prefix%char_array//"options(memory_type)="//memory_types(midx)%char_array//","// &
                        "options(btp_driver)="//drivers(idx)%char_array//":"
                call helper%run(assertion, my_prefix%char_array, options=options)
            end do
        end do
        priorities(1) = "unittest-"
        do idx = 1, size(drivers)
            do midx = 1, size(memory_types)
                call options%set_value("memory_type", "dummy")
                call options%set_value("btp_driver", "dummy")
                call options%set_value("unittest-memory_type", memory_types(midx)%char_array)
                call options%set_value("unittest-btp_driver", drivers(idx)%char_array)
                my_prefix = prefix%char_array//"options(unittest-memory_type)="//memory_types(midx)%char_array//","// &
                        "options(unittest-btp_driver)="//drivers(idx)%char_array//":"
                call helper%run(assertion, my_prefix%char_array, options=options, priorities=priorities)
            end do
        end do
    end subroutine run

    subroutine cleanup(this)
        class(batched_tensor_permute_test), intent(inout) :: this

        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(batched_tensor_permute_test), intent(inout) :: this
    end subroutine clear
end module batched_tensor_permute_test_module
