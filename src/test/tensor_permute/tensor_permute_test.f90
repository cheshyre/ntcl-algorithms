module tensor_permute_test_module
    use :: util_api, only : &
            assert, &
            string, &
            dictionary

    use :: data_api, only : memory_factory

    use :: algorithms_api, only : tensor_permute_factory

    use :: tensor_permute_test_helper_module, only : &
            test_all

    implicit none
    private

    public :: tensor_permute_test

    type :: tensor_permute_test
    contains
        procedure :: run => run
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type tensor_permute_test

    interface tensor_permute_test
        module procedure constructor
    end interface tensor_permute_test
contains
    function constructor() result(this)
        type(tensor_permute_test) :: this

        call this%clear()
    end function constructor

    subroutine run(this, assertion)
        class(tensor_permute_test), intent(in) :: this
        type(assert), intent(inout) :: assertion

        type(string), dimension(:), allocatable :: drivers, memory_types
        integer :: idx, midx
        type(string) :: prefix, my_prefix
        type(dictionary) :: options
        type(string), dimension(1) :: priorities

        call assertion%equal("tensor_permute::Test complete", .true.)

        drivers = tensor_permute_factory%get_available_permute_drivers()
        memory_types = memory_factory%get_available_memory_types()

        prefix = "tensor_permute::"

        call test_all(assertion, prefix%char_array//"default:")

        do idx = 1, size(drivers)
            do midx = 1, size(memory_types)
                my_prefix = prefix%char_array//"memory_type="//memory_types(midx)%char_array//","// &
                        "permute_driver="//drivers(idx)%char_array//":"
                call test_all(assertion, my_prefix%char_array, &
                        drivers(idx)%char_array, memory_types(midx)%char_array)
            end do
        end do

        options = dictionary()
        do idx = 1, size(drivers)
            do midx = 1, size(memory_types)
                call options%set_value("memory_type", memory_types(midx)%char_array)
                call options%set_value("permute_driver", drivers(idx)%char_array)
                my_prefix = prefix%char_array//"options(memory_type)="//memory_types(midx)%char_array//","// &
                        "options(permute_driver)="//drivers(idx)%char_array//":"
                call test_all(assertion, my_prefix%char_array, options=options)
            end do
        end do
        priorities(1) = "unittest-"
        do idx = 1, size(drivers)
            do midx = 1, size(memory_types)
                call options%set_value("memory_type", "dummy")
                call options%set_value("permute_driver", "dummy")
                call options%set_value("unittest-memory_type", memory_types(midx)%char_array)
                call options%set_value("unittest-permute_driver", drivers(idx)%char_array)
                my_prefix = prefix%char_array//"options(unittest-memory_type)="//memory_types(midx)%char_array//","// &
                        "options(unittest-permute_driver)="//drivers(idx)%char_array//":"
                call test_all(assertion, my_prefix%char_array, options=options, priorities=priorities)
            end do
        end do
    end subroutine run

    subroutine cleanup(this)
        class(tensor_permute_test), intent(inout) :: this

        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(tensor_permute_test), intent(inout) :: this
    end subroutine clear
end module tensor_permute_test_module
