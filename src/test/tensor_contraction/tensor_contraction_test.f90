module tensor_contraction_test_module
    use :: util_api, only : &
            assert, &
            string, &
            dictionary

    use :: data_api, only : memory_factory

    use :: algorithms_api, only : tensor_contraction_factory

    use :: tensor_contraction_test_helper_module, only : &
            test_all

    implicit none
    private

    public :: tensor_contraction_test

    type :: tensor_contraction_test
    contains
        procedure :: run => run
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type tensor_contraction_test

    interface tensor_contraction_test
        module procedure constructor
    end interface tensor_contraction_test
contains
    function constructor() result(this)
        type(tensor_contraction_test) :: this

        call this%clear()
    end function constructor

    subroutine run(this, assertion)
        class(tensor_contraction_test), intent(in) :: this
        type(assert), intent(inout) :: assertion

        type(string), dimension(:), allocatable :: drivers, memory_types
        integer :: idx, midx
        type(string) :: prefix, my_prefix
        type(dictionary) :: options
        type(string), dimension(1) :: priorities

        call assertion%equal("tensor_contraction::Test complete", .true.)

        drivers = tensor_contraction_factory%get_available_contraction_drivers()
        memory_types = memory_factory%get_available_memory_types()

        prefix = "tensor_contraction::"

        call test_all(assertion, prefix%char_array//"default:")

        do idx = 1, size(drivers)
            do midx = 1, size(memory_types)
                my_prefix = prefix%char_array//"memory_type="//memory_types(midx)%char_array//","// &
                        "contraction_driver="//drivers(idx)%char_array//":"
                call test_all(assertion, my_prefix%char_array, &
                        drivers(idx)%char_array, memory_types(midx)%char_array)
            end do
        end do

        options = dictionary()
        do idx = 1, size(drivers)
            do midx = 1, size(memory_types)
                call options%set_value("memory_type", memory_types(midx)%char_array)
                call options%set_value("contraction_driver", drivers(idx)%char_array)
                my_prefix = prefix%char_array//"options(memory_type)="//memory_types(midx)%char_array//","// &
                        "options(contraction_driver)="//drivers(idx)%char_array//":"
                call test_all(assertion, my_prefix%char_array, options=options)
            end do
        end do
        priorities(1) = "unittest-"
        do idx = 1, size(drivers)
            do midx = 1, size(memory_types)
                call options%set_value("memory_type", "dummy")
                call options%set_value("contraction_driver", "dummy")
                call options%set_value("unittest-memory_type", memory_types(midx)%char_array)
                call options%set_value("unittest-contraction_driver", drivers(idx)%char_array)
                my_prefix = prefix%char_array//"options(unittest-memory_type)="//memory_types(midx)%char_array//","// &
                        "options(unittest-contraction_driver)="//drivers(idx)%char_array//":"
                call test_all(assertion, my_prefix%char_array, options=options, priorities=priorities)
            end do
        end do
    end subroutine run

    subroutine cleanup(this)
        class(tensor_contraction_test), intent(inout) :: this

        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(tensor_contraction_test), intent(inout) :: this
    end subroutine clear
end module tensor_contraction_test_module
