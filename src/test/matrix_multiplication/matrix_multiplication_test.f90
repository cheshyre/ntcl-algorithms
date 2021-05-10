module matrix_multiplication_test_module
    use :: util_api, only : &
            assert, &
            string, &
            dictionary

    use :: data_api, only : memory_factory

    use :: algorithms_api, only : &
            matrix_multiplication_factory

    use :: matrix_multiplication_test_helper_module, only : &
            matrix_multiplication_test_helper

    implicit none
    private

    public :: matrix_multiplication_test

    type :: matrix_multiplication_test
    contains
        procedure :: run => run
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type matrix_multiplication_test

    interface matrix_multiplication_test
        module procedure constructor
    end interface matrix_multiplication_test
contains
    function constructor() result(this)
        type(matrix_multiplication_test) :: this

        call this%clear()
    end function constructor

    subroutine run(this, assertion)
        class(matrix_multiplication_test), intent(in) :: this
        type(assert), intent(inout) :: assertion

        type(string), dimension(:), allocatable :: drivers, memory_types
        integer :: idx, midx
        type(string) :: prefix, my_prefix
        type(matrix_multiplication_test_helper) :: helper
        type(dictionary) :: options
        type(string), dimension(1) :: priorities

        call assertion%equal("matrix_multiplication::Test complete", .true.)

        drivers = matrix_multiplication_factory%get_available_mm_drivers()
        memory_types = memory_factory%get_available_memory_types()
 
        prefix = "matrix_multiplication::"
        call helper%run(assertion, prefix%char_array//"default:")
        do idx = 1, size(drivers)
            do midx = 1, size(memory_types)
                my_prefix = prefix%char_array//"memory_type="//memory_types(midx)%char_array//","// &
                        "mm_driver="//drivers(idx)%char_array//":"
                call helper%run(assertion, my_prefix%char_array, &
                        drivers(idx)%char_array, memory_types(midx)%char_array)
            end do
        end do

        options = dictionary()
        do idx = 1, size(drivers)
            do midx = 1, size(memory_types)
                call options%set_value("memory_type", memory_types(midx)%char_array)
                call options%set_value("mm_driver", drivers(idx)%char_array)
                my_prefix = prefix%char_array//"options(memory_type)="//memory_types(midx)%char_array//","// &
                        "options(mm_driver)="//drivers(idx)%char_array//":"
                call helper%run(assertion, my_prefix%char_array, options=options)
            end do
        end do

        priorities(1) = "unittest-"
        do idx = 1, size(drivers)
            do midx = 1, size(memory_types)
                call options%set_value("memory_type", "dummy")
                call options%set_value("mm_driver", "dummy")
                call options%set_value("unittest-memory_type", memory_types(midx)%char_array)
                call options%set_value("unittest-mm_driver", drivers(idx)%char_array)
                my_prefix = prefix%char_array//"options(unittest-memory_type)="//memory_types(midx)%char_array//","// &
                        "options(unittest-mm_driver)="//drivers(idx)%char_array//":"
                call helper%run(assertion, my_prefix%char_array, options=options, priorities=priorities)
            end do
        end do
    end subroutine run

    subroutine cleanup(this)
        class(matrix_multiplication_test), intent(inout) :: this

        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(matrix_multiplication_test), intent(inout) :: this
    end subroutine clear
end module matrix_multiplication_test_module
