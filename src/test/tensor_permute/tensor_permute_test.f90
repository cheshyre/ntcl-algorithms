module tensor_permute_test_module
    use, intrinsic :: iso_fortran_env, only : &
            int64, &
            real64

    use :: util_api, only : &
            assert, &
            string, &
            dictionary

    use :: data_api, only : &
            memory_factory, &
            storage_helper

    use :: tensor_api, only : &
            tensor, &
            allocate_and_copy_tensor

    use :: algorithms_api, only : &
            tensor_permute_factory, &
            tensor_permute

    use :: tensor_permute_test_helper_module, only : &
            test_all, &
            permute_real64_tensor_rank4_1_4_3_2

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

        do idx = 1, size(drivers)
            do midx = 1, size(memory_types)
                my_prefix = prefix%char_array//"memory_type="//memory_types(midx)%char_array//","// &
                        "permute_driver="//drivers(idx)%char_array//":"
                call test_blocking_bug(assertion, my_prefix%char_array, &
                        drivers(idx)%char_array, memory_types(midx)%char_array)
            end do
        end do

    end subroutine run

    subroutine test_blocking_bug(assertion, prefix, driver, memory)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix, driver, memory

        real(real64), dimension(:,:,:,:), allocatable :: src_data, dst_data
        class(tensor), allocatable :: src, dst
        class(tensor_permute), allocatable :: permute
        integer, dimension(4) :: permutation
        integer(int64), dimension(4) :: ddimensions, dimensions
        type(storage_helper) :: helper

        permutation = [1,4,3,2]
        dimensions = [398,55,136,6]
        ddimensions = dimensions(permutation)

        allocate(src_data(dimensions(1), dimensions(2), dimensions(3), dimensions(4)))
        allocate(dst_data(ddimensions(1), ddimensions(2), ddimensions(3), ddimensions(4)))

        call random_number(src_data)
        dst_data = 0.0d0

        call allocate_and_copy_tensor(src, src_data, memory)
        call allocate_and_copy_tensor(dst, dst_data, memory)

        call permute_real64_tensor_rank4_1_4_3_2(dst_data, src_data)

        call tensor_permute_factory%create(permute, driver)
        call permute%permute(src, dst, permutation)

        call assertion%equal(prefix//":Block size bug", &
                helper%equal(dst%storage, dst_data, 1d-12))

        call src%cleanup()
        call dst%cleanup()
        call permute%cleanup()
    end subroutine test_blocking_bug

    subroutine cleanup(this)
        class(tensor_permute_test), intent(inout) :: this

        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(tensor_permute_test), intent(inout) :: this
    end subroutine clear
end module tensor_permute_test_module
