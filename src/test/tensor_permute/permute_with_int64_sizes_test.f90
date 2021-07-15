module permute_with_int64_sizes_test_module
    use :: util_api, only : &
            assert, &
            string

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
            permute_real32_tensor_rank4_1_4_3_2

    implicit none
    private

    public :: permute_with_int64_sizes_test

    type :: permute_with_int64_sizes_test
    contains
        procedure :: run => run
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type permute_with_int64_sizes_test

    interface permute_with_int64_sizes_test
        module procedure constructor
    end interface permute_with_int64_sizes_test
contains
    function constructor() result(this)
        type(permute_with_int64_sizes_test) :: this

        call this%clear()
    end function constructor

    subroutine run(this, assertion)
        class(permute_with_int64_sizes_test), intent(in) :: this
        type(assert), intent(inout) :: assertion

        type(string), dimension(:), allocatable :: drivers, memory_types
        integer :: idx, midx
        type(string) :: prefix, my_prefix

        drivers = tensor_permute_factory%get_available_permute_drivers()
        memory_types = memory_factory%get_available_memory_types()

        prefix = "permute_with_int64_sizes::"

        do idx = 1, size(drivers)
            do midx = 1, size(memory_types)
                my_prefix = prefix%char_array//"memory_type="//memory_types(midx)%char_array//","// &
                        "permute_driver="//drivers(idx)%char_array//":"
                call test_3B(assertion, my_prefix%char_array, &
                        drivers(idx)%char_array, memory_types(midx)%char_array)
            end do
        end do
    end subroutine run

    subroutine test_3B(assertion, prefix, driver, memory)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix, driver, memory

        real, dimension(:,:,:,:), allocatable :: src_data, dst_data
        class(tensor), allocatable :: src, dst
        class(tensor_permute), allocatable :: permute
        integer, dimension(4) :: permutation
        integer, dimension(4) :: ddimensions, dimensions
        type(storage_helper) :: helper

        permutation = [1,4,3,2]
        dimensions = [1003,255,136,86]
        ddimensions = dimensions(permutation)

        allocate(src_data(dimensions(1), dimensions(2), dimensions(3), dimensions(4)))
        allocate(dst_data(ddimensions(1), ddimensions(2), ddimensions(3), ddimensions(4)))

        call random_number(src_data)
        dst_data = 0.0

        call allocate_and_copy_tensor(dst, dst_data, memory)
        call allocate_and_copy_tensor(src, src_data, memory)

        call permute_real32_tensor_rank4_1_4_3_2(dst_data, src_data)

        call tensor_permute_factory%create(permute, driver)
        call permute%permute(src, dst, permutation)

        call assertion%equal(prefix//":Permute with 3B elements", &
                helper%equal(dst%storage, dst_data))

        call src%cleanup()
        call dst%cleanup()
        call permute%cleanup()
    end subroutine test_3B

    subroutine cleanup(this)
        class(permute_with_int64_sizes_test), intent(inout) :: this

        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(permute_with_int64_sizes_test), intent(inout) :: this
    end subroutine clear
end module permute_with_int64_sizes_test_module
