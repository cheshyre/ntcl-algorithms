module default_permute_factory_module
    use :: util_api, only : &
            string, &
            dictionary, &
            add_prefix_to_priorities

    use :: data_api, only : get_scratch_buffer

    use :: tensor_api, only : &
            tensor_converter_factory, &
            get_tensor_builder

    use :: tensor_permute_dev, only : &
            permute_factory, &
            tensor_permute, &
            tensor_permute_loops

#ifdef use_cuda
    use :: cuda_permute_plugin, only : &
            tensor_permute_cuda, &
            async_cuda_permute_driver
#endif

#ifdef use_hip
    use :: hip_permute_plugin, only : &
            tensor_permute_hip, &
            async_hip_permute_driver
#endif

    implicit none
    private

    public :: default_permute_factory

    type, extends(permute_factory) :: default_permute_factory
    contains
        procedure :: create_from_key => create_from_key
        procedure :: build => build
        procedure :: get_available_permute_drivers => get_available_permute_drivers
        procedure, private :: count_available_drivers => count_available_drivers
    end type default_permute_factory
contains
    subroutine create_from_key(this, permute, key)
        class(default_permute_factory), intent(in) :: this
        class(tensor_permute), allocatable, intent(inout) :: permute
        type(string), intent(in) :: key

        type(tensor_converter_factory) :: factory

        select case (key%char_array)
        case ("loops")
            permute = tensor_permute_loops()
#ifdef use_cuda
        case ("cuda")
            permute = tensor_permute_cuda(&
                    factory%get_c_pointer_converter("device"), &
                    factory%get_fortran_pointer_converter("pinned"))
        case ("cuda_async")
            permute = async_cuda_permute_driver()
#endif
#ifdef use_hip
        case ("hip")
            permute = tensor_permute_hip(&
                    factory%get_c_pointer_converter("device"), &
                    factory%get_fortran_pointer_converter("pinned"))
        case ("hip_async")
            permute = async_hip_permute_driver()
#endif
        case default
            error stop "default_permute_factory::create_from_key:Not a valid permute driver: "//key%char_array
        end select
    end subroutine create_from_key

    subroutine build(this, driver, options, priorities)
        class(default_permute_factory), intent(in) :: this
        class(tensor_permute), intent(inout) :: driver
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        type(string), dimension(:), allocatable :: pinned_priorities, device_priorities
        type(tensor_converter_factory) :: factory

        select type (driver)
        type is (tensor_permute_loops)
            continue ! No setup necessary
#ifdef use_cuda
        type is (tensor_permute_cuda)
            continue ! No setup necessary
        type is (async_cuda_permute_driver)
            pinned_priorities = add_prefix_to_priorities("cuda_async-", priorities)
            device_priorities = add_prefix_to_priorities("device_buffer-", pinned_priorities)
            pinned_priorities = add_prefix_to_priorities("pinned_buffer-", pinned_priorities)

            call driver%set_builder(get_tensor_builder("device"))
            call driver%set_converter(factory%get_c_pointer_converter("device"))
            call driver%set_scratch_buffers( &
                    get_scratch_buffer("pinned", options, pinned_priorities), &
                    get_scratch_buffer("device", options, device_priorities) )
#endif
#ifdef use_hip
        type is (tensor_permute_hip)
            continue ! No setup necessary
        type is (async_hip_permute_driver)
            pinned_priorities = add_prefix_to_priorities("hip_async-", priorities)
            device_priorities = add_prefix_to_priorities("device_buffer-", pinned_priorities)
            pinned_priorities = add_prefix_to_priorities("pinned_buffer-", pinned_priorities)

            call driver%set_builder(get_tensor_builder("device"))
            call driver%set_converter(factory%get_c_pointer_converter("device"))
            call driver%set_scratch_buffers( &
                    get_scratch_buffer("pinned", options, pinned_priorities), &
                    get_scratch_buffer("device", options, device_priorities) )
#endif
        class default
            error stop "default_permute_factory::build:Unknown type."
        end select
    end subroutine build

    function get_available_permute_drivers(this) result(drivers)
        class(default_permute_factory), intent(in) :: this
        type(string), dimension(:), allocatable :: drivers

        integer :: counter

        allocate(drivers(this%count_available_drivers()))

        drivers(1) = "loops"

        counter = 1
#ifdef use_cuda
        counter = counter + 1
        drivers(counter) = "cuda"
        counter = counter + 1
        drivers(counter) = "cuda_async"
#endif
#ifdef use_hip
        counter = counter + 1
        drivers(counter) = "hip"
        counter = counter + 1
        drivers(counter) = "hip_async"
#endif
    end function get_available_permute_drivers

    integer function count_available_drivers(this)
        class(default_permute_factory), intent(in) :: this

        count_available_drivers = 1

#ifdef use_cuda
        count_available_drivers = count_available_drivers + 2
#endif
#ifdef use_hip
        count_available_drivers = count_available_drivers + 2
#endif
    end function count_available_drivers
end module default_permute_factory_module
