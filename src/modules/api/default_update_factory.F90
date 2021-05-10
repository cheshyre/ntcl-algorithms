module default_update_factory_module
    use :: util_api, only : &
            string

    use :: tensor_api, only : tensor_converter_factory

    use :: tensor_update_dev, only : &
            update_factory, &
            scalar_inline_update, &
            fortran_siu_driver

#ifdef use_cuda
    use :: cuda_siu_driver_module, only : cuda_siu_driver
#endif
    implicit none
    private

    public :: default_update_factory

    type, extends(update_factory) :: default_update_factory
    contains
        procedure :: create_siu_from_key => create_siu_from_key
        procedure :: get_available_siu_drivers => get_available_siu_drivers
        procedure, private :: count_available_drivers => count_available_drivers
    end type default_update_factory

contains
    subroutine create_siu_from_key(this, update, key)
        class(default_update_factory), intent(in) :: this
        class(scalar_inline_update), allocatable, intent(inout) :: update
        type(string), intent(in) :: key

        type(tensor_converter_factory) :: factory

        select case (key%char_array)
        case ("fortran")
            update = fortran_siu_driver()
#ifdef use_cuda
        case ("cuda")
            update = cuda_siu_driver(factory%get_c_pointer_converter("device"))
#endif
        case default
            error stop "default_update_factory::create_siu_from_key:Not a valid siu driver: "//key%char_array
        end select
    end subroutine create_siu_from_key

    function get_available_siu_drivers(this) result(drivers)
        class(default_update_factory), intent(in) :: this
        type(string), dimension(:), allocatable :: drivers

        integer :: counter

        allocate(drivers(this%count_available_drivers()))

        counter = 1
        drivers(counter) = "fortran"
#ifdef use_cuda
        counter = counter + 1
        drivers(counter) = "cuda"
#endif
    end function get_available_siu_drivers

    integer function count_available_drivers(this)
        class(default_update_factory), intent(in) :: this

        count_available_drivers = 1

#ifdef use_cuda
        count_available_drivers = count_available_drivers + 1
#endif
    end function count_available_drivers
end module default_update_factory_module
