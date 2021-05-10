module default_bmm_factory_module
    use :: util_api, only : &
            string, &
            dictionary, &
            add_prefix_to_priorities

    use :: tensor_api, only : &
            tensor_converter_factory, &
            create_tensor_builder

    use :: matrix_multiplication_api, only : &
            matrix_multiplication_factory

    use :: batched_matrix_multiplication_dev, only : &
            bmm_factory, &
            batched_matrix_multiplication, &
            simple_loops_bmm_driver

#ifdef use_magma
    use :: magma_bmm_driver_module, only : magma_bmm_driver
#endif

    implicit none
    private

    public :: default_bmm_factory

    type, extends(bmm_factory) :: default_bmm_factory
    contains
        procedure :: create_from_key => create_from_key
        procedure :: get_available_bmm_drivers => get_available_bmm_drivers
        procedure :: build => build
        procedure, private :: count_available_drivers => count_available_drivers
    end type default_bmm_factory

    character(len=*), parameter :: bmm_driver_key = "bmm_driver"
    character(len=*), parameter :: default_bmm_driver = "simple_loops"
    character(len=*), parameter :: prefix = "bmm-"
contains
    subroutine build(this, bmm, options, priorities)
        class(default_bmm_factory), intent(in) :: this
        class(batched_matrix_multiplication), intent(inout) :: bmm
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        type(string), dimension(:), allocatable :: local_priorities
        type(tensor_converter_factory) :: factory

        local_priorities = add_prefix_to_priorities(prefix, priorities)

        select type (bmm)
        type is (simple_loops_bmm_driver)
            call matrix_multiplication_factory%create(bmm%driver, options=options, priorities=local_priorities)
#ifdef use_magma
        type is (magma_bmm_driver)
            call factory%build_c_pointer_converter(bmm%converter, "device")
            call create_tensor_builder(bmm%builder, "device")
#endif
        class default
            error stop "default_bmm_factory::build:Unknown bmm implementation."
        end select
    end subroutine build

    subroutine create_from_key(this, bmm, key)
        class(default_bmm_factory), intent(in) :: this
        class(batched_matrix_multiplication), allocatable, intent(inout) :: bmm
        type(string), intent(in) :: key


        select case (key%char_array)
        case ("simple_loops")
            bmm = simple_loops_bmm_driver()
#ifdef use_magma
        case ("magma")
            bmm = magma_bmm_driver()
#endif
        case default
            error stop "default_bmm_factory::create_from_key:Not a valid bmm driver: "//key%char_array
        end select
    end subroutine create_from_key

    function get_available_bmm_drivers(this) result(drivers)
        class(default_bmm_factory), intent(in) :: this
        type(string), dimension(:), allocatable :: drivers

        integer :: counter

        allocate(drivers(this%count_available_drivers()))

        counter = 1
        drivers(counter) = "simple_loops"
#ifdef use_magma
        counter = counter + 1
        drivers(counter) = "magma"
#endif
    end function get_available_bmm_drivers

    integer function count_available_drivers(this)
        class(default_bmm_factory), intent(in) :: this

        count_available_drivers = 1
#ifdef use_magma
        count_available_drivers = count_available_drivers + 1
#endif
    end function count_available_drivers
end module default_bmm_factory_module
