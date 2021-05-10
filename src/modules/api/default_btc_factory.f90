module default_btc_factory_module
    use :: util_api, only : &
            string, &
            dictionary, &
            add_prefix_to_priorities

    use :: tensor_contraction_api, only : &
            tensor_contraction_factory

    use :: batched_tensor_contraction_dev, only : &
            btc_factory, &
            batched_tensor_contraction, &
            simple_loops_btc_driver

    implicit none
    private

    public :: default_btc_factory

    type, extends(btc_factory) :: default_btc_factory
    contains
        procedure :: create_from_key => create_from_key
        procedure :: get_available_btc_drivers => get_available_btc_drivers
        procedure :: build => build
        procedure, private :: count_available_drivers => count_available_drivers
    end type default_btc_factory

    character(len=*), parameter :: btc_driver_key = "btc_driver"
    character(len=*), parameter :: default_btc_driver = "simple_loops"
    character(len=*), parameter :: prefix = "btc-"
contains
    subroutine build(this, btc, contraction, options, priorities)
        class(default_btc_factory), intent(in) :: this
        class(batched_tensor_contraction), intent(inout) :: btc
        type(string), intent(in) :: contraction
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        type(string), dimension(:), allocatable :: local_priorities

        local_priorities = add_prefix_to_priorities(prefix, priorities)

        select type (btc)
        type is (simple_loops_btc_driver)
            call tensor_contraction_factory%create(btc%driver, contraction%char_array, &
                    options=options, priorities=local_priorities)
        class default
            error stop "default_btc_factory::build:Unknown btc implementation."
        end select
    end subroutine build

    subroutine create_from_key(this, btc, key)
        class(default_btc_factory), intent(in) :: this
        class(batched_tensor_contraction), allocatable, intent(inout) :: btc
        type(string), intent(in) :: key

        select case (key%char_array)
        case ("simple_loops")
            btc = simple_loops_btc_driver()
        case default
            error stop "default_btc_factory::create_from_key:Not a valid btc driver: "//key%char_array
        end select
    end subroutine create_from_key

    function get_available_btc_drivers(this) result(drivers)
        class(default_btc_factory), intent(in) :: this
        type(string), dimension(:), allocatable :: drivers

        allocate(drivers(this%count_available_drivers()))

        drivers(1) = "simple_loops"
    end function get_available_btc_drivers

    integer function count_available_drivers(this)
        class(default_btc_factory), intent(in) :: this

        count_available_drivers = 1
    end function count_available_drivers
end module default_btc_factory_module
