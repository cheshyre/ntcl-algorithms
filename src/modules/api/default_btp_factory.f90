module default_btp_factory_module
    use :: util_api, only : &
            string, &
            dictionary, &
            add_prefix_to_priorities

    use :: tensor_permute_api, only : &
            tensor_permute_factory

    use :: batched_tensor_permute_dev, only : &
            btp_factory, &
            batched_tensor_permute, &
            simple_loops_btp_driver

    implicit none
    private

    public :: default_btp_factory

    type, extends(btp_factory) :: default_btp_factory
    contains
        procedure :: create_from_key => create_from_key
        procedure :: get_available_btp_drivers => get_available_btp_drivers
        procedure :: build => build
        procedure, private :: count_available_drivers => count_available_drivers
    end type default_btp_factory

    character(len=*), parameter :: btp_driver_key = "btp_driver"
    character(len=*), parameter :: default_btp_driver = "simple_loops"
    character(len=*), parameter :: prefix = "btp-"
contains
    subroutine build(this, btp, options, priorities)
        class(default_btp_factory), intent(in) :: this
        class(batched_tensor_permute), intent(inout) :: btp
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        type(string), dimension(:), allocatable :: local_priorities

        local_priorities = add_prefix_to_priorities(prefix, priorities)

        select type (btp)
        type is (simple_loops_btp_driver)
            call tensor_permute_factory%create(btp%driver, options=options, priorities=local_priorities)
        class default
            error stop "default_btp_factory::build:Unknown btp implementation."
        end select
    end subroutine build

    subroutine create_from_key(this, btp, key)
        class(default_btp_factory), intent(in) :: this
        class(batched_tensor_permute), allocatable, intent(inout) :: btp
        type(string), intent(in) :: key

        select case (key%char_array)
        case ("simple_loops")
            btp = simple_loops_btp_driver()
        case default
            error stop "default_btp_factory::create_from_key:Not a valid btp driver: "//key%char_array
        end select
    end subroutine create_from_key

    function get_available_btp_drivers(this) result(drivers)
        class(default_btp_factory), intent(in) :: this
        type(string), dimension(:), allocatable :: drivers

        allocate(drivers(this%count_available_drivers()))

        drivers(1) = "simple_loops"
    end function get_available_btp_drivers

    integer function count_available_drivers(this)
        class(default_btp_factory), intent(in) :: this

        count_available_drivers = 1
    end function count_available_drivers
end module default_btp_factory_module
