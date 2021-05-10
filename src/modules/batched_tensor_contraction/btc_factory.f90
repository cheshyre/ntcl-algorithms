module btc_factory_module
    use :: util_api, only : &
            string, &
            dictionary, &
            dictionary_converter

    use :: batched_tensor_contraction_module, only : batched_tensor_contraction

    implicit none
    private

    public :: btc_factory

    type, abstract :: btc_factory
    contains
        procedure :: get => get
        procedure :: create => create
        procedure(create_interface), deferred :: create_from_key
        procedure(build_interface), deferred :: build
        procedure(get_available_interface), deferred :: get_available_btc_drivers
    end type btc_factory

    abstract interface
        subroutine create_interface(this, btc, key)
            import :: btc_factory
            import :: batched_tensor_contraction
            import :: string

            class(btc_factory), intent(in) :: this
            class(batched_tensor_contraction), allocatable, intent(inout) :: btc
            type(string), intent(in) :: key
        end subroutine create_interface

        subroutine build_interface(this, btc, contraction, options, priorities)
            import :: btc_factory
            import :: batched_tensor_contraction
            import :: dictionary
            import :: string

            class(btc_factory), intent(in) :: this
            class(batched_tensor_contraction), allocatable, intent(inout) :: btc
            type(string), intent(in) :: contraction
            type(dictionary), intent(in), optional :: options
            type(string), dimension(:), intent(in), optional :: priorities
        end subroutine build_interface

        function get_available_interface(this) result(drivers)
            import :: btc_factory
            import :: string

            class(btc_factory), intent(in) :: this
            type(string), dimension(:), allocatable :: drivers
        end function get_available_interface
    end interface

    character(len=*), parameter :: btc_driver_key = "btc_driver"
    character(len=*), parameter :: default_btc_driver = "simple_loops"
contains
    function get(this, contraction, driver, options, priorities) result(btc)
        class(btc_factory), intent(in) :: this
        character(len=*), intent(in), optional :: contraction
        character(len=*), intent(in), optional :: driver
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities
        class(batched_tensor_contraction), allocatable :: btc

        call this%create(btc, contraction, driver, options, priorities)
    end function get

    subroutine create(this, btc, contraction, driver, options, priorities)
        class(btc_factory), intent(in) :: this
        class(batched_tensor_contraction), allocatable, intent(inout) :: btc
        character(len=*), intent(in) :: contraction
        character(len=*), intent(in), optional :: driver
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        type(string) :: key
        type(dictionary_converter) :: conv

        if ( present(driver) ) then
            key = driver
        else
            key = conv%to_string(btc_driver_key, options, priorities, default_btc_driver)
        end if
        call this%create_from_key(btc, key)

        call this%build(btc, string(contraction), options, priorities)
    end subroutine create
end module btc_factory_module
