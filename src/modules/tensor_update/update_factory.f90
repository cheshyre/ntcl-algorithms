module update_factory_module
    use :: util_api, only : &
            string, &
            dictionary, &
            dictionary_converter

    use :: scalar_inline_update_module, only : scalar_inline_update

    implicit none
    private

    public :: update_factory

    type, abstract :: update_factory
    contains
        generic :: create => &
                create_siu
        generic :: create_from_key => &
                create_siu_from_key

        procedure :: get_siu => get_siu
        procedure :: create_siu => create_siu
        procedure(create_siu_interface), deferred :: create_siu_from_key
        procedure(get_available_interface), deferred :: get_available_siu_drivers
    end type update_factory

    abstract interface
        subroutine create_siu_interface(this, update, key)
            import :: update_factory
            import :: scalar_inline_update
            import :: string

            class(update_factory), intent(in) :: this
            class(scalar_inline_update), allocatable, intent(inout) :: update
            type(string), intent(in) :: key
        end subroutine create_siu_interface

        function get_available_interface(this) result(drivers)
            import :: update_factory
            import :: string

            class(update_factory), intent(in) :: this
            type(string), dimension(:), allocatable :: drivers
        end function get_available_interface
    end interface

    character(len=*), parameter :: siu_driver_key = "siu_driver"
    character(len=*), parameter :: default_siu_driver = "fortran"
contains
    function get_siu(this, driver, options, priorities) result(update)
        class(update_factory), intent(in) :: this
        character(len=*), intent(in), optional :: driver
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities
        class(scalar_inline_update), allocatable :: update

        call this%create(update, driver, options, priorities)
    end function get_siu

    subroutine create_siu(this, update, driver, options, priorities)
        class(update_factory), intent(in) :: this
        class(scalar_inline_update), allocatable, intent(inout) :: update
        character(len=*), intent(in), optional :: driver
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        type(string) :: key
        type(dictionary_converter) :: conv

        if ( present(driver) ) then
            key = driver
        else
            key = conv%to_string(siu_driver_key, options, priorities, default_siu_driver)
        end if
        call this%create_from_key(update, key)
    end subroutine create_siu
end module update_factory_module
