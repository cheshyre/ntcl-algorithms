module permute_factory_module
    use :: util_api, only : &
            string, &
            dictionary, &
            dictionary_converter

    use :: tensor_permute_module, only : tensor_permute

    implicit none
    private

    public :: permute_factory

    type, abstract :: permute_factory
    contains
        procedure :: get => get
        procedure :: create => create
        procedure(create_interface), deferred :: create_from_key
        procedure(get_available_interface), deferred :: get_available_permute_drivers
        procedure(build_interface), deferred :: build
    end type permute_factory

    abstract interface
        subroutine create_interface(this, permute, key)
            import :: permute_factory
            import :: tensor_permute
            import :: string

            class(permute_factory), intent(in) :: this
            class(tensor_permute), allocatable, intent(inout) :: permute
            type(string), intent(in) :: key
        end subroutine create_interface

        subroutine build_interface(this, driver, options, priorities)
            import :: permute_factory
            import :: tensor_permute
            import :: dictionary
            import :: string

            class(permute_factory), intent(in) :: this
            class(tensor_permute), intent(inout) :: driver
            type(dictionary), intent(in), optional :: options
            type(string), dimension(:), intent(in), optional :: priorities
        end subroutine build_interface

        function get_available_interface(this) result(drivers)
            import :: permute_factory
            import :: string

            class(permute_factory), intent(in) :: this
            type(string), dimension(:), allocatable :: drivers
        end function get_available_interface
    end interface

    character(len=*), parameter :: permute_driver_key = "permute_driver"
    character(len=*), parameter :: default_permute_driver = "loops"
contains
    function get(this, driver, options, priorities) result(permute)
        class(permute_factory), intent(in) :: this
        character(len=*), intent(in), optional :: driver
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities
        class(tensor_permute), allocatable :: permute

        call this%create(permute, driver, options, priorities)
    end function get

    subroutine create(this, permute, driver, options, priorities)
        class(permute_factory), intent(in) :: this
        class(tensor_permute), allocatable, intent(inout) :: permute
        character(len=*), intent(in), optional :: driver
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        type(string) :: key
        type(dictionary_converter) :: conv

        if ( present(driver) ) then
            key = driver
        else
            key = conv%to_string(permute_driver_key, options, priorities, default_permute_driver)
        end if
        call this%create_from_key(permute, key)
        call this%build(permute, options, priorities)
    end subroutine create
end module permute_factory_module
