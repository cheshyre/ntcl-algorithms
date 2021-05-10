module contraction_factory_module
    use :: util_api, only : &
            string, &
            dictionary, &
            dictionary_converter

    use :: tensor_contraction_module, only : tensor_contraction

    implicit none
    private

    public :: contraction_factory

    type, abstract :: contraction_factory
    contains
        procedure :: get => get
        procedure :: create => create
        procedure(create_interface), deferred :: create_from_key
        procedure(get_available_interface), deferred :: get_available_contraction_drivers
        procedure(build_interface), deferred :: build
    end type contraction_factory

    abstract interface
        subroutine create_interface(this, tc, key)
            import :: contraction_factory
            import :: tensor_contraction
            import :: string

            class(contraction_factory), intent(in) :: this
            class(tensor_contraction), allocatable, intent(inout) :: tc
            type(string), intent(in) :: key
        end subroutine create_interface

        subroutine build_interface(this, tc, contraction, options, priorities)
            import :: contraction_factory
            import :: tensor_contraction
            import :: string
            import :: dictionary

            class(contraction_factory), intent(in) :: this
            class(tensor_contraction), allocatable, intent(inout) :: tc
            type(string), intent(in) :: contraction
            type(dictionary), intent(in), optional :: options
            type(string), dimension(:), intent(in), optional :: priorities
        end subroutine build_interface

        function get_available_interface(this) result(drivers)
            import :: contraction_factory
            import :: string

            class(contraction_factory), intent(in) :: this
            type(string), dimension(:), allocatable :: drivers
        end function get_available_interface
    end interface

    character(len=*), parameter :: contraction_driver_key = "contraction_driver"
    character(len=*), parameter :: default_contraction_driver = "loops"
contains
    function get(this, contraction, driver, options, priorities) result(tc)
        class(contraction_factory), intent(in) :: this
        character(len=*), intent(in), optional :: contraction
        character(len=*), intent(in), optional :: driver
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities
        class(tensor_contraction), allocatable :: tc

        call this%create(tc, contraction, driver, options, priorities)
    end function get

    subroutine create(this, tc, contraction, driver, options, priorities)
        class(contraction_factory), intent(in) :: this
        class(tensor_contraction), allocatable, intent(inout) :: tc
        character(len=*), intent(in), optional :: contraction
        character(len=*), intent(in), optional :: driver
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        type(string) :: key
        type(dictionary_converter) :: conv

        if ( present(driver) ) then
            key = driver
        else
            key = conv%to_string(contraction_driver_key, options, priorities, default_contraction_driver)
        end if
        call this%create_from_key(tc, key)
        call this%build(tc, string(contraction), options, priorities)
    end subroutine create
end module contraction_factory_module
