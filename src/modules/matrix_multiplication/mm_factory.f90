module mm_factory_module
    use :: util_api, only : &
            string, &
            dictionary, &
            dictionary_converter

    use :: matrix_multiplication_module, only : matrix_multiplication

    implicit none
    private

    public :: mm_factory

    type, abstract :: mm_factory
    contains
        procedure :: get => get
        procedure :: create => create
        procedure(create_interface), deferred :: create_from_key
        procedure(get_available_interface), deferred :: get_available_mm_drivers
    end type mm_factory

    abstract interface
        subroutine create_interface(this, mm, key)
            import :: mm_factory
            import :: matrix_multiplication
            import :: string

            class(mm_factory), intent(in) :: this
            class(matrix_multiplication), allocatable, intent(inout) :: mm
            type(string), intent(in) :: key
        end subroutine create_interface

        function get_available_interface(this) result(drivers)
            import :: mm_factory
            import :: string

            class(mm_factory), intent(in) :: this
            type(string), dimension(:), allocatable :: drivers
        end function get_available_interface
    end interface

    character(len=*), parameter :: mm_driver_key = "mm_driver"
    character(len=*), parameter :: default_mm_driver = "intrinsic"
contains
    function get(this, driver, options, priorities) result(mm)
        class(mm_factory), intent(in) :: this
        character(len=*), intent(in), optional :: driver
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities
        class(matrix_multiplication), allocatable :: mm

        call this%create(mm, driver, options, priorities)
    end function get

    subroutine create(this, mm, driver, options, priorities)
        class(mm_factory), intent(in) :: this
        class(matrix_multiplication), allocatable, intent(inout) :: mm
        character(len=*), intent(in), optional :: driver
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        type(string) :: key
        type(dictionary_converter) :: conv

        if ( present(driver) ) then
            key = driver
        else
            key = conv%to_string(mm_driver_key, options, priorities, default_mm_driver)
        end if
        call this%create_from_key(mm, key)
    end subroutine create
end module mm_factory_module
