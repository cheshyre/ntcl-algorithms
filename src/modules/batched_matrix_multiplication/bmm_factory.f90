module bmm_factory_module
    use :: util_api, only : &
            string, &
            dictionary, &
            dictionary_converter

    use :: batched_matrix_multiplication_module, only : batched_matrix_multiplication

    implicit none
    private

    public :: bmm_factory

    type, abstract :: bmm_factory
    contains
        procedure :: get => get
        procedure :: create => create
        procedure(create_interface), deferred :: create_from_key
        procedure(build_interface), deferred :: build
        procedure(get_available_interface), deferred :: get_available_bmm_drivers
    end type bmm_factory

    abstract interface
        subroutine create_interface(this, bmm, key)
            import :: bmm_factory
            import :: batched_matrix_multiplication
            import :: string

            class(bmm_factory), intent(in) :: this
            class(batched_matrix_multiplication), allocatable, intent(inout) :: bmm
            type(string), intent(in) :: key
        end subroutine create_interface

        subroutine build_interface(this, bmm, options, priorities)
            import :: bmm_factory
            import :: batched_matrix_multiplication
            import :: dictionary
            import :: string

            class(bmm_factory), intent(in) :: this
            class(batched_matrix_multiplication), allocatable, intent(inout) :: bmm
            type(dictionary), intent(in), optional :: options
            type(string), dimension(:), intent(in), optional :: priorities
        end subroutine build_interface

        function get_available_interface(this) result(drivers)
            import :: bmm_factory
            import :: string

            class(bmm_factory), intent(in) :: this
            type(string), dimension(:), allocatable :: drivers
        end function get_available_interface
    end interface

    character(len=*), parameter :: bmm_driver_key = "bmm_driver"
    character(len=*), parameter :: default_bmm_driver = "simple_loops"
contains
    function get(this, driver, options, priorities) result(bmm)
        class(bmm_factory), intent(in) :: this
        character(len=*), intent(in), optional :: driver
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities
        class(batched_matrix_multiplication), allocatable :: bmm

        call this%create(bmm, driver, options, priorities)
    end function get

    subroutine create(this, bmm, driver, options, priorities)
        class(bmm_factory), intent(in) :: this
        class(batched_matrix_multiplication), allocatable, intent(inout) :: bmm
        character(len=*), intent(in), optional :: driver
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        type(string) :: key
        type(dictionary_converter) :: conv

        if ( present(driver) ) then
            key = driver
        else
            key = conv%to_string(bmm_driver_key, options, priorities, default_bmm_driver)
        end if
        call this%create_from_key(bmm, key)

        call this%build(bmm, options, priorities)
    end subroutine create
end module bmm_factory_module
