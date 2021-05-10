module btp_factory_module
    use :: util_api, only : &
            string, &
            dictionary, &
            dictionary_converter

    use :: batched_tensor_permute_module, only : batched_tensor_permute

    implicit none
    private

    public :: btp_factory

    type, abstract :: btp_factory
    contains
        procedure :: get => get
        procedure :: create => create
        procedure(create_interface), deferred :: create_from_key
        procedure(build_interface), deferred :: build
        procedure(get_available_interface), deferred :: get_available_btp_drivers
    end type btp_factory

    abstract interface
        subroutine create_interface(this, btp, key)
            import :: btp_factory
            import :: batched_tensor_permute
            import :: string

            class(btp_factory), intent(in) :: this
            class(batched_tensor_permute), allocatable, intent(inout) :: btp
            type(string), intent(in) :: key
        end subroutine create_interface

        subroutine build_interface(this, btp, options, priorities)
            import :: btp_factory
            import :: batched_tensor_permute
            import :: dictionary
            import :: string

            class(btp_factory), intent(in) :: this
            class(batched_tensor_permute), allocatable, intent(inout) :: btp
            type(dictionary), intent(in), optional :: options
            type(string), dimension(:), intent(in), optional :: priorities
        end subroutine build_interface

        function get_available_interface(this) result(drivers)
            import :: btp_factory
            import :: string

            class(btp_factory), intent(in) :: this
            type(string), dimension(:), allocatable :: drivers
        end function get_available_interface
    end interface

    character(len=*), parameter :: btp_driver_key = "btp_driver"
    character(len=*), parameter :: default_btp_driver = "simple_loops"
contains
    function get(this, driver, options, priorities) result(btp)
        class(btp_factory), intent(in) :: this
        character(len=*), intent(in), optional :: driver
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities
        class(batched_tensor_permute), allocatable :: btp

        call this%create(btp, driver, options, priorities)
    end function get

    subroutine create(this, btp, driver, options, priorities)
        class(btp_factory), intent(in) :: this
        class(batched_tensor_permute), allocatable, intent(inout) :: btp
        character(len=*), intent(in), optional :: driver
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        type(string) :: key
        type(dictionary_converter) :: conv

        if ( present(driver) ) then
            key = driver
        else
            key = conv%to_string(btp_driver_key, options, priorities, default_btp_driver)
        end if
        call this%create_from_key(btp, key)

        call this%build(btp, options, priorities)
    end subroutine create
end module btp_factory_module
