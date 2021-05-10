module default_contraction_factory_module
    use :: util_api, only : &
            string, &
            dictionary, &
            dictionary_converter, &
            add_prefix_to_priorities

    use :: data_api, only : create_scratch_buffer

    use :: tensor_api, only : &
            create_tensor_builder, &
            tensor_converter_factory

    use :: algorithms_api, only : &
            tensor_contraction, &
            matrix_multiplication_factory, &
            tensor_permute_factory

    use :: tensor_contraction_dev, only : &
            contraction_factory, &
            tc_descriptor, &
            tc_parser, &
            ttgt_descriptor, &
            ttgt_parser, &
            tensor_contraction_loops, &
            tc_ttgt, &
            unbuffered_ttgt_tc_driver, &
            buffered_ttgt_tc_driver

#ifdef use_cutensor
    use :: cutensor_tc_driver_module, only : cutensor_tc_driver
#endif

    implicit none
    private

    public :: default_contraction_factory

    type, extends(contraction_factory) :: default_contraction_factory
    contains
        procedure :: create_from_key => create_from_key
        procedure :: get_available_contraction_drivers => get_available_contraction_drivers
        procedure :: build => build
        procedure, private :: build_ttgt => build_ttgt
        procedure, private :: build_unbuffered_ttgt => build_unbuffered_ttgt
        procedure, private :: build_buffered_ttgt => build_buffered_ttgt
        procedure, private :: count_available_drivers => count_available_drivers
        procedure, private :: get_tc_descriptor => get_tc_descriptor
        procedure, private :: get_ttgt_descriptor => get_ttgt_descriptor
    end type default_contraction_factory

    character(len=*), parameter :: contraction_driver_key = "contraction_driver"
    character(len=*), parameter :: default_contraction_driver = "loops"
contains
    subroutine create_from_key(this, tc, key)
        class(default_contraction_factory), intent(in) :: this
        class(tensor_contraction), allocatable, intent(inout) :: tc
        type(string), intent(in) :: key

        select case (key%char_array)
        case ("loops")
            tc = tensor_contraction_loops()
        case ("unbuffered_ttgt")
            tc = unbuffered_ttgt_tc_driver()
        case ("buffered_ttgt")
            tc = buffered_ttgt_tc_driver()
#ifdef use_cutensor
        case ("cutensor")
            tc = cutensor_tc_driver()
#endif
        case default
            error stop "default_contraction_factory::create_from_key:Not a valid contraction driver: "//key%char_array
        end select
    end subroutine create_from_key

    subroutine build(this, tc, contraction, options, priorities)
        class(default_contraction_factory), intent(in) :: this
        class(tensor_contraction), intent(inout) :: tc
        type(string), intent(in) :: contraction
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        type(tensor_converter_factory) :: factory

        select type (tc)
        type is (tensor_contraction_loops)
            call tc%set_descriptor(this%get_tc_descriptor(contraction))
        type is (unbuffered_ttgt_tc_driver)
            call this%build_unbuffered_ttgt(tc, contraction, options, priorities)
        type is (buffered_ttgt_tc_driver)
            call this%build_buffered_ttgt(tc, contraction, options, priorities)
#ifdef use_cutensor
        type is (cutensor_tc_driver)
            tc%descriptor = this%get_tc_descriptor(contraction)
            tc%converter = factory%get_c_pointer_converter("device")
            call tc%initialize()
#endif
        class default
            error stop "default_contraction_factory::build:Unknown type."
        end select
    end subroutine build

    subroutine build_ttgt(this, tc, contraction, options, priorities)
        class(default_contraction_factory), intent(in) :: this
        class(tc_ttgt), intent(inout) :: tc
        type(string), intent(in) :: contraction
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        call matrix_multiplication_factory%create(tc%mm, options=options, priorities=priorities)
        call tensor_permute_factory%create(tc%permuter, options=options, priorities=priorities)
        call tc%set_descriptor(this%get_ttgt_descriptor(contraction, options, priorities))
        call create_tensor_builder(tc%builder, options=options, priorities=priorities)
    end subroutine build_ttgt

    subroutine build_unbuffered_ttgt(this, tc, contraction, options, priorities)
        class(default_contraction_factory), intent(in) :: this
        type(unbuffered_ttgt_tc_driver), intent(inout) :: tc
        type(string), intent(in) :: contraction
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        type(string), dimension(:), allocatable :: local_priorities

        local_priorities = add_prefix_to_priorities("unbuffered_ttgt-", priorities)
        call this%build_ttgt(tc, contraction, options, local_priorities)
    end subroutine build_unbuffered_ttgt

    subroutine build_buffered_ttgt(this, tc, contraction, options, priorities)
        class(default_contraction_factory), intent(in) :: this
        type(buffered_ttgt_tc_driver), intent(inout) :: tc
        type(string), intent(in) :: contraction
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        type(string), dimension(:), allocatable :: local_priorities

        local_priorities = add_prefix_to_priorities("buffered_ttgt-", priorities)
        call this%build_ttgt(tc, contraction, options, local_priorities)
        call create_scratch_buffer(tc%scratch, options=options, priorities=local_priorities)
        call tc%scratch%initialize()
    end subroutine build_buffered_ttgt

    type(ttgt_descriptor) function get_ttgt_descriptor(this, contraction, options, priorities)
        class(default_contraction_factory), intent(in) :: this
        type(string), intent(in) :: contraction
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        type(ttgt_parser) :: parser
        type(string) :: ttgt_strategy
        type(dictionary_converter) :: conv

        parser = ttgt_parser(contraction)
        ttgt_strategy = conv%to_string(string('ttgt_strategy'), options, priorities, parser%get_default_strategy())

        get_ttgt_descriptor = parser%get_descriptor(ttgt_strategy)
    end function get_ttgt_descriptor

    type(tc_descriptor) function get_tc_descriptor(this, contraction)
        class(default_contraction_factory), intent(in) :: this
        type(string), intent(in) :: contraction

        type(tc_parser) :: parser

        get_tc_descriptor = parser%parse(contraction)
    end function get_tc_descriptor

    function get_available_contraction_drivers(this) result(drivers)
        class(default_contraction_factory), intent(in) :: this
        type(string), dimension(:), allocatable :: drivers

        integer :: counter

        allocate(drivers(this%count_available_drivers()))

        drivers(1) = "loops"
        drivers(2) = "unbuffered_ttgt"
        drivers(3) = "buffered_ttgt"

        counter = 3
#ifdef use_cutensor
        counter = counter + 1
        drivers(counter) = "cutensor"
#endif
    end function get_available_contraction_drivers

    integer function count_available_drivers(this)
        class(default_contraction_factory), intent(in) :: this

        count_available_drivers = 3
#ifdef use_cutensor
        count_available_drivers = count_available_drivers + 1
#endif
    end function count_available_drivers
end module default_contraction_factory_module
