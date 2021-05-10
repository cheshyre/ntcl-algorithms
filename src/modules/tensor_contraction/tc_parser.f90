module tc_parser_module
    use :: util_api, only : string

    use :: tc_descriptor_module, only : tc_descriptor

    implicit none
    private

    public :: tc_parser

    type :: tc_parser
        type(string) :: c, a, b, unique_indices
    contains
        procedure :: parse => parse
        procedure :: cleanup => cleanup
        procedure :: get_tc_descriptor => get_tc_descriptor
        procedure, private, nopass :: find_sides => find_sides
        procedure, private, nopass :: get_index_chars => get_index_chars
        procedure, private :: find_a_b => find_a_b
        procedure, private :: find_c_a_b => find_c_a_b
        procedure, private, nopass :: get_unique_index_chars => get_unique_index_chars
        procedure, private, nopass :: get_indices_from_chars => get_indices_from_chars
    end type tc_parser

    character(len=1), dimension(2), parameter :: equal_delimiters = ["=", ":"]
    character(len=1), dimension(1), parameter :: product_delimiters = ["*"]

    interface tc_parser
        module procedure constructor_empty
        module procedure constructor
    end interface tc_parser
contains
    function constructor_empty() result(this)
        type(tc_parser) :: this

        this%c = ''
        this%a = ''
        this%b = ''
        this%unique_indices = ''
    end function constructor_empty

    function constructor(string_in) result(this)
        type(string), intent(in) :: string_in
        type(tc_parser) :: this

        this = tc_parser()

        call this%find_c_a_b(string_in, this%c, this%a, this%b)
        this%unique_indices = this%get_unique_index_chars(this%c, this%a, this%b)
    end function constructor

    function get_tc_descriptor(this) result(tcd)
        class(tc_parser), intent(in) :: this
        type(tc_descriptor) :: tcd

        tcd = tc_descriptor( this%get_indices_from_chars(this%c, this%unique_indices), &
                this%get_indices_from_chars(this%a, this%unique_indices), &
                this%get_indices_from_chars(this%b, this%unique_indices) )
    end function get_tc_descriptor

    function parse(this, string_in) result(tcd)
        class(tc_parser), intent(in) :: this
        type(string), intent(in) :: string_in
        type(tc_descriptor) :: tcd

        type(string) :: unique_indices
        type(string) :: c, a, b

        call this%find_c_a_b(string_in, c, a, b)
        unique_indices = this%get_unique_index_chars(c, a, b)

        tcd = tc_descriptor( this%get_indices_from_chars(c, unique_indices), &
                this%get_indices_from_chars(a, unique_indices), &
                this%get_indices_from_chars(b, unique_indices) )
    end function parse

    function get_indices_from_chars(chars, all_chars) result(indices)
        type(string), intent(in) :: chars, all_chars
        integer, dimension(:), allocatable :: indices

        integer :: idx

        allocate(indices(chars%length))
        do idx = 1, chars%length
            indices(idx) = index( all_chars%as_char_array(), chars%get(idx) )
        end do
    end function get_indices_from_chars

    subroutine find_sides(string_in, dls, left, right)
        type(string), intent(in) :: string_in
        character(len=1), dimension(:), intent(in) :: dls
        type(string), intent(out) :: left, right

        type(string), dimension(:), allocatable :: sides
        integer :: idx
        character(len=1) :: ch

        do idx = 1, len(dls)
            ch = dls(idx)
            sides = string_in%split(ch)
            if ( size(sides) == 2 ) exit
        end do

        if ( size(sides) /= 2 ) &
                error stop "tc_parser::find_sides:Expression must have right and left hand sides."
        left = sides(1); right = sides(2)

        do idx = 1, size(sides)
            call sides(idx)%cleanup()
        end do
        deallocate(sides)
    end subroutine find_sides

    type(string) function get_index_chars(string_in)
        type(string), intent(in) :: string_in

        type(string), dimension(:), allocatable :: splits
        integer :: idx

        splits = string_in%find_substrings("(",")")
        get_index_chars = string("")
        if ( size(splits)==1 ) get_index_chars = splits(1)%char_remove(',')

        if ( size(splits) > 1) &
                error stop "tc_parser::get_index_chars:String contains multiple index blocks."
        do idx = 1, size(splits)
            call splits(idx)%cleanup()
        end do
        deallocate(splits)
    end function get_index_chars

    subroutine find_a_b(this, string_in, a, b)
        class(tc_parser), intent(in) :: this
        type(string), intent(in) :: string_in
        type(string), intent(out) :: a, b

        type(string) :: left, right

        call this%find_sides(string_in, ["*"], left, right)
        a = this%get_index_chars(left)
        b = this%get_index_chars(right)
    end subroutine find_a_b

    subroutine find_c_a_b(this, string_in, c, a, b)
        class(tc_parser), intent(in) :: this
        type(string), intent(in) :: string_in
        type(string), intent(out) :: c, a, b

        type(string) :: left, right

        call this%find_sides(string_in, ["=",":"], left, right)
        call this%find_a_b(right, a, b)
        c = get_index_chars(left)
    end subroutine find_c_a_b

    type(string) function get_unique_index_chars(c, a, b)
        type(string), intent(in) :: c, a, b

        get_unique_index_chars = c
        get_unique_index_chars = get_unique_index_chars%cat(a)
        get_unique_index_chars = get_unique_index_chars%cat(b)
        get_unique_index_chars = get_unique_index_chars%unique_chars()
    end function get_unique_index_chars


    subroutine cleanup(this)
        class(tc_parser), intent(inout) :: this

        call this%c%cleanup()
        call this%a%cleanup()
        call this%b%cleanup()
        call this%unique_indices%cleanup()
    end subroutine cleanup
end module tc_parser_module
