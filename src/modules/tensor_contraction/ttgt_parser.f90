module ttgt_parser_module
    use :: util_api, only : string

    use :: tc_parser_module, only : tc_parser
    use :: ttgt_descriptor_module, only : ttgt_descriptor

    implicit none
    private

    public :: ttgt_parser

    type :: ttgt_parser
        type(string) :: contraction
        type(tc_parser) :: parser
    contains
        procedure :: get_default_strategy => get_default_strategy
        procedure :: get_descriptor => get_descriptor
        procedure :: find_common_indices => find_common_indices
        procedure :: get_block => get_block
        procedure :: get_indices_from_chars => get_indices_from_chars
        procedure :: is_equivalent => is_equivalent
        procedure :: cleanup => cleanup
    end type ttgt_parser

    interface ttgt_parser
        module procedure constructor_empty
        module procedure constructor
    end interface ttgt_parser
contains
    function constructor_empty() result(this)
        type(ttgt_parser) :: this

        this%contraction = ''
        this%parser = tc_parser()
    end function constructor_empty

    function constructor(contraction) result(this)
        type(string), intent(in) :: contraction
        type(ttgt_parser) :: this

        this = ttgt_parser()
        this%contraction = contraction
        this%parser = tc_parser(contraction)
    end function constructor

    type(string) function get_default_strategy(this)
        class(ttgt_parser), intent(in) :: this

        type(string) :: m, n, k

        m = this%find_common_indices(this%parser%c, this%parser%a)
        n = this%find_common_indices(this%parser%c, this%parser%b)
        k = this%find_common_indices(this%parser%a, this%parser%b)

        get_default_strategy = "c"//this%get_block(m,n)//"=a"//this%get_block(m,k)//"*b"//this%get_block(k,n)
    end function get_default_strategy

    type(string) function find_common_indices(this, left, right)
        class(ttgt_parser), intent(in) :: this
        type(string), intent(in) :: left, right

        integer :: idx
        character :: ch

        find_common_indices = ''
        do idx = 1, left%length
            ch = left%get(idx)
            if ( index(right%char_array, ch) /= 0 ) &
                    find_common_indices = find_common_indices%cat(ch)
        end do
    end function find_common_indices

    function get_block(this, left, right) result(ablock)
        class(ttgt_parser), intent(in) :: this
        type(string), intent(in) :: left, right
        character(len=:), allocatable :: ablock

        type(string) :: temp

        if (left%length==0 .and. right%length==0) then
            ablock = ''
        else if ( left%length > 0 .and. right%length==0) then
            ablock = "("//left%char_array//")"
        else if ( left%length==0 .and. right%length>0) then
            ablock = "("//right%char_array//")"
        else
            ablock = "("//left%char_array//","//right%char_array//")"
        end if
    end function get_block

    function get_indices_from_chars(this, index_chars, all_chars) result(indices)
        class(ttgt_parser), intent(in) :: this
        type(string), intent(in) :: index_chars, all_chars
        integer, dimension(:), allocatable :: indices

        integer :: idx

        allocate(indices(index_chars%length))
        do idx = 1, index_chars%length
            indices(idx) = index(all_chars%char_array, index_chars%get(idx))
        end do
    end function get_indices_from_chars

    type(ttgt_descriptor) function get_descriptor(this, strategy)
        class(ttgt_parser), intent(in) :: this
        type(string), intent(in) :: strategy

        type(tc_parser) :: strategy_parser
        type(string) :: m, n, k
        integer, dimension(:), allocatable :: mi, ni, ki
        logical :: switch_ab

        strategy_parser = tc_parser(strategy)

        m = this%find_common_indices(strategy_parser%c, strategy_parser%a)
        n = this%find_common_indices(strategy_parser%c, strategy_parser%b)
        k = this%find_common_indices(strategy_parser%a, strategy_parser%b)

        mi = this%get_indices_from_chars(m, this%parser%unique_indices)
        ni = this%get_indices_from_chars(n, this%parser%unique_indices)
        ki = this%get_indices_from_chars(k, this%parser%unique_indices)

        switch_ab = .not. this%is_equivalent(strategy_parser%a, this%parser%a)
        get_descriptor = ttgt_descriptor(this%parser%get_tc_descriptor(), mi, ni, ki, switch_ab)
    end function get_descriptor

    logical function is_equivalent(this, a, b)
        class(ttgt_parser), intent(in) :: this
        type(string), intent(in) :: a, b

        integer :: idx

        is_equivalent = .false.

        if ( a%length /= b%length ) return

        do idx = 1, a%length
            if ( index(b%char_array, a%get(idx)) == 0 ) return
        end do
        is_equivalent = .true.
    end function is_equivalent

    subroutine cleanup(this)
        class(ttgt_parser), intent(inout) :: this

        call this%contraction%cleanup()
        call this%parser%cleanup()
    end subroutine cleanup
end module ttgt_parser_module
