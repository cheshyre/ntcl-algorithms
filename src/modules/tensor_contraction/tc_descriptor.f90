module tc_descriptor_module
    use :: util_api, only : string

    implicit none
    private

    public :: tc_descriptor

    type :: tc_descriptor
        integer :: num_unique_indices
        integer :: num_external_indices, num_internal_indices
        integer, dimension(:), allocatable :: c_indices, a_indices, b_indices
    contains
        procedure :: is_valid => is_valid
        procedure :: allocate_arrays => allocate_arrays
        procedure :: deallocate_arrays => deallocate_arrays
        procedure :: get_inverse => get_inverse
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type tc_descriptor

    interface tc_descriptor
        module procedure constructor_empty
        module procedure constructor
    end interface tc_descriptor

contains
    function constructor_empty() result(this)
        type(tc_descriptor) :: this

        call this%clear()
    end function constructor_empty

    function constructor(c_inds, a_inds, b_inds) result(this)
        integer, dimension(:), intent(in) :: c_inds, a_inds, b_inds
        type(tc_descriptor) :: this

        if ( .not. this%is_valid(c_inds, a_inds, b_inds) ) then
            error stop "tc_descriptor::constructor:Not valid."
        end if

        this = tc_descriptor()
        call this%allocate_arrays( size(c_inds), size(a_inds), size(b_inds) )

        this%c_indices(:) = c_inds(:)
        this%a_indices(:) = a_inds(:)
        this%b_indices(:) = b_inds(:)

        this%num_unique_indices = maxval( [maxval(c_inds), maxval(a_inds), maxval(b_inds)] )
        this%num_external_indices = size(c_inds)
        this%num_internal_indices = this%num_unique_indices - this%num_external_indices
    end function constructor

    function get_inverse(this, indices) result(inverse)
        class(tc_descriptor), intent(in) :: this
        integer, dimension(:), intent(in) :: indices
        integer, dimension(:), allocatable :: inverse

        integer :: idx

        allocate(inverse(this%num_unique_indices))
        inverse = 0
        do idx = 1, size(indices)
            inverse(indices(idx)) = idx
        end do
    end function get_inverse

    logical function all_indexes_are_used(c_inds, a_inds, b_inds)
        integer, dimension(:), intent(in) :: c_inds, a_inds, b_inds

        integer :: idx, num_indices
        logical, dimension(:), allocatable :: index_used

        num_indices = maxval( [maxval(c_inds), maxval(a_inds), maxval(b_inds)] )
        allocate(index_used(num_indices))

        index_used = .false.
        do idx = 1, size(a_inds)
            index_used(a_inds(idx)) = .true.
        end do
        do idx = 1, size(b_inds)
            index_used(b_inds(idx)) = .true.
        end do
        do idx = 1, size(c_inds)
            index_used(c_inds(idx)) = .true.
        end do

        all_indexes_are_used = all(index_used)
        deallocate(index_used)
    end function all_indexes_are_used

    integer function get_number_of_times_used(inds, idx)
        integer, dimension(:), intent(in) :: inds
        integer, intent(in) :: idx

        integer :: i

        get_number_of_times_used = 0
        do i = 1, size(inds)
            if (inds(i) == idx ) get_number_of_times_used = get_number_of_times_used + 1
        end do
    end function get_number_of_times_used

    logical function external_indexes_are_used_exactly_once( c_inds, a_inds, b_inds)
        integer, dimension(:), intent(in) :: c_inds, a_inds, b_inds

        integer :: i, ntimes

        external_indexes_are_used_exactly_once = .false.
        do i = 1, size(c_inds)
            ntimes = get_number_of_times_used(a_inds, c_inds(i)) + get_number_of_times_used(b_inds, c_inds(i))
            if ( ntimes /= 1 ) return
        end do
        external_indexes_are_used_exactly_once = .true.
    end function external_indexes_are_used_exactly_once

    logical function has_index(inds, idx)
        integer, dimension(:), intent(in) :: inds
        integer, intent(in) :: idx

        integer :: i

        has_index = .true.
        do i = 1, size(inds)
            if ( inds(i) == idx ) return
        end do
        has_index = .false.
    end function has_index

    logical function internal_indexes_are_used_exactly_twice(c_inds, a_inds, b_inds)
        integer, dimension(:), intent(in) :: c_inds, a_inds, b_inds

        integer :: i, times_used

        internal_indexes_are_used_exactly_twice = .false.

        do i = 1, size(a_inds)
            if ( has_index(c_inds, a_inds(i)) ) cycle ! external
            times_used = get_number_of_times_used(a_inds, a_inds(i)) + get_number_of_times_used(b_inds, a_inds(i))
            if ( times_used /= 2 ) return
        end do

        do i = 1, size(b_inds)
            if ( has_index(c_inds, b_inds(i)) ) cycle
            times_used = get_number_of_times_used(a_inds, b_inds(i)) + get_number_of_times_used(b_inds, b_inds(i))
            if ( times_used /= 2 ) return
        end do
        internal_indexes_are_used_exactly_twice = .true.
    end function internal_indexes_are_used_exactly_twice

    logical function is_valid(this, c_inds, a_inds, b_inds)
        class(tc_descriptor), intent(in) :: this
        integer, dimension(:) :: c_inds, a_inds, b_inds

        is_valid = all_indexes_are_used(c_inds, a_inds, b_inds) .and. &
            external_indexes_are_used_exactly_once(c_inds, a_inds, b_inds) .and. &
            internal_indexes_are_used_exactly_twice(c_inds, a_inds, b_inds)
    end function is_valid

    subroutine allocate_arrays(this, c_size, a_size, b_size)
        class(tc_descriptor), intent(inout) :: this
        integer, intent(in) :: c_size, a_size, b_size

        call this%deallocate_arrays()

        allocate(this%c_indices( c_size ))
        allocate(this%a_indices( a_size ))
        allocate(this%b_indices( b_size ))
    end subroutine allocate_arrays

    subroutine deallocate_arrays(this)
        class(tc_descriptor), intent(inout) :: this

        if ( allocated(this%c_indices) ) deallocate(this%c_indices)
        if ( allocated(this%a_indices) ) deallocate(this%a_indices)
        if ( allocated(this%b_indices) ) deallocate(this%b_indices)
    end subroutine deallocate_arrays

    subroutine cleanup(this)
        class(tc_descriptor), intent(inout) :: this

        call this%deallocate_arrays()
        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(tc_descriptor), intent(inout) :: this

        this%num_unique_indices = 0
        this%num_external_indices = 0
        this%num_internal_indices = 0
    end subroutine clear
end module tc_descriptor_module
