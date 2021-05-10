module tc_index_helper_module
    use, intrinsic :: iso_fortran_env, only : int64
    use :: tc_descriptor_module, only : tc_descriptor

    implicit none
    private

    public :: tc_index_helper
    public :: max_number_of_indices

    integer, parameter :: max_number_of_indices = 6

    type :: tc_index_helper
        integer :: number_of_indices
        integer(int64) :: number_of_products
        integer(int64), dimension(max_number_of_indices) :: c_multipliers
        integer(int64), dimension(max_number_of_indices) :: a_multipliers
        integer(int64), dimension(max_number_of_indices) :: b_multipliers
        integer(int64), dimension(max_number_of_indices) :: all_multipliers
    contains
        procedure :: set_number_of_products => set_number_of_products
        procedure :: calculate_all_multipliers => calculate_all_multipliers
        procedure :: calculate_abc_multipliers => calculate_abc_multipliers
        procedure :: calculate_all_indexes => calculate_all_indexes
        procedure :: calculate_index => calculate_index
        procedure :: calculate_abc_indexes => calculate_abc_indexes
    end type tc_index_helper

    interface tc_index_helper
        module procedure constructor
    end interface tc_index_helper
contains
    function constructor(tcd, sizes) result(this)
        type(tc_descriptor), intent(in) :: tcd
        integer(int64), dimension(max_number_of_indices), intent(in) :: sizes
        type(tc_index_helper) :: this

        this%number_of_indices = tcd%num_unique_indices
        call this%set_number_of_products(sizes)
        call this%calculate_all_multipliers(this%all_multipliers, sizes)
        call this%calculate_abc_multipliers(this%c_multipliers, sizes, tcd%c_indices)
        call this%calculate_abc_multipliers(this%a_multipliers, sizes, tcd%a_indices)
        call this%calculate_abc_multipliers(this%b_multipliers, sizes, tcd%b_indices)
    end function constructor

    pure subroutine set_number_of_products(this, sizes)
        class(tc_index_helper), intent(inout) :: this
        integer(int64), dimension(:), intent(in) :: sizes

        integer(int64) :: idx

        this%number_of_products = 1
        do idx = 1, this%number_of_indices
            this%number_of_products = this%number_of_products * sizes(idx)
        end do
    end subroutine set_number_of_products

    pure subroutine calculate_all_indexes(this, all_indexes, idx)
        class(tc_index_helper), intent(in) :: this
        integer(int64), dimension(:), intent(inout) :: all_indexes
        integer(int64), intent(in) :: idx

        integer(int64) :: left, na
        integer :: order

        left = idx
        do order = this%number_of_indices, 2, -1
            na = this%all_multipliers(order)
            all_indexes(order) = (left-1)/na + 1
            left = mod((left-1), na)+1
        end do
        all_indexes(1) = left
    end subroutine calculate_all_indexes

    pure subroutine calculate_all_multipliers(this, all_multipliers, sizes)
        class(tc_index_helper), intent(in) :: this
        integer(int64), dimension(:), intent(inout) :: all_multipliers
        integer(int64), dimension(:), intent(in) :: sizes

        integer :: idx

        all_multipliers(1) = 1
        do idx = 2, this%number_of_indices
            all_multipliers(idx) = all_multipliers(idx-1)*sizes(idx-1)
        end do
    end subroutine calculate_all_multipliers

    pure subroutine calculate_abc_multipliers(this, abc_multipliers, sizes, indices)
        class(tc_index_helper), intent(in) :: this
        integer(int64), dimension(:), intent(inout) :: abc_multipliers
        integer(int64), dimension(:), intent(in) :: sizes
        integer, dimension(:), intent(in) :: indices

        integer :: idx, cidx
        integer(int64) :: multiplier

        do idx = 1, this%number_of_indices
            abc_multipliers(idx) = 0
        end do

        multiplier = 1
        do idx = 1, size(indices)
            cidx = indices(idx)
            abc_multipliers(cidx) = multiplier
            multiplier = multiplier*sizes(cidx)
        end do
    end subroutine calculate_abc_multipliers

    pure integer(int64) function calculate_index(this, indexes, multipliers)
        class(tc_index_helper), intent(in) :: this
        integer(int64), dimension(:), intent(in) :: indexes, multipliers

        integer :: idx

        calculate_index = 1
        do idx = 1, this%number_of_indices
            calculate_index = calculate_index + (indexes(idx)-1)*multipliers(idx)
        end do
    end function calculate_index

    pure subroutine calculate_abc_indexes(this, abc_indexes, idx)
        class(tc_index_helper), intent(in) :: this
        integer(int64), dimension(3), intent(inout) :: abc_indexes
        integer(int64), intent(in) :: idx

        integer(int64), dimension(max_number_of_indices) :: indexes

        call this%calculate_all_indexes(indexes, idx)
        abc_indexes(1) = this%calculate_index(indexes, this%c_multipliers)
        abc_indexes(2) = this%calculate_index(indexes, this%a_multipliers)
        abc_indexes(3) = this%calculate_index(indexes, this%b_multipliers)
    end subroutine calculate_abc_indexes
end module tc_index_helper_module
