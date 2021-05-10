module ttgt_descriptor_module
    use, intrinsic :: iso_fortran_env, only : int64
    use :: tensor_api, only : tensor

    use :: tc_descriptor_module, only : tc_descriptor

    implicit none
    private

    public :: ttgt_descriptor

    type :: ttgt_descriptor
        logical :: switch_ab
        integer, dimension(:), allocatable :: m, n, k
        integer, dimension(:), allocatable :: corder, aorder, border, cinverseorder
        logical :: permute_c, permute_a, permute_b
        integer, dimension(:), allocatable :: cinverse, ainverse, binverse
    contains
        procedure :: get_matrix_dimensions => get_matrix_dimensions
        procedure :: get_dimensions => get_dimensions
        procedure :: setup => setup
        procedure :: setup_inverse_arrays => setup_inverse_arrays
        procedure :: setup_permute_orders => setup_permute_orders
        procedure :: get_order => get_order
        procedure :: get_inverse_order => get_inverse_order
        procedure :: is_permuted => is_permuted
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type ttgt_descriptor

    interface ttgt_descriptor
        module procedure constructor_empty
        module procedure constructor
    end interface ttgt_descriptor

contains
    function constructor_empty() result(this)
        type(ttgt_descriptor) :: this

        call this%clear()
    end function constructor_empty

    function constructor(descr, m, n, k, switch_ab) result(this)
        type(tc_descriptor), intent(in) :: descr
        integer, dimension(:), intent(in) :: m, n, k
        logical, intent(in), optional :: switch_ab
        type(ttgt_descriptor) :: this

        this = ttgt_descriptor()

        this%m = m
        this%n = n
        this%k = k

        if ( present(switch_ab)) this%switch_ab = switch_ab

        call this%setup(descr)

    end function constructor

    function get_matrix_dimensions(this, a, b) result(dimensions)
        class(ttgt_descriptor), intent(in) :: this
        class(tensor), intent(in) :: a, b
        integer(int64), dimension(3) :: dimensions

        integer(int64), dimension(:), allocatable :: all_dims
        integer :: idx

        all_dims = this%get_dimensions(a, b)
        dimensions = 1
        do idx = 1, size(this%m)
            dimensions(1) = dimensions(1)*all_dims(this%m(idx))
        end do
        do idx = 1, size(this%n)
            dimensions(2) = dimensions(2)*all_dims(this%n(idx))
        end do
        do idx = 1, size(this%k)
            dimensions(3) = dimensions(3)*all_dims(this%k(idx))
        end do
    end function get_matrix_dimensions

    function get_dimensions(this, a, b) result(dimensions)
        class(ttgt_descriptor), intent(in) :: this
        class(tensor), intent(in) :: a, b
        integer(int64), dimension(:), allocatable :: dimensions

        integer :: idx

        allocate(dimensions(size(this%ainverse)))
        do idx = 1, size(this%ainverse)
            if ( this%ainverse(idx) > 0 ) dimensions(idx) = a%dims(this%ainverse(idx))
        end do

        do idx = 1, size(this%binverse)
            if ( this%binverse(idx) > 0 ) dimensions(idx) = b%dims(this%binverse(idx))
        end do
    end function get_dimensions

    subroutine setup(this, descr)
        class(ttgt_descriptor), intent(inout) :: this
        type(tc_descriptor), intent(in) :: descr

        call this%setup_inverse_arrays(descr)
        call this%setup_permute_orders()
        this%cinverseorder = this%get_inverse_order(this%corder)
    end subroutine setup

    function get_inverse_order(this, order) result(inverse)
        class(ttgt_descriptor), intent(in) :: this
        integer, dimension(:), intent(in) :: order
        integer, dimension(:), allocatable :: inverse

        integer :: idx

        allocate(inverse(size(order)))
        do idx = 1, size(order)
            inverse(order(idx)) = idx
        end do
    end function get_inverse_order

    subroutine setup_inverse_arrays(this, descr)
        class(ttgt_descriptor), intent(inout) :: this
        type(tc_descriptor), intent(in) :: descr

        this%cinverse = descr%get_inverse(descr%c_indices)
        this%ainverse = descr%get_inverse(descr%a_indices)
        this%binverse = descr%get_inverse(descr%b_indices)
    end subroutine setup_inverse_arrays

    subroutine setup_permute_orders(this)
        class(ttgt_descriptor), intent(inout) :: this

        this%corder = this%get_order(this%cinverse, this%m, this%n)
        if ( this%switch_ab) then
            this%aorder = this%get_order(this%ainverse, this%k, this%n)
            this%border = this%get_order(this%binverse, this%m, this%k)
        else
            this%aorder = this%get_order(this%ainverse, this%m, this%k)
            this%border = this%get_order(this%binverse, this%k, this%n)
        end if

        this%permute_c = this%is_permuted(this%corder)
        this%permute_a = this%is_permuted(this%aorder)
        this%permute_b = this%is_permuted(this%border)
    end subroutine setup_permute_orders

    logical function is_permuted(this, order)
        class(ttgt_descriptor), intent(in) :: this
        integer, dimension(:), intent(in) :: order

        integer :: idx

        is_permuted = .not. all(order == [(idx, idx=1, size(order))])
    end function is_permuted

    function get_order(this, inverse, m, n) result(order_array)
        class(ttgt_descriptor), intent(in) :: this
        integer, dimension(:), intent(in) :: inverse, m, n
        integer, dimension(:), allocatable :: order_array

        integer :: idx, offset

        allocate(order_array(size(m) + size(n)))

        offset = 0
        do idx = 1, size(m)
            order_array(offset + idx) = inverse(m(idx))
        end do

        offset = size(m)
        do idx = 1, size(n)
            order_array(offset + idx) = inverse(n(idx))
        end do
    end function get_order

    subroutine cleanup(this)
        class(ttgt_descriptor), intent(inout) :: this

        if ( allocated(this%m) ) deallocate(this%m)
        if ( allocated(this%n) ) deallocate(this%n)
        if ( allocated(this%k) ) deallocate(this%k)
        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(ttgt_descriptor), intent(inout) :: this

        this%switch_ab = .false.
        this%permute_c = .false.
        this%permute_a = .false.
        this%permute_b = .false.
    end subroutine clear
end module ttgt_descriptor_module
