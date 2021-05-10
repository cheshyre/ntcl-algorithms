module simple_loops_bmm_driver_module
    use :: data_api, only : stream

    use :: tensor_api, only : &
            scalar, &
            matrix

    use :: matrix_multiplication_module, only : matrix_multiplication

    use :: batched_matrix_multiplication_module, only : &
            batched_matrix_multiplication

    implicit none
    private

    public :: simple_loops_bmm_driver

    type, extends(batched_matrix_multiplication) :: simple_loops_bmm_driver
        class(matrix_multiplication), allocatable :: driver
    contains
        procedure :: bmm_array => bmm
        procedure :: bmm_single => bmm_single
        procedure :: synchronize => synchronize
        procedure :: cleanup => cleanup
    end type simple_loops_bmm_driver

    interface simple_loops_bmm_driver
        module procedure constructor_empty
        module procedure constructor
    end interface simple_loops_bmm_driver

contains
    function constructor_empty() result(this)
        type(simple_loops_bmm_driver) :: this

        continue
    end function constructor_empty

    function constructor(driver) result(this)
        class(matrix_multiplication), intent(in) :: driver
        type(simple_loops_bmm_driver) :: this

        this%driver = driver
    end function constructor

    subroutine bmm(this, dst, left, right, alpha, beta, astream)
        class(simple_loops_bmm_driver), intent(inout) :: this
        type(matrix), dimension(:), intent(inout) :: dst
        type(matrix), dimension(:), intent(in) :: left, right
        type(scalar), intent(in), dimension(:), optional :: alpha, beta
        type(stream), intent(in), optional :: astream

        integer :: idx

        if ( present(alpha) .and. present(beta) ) then
            do idx = 1, size(dst)
                call this%driver%mm(dst(idx), left(idx), right(idx), alpha(idx), beta(idx), astream)
            end do
        else if ( present(alpha) ) then
            do idx = 1, size(dst)
                call this%driver%mm(dst(idx), left(idx), right(idx), alpha=alpha(idx), astream=astream)
            end do
        else if ( present(beta) ) then
            do idx = 1, size(dst)
                call this%driver%mm(dst(idx), left(idx), right(idx), beta=beta(idx), astream=astream)
            end do
        else
            do idx = 1, size(dst)
                call this%driver%mm(dst(idx), left(idx), right(idx), astream=astream)
            end do
        end if
    end subroutine bmm

    subroutine bmm_single(this, dst, left, right, alpha, beta, astream)
        class(simple_loops_bmm_driver), intent(inout) :: this
        type(matrix), dimension(:), intent(inout) :: dst
        type(matrix), dimension(:), intent(in) :: left, right
        type(scalar), intent(in) :: alpha, beta
        type(stream), intent(in), optional :: astream

        integer :: idx

        do idx = 1, size(dst)
            call this%driver%mm(dst(idx), left(idx), right(idx), alpha, beta, astream)
        end do
    end subroutine bmm_single

    subroutine synchronize(this, astream)
        class(simple_loops_bmm_driver), intent(in) :: this
        type(stream), intent(in), optional :: astream

        call this%driver%synchronize(astream)
    end subroutine synchronize

    subroutine cleanup(this)
        class(simple_loops_bmm_driver), intent(inout) :: this

        if ( allocated(this%driver) ) then
            call this%driver%cleanup()
            deallocate(this%driver)
        end if
    end subroutine cleanup
end module simple_loops_bmm_driver_module
