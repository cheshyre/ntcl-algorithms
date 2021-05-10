module raw_mm_driver_module
    use, intrinsic :: iso_fortran_env, only : &
            real64, &
            int64, &
            int32

    use :: data_api, only : stream

    use :: tensor_api, only : &
            scalar, &
            matrix, &
            dt_c128, &
            dt_c64, &
            dt_r32, &
            dt_r64

    use :: matrix_multiplication_module, only : matrix_multiplication

    implicit none
    private

    public :: raw_mm_driver

    type, abstract, extends(matrix_multiplication) :: raw_mm_driver
    contains
        procedure :: mm => mm
        procedure :: is_compatible => is_compatible
        procedure :: set_and_check_dimensions => set_and_check_dimensions
        procedure :: within_blas_bounds => within_blas_bounds
        procedure(mm_interface), deferred :: mm_real32
        procedure(mm_interface), deferred :: mm_real64
        procedure(mm_interface), deferred :: mm_complex64
        procedure(mm_interface), deferred :: mm_complex128
    end type raw_mm_driver

    abstract interface
        subroutine mm_interface(this, dst, left, right, m, n, k, alpha, beta, astream)
            import :: raw_mm_driver
            import :: matrix
            import :: int64
            import :: scalar
            import :: stream

            class(raw_mm_driver), intent(in) :: this
            type(matrix), intent(inout) :: dst
            type(matrix), intent(in) :: left, right
            integer(int64), intent(in) :: m, n, k
            type(scalar), intent(in), optional :: alpha, beta
            type(stream), intent(in), optional :: astream
        end subroutine mm_interface
    end interface
contains
    subroutine mm(this, dst, left, right, alpha, beta, astream)
        class(raw_mm_driver), intent(in) :: this
        type(matrix), intent(inout) :: dst
        type(matrix), intent(in) :: left, right
        type(scalar), intent(in), optional :: alpha, beta
        type(stream), intent(in), optional :: astream

        integer(int64) :: m, n, k

        if ( .not. this%is_compatible(dst, left, right) ) &
                error stop "raw_mm_driver::mm:Matrices are not compatible."

        call this%set_and_check_dimensions(dst, left, m, n, k)

        select case ( dst%get_datatype() )
        case (dt_r32)
            call this%mm_real32(dst, left, right, m, n, k, alpha, beta, astream)
        case (dt_r64)
            call this%mm_real64(dst, left, right, m, n, k, alpha, beta, astream)
        case (dt_c64)
            call this%mm_complex64(dst, left, right, m, n, k, alpha, beta, astream)
        case (dt_c128)
            call this%mm_complex128(dst, left, right, m, n, k, alpha, beta, astream)
        case default
            error stop "raw_mm_driver::mm:Not a supported datatype."
        end select
    end subroutine mm

    logical function is_compatible(this, dst, left, right)
        class(raw_mm_driver), intent(in) :: this
        type(matrix), intent(in) :: dst, left, right

        if ( left%is_transposed .and. right%is_transposed ) then
            is_compatible = dst%dims(1) == left%dims(2) .and. &
                    dst%dims(2) == right%dims(1) .and. &
                    left%dims(1) == right%dims(2)
        else if ( left%is_transposed ) then
            is_compatible = dst%dims(1) == left%dims(2) .and. &
                    dst%dims(2) == right%dims(2) .and. &
                    left%dims(1) == right%dims(1)
        else if ( right%is_transposed ) then
            is_compatible = dst%dims(1) == left%dims(1) .and. &
                    dst%dims(2) == right%dims(1) .and. &
                    left%dims(2) == right%dims(2)
        else
            is_compatible = dst%dims(1) == left%dims(1) .and. &
                    dst%dims(2) == right%dims(2) .and. &
                    left%dims(2) == right%dims(1)
        end if
    end function is_compatible

    subroutine set_and_check_dimensions(this, dst, left, m, n, k)
        class(raw_mm_driver), intent(in) :: this
        type(matrix), intent(in) :: dst, left
        integer(int64), intent(inout) :: m, n, k

        m = dst%dims(1)
        n = dst%dims(2)
        if ( left%is_transposed ) then
            k = left%dims(1)
        else
            k = left%dims(2)
        end if

        if ( .not. this%within_blas_bounds(m, n, k) ) &
                error stop "raw_mm_driver::get_dimensions:Dimensions outside mm implementation bounds."
    end subroutine set_and_check_dimensions

    logical function within_blas_bounds(this, m, n, k)
        class(raw_mm_driver), intent(in) :: this
        integer(int64), intent(in) :: m, n, k

        within_blas_bounds = m <= huge(int(1, int32)) .and. &
                n <= huge(int(1, int32)) .and. k <= huge(int(1, int32))
    end function within_blas_bounds
end module raw_mm_driver_module
