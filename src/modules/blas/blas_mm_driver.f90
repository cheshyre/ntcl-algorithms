module blas_mm_driver_module
    use, intrinsic :: iso_fortran_env, only : &
            real32, &
            real64, &
            int64

    use :: data_api, only : stream

    use :: matrix_multiplication_dev, only : &
            fortran_data_mm_driver

    implicit none
    private

    public :: blas_mm_driver

    type, extends(fortran_data_mm_driver) :: blas_mm_driver
    contains
        procedure :: perform_mm_r32 => perform_mm_r32
        procedure :: perform_mm_r64 => perform_mm_r64
        procedure :: perform_mm_c64 => perform_mm_c64
        procedure :: perform_mm_c128 => perform_mm_c128
        procedure :: synchronize => synchronize
        procedure :: cleanup => cleanup
    end type blas_mm_driver

contains
    subroutine perform_mm_r32(this, dst, left, right, m, n, k, &
            alpha, beta, transposed_left, transposed_right, astream)
        class(blas_mm_driver), intent(in) :: this
        real(real32), dimension(:,:), contiguous, intent(inout) :: dst
        real(real32), dimension(:,:), contiguous, intent(in) :: left, right
        integer(int64), intent(in) :: m, n, k
        real(real32), intent(in) :: alpha, beta
        logical, intent(in) :: transposed_left, transposed_right
        type(stream), intent(in), optional :: astream

        character :: ta, tb
        integer :: lda, ldb, ldc

        ta = 'n'
        if ( transposed_left ) ta = 't'

        tb = 'n'
        if ( transposed_right ) tb = 't'

        lda = size(left, 1); ldb = size(right, 1); ldc = size(dst, 1)
        call sgemm( ta, tb, int(m), int(n), int(k), alpha, left, lda, right, ldb, &
                beta, dst, ldc)
    end subroutine perform_mm_r32

    subroutine perform_mm_r64(this, dst, left, right, m, n, k, &
            alpha, beta, transposed_left, transposed_right, astream)
        class(blas_mm_driver), intent(in) :: this
        real(real64), dimension(:,:), contiguous, intent(inout) :: dst
        real(real64), dimension(:,:), contiguous, intent(in) :: left, right
        integer(int64), intent(in) :: m, n, k
        real(real64), intent(in) :: alpha, beta
        logical, intent(in) :: transposed_left, transposed_right
        type(stream), intent(in), optional :: astream

        character :: ta, tb
        integer :: lda, ldb, ldc

        ta = 'n'
        if ( transposed_left ) ta = 't'

        tb = 'n'
        if ( transposed_right ) tb = 't'

        lda = size(left, 1); ldb = size(right, 1); ldc = size(dst, 1)
        call dgemm( ta, tb, int(m), int(n), int(k), alpha, left, lda, right, ldb, &
                beta, dst, ldc)
    end subroutine perform_mm_r64

    subroutine perform_mm_c64(this, dst, left, right, m, n, k, &
            alpha, beta, transposed_left, transposed_right, astream)
        class(blas_mm_driver), intent(in) :: this
        complex(real32), dimension(:,:), contiguous, intent(inout) :: dst
        complex(real32), dimension(:,:), contiguous, intent(in) :: left, right
        integer(int64), intent(in) :: m, n, k
        complex(real32), intent(in) :: alpha, beta
        logical, intent(in) :: transposed_left, transposed_right
        type(stream), intent(in), optional :: astream

        character :: ta, tb
        integer :: lda, ldb, ldc

        ta = 'n'
        if ( transposed_left ) ta = 't'

        tb = 'n'
        if ( transposed_right ) tb = 't'

        lda = size(left, 1); ldb = size(right, 1); ldc = size(dst, 1)
        call cgemm( ta, tb, int(m), int(n), int(k), alpha, left, lda, right, ldb, &
                beta, dst, ldc)
    end subroutine perform_mm_c64

    subroutine perform_mm_c128(this, dst, left, right, m, n, k, &
            alpha, beta, transposed_left, transposed_right, astream)
        class(blas_mm_driver), intent(in) :: this
        complex(real64), dimension(:,:), contiguous, intent(inout) :: dst
        complex(real64), dimension(:,:), contiguous, intent(in) :: left, right
        integer(int64), intent(in) :: m, n, k
        complex(real64), intent(in) :: alpha, beta
        logical, intent(in) :: transposed_left, transposed_right
        type(stream), intent(in), optional :: astream

        character :: ta, tb
        integer :: lda, ldb, ldc

        ta = 'n'
        if ( transposed_left ) ta = 't'

        tb = 'n'
        if ( transposed_right ) tb = 't'

        lda = size(left, 1); ldb = size(right, 1); ldc = size(dst, 1)
        call zgemm( ta, tb, int(m), int(n), int(k), alpha, left, lda, right, ldb, &
                beta, dst, ldc)
    end subroutine perform_mm_c128

    subroutine cleanup(this)
        class(blas_mm_driver), intent(inout) :: this

        continue
    end subroutine cleanup

    subroutine synchronize(this, astream)
        class(blas_mm_driver), intent(in) :: this
        type(stream), intent(in), optional :: astream

        continue
    end subroutine synchronize
end module blas_mm_driver_module
