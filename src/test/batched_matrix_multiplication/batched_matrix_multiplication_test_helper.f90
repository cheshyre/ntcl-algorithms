module batched_matrix_multiplication_test_helper_module
    use, intrinsic :: iso_fortran_env, only : &
            real32, &
            real64

    use :: util_api, only : &
            assert, &
            string, &
            dictionary

    use :: data_api, only : storage_helper

    use :: tensor_api, only : &
            scalar, &
            matrix, &
            copy_tensor, &
            secure_fortran_pointer_from_tensor

    use :: algorithms_api, only : &
            batched_matrix_multiplication, &
            batched_matrix_multiplication_factory

    implicit none
    private

    public :: batched_matrix_multiplication_test_helper

    type :: batched_matrix_multiplication_test_helper
    contains
        procedure :: run => run
        procedure, nopass :: run_real32 => run_real32
        procedure, nopass :: run_real64 => run_real64
        procedure, nopass :: run_complex64 => run_complex64
        procedure, nopass :: run_complex128 => run_complex128
    end type batched_matrix_multiplication_test_helper

    real, parameter :: single = 1e-4
    real(real64), parameter :: double = 1d-13
contains
    subroutine run(this, assertion, prefix, driver, memory_type, options, priorities)
        class(batched_matrix_multiplication_test_helper), intent(in) :: this
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver, memory_type
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        call this%run_real32(assertion, prefix//"real32:", driver, memory_type, options, priorities)
        call this%run_real64(assertion, prefix//"real64:", driver, memory_type, options, priorities)
        call this%run_complex64(assertion, prefix//"complex64:", driver, memory_type, options, priorities)
        call this%run_complex128(assertion, prefix//"complex128:", driver, memory_type, options, priorities)
    end subroutine run

    subroutine run_real32(assertion, prefix, driver, memory_type, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver, memory_type
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        type(matrix), dimension(10) :: a, b, c, ctest
        integer :: m, n, k
        real(real32), dimension(:,:), allocatable :: am, bm, cm
        type(storage_helper) :: helper
        class(batched_matrix_multiplication), allocatable :: mm
        real(real32), dimension(:,:), pointer, contiguous :: ptr
        logical :: res
        integer :: idx

        call batched_matrix_multiplication_factory%create(mm, driver, options, priorities)

        do idx = 1, 10
            a(idx) = matrix()
            b(idx) = matrix()
            c(idx) = matrix()
            ctest(idx) = matrix()
        end do

        m = 20; n = 30; k = 50
        do idx = 1, 10
            allocate(am(m, k), bm(k, n), cm(m,n))
            call random_number(am)
            call random_number(bm)
            cm = 0.0

            call copy_tensor(a(idx), am, memory_type, options, priorities)
            call copy_tensor(b(idx), bm, memory_type, options, priorities)
            call copy_tensor(c(idx), cm, memory_type, options, priorities)

            deallocate(cm)
            cm = matmul(am, bm)
            call copy_tensor(ctest(idx), cm, memory_type, options, priorities)
            deallocate(am, bm, cm)
            m = m+1; n = n+1; k=k+1
        end do

        call mm%bmm(c, a, b, scalar(1), scalar(0))

        res = .true.
        do idx = 1, 10
            call secure_fortran_pointer_from_tensor(ptr, ctest(idx))
            res = res.and. helper%equal(c(idx)%storage, ptr, single)
        end do
        call assertion%equal(prefix//"::Product of random matrices", res)

        do idx = 1, 10
            call a(idx)%cleanup()
            call b(idx)%cleanup()
            call c(idx)%cleanup()
            call ctest(idx)%cleanup()
        end do

        call mm%cleanup()
        deallocate(mm)
    end subroutine run_real32

    subroutine run_real64(assertion, prefix, driver, memory_type, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver, memory_type
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        type(matrix), dimension(10) :: a, b, c, ctest
        integer :: m, n, k
        real(real64), dimension(:,:), allocatable :: am, bm, cm
        type(storage_helper) :: helper
        class(batched_matrix_multiplication), allocatable :: mm
        real(real64), dimension(:,:), pointer, contiguous :: ptr
        logical :: res
        integer :: idx

        call batched_matrix_multiplication_factory%create(mm, driver, options, priorities)

        do idx = 1, 10
            a(idx) = matrix()
            b(idx) = matrix()
            c(idx) = matrix()
            ctest(idx) = matrix()
        end do

        m = 20; n = 30; k = 50
        do idx = 1, 10
            allocate(am(m, k), bm(k, n), cm(m,n))
            call random_number(am)
            call random_number(bm)
            cm = 0.0d0

            call copy_tensor(a(idx), am, memory_type, options, priorities)
            call copy_tensor(b(idx), bm, memory_type, options, priorities)
            call copy_tensor(c(idx), cm, memory_type, options, priorities)

            deallocate(cm)
            cm = matmul(am, bm)
            call copy_tensor(ctest(idx), cm, memory_type, options, priorities)
            deallocate(am, bm, cm)
            m = m+1; n = n+1; k=k+1
        end do

        call mm%bmm(c, a, b, scalar(1), scalar(0))

        res = .true.
        do idx = 1, 10
            call secure_fortran_pointer_from_tensor(ptr, ctest(idx))
            res = res.and. helper%equal(c(idx)%storage, ptr, double)
        end do
        call assertion%equal(prefix//"::Product of random matrices", res)

        do idx = 1, 10
            call a(idx)%cleanup()
            call b(idx)%cleanup()
            call c(idx)%cleanup()
            call ctest(idx)%cleanup()
        end do

        call mm%cleanup()
        deallocate(mm)
    end subroutine run_real64

    subroutine run_complex64(assertion, prefix, driver, memory_type, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver, memory_type
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        type(matrix), dimension(10) :: a, b, c, ctest
        integer :: m, n, k
        complex(real32), dimension(:,:), allocatable :: am, bm, cm
        real(real32), dimension(:,:), allocatable :: are, aim, bre, bim
        type(storage_helper) :: helper
        class(batched_matrix_multiplication), allocatable :: mm
        complex(real32) :: tolerance
        complex(real32), dimension(:,:), pointer, contiguous :: ptr
        logical :: res
        integer :: idx

        call batched_matrix_multiplication_factory%create(mm, driver, options, priorities)

        do idx = 1, 10
            a(idx) = matrix()
            b(idx) = matrix()
            c(idx) = matrix()
            ctest(idx) = matrix()
        end do

        m = 20; n = 30; k = 50
        do idx = 1, 10
            allocate(cm(m,n))
            cm = (0.0, 0.0)

            allocate(are(m,k), aim(m,k))
            allocate(bre(k,n), bim(k,n))
            call random_number(are)
            call random_number(aim)
            call random_number(bre)
            call random_number(bim)

            allocate(am(m,k), bm(k,n))
            am = cmplx(are, aim)
            bm = cmplx(bre, bim)
            deallocate(are, aim, bre, bim)

            call copy_tensor(a(idx), am, memory_type, options, priorities)
            call copy_tensor(b(idx), bm, memory_type, options, priorities)
            call copy_tensor(c(idx), cm, memory_type, options, priorities)

            deallocate(cm)
            cm = matmul(am, bm)
            call copy_tensor(ctest(idx), cm, memory_type, options, priorities)
            deallocate(am, bm, cm)
            m = m+1; n = n+1; k=k+1
        end do

        call mm%bmm(c, a, b, scalar(1), scalar(0))

        res = .true.
        do idx = 1, 10
            call secure_fortran_pointer_from_tensor(ptr, ctest(idx))
            res = res.and. helper%equal(c(idx)%storage, ptr, (single, single))
        end do
        call assertion%equal(prefix//"::Product of random matrices", res)

        do idx = 1, 10
            call a(idx)%cleanup()
            call b(idx)%cleanup()
            call c(idx)%cleanup()
            call ctest(idx)%cleanup()
        end do

        call mm%cleanup()
        deallocate(mm)
    end subroutine run_complex64

    subroutine run_complex128(assertion, prefix, driver, memory_type, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver, memory_type
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        type(matrix), dimension(10) :: a, b, c, ctest
        integer :: m, n, k
        complex(real64), dimension(:,:), allocatable :: am, bm, cm
        real(real64), dimension(:,:), allocatable :: are, aim, bre, bim
        type(storage_helper) :: helper
        class(batched_matrix_multiplication), allocatable :: mm
        complex(real64) :: tolerance
        complex(real64), dimension(:,:), pointer, contiguous :: ptr
        logical :: res
        integer :: idx

        call batched_matrix_multiplication_factory%create(mm, driver, options, priorities)

        do idx = 1, 10
            a(idx) = matrix()
            b(idx) = matrix()
            c(idx) = matrix()
            ctest(idx) = matrix()
        end do

        m = 20; n = 30; k = 50
        do idx = 1, 10
            allocate(cm(m,n))
            cm = (0.0d0, 0.0d0)

            allocate(are(m,k), aim(m,k))
            allocate(bre(k,n), bim(k,n))
            call random_number(are)
            call random_number(aim)
            call random_number(bre)
            call random_number(bim)

            allocate(am(m,k), bm(k,n))
            am = cmplx(are, aim, kind=real64)
            bm = cmplx(bre, bim, kind=real64)
            deallocate(are, aim, bre, bim)

            call copy_tensor(a(idx), am, memory_type, options, priorities)
            call copy_tensor(b(idx), bm, memory_type, options, priorities)
            call copy_tensor(c(idx), cm, memory_type, options, priorities)

            deallocate(cm)
            cm = matmul(am, bm)
            call copy_tensor(ctest(idx), cm, memory_type, options, priorities)
            deallocate(am, bm, cm)
            m = m+1; n = n+1; k=k+1
        end do

        call mm%bmm(c, a, b, scalar(1), scalar(0))

        res = .true.
        do idx = 1, 10
            call secure_fortran_pointer_from_tensor(ptr, ctest(idx))
            res = res.and. helper%equal(c(idx)%storage, ptr, (double, double))
        end do
        call assertion%equal(prefix//"::Product of random matrices", res)

        do idx = 1, 10
            call a(idx)%cleanup()
            call b(idx)%cleanup()
            call c(idx)%cleanup()
            call ctest(idx)%cleanup()
        end do

        call mm%cleanup()
        deallocate(mm)
    end subroutine run_complex128
end module batched_matrix_multiplication_test_helper_module
