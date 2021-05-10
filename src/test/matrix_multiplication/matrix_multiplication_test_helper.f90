module matrix_multiplication_test_helper_module
    use, intrinsic :: iso_fortran_env, only : &
            real32, &
            real64

    use :: util_api, only : &
            assert, &
            string, &
            dictionary

    use :: data_api, only : &
            storage_helper, &
            stream_handler, &
            stream, &
            concurrency_factory

    use :: tensor_api, only : &
            scalar, &
            matrix, &
            copy_tensor

    use :: algorithms_api, only : &
            matrix_multiplication, &
            matrix_multiplication_factory

    implicit none
    private

    public :: matrix_multiplication_test_helper

    type :: matrix_multiplication_test_helper
    contains
        procedure :: run => run
        procedure, nopass :: run_real32 => run_real32
        procedure, nopass :: run_real64 => run_real64
        procedure, nopass :: run_complex64 => run_complex64
        procedure, nopass :: run_complex128 => run_complex128
    end type matrix_multiplication_test_helper

    real(real32), parameter :: single = 1.0e-4
    real(real64), parameter :: double = 1.0d-13
contains
    subroutine run(this, assertion, prefix, driver, memory_type, options, priorities)
        class(matrix_multiplication_test_helper), intent(in) :: this
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

        type(matrix) :: a, b, c
        integer :: m, n, k
        real(real32), dimension(:,:), allocatable :: am, bm, cm
        type(storage_helper) :: helper
        class(matrix_multiplication), allocatable :: mm
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        a = matrix(); b=matrix(); c=matrix()
        call matrix_multiplication_factory%create(mm, driver, options, priorities)

        m = 20; n = 30; k = 50
        allocate(am(m, k), bm(k, n), cm(m,n))
        am = 0.0; bm=0.0; cm=0.0

        call copy_tensor(a, am, memory_type, options, priorities)
        call copy_tensor(b, bm, memory_type, options, priorities)
        call copy_tensor(c, cm, memory_type, options, priorities)

        call mm%mm(c, a, b)

        call assertion%equal(prefix//"::Product of zero matrices", &
                helper%equal(c%storage, cm, single))

        call a%cleanup(); call b%cleanup(); call c%cleanup()

        call random_number(am)
        call random_number(bm)
        cm=0.0

        call copy_tensor(a, am, memory_type, options, priorities)
        call copy_tensor(b, bm, memory_type, options, priorities)
        call copy_tensor(c, cm, memory_type, options, priorities)

        call concurrency_factory%create_default_stream_handler(handler, memory_type, options, priorities)
        call handler%create(astream)
        cm = matmul(am, bm)

        call mm%mm(c, a, b, astream=astream)
        call mm%synchronize(astream)
        call handler%destroy(astream)
        deallocate(handler)
        call assertion%equal(prefix//"::Product of random matrices", &
                helper%equal(c%storage, cm, single))



        call mm%mm(c, a, b)
        call mm%mm(c, a, b)
        call assertion%equal(prefix//"::Product of random matrices twice", &
                helper%equal(c%storage, cm, single))
        call a%cleanup(); call b%cleanup(); call c%cleanup()

        call mm%cleanup()
        deallocate(mm)
        deallocate(am, bm, cm)
    end subroutine run_real32

    subroutine run_real64(assertion, prefix, driver, memory_type, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver, memory_type
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        type(matrix) :: a, b, c
        integer :: m, n, k
        real(real64), dimension(:,:), allocatable :: am, bm, cm
        type(storage_helper) :: helper
        class(matrix_multiplication), allocatable :: mm
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        a = matrix(); b=matrix(); c=matrix()
        call matrix_multiplication_factory%create(mm, driver, options, priorities)

        m = 20; n = 30; k = 50
        allocate(am(m, k), bm(k, n), cm(m,n))
        am = 0.0d0; bm=0.0d0; cm=0.0d0

        call copy_tensor(a, am, memory_type, options, priorities)
        call copy_tensor(b, bm, memory_type, options, priorities)
        call copy_tensor(c, cm, memory_type, options, priorities)

        call mm%mm(c, a, b)

        call assertion%equal(prefix//"::Product of zero matrices", &
                helper%equal(c%storage, cm, double))

        call a%cleanup(); call b%cleanup(); call c%cleanup()

        call random_number(am)
        call random_number(bm)
        cm=0.0d0

        call copy_tensor(a, am, memory_type, options, priorities)
        call copy_tensor(b, bm, memory_type, options, priorities)
        call copy_tensor(c, cm, memory_type, options, priorities)

        cm = matmul(am, bm)

        call concurrency_factory%create_default_stream_handler(handler, memory_type, options, priorities)
        call handler%create(astream)
        call mm%mm(c, a, b, astream=astream)
        call mm%synchronize(astream)
        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//"::Product of random matrices", &
                helper%equal(c%storage, cm, double))

        call mm%mm(c, a, b)
        call mm%mm(c, a, b)
        call assertion%equal(prefix//"::Product of random matrices twice", &
                helper%equal(c%storage, cm, double))
        call a%cleanup(); call b%cleanup(); call c%cleanup()

        call mm%cleanup()
        deallocate(mm)
        deallocate(am, bm, cm)
    end subroutine run_real64

    subroutine run_complex64(assertion, prefix, driver, memory_type, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver, memory_type
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        type(matrix) :: a, b, c
        integer :: m, n, k
        complex(real32), dimension(:,:), allocatable :: am, bm, cm
        real(real32), dimension(:,:), allocatable :: are, aim, bre, bim
        type(storage_helper) :: helper
        class(matrix_multiplication), allocatable :: mm
        complex(real32) :: tolerance
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        a = matrix(); b=matrix(); c=matrix()
        tolerance = (single,single)

        call matrix_multiplication_factory%create(mm, driver, options, priorities)

        m = 20; n = 30; k = 50
        allocate(am(m, k), bm(k, n), cm(m,n))
        allocate(are(m, k), aim(m, k))
        allocate(bre(k, n), bim(k, n))
        am = (0.0, 0.0); bm=(0.0, 0.0); cm=(0.0, 0.0)

        call copy_tensor(a, am, memory_type, options, priorities)
        call copy_tensor(b, bm, memory_type, options, priorities)
        call copy_tensor(c, cm, memory_type, options, priorities)

        call mm%mm(c, a, b)

        call assertion%equal(prefix//"::Product of zero matrices", &
                helper%equal(c%storage, cm, tolerance))

        call a%cleanup(); call b%cleanup(); call c%cleanup()

        call random_number(are)
        call random_number(aim)
        call random_number(bre)
        call random_number(bim)
        am = cmplx(are, aim)
        bm = cmplx(bre, bim)

        cm=(0.0, 0.0)

        call copy_tensor(a, am, memory_type, options, priorities)
        call copy_tensor(b, bm, memory_type, options, priorities)
        call copy_tensor(c, cm, memory_type, options, priorities)

        cm = matmul(am, bm)

        call concurrency_factory%create_default_stream_handler(handler, memory_type, options, priorities)
        call handler%create(astream)
        call mm%mm(c, a, b, astream=astream)
        call mm%synchronize(astream)
        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//"::Product of random matrices", &
                helper%equal(c%storage, cm, tolerance))

        call mm%mm(c, a, b)
        call mm%mm(c, a, b)
        call assertion%equal(prefix//"::Product of random matrices twice", &
                helper%equal(c%storage, cm, tolerance))
        call a%cleanup(); call b%cleanup(); call c%cleanup()

        call mm%cleanup()
        deallocate(mm)
        deallocate(am, bm, cm, are, aim, bre, bim)
    end subroutine run_complex64

    subroutine run_complex128(assertion, prefix, driver, memory_type, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver, memory_type
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        type(matrix) :: a, b, c
        integer :: m, n, k
        complex(real64), dimension(:,:), allocatable :: am, bm, cm
        real(real64), dimension(:,:), allocatable :: are, aim, bre, bim
        type(storage_helper) :: helper
        class(matrix_multiplication), allocatable :: mm
        complex(real64) :: tolerance
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        a = matrix(); b=matrix(); c=matrix()
        tolerance = (double,double)

        call matrix_multiplication_factory%create(mm, driver, options, priorities)

        m = 20; n = 30; k = 50
        allocate(am(m, k), bm(k, n), cm(m,n))
        allocate(are(m, k), aim(m, k))
        allocate(bre(k, n), bim(k, n))
        am = (0.0d0, 0.0d0); bm=(0.0d0, 0.0d0); cm=(0.0d0, 0.0d0)

        call copy_tensor(a, am, memory_type, options, priorities)
        call copy_tensor(b, bm, memory_type, options, priorities)
        call copy_tensor(c, cm, memory_type, options, priorities)

        call mm%mm(c, a, b)

        call assertion%equal(prefix//"::Product of zero matrices", &
                helper%equal(c%storage, cm, tolerance))

        call a%cleanup(); call b%cleanup(); call c%cleanup()

        call random_number(are)
        call random_number(aim)
        call random_number(bre)
        call random_number(bim)
        am = cmplx(are, aim, kind=real64)
        bm = cmplx(bre, bim, kind=real64)

        cm=(0.0d0, 0.0d0)

        call copy_tensor(a, am, memory_type, options, priorities)
        call copy_tensor(b, bm, memory_type, options, priorities)
        call copy_tensor(c, cm, memory_type, options, priorities)

        cm = matmul(am, bm)

        call concurrency_factory%create_default_stream_handler(handler, memory_type, options, priorities)
        call handler%create(astream)
        call mm%mm(c, a, b, astream=astream)
        call mm%synchronize(astream)
        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//"::Product of random matrices", &
                helper%equal(c%storage, cm, tolerance))

        call mm%mm(c, a, b)
        call mm%mm(c, a, b)
        call assertion%equal(prefix//"::Product of random matrices twice", &
                helper%equal(c%storage, cm, tolerance))
        call a%cleanup(); call b%cleanup(); call c%cleanup()

        call mm%cleanup()
        deallocate(mm)
        deallocate(am, bm, cm, are, aim, bre, bim)
    end subroutine run_complex128
end module matrix_multiplication_test_helper_module
