module scalar_inline_update_test_helper_module
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
            copy_tensor

    use :: algorithms_api, only : &
            scalar_inline_update, &
            tensor_update_factory

    implicit none
    private

    public :: scalar_inline_update_test_helper

    type :: scalar_inline_update_test_helper
    contains
        procedure :: run => run
        procedure, nopass :: run_real32 => run_real32
        procedure, nopass :: run_real64 => run_real64
        procedure, nopass :: run_complex64 => run_complex64
        procedure, nopass :: run_complex128 => run_complex128
    end type scalar_inline_update_test_helper

    real(real32), parameter :: single = 1.0e-7
    real(real64), parameter :: double = 1.0d-14
contains
    subroutine run(this, assertion, prefix, driver, memory_type, options, priorities)
        class(scalar_inline_update_test_helper), intent(in) :: this
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

        type(matrix) :: a
        integer :: m, n
        real(real32), dimension(:,:), allocatable :: am
        type(storage_helper) :: helper
        class(scalar_inline_update), allocatable :: siu

        a = matrix()
        call tensor_update_factory%create(siu, driver, options, priorities)

        m = 20; n = 30
        allocate(am(m, n))

        call random_number(am)
        call copy_tensor(a, am, memory_type, options, priorities)

        am = 0.33*am
        call siu%update(a, scalar(0.33))

        call assertion%equal(prefix//"::Equal", &
                helper%equal(a%storage, am, single))

        call a%cleanup()
        call siu%cleanup()
        deallocate(siu)
        deallocate(am)
    end subroutine run_real32

    subroutine run_real64(assertion, prefix, driver, memory_type, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver, memory_type
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        type(matrix) :: a
        integer :: m, n
        real(real64), dimension(:,:), allocatable :: am
        type(storage_helper) :: helper
        class(scalar_inline_update), allocatable :: siu

        a = matrix()
        call tensor_update_factory%create(siu, driver, options, priorities)

        m = 20; n = 30
        allocate(am(m, n))

        call random_number(am)
        call copy_tensor(a, am, memory_type, options, priorities)

        am = 0.33*am
        call siu%update(a, scalar(0.33))

        call assertion%equal(prefix//"::Equal", &
            helper%equal(a%storage, am, double))

        call a%cleanup()
        call siu%cleanup()
        deallocate(siu)
        deallocate(am)
    end subroutine run_real64

    subroutine run_complex64(assertion, prefix, driver, memory_type, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver, memory_type
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        type(matrix) :: a
        integer :: m, n
        complex(real32), dimension(:,:), allocatable :: am
        real(real32), dimension(:,:), allocatable :: are, aim
        type(storage_helper) :: helper
        class(scalar_inline_update), allocatable :: siu

        a = matrix()
        call tensor_update_factory%create(siu, driver, options, priorities)

        m = 20; n = 30
        allocate(am(m, n), are(m,n), aim(m,n))

        call random_number(are)
        call random_number(aim)
        am = cmplx(are, aim)

        call copy_tensor(a, am, memory_type, options, priorities)

        am = 0.33*am
        call siu%update(a, scalar(0.33))

        call assertion%equal(prefix//"::Equal", &
                helper%equal(a%storage, am, (single, single)))

        call a%cleanup()
        call siu%cleanup()
        deallocate(siu)
        deallocate(am, are, aim)
    end subroutine run_complex64

    subroutine run_complex128(assertion, prefix, driver, memory_type, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver, memory_type
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        type(matrix) :: a
        integer :: m, n
        complex(real64), dimension(:,:), allocatable :: am
        real(real64), dimension(:,:), allocatable :: are, aim
        type(storage_helper) :: helper
        class(scalar_inline_update), allocatable :: siu

        a = matrix()
        call tensor_update_factory%create(siu, driver, options, priorities)

        m = 20; n = 30
        allocate(am(m, n), are(m,n), aim(m,n))

        call random_number(are)
        call random_number(aim)
        am = cmplx(are, aim, kind=real64)

        call copy_tensor(a, am, memory_type, options, priorities)

        am = 0.33*am
        call siu%update(a, scalar(0.33))

        call assertion%equal(prefix//"::Equal", &
            helper%equal(a%storage, am, (double, double)))

        call a%cleanup()
        call siu%cleanup()
        deallocate(siu)
        deallocate(am, are, aim)
    end subroutine run_complex128
end module scalar_inline_update_test_helper_module
