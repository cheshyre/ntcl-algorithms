module batched_tensor_contraction_test_helper_module
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
            tensor_array_element, &
            allocate_and_copy_tensor, &
            secure_fortran_pointer_from_tensor

    use :: algorithms_api, only : &
            batched_tensor_contraction, &
            batched_tensor_contraction_factory, &
            tensor_contraction, &
            tensor_contraction_factory

    implicit none
    private

    public :: batched_tensor_contraction_test_helper

    type :: batched_tensor_contraction_test_helper
    contains
        procedure :: run => run
        procedure, nopass :: run_real32 => run_real32
        procedure, nopass :: run_real64 => run_real64
        procedure, nopass :: run_complex64 => run_complex64
        procedure, nopass :: run_complex128 => run_complex128
    end type batched_tensor_contraction_test_helper
contains
    subroutine run(this, assertion, prefix, driver, memory_type, options, priorities)
        class(batched_tensor_contraction_test_helper), intent(in) :: this
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

        type(tensor_array_element), dimension(10) :: a, b, c, c_original
        real(real32), dimension(:,:), allocatable :: am, bm, cm
        integer :: m, n, k
        type(storage_helper) :: helper
        class(batched_tensor_contraction), allocatable :: btc
        class(tensor_contraction), allocatable :: tc
        real(real32), dimension(:,:), pointer, contiguous :: ptr
        logical :: res
        integer :: idx

        call batched_tensor_contraction_factory%create(btc, "c(m,n)=a(m,k)*b(k,n)", driver, options, priorities)
        call tensor_contraction_factory%create(tc, "c(m,n)=a(m,k)*b(k,n)")

        m=3; n=5; k=7
        do idx = 1, 10
            allocate(am(m,k), bm(k,n), cm(m,n))
            call random_number(am)
            call random_number(bm)
            cm = 0.0

            call allocate_and_copy_tensor(a(idx)%element, am, memory_type, options, priorities)
            call allocate_and_copy_tensor(b(idx)%element, bm, memory_type, options, priorities)
            call allocate_and_copy_tensor(c(idx)%element, cm, memory_type, options, priorities)
            call allocate_and_copy_tensor(c_original(idx)%element, cm, memory_type, options, priorities)
            deallocate(cm, am, bm)

            call tc%contract(c_original(idx)%element, a(idx)%element, b(idx)%element)

            m=m+1; n=n+1; k=k+1
        end do

        call btc%contract(c, a, b)

        res = .true.
        do idx = 1, 10
            call secure_fortran_pointer_from_tensor(ptr, c_original(idx)%element)
            res = res.and. helper%equal(c(idx)%element%storage, ptr, 1.0e-7)
        end do
        call assertion%equal(prefix//"::Batched contraction of matrices", res)

        do idx = 1, 10
            call a(idx)%cleanup()
            call b(idx)%cleanup()
            call c(idx)%cleanup()
            call c_original(idx)%cleanup()
        end do

        call btc%cleanup()
        call tc%cleanup()
        deallocate(btc, tc)
    end subroutine run_real32

    subroutine run_real64(assertion, prefix, driver, memory_type, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver, memory_type
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        type(tensor_array_element), dimension(10) :: a, b, c, c_original
        real(real64), dimension(:,:), allocatable :: am, bm, cm
        integer :: m, n, k
        type(storage_helper) :: helper
        class(batched_tensor_contraction), allocatable :: btc
        class(tensor_contraction), allocatable :: tc
        real(real64), dimension(:,:), pointer, contiguous :: ptr
        logical :: res
        integer :: idx

        call batched_tensor_contraction_factory%create(btc, "c(m,n)=a(m,k)*b(k,n)", driver, options, priorities)
        call tensor_contraction_factory%create(tc, "c(m,n)=a(m,k)*b(k,n)")

        m=3; n=5; k=7
        do idx = 1, 10
            allocate(am(m,k), bm(k,n), cm(m,n))
            call random_number(am)
            call random_number(bm)
            cm = 0.0d0

            call allocate_and_copy_tensor(a(idx)%element, am, memory_type, options, priorities)
            call allocate_and_copy_tensor(b(idx)%element, bm, memory_type, options, priorities)
            call allocate_and_copy_tensor(c(idx)%element, cm, memory_type, options, priorities)
            call allocate_and_copy_tensor(c_original(idx)%element, cm, memory_type, options, priorities)
            deallocate(cm, am, bm)

            call tc%contract(c_original(idx)%element, a(idx)%element, b(idx)%element)

            m=m+1; n=n+1; k=k+1
        end do

        call btc%contract(c, a, b)

        res = .true.
        do idx = 1, 10
            call secure_fortran_pointer_from_tensor(ptr, c_original(idx)%element)
            res = res.and. helper%equal(c(idx)%element%storage, ptr, 1.0d-13)
        end do
        call assertion%equal(prefix//"::Batched contraction of matrices", res)

        do idx = 1, 10
            call a(idx)%cleanup()
            call b(idx)%cleanup()
            call c(idx)%cleanup()
            call c_original(idx)%cleanup()
        end do

        call btc%cleanup()
        call tc%cleanup()
        deallocate(btc, tc)
    end subroutine run_real64

    subroutine run_complex64(assertion, prefix, driver, memory_type, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver, memory_type
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        type(tensor_array_element), dimension(10) :: a, b, c, c_original
        complex(real32), dimension(:,:), allocatable :: am, bm, cm
        real(real32), dimension(:,:), allocatable :: are, aim, bre, bim
        integer :: m, n, k
        type(storage_helper) :: helper
        class(batched_tensor_contraction), allocatable :: btc
        class(tensor_contraction), allocatable :: tc
        complex(real32), dimension(:,:), pointer, contiguous :: ptr
        logical :: res
        integer :: idx

        call batched_tensor_contraction_factory%create(btc, "c(m,n)=a(m,k)*b(k,n)", driver, options, priorities)
        call tensor_contraction_factory%create(tc, "c(m,n)=a(m,k)*b(k,n)")

        m=3; n=5; k=7
        do idx = 1, 10
            allocate(am(m,k), bm(k,n), cm(m,n))
            allocate(are(m,k), aim(m,k), bre(k,n), bim(k,n))
            call random_number(are)
            call random_number(aim)
            call random_number(bre)
            call random_number(bim)

            am = cmplx(are, aim)
            bm = cmplx(bre, bim)
            deallocate(are, aim, bre, bim)
            cm = (0.0,0.0)

            call allocate_and_copy_tensor(a(idx)%element, am, memory_type, options, priorities)
            call allocate_and_copy_tensor(b(idx)%element, bm, memory_type, options, priorities)
            call allocate_and_copy_tensor(c(idx)%element, cm, memory_type, options, priorities)
            call allocate_and_copy_tensor(c_original(idx)%element, cm, memory_type, options, priorities)
            deallocate(cm, am, bm)

            call tc%contract(c_original(idx)%element, a(idx)%element, b(idx)%element)

            m=m+1; n=n+1; k=k+1
        end do

        call btc%contract(c, a, b)

        res = .true.
        do idx = 1, 10
            call secure_fortran_pointer_from_tensor(ptr, c_original(idx)%element)
            res = res.and. helper%equal(c(idx)%element%storage, ptr, (1.0e-7,1.0e-7))
        end do
        call assertion%equal(prefix//"::Batched contraction of matrices", res)

        do idx = 1, 10
            call a(idx)%cleanup()
            call b(idx)%cleanup()
            call c(idx)%cleanup()
            call c_original(idx)%cleanup()
        end do

        call btc%cleanup()
        call tc%cleanup()
        deallocate(btc, tc)
    end subroutine run_complex64

    subroutine run_complex128(assertion, prefix, driver, memory_type, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver, memory_type
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        type(tensor_array_element), dimension(10) :: a, b, c, c_original
        complex(real64), dimension(:,:), allocatable :: am, bm, cm
        real(real64), dimension(:,:), allocatable :: are, aim, bre, bim
        integer :: m, n, k
        type(storage_helper) :: helper
        class(batched_tensor_contraction), allocatable :: btc
        class(tensor_contraction), allocatable :: tc
        complex(real64), dimension(:,:), pointer, contiguous :: ptr
        logical :: res
        integer :: idx

        call batched_tensor_contraction_factory%create(btc, "c(m,n)=a(m,k)*b(k,n)", driver, options, priorities)
        call tensor_contraction_factory%create(tc, "c(m,n)=a(m,k)*b(k,n)")

        m=3; n=5; k=7
        do idx = 1, 10
            allocate(am(m,k), bm(k,n), cm(m,n))
            allocate(are(m,k), aim(m,k), bre(k,n), bim(k,n))
            call random_number(are)
            call random_number(aim)
            call random_number(bre)
            call random_number(bim)

            am = cmplx(are, aim, kind=real64)
            bm = cmplx(bre, bim, kind=real64)
            deallocate(are, aim, bre, bim)
            cm = (0.0d0,0.0d0)

            call allocate_and_copy_tensor(a(idx)%element, am, memory_type, options, priorities)
            call allocate_and_copy_tensor(b(idx)%element, bm, memory_type, options, priorities)
            call allocate_and_copy_tensor(c(idx)%element, cm, memory_type, options, priorities)
            call allocate_and_copy_tensor(c_original(idx)%element, cm, memory_type, options, priorities)
            deallocate(cm, am, bm)

            call tc%contract(c_original(idx)%element, a(idx)%element, b(idx)%element)

            m=m+1; n=n+1; k=k+1
        end do

        call btc%contract(c, a, b)

        res = .true.
        do idx = 1, 10
            call secure_fortran_pointer_from_tensor(ptr, c_original(idx)%element)
            res = res.and. helper%equal(c(idx)%element%storage, ptr, (1.0d-14,1.0d-14))
        end do
        call assertion%equal(prefix//"::Batched contraction of matrices", res)

        do idx = 1, 10
            call a(idx)%cleanup()
            call b(idx)%cleanup()
            call c(idx)%cleanup()
            call c_original(idx)%cleanup()
        end do

        call btc%cleanup()
        call tc%cleanup()
        deallocate(btc, tc)
    end subroutine run_complex128
end module batched_tensor_contraction_test_helper_module
