module batched_tensor_permute_test_helper_module
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
            tensor_array_element, &
            allocate_and_copy_tensor, &
            secure_fortran_pointer_from_tensor

    use :: algorithms_api, only : &
            batched_tensor_permute, &
            batched_tensor_permute_factory, &
            tensor_permute, &
            tensor_permute_factory

    implicit none
    private

    public :: batched_tensor_permute_test_helper

    type :: batched_tensor_permute_test_helper
    contains
        procedure :: run => run
        procedure, nopass :: run_real32 => run_real32
        procedure, nopass :: run_real64 => run_real64
        procedure, nopass :: run_complex64 => run_complex64
        procedure, nopass :: run_complex128 => run_complex128
    end type batched_tensor_permute_test_helper
contains
    subroutine run(this, assertion, prefix, driver, memory_type, options, priorities)
        class(batched_tensor_permute_test_helper), intent(in) :: this
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

        type(tensor_array_element), dimension(10) :: ma, mc, mc_original
        type(tensor_array_element), dimension(10) :: ta, tc, tc_original
        real(real32), dimension(:,:), allocatable :: mam, mcm
        real(real32), dimension(:,:,:,:), allocatable :: tam, tcm
        integer :: d1, d2, d3, d4
        type(storage_helper) :: helper
        class(batched_tensor_permute), allocatable :: btp
        class(tensor_permute), allocatable :: tp
        real(real32), dimension(:,:), pointer, contiguous :: ptr
        real(real32), dimension(:,:,:,:), pointer, contiguous :: ptr2
        logical :: res
        integer :: idx

        call batched_tensor_permute_factory%create(btp, driver, options, priorities)
        call tensor_permute_factory%create(tp)

        d1 = 7; d2 = 7; d3 = 11; d4 = 13
        do idx = 1, 10
            allocate(mam(d1, d2), mcm(d2,d1))
            allocate(tam(d1, d2, d3, d4), tcm(d2,d1,d4,d3))
            call random_number(mam)
            call random_number(tam)
            mcm = 0.0
            tcm = 0.0

            call allocate_and_copy_tensor(ma(idx)%element, mam, memory_type, options, priorities)
            call allocate_and_copy_tensor(mc(idx)%element, mcm, memory_type, options, priorities)
            call allocate_and_copy_tensor(mc_original(idx)%element, mcm, memory_type, options, priorities)
            deallocate(mcm, mam)

            call tp%permute(ma(idx)%element, mc_original(idx)%element, [2,1])

            call allocate_and_copy_tensor(ta(idx)%element, tam, memory_type, options, priorities)
            call allocate_and_copy_tensor(tc(idx)%element, tcm, memory_type, options, priorities)
            call allocate_and_copy_tensor(tc_original(idx)%element, tcm, memory_type, options, priorities)
            deallocate(tcm, tam)

            call tp%permute(ta(idx)%element, tc_original(idx)%element, [2,1,4,3])
            d1 = d1 + 1; d2 = d2+1; d3 = d3+1; d4=d4+1
        end do

        call btp%btp(ma, mc, [2,1])
        call btp%btp(ta, tc, [2,1,4,3])

        res = .true.
        do idx = 1, 10
            call secure_fortran_pointer_from_tensor(ptr, mc_original(idx)%element)
            res = res.and. helper%equal(mc(idx)%element%storage, ptr, 1.0e-7)
        end do
        call assertion%equal(prefix//"::Batched permute of matrices", res)

        res = .true.
        do idx = 1, 10
            call secure_fortran_pointer_from_tensor(ptr2, tc_original(idx)%element)
            res = res.and. helper%equal(tc(idx)%element%storage, ptr2, 1.0e-7)
        end do
        call assertion%equal(prefix//"::Batched permute of tensor_rank4", res)

        do idx = 1, 10
            call ma(idx)%cleanup()
            call mc(idx)%cleanup()
            call ta(idx)%cleanup()
            call tc(idx)%cleanup()
            call mc_original(idx)%cleanup()
            call tc_original(idx)%cleanup()
        end do

        call btp%cleanup()
        call tp%cleanup()
        deallocate(btp, tp)
    end subroutine run_real32

    subroutine run_real64(assertion, prefix, driver, memory_type, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver, memory_type
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        type(tensor_array_element), dimension(10) :: ma, mc, mc_original
        type(tensor_array_element), dimension(10) :: ta, tc, tc_original
        real(real64), dimension(:,:), allocatable :: mam, mcm
        real(real64), dimension(:,:,:,:), allocatable :: tam, tcm
        integer :: d1, d2, d3, d4
        type(storage_helper) :: helper
        class(batched_tensor_permute), allocatable :: btp
        class(tensor_permute), allocatable :: tp
        real(real64), dimension(:,:), pointer, contiguous :: ptr
        real(real64), dimension(:,:,:,:), pointer, contiguous :: ptr2
        logical :: res
        integer :: idx

        call batched_tensor_permute_factory%create(btp, driver, options, priorities)
        call tensor_permute_factory%create(tp)

        d1 = 7; d2 = 7; d3 = 11; d4 = 13
        do idx = 1, 10
            allocate(mam(d1, d2), mcm(d2,d1))
            allocate(tam(d1, d2, d3, d4), tcm(d2,d1,d4,d3))
            call random_number(mam)
            call random_number(tam)
            mcm = 0.0
            tcm = 0.0

            call allocate_and_copy_tensor(ma(idx)%element, mam, memory_type, options, priorities)
            call allocate_and_copy_tensor(mc(idx)%element, mcm, memory_type, options, priorities)
            call allocate_and_copy_tensor(mc_original(idx)%element, mcm, memory_type, options, priorities)
            deallocate(mcm, mam)

            call tp%permute(ma(idx)%element, mc_original(idx)%element, [2,1])

            call allocate_and_copy_tensor(ta(idx)%element, tam, memory_type, options, priorities)
            call allocate_and_copy_tensor(tc(idx)%element, tcm, memory_type, options, priorities)
            call allocate_and_copy_tensor(tc_original(idx)%element, tcm, memory_type, options, priorities)
            deallocate(tcm, tam)

            call tp%permute(ta(idx)%element, tc_original(idx)%element, [2,1,4,3])
            d1 = d1 + 1; d2 = d2+1; d3 = d3+1; d4=d4+1
        end do

        call btp%btp(ma, mc, [2,1])
        call btp%btp(ta, tc, [2,1,4,3])

        res = .true.
        do idx = 1, 10
            call secure_fortran_pointer_from_tensor(ptr, mc_original(idx)%element)
            res = res.and. helper%equal(mc(idx)%element%storage, ptr, 1.0d-15)
        end do
        call assertion%equal(prefix//"::Batched permute of matrices", res)

        res = .true.
        do idx = 1, 10
            call secure_fortran_pointer_from_tensor(ptr2, tc_original(idx)%element)
            res = res.and. helper%equal(tc(idx)%element%storage, ptr2, 1.0d-15)
        end do
        call assertion%equal(prefix//"::Batched permute of tensor_rank4", res)

        do idx = 1, 10
            call ma(idx)%cleanup()
            call mc(idx)%cleanup()
            call ta(idx)%cleanup()
            call tc(idx)%cleanup()
            call mc_original(idx)%cleanup()
            call tc_original(idx)%cleanup()
        end do

        call btp%cleanup()
        call tp%cleanup()
        deallocate(btp, tp)
    end subroutine run_real64

    subroutine run_complex64(assertion, prefix, driver, memory_type, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver, memory_type
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        type(tensor_array_element), dimension(10) :: ma, mc, mc_original
        type(tensor_array_element), dimension(10) :: ta, tc, tc_original
        complex(real32), dimension(:,:), allocatable :: mam, mcm
        complex(real32), dimension(:,:,:,:), allocatable :: tam, tcm
        real(real32), dimension(:,:), allocatable :: mar, mai
        real(real32), dimension(:,:,:,:), allocatable :: tar, tai
        integer :: d1, d2, d3, d4
        type(storage_helper) :: helper
        class(batched_tensor_permute), allocatable :: btp
        class(tensor_permute), allocatable :: tp
        complex(real32), dimension(:,:), pointer, contiguous :: ptr
        complex(real32), dimension(:,:,:,:), pointer, contiguous :: ptr2
        logical :: res
        integer :: idx

        call batched_tensor_permute_factory%create(btp, driver, options, priorities)
        call tensor_permute_factory%create(tp)

        d1 = 7; d2 = 7; d3 = 11; d4 = 13
        do idx = 1, 10
            allocate(mar(d1, d2), mai(d1,d2), mcm(d2,d1))
            allocate(tar(d1, d2, d3, d4), tai(d1, d2, d3, d4), tcm(d2,d1,d4,d3))
            call random_number(mar)
            call random_number(mai)
            allocate(mam(d1,d2))
            mam = cmplx(mar, mai)
            call random_number(tar)
            call random_number(tai)
            allocate(tam(d1, d2, d3, d4))
            tam = cmplx(tar, tai)
            mcm = (0.0,0.0)
            tcm = (0.0,0.0)

            call allocate_and_copy_tensor(ma(idx)%element, mam, memory_type, options, priorities)
            call allocate_and_copy_tensor(mc(idx)%element, mcm, memory_type, options, priorities)
            call allocate_and_copy_tensor(mc_original(idx)%element, mcm, memory_type, options, priorities)
            deallocate(mcm, mam, mar, mai)

            call tp%permute(ma(idx)%element, mc_original(idx)%element, [2,1])

            call allocate_and_copy_tensor(ta(idx)%element, tam, memory_type, options, priorities)
            call allocate_and_copy_tensor(tc(idx)%element, tcm, memory_type, options, priorities)
            call allocate_and_copy_tensor(tc_original(idx)%element, tcm, memory_type, options, priorities)
            deallocate(tcm, tam, tar, tai)

            call tp%permute(ta(idx)%element, tc_original(idx)%element, [2,1,4,3])
            d1 = d1 + 1; d2 = d2+1; d3 = d3+1; d4=d4+1
        end do

        call btp%btp(ma, mc, [2,1])
        call btp%btp(ta, tc, [2,1,4,3])

        res = .true.
        do idx = 1, 10
            call secure_fortran_pointer_from_tensor(ptr, mc_original(idx)%element)
            res = res.and. helper%equal(mc(idx)%element%storage, ptr, (1.0e-7, 1.0e-7))
        end do
        call assertion%equal(prefix//"::Batched permute of matrices", res)

        res = .true.
        do idx = 1, 10
            call secure_fortran_pointer_from_tensor(ptr2, tc_original(idx)%element)
            res = res.and. helper%equal(tc(idx)%element%storage, ptr2, (1.0e-7, 1.0e-7))
        end do
        call assertion%equal(prefix//"::Batched permute of tensor_rank4", res)

        do idx = 1, 10
            call ma(idx)%cleanup()
            call mc(idx)%cleanup()
            call ta(idx)%cleanup()
            call tc(idx)%cleanup()
            call mc_original(idx)%cleanup()
            call tc_original(idx)%cleanup()
        end do

        call btp%cleanup()
        call tp%cleanup()
        deallocate(btp, tp)
    end subroutine run_complex64

    subroutine run_complex128(assertion, prefix, driver, memory_type, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver, memory_type
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        type(tensor_array_element), dimension(10) :: ma, mc, mc_original
        type(tensor_array_element), dimension(10) :: ta, tc, tc_original
        complex(real64), dimension(:,:), allocatable :: mam, mcm
        complex(real64), dimension(:,:,:,:), allocatable :: tam, tcm
        real(real64), dimension(:,:), allocatable :: mar, mai
        real(real64), dimension(:,:,:,:), allocatable :: tar, tai
        integer :: d1, d2, d3, d4
        type(storage_helper) :: helper
        class(batched_tensor_permute), allocatable :: btp
        class(tensor_permute), allocatable :: tp
        complex(real64), dimension(:,:), pointer, contiguous :: ptr
        complex(real64), dimension(:,:,:,:), pointer, contiguous :: ptr2
        logical :: res
        integer :: idx

        call batched_tensor_permute_factory%create(btp, driver, options, priorities)
        call tensor_permute_factory%create(tp)

        d1 = 7; d2 = 7; d3 = 11; d4 = 13
        do idx = 1, 10
            allocate(mar(d1, d2), mai(d1,d2), mcm(d2,d1))
            allocate(tar(d1, d2, d3, d4), tai(d1, d2, d3, d4), tcm(d2,d1,d4,d3))
            call random_number(mar)
            call random_number(mai)
            allocate(mam(d1,d2))
            mam = cmplx(mar, mai, kind=real64)
            call random_number(tar)
            call random_number(tai)
            allocate(tam(d1, d2, d3, d4))
            tam = cmplx(tar, tai, kind=real64)
            mcm = (0.0d0,0.0d0)
            tcm = (0.0d0,0.0d0)

            call allocate_and_copy_tensor(ma(idx)%element, mam, memory_type, options, priorities)
            call allocate_and_copy_tensor(mc(idx)%element, mcm, memory_type, options, priorities)
            call allocate_and_copy_tensor(mc_original(idx)%element, mcm, memory_type, options, priorities)
            deallocate(mcm, mam, mar, mai)

            call tp%permute(ma(idx)%element, mc_original(idx)%element, [2,1])

            call allocate_and_copy_tensor(ta(idx)%element, tam, memory_type, options, priorities)
            call allocate_and_copy_tensor(tc(idx)%element, tcm, memory_type, options, priorities)
            call allocate_and_copy_tensor(tc_original(idx)%element, tcm, memory_type, options, priorities)
            deallocate(tcm, tam, tar, tai)

            call tp%permute(ta(idx)%element, tc_original(idx)%element, [2,1,4,3])
            d1 = d1 + 1; d2 = d2+1; d3 = d3+1; d4=d4+1
        end do

        call btp%btp(ma, mc, [2,1])
        call btp%btp(ta, tc, [2,1,4,3])

        res = .true.
        do idx = 1, 10
            call secure_fortran_pointer_from_tensor(ptr, mc_original(idx)%element)
            res = res.and. helper%equal(mc(idx)%element%storage, ptr, (1.0d-14, 1.0d-14))
        end do
        call assertion%equal(prefix//"::Batched permute of matrices", res)

        res = .true.
        do idx = 1, 10
            call secure_fortran_pointer_from_tensor(ptr2, tc_original(idx)%element)
            res = res.and. helper%equal(tc(idx)%element%storage, ptr2, (1.0d-14, 1.0d-14))
        end do
        call assertion%equal(prefix//"::Batched permute of tensor_rank4", res)

        do idx = 1, 10
            call ma(idx)%cleanup()
            call mc(idx)%cleanup()
            call ta(idx)%cleanup()
            call tc(idx)%cleanup()
            call mc_original(idx)%cleanup()
            call tc_original(idx)%cleanup()
        end do

        call btp%cleanup()
        call tp%cleanup()
        deallocate(btp, tp)
    end subroutine run_complex128
end module batched_tensor_permute_test_helper_module
