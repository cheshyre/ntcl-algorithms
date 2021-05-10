module fortran_siu_driver_module
    use, intrinsic :: iso_fortran_env, only : &
            int64, &
            real32, &
            real64

    use :: data_api, only : stream

    use :: tensor_api, only : &
            scalar, &
            tensor, &
            dt_r32, &
            dt_r64, &
            dt_c64, &
            dt_c128, &
            secure_fortran_pointer_from_tensor, &
            update_remote_tensor_and_release_pointer

    use :: scalar_inline_update_module, only : scalar_inline_update

    implicit none
    private

    public :: fortran_siu_driver

    type, extends(scalar_inline_update) :: fortran_siu_driver
    contains
        procedure :: update => update
        procedure :: synchronize => synchronize
        procedure :: cleanup => cleanup
        procedure :: update_real32 => update_real32
        procedure :: update_real64 => update_real64
        procedure :: update_complex64 => update_complex64
        procedure :: update_complex128 => update_complex128
        procedure :: clear => clear
    end type fortran_siu_driver

    interface fortran_siu_driver
        module procedure constructor
    end interface fortran_siu_driver

contains
    function constructor() result(this)
        type(fortran_siu_driver) :: this

        call this%clear()
    end function constructor

    subroutine update(this, dst, alpha, astream)
        class(fortran_siu_driver), intent(inout) :: this
        class(tensor), intent(inout) :: dst
        type(scalar), intent(in) :: alpha
        type(stream), intent(in), optional :: astream

        select case (dst%datatype)
        case (dt_r32)
            call this%update_real32(dst, alpha%as_real32(), astream)
        case (dt_r64)
            call this%update_real64(dst, alpha%as_real64(), astream)
        case (dt_c64)
            call this%update_complex64(dst, alpha%as_complex64(), astream)
        case (dt_c128)
            call this%update_complex128(dst, alpha%as_complex128(), astream)
        case default
            error stop "fortran_siu_driver::update:Datatype not supported."
        end select
    end subroutine update

    subroutine update_real32(this, dst, alpha, astream)
        class(fortran_siu_driver), intent(inout) :: this
        class(tensor), intent(inout) :: dst
        real(real32), intent(in) :: alpha
        type(stream), intent(in), optional :: astream

        real(real32), dimension(:), pointer, contiguous :: ptr
        integer(int64) :: idx

        call secure_fortran_pointer_from_tensor(ptr, dst, astream)

        do idx = 1, size(ptr, kind=int64)
            ptr(idx) = alpha*ptr(idx)
        end do

        call update_remote_tensor_and_release_pointer(ptr, dst, astream)
    end subroutine update_real32

    subroutine update_real64(this, dst, alpha, astream)
        class(fortran_siu_driver), intent(inout) :: this
        class(tensor), intent(inout) :: dst
        real(real64), intent(in) :: alpha
        type(stream), intent(in), optional :: astream

        real(real64), dimension(:), pointer, contiguous :: ptr
        integer(int64) :: idx

        call secure_fortran_pointer_from_tensor(ptr, dst, astream)

        do idx = 1, size(ptr, kind=int64)
            ptr(idx) = alpha*ptr(idx)
        end do

        call update_remote_tensor_and_release_pointer(ptr, dst, astream)
    end subroutine update_real64

    subroutine update_complex64(this, dst, alpha, astream)
        class(fortran_siu_driver), intent(inout) :: this
        class(tensor), intent(inout) :: dst
        complex(real32), intent(in) :: alpha
        type(stream), intent(in), optional :: astream

        complex(real32), dimension(:), pointer, contiguous :: ptr
        integer(int64) :: idx

        call secure_fortran_pointer_from_tensor(ptr, dst, astream)

        do idx = 1, size(ptr, kind=int64)
            ptr(idx) = alpha*ptr(idx)
        end do

        call update_remote_tensor_and_release_pointer(ptr, dst, astream)
    end subroutine update_complex64

    subroutine update_complex128(this, dst, alpha, astream)
        class(fortran_siu_driver), intent(inout) :: this
        class(tensor), intent(inout) :: dst
        complex(real64), intent(in) :: alpha
        type(stream), intent(in), optional :: astream

        complex(real64), dimension(:), pointer, contiguous :: ptr
        integer(int64) :: idx

        call secure_fortran_pointer_from_tensor(ptr, dst, astream)

        do idx = 1, size(ptr, kind=int64)
            ptr(idx) = alpha*ptr(idx)
        end do

        call update_remote_tensor_and_release_pointer(ptr, dst, astream)
    end subroutine update_complex128

    subroutine synchronize(this, astream)
        class(fortran_siu_driver), intent(inout) :: this
        type(stream), intent(in), optional :: astream

        continue ! Nothing to be done for host algo.
    end subroutine synchronize

    subroutine cleanup(this)
        class(fortran_siu_driver), intent(inout) :: this

        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(fortran_siu_driver), intent(inout) :: this
    end subroutine clear
end module fortran_siu_driver_module
