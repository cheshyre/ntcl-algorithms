module intrinsic_mm_driver_module
    use, intrinsic :: iso_fortran_env, only : &
            real32, &
            real64, &
            int64

    use :: data_api, only : stream

    use :: tensor_api, only : tensor_fortran_converter

    use :: fortran_data_mm_driver_module, only : fortran_data_mm_driver

    implicit none
    private

    public :: intrinsic_mm_driver

    type, extends(fortran_data_mm_driver) :: intrinsic_mm_driver
    contains
        procedure :: perform_mm_r32 => perform_mm_r32
        procedure :: perform_mm_r64 => perform_mm_r64
        procedure :: perform_mm_c64 => perform_mm_c64
        procedure :: perform_mm_c128 => perform_mm_c128
        procedure :: synchronize => synchronize
        procedure :: cleanup => cleanup
    end type intrinsic_mm_driver

    interface intrinsic_mm_driver
        module procedure constructor
    end interface intrinsic_mm_driver
contains
    function constructor() result(this)
        type(intrinsic_mm_driver) :: this

        this%converter = tensor_fortran_converter()
    end function constructor

    subroutine perform_mm_r32(this, dst, left, right, m, n, k, &
            alpha, beta, transposed_left, transposed_right, astream)
        class(intrinsic_mm_driver), intent(in) :: this
        real(real32), dimension(:,:), contiguous, intent(inout) :: dst
        real(real32), dimension(:,:), contiguous, intent(in) :: left, right
        integer(int64), intent(in) :: m, n, k
        real(real32), intent(in) :: alpha, beta
        logical, intent(in) :: transposed_left, transposed_right
        type(stream), intent(in), optional :: astream

        if ( all(shape(dst) /= [m, n]) ) &
                error stop "intrinsic_mm_driver::perform_mm_r32:Strided mm not implemented."

        if ( transposed_left .and. transposed_right ) then
            if ( all(shape(left) /= [k, m]) .or. all(shape(right) /= [n, k]) ) &
                    error stop "intrinsic_mm_driver::perform_mm_r32:Strided mm not implemented."

            if ( beta == 0.0 ) then
                dst = alpha*matmul(transpose(left), transpose(right))
            else
                dst = beta*dst + alpha*matmul(transpose(left), transpose(right))
            end if
        else if (transposed_left) then
            if ( all(shape(left) /= [k, m]) .or. all(shape(right) /= [k, n]) ) &
                    error stop "intrinsic_mm_driver::perform_mm_r32:Strided mm not implemented."

            if ( beta == 0.0 ) then
                dst = alpha*matmul(transpose(left), right)
            else
                dst = beta*dst + alpha*matmul(transpose(left), right)
            end if
        else if ( transposed_right ) then
            if ( all(shape(left) /= [m, k]) .or. all(shape(right) /= [n, k]) ) &
                    error stop "intrinsic_mm_driver::perform_mm_r32:Strided mm not implemented."

            if ( beta == 0.0 ) then
                dst = alpha*matmul(left, transpose(right))
            else
                dst = beta*dst + alpha*matmul(left, transpose(right))
            end if
        else
            if ( all(shape(left) /= [m, k]) .or. all(shape(right) /= [k, n]) ) &
                    error stop "intrinsic_mm_driver::perform_mm_r32:Strided mm not implemented."

            if ( beta == 0.0 ) then
                dst = alpha*matmul(left, right)
            else
                dst = beta*dst + alpha*matmul(left, right)
            end if
        end if
    end subroutine perform_mm_r32

    subroutine perform_mm_r64(this, dst, left, right, m, n, k, &
            alpha, beta, transposed_left, transposed_right, astream)
        class(intrinsic_mm_driver), intent(in) :: this
        real(real64), dimension(:,:), contiguous, intent(inout) :: dst
        real(real64), dimension(:,:), contiguous, intent(in) :: left, right
        integer(int64), intent(in) :: m, n, k
        real(real64), intent(in) :: alpha, beta
        logical, intent(in) :: transposed_left, transposed_right
        type(stream), intent(in), optional :: astream

        if ( all(shape(dst) /= [m, n]) ) &
                error stop "intrinsic_mm_driver::perform_mm_r64:Strided mm not implemented."

        if ( transposed_left .and. transposed_right ) then
            if ( all(shape(left) /= [k, m]) .or. all(shape(right) /= [n, k]) ) &
                    error stop "intrinsic_mm_driver::perform_mm_r64:Strided mm not implemented."

            if ( beta == 0.0d0 ) then
                dst = alpha*matmul(transpose(left), transpose(right))
            else
                dst = beta*dst + alpha*matmul(transpose(left), transpose(right))
            end if
        else if (transposed_left) then
            if ( all(shape(left) /= [k, m]) .or. all(shape(right) /= [k, n]) ) &
                    error stop "intrinsic_mm_driver::perform_mm_r64:Strided mm not implemented."

            if ( beta == 0.0d0 ) then
                dst = alpha*matmul(transpose(left), right)
            else
                dst = beta*dst + alpha*matmul(transpose(left), right)
            end if
        else if ( transposed_right ) then
            if ( all(shape(left) /= [m, k]) .or. all(shape(right) /= [n, k]) ) &
                    error stop "intrinsic_mm_driver::perform_mm_r64:Strided mm not implemented."

            if ( beta == 0.0d0 ) then
                dst = alpha*matmul(left, transpose(right))
            else
                dst = beta*dst + alpha*matmul(left, transpose(right))
            end if
        else
            if ( all(shape(left) /= [m, k]) .or. all(shape(right) /= [k, n]) ) &
                    error stop "intrinsic_mm_driver::perform_mm_r64:Strided mm not implemented."

            if ( beta == 0.0d0 ) then
                dst = alpha*matmul(left, right)
            else
                dst = beta*dst + alpha*matmul(left, right)
            end if
        end if
    end subroutine perform_mm_r64

    subroutine perform_mm_c64(this, dst, left, right, m, n, k, &
            alpha, beta, transposed_left, transposed_right, astream)
        class(intrinsic_mm_driver), intent(in) :: this
        complex(real32), dimension(:,:), contiguous, intent(inout) :: dst
        complex(real32), dimension(:,:), contiguous, intent(in) :: left, right
        integer(int64), intent(in) :: m, n, k
        complex(real32), intent(in) :: alpha, beta
        logical, intent(in) :: transposed_left, transposed_right
        type(stream), intent(in), optional :: astream

        if ( all(shape(dst) /= [m, n]) ) &
                error stop "intrinsic_mm_driver::perform_mm_c64:Strided mm not implemented."

        if ( transposed_left .and. transposed_right ) then
            if ( all(shape(left) /= [k, m]) .or. all(shape(right) /= [n, k]) ) &
                    error stop "intrinsic_mm_driver::perform_mm_c64:Strided mm not implemented."

            if ( beta == 0.0 ) then
                dst = alpha*matmul(transpose(left), transpose(right))
            else
                dst = beta*dst + alpha*matmul(transpose(left), transpose(right))
            end if
        else if (transposed_left) then
            if ( all(shape(left) /= [k, m]) .or. all(shape(right) /= [k, n]) ) &
                    error stop "intrinsic_mm_driver::perform_mm_c64:Strided mm not implemented."

            if ( beta == 0.0 ) then
                dst = alpha*matmul(transpose(left), right)
            else
                dst = beta*dst + alpha*matmul(transpose(left), right)
            end if
        else if ( transposed_right ) then
            if ( all(shape(left) /= [m, k]) .or. all(shape(right) /= [n, k]) ) &
                    error stop "intrinsic_mm_driver::perform_mm_c64:Strided mm not implemented."

            if ( beta == 0.0 ) then
                dst = alpha*matmul(left, transpose(right))
            else
                dst = beta*dst + alpha*matmul(left, transpose(right))
            end if
        else
            if ( all(shape(left) /= [m, k]) .or. all(shape(right) /= [k, n]) ) &
                    error stop "intrinsic_mm_driver::perform_mm_c64:Strided mm not implemented."

            if ( beta == 0.0 ) then
                dst = alpha*matmul(left, right)
            else
                dst = beta*dst + alpha*matmul(left, right)
            end if
        end if
    end subroutine perform_mm_c64

    subroutine perform_mm_c128(this, dst, left, right, m, n, k, &
            alpha, beta, transposed_left, transposed_right, astream)
        class(intrinsic_mm_driver), intent(in) :: this
        complex(real64), dimension(:,:), contiguous, intent(inout) :: dst
        complex(real64), dimension(:,:), contiguous, intent(in) :: left, right
        integer(int64), intent(in) :: m, n, k
        complex(real64), intent(in) :: alpha, beta
        logical, intent(in) :: transposed_left, transposed_right
        type(stream), intent(in), optional :: astream

        if ( all(shape(dst) /= [m, n]) ) &
                error stop "intrinsic_mm_driver::perform_mm_c128:Strided mm not implemented."

        if ( transposed_left .and. transposed_right ) then
            if ( all(shape(left) /= [k, m]) .or. all(shape(right) /= [n, k]) ) &
                    error stop "intrinsic_mm_driver::perform_mm_c128:Strided mm not implemented."

            if ( beta == 0.0d0 ) then
                dst = alpha*matmul(transpose(left), transpose(right))
            else
                dst = beta*dst + alpha*matmul(transpose(left), transpose(right))
            end if
        else if (transposed_left) then
            if ( all(shape(left) /= [k, m]) .or. all(shape(right) /= [k, n]) ) &
                    error stop "intrinsic_mm_driver::perform_mm_c128:Strided mm not implemented."

            if ( beta == 0.0d0 ) then
                dst = alpha*matmul(transpose(left), right)
            else
                dst = beta*dst + alpha*matmul(transpose(left), right)
            end if
        else if ( transposed_right ) then
            if ( all(shape(left) /= [m, k]) .or. all(shape(right) /= [n, k]) ) &
                    error stop "intrinsic_mm_driver::perform_mm_c128:Strided mm not implemented."

            if ( beta == 0.0d0 ) then
                dst = alpha*matmul(left, transpose(right))
            else
                dst = beta*dst + alpha*matmul(left, transpose(right))
            end if
        else
            if ( all(shape(left) /= [m, k]) .or. all(shape(right) /= [k, n]) ) &
                    error stop "intrinsic_mm_driver::perform_mm_c128:Strided mm not implemented."

            if ( beta == 0.0d0 ) then
                dst = alpha*matmul(left, right)
            else
                dst = beta*dst + alpha*matmul(left, right)
            end if
        end if
    end subroutine perform_mm_c128

    subroutine cleanup(this)
        class(intrinsic_mm_driver), intent(inout) :: this

        continue
    end subroutine cleanup

    subroutine synchronize(this, astream)
        class(intrinsic_mm_driver), intent(in) :: this
        type(stream), intent(in), optional :: astream

        continue
    end subroutine synchronize
end module intrinsic_mm_driver_module
