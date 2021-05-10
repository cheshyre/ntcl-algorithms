module fortran_data_mm_driver_module
    use, intrinsic :: iso_fortran_env, only : &
            real32, &
            real64, &
            int64

    use :: data_api, only : stream

    use :: tensor_api, only : &
            scalar, &
            matrix, &
            dt_r32, &
            dt_r64, &
            dt_c64, &
            dt_c128, &
            tensor_fortran_converter

    use :: raw_mm_driver_module, only : raw_mm_driver

    implicit none
    private

    public :: fortran_data_mm_driver

    type, abstract, extends(raw_mm_driver) :: fortran_data_mm_driver
        type(tensor_fortran_converter) :: converter
    contains
        procedure :: mm_real32 => mm_real32
        procedure :: mm_real64 => mm_real64
        procedure :: mm_complex64 => mm_complex64
        procedure :: mm_complex128 => mm_complex128
        procedure(perform_mm_r32_interface), deferred :: perform_mm_r32
        procedure(perform_mm_r64_interface), deferred :: perform_mm_r64
        procedure(perform_mm_c64_interface), deferred :: perform_mm_c64
        procedure(perform_mm_c128_interface), deferred :: perform_mm_c128
    end type fortran_data_mm_driver

    abstract interface
        subroutine perform_mm_r32_interface(this, dst, left, right, m, n, k, &
                alpha, beta, transposed_left, transposed_right, astream)
            import :: fortran_data_mm_driver
            import :: real32
            import :: int64
            import :: stream

            class(fortran_data_mm_driver), intent(in) :: this
            real(real32), dimension(:,:), contiguous, intent(inout) :: dst
            real(real32), dimension(:,:), contiguous, intent(in) :: left, right
            integer(int64), intent(in) :: m, n, k
            real(real32), intent(in) :: alpha, beta
            logical, intent(in) :: transposed_left, transposed_right
            type(stream), intent(in), optional :: astream
        end subroutine perform_mm_r32_interface

        subroutine perform_mm_r64_interface(this, dst, left, right, m, n, k, &
                alpha, beta, transposed_left, transposed_right, astream)
            import :: fortran_data_mm_driver
            import :: real64
            import :: int64
            import :: stream

            class(fortran_data_mm_driver), intent(in) :: this
            real(real64), dimension(:,:), contiguous, intent(inout) :: dst
            real(real64), dimension(:,:), contiguous, intent(in) :: left, right
            integer(int64), intent(in) :: m, n, k
            real(real64), intent(in) :: alpha, beta
            logical, intent(in) :: transposed_left, transposed_right
            type(stream), intent(in), optional :: astream
        end subroutine perform_mm_r64_interface

        subroutine perform_mm_c64_interface(this, dst, left, right, m, n, k, &
                alpha, beta, transposed_left, transposed_right, astream)
            import :: fortran_data_mm_driver
            import :: real32
            import :: int64
            import :: stream

            class(fortran_data_mm_driver), intent(in) :: this
            complex(real32), dimension(:,:), contiguous, intent(inout) :: dst
            complex(real32), dimension(:,:), contiguous, intent(in) :: left, right
            integer(int64), intent(in) :: m, n, k
            complex(real32), intent(in) :: alpha, beta
            logical, intent(in) :: transposed_left, transposed_right
            type(stream), intent(in), optional :: astream
        end subroutine perform_mm_c64_interface

        subroutine perform_mm_c128_interface(this, dst, left, right, m, n, k, &
                alpha, beta, transposed_left, transposed_right, astream)
            import :: fortran_data_mm_driver
            import :: real64
            import :: int64
            import :: stream

            class(fortran_data_mm_driver), intent(in) :: this
            complex(real64), dimension(:,:), contiguous, intent(inout) :: dst
            complex(real64), dimension(:,:), contiguous, intent(in) :: left, right
            integer(int64), intent(in) :: m, n, k
            complex(real64), intent(in) :: alpha, beta
            logical, intent(in) :: transposed_left, transposed_right
            type(stream), intent(in), optional :: astream
        end subroutine perform_mm_c128_interface
    end interface
contains
    subroutine mm_real32(this, dst, left, right, m, n, k, alpha, beta, astream)
        class(fortran_data_mm_driver), intent(in) :: this
        type(matrix), intent(inout) :: dst
        type(matrix), intent(in) :: left, right
        integer(int64), intent(in) :: m, n, k
        type(scalar), intent(in), optional :: alpha, beta
        type(stream), intent(in), optional :: astream

        real(real32), dimension(:,:), pointer, contiguous :: d, l, r
        real(real32) :: a, b

        call this%converter%secure_fortran_pointer(d, dst, astream=astream)
        call this%converter%secure_fortran_pointer(l, left, astream=astream)
        call this%converter%secure_fortran_pointer(r, right, astream=astream)
        a = 1.0; b = 0.0
        if ( present(alpha) ) a = alpha%as_real32()
        if ( present(beta) ) b = beta%as_real32()

        call this%perform_mm_r32(d, l, r, m, n, k, a, b, &
                left%is_transposed, right%is_transposed, astream)

        call this%converter%update_remote_and_release_pointer(d, dst, astream)
        call this%converter%release_pointer(l, left, astream)
        call this%converter%release_pointer(r, right, astream)
    end subroutine mm_real32

    subroutine mm_real64(this, dst, left, right, m, n, k, alpha, beta, astream)
        class(fortran_data_mm_driver), intent(in) :: this
        type(matrix), intent(inout) :: dst
        type(matrix), intent(in) :: left, right
        integer(int64), intent(in) :: m, n, k
        type(scalar), intent(in), optional :: alpha, beta
        type(stream), intent(in), optional :: astream

        real(real64), dimension(:,:), pointer, contiguous :: d, l, r
        real(real64) :: a, b

        call this%converter%secure_fortran_pointer(d, dst, astream=astream)
        call this%converter%secure_fortran_pointer(l, left, astream=astream)
        call this%converter%secure_fortran_pointer(r, right, astream=astream)
        a = 1.0d0; b = 0.0d0
        if ( present(alpha) ) a = alpha%as_real64()
        if ( present(beta) ) b = beta%as_real64()

        call this%perform_mm_r64(d, l, r, m, n, k, a, b, &
                left%is_transposed, right%is_transposed, astream)

        call this%converter%update_remote_and_release_pointer(d, dst, astream)
        call this%converter%release_pointer(l, left, astream)
        call this%converter%release_pointer(r, right, astream)
    end subroutine mm_real64

    subroutine mm_complex64(this, dst, left, right, m, n, k, alpha, beta, astream)
        class(fortran_data_mm_driver), intent(in) :: this
        type(matrix), intent(inout) :: dst
        type(matrix), intent(in) :: left, right
        integer(int64), intent(in) :: m, n, k
        type(scalar), intent(in), optional :: alpha, beta
        type(stream), intent(in), optional :: astream

        complex(real32), dimension(:,:), pointer, contiguous :: d, l, r
        complex(real32) :: a, b

        call this%converter%secure_fortran_pointer(d, dst, astream=astream)
        call this%converter%secure_fortran_pointer(l, left, astream=astream)
        call this%converter%secure_fortran_pointer(r, right, astream=astream)
        a = cmplx(1.0, kind=real32); b = cmplx(0.0, kind=real32)
        if ( present(alpha) ) a = alpha%as_complex64()
        if ( present(beta) ) b = beta%as_complex64()

        call this%perform_mm_c64(d, l, r, m, n, k, a, b, &
                left%is_transposed, right%is_transposed, astream)

        call this%converter%update_remote_and_release_pointer(d, dst, astream)
        call this%converter%release_pointer(l, left, astream)
        call this%converter%release_pointer(r, right, astream)
    end subroutine mm_complex64

    subroutine mm_complex128(this, dst, left, right, m, n, k, alpha, beta, astream)
        class(fortran_data_mm_driver), intent(in) :: this
        type(matrix), intent(inout) :: dst
        type(matrix), intent(in) :: left, right
        integer(int64), intent(in) :: m, n, k
        type(scalar), intent(in), optional :: alpha, beta
        type(stream), intent(in), optional :: astream

        complex(real64), dimension(:,:), pointer, contiguous :: d, l, r
        complex(real64) :: a, b

        call this%converter%secure_fortran_pointer(d, dst, astream=astream)
        call this%converter%secure_fortran_pointer(l, left, astream=astream)
        call this%converter%secure_fortran_pointer(r, right, astream=astream)
        a = cmplx(1.0d0, kind=real64); b = cmplx(0.0d0, kind=real64)
        if ( present(alpha) ) a = alpha%as_complex128()
        if ( present(beta) ) b = beta%as_complex128()

        call this%perform_mm_c128(d, l, r, m, n, k, a, b, &
                left%is_transposed, right%is_transposed, astream)

        call this%converter%update_remote_and_release_pointer(d, dst, astream)
        call this%converter%release_pointer(l, left, astream)
        call this%converter%release_pointer(r, right, astream)
    end subroutine mm_complex128
end module fortran_data_mm_driver_module
