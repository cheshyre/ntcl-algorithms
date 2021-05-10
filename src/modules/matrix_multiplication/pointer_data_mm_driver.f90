module pointer_data_mm_driver_module
    use, intrinsic :: iso_fortran_env, only : &
            real32, &
            real64, &
            int64
    use, intrinsic :: iso_c_binding, only : c_ptr

    use :: data_api, only : stream

    use :: tensor_api, only : &
            scalar, &
            matrix, &
            dt_r32, &
            dt_r64, &
            dt_c64, &
            dt_c128, &
            tensor_c_pointer_converter

    use :: raw_mm_driver_module, only : raw_mm_driver

    implicit none
    private

    public :: pointer_data_mm_driver

    type, abstract, extends(raw_mm_driver) :: pointer_data_mm_driver
        type(tensor_c_pointer_converter) :: converter
    contains
        procedure :: mm_real32 => mm_real32
        procedure :: mm_real64 => mm_real64
        procedure :: mm_complex64 => mm_complex64
        procedure :: mm_complex128 => mm_complex128
        procedure(perform_mm_r32_interface), deferred :: perform_mm_r32
        procedure(perform_mm_r64_interface), deferred :: perform_mm_r64
        procedure(perform_mm_c64_interface), deferred :: perform_mm_c64
        procedure(perform_mm_c128_interface), deferred :: perform_mm_c128
        procedure :: cleanup => cleanup
    end type pointer_data_mm_driver

    abstract interface
        subroutine perform_mm_r32_interface(this, dst, left, right, m, n, k, ldd, ldl, ldr, &
                alpha, beta, transposed_left, transposed_right, astream)
            import :: pointer_data_mm_driver
            import :: real32
            import :: int64
            import :: stream
            import :: c_ptr

            class(pointer_data_mm_driver), intent(in) :: this
            type(c_ptr), intent(inout) :: dst
            type(c_ptr), intent(in) :: left, right
            integer(int64), intent(in) :: m, n, k
            integer(int64), intent(in) :: ldd, ldl, ldr
            real(real32), intent(in) :: alpha, beta
            logical, intent(in) :: transposed_left, transposed_right
            type(stream), intent(in), optional :: astream
        end subroutine perform_mm_r32_interface

        subroutine perform_mm_r64_interface(this, dst, left, right, m, n, k, ldd, ldl, ldr, &
                alpha, beta, transposed_left, transposed_right, astream)
            import :: pointer_data_mm_driver
            import :: real64
            import :: int64
            import :: stream
            import :: c_ptr

            class(pointer_data_mm_driver), intent(in) :: this
            type(c_ptr), intent(inout) :: dst
            type(c_ptr), intent(in) :: left, right
            integer(int64), intent(in) :: m, n, k
            integer(int64), intent(in) :: ldd, ldl, ldr
            real(real64), intent(in) :: alpha, beta
            logical, intent(in) :: transposed_left, transposed_right
            type(stream), intent(in), optional :: astream
        end subroutine perform_mm_r64_interface

        subroutine perform_mm_c64_interface(this, dst, left, right, m, n, k, ldd, ldl, ldr, &
                alpha, beta, transposed_left, transposed_right, astream)
            import :: pointer_data_mm_driver
            import :: real32
            import :: int64
            import :: stream
            import :: c_ptr

            class(pointer_data_mm_driver), intent(in) :: this
            type(c_ptr), intent(inout) :: dst
            type(c_ptr), intent(in) :: left, right
            integer(int64), intent(in) :: m, n, k
            integer(int64), intent(in) :: ldd, ldl, ldr
            complex(real32), intent(in) :: alpha, beta
            logical, intent(in) :: transposed_left, transposed_right
            type(stream), intent(in), optional :: astream
        end subroutine perform_mm_c64_interface

        subroutine perform_mm_c128_interface(this, dst, left, right, m, n, k, ldd, ldl, ldr, &
                alpha, beta, transposed_left, transposed_right, astream)
            import :: pointer_data_mm_driver
            import :: real64
            import :: int64
            import :: stream
            import :: c_ptr

            class(pointer_data_mm_driver), intent(in) :: this
            type(c_ptr), intent(inout) :: dst
            type(c_ptr), intent(in) :: left, right
            integer(int64), intent(in) :: m, n, k
            integer(int64), intent(in) :: ldd, ldl, ldr
            complex(real64), intent(in) :: alpha, beta
            logical, intent(in) :: transposed_left, transposed_right
            type(stream), intent(in), optional :: astream
        end subroutine perform_mm_c128_interface
    end interface
contains
    subroutine mm_real32(this, dst, left, right, m, n, k, alpha, beta, astream)
        class(pointer_data_mm_driver), intent(in) :: this
        type(matrix), intent(inout) :: dst
        type(matrix), intent(in) :: left, right
        integer(int64), intent(in) :: m, n, k
        type(scalar), intent(in), optional :: alpha, beta
        type(stream), intent(in), optional :: astream

        type(c_ptr) :: d, l, r
        real(real32) :: a, b

        call this%converter%secure_pointer(dst, d, astream)
        call this%converter%secure_pointer(left, l, astream)
        call this%converter%secure_pointer(right, r, astream)

        a = 1.0; b = 0.0
        if ( present(alpha) ) a = alpha%as_real32()
        if ( present(beta) ) b = beta%as_real32()

        call this%perform_mm_r32(d, l, r, m, n, k, dst%dims(1), &
                left%dims(1), right%dims(1), a, b, &
                left%is_transposed, right%is_transposed, astream)

        call this%converter%update_and_release(dst, d, astream)
        call this%converter%release(left, l, astream)
        call this%converter%release(right, r, astream)
    end subroutine mm_real32

    subroutine mm_real64(this, dst, left, right, m, n, k, alpha, beta, astream)
        class(pointer_data_mm_driver), intent(in) :: this
        type(matrix), intent(inout) :: dst
        type(matrix), intent(in) :: left, right
        integer(int64), intent(in) :: m, n, k
        type(scalar), intent(in), optional :: alpha, beta
        type(stream), intent(in), optional :: astream

        type(c_ptr) :: d, l, r
        real(real64) :: a, b

        call this%converter%secure_pointer(dst, d, astream)
        call this%converter%secure_pointer(left, l, astream)
        call this%converter%secure_pointer(right, r, astream)

        a = 1.0d0; b = 0.0d0
        if ( present(alpha) ) a = alpha%as_real64()
        if ( present(beta) ) b = beta%as_real64()

        call this%perform_mm_r64(d, l, r, m, n, k, dst%dims(1), &
                left%dims(1), right%dims(1), a, b, &
                left%is_transposed, right%is_transposed, astream)

        call this%converter%update_and_release(dst, d, astream)
        call this%converter%release(left, l, astream)
        call this%converter%release(right, r, astream)
    end subroutine mm_real64

    subroutine mm_complex64(this, dst, left, right, m, n, k, alpha, beta, astream)
        class(pointer_data_mm_driver), intent(in) :: this
        type(matrix), intent(inout) :: dst
        type(matrix), intent(in) :: left, right
        integer(int64), intent(in) :: m, n, k
        type(scalar), intent(in), optional :: alpha, beta
        type(stream), intent(in), optional :: astream

        type(c_ptr) :: d, l, r
        complex(real32) :: a, b

        call this%converter%secure_pointer(dst, d, astream)
        call this%converter%secure_pointer(left, l, astream)
        call this%converter%secure_pointer(right, r, astream)

        a = (1.0, 0.0); b = (0.0, 0.0)
        if ( present(alpha) ) a = alpha%as_complex64()
        if ( present(beta) ) b = beta%as_complex64()

        call this%perform_mm_c64(d, l, r, m, n, k, dst%dims(1), &
                left%dims(1), right%dims(1), a, b, &
                left%is_transposed, right%is_transposed, astream)

        call this%converter%update_and_release(dst, d, astream)
        call this%converter%release(left, l, astream)
        call this%converter%release(right, r, astream)
    end subroutine mm_complex64

    subroutine mm_complex128(this, dst, left, right, m, n, k, alpha, beta, astream)
        class(pointer_data_mm_driver), intent(in) :: this
        type(matrix), intent(inout) :: dst
        type(matrix), intent(in) :: left, right
        integer(int64), intent(in) :: m, n, k
        type(scalar), intent(in), optional :: alpha, beta
        type(stream), intent(in), optional :: astream

        type(c_ptr) :: d, l, r
        complex(real64) :: a, b

        call this%converter%secure_pointer(dst, d, astream)
        call this%converter%secure_pointer(left, l, astream)
        call this%converter%secure_pointer(right, r, astream)

        a = (1.0d0, 0.0d0); b = (0.0d0, 0.0d0)
        if ( present(alpha) ) a = alpha%as_complex128()
        if ( present(beta) ) b = beta%as_complex128()

        call this%perform_mm_c128(d, l, r, m, n, k, dst%dims(1), &
                left%dims(1), right%dims(1), a, b, &
                left%is_transposed, right%is_transposed, astream)

        call this%converter%update_and_release(dst, d, astream)
        call this%converter%release(left, l, astream)
        call this%converter%release(right, r, astream)
    end subroutine mm_complex128

    subroutine cleanup(this)
        class(pointer_data_mm_driver), intent(inout) :: this

        call this%converter%cleanup()
    end subroutine cleanup
end module pointer_data_mm_driver_module
