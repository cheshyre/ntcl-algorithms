module magma_mm_driver_module
    use, intrinsic :: iso_fortran_env, only : &
            real64, &
            real32, &
            int64

    use, intrinsic :: iso_c_binding, only : &
            c_ptr, &
            c_int, &
            c_double, &
            c_float, &
            c_null_ptr

    use :: data_api, only : stream

    use :: tensor_api, only : tensor_c_pointer_converter

    use :: pointer_data_mm_driver_module, only : pointer_data_mm_driver
    use :: magma_module, only : &
            magma, &
            magma_sgemm, &
            magma_dgemm, &
            magma_cgemm, &
            magma_zgemm

    implicit none
    private

    public :: magma_mm_driver

    type, extends(pointer_data_mm_driver):: magma_mm_driver
        type(magma) :: magma
    contains
        procedure :: perform_mm_r32 => perform_mm_r32
        procedure :: perform_mm_r64 => perform_mm_r64
        procedure :: perform_mm_c64 => perform_mm_c64
        procedure :: perform_mm_c128 => perform_mm_c128
        procedure :: initialize => initialize
        procedure :: synchronize => synchronize
        procedure :: cleanup => cleanup
    end type magma_mm_driver

    interface magma_mm_driver
        module procedure constructor_empty
        module procedure constructor
    end interface magma_mm_driver

contains
    function constructor_empty() result(this)
        type(magma_mm_driver) :: this

        call this%initialize()
    end function constructor_empty

    function constructor(converter) result(this)
        type(tensor_c_pointer_converter), intent(in) :: converter
        type(magma_mm_driver) :: this

        this = magma_mm_driver()
        this%converter = converter
    end function constructor

    subroutine initialize(this)
        class(magma_mm_driver), intent(inout) :: this

        this%magma = magma()
    end subroutine initialize

    subroutine perform_mm_r32(this, dst, left, right, m, n, k, ldd, ldl, ldr, &
            alpha, beta, transposed_left, transposed_right, astream)
        class(magma_mm_driver), intent(in) :: this
        type(c_ptr), intent(inout) :: dst
        type(c_ptr), intent(in) :: left, right
        integer(int64), intent(in) :: m, n, k
        integer(int64), intent(in) :: ldd, ldl, ldr
        real(real32), intent(in) :: alpha, beta
        logical, intent(in) :: transposed_left, transposed_right
        type(stream), intent(in), optional :: astream

        integer(c_int) :: transa, transb
        type(c_ptr) :: stream_queue

        transa = 111; transb = 111
        if ( transposed_left) transa = 112
        if ( transposed_right ) transb = 112

        call this%magma%populate_a_queue(stream_queue, astream)

        call magma_sgemm(transa, transb, &
                int(m, c_int), int(n, c_int), int(k, c_int), &
                alpha, left, int(ldl, c_int), right, int(ldr, c_int), &
                beta, dst, int(ldd, c_int), stream_queue )

        call this%magma%destroy_queue_if_necessary(stream_queue, astream)
    end subroutine perform_mm_r32

    subroutine perform_mm_r64(this, dst, left, right, m, n, k, ldd, ldl, ldr, &
            alpha, beta, transposed_left, transposed_right, astream)
        class(magma_mm_driver), intent(in) :: this
        type(c_ptr), intent(inout) :: dst
        type(c_ptr), intent(in) :: left, right
        integer(int64), intent(in) :: m, n, k
        integer(int64), intent(in) :: ldd, ldl, ldr
        real(real64), intent(in) :: alpha, beta
        logical, intent(in) :: transposed_left, transposed_right
        type(stream), intent(in), optional :: astream

        integer(c_int) :: transa, transb
        type(c_ptr) :: stream_queue

        transa = 111; transb = 111
        if ( transposed_left) transa = 112
        if ( transposed_right ) transb = 112

        call this%magma%populate_a_queue(stream_queue, astream)

        call magma_dgemm(transa, transb, &
                int(m, c_int), int(n, c_int), int(k, c_int), &
                alpha, left, int(ldl, c_int), right, int(ldr, c_int), &
                beta, dst, int(ldd, c_int), stream_queue )

        call this%magma%destroy_queue_if_necessary(stream_queue, astream)
    end subroutine perform_mm_r64

    subroutine perform_mm_c64(this, dst, left, right, m, n, k, ldd, ldl, ldr, &
            alpha, beta, transposed_left, transposed_right, astream)
        class(magma_mm_driver), intent(in) :: this
        type(c_ptr), intent(inout) :: dst
        type(c_ptr), intent(in) :: left, right
        integer(int64), intent(in) :: m, n, k
        integer(int64), intent(in) :: ldd, ldl, ldr
        complex(real32), intent(in) :: alpha, beta
        logical, intent(in) :: transposed_left, transposed_right
        type(stream), intent(in), optional :: astream

        integer(c_int) :: transa, transb
        type(c_ptr) :: stream_queue

        transa = 111; transb = 111
        if ( transposed_left) transa = 112
        if ( transposed_right ) transb = 112

        call this%magma%populate_a_queue(stream_queue, astream)

        call magma_cgemm(transa, transb, &
                int(m, c_int), int(n, c_int), int(k, c_int), &
                alpha, left, int(ldl, c_int), right, int(ldr, c_int), &
                beta, dst, int(ldd, c_int), stream_queue )

        call this%magma%destroy_queue_if_necessary(stream_queue, astream)
    end subroutine perform_mm_c64

    subroutine perform_mm_c128(this, dst, left, right, m, n, k, ldd, ldl, ldr, &
            alpha, beta, transposed_left, transposed_right, astream)
        class(magma_mm_driver), intent(in) :: this
        type(c_ptr), intent(inout) :: dst
        type(c_ptr), intent(in) :: left, right
        integer(int64), intent(in) :: m, n, k
        integer(int64), intent(in) :: ldd, ldl, ldr
        complex(real64), intent(in) :: alpha, beta
        logical, intent(in) :: transposed_left, transposed_right
        type(stream), intent(in), optional :: astream

        integer(c_int) :: transa, transb
        type(c_ptr) :: stream_queue

        transa = 111; transb = 111
        if ( transposed_left) transa = 112
        if ( transposed_right ) transb = 112

        call this%magma%populate_a_queue(stream_queue, astream)

        call magma_zgemm(transa, transb, &
                int(m, c_int), int(n, c_int), int(k, c_int), &
                alpha, left, int(ldl, c_int), right, int(ldr, c_int), &
                beta, dst, int(ldd, c_int), stream_queue )

        call this%magma%destroy_queue_if_necessary(stream_queue, astream)
    end subroutine perform_mm_c128

    subroutine synchronize(this, astream)
        class(magma_mm_driver), intent(in) :: this
        type(stream), intent(in), optional :: astream

        integer(c_int) :: error

        error = this%magma%sync(astream)
    end subroutine synchronize

    subroutine cleanup(this)
        class(magma_mm_driver), intent(inout) :: this

        call this%magma%cleanup()
        call this%converter%cleanup()
    end subroutine cleanup
end module magma_mm_driver_module
