module rocblas_mm_driver_module
    use, intrinsic :: iso_fortran_env, only : &
        real32, &
        real64, &
        int64

    use, intrinsic :: iso_c_binding, only : &
            c_ptr, &
            c_int, &
            c_float, &
            c_double, &
            c_null_ptr, &
            c_associated

    use :: data_api, only : &
            stream

    use :: hip_data_plugin, only : hip_synchronize_wrapper, hip_query_stream

    use :: tensor_api, only : tensor_c_pointer_converter

    use :: matrix_multiplication_dev, only : pointer_data_mm_driver

    use :: rocblas_wrapper_module, only : &
            rocblas_create_handle, &
            rocblas_destroy_handle, &
            rocblas_set_stream, &
            rocblas_sgemm, &
            rocblas_dgemm, &
            rocblas_cgemm, &
            rocblas_zgemm, &
            rocblas_operation_none, &
            rocblas_operation_transpose, &
            rocblas_operation_conjugate_transpose, &
            rocblas_status_success

    implicit none
    private

    public :: rocblas_mm_driver

    type, extends(pointer_data_mm_driver):: rocblas_mm_driver
        type(c_ptr) :: handle = c_null_ptr
        logical :: is_initialized = .false.
    contains
        procedure :: perform_mm_r32 => perform_mm_r32
        procedure :: perform_mm_r64 => perform_mm_r64
        procedure :: perform_mm_c64 => perform_mm_c64
        procedure :: perform_mm_c128 => perform_mm_c128
        procedure :: initialize => initialize
        procedure :: cleanup => cleanup
        procedure :: synchronize => synchronize
        procedure :: clear => clear
    end type rocblas_mm_driver

    interface rocblas_mm_driver
        module procedure constructor_empty
        module procedure constructor
    end interface rocblas_mm_driver

contains
    function constructor_empty() result(this)
        type(rocblas_mm_driver) :: this

        call this%clear()
    end function constructor_empty

    function constructor(converter) result(this)
        type(tensor_c_pointer_converter), intent(in) :: converter
        type(rocblas_mm_driver) :: this

        this = rocblas_mm_driver()

        this%converter = converter
        call this%initialize()
    end function constructor

    subroutine initialize(this)
        class(rocblas_mm_driver), intent(inout) :: this

        integer(c_int) :: error

        error = rocblas_create_handle(this%handle)
        if ( error == rocblas_status_success ) then
            this%is_initialized = .true.
        else
             error stop "rocblas_mm_driver::initialize:Could not initialize."
        end if
    end subroutine initialize

    subroutine perform_mm_r32(this, dst, left, right, m, n, k, ldd, ldl, ldr, &
            alpha, beta, transposed_left, transposed_right, astream)
        class(rocblas_mm_driver), intent(in) :: this
        type(c_ptr), intent(inout) :: dst
        type(c_ptr), intent(in) :: left, right
        integer(int64), intent(in) :: m, n, k
        integer(int64), intent(in) :: ldd, ldl, ldr
        real(real32), intent(in) :: alpha, beta
        logical, intent(in) :: transposed_left, transposed_right
        type(stream), intent(in), optional :: astream

        integer(c_int) :: error, transa, transb

        if (.not. this%is_initialized ) &
                error stop "rocblas_mm_driver::perform_mm_r32:Not initialized."
        transa = rocblas_operation_none; transb = rocblas_operation_none
        if ( transposed_left) transa = rocblas_operation_transpose
        if ( transposed_right ) transb = rocblas_operation_transpose

        if ( present( astream ) ) then
            error = rocblas_set_stream(this%handle, astream%sid)
        end if

        error = rocblas_sgemm(this%handle, transa, transb, &
                int(m, c_int), int(n, c_int), int(k, c_int), &
                alpha, left, int(ldl, c_int), right, int(ldr, c_int), &
                beta, dst, int(ldd, c_int) )

        if ( error /= 0 ) &
                error stop "rocblas_mm_driver::perform_mm_r32:Error in call to rocblas_sgemm."
        error = rocblas_set_stream(this%handle, c_null_ptr)
    end subroutine perform_mm_r32

    subroutine perform_mm_r64(this, dst, left, right, m, n, k, ldd, ldl, ldr, &
            alpha, beta, transposed_left, transposed_right, astream)
        class(rocblas_mm_driver), intent(in) :: this
        type(c_ptr), intent(inout) :: dst
        type(c_ptr), intent(in) :: left, right
        integer(int64), intent(in) :: m, n, k
        integer(int64), intent(in) :: ldd, ldl, ldr
        real(real64), intent(in) :: alpha, beta
        logical, intent(in) :: transposed_left, transposed_right
        type(stream), intent(in), optional :: astream

        integer(c_int) :: error, transa, transb

        if (.not. this%is_initialized ) &
                error stop "rocblas_mm_driver::perform_mm_r64:Not initialized."
        transa = rocblas_operation_none; transb = rocblas_operation_none
        if ( transposed_left) transa = rocblas_operation_transpose
        if ( transposed_right ) transb = rocblas_operation_transpose

        if ( present( astream ) ) then
            error = rocblas_set_stream(this%handle, astream%sid)
        end if

        error = rocblas_dgemm(this%handle, transa, transb, &
                int(m, c_int), int(n, c_int), int(k, c_int), &
                alpha, left, int(ldl, c_int), right, int(ldr, c_int), &
                beta, dst, int(ldd, c_int) )

        if ( error /= 0 ) &
                error stop "rocblas_mm_driver::perform_mm_r64:Error in call to rocblas_dgemm."
        error = rocblas_set_stream(this%handle, c_null_ptr)
    end subroutine perform_mm_r64

    subroutine perform_mm_c64(this, dst, left, right, m, n, k, ldd, ldl, ldr, &
            alpha, beta, transposed_left, transposed_right, astream)
        class(rocblas_mm_driver), intent(in) :: this
        type(c_ptr), intent(inout) :: dst
        type(c_ptr), intent(in) :: left, right
        integer(int64), intent(in) :: m, n, k
        integer(int64), intent(in) :: ldd, ldl, ldr
        complex(real32), intent(in) :: alpha, beta
        logical, intent(in) :: transposed_left, transposed_right
        type(stream), intent(in), optional :: astream

        integer(c_int) :: error, transa, transb

        if (.not. this%is_initialized ) &
                error stop "rocblas_mm_driver::perform_mm_c64:Not initialized."
        transa = rocblas_operation_none; transb = rocblas_operation_none
        if ( transposed_left) transa = rocblas_operation_transpose
        if ( transposed_right ) transb = rocblas_operation_transpose

        if ( present( astream ) ) then
            error = rocblas_set_stream(this%handle, astream%sid)
        end if

        error = rocblas_cgemm(this%handle, transa, transb, &
                int(m, c_int), int(n, c_int), int(k, c_int), &
                alpha, left, int(ldl, c_int), right, int(ldr, c_int), &
                beta, dst, int(ldd, c_int) )

        if ( error /= 0 ) &
                error stop "rocblas_mm_driver::perform_mm_c64:Error in call to rocblas_dgemm."
        error = rocblas_set_stream(this%handle, c_null_ptr)
    end subroutine perform_mm_c64

    subroutine perform_mm_c128(this, dst, left, right, m, n, k, ldd, ldl, ldr, &
            alpha, beta, transposed_left, transposed_right, astream)
        class(rocblas_mm_driver), intent(in) :: this
        type(c_ptr), intent(inout) :: dst
        type(c_ptr), intent(in) :: left, right
        integer(int64), intent(in) :: m, n, k
        integer(int64), intent(in) :: ldd, ldl, ldr
        complex(real64), intent(in) :: alpha, beta
        logical, intent(in) :: transposed_left, transposed_right
        type(stream), intent(in), optional :: astream

        integer(c_int) :: error, transa, transb

        if (.not. this%is_initialized ) &
                error stop "rocblas_mm_driver::perform_mm_c128:Not initialized."
        transa = rocblas_operation_none; transb = rocblas_operation_none
        if ( transposed_left) transa = rocblas_operation_transpose
        if ( transposed_right ) transb = rocblas_operation_transpose

        if ( present( astream ) ) then
            error = rocblas_set_stream(this%handle, astream%sid)
        end if

        error = rocblas_zgemm(this%handle, transa, transb, &
                int(m, c_int), int(n, c_int), int(k, c_int), &
                alpha, left, int(ldl, c_int), right, int(ldr, c_int), &
                beta, dst, int(ldd, c_int) )

        if ( error /= 0 ) &
                error stop "rocblas_mm_driver::perform_mm_c128:Error in call to rocblas_dgemm."
        error = rocblas_set_stream(this%handle, c_null_ptr)
    end subroutine perform_mm_c128

    subroutine cleanup(this)
        class(rocblas_mm_driver), intent(inout) :: this

        integer(c_int) :: error

        call this%converter%cleanup()
        if (this%is_initialized) &
                 error = rocblas_destroy_handle(this%handle)

        call this%clear()
    end subroutine cleanup

    subroutine synchronize(this, astream)
        class(rocblas_mm_driver), intent(in) :: this
        type(stream), intent(in), optional :: astream

        integer :: error

        error = hip_synchronize_wrapper(astream)

        if (error /= 0) &
                error stop "rocblas_mm_driver::synchronize:Error in call to synchronize wrapper."
    end subroutine synchronize


    subroutine clear(this)
        class(rocblas_mm_driver), intent(inout) :: this

        this%handle = c_null_ptr
        this%is_initialized = .false.
    end subroutine clear
end module rocblas_mm_driver_module
