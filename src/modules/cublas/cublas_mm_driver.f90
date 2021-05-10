module cublas_mm_driver_module
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
            c_associated, &
            c_loc

    use :: data_api, only : &
            stream

    use :: cuda_data_plugin, only : cuda_synchronize_wrapper

    use :: tensor_api, only : tensor_c_pointer_converter

    use :: matrix_multiplication_dev, only : pointer_data_mm_driver

    use :: cublas_wrapper_module, only : &
        cublascreate, &
        cublasdestroy, &
        cublassgemm, &
        cublasdgemm, &
        cublascgemm, &
        cublaszgemm, &
        cublassetstream

    implicit none
    private

    public :: cublas_mm_driver

    type, extends(pointer_data_mm_driver):: cublas_mm_driver
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
    end type cublas_mm_driver

    interface cublas_mm_driver
        module procedure constructor_empty
        module procedure constructor
    end interface cublas_mm_driver

contains
    function constructor_empty() result(this)
        type(cublas_mm_driver) :: this

        call this%clear()
    end function constructor_empty

    function constructor(converter) result(this)
        type(tensor_c_pointer_converter), intent(in) :: converter
        type(cublas_mm_driver) :: this

        this = cublas_mm_driver()

        this%converter = converter
        call this%initialize()
    end function constructor

    subroutine initialize(this)
        class(cublas_mm_driver), intent(inout) :: this

        integer(c_int) :: error

        error = cublascreate(this%handle)
        if ( error == 0 ) then
            this%is_initialized = .true.
        else
             error stop "cublas_mm_driver::initialize:Could not initialize."
        end if
    end subroutine initialize

    subroutine perform_mm_r32(this, dst, left, right, m, n, k, ldd, ldl, ldr, &
            alpha, beta, transposed_left, transposed_right, astream)
        class(cublas_mm_driver), intent(in) :: this
        type(c_ptr), intent(inout) :: dst
        type(c_ptr), intent(in) :: left, right
        integer(int64), intent(in) :: m, n, k
        integer(int64), intent(in) :: ldd, ldl, ldr
        real(real32), intent(in) :: alpha, beta
        logical, intent(in) :: transposed_left, transposed_right
        type(stream), intent(in), optional :: astream

        integer(c_int) :: error, transa, transb

        if (.not. this%is_initialized ) &
                error stop "cublas_mm_driver::perform_mm_r32:Not initialized."
        transa = 0; transb = 0
        if ( transposed_left) transa = 1
        if ( transposed_right ) transb = 1

        if ( present( astream ) ) then
            error = cublassetstream(this%handle, astream%sid)
        end if

        error = cublassgemm(this%handle, transa, transb, &
                int(m, c_int), int(n, c_int), int(k, c_int), &
                real(alpha, c_float), left, int(ldl, c_int), right, int(ldr, c_int), &
                real(beta, c_float), dst, int(ldd, c_int) )

        if ( error /= 0 ) &
                error stop "cublas_mm_driver::perform_mm_r32:Error in call to cublassgemm."
        error = cublassetstream(this%handle, c_null_ptr)
    end subroutine perform_mm_r32

    subroutine perform_mm_r64(this, dst, left, right, m, n, k, ldd, ldl, ldr, &
            alpha, beta, transposed_left, transposed_right, astream)
        class(cublas_mm_driver), intent(in) :: this
        type(c_ptr), intent(inout) :: dst
        type(c_ptr), intent(in) :: left, right
        integer(int64), intent(in) :: m, n, k
        integer(int64), intent(in) :: ldd, ldl, ldr
        real(real64), intent(in) :: alpha, beta
        logical, intent(in) :: transposed_left, transposed_right
        type(stream), intent(in), optional :: astream

        integer(c_int) :: error, transa, transb

        if (.not. this%is_initialized ) &
                error stop "cublas_mm_driver::perform_mm_r64:Not initialized."
        transa = 0; transb = 0
        if ( transposed_left) transa = 1
        if ( transposed_right ) transb = 1

        if ( present( astream ) ) then
            error = cublassetstream(this%handle, astream%sid)
        end if

        error = cublasdgemm(this%handle, transa, transb, &
                int(m, c_int), int(n, c_int), int(k, c_int), &
                real(alpha, c_double), left, int(ldl, c_int), right, int(ldr, c_int), &
                real(beta, c_double), dst, int(ldd, c_int) )

        if ( error /= 0 ) &
                error stop "cublas_mm_driver::perform_mm_r64:Error in call to cublasdgemm."
        error = cublassetstream(this%handle, c_null_ptr)
    end subroutine perform_mm_r64

    subroutine perform_mm_c64(this, dst, left, right, m, n, k, ldd, ldl, ldr, &
            alpha, beta, transposed_left, transposed_right, astream)
        class(cublas_mm_driver), intent(in) :: this
        type(c_ptr), intent(inout) :: dst
        type(c_ptr), intent(in) :: left, right
        integer(int64), intent(in) :: m, n, k
        integer(int64), intent(in) :: ldd, ldl, ldr
        complex(real32), intent(in) :: alpha, beta
        logical, intent(in) :: transposed_left, transposed_right
        type(stream), intent(in), optional :: astream

        integer(c_int) :: error, transa, transb
        complex(real32), target :: aptr, bptr

        if (.not. this%is_initialized ) &
                error stop "cublas_mm_driver::perform_mm_c64:Not initialized."
        transa = 0; transb = 0
        if ( transposed_left) transa = 1
        if ( transposed_right ) transb = 1

        if ( present( astream ) ) then
            error = cublassetstream(this%handle, astream%sid)
        end if

        aptr = alpha; bptr = beta
        error = cublascgemm(this%handle, transa, transb, &
                int(m, c_int), int(n, c_int), int(k, c_int), &
                c_loc(aptr), left, int(ldl, c_int), right, int(ldr, c_int), &
                c_loc(bptr), dst, int(ldd, c_int) )

        if ( error /= 0 ) &
                error stop "cublas_mm_driver::perform_mm_c64:Error in call to cublasdgemm."
        error = cublassetstream(this%handle, c_null_ptr)
    end subroutine perform_mm_c64

    subroutine perform_mm_c128(this, dst, left, right, m, n, k, ldd, ldl, ldr, &
            alpha, beta, transposed_left, transposed_right, astream)
        class(cublas_mm_driver), intent(in) :: this
        type(c_ptr), intent(inout) :: dst
        type(c_ptr), intent(in) :: left, right
        integer(int64), intent(in) :: m, n, k
        integer(int64), intent(in) :: ldd, ldl, ldr
        complex(real64), intent(in) :: alpha, beta
        logical, intent(in) :: transposed_left, transposed_right
        type(stream), intent(in), optional :: astream

        integer(c_int) :: error, transa, transb
        complex(real64), target :: aptr, bptr

        if (.not. this%is_initialized ) &
                error stop "cublas_mm_driver::perform_mm_c128:Not initialized."
        transa = 0; transb = 0
        if ( transposed_left) transa = 1
        if ( transposed_right ) transb = 1

        if ( present( astream ) ) then
            error = cublassetstream(this%handle, astream%sid)
        end if

        aptr = alpha; bptr = beta
        error = cublaszgemm(this%handle, transa, transb, &
                int(m, c_int), int(n, c_int), int(k, c_int), &
                c_loc(aptr), left, int(ldl, c_int), right, int(ldr, c_int), &
                c_loc(bptr), dst, int(ldd, c_int) )

        if ( error /= 0 ) &
                error stop "cublas_mm_driver::perform_mm_c128:Error in call to cublasdgemm."
        error = cublassetstream(this%handle, c_null_ptr)
    end subroutine perform_mm_c128

    subroutine cleanup(this)
        class(cublas_mm_driver), intent(inout) :: this

        integer(c_int) :: error

        call this%converter%cleanup()
        if (this%is_initialized) &
                 error = cublasdestroy(this%handle)

        call this%clear()
    end subroutine cleanup

    subroutine synchronize(this, astream)
        class(cublas_mm_driver), intent(in) :: this
        type(stream), intent(in), optional :: astream

        integer :: error

        error = cuda_synchronize_wrapper(astream)

        if (error /= 0) &
                error stop "cublas_mm_driver::synchronize:Error in call to synchronize wrapper."
    end subroutine synchronize


    subroutine clear(this)
        class(cublas_mm_driver), intent(inout) :: this

        this%handle = c_null_ptr
        this%is_initialized = .false.
    end subroutine clear
end module cublas_mm_driver_module
