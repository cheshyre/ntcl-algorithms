module cuda_loops_mm_driver_module
    use, intrinsic :: iso_fortran_env, only : &
            real32, &
            real64, &
            int64

    use, intrinsic :: iso_c_binding, only : & 
            c_ptr, &
            c_int, &
            c_float, &
            c_double, &
            c_loc

    use :: data_api, only : stream

    use :: cuda_data_plugin, only : cuda_synchronize_wrapper

    use :: tensor_api, only: tensor_c_pointer_converter
 
    use :: matrix_multiplication_dev, only : & 
            pointer_data_mm_driver

    implicit none 
    private
   
    public :: cuda_loops_mm_driver
    public :: cuda_mmp_execute_r32
    public :: cuda_mmp_execute_r64
    public :: cuda_mmp_execute_c64
    public :: cuda_mmp_execute_c128
 
    integer, parameter, public :: CUDA_LOOPS_BLAS_SUCCESS = 0 

    type, extends(pointer_data_mm_driver) :: cuda_loops_mm_driver
    contains
        procedure :: perform_mm_r32 => perform_mm_r32
        procedure :: perform_mm_r64 => perform_mm_r64
        procedure :: perform_mm_c64 => perform_mm_c64
        procedure :: perform_mm_c128 => perform_mm_c128
        procedure :: synchronize => synchronize
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type cuda_loops_mm_driver

    interface cuda_loops_mm_driver
        module procedure constructor_empty
        module procedure constructor
    end interface cuda_loops_mm_driver


    interface 
      integer(c_int) function cuda_mmp_execute_r32( alpha, beta, m, n, k, a, b, c) &
                            bind(c, name="cuda_mmp_execute_r32")
          import :: c_ptr, c_float, c_int
          real(c_float), value :: alpha, beta
          integer(c_int), value :: m, n, k
          type(c_ptr), value :: a, b, c
      end function cuda_mmp_execute_r32

      integer(c_int) function cuda_mmp_execute_r64( alpha, beta, m, n, k, a, b, c) &
                           bind(c, name="cuda_mmp_execute_r64")
          import :: c_ptr, c_double, c_int
          real(c_double), value :: alpha, beta
          integer(c_int), value :: m, n, k
          type(c_ptr), value :: a, b, c
      end function cuda_mmp_execute_r64

      integer(c_int) function cuda_mmp_execute_c64( alpha, beta, m, n, k, a, b, c) &
                           bind(c, name="cuda_mmp_execute_c64")
          import :: c_ptr, c_float, c_int
          integer(c_int), value :: m, n, k
          ! Passed by reference due to bug in gcc on P9 for single precision complex numbers.
          complex(c_float) :: alpha, beta
          type(c_ptr), value :: a, b, c
      end function cuda_mmp_execute_c64

      integer(c_int) function cuda_mmp_execute_c128( alpha, beta, m, n, k, a, b, c) &
                           bind(c, name="cuda_mmp_execute_c128")
          import :: c_ptr, c_double, c_int
          integer(c_int), value :: m, n, k
          complex(c_double), value :: alpha, beta
          type(c_ptr), value :: a, b, c
      end function cuda_mmp_execute_c128
    end interface

contains
    function constructor_empty() result(this)
        type(cuda_loops_mm_driver) :: this
      
        call this%clear()
    end function constructor_empty

    function constructor(converter) result(this)
        type(tensor_c_pointer_converter), intent(in) :: converter
        type(cuda_loops_mm_driver) :: this

        this = cuda_loops_mm_driver()
        this%converter = converter
    end function constructor

    subroutine perform_mm_r32(this, dst, left, right, m, n, k, ldd, ldl, ldr, &
            alpha, beta, transposed_left, transposed_right, astream)
        class(cuda_loops_mm_driver), intent(in) :: this
        type(c_ptr), intent(inout) :: dst
        type(c_ptr), intent(in) :: left, right
        integer(int64), intent(in) :: m, n, k
        integer(int64), intent(in) :: ldd, ldl, ldr
        real(real32), intent(in) :: alpha, beta
        logical, intent(in) :: transposed_left, transposed_right
        type(stream), intent(in), optional :: astream

        integer(c_int) :: error

        if ( transposed_left .or. transposed_right ) &
                error stop "cuda_loops_mm_driver::perform_mm_r32:Transposed matrices not implemented."
        if ( m /= ldd .or. m /= ldl .or. k /= ldr) &
                error stop "cuda_loops_mm_driver::perform_mm_r32:Strided mm not implemented."
        
        error = cuda_mmp_execute_r32(real(alpha, c_float), real(beta, c_float), &
               int(m, c_int), int(n, c_int), int(k, c_int), left, right, dst)

        if ( error /= cuda_loops_blas_success ) &
                error stop & 
                    "cuda_loops_mm_driver::perform_mm_r32:Error in call to cuda_mmp_execute_r32."
    end subroutine perform_mm_r32

    subroutine perform_mm_r64(this, dst, left, right, m, n, k, ldd, ldl, ldr, &
            alpha, beta, transposed_left, transposed_right, astream)
        class(cuda_loops_mm_driver), intent(in) :: this
        type(c_ptr), intent(inout) :: dst
        type(c_ptr), intent(in) :: left, right
        integer(int64), intent(in) :: m, n, k
        integer(int64), intent(in) :: ldd, ldl, ldr
        real(real64), intent(in) :: alpha, beta
        logical, intent(in) :: transposed_left, transposed_right
        type(stream), intent(in), optional :: astream

        integer(c_int) :: error

        if ( transposed_left .or. transposed_right ) &
                error stop "cuda_loops_mm_driver::perform_mm_r64:Transposed matrices not implemented."
        if ( m /= ldd .or. m /= ldl .or. k /= ldr) &
                error stop "cuda_loops_mm_driver::perform_mm_r64:Strided mm not implemented."
        
        error = cuda_mmp_execute_r64(real(alpha, c_double), real(beta, c_double), &
               int(m, c_int), int(n, c_int), int(k, c_int), left, right, dst)

        if ( error /= cuda_loops_blas_success ) &
                error stop & 
                    "cuda_loops_mm_driver::perform_mm_r64:Error in call to cuda_mmp_execute_r64."
    end subroutine perform_mm_r64

    subroutine perform_mm_c64(this, dst, left, right, m, n, k, ldd, ldl, ldr, &
            alpha, beta, transposed_left, transposed_right, astream)
        class(cuda_loops_mm_driver), intent(in) :: this
        type(c_ptr), intent(inout) :: dst
        type(c_ptr), intent(in) :: left, right
        integer(int64), intent(in) :: m, n, k
        integer(int64), intent(in) :: ldd, ldl, ldr
        complex(real32), intent(in) :: alpha, beta
        logical, intent(in) :: transposed_left, transposed_right
        type(stream), intent(in), optional :: astream

        integer(c_int) :: error

        if ( transposed_left .or. transposed_right ) &
                error stop "cuda_loops_mm_driver::perform_mm_c64:Transposed matrices not implemented."
        if ( m /= ldd .or. m /= ldl .or. k /= ldr) &
                error stop "cuda_loops_mm_driver::perform_mm_c64:Strided mm not implemented."

        error = cuda_mmp_execute_c64(alpha,beta,&
               int(m, c_int), int(n, c_int), int(k, c_int), left, right, dst)

        if ( error /= cuda_loops_blas_success ) &
                error stop & 
                    "cuda_loops_mm_driver::perform_mm_c64:Error in call to cuda_mmp_execute_c64."
    end subroutine perform_mm_c64

   subroutine perform_mm_c128(this, dst, left, right, m, n, k, ldd, ldl, ldr, &
            alpha, beta, transposed_left, transposed_right, astream)
        class(cuda_loops_mm_driver), intent(in) :: this
        type(c_ptr), intent(inout) :: dst
        type(c_ptr), intent(in) :: left, right
        integer(int64), intent(in) :: m, n, k
        integer(int64), intent(in) :: ldd, ldl, ldr
        complex(c_double), intent(in) :: alpha, beta
        logical, intent(in) :: transposed_left, transposed_right
        type(stream), intent(in), optional :: astream

        integer(c_int) :: error

        if ( transposed_left .or. transposed_right ) &
                error stop "cuda_loops_mm_driver::perform_mm_c128:Transposed matrices not implemented."
        if ( m /= ldd .or. m /= ldl .or. k /= ldr) &
                error stop "cuda_loops_mm_driver::perform_mm_c128:Strided mm not implemented."
       
        error = cuda_mmp_execute_c128(alpha, beta, &
               int(m, c_int), int(n, c_int), int(k, c_int), left, right, dst)

        if ( error /= cuda_loops_blas_success ) &
                error stop & 
                    "cuda_loops_mm_driver::perform_mm_c128:Error in call to cuda_mmp_execute_c128."
    end subroutine perform_mm_c128

    subroutine synchronize(this, astream)
        class(cuda_loops_mm_driver), intent(in) :: this
        type(stream), intent(in), optional :: astream

        integer :: error
        
        error = cuda_synchronize_wrapper(astream)

        if (error /= 0) &
                error stop "cuda_loops_mm_driver::synchronize:Error in call to synchronize wrapper."
    end subroutine synchronize
   
    subroutine cleanup(this)
        class(cuda_loops_mm_driver), intent(inout) :: this
      
        call this%converter%cleanup()
        call this%clear() 
    end subroutine cleanup
 
    subroutine clear(this)
        class(cuda_loops_mm_driver), intent(inout) :: this
    end subroutine clear
end module cuda_loops_mm_driver_module
