module default_mm_factory_module
    use :: util_api, only : &
            string

    use :: tensor_api, only : tensor_converter_factory

    use :: matrix_multiplication_dev, only : &
            mm_factory, &
            matrix_multiplication, &
            intrinsic_mm_driver

#ifdef use_blas
    use :: blas_plugin, only : &
            blas_mm_driver
#endif

#ifdef use_cublas
    use :: cublas_mm_driver_module, only : cublas_mm_driver
#endif

#ifdef use_cuda
    use :: cuda_loops_mm_driver_module, only : cuda_loops_mm_driver
#endif

#ifdef use_magma
    use :: magma_mm_driver_module, only : magma_mm_driver
#endif

#ifdef use_rocblas
    use :: rocblas_mm_driver_module, only : rocblas_mm_driver
#endif

    implicit none
    private

    public :: default_mm_factory

    type, extends(mm_factory) :: default_mm_factory
    contains
        procedure :: create_from_key => create_from_key
        procedure :: get_available_mm_drivers => get_available_mm_drivers
        procedure, private :: count_available_drivers => count_available_drivers
    end type default_mm_factory

    character(len=*), parameter :: mm_driver_key = "mm_driver"
    character(len=*), parameter :: default_mm_driver = "intrinsic"
contains
    subroutine create_from_key(this, mm, key)
        class(default_mm_factory), intent(in) :: this
        class(matrix_multiplication), allocatable, intent(inout) :: mm
        type(string), intent(in) :: key

        type(tensor_converter_factory) :: factory

        select case (key%char_array)
        case ("intrinsic")
            mm = intrinsic_mm_driver()
#ifdef use_blas
        case ("blas")
            mm = blas_mm_driver()
#endif
#ifdef use_cublas
        case ("cublas")
            mm = cublas_mm_driver(factory%get_c_pointer_converter("device"))
#endif
#ifdef use_cuda
        case ("cuda")
           mm = cuda_loops_mm_driver(factory%get_c_pointer_converter("device"))
#endif
#ifdef use_magma
        case ("magma")
            mm = magma_mm_driver(factory%get_c_pointer_converter("device"))
#endif
#ifdef use_rocblas
        case ("rocblas")
            mm = rocblas_mm_driver(factory%get_c_pointer_converter("device"))
#endif
        case default
            error stop "default_mm_factory::create_from_key:Not a valid mm driver: "//key%char_array
        end select
    end subroutine create_from_key

    function get_available_mm_drivers(this) result(drivers)
        class(default_mm_factory), intent(in) :: this
        type(string), dimension(:), allocatable :: drivers

        integer :: counter

        allocate(drivers(this%count_available_drivers()))

        counter = 1
        drivers(counter) = "intrinsic"
#ifdef use_blas
        counter = counter + 1
        drivers(counter) = "blas"
#endif

#ifdef use_cublas
        counter = counter + 1
        drivers(counter) = "cublas"
#endif

#ifdef use_cuda
        counter = counter + 1
        drivers(counter) = "cuda"
#endif

#ifdef use_magma
        counter = counter + 1
        drivers(counter) = "magma"
#endif

#ifdef use_rocblas
        counter = counter + 1
        drivers(counter) = "rocblas"
#endif
    end function get_available_mm_drivers

    integer function count_available_drivers(this)
        class(default_mm_factory), intent(in) :: this

        count_available_drivers = 1

#ifdef use_blas
        count_available_drivers = count_available_drivers + 1
#endif

#ifdef use_cublas
        count_available_drivers = count_available_drivers + 1
#endif

#ifdef use_cuda
        count_available_drivers = count_available_drivers + 1
#endif

#ifdef use_magma
        count_available_drivers = count_available_drivers + 1
#endif

#ifdef use_rocblas
        count_available_drivers = count_available_drivers + 1
#endif

    end function count_available_drivers
end module default_mm_factory_module
