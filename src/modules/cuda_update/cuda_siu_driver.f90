module cuda_siu_driver_module
    use iso_c_binding, only : &
            c_ptr, &
            c_null_ptr, &
            c_float, &
            c_double, &
            c_int64_t

    use :: data_api, only : stream

    use :: cuda_data_plugin, only : cuda_synchronize_wrapper

    use :: tensor_api, only : &
            scalar, &
            tensor, &
            tensor_c_pointer_converter, &
            dt_r32, &
            dt_r64, &
            dt_c64, &
            dt_c128

    use :: tensor_update_dev, only : scalar_inline_update

    implicit none
    private

    public :: cuda_siu_driver

    type, extends(scalar_inline_update) :: cuda_siu_driver
        type(tensor_c_pointer_converter) :: converter
    contains
        procedure :: update => update
        procedure :: synchronize => synchronize
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type cuda_siu_driver

    interface cuda_siu_driver
        module procedure constructor_empty
        module procedure constructor
    end interface cuda_siu_driver

    interface
        subroutine perform_siu_r32(dst, nelements, alpha, astream) bind(C, name="perform_siu_r32")
            import :: c_ptr
            import :: c_int64_t
            import :: c_float

            type(c_ptr), value :: dst
            integer(c_int64_t), value :: nelements
            real(c_float), value :: alpha
            type(c_ptr), value :: astream
        end subroutine perform_siu_r32

        subroutine perform_siu_r64(dst, nelements, alpha, astream) bind(C, name="perform_siu_r64")
            import :: c_ptr
            import :: c_int64_t
            import :: c_double

            type(c_ptr), value :: dst
            integer(c_int64_t), value :: nelements
            real(c_double), value :: alpha
            type(c_ptr), value :: astream
        end subroutine perform_siu_r64

        subroutine perform_siu_c64(dst, nelements, alpha, astream) bind(C, name="perform_siu_c64")
            import :: c_ptr
            import :: c_int64_t
            import :: c_float

            type(c_ptr), value :: dst
            integer(c_int64_t), value :: nelements
            complex(c_float), value :: alpha
            type(c_ptr), value :: astream
        end subroutine perform_siu_c64

        subroutine perform_siu_c128(dst, nelements, alpha, astream) bind(C, name="perform_siu_c128")
            import :: c_ptr
            import :: c_int64_t
            import :: c_double

            type(c_ptr), value :: dst
            integer(c_int64_t), value :: nelements
            complex(c_double), value :: alpha
            type(c_ptr), value :: astream
        end subroutine perform_siu_c128
    end interface
contains
    function constructor_empty() result(this)
        type(cuda_siu_driver) :: this

        call this%clear()
    end function constructor_empty

    function constructor(converter) result(this)
        type(tensor_c_pointer_converter), intent(in) :: converter
        type(cuda_siu_driver) :: this

        this = cuda_siu_driver()
        this%converter = converter
    end function constructor

    subroutine update(this, dst, alpha, astream)
        class(cuda_siu_driver), intent(inout) :: this
        class(tensor), intent(inout) :: dst
        type(scalar), intent(in) :: alpha
        type(stream), intent(in), optional :: astream

        type(c_ptr) :: actual_stream, dst_device_cptr

        actual_stream = c_null_ptr
        if ( present(astream) ) actual_stream = astream%sid

        call this%converter%secure_pointer(dst, dst_device_cptr, astream)

        select case (dst%datatype)
        case (dt_r32)
            call perform_siu_r32(dst_device_cptr, dst%get_number_of_elements(), alpha%as_real32(), actual_stream)
        case (dt_r64)
            call perform_siu_r64(dst_device_cptr, dst%get_number_of_elements(), alpha%as_real64(), actual_stream)
        case (dt_c64)
            call perform_siu_c64(dst_device_cptr, dst%get_number_of_elements(), alpha%as_complex64(), actual_stream)
        case (dt_c128)
            call perform_siu_c128(dst_device_cptr, dst%get_number_of_elements(), alpha%as_complex128(), actual_stream)
        case default
            error stop "cuda_siu_driver:update:Datatype not supported."
        end select

        call this%converter%update_and_release(dst, dst_device_cptr, astream)
    end subroutine update

    subroutine synchronize(this, astream)
        class(cuda_siu_driver), intent(inout) :: this
        type(stream), intent(in), optional :: astream

        integer :: error

        error = cuda_synchronize_wrapper(astream)

        if (error /= 0) &
                error stop "cuda_siu_driver::synchronize:Error in call to synchronize wrapper."
    end subroutine synchronize

    subroutine cleanup(this)
        class(cuda_siu_driver), intent(inout) :: this

        call this%converter%cleanup()
        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(cuda_siu_driver), intent(inout) :: this
    end subroutine clear
end module cuda_siu_driver_module
