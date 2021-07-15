module tailored_hip_permute_driver_module
    use, intrinsic :: iso_fortran_env, only : &
            int64, &
            real32, &
            real64

    use iso_c_binding, only : &
            c_int, &
            c_float, &
            c_double, &
            c_int64_t, &
            c_ptr, &
            c_null_ptr

    use :: data_api, only : &
            stream

    use :: tensor_api, only : &
            tensor, &
            vector, &
            dt_r32, &
            dt_r64, &
            dt_c64, &
            dt_c128

    use :: tensor_permute_dev, only : &
            raw_tensor_permute

    use :: async_hip_permute_driver_module, only : async_hip_permute_driver

    implicit none
    private

    public :: tailored_hip_permute_driver

    type, extends(raw_tensor_permute) :: tailored_hip_permute_driver
        type(async_hip_permute_driver) :: driver
    contains
        procedure :: permute_tensor => permute_tensor
        procedure :: synchronize => synchronize
        procedure :: cleanup => cleanup

        procedure :: permute_large_first => permute_large_first
    end type tailored_hip_permute_driver

    interface tailored_hip_permute_driver
        module procedure constructor_empty
        module procedure constructor
    end interface tailored_hip_permute_driver

    interface
        subroutine hip_tensor_permute_1_4_3_2_real32_execute(d_src, d_dst, number_of_elements, &
                    np, d_sdims, astream) &
                    bind(C, name="hip_tensor_permute_1_4_3_2_real32_execute")
            import :: c_ptr
            import :: c_int
            import :: c_int64_t

            type(c_ptr), value, intent(in) :: d_src, d_dst
            integer(c_int64_t), value, intent(in) :: number_of_elements
            integer(c_int), value, intent(in) :: np
            type(c_ptr), value, intent(in) :: d_sdims
            type(c_ptr), value, intent(in) :: astream
        end subroutine hip_tensor_permute_1_4_3_2_real32_execute

        subroutine hip_tensor_permute_1_4_3_2_real64_execute(d_src, d_dst, number_of_elements, &
                    np, d_sdims, astream) &
                    bind(C, name="hip_tensor_permute_1_4_3_2_real64_execute")
            import :: c_ptr
            import :: c_int
            import :: c_int64_t

            type(c_ptr), value, intent(in) :: d_src, d_dst
            integer(c_int64_t), value, intent(in) :: number_of_elements
            integer(c_int), value, intent(in) :: np
            type(c_ptr), value, intent(in) :: d_sdims
            type(c_ptr), value, intent(in) :: astream
        end subroutine hip_tensor_permute_1_4_3_2_real64_execute

        subroutine hip_tensor_permute_1_4_3_2_complex64_execute(d_src, d_dst, number_of_elements, &
                    np, d_sdims, astream) &
                    bind(C, name="hip_tensor_permute_1_4_3_2_complex64_execute")
            import :: c_ptr
            import :: c_int
            import :: c_int64_t

            type(c_ptr), value, intent(in) :: d_src, d_dst
            integer(c_int64_t), value, intent(in) :: number_of_elements
            integer(c_int), value, intent(in) :: np
            type(c_ptr), value, intent(in) :: d_sdims
            type(c_ptr), value, intent(in) :: astream
        end subroutine hip_tensor_permute_1_4_3_2_complex64_execute

        subroutine hip_tensor_permute_1_4_3_2_complex128_execute(d_src, d_dst, number_of_elements, &
                    np, d_sdims, astream) &
                    bind(C, name="hip_tensor_permute_1_4_3_2_complex128_execute")
            import :: c_ptr
            import :: c_int
            import :: c_int64_t

            type(c_ptr), value, intent(in) :: d_src, d_dst
            integer(c_int64_t), value, intent(in) :: number_of_elements
            integer(c_int), value, intent(in) :: np
            type(c_ptr), value, intent(in) :: d_sdims
            type(c_ptr), value, intent(in) :: astream
        end subroutine hip_tensor_permute_1_4_3_2_complex128_execute

        subroutine hip_tensor_permute_large_first_real32_execute(d_src, d_dst, number_of_elements, &
                    np, d_sdims, d_perm, order, astream) &
                    bind(C, name="hip_tensor_permute_large_first_real32_execute")
            import :: c_ptr
            import :: c_int
            import :: c_int64_t

            type(c_ptr), value, intent(in) :: d_src, d_dst
            integer(c_int64_t), value, intent(in) :: number_of_elements
            integer(c_int), value, intent(in) :: np
            type(c_ptr), value, intent(in) :: d_sdims, d_perm
            integer(c_int), value, intent(in) :: order
            type(c_ptr), value, intent(in) :: astream
        end subroutine hip_tensor_permute_large_first_real32_execute

        subroutine hip_tensor_permute_large_first_real64_execute(d_src, d_dst, number_of_elements, &
                    np, d_sdims, d_perm, order, astream) &
                    bind(C, name="hip_tensor_permute_large_first_real64_execute")
            import :: c_ptr
            import :: c_int
            import :: c_int64_t

            type(c_ptr), value, intent(in) :: d_src, d_dst
            integer(c_int64_t), value, intent(in) :: number_of_elements
            integer(c_int), value, intent(in) :: np
            type(c_ptr), value, intent(in) :: d_sdims, d_perm
            integer(c_int), value, intent(in) :: order
            type(c_ptr), value, intent(in) :: astream
        end subroutine hip_tensor_permute_large_first_real64_execute

        subroutine hip_tensor_permute_large_first_complex64_execute(d_src, d_dst, number_of_elements, &
                    np, d_sdims, d_perm, order, astream) &
                    bind(C, name="hip_tensor_permute_large_first_complex64_execute")
            import :: c_ptr
            import :: c_int
            import :: c_int64_t

            type(c_ptr), value, intent(in) :: d_src, d_dst
            integer(c_int64_t), value, intent(in) :: number_of_elements
            integer(c_int), value, intent(in) :: np
            type(c_ptr), value, intent(in) :: d_sdims, d_perm
            integer(c_int), value, intent(in) :: order
            type(c_ptr), value, intent(in) :: astream
        end subroutine hip_tensor_permute_large_first_complex64_execute

        subroutine hip_tensor_permute_large_first_complex128_execute(d_src, d_dst, number_of_elements, &
                    np, d_sdims, d_perm, order, astream) &
                    bind(C, name="hip_tensor_permute_large_first_complex128_execute")
            import :: c_ptr
            import :: c_int
            import :: c_int64_t

            type(c_ptr), value, intent(in) :: d_src, d_dst
            integer(c_int64_t), value, intent(in) :: number_of_elements
            integer(c_int), value, intent(in) :: np
            type(c_ptr), value, intent(in) :: d_sdims, d_perm
            integer(c_int), value, intent(in) :: order
            type(c_ptr), value, intent(in) :: astream
        end subroutine hip_tensor_permute_large_first_complex128_execute
    end interface

    integer, parameter :: max_tensor_order = 6
contains
    function constructor_empty() result(this)
        type(tailored_hip_permute_driver) :: this

    end function constructor_empty

    function constructor(driver) result(this)
        type(async_hip_permute_driver), intent(in) :: driver
        type(tailored_hip_permute_driver) :: this

        this = tailored_hip_permute_driver()
        this%driver = driver
    end function constructor

    subroutine permute_tensor(this, src, dst, perm, astream)
        class(tailored_hip_permute_driver), intent(inout) :: this
        class(tensor), intent(in) :: src
        class(tensor), intent(inout) :: dst
        integer, dimension(:), intent(in) :: perm
        type(stream), intent(in), optional :: astream

        if ( perm(1) == 1) then
            call this%permute_large_first(src, dst, perm, astream)
        else
            call this%driver%permute_tensor(src, dst, perm, astream)
        end if
    end subroutine permute_tensor
        
    subroutine permute_large_first(this, src, dst, perm, astream)
        class(tailored_hip_permute_driver), intent(inout) :: this
        class(tensor), intent(in) :: src
        class(tensor), intent(inout) :: dst
        integer, dimension(:), intent(in) :: perm
        type(stream), intent(in), optional :: astream

        type(c_ptr) :: src_device_cptr, dst_device_cptr, device_dims_cptr, device_perms_cptr
        type(vector) :: device_dims, device_perms
        type(c_ptr) :: actual_stream

        if ( .not. this%is_compatible(src, dst, perm) ) &
                error stop "tailored_hip_permute_driver::permute_large_first:Tensors are not compatible."

        ! get c pointers to src and dst on device
        call this%driver%converter%secure_pointer(src, src_device_cptr, astream)
        call this%driver%converter%secure_pointer(dst, dst_device_cptr, astream)

        ! Update device with dimensions and perm vector
        call this%driver%secure_buffered_data(src%dims, perm, device_dims, device_perms, astream)
        call this%driver%converter%secure_pointer(device_dims, device_dims_cptr, astream)
        call this%driver%converter%secure_pointer(device_perms, device_perms_cptr, astream)

        ! TODO: Use default stream?
        actual_stream = c_null_ptr
        if ( present(astream) ) actual_stream = astream%sid

        select case (src%get_datatype())
        case ( dt_r32 )
!            call hip_tensor_permute_large_first_real32_execute( &
!                    src_device_cptr, dst_device_cptr, src%get_number_of_elements(), &
!                    int(src%dims(1)), device_dims_cptr, actual_stream)
            call hip_tensor_permute_large_first_real32_execute( &
                    src_device_cptr, dst_device_cptr, src%get_number_of_elements(), &
                    int(src%dims(1)), device_dims_cptr, device_perms_cptr, src%get_rank(), actual_stream)
        case ( dt_r64 )
!            call hip_tensor_permute_large_first_real64_execute( &
!                    src_device_cptr, dst_device_cptr, src%get_number_of_elements(), &
!                    int(src%dims(1)), device_dims_cptr, actual_stream)
            call hip_tensor_permute_large_first_real64_execute( &
                    src_device_cptr, dst_device_cptr, src%get_number_of_elements(), &
                    int(src%dims(1)), device_dims_cptr, device_perms_cptr, src%get_rank(), actual_stream)
        case ( dt_c64 )
!            call hip_tensor_permute_large_first_complex64_execute( &
!                    src_device_cptr, dst_device_cptr, src%get_number_of_elements(), &
!                    int(src%dims(1)), device_dims_cptr, actual_stream)
            call hip_tensor_permute_large_first_complex64_execute( &
                    src_device_cptr, dst_device_cptr, src%get_number_of_elements(), &
                    int(src%dims(1)), device_dims_cptr, device_perms_cptr, src%get_rank(), actual_stream)
        case ( dt_c128 )
!            call hip_tensor_permute_large_first_complex128_execute( &
!                    src_device_cptr, dst_device_cptr, src%get_number_of_elements(), &
!                    int(src%dims(1)), device_dims_cptr, actual_stream)
            call hip_tensor_permute_large_first_complex128_execute( &
                    src_device_cptr, dst_device_cptr, src%get_number_of_elements(), &
                    int(src%dims(1)), device_dims_cptr, device_perms_cptr, src%get_rank(), actual_stream)
        case default
            error stop "tailored_hip_permute_driver::permute_large_first:Datatype not supported."
        end select

        ! Release from device scratch when kernels are executed
        call this%driver%release_buffered_data(device_dims, device_perms, this%driver%device_scratch, astream)

        call this%driver%converter%update_and_release(dst, dst_device_cptr, astream)
        call this%driver%converter%release(src, src_device_cptr, astream)
    end subroutine permute_large_first

    subroutine synchronize(this, astream)
        class(tailored_hip_permute_driver), intent(in) :: this
        type(stream), intent(in), optional :: astream

        call this%driver%synchronize(astream)
    end subroutine synchronize

    subroutine cleanup(this)
        class(tailored_hip_permute_driver), intent(inout) :: this

        call this%driver%cleanup()
    end subroutine cleanup
end module tailored_hip_permute_driver_module
