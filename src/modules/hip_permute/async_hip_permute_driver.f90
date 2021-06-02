module async_hip_permute_driver_module
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
            stream, &
            scratch_buffer

    use :: hip_data_plugin, only : hip_synchronize_wrapper

    use :: tensor_api, only : &
            tensor, &
            vector, &
            dt_r32, &
            dt_r64, &
            dt_c64, &
            dt_c128,&
            tensor_builder, &
            tensor_c_pointer_converter

    use :: tensor_permute_dev, only : raw_tensor_permute

    implicit none
    private

    public :: async_hip_permute_driver

    type, extends(raw_tensor_permute) :: async_hip_permute_driver
        type(tensor_builder) :: builder
        type(tensor_c_pointer_converter) :: converter
        type(scratch_buffer) :: pinned_scratch, device_scratch
    contains
        procedure :: set_builder => set_builder
        procedure :: set_converter => set_converter
        procedure :: set_scratch_buffers => set_scratch_buffers
        procedure :: permute_tensor => permute_tensor
        procedure :: synchronize => synchronize
        procedure, private :: release_buffered_data => release_buffered_data
        procedure, private :: secure_buffered_data => secure_buffered_data
        procedure :: cleanup => cleanup
    end type async_hip_permute_driver

    interface async_hip_permute_driver
        module procedure constructor_empty
        module procedure constructor
    end interface async_hip_permute_driver

    interface
        subroutine hip_tensor_permute_generic_real32_execute(d_src, d_dst, number_of_elements, &
                    d_sdims, d_perm, order, astream) &
                    bind(C, name="hip_tensor_permute_async_real32_execute")
            import :: c_ptr
            import :: c_int
            import :: c_int64_t

            type(c_ptr), value, intent(in) :: d_src, d_dst
            integer(c_int64_t), value, intent(in) :: number_of_elements
            type(c_ptr), value, intent(in) :: d_sdims, d_perm
            integer(c_int), value, intent(in) :: order
            type(c_ptr), value, intent(in) :: astream
        end subroutine hip_tensor_permute_generic_real32_execute

        subroutine hip_tensor_permute_generic_real64_execute(d_src, d_dst, number_of_elements, &
                    d_sdims, d_perm, order, astream) &
                    bind(C, name="hip_tensor_permute_async_real64_execute")
            import :: c_ptr
            import :: c_int
            import :: c_int64_t

            type(c_ptr), value, intent(in) :: d_src, d_dst
            integer(c_int64_t), value, intent(in) :: number_of_elements
            type(c_ptr), value, intent(in) :: d_sdims, d_perm
            integer(c_int), value, intent(in) :: order
            type(c_ptr), value, intent(in) :: astream
        end subroutine hip_tensor_permute_generic_real64_execute

        subroutine hip_tensor_permute_generic_complex64_execute(d_src, d_dst, number_of_elements, &
                    d_sdims, d_perm, order, astream) &
                    bind(C, name="hip_tensor_permute_async_complex64_execute")
            import :: c_ptr
            import :: c_int
            import :: c_int64_t

            type(c_ptr), value, intent(in) :: d_src, d_dst
            integer(c_int64_t), value, intent(in) :: number_of_elements
            type(c_ptr), value, intent(in) :: d_sdims, d_perm
            integer(c_int), value, intent(in) :: order
            type(c_ptr), value, intent(in) :: astream
        end subroutine hip_tensor_permute_generic_complex64_execute

        subroutine hip_tensor_permute_generic_complex128_execute(d_src, d_dst, number_of_elements, &
                    d_sdims, d_perm, order, astream) &
                    bind(C, name="hip_tensor_permute_async_complex128_execute")
            import :: c_ptr
            import :: c_int
            import :: c_int64_t

            type(c_ptr), value, intent(in) :: d_src, d_dst
            integer(c_int64_t), value, intent(in) :: number_of_elements
            type(c_ptr), value, intent(in) :: d_sdims, d_perm
            integer(c_int), value, intent(in) :: order
            type(c_ptr), value, intent(in) :: astream
        end subroutine hip_tensor_permute_generic_complex128_execute
    end interface

    integer, parameter :: max_tensor_order = 6
contains
    function constructor_empty() result(this)
        type(async_hip_permute_driver) :: this

    end function constructor_empty

    function constructor(builder, converter, pinned_scratch, device_scratch) result(this)
        type(tensor_builder), intent(in) :: builder
        type(tensor_c_pointer_converter), intent(in) :: converter
        type(scratch_buffer), intent(in) :: pinned_scratch, device_scratch
        type(async_hip_permute_driver) :: this

        this = async_hip_permute_driver()

        call this%set_builder(builder)
        call this%set_converter(converter)
        call this%set_scratch_buffers(pinned_scratch, device_scratch)
    end function constructor

    subroutine set_builder(this, builder)
        class(async_hip_permute_driver), intent(inout) :: this
        type(tensor_builder), intent(in) :: builder

        this%builder = builder
    end subroutine set_builder

    subroutine set_scratch_buffers(this, pinned_scratch, device_scratch)
        class(async_hip_permute_driver), intent(inout) :: this
        type(scratch_buffer), intent(in) :: pinned_scratch, device_scratch

        this%pinned_scratch = pinned_scratch
        call this%pinned_scratch%initialize()
        this%device_scratch = device_scratch
        call this%device_scratch%initialize()
    end subroutine set_scratch_buffers

    subroutine set_converter(this, converter)
        class(async_hip_permute_driver), intent(inout) :: this
        type(tensor_c_pointer_converter), intent(in) :: converter

        this%converter = converter
    end subroutine set_converter

    subroutine secure_buffered_data(this, src_dims, src_perms, dst_dims, dst_perms, astream)
        class(async_hip_permute_driver), intent(inout) :: this
        integer(int64), dimension(:), intent(in) :: src_dims
        integer, dimension(:), intent(in) :: src_perms
        type(vector), intent(inout) :: dst_dims, dst_perms
        type(stream), intent(in), optional :: astream

        type(vector) :: pinned_dims, pinned_perms

        ! Downshift to 32 bit ints and shift to c indexing in pinned buffers.
        call this%builder%copy_to_scratch(pinned_dims, this%pinned_scratch, int(src_dims))
        call this%builder%copy_to_scratch(pinned_perms, this%pinned_scratch, src_perms-1)

        ! Copy to device buffers
        call this%builder%copy_to_scratch(dst_dims, this%device_scratch, pinned_dims, astream)
        call this%builder%copy_to_scratch(dst_perms, this%device_scratch, pinned_perms, astream)

        ! Release from pinned scratch when transferred
        call this%release_buffered_data(pinned_dims, pinned_perms, this%pinned_scratch, astream)
    end subroutine secure_buffered_data

    subroutine permute_tensor(this, src, dst, perm, astream)
        class(async_hip_permute_driver), intent(inout) :: this
        class(tensor), intent(in) :: src
        class(tensor), intent(inout) :: dst
        integer, dimension(:), intent(in) :: perm
        type(stream), intent(in), optional :: astream

        type(c_ptr) :: src_device_cptr, dst_device_cptr, device_dims_cptr, device_perms_cptr
        type(vector) :: device_dims, device_perms
        type(c_ptr) :: actual_stream

        if ( .not. this%is_compatible(src, dst, perm) ) &
                error stop "async_hip_permute_driver::permute_tensor:Tensors are not compatible."

        ! get c pointers to src and dst on device
        call this%converter%secure_pointer(src, src_device_cptr, astream)
        call this%converter%secure_pointer(dst, dst_device_cptr, astream)

        ! Update device with dimensions and perm vector
        call this%secure_buffered_data(src%dims, perm, device_dims, device_perms, astream)
        call this%converter%secure_pointer(device_dims, device_dims_cptr, astream)
        call this%converter%secure_pointer(device_perms, device_perms_cptr, astream)

        ! TODO: Use default stream?
        actual_stream = c_null_ptr
        if ( present(astream) ) actual_stream = astream%sid

        select case (src%get_datatype())
        case ( dt_r32 )
            call hip_tensor_permute_generic_real32_execute( &
                    src_device_cptr, dst_device_cptr, src%get_number_of_elements(), &
                    device_dims_cptr, device_perms_cptr, src%get_rank(), actual_stream)
        case ( dt_r64 )
            call hip_tensor_permute_generic_real64_execute( &
                    src_device_cptr, dst_device_cptr, src%get_number_of_elements(), &
                    device_dims_cptr, device_perms_cptr, src%get_rank(), actual_stream)
        case ( dt_c64 )
            call hip_tensor_permute_generic_complex64_execute( &
                    src_device_cptr, dst_device_cptr, src%get_number_of_elements(), &
                    device_dims_cptr, device_perms_cptr, src%get_rank(), actual_stream)
        case ( dt_c128 )
            call hip_tensor_permute_generic_complex128_execute( &
                    src_device_cptr, dst_device_cptr, src%get_number_of_elements(), &
                    device_dims_cptr, device_perms_cptr, src%get_rank(), actual_stream)
        case default
            error stop "async_hip_permute_driver::permute_tensor:Datatype not supported."
        end select

        ! Release from device scratch when kernels are executed
        call this%release_buffered_data(device_dims, device_perms, this%device_scratch, astream)

        call this%converter%update_and_release(dst, dst_device_cptr, astream)
        call this%converter%release(src, src_device_cptr, astream)
    end subroutine permute_tensor

    subroutine release_buffered_data(this, dims, perms, scratch, astream)
        class(async_hip_permute_driver), intent(in) :: this
        type(vector), intent(inout) :: dims, perms
        type(scratch_buffer), intent(inout) :: scratch
        type(stream), intent(in), optional :: astream

        call scratch%destroy(dims%storage)
        call scratch%destroy(perms%storage)
        call dims%release(); call perms%release()
        call scratch%checkpoint(astream)
    end subroutine release_buffered_data

    subroutine synchronize(this, astream)
        class(async_hip_permute_driver), intent(in) :: this
        type(stream), intent(in), optional :: astream

        integer :: error

        error = hip_synchronize_wrapper(astream)

        if (error /= 0) &
                error stop "async_hip_permute_driver::synchronize:Error in call to synchronize wrapper."
    end subroutine synchronize

    subroutine cleanup(this)
        class(async_hip_permute_driver), intent(inout) :: this

        call this%builder%cleanup()
        call this%converter%cleanup()
        call this%pinned_scratch%cleanup()
        call this%device_scratch%cleanup()
    end subroutine cleanup
end module async_hip_permute_driver_module
