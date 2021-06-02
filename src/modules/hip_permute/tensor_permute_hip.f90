module tensor_permute_hip_module
    use, intrinsic :: iso_fortran_env, only : &
            int64, &
            real32, &
            real64

    use iso_c_binding, only : &
            c_int, &
            c_float, &
            c_double, &
            c_int64_t, &
            c_ptr

    use :: data_api, only : stream

    use :: hip_data_plugin, only : hip_synchronize_wrapper

    use :: tensor_api, only : &
            tensor, &
            vector, &
            dt_r32, &
            dt_r64, &
            dt_c64, &
            dt_c128,&
            tensor_fortran_converter, &
            tensor_c_pointer_converter, &
            create_tensor

    use :: tensor_permute_dev, only : raw_tensor_permute

    implicit none
    private

    public :: tensor_permute_hip

    type, extends(raw_tensor_permute) :: tensor_permute_hip
        type(tensor_c_pointer_converter) :: c_converter
        type(tensor_fortran_converter) :: f_converter
        type(vector) :: device_dims, device_perms
        integer, dimension(:), pointer, contiguous :: intrinsic_dims, intrinsic_perms
        type(c_ptr) :: device_dims_cptr, device_perms_cptr
    contains
        procedure :: set_converters => set_converters
        procedure :: setup_buffers => setup_buffers
        procedure :: permute_tensor => permute_tensor
        procedure :: synchronize => synchronize
        procedure :: cleanup => cleanup
    end type tensor_permute_hip

    interface tensor_permute_hip
        module procedure constructor_empty
        module procedure constructor
    end interface tensor_permute_hip

    interface
        subroutine hip_tensor_permute_generic_real32_execute(d_src, d_dst, number_of_elements, d_sdims, d_perm, order) &
                    bind(C, name="hip_tensor_permute_generic_real32_execute")
            import :: c_ptr
            import :: c_int
            import :: c_int64_t

            type(c_ptr), value, intent(in) :: d_src, d_dst
            integer(c_int64_t), value, intent(in) :: number_of_elements
            type(c_ptr), value, intent(in) :: d_sdims, d_perm
            integer(c_int), value, intent(in) :: order
        end subroutine hip_tensor_permute_generic_real32_execute

        subroutine hip_tensor_permute_generic_real64_execute(d_src, d_dst, number_of_elements, d_sdims, d_perm, order) &
                    bind(C, name="hip_tensor_permute_generic_real64_execute")
            import :: c_ptr
            import :: c_int
            import :: c_int64_t

            type(c_ptr), value, intent(in) :: d_src, d_dst
            integer(c_int64_t), value, intent(in) :: number_of_elements
            type(c_ptr), value, intent(in) :: d_sdims, d_perm
            integer(c_int), value, intent(in) :: order
        end subroutine hip_tensor_permute_generic_real64_execute

        subroutine hip_tensor_permute_generic_complex64_execute(d_src, d_dst, number_of_elements, d_sdims, d_perm, order) &
                    bind(C, name="hip_tensor_permute_generic_complex64_execute")
            import :: c_ptr
            import :: c_int
            import :: c_int64_t

            type(c_ptr), value, intent(in) :: d_src, d_dst
            integer(c_int64_t), value, intent(in) :: number_of_elements
            type(c_ptr), value, intent(in) :: d_sdims, d_perm
            integer(c_int), value, intent(in) :: order
        end subroutine hip_tensor_permute_generic_complex64_execute

        subroutine hip_tensor_permute_generic_complex128_execute(d_src, d_dst, number_of_elements, d_sdims, d_perm, order) &
                    bind(C, name="hip_tensor_permute_generic_complex128_execute")
            import :: c_ptr
            import :: c_int
            import :: c_int64_t

            type(c_ptr), value, intent(in) :: d_src, d_dst
            integer(c_int64_t), value, intent(in) :: number_of_elements
            type(c_ptr), value, intent(in) :: d_sdims, d_perm
            integer(c_int), value, intent(in) :: order
        end subroutine hip_tensor_permute_generic_complex128_execute
    end interface

    integer, parameter :: max_tensor_order = 6
contains
    function constructor_empty() result(this)
        type(tensor_permute_hip) :: this

    end function constructor_empty

    function constructor(c_converter, f_converter) result(this)
        type(tensor_c_pointer_converter), intent(in) :: c_converter
        type(tensor_fortran_converter), intent(in) :: f_converter
        type(tensor_permute_hip) :: this

        this = tensor_permute_hip()

        call this%set_converters(c_converter, f_converter)
        call this%setup_buffers()
    end function constructor

    subroutine set_converters(this, c_converter, f_converter)
        class(tensor_permute_hip), intent(inout) :: this
        type(tensor_c_pointer_converter), intent(in) :: c_converter
        type(tensor_fortran_converter), intent(in) :: f_converter

        this%c_converter = c_converter
        this%f_converter = f_converter
    end subroutine set_converters

    subroutine setup_buffers(this)
        class(tensor_permute_hip), intent(inout) :: this

        call create_tensor(this%device_dims, [max_tensor_order], "int32", "device")
        call this%f_converter%secure_fortran_pointer(this%intrinsic_dims, this%device_dims)
        call this%c_converter%secure_pointer(this%device_dims, this%device_dims_cptr)

        call create_tensor(this%device_perms, [max_tensor_order], "int32", "device")
        call this%f_converter%secure_fortran_pointer(this%intrinsic_perms, this%device_perms)
        call this%c_converter%secure_pointer(this%device_perms, this%device_perms_cptr)
    end subroutine setup_buffers

    subroutine permute_tensor(this, src, dst, perm, astream)
        class(tensor_permute_hip), intent(inout) :: this
        class(tensor), intent(in) :: src
        class(tensor), intent(inout) :: dst
        integer, dimension(:), intent(in) :: perm
        type(stream), intent(in), optional :: astream

        integer :: i
        type(c_ptr) :: src_device_cptr, dst_device_cptr

        if ( .not. this%is_compatible(src, dst, perm) ) &
                error stop "tensor_permute_hip::permute_tensor:Tensors are not compatible."


        ! Downshift to 32 bit ints and shift to c indexing in buffers.
        do i = 1, size(perm)
            this%intrinsic_dims(i) = int(src%dims(i))
            this%intrinsic_perms(i) = int(perm(i)) - 1
        end do

        ! Update device buffers
        call this%f_converter%update_remote(this%intrinsic_dims, this%device_dims, astream)
        call this%f_converter%update_remote(this%intrinsic_perms, this%device_perms, astream)

        ! get c pointers to src and dst on device
        call this%c_converter%secure_pointer(src, src_device_cptr, astream)
        call this%c_converter%secure_pointer(dst, dst_device_cptr, astream)

        call this%synchronize(astream)
        select case (src%get_datatype())
        case ( dt_r32 )
            call hip_tensor_permute_generic_real32_execute( &
                    src_device_cptr, dst_device_cptr, src%get_number_of_elements(), &
                    this%device_dims_cptr, this%device_perms_cptr, src%get_rank())
        case ( dt_r64 )
            call hip_tensor_permute_generic_real64_execute( &
                    src_device_cptr, dst_device_cptr, src%get_number_of_elements(), &
                    this%device_dims_cptr, this%device_perms_cptr, src%get_rank())
        case ( dt_c64 )
            call hip_tensor_permute_generic_complex64_execute( &
                    src_device_cptr, dst_device_cptr, src%get_number_of_elements(), &
                    this%device_dims_cptr, this%device_perms_cptr, src%get_rank())
        case ( dt_c128 )
            call hip_tensor_permute_generic_complex128_execute( &
                    src_device_cptr, dst_device_cptr, src%get_number_of_elements(), &
                    this%device_dims_cptr, this%device_perms_cptr, src%get_rank())
        case default
            error stop "tensor_permute_hip::permute_tensor:Datatype not supported."
        end select
        call this%synchronize(astream)
        call this%c_converter%update_and_release(dst, dst_device_cptr, astream)
        call this%c_converter%release(src, src_device_cptr, astream)
        call this%synchronize(astream)
    end subroutine permute_tensor

    subroutine synchronize(this, astream)
        class(tensor_permute_hip), intent(in) :: this
        type(stream), intent(in), optional :: astream

        integer :: error

        error = hip_synchronize_wrapper(astream)

        if (error /= 0) &
                error stop "tensor_permute_hip::synchronize:Error in call to synchronize wrapper."
    end subroutine synchronize

    subroutine cleanup(this)
        class(tensor_permute_hip), intent(inout) :: this

        call this%f_converter%release_pointer(this%intrinsic_dims, this%device_dims)
        call this%c_converter%release(this%device_dims, this%device_dims_cptr)
        call this%device_dims%cleanup()

        call this%f_converter%release_pointer(this%intrinsic_perms, this%device_perms)
        call this%c_converter%release(this%device_perms, this%device_perms_cptr)
        call this%device_perms%cleanup()

        call this%c_converter%cleanup()
        call this%f_converter%cleanup()
    end subroutine cleanup
end module tensor_permute_hip_module
