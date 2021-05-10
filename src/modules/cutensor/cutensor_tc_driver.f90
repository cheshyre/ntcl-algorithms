module cutensor_tc_driver_module
    use, intrinsic :: iso_fortran_env, only : &
            int64, &
            int32, &
            real32, &
            real64, &
            error_unit

    use, intrinsic :: iso_c_binding, only : &
            c_int, &
            c_float, &
            c_double, &
            c_ptr, &
            c_loc, &
            c_associated

    use :: data_api, only : &
            stream

    use :: cuda_data_plugin, only : cuda_synchronize_wrapper

    use :: tensor_api, only : &
            tensor, &
            scalar, &
            tensor_c_pointer_converter, &
            dt_r32, &
            dt_r64, &
            dt_c64, &
            dt_c128

    use :: tensor_contraction_api, only : tensor_contraction
    use :: tensor_contraction_dev, only : tc_descriptor

    implicit none
    private

    public :: cutensor_tc_driver

    type, extends(tensor_contraction) :: cutensor_tc_driver
        type(tensor_c_pointer_converter) :: converter
        type(tc_descriptor) :: descriptor
        type(c_ptr) :: cutensor_handle
    contains
        procedure :: contract => contract
        procedure :: synchronize => synchronize
        procedure :: initialize => initialize
        procedure :: cleanup => cleanup
        procedure :: clear => clear

        procedure :: contract_real32 => contract_real32
        procedure :: contract_real64 => contract_real64
        procedure :: contract_complex64 => contract_complex64
        procedure :: contract_complex128 => contract_complex128
    end type cutensor_tc_driver

    interface cutensor_tc_driver
        module procedure constructor_empty
        module procedure constructor
    end interface cutensor_tc_driver

    interface
        integer(c_int) function cutensor_contract_r32(handle, &
                                                    c, c_rank, c_dims, c_inds, &
                                                    a, a_rank, a_dims, a_inds, &
                                                    b, b_rank, b_dims, b_inds, &
                                                    alpha, &
                                                    beta) &
                        bind(c, name="cutensor_contract_r32")
            import :: c_ptr, c_int, c_float

            type(c_ptr), value :: handle
            type(c_ptr), value :: c, a, b
            type(c_ptr), value :: c_dims, a_dims, b_dims
            type(c_ptr), value :: c_inds, a_inds, b_inds
            integer(c_int), value :: c_rank, a_rank, b_rank
            real(c_float), value :: alpha, beta
        end function cutensor_contract_r32

        integer(c_int) function cutensor_contract_r64(handle, &
                                                    c, c_rank, c_dims, c_inds, &
                                                    a, a_rank, a_dims, a_inds, &
                                                    b, b_rank, b_dims, b_inds, &
                                                    alpha, &
                                                    beta) &
                        bind(c, name="cutensor_contract_r64")
            import :: c_ptr, c_int, c_double

            type(c_ptr), value :: handle
            type(c_ptr), value :: c, a, b
            type(c_ptr), value :: c_dims, a_dims, b_dims
            type(c_ptr), value :: c_inds, a_inds, b_inds
            integer(c_int), value :: c_rank, a_rank, b_rank
            real(c_double), value :: alpha, beta
        end function cutensor_contract_r64

        integer(c_int) function cutensor_contract_c64(handle, &
                                                    c, c_rank, c_dims, c_inds, &
                                                    a, a_rank, a_dims, a_inds, &
                                                    b, b_rank, b_dims, b_inds, &
                                                    alpha, &
                                                    beta) &
                        bind(c, name="cutensor_contract_c64")
            import :: c_ptr, c_int, c_float

            type(c_ptr), value :: handle
            type(c_ptr), value :: c, a, b
            type(c_ptr), value :: c_dims, a_dims, b_dims
            type(c_ptr), value :: c_inds, a_inds, b_inds
            integer(c_int), value :: c_rank, a_rank, b_rank
            complex(c_float) :: alpha, beta
        end function cutensor_contract_c64

        integer(c_int) function cutensor_contract_c128(handle, &
                                                    c, c_rank, c_dims, c_inds, &
                                                    a, a_rank, a_dims, a_inds, &
                                                    b, b_rank, b_dims, b_inds, &
                                                    alpha, &
                                                    beta) &
                        bind(c, name="cutensor_contract_c128")
            import :: c_ptr, c_int, c_double

            type(c_ptr), value :: handle
            type(c_ptr), value :: c, a, b
            type(c_ptr), value :: c_dims, a_dims, b_dims
            type(c_ptr), value :: c_inds, a_inds, b_inds
            integer(c_int), value :: c_rank, a_rank, b_rank
            complex(c_double) :: alpha, beta
        end function cutensor_contract_c128

        integer(c_int) function cutensor_init(handle) bind(c, name="cutensor_init")
            import :: c_ptr, c_int

            type(c_ptr) :: handle
        end function cutensor_init

        integer(c_int) function cutensor_free(handle) bind(c, name="cutensor_free")
            import :: c_ptr, c_int

            type(c_ptr), value :: handle
        end function cutensor_free
    end interface
contains
    function constructor_empty() result(this)
        type(cutensor_tc_driver) :: this

        call this%clear()
    end function constructor_empty

    function constructor(converter, descriptor) result(this)
        type(tensor_c_pointer_converter), intent(in) :: converter
        type(tc_descriptor), intent(in) :: descriptor
        type(cutensor_tc_driver) :: this

        this = cutensor_tc_driver()

        this%converter = converter
        this%descriptor = descriptor
        call this%initialize()
    end function constructor

    subroutine initialize(this)
        class(cutensor_tc_driver), intent(inout) :: this

        integer :: error

        error = cutensor_init( this%cutensor_handle )
    end subroutine initialize

    subroutine contract(this, c, a, b, alpha, beta, astream)
        class(cutensor_tc_driver), intent(inout) :: this
        class(tensor), intent(inout) :: c
        class(tensor), intent(in) :: a, b
        type(scalar), intent(in), optional :: alpha, beta
        type(stream), intent(in), optional :: astream

        select case (c%datatype)
        case (dt_r32)
            call this%contract_real32(c, a, b, alpha, beta, astream)
        case (dt_r64)
            call this%contract_real64(c, a, b, alpha, beta, astream)
        case (dt_c64)
            call this%contract_complex64(c, a, b, alpha, beta, astream)
        case (dt_c128)
            call this%contract_complex128(c, a, b, alpha, beta, astream)
        case default
            error stop "cutensor_tc_driver::contract:Datatype not supported."
        end select
    end subroutine contract

    subroutine contract_real32(this, c, a, b, alpha, beta, astream)
        class(cutensor_tc_driver), intent(inout) :: this
        class(tensor), intent(inout) :: c
        class(tensor), intent(in) :: a, b
        type(scalar), intent(in), optional :: alpha, beta
        type(stream), intent(in), optional :: astream

        type(c_ptr) :: d_c, d_a, d_b
        integer(int64), dimension(:), allocatable, target :: c_dims, a_dims, b_dims
        integer(int32), dimension(:), allocatable, target :: c_inds, a_inds, b_inds
        real(real32) :: alpha32, beta32
        integer :: i
        integer(c_int) :: error

        c_dims = c%get_dims()
        a_dims = a%get_dims()
        b_dims = b%get_dims()

        c_inds = this%descriptor%c_indices
        a_inds = this%descriptor%a_indices
        b_inds = this%descriptor%b_indices

        call this%converter%secure_pointer(c, d_c, astream)
        call this%converter%secure_pointer(a, d_a, astream)
        call this%converter%secure_pointer(b, d_b, astream)

        alpha32 = 1
        if ( present(alpha) ) alpha32 = alpha%as_real32()
        beta32 = 1
        if ( present(beta) ) beta32 = beta%as_real32()

        error = cutensor_contract_r32(this%cutensor_handle, &
                                    d_c, c%get_rank(), c_loc( c_dims ), c_loc( c_inds ), &
                                    d_a, a%get_rank(), c_loc( a_dims ), c_loc( a_inds ), &
                                    d_b, b%get_rank(), c_loc( b_dims ), c_loc( b_inds ), &
                                    alpha32, &
                                    beta32)

        call this%converter%update_and_release(c, d_c, astream)
        call this%converter%release(a, d_a, astream)
        call this%converter%release(b, d_b, astream)
    end subroutine contract_real32

    subroutine contract_real64(this, c, a, b, alpha, beta, astream)
        class(cutensor_tc_driver), intent(inout) :: this
        class(tensor), intent(inout) :: c
        class(tensor), intent(in) :: a, b
        type(scalar), intent(in), optional :: alpha, beta
        type(stream), intent(in), optional :: astream

        type(c_ptr) :: d_c, d_a, d_b
        integer(int64), dimension(:), allocatable, target :: c_dims, a_dims, b_dims
        integer(int32), dimension(:), allocatable, target :: c_inds, a_inds, b_inds
        real(real64) :: alpha64, beta64
        integer :: i
        integer(c_int) :: error

        c_dims = c%get_dims()
        a_dims = a%get_dims()
        b_dims = b%get_dims()

        c_inds = this%descriptor%c_indices
        a_inds = this%descriptor%a_indices
        b_inds = this%descriptor%b_indices

        call this%converter%secure_pointer(c, d_c, astream)
        call this%converter%secure_pointer(a, d_a, astream)
        call this%converter%secure_pointer(b, d_b, astream)

        alpha64 = 1
        if ( present(alpha) ) alpha64 = alpha%as_real64()
        beta64 = 1
        if ( present(beta) ) beta64 = beta%as_real64()

        error = cutensor_contract_r64(this%cutensor_handle, &
                                    d_c, c%get_rank(), c_loc( c_dims ), c_loc( c_inds ), &
                                    d_a, a%get_rank(), c_loc( a_dims ), c_loc( a_inds ), &
                                    d_b, b%get_rank(), c_loc( b_dims ), c_loc( b_inds ), &
                                    alpha64, &
                                    beta64)

        call this%converter%update_and_release(c, d_c, astream)
        call this%converter%release(a, d_a, astream)
        call this%converter%release(b, d_b, astream)
    end subroutine contract_real64

    subroutine contract_complex64(this, c, a, b, alpha, beta, astream)
        class(cutensor_tc_driver), intent(inout) :: this
        class(tensor), intent(inout) :: c
        class(tensor), intent(in) :: a, b
        type(scalar), intent(in), optional :: alpha, beta
        type(stream), intent(in), optional :: astream

        type(c_ptr) :: d_c, d_a, d_b
        integer(int64), dimension(:), allocatable, target :: c_dims, a_dims, b_dims
        integer(int32), dimension(:), allocatable, target :: c_inds, a_inds, b_inds
        complex(real32) :: alpha32, beta32
        integer :: i
        integer(c_int) :: error

        c_dims = c%get_dims()
        a_dims = a%get_dims()
        b_dims = b%get_dims()

        c_inds = this%descriptor%c_indices
        a_inds = this%descriptor%a_indices
        b_inds = this%descriptor%b_indices

        call this%converter%secure_pointer(c, d_c, astream)
        call this%converter%secure_pointer(a, d_a, astream)
        call this%converter%secure_pointer(b, d_b, astream)

        alpha32 = 1
        if ( present(alpha) ) alpha32 = alpha%as_complex64()
        beta32 = 1
        if ( present(beta) ) beta32 = beta%as_complex64()

        error = cutensor_contract_c64(this%cutensor_handle, &
                                    d_c, c%get_rank(), c_loc( c_dims ), c_loc( c_inds ), &
                                    d_a, a%get_rank(), c_loc( a_dims ), c_loc( a_inds ), &
                                    d_b, b%get_rank(), c_loc( b_dims ), c_loc( b_inds ), &
                                    alpha32, beta32)

        call this%converter%update_and_release(c, d_c, astream)
        call this%converter%release(a, d_a, astream)
        call this%converter%release(b, d_b, astream)
    end subroutine contract_complex64

    subroutine contract_complex128(this, c, a, b, alpha, beta, astream)
        class(cutensor_tc_driver), intent(inout) :: this
        class(tensor), intent(inout) :: c
        class(tensor), intent(in) :: a, b
        type(scalar), intent(in), optional :: alpha, beta
        type(stream), intent(in), optional :: astream

        type(c_ptr) :: d_c, d_a, d_b
        integer(int64), dimension(:), allocatable, target :: c_dims, a_dims, b_dims
        integer(int32), dimension(:), allocatable, target :: c_inds, a_inds, b_inds
        complex(real64) :: alpha64, beta64
        integer :: i
        integer(c_int) :: error

        c_dims = c%get_dims()
        a_dims = a%get_dims()
        b_dims = b%get_dims()

        c_inds = this%descriptor%c_indices
        a_inds = this%descriptor%a_indices
        b_inds = this%descriptor%b_indices

        call this%converter%secure_pointer(c, d_c, astream)
        call this%converter%secure_pointer(a, d_a, astream)
        call this%converter%secure_pointer(b, d_b, astream)

        alpha64 = 1
        if ( present(alpha) ) alpha64 = alpha%as_complex128()
        beta64 = 1
        if ( present(beta) ) beta64 = beta%as_complex128()

        error = cutensor_contract_c128(this%cutensor_handle, &
                                    d_c, c%get_rank(), c_loc( c_dims ), c_loc( c_inds ), &
                                    d_a, a%get_rank(), c_loc( a_dims ), c_loc( a_inds ), &
                                    d_b, b%get_rank(), c_loc( b_dims ), c_loc( b_inds ), &
                                    alpha64, beta64)

        call this%converter%update_and_release(c, d_c, astream)
        call this%converter%release(a, d_a, astream)
        call this%converter%release(b, d_b, astream)
    end subroutine contract_complex128

    subroutine synchronize(this, astream)
        class(cutensor_tc_driver), intent(in) :: this
        type(stream), intent(in), optional :: astream

        integer :: error

        error = cuda_synchronize_wrapper(astream)

        if (error /= 0) &
                 error stop "cutensor_tc_driver::synchronize:Error in call to synchronize wrapper."
    end subroutine synchronize

    subroutine clear(this)
        class(cutensor_tc_driver), intent(inout) :: this

        this%descriptor = tc_descriptor()
    end subroutine clear

    subroutine cleanup(this)
        class(cutensor_tc_driver), intent(inout) :: this

        integer(c_int) :: error

        call this%descriptor%cleanup()
        call this%converter%cleanup()
        error = cutensor_free( this%cutensor_handle )
    end subroutine cleanup
end module cutensor_tc_driver_module
