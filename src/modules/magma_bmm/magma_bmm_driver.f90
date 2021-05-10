module magma_bmm_driver_module
    use, intrinsic :: iso_fortran_env, only : &
            real32, &
            real64, &
            int64

    use, intrinsic :: iso_c_binding, only : &
            c_ptr, &
            c_int, &
            c_float, &
            c_double

    use :: util_api, only : string

    use :: data_api, only : stream

    use :: tensor_api, only : &
            tensor_c_pointer_converter, &
            tensor_builder, &
            tensor, &
            scalar, &
            vector

    use :: pointer_data_bmm_driver_module, only : pointer_data_bmm_driver

    use :: magma_module, only : &
            magma, &
            magma_sgemm_vbatched, &
            magma_dgemm_vbatched, &
            magma_cgemm_vbatched, &
            magma_zgemm_vbatched

    implicit none
    private

    public :: magma_bmm_driver

    type, extends(pointer_data_bmm_driver) :: magma_bmm_driver
        type(magma) :: magma
    contains
        procedure :: bmm_single_real64 => bmm_single_real64
        procedure :: bmm_single_real32 => bmm_single_real32
        procedure :: bmm_single_complex64 => bmm_single_complex64
        procedure :: bmm_single_complex128 => bmm_single_complex128
        procedure :: get_padding_for_sizes => get_padding_for_sizes
        procedure :: initialize => initialize
        procedure :: synchronize => synchronize
        procedure :: cleanup => cleanup
    end type magma_bmm_driver

    interface magma_bmm_driver
        module procedure constructor_empty
        module procedure constructor
    end interface magma_bmm_driver

contains
    function constructor_empty() result(this)
        type(magma_bmm_driver) :: this

        call this%initialize()
    end function constructor_empty

    function constructor(converter, builder) result(this)
        type(tensor_c_pointer_converter), intent(in) :: converter
        type(tensor_builder) :: builder
        type(magma_bmm_driver) :: this

        this = magma_bmm_driver()
        this%converter = converter
        this%builder = builder
    end function constructor

    subroutine initialize(this)
        class(magma_bmm_driver), intent(inout) :: this

        this%magma = magma()
    end subroutine initialize

    subroutine bmm_single_real32(this, batch_count, dst, left, right, m, n, k, &
                transa, transb, alpha, beta, astream)
        class(magma_bmm_driver), intent(in) :: this
        integer, intent(in) :: batch_count
        type(c_ptr), intent(inout) :: dst
        type(c_ptr), intent(in) :: left, right
        type(c_ptr), intent(in) :: m, n, k
        logical, intent(in) :: transa, transb
        real(c_float), intent(in) :: alpha, beta
        type(stream), intent(in), optional :: astream

        integer(c_int) :: my_transa, my_transb
        type(c_ptr) :: stream_queue

        my_transa = 111; my_transb = 111
        if ( transa) my_transa = 112
        if ( transb ) my_transb = 112

        call this%magma%populate_a_queue(stream_queue, astream)

        call magma_sgemm_vbatched( my_transa, my_transb, m, n, k, alpha, left, &
                m, right, k, beta, dst, m, batch_count, stream_queue)

        call this%magma%destroy_queue_if_necessary(stream_queue, astream)
    end subroutine bmm_single_real32

    subroutine bmm_single_real64(this, batch_count, dst, left, right, m, n, k, &
                transa, transb, alpha, beta, astream)
        class(magma_bmm_driver), intent(in) :: this
        integer, intent(in) :: batch_count
        type(c_ptr), intent(inout) :: dst
        type(c_ptr), intent(in) :: left, right
        type(c_ptr), intent(in) :: m, n, k
        logical, intent(in) :: transa, transb
        real(c_double), intent(in) :: alpha, beta
        type(stream), intent(in), optional :: astream

        integer(c_int) :: my_transa, my_transb
        type(c_ptr) :: stream_queue

        my_transa = 111; my_transb = 111
        if ( transa) my_transa = 112
        if ( transb ) my_transb = 112

        call this%magma%populate_a_queue(stream_queue, astream)

        call magma_dgemm_vbatched( my_transa, my_transb, m, n, k, alpha, left, &
                m, right, k, beta, dst, m, batch_count, stream_queue)

        call this%magma%destroy_queue_if_necessary(stream_queue, astream)
    end subroutine bmm_single_real64

    subroutine bmm_single_complex64(this, batch_count, dst, left, right, m, n, k, &
                transa, transb, alpha, beta, astream)
        class(magma_bmm_driver), intent(in) :: this
        integer, intent(in) :: batch_count
        type(c_ptr), intent(inout) :: dst
        type(c_ptr), intent(in) :: left, right
        type(c_ptr), intent(in) :: m, n, k
        logical, intent(in) :: transa, transb
        complex(c_float), intent(in) :: alpha, beta
        type(stream), intent(in), optional :: astream

        integer(c_int) :: my_transa, my_transb
        type(c_ptr) :: stream_queue

        my_transa = 111; my_transb = 111
        if ( transa) my_transa = 112
        if ( transb ) my_transb = 112

        call this%magma%populate_a_queue(stream_queue, astream)

        call magma_cgemm_vbatched( my_transa, my_transb, m, n, k, alpha, left, &
                m, right, k, beta, dst, m, batch_count, stream_queue)

        call this%magma%destroy_queue_if_necessary(stream_queue, astream)
    end subroutine bmm_single_complex64

    subroutine bmm_single_complex128(this, batch_count, dst, left, right, m, n, k, &
                transa, transb, alpha, beta, astream)
        class(magma_bmm_driver), intent(in) :: this
        integer, intent(in) :: batch_count
        type(c_ptr), intent(inout) :: dst
        type(c_ptr), intent(in) :: left, right
        type(c_ptr), intent(in) :: m, n, k
        logical, intent(in) :: transa, transb
        complex(c_double), intent(in) :: alpha, beta
        type(stream), intent(in), optional :: astream

        integer(c_int) :: my_transa, my_transb
        type(c_ptr) :: stream_queue

        my_transa = 111; my_transb = 111
        if ( transa) my_transa = 112
        if ( transb ) my_transb = 112

        call this%magma%populate_a_queue(stream_queue, astream)

        call magma_zgemm_vbatched( my_transa, my_transb, m, n, k, alpha, left, &
                m, right, k, beta, dst, m, batch_count, stream_queue)

        call this%magma%destroy_queue_if_necessary(stream_queue, astream)
    end subroutine bmm_single_complex128

    integer function get_padding_for_sizes(this)
        class(magma_bmm_driver), intent(in) :: this

        get_padding_for_sizes = 1
    end function get_padding_for_sizes

    subroutine synchronize(this, astream)
        class(magma_bmm_driver), intent(in) :: this
        type(stream), intent(in), optional :: astream

        integer(c_int) :: error

        error = this%magma%sync(astream)
    end subroutine synchronize

    subroutine cleanup(this)
        class(magma_bmm_driver), intent(inout) :: this

        call this%magma%cleanup()
        call this%converter%cleanup()
        call this%builder%cleanup()
    end subroutine cleanup
end module magma_bmm_driver_module
