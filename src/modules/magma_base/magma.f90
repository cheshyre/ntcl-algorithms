module magma_module
    use, intrinsic :: iso_c_binding, only : &
            c_int, &
            c_double, &
            c_float, &
            c_ptr, &
            c_null_ptr

    use :: data_api, only : stream

    implicit none
    private

    public :: magma
    public :: magma_sgemm
    public :: magma_dgemm
    public :: magma_cgemm
    public :: magma_zgemm
    public :: magma_sgemm_vbatched
    public :: magma_dgemm_vbatched
    public :: magma_cgemm_vbatched
    public :: magma_zgemm_vbatched

    ! Magma_kit has its own queue, and functions without
    ! queue function arguement for convenience.
    ! You can call the wrapper functions to create other queues
    ! and access magma directly.
    type :: magma
        type(c_ptr) :: queue = c_null_ptr
        integer(c_int) :: dev
    contains
        procedure :: create_queue => create_queue
        procedure :: populate_a_queue => populate_a_queue
        procedure :: destroy_queue_if_necessary => destroy_queue_if_necessary
        procedure :: get_num_initialized => get_num_initialized
        procedure :: initialize => initialize
        procedure :: get_device => get_device
        procedure :: get_version => get_version
        procedure :: sync => sync
        procedure :: destroy_queue => destroy_queue
        procedure :: cleanup => cleanup
    end type magma

    interface magma
        module procedure constructor_empty
        module procedure constructor_from_cuda
    end interface

    interface
        integer(c_int) function magmainit() bind(c, name="magma_init_wrapper")
            import ::  c_int
        end function magmainit

        integer(c_int) function magmafinalize() bind(c, name="magma_finalize_wrapper")
            import :: c_int
        end function magmafinalize

        integer(c_int) function magmagetdevice(dev) bind(c, name="magma_getdevice_wrapper")
            import :: c_ptr, c_int
            integer(c_int) :: dev
        end function magmagetdevice

        integer(c_int) function magmagetversion(major, minor, micro) bind(c, name="magma_version_wrapper")
            import :: c_ptr, c_int
            integer(c_int) :: major
            integer(c_int) :: minor
            integer(c_int) :: micro
        end function magmagetversion

        integer(c_int) function magmacreate(dev, queue) bind(c, name="magma_queue_create_wrapper")
            import :: c_ptr, c_int
            integer(c_int), value :: dev
            type(c_ptr) :: queue
        end function magmacreate

        integer(c_int) function magmacreate_from_cuda(dev, astream, cublas_handle, &
                    cusparse_handle, queue) bind(c, name="magma_queue_create_from_cuda_wrapper")
            import :: c_ptr, c_int
            integer(c_int), value :: dev
            type(c_ptr), value :: astream
            type(c_ptr), value :: cublas_handle
            type(c_ptr), value :: cusparse_handle
            type(c_ptr):: queue
        end function magmacreate_from_cuda

        type(c_ptr) function magma_queue_get_cuda_stream(queue) bind(c, name="magma_queue_get_cuda_stream_wrapper")
            import :: c_ptr
            type(c_ptr), value :: queue
        end function magma_queue_get_cuda_stream

        integer(c_int) function magmadestroy(queue) bind(c, name="magma_queue_destroy_wrapper")
            import :: c_int, c_ptr
            type(c_ptr), value :: queue
        end function magmadestroy

        integer(c_int) function magmasync(queue) bind(c, name="magma_queue_sync_wrapper")
            import :: c_int, c_ptr
            type(c_ptr), value :: queue
        end function magmasync

        subroutine magma_sgemm(transa, transb, m, n, k, &
                    alpha, a, lda, b, ldb, beta, c, ldc, queue) bind(c, name="magma_sgemm")
            import :: c_ptr, c_int, c_float
            integer(c_int), value :: transa, transb, m, n, k, lda, ldb, ldc
            type(c_ptr), value :: a, b
            type(c_ptr), value :: c
            real(c_float), value :: alpha, beta
            type(c_ptr), value :: queue
        end subroutine magma_sgemm

        subroutine magma_dgemm(transa, transb, m, n, k, &
                    alpha, a, lda, b, ldb, beta, c, ldc, queue) bind(c, name="magma_dgemm")
            import :: c_ptr, c_int, c_double
            integer(c_int), value :: transa, transb, m, n, k, lda, ldb, ldc
            type(c_ptr), value :: a, b
            type(c_ptr), value :: c
            real(c_double), value :: alpha, beta
            type(c_ptr), value :: queue
        end subroutine magma_dgemm

        subroutine magma_cgemm(transa, transb, m, n, k, &
                    alpha, a, lda, b, ldb, beta, c, ldc, queue) bind(c, name="magma_cgemm")
            import :: c_ptr, c_int, c_float
            integer(c_int), value :: transa, transb, m, n, k, lda, ldb, ldc
            type(c_ptr), value :: a, b
            type(c_ptr), value :: c
            complex(c_float), value :: alpha, beta
            type(c_ptr), value :: queue
        end subroutine magma_cgemm

        subroutine magma_zgemm(transa, transb, m, n, k, &
                    alpha, a, lda, b, ldb, beta, c, ldc, queue) bind(c, name="magma_zgemm")
            import :: c_ptr, c_int, c_double
            integer(c_int), value :: transa, transb, m, n, k, lda, ldb, ldc
            type(c_ptr), value :: a, b
            type(c_ptr), value :: c
            complex(c_double), value :: alpha, beta
            type(c_ptr), value :: queue
        end subroutine magma_zgemm

        subroutine magma_sgemm_vbatched(transa, transb, m, n, k, &
                    alpha, a, lda, b, ldb, beta, c, ldc, batchCount, queue) &
                    bind(c, name="magmablas_sgemm_vbatched")
            import :: c_ptr, c_int, c_float
            integer(c_int), value :: transa, transb
            type(c_ptr), value :: a, b
            type(c_ptr), value :: m, n, k, lda, ldb, ldc
            type(c_ptr), value :: c
            real(c_float), value :: alpha, beta
            integer(c_int), value :: batchCount
            type(c_ptr), value :: queue
        end subroutine magma_sgemm_vbatched

        subroutine magma_dgemm_vbatched(transa, transb, m, n, k, &
                    alpha, a, lda, b, ldb, beta, c, ldc, batchCount, queue) &
                    bind(c, name="magmablas_dgemm_vbatched")
            import :: c_ptr, c_int, c_double
            integer(c_int), value :: transa, transb
            type(c_ptr), value :: a, b
            type(c_ptr), value :: m, n, k, lda, ldb, ldc
            type(c_ptr), value :: c
            real(c_double), value :: alpha, beta
            integer(c_int), value :: batchCount
            type(c_ptr), value :: queue
        end subroutine magma_dgemm_vbatched

        subroutine magma_cgemm_vbatched(transa, transb, m, n, k, &
                    alpha, a, lda, b, ldb, beta, c, ldc, batchCount, queue) &
                    bind(c, name="magmablas_cgemm_vbatched")
            import :: c_ptr, c_int, c_float
            integer(c_int), value :: transa, transb
            type(c_ptr), value :: a, b
            type(c_ptr), value :: m, n, k, lda, ldb, ldc
            type(c_ptr), value :: c
            complex(c_float), value :: alpha, beta
            integer(c_int), value :: batchCount
            type(c_ptr), value :: queue
        end subroutine magma_cgemm_vbatched

        subroutine magma_zgemm_vbatched(transa, transb, m, n, k, &
                    alpha, a, lda, b, ldb, beta, c, ldc, batchCount, queue) &
                    bind(c, name="magmablas_zgemm_vbatched")
            import :: c_ptr, c_int, c_double
            integer(c_int), value :: transa, transb
            type(c_ptr), value :: a, b
            type(c_ptr), value :: m, n, k, lda, ldb, ldc
            type(c_ptr), value :: c
            complex(c_double), value :: alpha, beta
            integer(c_int), value :: batchCount
            type(c_ptr), value :: queue
        end subroutine magma_zgemm_vbatched
    end interface

    integer, save :: num_initialized = 0
contains
    function constructor_empty() result(this)
        type(magma) :: this

        call this%initialize()
    end function constructor_empty

    function constructor_from_cuda(astream) result(this)
        type(magma) :: this
        type(stream), intent(in) :: astream

        call this%initialize(astream)
    end function constructor_from_cuda

    subroutine initialize(this, astream)
        class(magma), intent(inout) :: this
        type(stream), intent(in), optional :: astream

        integer(c_int) :: error

        if( num_initialized > 0 ) then
            num_initialized = num_initialized + 1
        end if

        if( num_initialized == 0 ) then
            error = magmainit()
            if ( error /= 0 ) then
                error stop "magma::initialize:Could not initialize."
                stop
            end if

            error = magmagetdevice(this%dev)
            if ( error /= 0 ) then
                error stop "magma::initialize:Could not get device."
                stop
            end if

            error = this%create_queue(this%queue, astream)

            if ( error /= 0 ) then
                error stop "magma::initialize:Could not create queue."
                stop
            end if
            num_initialized = num_initialized + 1
        end if
    end subroutine initialize

    function get_num_initialized(this) result(num)
        class(magma), intent(inout) :: this

        integer :: num

        num = num_initialized
    end function get_num_initialized

    function get_device(this, dev) result(error)
        class(magma), intent(inout) :: this
        integer(c_int), intent(inout) :: dev

        integer(c_int) :: error

        error = magmagetdevice(dev)
    end function get_device

    function get_version(this, major, minor, micro) result(error)
        class(magma), intent(inout) :: this
        integer(c_int), intent(inout) :: major
        integer(c_int), intent(inout) :: minor
        integer(c_int), intent(inout) :: micro

        integer(c_int) :: error

        error = magmagetversion(major, minor, micro)
    end function get_version

    function sync(this, astream) result(error)
        class(magma), intent(in) :: this
        type(stream), intent(in), optional :: astream

        integer(c_int) :: error
        type(c_ptr) :: my_queue

        call this%populate_a_queue(my_queue, astream)
        error = magmasync(my_queue)
        call this%destroy_queue_if_necessary(my_queue, astream)
    end function sync

    function create_queue(this, aqueue, astream) result(error)
        class(magma), intent(in) :: this
        type(c_ptr), intent(inout) :: aqueue
        type(stream), intent(in), optional :: astream
        integer(c_int) :: error

        if ( present( astream ) ) then
            error = magmacreate_from_cuda(this%dev, astream%sid, c_null_ptr, c_null_ptr, aqueue)
        else
            error = magmacreate(this%dev, aqueue)
        end if
    end function create_queue

    subroutine populate_a_queue(this, aqueue, astream)
        class(magma), intent(in) :: this
        type(c_ptr), intent(inout) :: aqueue
        type(stream), intent(in), optional :: astream

        integer :: error

        error = 0
        if ( present( astream ) ) then
            error = this%create_queue(aqueue, astream)
        else
            aqueue = this%queue
        end if

        if ( error /= 0 ) &
                error stop "magma::populate_a_queue:Could not create a queue."
    end subroutine populate_a_queue

    function destroy_queue(this, aqueue) result(error)
        class(magma), intent(in) :: this
        type(c_ptr), intent(inout) :: aqueue
        integer(c_int) :: error

        error = magmadestroy(aqueue)
    end function destroy_queue

    subroutine destroy_queue_if_necessary(this, aqueue, astream)
        class(magma), intent(in) :: this
        type(c_ptr), intent(inout) :: aqueue
        type(stream), intent(in), optional :: astream

        integer :: error

        error = 0
        if ( present( astream ) ) &
                error = this%destroy_queue(aqueue)

        if ( error /= 0 ) &
                error stop "magma::destroy_queue_if_necessary:Could not destroy the queue."
    end subroutine destroy_queue_if_necessary

    subroutine cleanup(this)
        class(magma), intent(inout) :: this

        integer(c_int) :: error

        if (num_initialized == 1) then
            error = magmadestroy(this%queue)
            error = magmafinalize()
            this%queue = c_null_ptr
        end if
        if (num_initialized <= 0) then
            error stop "magma:: cleanup: kit has no instance to cleanup"
        end if
        num_initialized = num_initialized - 1
    end subroutine cleanup
end module magma_module
