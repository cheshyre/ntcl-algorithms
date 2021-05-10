module cublas_wrapper_module
    use, intrinsic :: iso_c_binding, only : &
        c_ptr, &
        c_int, &
        c_float, &
        c_double

    implicit none
    private

    public :: cublascreate
    public :: cublasdestroy
    public :: cublassgemm
    public :: cublasdgemm
    public :: cublascgemm
    public :: cublaszgemm
    public :: cublasdgemv
    public :: cublassetstream
    public :: cublasdgemm_wrapper

    interface
        integer(c_int) function cublascreate(handle) bind(c, name="cublasCreate_v2")
            import :: c_ptr, c_int
            type(c_ptr) :: handle
        end function cublascreate

        integer(c_int) function cublasdestroy(handle) bind(c, name="cublasDestroy_v2")
            import :: c_ptr, c_int
            type(c_ptr), value :: handle
        end function cublasdestroy

        integer(c_int) function cublasgetversion(handle, version) bind(c, name="cublasGetVersion_v2")
            import :: c_ptr, c_int
            type(c_ptr), value :: handle
            integer(c_int) :: version
        end function cublasgetversion

        integer(c_int) function cublassgemm(handle, transa, transb, m, n, k, &
                    alpha, a, lda, b, ldb, beta, c, ldc) bind(c, name="cublasSgemm_v2")
            import :: c_ptr, c_int, c_float
            integer(c_int), value :: transa, transb, m, n, k, lda, ldb, ldc
            type(c_ptr), value :: handle
            type(c_ptr), value :: a, b, c
            real(c_float) :: alpha, beta
        end function cublassgemm

        integer(c_int) function cublasdgemm(handle, transa, transb, m, n, k, &
                    alpha, a, lda, b, ldb, beta, c, ldc) bind(c, name="cublasDgemm_v2")
            import :: c_ptr, c_int, c_double
            integer(c_int), value :: transa, transb, m, n, k, lda, ldb, ldc
            type(c_ptr), value :: handle
            type(c_ptr), value :: a, b, c
            real(c_double) :: alpha, beta
        end function cublasdgemm

        integer(c_int) function cublascgemm(handle, transa, transb, m, n, k, &
                    alpha, a, lda, b, ldb, beta, c, ldc) bind(c, name="cublasCgemm_v2")
            import :: c_ptr, c_int
            integer(c_int), value :: transa, transb, m, n, k, lda, ldb, ldc
            type(c_ptr), value :: handle
            type(c_ptr), value :: a, b, c, alpha, beta
        end function cublascgemm

        integer(c_int) function cublaszgemm(handle, transa, transb, m, n, k, &
                    alpha, a, lda, b, ldb, beta, c, ldc) bind(c, name="cublasZgemm_v2")
            import :: c_ptr, c_int
            integer(c_int), value :: transa, transb, m, n, k, lda, ldb, ldc
            type(c_ptr), value :: handle
            type(c_ptr), value :: a, b, c, alpha, beta
        end function cublaszgemm

        integer(c_int) function cublasdgemv(handle, transa, m, n, &
                    alpha, a, lda, x, incx, beta, y, incy) bind(c, name="cublasDgemv_v2")
            import :: c_ptr, c_int, c_double
            integer(c_int), value :: transa, m, n, lda, incx, incy
            type(c_ptr), value :: handle
            type(c_ptr), value :: a, x, y
            real(c_double) :: alpha, beta
        end function cublasdgemv

        integer(c_int) function cublasdgemm_wrapper(handle, transa, transb, m, n, k, &
                    alpha, a, lda, b, ldb, beta, c, ldc) bind(c, name="cublasdgemm_wrapper")
            import :: c_ptr, c_int, c_double
            integer(c_int), value :: transa, transb, m, n, k, lda, ldb, ldc
            type(c_ptr), value :: handle
            type(c_ptr), value :: a, b, c
            real(c_double) :: alpha, beta
        end function cublasdgemm_wrapper

        integer(c_int) function cublassetstream(handle, stream) &
                bind(c, name="cublassetstream_wrapper")
            import :: c_ptr, c_int
            type(c_ptr), value :: handle, stream
        end function cublassetstream
    end interface
end module cublas_wrapper_module
