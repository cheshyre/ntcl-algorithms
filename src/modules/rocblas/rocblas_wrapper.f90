!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Copyright (c) 2020 Advanced Micro Devices, Inc.
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in
! all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
! THE SOFTWARE.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module rocblas_wrapper_module
    use, intrinsic :: iso_c_binding, only : &
            c_int, &
            c_ptr, &
            c_float, &
            c_double

    implicit none

    enum, bind(c)
        enumerator :: rocblas_operation_none = 111
        enumerator :: rocblas_operation_transpose = 112
        enumerator :: rocblas_operation_conjugate_transpose = 113
    end enum

    enum, bind(c)
        enumerator :: rocblas_status_success         = 0
    end enum

    interface
        integer(c_int) function rocblas_create_handle(handle) bind(C, name='rocblas_create_handle')
            import :: c_int
            import :: c_ptr
            type(c_ptr) :: handle
        end function rocblas_create_handle

        integer(c_int) function rocblas_destroy_handle(handle) bind(C, name='rocblas_destroy_handle')
            import :: c_int
            import :: c_ptr
            type(c_ptr), value :: handle
        end function rocblas_destroy_handle

        integer(c_int) function rocblas_set_stream(handle, stream) bind(c, name = 'rocblas_set_stream_wrapper')
            import :: c_int
            import :: c_ptr
            type(c_ptr), value :: handle
            type(c_ptr), value :: stream
        end function rocblas_set_stream

        integer(c_int) function rocblas_sgemm(handle, transA, transB, m, n, k, alpha, &
                A, lda, B, ldb, beta, C, ldc) &
                bind(c, name = 'rocblas_sgemm')
            import :: c_int
            import :: c_ptr
            import :: c_float
            import :: rocblas_operation_none

            type(c_ptr), value :: handle
            integer(kind(rocblas_operation_none)), value :: transA
            integer(kind(rocblas_operation_none)), value :: transB
            integer(c_int), value :: m
            integer(c_int), value :: n
            integer(c_int), value :: k
            real(c_float) :: alpha
            type(c_ptr), value :: A
            integer(c_int), value :: lda
            type(c_ptr), value :: B
            integer(c_int), value :: ldb
            real(c_float) :: beta
            type(c_ptr), value :: C
            integer(c_int), value :: ldc
        end function rocblas_sgemm

        integer(c_int) function rocblas_dgemm(handle, transA, transB, m, n, k, alpha, &
                A, lda, B, ldb, beta, C, ldc) &
                bind(c, name = 'rocblas_dgemm')
            import :: c_int
            import :: c_ptr
            import :: c_double
            import :: rocblas_operation_none

            type(c_ptr), value :: handle
            integer(kind(rocblas_operation_none)), value :: transA
            integer(kind(rocblas_operation_none)), value :: transB
            integer(c_int), value :: m
            integer(c_int), value :: n
            integer(c_int), value :: k
            real(c_double) :: alpha
            type(c_ptr), value :: A
            integer(c_int), value :: lda
            type(c_ptr), value :: B
            integer(c_int), value :: ldb
            real(c_double) :: beta
            type(c_ptr), value :: C
            integer(c_int), value :: ldc
        end function rocblas_dgemm

        integer(c_int) function rocblas_cgemm(handle, transA, transB, m, n, k, alpha, &
                A, lda, B, ldb, beta, C, ldc) &
                bind(c, name = 'rocblas_cgemm')
            import :: c_int
            import :: c_ptr
            import :: c_float
            import :: rocblas_operation_none

            type(c_ptr), value :: handle
            integer(kind(rocblas_operation_none)), value :: transA
            integer(kind(rocblas_operation_none)), value :: transB
            integer(c_int), value :: m
            integer(c_int), value :: n
            integer(c_int), value :: k
            complex(c_float) :: alpha
            type(c_ptr), value :: A
            integer(c_int), value :: lda
            type(c_ptr), value :: B
            integer(c_int), value :: ldb
            complex(c_float) :: beta
            type(c_ptr), value :: C
            integer(c_int), value :: ldc
        end function rocblas_cgemm

        integer(c_int) function rocblas_zgemm(handle, transA, transB, m, n, k, alpha, &
                A, lda, B, ldb, beta, C, ldc) &
                bind(c, name = 'rocblas_zgemm')
            import :: c_int
            import :: c_ptr
            import :: c_double
            import :: rocblas_operation_none

            type(c_ptr), value :: handle
            integer(kind(rocblas_operation_none)), value :: transA
            integer(kind(rocblas_operation_none)), value :: transB
            integer(c_int), value :: m
            integer(c_int), value :: n
            integer(c_int), value :: k
            complex(c_double) :: alpha
            type(c_ptr), value :: A
            integer(c_int), value :: lda
            type(c_ptr), value :: B
            integer(c_int), value :: ldb
            complex(c_double) :: beta
            type(c_ptr), value :: C
            integer(c_int), value :: ldc
        end function rocblas_zgemm
    end interface
end module rocblas_wrapper_module
