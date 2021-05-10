#include <stdio.h>
#include <cublas_v2.h>

extern "C" int cublassetstream_wrapper( cublasHandle_t handle, cudaStream_t *stream )
{
    int error;
    if ( stream )
    {
        error = cublasSetStream(handle, *stream);
    } else
    {
        error = cublasSetStream(handle, NULL);
    }
    return error;
}
