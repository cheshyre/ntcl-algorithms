#include "hip/hip_runtime_api.h"
#include "rocblas.h"
#include "hip_common.h"

extern "C" rocblas_status rocblas_set_stream_wrapper(rocblas_handle handle, hipStream_t *stream) {
    rocblas_status error;
    if (stream ) {
        error = rocblas_set_stream(handle, *stream);
    } else error = rocblas_set_stream(handle, NULL);
    check_hip_error((hipError_t) error);
    return error;
}
