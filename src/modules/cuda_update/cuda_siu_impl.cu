#include <complex>
#include <iostream>
#include <cuComplex.h>

#include "cuda_common.h"

template <typename T>
__global__ void siu_kernel(T *dst, long nelements, T alpha) {
    long idx = ((long) blockIdx.x)*((long) blockDim.x) + threadIdx.x;

    if ( idx < nelements) dst[idx] = alpha*dst[idx];
}

__global__ void siu_kernel_c64(cuComplex *dst, long nelements, cuComplex alpha) {
    long idx = ((long) blockIdx.x)*((long) blockDim.x) + threadIdx.x;

    if ( idx < nelements) dst[idx] = cuCmulf(alpha,dst[idx]);
}

__global__ void siu_kernel_c128(cuDoubleComplex *dst, long nelements, cuDoubleComplex alpha) {
    long idx = ((long) blockIdx.x)*((long) blockDim.x) + threadIdx.x;

    if ( idx < nelements) dst[idx] = cuCmul(alpha,dst[idx]);
}

extern "C" void perform_siu_r32(float *dst, int64_t nelements, float alpha, cudaStream_t *stream) {
    int block_size = 256;
    int nblocks = (int) (nelements-1)/(block_size)+1;

    dim3 blockGrid(nblocks);
    dim3 thread_per_block(block_size);
    if ( stream ) {
        siu_kernel<float><<<nblocks, block_size, 0, *stream>>>(dst, nelements, alpha);
    } else {
        siu_kernel<float><<<nblocks, block_size>>>(dst, nelements, alpha);
    }
    check_cuda_error( cudaPeekAtLastError() );
}

extern "C" void perform_siu_r64(double *dst, int64_t nelements, double alpha, cudaStream_t *stream) {
    int block_size = 256;
    int nblocks = (int) (nelements-1)/(block_size)+1;

    dim3 blockGrid(nblocks);
    dim3 thread_per_block(block_size);
    if ( stream ) {
        siu_kernel<double><<<nblocks, block_size, 0, *stream>>>(dst, nelements, alpha);
    } else {
        siu_kernel<double><<<nblocks, block_size>>>(dst, nelements, alpha);
    }
    check_cuda_error( cudaPeekAtLastError() );
}

extern "C" void perform_siu_c64(cuComplex *dst, int64_t nelements, cuComplex alpha, cudaStream_t *stream) {
    int block_size = 256;
    int nblocks = (int) (nelements-1)/(block_size)+1;

    dim3 blockGrid(nblocks);
    dim3 thread_per_block(block_size);
    if ( stream ) {
        siu_kernel_c64<<<nblocks, block_size, 0, *stream>>>(dst, nelements, alpha);
    } else {
        siu_kernel_c64<<<nblocks, block_size>>>(dst, nelements, alpha);
    }
    check_cuda_error( cudaPeekAtLastError() );
}

extern "C" void perform_siu_c128(cuDoubleComplex *dst, int64_t nelements, cuDoubleComplex alpha, cudaStream_t *stream) {
    int block_size = 256;
    int nblocks = (int) (nelements-1)/(block_size)+1;

    dim3 blockGrid(nblocks);
    dim3 thread_per_block(block_size);
    if ( stream ) {
        siu_kernel_c128<<<nblocks, block_size, 0, *stream>>>(dst, nelements, alpha);
    } else {
        siu_kernel_c128<<<nblocks, block_size>>>(dst, nelements, alpha);
    }
    check_cuda_error( cudaPeekAtLastError() );
}
