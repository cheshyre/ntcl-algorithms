#include <complex>
#include "cuda_tailored_permute_kernels.h"

template <typename T>
__global__ void cuda_tensor_permute_1_4_3_2_kernel(T *src_tensor, T *dst_tensor, int *src_dims, unsigned int elements_per_thread){

  __shared__ uint3 sdim, qrs;
  __shared__ unsigned int np;

  unsigned int soffset, doffset, idx;

  idx = blockIdx.x*blockDim.x + threadIdx.x;

  if ( threadIdx.x == 0 ) {
    np = src_dims[0];
    sdim.x = src_dims[1]; sdim.y = src_dims[2]; sdim.z = src_dims[3];
  }
  __syncthreads();

  soffset = blockIdx.y*np;

  if ( threadIdx.x == 0 ) {
    // x == q, y == r, z == s
    // qrs.x + sdims.x*qrs.y + sdims.x*sdims.y*qrs.z = blockIdx.y
    qrs.x = blockIdx.y%sdim.x;
    qrs.y = (blockIdx.y/sdim.x)%sdim.y;
    qrs.z = (blockIdx.y/(sdim.x*sdim.y))%sdim.z;
  }
  __syncthreads();

  doffset = (qrs.x*sdim.y*sdim.z + qrs.y*sdim.z + qrs.z)*np;

  if ( idx < np ) {
    dst_tensor[doffset + idx]= src_tensor[soffset + idx];
  }
}

template __global__ void cuda_tensor_permute_1_4_3_2_kernel<float>(float*, float*, int*, unsigned int);
template __global__ void cuda_tensor_permute_1_4_3_2_kernel<double>(double*, double*, int*, unsigned int);
template __global__ void cuda_tensor_permute_1_4_3_2_kernel<std::complex<float>>(std::complex<float>*, std::complex<float>*, int*, unsigned int);
template __global__ void cuda_tensor_permute_1_4_3_2_kernel<std::complex<double>>(std::complex<double>*, std::complex<double>*, int*, unsigned int);

template <typename T>
__global__ void cuda_tensor_permute_large_first_kernel(T *src_tensor, T *dst_tensor, int *src_dims, int *perm,
    unsigned int order, unsigned int elements_per_thread){

  extern __shared__ unsigned int shared[]; // 4*order*sizeof(unsigned int)

  unsigned int *dims = shared;
  unsigned int *lookup = &dims[order];
  unsigned int *strides = &lookup[order];
  unsigned int *qrs = &strides[order];

  unsigned int soffset, doffset, firstidx, stride;

  if ( threadIdx.x < order ) dims[threadIdx.x] = src_dims[threadIdx.x];
  if ( threadIdx.x >= order && threadIdx.x < 2*order ) lookup[threadIdx.x-order] = perm[threadIdx.x-order];

  firstidx = blockIdx.x*blockDim.x + threadIdx.x;
  __syncthreads();

  if ( threadIdx.x == 0 ) {
    strides[0] = 1;
    for (unsigned int idx=0; idx < order-1; idx++) {
      strides[idx+1] = strides[idx]*dims[idx];
    }
  }
  soffset = blockIdx.y*dims[0];
  __syncthreads();

  if ( threadIdx.x > 0 && threadIdx.x < order ) {
    qrs[threadIdx.x] = (soffset/strides[threadIdx.x])%dims[threadIdx.x];
  }
  __syncthreads();

  doffset = 0;
  stride = dims[0];
  for (unsigned int idx=1; idx < order; idx++) {
    doffset += stride*qrs[lookup[idx]];
    stride *= dims[lookup[idx]];
  }

  if ( firstidx < dims[0] ) {
    dst_tensor[doffset + firstidx]= src_tensor[soffset + firstidx];
  }
}

template __global__ void cuda_tensor_permute_large_first_kernel<float>(float*, float*, int*, int*, unsigned int, unsigned int);
template __global__ void cuda_tensor_permute_large_first_kernel<double>(double*, double*, int*, int*, unsigned int, unsigned int);
template __global__ void cuda_tensor_permute_large_first_kernel<std::complex<float>>(std::complex<float>*, std::complex<float>*, int*, int*, unsigned int, unsigned int);
template __global__ void cuda_tensor_permute_large_first_kernel<std::complex<double>>(std::complex<double>*, std::complex<double>*, int*, int*, unsigned int, unsigned int);

