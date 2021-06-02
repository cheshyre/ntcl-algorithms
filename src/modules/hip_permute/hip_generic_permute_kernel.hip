#include <complex>
#include "hip_generic_permute_kernel.h"
// Fortran is column major, where the leftmost index is the fastest.
// These functions will assume that indices move left to right
// in order of fastest to slowest.

template <typename T>
__global__ void hip_tensor_permute_generic_kernel(T *src_tensor, T *dst_tensor, int *src_dims, int *perm, int order, int tileDim, int threadStride){
  int dst_idx[MAX_ORDER];
  int src_idx[MAX_ORDER];
  int src_stride[MAX_ORDER];
  int dst_stride[MAX_ORDER];

  // calculate strides
  int accumulate = 1;
  for(int i = 0; i < order; i++){
    src_stride[i] = accumulate;
    accumulate *= src_dims[i];
  }

  accumulate = 1;
  for(int i = 0; i < order; i++){
    dst_stride[i] = accumulate;
    accumulate *= src_dims[ perm[i] ];
  }

  for(int idx = 0; idx < tileDim; idx += threadStride){
    int src_global_idx = blockIdx.x*(tileDim*tileDim) + threadIdx.y*tileDim + threadIdx.x;
    src_global_idx += idx*tileDim;
    // calculate multi-indices
    for(int i = 0; i < order; i++){
      src_idx[i] = (src_global_idx/src_stride[i])%src_dims[i];
    }

    for(int i = 0; i < order; i++){
      dst_idx[i] = src_idx[ perm[i] ];
    }

    // calculate global dst index
    accumulate = 0;
    for(int i = 0; i < order; i ++){
      accumulate += dst_idx[i]*dst_stride[i];
    }
    int dst_global_idx = accumulate;

    if( src_global_idx < src_stride[order-1]*src_dims[order-1] ){
      dst_tensor[ dst_global_idx ] = src_tensor[ src_global_idx ];
    }
  }
}

template __global__ void hip_tensor_permute_generic_kernel<float>(float*, float*, int*, int*, int, int, int);
template __global__ void hip_tensor_permute_generic_kernel<double>(double*, double*, int*, int*, int, int, int);
template __global__ void hip_tensor_permute_generic_kernel<std::complex<float>>(std::complex<float>*, std::complex<float>*, int*, int*, int, int, int);
template __global__ void hip_tensor_permute_generic_kernel<std::complex<double>>(std::complex<double>*, std::complex<double>*, int*, int*, int, int, int);
