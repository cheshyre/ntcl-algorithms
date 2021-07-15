#ifndef hip_tailored_permute_kernels_hpp
#define hip_tailored_permute_kernels_hpp

#include "hip/hip_runtime.h"

template <typename T>
__global__ void hip_tensor_permute_1_4_3_2_kernel(T*, T*, int*, unsigned int );

template <typename T>
__global__ void hip_tensor_permute_large_first_kernel(T*, T*, int*, int*, unsigned int, unsigned int);
#endif
