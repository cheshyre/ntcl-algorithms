#ifndef hip_generic_permute_kernel_hpp
#define hip_generic_permute_kernel_hpp

#include "hip/hip_runtime.h"

#define MAX_ORDER 16

template <typename T>
__global__ void hip_tensor_permute_generic_kernel(T*, T*, int*, int*, int, int, int );
#endif
