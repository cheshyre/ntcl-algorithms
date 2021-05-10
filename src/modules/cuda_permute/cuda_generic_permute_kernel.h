#ifndef cuda_generic_permute_kernel_hpp
#define cuda_generic_permute_kernel_hpp

#define MAX_ORDER 16

template <typename T>
__global__ void cuda_tensor_permute_generic_kernel(T*, T*, int*, int*, int, int, int );
#endif
