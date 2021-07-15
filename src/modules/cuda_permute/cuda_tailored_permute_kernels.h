#ifndef cuda_tailored_permute_kernels_hpp
#define cuda_tailored_permute_kernels_hpp

#define MAX_ORDER 16

template <typename T>
__global__ void cuda_tensor_permute_1_4_3_2_kernel(T*, T*, int*, unsigned int );

template <typename T>
__global__ void cuda_tensor_permute_large_first_kernel(T*, T*, int*, int*, unsigned int, unsigned int);
#endif
