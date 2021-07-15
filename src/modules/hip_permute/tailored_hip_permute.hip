#include <complex>

#include <hip_common.h>
#include "hip_tailored_permute_kernels.h"

extern "C" void hip_tensor_permute_1_4_3_2_real32_execute(float *d_src, float *d_dst, int64_t nElems, int np, int* d_sdim, hipStream_t *stream){

  unsigned int elements_per_thread = 1; // Not implemented
  unsigned int threads_per_block = 256;
  unsigned int elements_per_block = elements_per_thread*threads_per_block;
  unsigned int nBlocksx = ((np-1)/elements_per_block) + 1;
  unsigned int nBlocksy = nElems/np;

  dim3 blockGrid(nBlocksx, nBlocksy);
  if ( stream ) {
      hipLaunchKernelGGL(hip_tensor_permute_1_4_3_2_kernel<float>, blockGrid, threads_per_block, 0, *stream, d_src, d_dst, d_sdim, elements_per_thread);
  } else {
      hipLaunchKernelGGL(hip_tensor_permute_1_4_3_2_kernel<float>, blockGrid, threads_per_block, 0, 0, d_src, d_dst, d_sdim, elements_per_thread);
  }
  check_hip_error( hipPeekAtLastError() );
}

extern "C" void hip_tensor_permute_1_4_3_2_real64_execute(double *d_src, double *d_dst, int64_t nElems, int np, int* d_sdim, hipStream_t *stream){

  unsigned int elements_per_thread = 1; // Not implemented
  unsigned int threads_per_block = 256;
  unsigned int elements_per_block = elements_per_thread*threads_per_block;
  unsigned int nBlocksx = ((np-1)/elements_per_block) + 1;
  unsigned int nBlocksy = nElems/np;

  dim3 blockGrid(nBlocksx, nBlocksy);
  if ( stream ) {
      hipLaunchKernelGGL(hip_tensor_permute_1_4_3_2_kernel<double>, blockGrid, threads_per_block, 0, *stream, d_src, d_dst, d_sdim, elements_per_thread);
  } else {
      hipLaunchKernelGGL(hip_tensor_permute_1_4_3_2_kernel<double>, blockGrid, threads_per_block, 0, 0, d_src, d_dst, d_sdim, elements_per_thread);
  }
  check_hip_error( hipPeekAtLastError() );

}

extern "C" void hip_tensor_permute_1_4_3_2_complex64_execute(std::complex<float> *d_src, std::complex<float> *d_dst, int64_t nElems, int np,
    int* d_sdim, hipStream_t *stream){

  unsigned int elements_per_thread = 1; // Not implemented
  unsigned int threads_per_block = 256;
  unsigned int elements_per_block = elements_per_thread*threads_per_block;
  unsigned int nBlocksx = ((np-1)/elements_per_block) + 1;
  unsigned int nBlocksy = nElems/np;

  dim3 blockGrid(nBlocksx, nBlocksy);
  if ( stream ) {
      hipLaunchKernelGGL(hip_tensor_permute_1_4_3_2_kernel<std::complex<float> >, blockGrid, threads_per_block, 0, *stream, d_src, d_dst, d_sdim, elements_per_thread);
  } else {
      hipLaunchKernelGGL(hip_tensor_permute_1_4_3_2_kernel<std::complex<float> >, blockGrid, threads_per_block, 0, 0, d_src, d_dst, d_sdim, elements_per_thread);
  }
  check_hip_error( hipPeekAtLastError() );

}

extern "C" void hip_tensor_permute_1_4_3_2_complex128_execute(std::complex<double> *d_src, std::complex<double> *d_dst, int64_t nElems, int np,
    int* d_sdim, hipStream_t *stream){

  unsigned int elements_per_thread = 1; // Not implemented
  unsigned int threads_per_block = 256;
  unsigned int elements_per_block = elements_per_thread*threads_per_block;
  unsigned int nBlocksx = ((np-1)/elements_per_block) + 1;
  unsigned int nBlocksy = nElems/np;

  dim3 blockGrid(nBlocksx, nBlocksy);
  if ( stream ) {
      hipLaunchKernelGGL(hip_tensor_permute_1_4_3_2_kernel<std::complex<double> >, blockGrid, threads_per_block, 0, *stream, d_src, d_dst, d_sdim, elements_per_thread);
  } else {
      hipLaunchKernelGGL(hip_tensor_permute_1_4_3_2_kernel<std::complex<double> >, blockGrid, threads_per_block, 0, 0, d_src, d_dst, d_sdim, elements_per_thread);
  }
  check_hip_error( hipPeekAtLastError() );

}

// Large first unpermuted dimension
extern "C" void hip_tensor_permute_large_first_real32_execute(float *d_src, float *d_dst, int64_t nElems, int np, int* d_sdim, int* d_perm, int order, hipStream_t *stream){

  unsigned int elements_per_thread = 1; // Not implemented
  unsigned int threads_per_block = 256;
  unsigned int elements_per_block = elements_per_thread*threads_per_block;
  unsigned int nBlocksx = ((np-1)/elements_per_block) + 1;
  unsigned int nBlocksy = nElems/np;
  unsigned int bytes_of_shared_memory = 3*order*sizeof(unsigned int) + order*sizeof(uint64_t);

  dim3 blockGrid(nBlocksx, nBlocksy);
  if ( stream ) {
      hipLaunchKernelGGL(hip_tensor_permute_large_first_kernel<float>, blockGrid, threads_per_block, bytes_of_shared_memory, *stream, d_src,
          d_dst, d_sdim, d_perm, order, elements_per_thread);
  } else {
      hipLaunchKernelGGL(hip_tensor_permute_large_first_kernel<float>, blockGrid, threads_per_block, bytes_of_shared_memory, 0, d_src,
          d_dst, d_sdim, d_perm, order, elements_per_thread);
  }
  check_hip_error( hipPeekAtLastError() );
}

extern "C" void hip_tensor_permute_large_first_real64_execute(double *d_src, double *d_dst, int64_t nElems, int np, int* d_sdim, int* d_perm, int order, hipStream_t *stream){

  unsigned int elements_per_thread = 1; // Not implemented
  unsigned int threads_per_block = 256;
  unsigned int elements_per_block = elements_per_thread*threads_per_block;
  unsigned int nBlocksx = ((np-1)/elements_per_block) + 1;
  unsigned int nBlocksy = nElems/np;
  unsigned int bytes_of_shared_memory = 3*order*sizeof(unsigned int) + order*sizeof(uint64_t);

  dim3 blockGrid(nBlocksx, nBlocksy);
  if ( stream ) {
      hipLaunchKernelGGL(hip_tensor_permute_large_first_kernel<double>, blockGrid, threads_per_block, bytes_of_shared_memory, *stream, d_src,
          d_dst, d_sdim, d_perm, order, elements_per_thread);
  } else {
      hipLaunchKernelGGL(hip_tensor_permute_large_first_kernel<double>, blockGrid, threads_per_block, bytes_of_shared_memory, 0, d_src,
          d_dst, d_sdim, d_perm, order, elements_per_thread);
  }
  check_hip_error( hipPeekAtLastError() );
}

extern "C" void hip_tensor_permute_large_first_complex64_execute(std::complex<float> *d_src, std::complex<float> *d_dst, int64_t nElems, int np, int* d_sdim, int* d_perm, int order, hipStream_t *stream){

  unsigned int elements_per_thread = 1; // Not implemented
  unsigned int threads_per_block = 256;
  unsigned int elements_per_block = elements_per_thread*threads_per_block;
  unsigned int nBlocksx = ((np-1)/elements_per_block) + 1;
  unsigned int nBlocksy = nElems/np;
  unsigned int bytes_of_shared_memory = 3*order*sizeof(unsigned int) + order*sizeof(uint64_t);

  dim3 blockGrid(nBlocksx, nBlocksy);
  if ( stream ) {
      hipLaunchKernelGGL(hip_tensor_permute_large_first_kernel<std::complex<float>>, blockGrid, threads_per_block, bytes_of_shared_memory, *stream, d_src,
          d_dst, d_sdim, d_perm, order, elements_per_thread);
  } else {
      hipLaunchKernelGGL(hip_tensor_permute_large_first_kernel<std::complex<float>>, blockGrid, threads_per_block, bytes_of_shared_memory, 0, d_src,
          d_dst, d_sdim, d_perm, order, elements_per_thread);
  }
  check_hip_error( hipPeekAtLastError() );
}

extern "C" void hip_tensor_permute_large_first_complex128_execute(std::complex<double> *d_src, std::complex<double> *d_dst, int64_t nElems, int np, int* d_sdim, int* d_perm, int order, hipStream_t *stream){

  unsigned int elements_per_thread = 1; // Not implemented
  unsigned int threads_per_block = 256;
  unsigned int elements_per_block = elements_per_thread*threads_per_block;
  unsigned int nBlocksx = ((np-1)/elements_per_block) + 1;
  unsigned int nBlocksy = nElems/np;
  unsigned int bytes_of_shared_memory = 3*order*sizeof(unsigned int) + order*sizeof(uint64_t);

  dim3 blockGrid(nBlocksx, nBlocksy);
  if ( stream ) {
      hipLaunchKernelGGL(hip_tensor_permute_large_first_kernel<std::complex<double>>, blockGrid, threads_per_block, bytes_of_shared_memory, *stream, d_src,
          d_dst, d_sdim, d_perm, order, elements_per_thread);
  } else {
      hipLaunchKernelGGL(hip_tensor_permute_large_first_kernel<std::complex<double>>, blockGrid, threads_per_block, bytes_of_shared_memory, 0, d_src,
          d_dst, d_sdim, d_perm, order, elements_per_thread);
  }
  check_hip_error( hipPeekAtLastError() );
}
