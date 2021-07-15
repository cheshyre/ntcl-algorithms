#include <complex>
#include <stdio.h>

#include <cuda_common.h>
#include "cuda_generic_permute_kernel.h"

extern "C" void cuda_tensor_permute_generic_real32_execute(float *d_src, float *d_dst, int64_t nElems, int* d_sdim, int* d_perm, int order){

  // For a TILE_DIM*TILE_DIM block of tensor elements
  // launch TILE_DIM*threadStride number of threads.
  // Each thread is responsible for TILE_DIM/threadStride
  // number of elements, so this number must divide evenly
  int TILE_DIM = 32;
  int threadStride = 8;
  int nBlocks = ((nElems-1) / TILE_DIM/TILE_DIM) + 1;

  dim3 blockGrid(nBlocks);
  dim3 threadsPerBlock(TILE_DIM, threadStride, 1);
  cuda_tensor_permute_generic_kernel<float><<<blockGrid, threadsPerBlock>>>(d_src, d_dst, d_sdim, d_perm, order, TILE_DIM, threadStride);
  check_cuda_error( cudaPeekAtLastError() );
}

extern "C" void cuda_tensor_permute_generic_real64_execute(double *d_src, double *d_dst, int64_t nElems, int* d_sdim, int* d_perm, int order){

  // For a TILE_DIM*TILE_DIM block of tensor elements
  // launch TILE_DIM*threadStride number of threads.
  // Each thread is responsible for TILE_DIM/threadStride
  // number of elements, so this number must divide evenly
  int TILE_DIM = 32;
  int threadStride = 8;
  int nBlocks = ((nElems-1) / TILE_DIM/TILE_DIM) + 1;

  dim3 blockGrid(nBlocks);
  dim3 threadsPerBlock(TILE_DIM, threadStride, 1);
  cuda_tensor_permute_generic_kernel<double><<<blockGrid, threadsPerBlock>>>(d_src, d_dst, d_sdim, d_perm, order, TILE_DIM, threadStride);
  check_cuda_error( cudaPeekAtLastError() );
}

extern "C" void cuda_tensor_permute_generic_complex64_execute(std::complex<float> *d_src, std::complex<float> *d_dst,
        int64_t nElems, int* d_sdim, int* d_perm, int order){

  // For a TILE_DIM*TILE_DIM block of tensor elements
  // launch TILE_DIM*threadStride number of threads.
  // Each thread is responsible for TILE_DIM/threadStride
  // number of elements, so this number must divide evenly
  int TILE_DIM = 32;
  int threadStride = 8;
  int nBlocks = ((nElems-1) / TILE_DIM/TILE_DIM) + 1;

  dim3 blockGrid(nBlocks);
  dim3 threadsPerBlock(TILE_DIM, threadStride, 1);
  cuda_tensor_permute_generic_kernel<std::complex<float>><<<blockGrid, threadsPerBlock>>>(d_src, d_dst,
              d_sdim, d_perm, order, TILE_DIM, threadStride);
  check_cuda_error( cudaPeekAtLastError() );
}

extern "C" void cuda_tensor_permute_generic_complex128_execute(std::complex<double> *d_src, std::complex<double> *d_dst,
        int64_t nElems, int* d_sdim, int* d_perm, int order){

  // For a TILE_DIM*TILE_DIM block of tensor elements
  // launch TILE_DIM*threadStride number of threads.
  // Each thread is responsible for TILE_DIM/threadStride
  // number of elements, so this number must divide evenly
  int TILE_DIM = 32;
  int threadStride = 8;
  int nBlocks = ((nElems-1) / TILE_DIM/TILE_DIM) + 1;

  dim3 blockGrid(nBlocks);
  dim3 threadsPerBlock(TILE_DIM, threadStride, 1);
  cuda_tensor_permute_generic_kernel<std::complex<double>><<<blockGrid, threadsPerBlock>>>(d_src, d_dst,
              d_sdim, d_perm, order, TILE_DIM, threadStride);
  check_cuda_error( cudaPeekAtLastError() );
}
