#include <stdio.h>
#include <complex>
#include <cuComplex.h>

#include "cuda_common.h"

#define BLOCKSIZE 32

template <typename T>
__global__ void mmp_double_kernel(T alpha, T beta, int m, int n, int k, T *a, T *b, T *c)
{
    double ctemp = 0.0;
    int row = blockIdx.x*blockDim.x + threadIdx.x; // Transposed from Fortran
    int col = blockIdx.y*blockDim.y + threadIdx.y;

    if ( row >=m || col >= n ) return;
    for (int i = 0; i < k; i++)
    {
        ctemp += a[i*m + row]*b[col*k + i];
    }
    c[col*m + row] = ctemp*alpha + c[col*m + row]*beta;
}

__global__ void mmp_c64(cuComplex alpha, cuComplex beta, int m, int n, int k, cuComplex *a, cuComplex *b, cuComplex *c)
{
    cuComplex ctemp;
    ctemp.x = 0.0;
    ctemp.y = 0.0;
    int row = blockIdx.x*blockDim.x + threadIdx.x; // Transposed from Fortran
    int col = blockIdx.y*blockDim.y + threadIdx.y;

    if ( row >=m || col >= n ) return;
    for (int i = 0; i < k; i++)
    {
        ctemp = cuCaddf(ctemp, cuCmulf(a[i*m + row], b[col*k + i]));
    }
    c[col*m + row] = cuCaddf(cuCmulf(ctemp, alpha), cuCmulf(c[col*m + row], beta));
} 

__global__ void mmp_c128(cuDoubleComplex alpha, cuDoubleComplex beta, int m, int n, int k, cuDoubleComplex *a, cuDoubleComplex *b, cuDoubleComplex *c)
{
    cuDoubleComplex ctemp;
    ctemp.x = 0.0;
    ctemp.y = 0.0;
    int row = blockIdx.x*blockDim.x + threadIdx.x; // Transposed from Fortran
    int col = blockIdx.y*blockDim.y + threadIdx.y;

    if ( row >=m || col >= n ) return;
    for (int i = 0; i < k; i++)
    {
        ctemp = cuCadd(ctemp, cuCmul(a[i*m + row], b[col*k + i]));
    }
    c[col*m + row] = cuCadd(cuCmul(ctemp, alpha), cuCmul(c[col*m + row], beta));
}


extern "C" int cuda_mmp_execute_r32(float alpha, float beta, int m, int n, int k,
                    float *a, float *b, float *c)
{
    dim3 dimBlock(BLOCKSIZE,BLOCKSIZE);
    dim3 dimGrid((m-1)/(BLOCKSIZE)+1,(n-1)/(BLOCKSIZE)+1);
    mmp_double_kernel<<<dimGrid,dimBlock>>>(alpha, beta, m, n, k, a, b, c);
    return 0;
}

extern "C" int cuda_mmp_execute_r64(double alpha, double beta, int m, int n, int k,
                    double *a, double *b, double *c)
{
    dim3 dimBlock(BLOCKSIZE,BLOCKSIZE);
    dim3 dimGrid((m-1)/(BLOCKSIZE)+1,(n-1)/(BLOCKSIZE)+1);
    mmp_double_kernel<<<dimGrid,dimBlock>>>(alpha, beta, m, n, k, a, b, c);
    return 0;
}

extern "C" int cuda_mmp_execute_c64(cuComplex *alpha, cuComplex *beta, int m, int n, int k,
                    cuComplex *a, cuComplex *b, cuComplex *c)
{
    // data was passed using pointer due to compiler bug. 
    //In the kernel function, to pass data from host to device, the address can not be used and 
    // need to be dereference by * to pass the a copy of the value
    dim3 dimBlock(BLOCKSIZE,BLOCKSIZE);
    dim3 dimGrid((m-1)/(BLOCKSIZE)+1,(n-1)/(BLOCKSIZE)+1);
    mmp_c64<<<dimGrid,dimBlock>>>(*alpha, *beta, m, n, k, a, b, c);
    return 0;
}

extern "C" int cuda_mmp_execute_c128(cuDoubleComplex alpha, cuDoubleComplex beta, int m, int n, int k,
                    cuDoubleComplex *a, cuDoubleComplex *b, cuDoubleComplex *c)
{
    dim3 dimBlock(BLOCKSIZE,BLOCKSIZE);
    dim3 dimGrid((m-1)/(BLOCKSIZE)+1,(n-1)/(BLOCKSIZE)+1);
    mmp_c128<<<dimGrid,dimBlock>>>(alpha, beta, m, n, k, a, b, c);
    return 0;
}
