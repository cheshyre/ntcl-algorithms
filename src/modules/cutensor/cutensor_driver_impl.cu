#include <stdlib.h>
#include <iostream>
#include <complex>

#include <cuda_runtime.h>
#include <cutensor.h>

#include <unordered_map>
#include <vector>

// Handle cuTENSOR errors
#define HANDLE_ERROR(x) {                                                              \
  const auto err = x;                                                                  \
  if( x != CUTENSOR_STATUS_SUCCESS )                                                   \
  { printf("Error: %s in line %d\n", cutensorGetErrorString(x), __LINE__); exit(-1); } \
}

// Initialize cuTENSOR library
extern "C"
int cutensor_init(cutensorHandle_t* &vhandle){
  cutensorStatus_t err;

  vhandle = (cutensorHandle_t*) malloc( sizeof(cutensorHandle_t) );

  err = cutensorInit(vhandle);

  // Check for errors
  if(err != CUTENSOR_STATUS_SUCCESS)
  {
    printf("ERROR: %s\n", cutensorGetErrorString(err));
    return -1;
  }

  return 0;
}

// free handle
extern "C"
int cutensor_free(cutensorHandle_t* vhandle){

  free(vhandle);
  vhandle = NULL;

  return 0;
}

extern "C"
int cutensor_contract_r32(cutensorHandle_t* handle,
    float* c, int c_rank, long* c_dims, int* c_inds,
    float* a, int a_rank, long* a_dims, int* a_inds,
    float* b, int b_rank, long* b_dims, int* b_inds,
    float alpha,
    float beta){

  cudaDataType_t typeA = CUDA_R_32F;
  cudaDataType_t typeB = CUDA_R_32F;
  cudaDataType_t typeC = CUDA_R_32F;
  cutensorComputeType_t typeCompute = CUTENSOR_COMPUTE_32F;

  /* ***************************** */

  // Create Tensor Descriptors
  cutensorTensorDescriptor_t descA;
  HANDLE_ERROR( cutensorInitTensorDescriptor( handle,
        &descA,
        a_rank,
        a_dims,
        NULL,/*stride*/
        typeA, CUTENSOR_OP_IDENTITY ) );

  cutensorTensorDescriptor_t descB;
  HANDLE_ERROR( cutensorInitTensorDescriptor( handle,
        &descB,
        b_rank,
        b_dims,
        NULL,/*stride*/
        typeB, CUTENSOR_OP_IDENTITY ) );

  cutensorTensorDescriptor_t descC;
  HANDLE_ERROR( cutensorInitTensorDescriptor( handle,
        &descC,
        c_rank,
        c_dims,
        NULL,/*stride*/
        typeC, CUTENSOR_OP_IDENTITY ) );

  /* ***************************** */

  //Retrieve the memory alignment for each tensor
  uint32_t alignmentRequirementA;
  HANDLE_ERROR( cutensorGetAlignmentRequirement( handle,
        a,
        &descA,
        &alignmentRequirementA) );

  uint32_t alignmentRequirementB;
  HANDLE_ERROR( cutensorGetAlignmentRequirement( handle,
        b,
        &descB,
        &alignmentRequirementB) );

  uint32_t alignmentRequirementC;
  HANDLE_ERROR( cutensorGetAlignmentRequirement( handle,
        c,
        &descC,
        &alignmentRequirementC) );


  /* ***************************** */

  // Create the Contraction Descriptor
  cutensorContractionDescriptor_t desc;
  HANDLE_ERROR( cutensorInitContractionDescriptor( handle,
        &desc,
        &descA, a_inds, alignmentRequirementA,
        &descB, b_inds, alignmentRequirementB,
        &descC, c_inds, alignmentRequirementC,
        &descC, c_inds, alignmentRequirementC,
        typeCompute) );

  /* ***************************** */

  // Set the algorithm to use
  cutensorContractionFind_t find;
  HANDLE_ERROR( cutensorInitContractionFind(
        handle, &find,
        CUTENSOR_ALGO_DEFAULT) );


  /* ***************************** */

  // Query workspace
  size_t worksize = 0;
  HANDLE_ERROR( cutensorContractionGetWorkspace(handle,
        &desc,
        &find,
        CUTENSOR_WORKSPACE_RECOMMENDED, &worksize ) );

  // Allocate workspace
  void *work = nullptr;
  if(worksize > 0)
  {
    if( cudaSuccess != cudaMalloc(&work, worksize) ) // This is optional!
    {
      work = nullptr;
      worksize = 0;
    }
  }


  /* ***************************** */

  // Create Contraction Plan
  cutensorContractionPlan_t plan;
  HANDLE_ERROR( cutensorInitContractionPlan(handle,
        &plan,
        &desc,
        &find,
        worksize) );

  /* ***************************** */

  cutensorStatus_t err;

  // Execute the tensor contraction
  err = cutensorContraction(handle,
      &plan,
      (void*)&alpha, a,
      b,
      (void*)&beta,  c,
      c,
      work, worksize, 0 /* stream */);
  /* cudaDeviceSynchronize(); */

  // Check for errors
  if(err != CUTENSOR_STATUS_SUCCESS)
  {
    printf("ERROR: %s\n", cutensorGetErrorString(err));
  }

  if ( work ) cudaFree( work );

  return 0;
}

// init cutensor before use
extern "C"
int cutensor_contract_r64(cutensorHandle_t* handle,
    double* c, int c_rank, long* c_dims, int* c_inds,
    double* a, int a_rank, long* a_dims, int* a_inds,
    double* b, int b_rank, long* b_dims, int* b_inds,
    double alpha,
    double beta){

  cudaDataType_t typeA = CUDA_R_64F;
  cudaDataType_t typeB = CUDA_R_64F;
  cudaDataType_t typeC = CUDA_R_64F;
  cutensorComputeType_t typeCompute = CUTENSOR_COMPUTE_64F;

  /* ***************************** */

  // Create Tensor Descriptors
  cutensorTensorDescriptor_t descA;
  HANDLE_ERROR( cutensorInitTensorDescriptor( handle,
        &descA,
        a_rank,
        a_dims,
        NULL,/*stride*/
        typeA, CUTENSOR_OP_IDENTITY ) );

  cutensorTensorDescriptor_t descB;
  HANDLE_ERROR( cutensorInitTensorDescriptor( handle,
        &descB,
        b_rank,
        b_dims,
        NULL,/*stride*/
        typeB, CUTENSOR_OP_IDENTITY ) );

  cutensorTensorDescriptor_t descC;
  HANDLE_ERROR( cutensorInitTensorDescriptor( handle,
        &descC,
        c_rank,
        c_dims,
        NULL,/*stride*/
        typeC, CUTENSOR_OP_IDENTITY ) );

  /* ***************************** */

  //Retrieve the memory alignment for each tensor
  uint32_t alignmentRequirementA;
  HANDLE_ERROR( cutensorGetAlignmentRequirement( handle,
        a,
        &descA,
        &alignmentRequirementA) );

  uint32_t alignmentRequirementB;
  HANDLE_ERROR( cutensorGetAlignmentRequirement( handle,
        b,
        &descB,
        &alignmentRequirementB) );

  uint32_t alignmentRequirementC;
  HANDLE_ERROR( cutensorGetAlignmentRequirement( handle,
        c,
        &descC,
        &alignmentRequirementC) );


  /* ***************************** */

  // Create the Contraction Descriptor
  cutensorContractionDescriptor_t desc;
  HANDLE_ERROR( cutensorInitContractionDescriptor( handle,
        &desc,
        &descA, a_inds, alignmentRequirementA,
        &descB, b_inds, alignmentRequirementB,
        &descC, c_inds, alignmentRequirementC,
        &descC, c_inds, alignmentRequirementC,
        typeCompute) );

  /* ***************************** */

  // Set the algorithm to use
  cutensorContractionFind_t find;
  HANDLE_ERROR( cutensorInitContractionFind(
        handle, &find,
        CUTENSOR_ALGO_DEFAULT) );


  /* ***************************** */

  // Query workspace
  size_t worksize = 0;
  HANDLE_ERROR( cutensorContractionGetWorkspace(handle,
        &desc,
        &find,
        CUTENSOR_WORKSPACE_RECOMMENDED, &worksize ) );

  // Allocate workspace
  void *work = nullptr;
  if(worksize > 0)
  {
    if( cudaSuccess != cudaMalloc(&work, worksize) ) // This is optional!
    {
      work = nullptr;
      worksize = 0;
    }
  }


  /* ***************************** */

  // Create Contraction Plan
  cutensorContractionPlan_t plan;
  HANDLE_ERROR( cutensorInitContractionPlan(handle,
        &plan,
        &desc,
        &find,
        worksize) );

  /* ***************************** */

  cutensorStatus_t err;

  // Execute the tensor contraction
  err = cutensorContraction(handle,
      &plan,
      (void*)&alpha, a,
      b,
      (void*)&beta,  c,
      c,
      work, worksize, 0 /* stream */);
  /* cudaDeviceSynchronize(); */

  // Check for errors
  if(err != CUTENSOR_STATUS_SUCCESS)
  {
    printf("ERROR: %s\n", cutensorGetErrorString(err));
  }

  if ( work ) cudaFree( work );

  return 0;
}

extern "C"
int cutensor_contract_c64(cutensorHandle_t* handle,
    std::complex<float>* c, int c_rank, long* c_dims, int* c_inds,
    std::complex<float>* a, int a_rank, long* a_dims, int* a_inds,
    std::complex<float>* b, int b_rank, long* b_dims, int* b_inds,
    std::complex<float> *alpha,
    std::complex<float> *beta){

  cudaDataType_t typeA = CUDA_C_32F;
  cudaDataType_t typeB = CUDA_C_32F;
  cudaDataType_t typeC = CUDA_C_32F;
  cutensorComputeType_t typeCompute = CUTENSOR_COMPUTE_32F;

  /* ***************************** */

  // Create Tensor Descriptors
  cutensorTensorDescriptor_t descA;
  HANDLE_ERROR( cutensorInitTensorDescriptor( handle,
        &descA,
        a_rank,
        a_dims,
        NULL,/*stride*/
        typeA, CUTENSOR_OP_IDENTITY ) );

  cutensorTensorDescriptor_t descB;
  HANDLE_ERROR( cutensorInitTensorDescriptor( handle,
        &descB,
        b_rank,
        b_dims,
        NULL,/*stride*/
        typeB, CUTENSOR_OP_IDENTITY ) );

  cutensorTensorDescriptor_t descC;
  HANDLE_ERROR( cutensorInitTensorDescriptor( handle,
        &descC,
        c_rank,
        c_dims,
        NULL,/*stride*/
        typeC, CUTENSOR_OP_IDENTITY ) );

  /* ***************************** */

  //Retrieve the memory alignment for each tensor
  uint32_t alignmentRequirementA;
  HANDLE_ERROR( cutensorGetAlignmentRequirement( handle,
        a,
        &descA,
        &alignmentRequirementA) );

  uint32_t alignmentRequirementB;
  HANDLE_ERROR( cutensorGetAlignmentRequirement( handle,
        b,
        &descB,
        &alignmentRequirementB) );

  uint32_t alignmentRequirementC;
  HANDLE_ERROR( cutensorGetAlignmentRequirement( handle,
        c,
        &descC,
        &alignmentRequirementC) );


  /* ***************************** */

  // Create the Contraction Descriptor
  cutensorContractionDescriptor_t desc;
  HANDLE_ERROR( cutensorInitContractionDescriptor( handle,
        &desc,
        &descA, a_inds, alignmentRequirementA,
        &descB, b_inds, alignmentRequirementB,
        &descC, c_inds, alignmentRequirementC,
        &descC, c_inds, alignmentRequirementC,
        typeCompute) );

  /* ***************************** */

  // Set the algorithm to use
  cutensorContractionFind_t find;
  HANDLE_ERROR( cutensorInitContractionFind(
        handle, &find,
        CUTENSOR_ALGO_DEFAULT) );


  /* ***************************** */

  // Query workspace
  size_t worksize = 0;
  HANDLE_ERROR( cutensorContractionGetWorkspace(handle,
        &desc,
        &find,
        CUTENSOR_WORKSPACE_RECOMMENDED, &worksize ) );

  // Allocate workspace
  void *work = nullptr;
  if(worksize > 0)
  {
    if( cudaSuccess != cudaMalloc(&work, worksize) ) // This is optional!
    {
      work = nullptr;
      worksize = 0;
    }
  }


  /* ***************************** */

  // Create Contraction Plan
  cutensorContractionPlan_t plan;
  HANDLE_ERROR( cutensorInitContractionPlan(handle,
        &plan,
        &desc,
        &find,
        worksize) );

  /* ***************************** */

  cutensorStatus_t err;

  // Execute the tensor contraction
  err = cutensorContraction(handle,
      &plan,
      alpha, a,
      b,
      beta,  c,
      c,
      work, worksize, 0 /* stream */);
  /* cudaDeviceSynchronize(); */

  // Check for errors
  if(err != CUTENSOR_STATUS_SUCCESS)
  {
    printf("ERROR: %s\n", cutensorGetErrorString(err));
  }

  if ( work ) cudaFree( work );

  return 0;
}

extern "C"
int cutensor_contract_c128(cutensorHandle_t* handle,
    std::complex<double>* c, int c_rank, long* c_dims, int* c_inds,
    std::complex<double>* a, int a_rank, long* a_dims, int* a_inds,
    std::complex<double>* b, int b_rank, long* b_dims, int* b_inds,
    std::complex<double> *alpha,
    std::complex<double> *beta){

  cudaDataType_t typeA = CUDA_C_64F;
  cudaDataType_t typeB = CUDA_C_64F;
  cudaDataType_t typeC = CUDA_C_64F;
  cutensorComputeType_t typeCompute = CUTENSOR_COMPUTE_64F;

  /* ***************************** */

  // Create Tensor Descriptors
  cutensorTensorDescriptor_t descA;
  HANDLE_ERROR( cutensorInitTensorDescriptor( handle,
        &descA,
        a_rank,
        a_dims,
        NULL,/*stride*/
        typeA, CUTENSOR_OP_IDENTITY ) );

  cutensorTensorDescriptor_t descB;
  HANDLE_ERROR( cutensorInitTensorDescriptor( handle,
        &descB,
        b_rank,
        b_dims,
        NULL,/*stride*/
        typeB, CUTENSOR_OP_IDENTITY ) );

  cutensorTensorDescriptor_t descC;
  HANDLE_ERROR( cutensorInitTensorDescriptor( handle,
        &descC,
        c_rank,
        c_dims,
        NULL,/*stride*/
        typeC, CUTENSOR_OP_IDENTITY ) );

  /* ***************************** */

  //Retrieve the memory alignment for each tensor
  uint32_t alignmentRequirementA;
  HANDLE_ERROR( cutensorGetAlignmentRequirement( handle,
        a,
        &descA,
        &alignmentRequirementA) );

  uint32_t alignmentRequirementB;
  HANDLE_ERROR( cutensorGetAlignmentRequirement( handle,
        b,
        &descB,
        &alignmentRequirementB) );

  uint32_t alignmentRequirementC;
  HANDLE_ERROR( cutensorGetAlignmentRequirement( handle,
        c,
        &descC,
        &alignmentRequirementC) );


  /* ***************************** */

  // Create the Contraction Descriptor
  cutensorContractionDescriptor_t desc;
  HANDLE_ERROR( cutensorInitContractionDescriptor( handle,
        &desc,
        &descA, a_inds, alignmentRequirementA,
        &descB, b_inds, alignmentRequirementB,
        &descC, c_inds, alignmentRequirementC,
        &descC, c_inds, alignmentRequirementC,
        typeCompute) );

  /* ***************************** */

  // Set the algorithm to use
  cutensorContractionFind_t find;
  HANDLE_ERROR( cutensorInitContractionFind(
        handle, &find,
        CUTENSOR_ALGO_DEFAULT) );


  /* ***************************** */

  // Query workspace
  size_t worksize = 0;
  HANDLE_ERROR( cutensorContractionGetWorkspace(handle,
        &desc,
        &find,
        CUTENSOR_WORKSPACE_RECOMMENDED, &worksize ) );

  // Allocate workspace
  void *work = nullptr;
  if(worksize > 0)
  {
    if( cudaSuccess != cudaMalloc(&work, worksize) ) // This is optional!
    {
      work = nullptr;
      worksize = 0;
    }
  }


  /* ***************************** */

  // Create Contraction Plan
  cutensorContractionPlan_t plan;
  HANDLE_ERROR( cutensorInitContractionPlan(handle,
        &plan,
        &desc,
        &find,
        worksize) );

  /* ***************************** */

  cutensorStatus_t err;

  // Execute the tensor contraction
  err = cutensorContraction(handle,
      &plan,
      alpha, a,
      b,
      beta,  c,
      c,
      work, worksize, 0 /* stream */);
  /* cudaDeviceSynchronize(); */

  // Check for errors
  if(err != CUTENSOR_STATUS_SUCCESS)
  {
    printf("ERROR: %s\n", cutensorGetErrorString(err));
  }

  if ( work ) cudaFree( work );

  return 0;
}

