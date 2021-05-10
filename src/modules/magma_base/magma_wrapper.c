#include <stdlib.h>
#include <stdio.h>
#include <magma_v2.h>

void TESTING_CHECK( magma_int_t err ){
  if( err != 0 ){
    printf("error %lld: %s\n", (long long) err, magma_strerror(err) );
  }
}

int magma_init_wrapper(){
  TESTING_CHECK( magma_init() );
  return 0;
}

int magma_finalize_wrapper(){
  TESTING_CHECK( magma_finalize() );
  return 0;
}

int magma_getdevice_wrapper(int* dev){
  magma_getdevice(dev);
  return 0;
}

int magma_queue_sync_wrapper( magma_queue_t queue ){
  magma_queue_sync(queue);
  return 0;
}

int magma_version_wrapper(int* major, int* minor, int* micro){
  magma_version(major,minor,micro);
  return 0;
}

int magma_queue_create_wrapper(int dev, magma_queue_t* queue){
  magma_queue_create(dev, queue);
  return 0;
}

int magma_queue_create_from_cuda_wrapper(int dev,
    cudaStream_t *stream,
    cublasHandle_t cublas_handle,
    cusparseHandle_t cusparse_handle,
    magma_queue_t* queue){

  if ( stream == 0 ) {
      magma_queue_create_from_cuda(dev, 0, cublas_handle, cusparse_handle, queue);
  } else {
      magma_queue_create_from_cuda(dev, *stream, cublas_handle, cusparse_handle, queue);
  }
  return 0;
}

cudaStream_t magma_queue_get_cuda_stream_wrapper(magma_queue_t *queue){

  cudaStream_t result;
  result = magma_queue_get_cuda_stream(*queue);
  return result;
}

int magma_queue_destroy_wrapper(magma_queue_t queue){
  magma_queue_destroy(queue);
  return 0;
}
