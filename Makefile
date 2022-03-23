# Auto-generated -- do not modify!

SOURCE_DIR := $(shell dirname ${MAKEFILE_LIST})
MAKEINC := ${NTCL_ROOT}/ntcl-build/makefile_fragments

include ${MAKEINC}/standard_preample.mk

modules      += matrix_multiplication tensor_permute tensor_contraction tensor_update batched_matrix_multiplication batched_tensor_permute batched_tensor_contraction
test_modules += matrix_multiplication tensor_permute tensor_contraction tensor_update batched_matrix_multiplication batched_tensor_permute batched_tensor_contraction

ifdef use_blas
modules      += blas
test_modules += blas
FFLAGS += -Duse_blas
endif

ifdef use_magma
modules      += magma_base magma_mm magma_bmm
test_modules += magma_base magma_mm magma_bmm
FFLAGS += -Duse_magma
endif

ifdef use_cublas
modules      += cublas
test_modules += cublas
FFLAGS += -Duse_cublas
endif

ifdef use_cutensor
modules      += cutensor
test_modules += cutensor
FFLAGS += -Duse_cutensor
endif

ifdef use_cuda
modules      += cuda_permute cuda_mm cuda_update
test_modules += cuda_permute cuda_mm cuda_update
FFLAGS += -Duse_cuda
endif

ifdef use_rocblas
modules      += rocblas
test_modules += rocblas
FFLAGS += -Duse_rocblas
endif

ifdef use_hip
modules      += hip_permute
test_modules += hip_permute
FFLAGS += -Duse_hip
endif

modules      += api
test_modules += api

test_modules += unittest

library_name := libntcl-algorithms.a
library_name_full := libntcl_algorithms_full.a

external_include := 
external_libraries := ${NTCL_ROOT}/ntcl-tensor/lib/libntcl-tensor.a ${NTCL_ROOT}/ntcl-data/lib/libntcl-data.a ${NTCL_ROOT}/ntcl-util/lib/libntcl-util.a
internal_include_dirs := ${NTCL_ROOT}/ntcl-tensor/include ${NTCL_ROOT}/ntcl-data/include ${NTCL_ROOT}/ntcl-util/include

ifdef use_blas
external_include += ${INCBLAS}
external_libraries += ${LIBBLAS}
endif

ifdef use_magma
external_include += -I${MAGMA_ROOT}/include ${INCBLAS}
external_libraries += -L${MAGMA_ROOT}/lib -lmagma -L${CUDA_ROOT}/lib64 -lcublas -L${CUDA_ROOT}/lib64 -lcudart -lcuda -lstdc++ ${LIBBLAS}
endif

ifdef use_cublas
external_libraries += -L${CUDA_ROOT}/lib64 -lcublas -L${CUDA_ROOT}/lib64 -lcudart -lcuda -lstdc++
endif

ifdef use_cutensor
external_include += -I${CUTENSOR_ROOT}/include
external_libraries += -L${CUTENSOR_ROOT}/lib -lcutensor -L${CUDA_ROOT}/lib64 -lcudart -lcuda -lstdc++
endif

ifdef use_cuda
external_libraries += -L${CUDA_ROOT}/lib64 -lcudart -lcuda -lstdc++
endif

ifdef use_rocblas
ifeq (${HIP_PLATFORM},amd)
external_include += -I${ROCM_PATH}/rocblas/include
external_libraries += -L${ROCM_PATH}/rocblas/lib -lrocblas -ldl -L${HIP_PATH}/lib -lamdhip64 -lstdc++
endif

ifeq (${HIP_PLATFORM},nvidia)
external_include += -I${ROCM_PATH}/rocblas/include
external_libraries += -L${ROCM_PATH}/rocblas/lib -lrocblas -ldl -L${CUDA_ROOT}/lib64 -lcudart -lcuda -lstdc++
endif
endif

ifdef use_hip
ifeq (${HIP_PLATFORM},amd)
external_libraries += -L${HIP_PATH}/lib -lamdhip64 -lstdc++
endif

ifeq (${HIP_PLATFORM},nvidia)
external_libraries += -L${CUDA_ROOT}/lib64 -lcudart -lcuda -lstdc++
endif
endif

include ${MAKEINC}/standard_defs.mk
