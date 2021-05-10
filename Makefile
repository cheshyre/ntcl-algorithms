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

modules      += api
test_modules += api

test_modules += unittest

library_name := libntcl-algorithms.a

external_include := 
external_libraries := ${NTCL_ROOT}/ntcl-tensor/lib/libntcl-tensor.a ${NTCL_ROOT}/ntcl-data/lib/libntcl-data.a ${NTCL_ROOT}/ntcl-util/lib/libntcl-util.a
internal_include_dirs := ${NTCL_ROOT}/ntcl-tensor/include ${NTCL_ROOT}/ntcl-data/include ${NTCL_ROOT}/ntcl-util/include

ifdef use_blas
external_include += ${INCBLAS}
external_libraries += ${LIBBLAS}
endif

ifdef use_magma
external_include += -I${MAGMA_ROOT}/include ${INCBLAS}
external_libraries += -L${MAGMA_ROOT}/lib -lmagma -L${CUDA_ROOT}/lib64 -lcublas ${CUDA_LIBS} -lstdc++ ${LIBBLAS}
endif

ifdef use_cublas
external_libraries += -L${CUDA_ROOT}/lib64 -lcublas ${CUDA_LIBS} -lstdc++
endif

ifdef use_cutensor
external_include += -I${CUTENSOR_ROOT}/include
external_libraries += -L${CUTENSOR_ROOT}/lib -lcutensor ${CUDA_LIBS} -lstdc++
endif

ifdef use_cuda
external_libraries += ${CUDA_LIBS} -lstdc++
endif

include ${MAKEINC}/standard_defs.mk
