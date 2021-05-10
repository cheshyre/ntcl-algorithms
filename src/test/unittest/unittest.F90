! Auto-generated -- DO NOT MODIFY
program unittest
    use :: util_api, only : &
            assert, &
            selector

    use :: algorithms_initializer, only : &
            algorithms_initialize, &
            algorithms_finalize

    use :: matrix_multiplication_package_test_module, only : &
            matrix_multiplication_package_test
    use :: tensor_permute_package_test_module, only : &
            tensor_permute_package_test
    use :: tensor_contraction_package_test_module, only : &
            tensor_contraction_package_test
    use :: tensor_update_package_test_module, only : &
            tensor_update_package_test
    use :: batched_matrix_multiplication_package_test_module, only : &
            batched_matrix_multiplication_package_test
    use :: batched_tensor_permute_package_test_module, only : &
            batched_tensor_permute_package_test
    use :: batched_tensor_contraction_package_test_module, only : &
            batched_tensor_contraction_package_test

#ifdef use_blas
    use :: blas_package_test_module, only : &
            blas_package_test
#endif

#ifdef use_magma
    use :: magma_base_package_test_module, only : &
            magma_base_package_test
    use :: magma_mm_package_test_module, only : &
            magma_mm_package_test
    use :: magma_bmm_package_test_module, only : &
            magma_bmm_package_test
#endif

#ifdef use_cublas
    use :: cublas_package_test_module, only : &
            cublas_package_test
#endif

#ifdef use_cutensor
    use :: cutensor_package_test_module, only : &
            cutensor_package_test
#endif

#ifdef use_cuda
    use :: cuda_permute_package_test_module, only : &
            cuda_permute_package_test
    use :: cuda_mm_package_test_module, only : &
            cuda_mm_package_test
    use :: cuda_update_package_test_module, only : &
            cuda_update_package_test
#endif

    use :: api_package_test_module, only : &
            api_package_test

    implicit none

    type(assert) :: assertion
    type(selector) :: aselector

    type(matrix_multiplication_package_test) :: &
            amatrix_multiplication_package_test
    type(tensor_permute_package_test) :: &
            atensor_permute_package_test
    type(tensor_contraction_package_test) :: &
            atensor_contraction_package_test
    type(tensor_update_package_test) :: &
            atensor_update_package_test
    type(batched_matrix_multiplication_package_test) :: &
            abatched_matrix_multiplication_package_test
    type(batched_tensor_permute_package_test) :: &
            abatched_tensor_permute_package_test
    type(batched_tensor_contraction_package_test) :: &
            abatched_tensor_contraction_package_test

#ifdef use_blas
    type(blas_package_test) :: &
            ablas_package_test
#endif

#ifdef use_magma
    type(magma_base_package_test) :: &
            amagma_base_package_test
    type(magma_mm_package_test) :: &
            amagma_mm_package_test
    type(magma_bmm_package_test) :: &
            amagma_bmm_package_test
#endif

#ifdef use_cublas
    type(cublas_package_test) :: &
            acublas_package_test
#endif

#ifdef use_cutensor
    type(cutensor_package_test) :: &
            acutensor_package_test
#endif

#ifdef use_cuda
    type(cuda_permute_package_test) :: &
            acuda_permute_package_test
    type(cuda_mm_package_test) :: &
            acuda_mm_package_test
    type(cuda_update_package_test) :: &
            acuda_update_package_test
#endif

    type(api_package_test) :: &
            aapi_package_test

    assertion = assert()
    aselector = selector()

    call algorithms_initialize()

    amatrix_multiplication_package_test = matrix_multiplication_package_test(aselector)
    call amatrix_multiplication_package_test%run(assertion)
    call amatrix_multiplication_package_test%cleanup()

    atensor_permute_package_test = tensor_permute_package_test(aselector)
    call atensor_permute_package_test%run(assertion)
    call atensor_permute_package_test%cleanup()

    atensor_contraction_package_test = tensor_contraction_package_test(aselector)
    call atensor_contraction_package_test%run(assertion)
    call atensor_contraction_package_test%cleanup()

    atensor_update_package_test = tensor_update_package_test(aselector)
    call atensor_update_package_test%run(assertion)
    call atensor_update_package_test%cleanup()

    abatched_matrix_multiplication_package_test = batched_matrix_multiplication_package_test(aselector)
    call abatched_matrix_multiplication_package_test%run(assertion)
    call abatched_matrix_multiplication_package_test%cleanup()

    abatched_tensor_permute_package_test = batched_tensor_permute_package_test(aselector)
    call abatched_tensor_permute_package_test%run(assertion)
    call abatched_tensor_permute_package_test%cleanup()

    abatched_tensor_contraction_package_test = batched_tensor_contraction_package_test(aselector)
    call abatched_tensor_contraction_package_test%run(assertion)
    call abatched_tensor_contraction_package_test%cleanup()

#ifdef use_blas
    ablas_package_test = blas_package_test(aselector)
    call ablas_package_test%run(assertion)
    call ablas_package_test%cleanup()
#endif

#ifdef use_magma
    amagma_base_package_test = magma_base_package_test(aselector)
    call amagma_base_package_test%run(assertion)
    call amagma_base_package_test%cleanup()

    amagma_mm_package_test = magma_mm_package_test(aselector)
    call amagma_mm_package_test%run(assertion)
    call amagma_mm_package_test%cleanup()

    amagma_bmm_package_test = magma_bmm_package_test(aselector)
    call amagma_bmm_package_test%run(assertion)
    call amagma_bmm_package_test%cleanup()
#endif

#ifdef use_cublas
    acublas_package_test = cublas_package_test(aselector)
    call acublas_package_test%run(assertion)
    call acublas_package_test%cleanup()
#endif

#ifdef use_cutensor
    acutensor_package_test = cutensor_package_test(aselector)
    call acutensor_package_test%run(assertion)
    call acutensor_package_test%cleanup()
#endif

#ifdef use_cuda
    acuda_permute_package_test = cuda_permute_package_test(aselector)
    call acuda_permute_package_test%run(assertion)
    call acuda_permute_package_test%cleanup()

    acuda_mm_package_test = cuda_mm_package_test(aselector)
    call acuda_mm_package_test%run(assertion)
    call acuda_mm_package_test%cleanup()

    acuda_update_package_test = cuda_update_package_test(aselector)
    call acuda_update_package_test%run(assertion)
    call acuda_update_package_test%cleanup()
#endif

    aapi_package_test = api_package_test(aselector)
    call aapi_package_test%run(assertion)
    call aapi_package_test%cleanup()

    call algorithms_finalize()

    call assertion%write_summary()

    call aselector%cleanup()
    call assertion%cleanup()
end program unittest
