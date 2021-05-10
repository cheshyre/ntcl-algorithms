module algorithms_api
    use :: matrix_multiplication_api, only : &
            matrix_multiplication, &
            matrix_multiplication_factory, &
            mm_initialize, &
            mm_finalize

    use :: tensor_permute_api, only : &
            tensor_permute, &
            tensor_permute_factory, &
            tp_initialize, &
            tp_finalize

    use :: tensor_contraction_api, only : &
            tensor_contraction, &
            tensor_contraction_factory, &
            tc_initialize, &
            tc_finalize
    use :: easy_tensor_contraction_interface, only : contract

    use :: tensor_update_api, only : &
            scalar_inline_update, &
            tensor_update_factory, &
            tu_initialize, &
            tu_finalize

    use :: batched_matrix_multiplication_api, only : &
            batched_matrix_multiplication, &
            batched_matrix_multiplication_factory, &
            bmm_initialize, &
            bmm_finalize

    use :: batched_tensor_permute_api, only : &
            batched_tensor_permute, &
            batched_tensor_permute_factory, &
            btp_initialize, &
            btp_finalize

    use :: batched_tensor_contraction_api, only : &
            batched_tensor_contraction, &
            batched_tensor_contraction_factory, &
            btc_initialize, &
            btc_finalize

    implicit none
    public
end module algorithms_api
