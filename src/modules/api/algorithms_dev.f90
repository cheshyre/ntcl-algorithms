module algorithms_dev
    use :: algorithms_api

    use :: matrix_multiplication_dev, only : &
            raw_mm_driver, &
            fortran_data_mm_driver, &
            pointer_data_mm_driver, &
            intrinsic_mm_driver, &
            mm_factory
    use :: default_mm_factory_module, only : default_mm_factory

    use :: tensor_permute_dev, only : &
            raw_tensor_permute, &
            tensor_permute_loops, &
            permute_factory
    use :: default_permute_factory_module, only : default_permute_factory

    use :: tensor_contraction_dev, only : &
            tc_descriptor, &
            tc_index_helper, &
            tc_parser, &
            ttgt_descriptor, &
            ttgt_parser, &
            tensor_contraction_loops, &
            contraction_factory
    use :: default_contraction_factory_module, only : default_contraction_factory

    use :: tensor_update_dev, only : &
            fortran_siu_driver, &
            update_factory
    use :: default_update_factory_module, only : default_update_factory

    use :: batched_matrix_multiplication_dev, only : &
            simple_loops_bmm_driver, &
            pointer_data_bmm_driver, &
            bmm_factory
    use :: default_bmm_factory_module, only : default_bmm_factory

    use :: batched_tensor_permute_dev, only : &
            simple_loops_btp_driver, &
            btp_factory
    use :: default_btp_factory_module, only : default_btp_factory

    use :: batched_tensor_contraction_dev, only : &
            simple_loops_btc_driver, &
            btc_factory
    use :: default_btc_factory_module, only : default_btc_factory

    implicit none
    public
end module algorithms_dev
