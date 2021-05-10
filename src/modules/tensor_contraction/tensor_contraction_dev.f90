module tensor_contraction_dev
    use :: tensor_contraction_api
    use :: contraction_factory_module, only : contraction_factory
    use :: tensor_contraction_loops_module, only : tensor_contraction_loops
    use :: tc_descriptor_module, only : tc_descriptor
    use :: tc_index_helper_module, only : tc_index_helper
    use :: tc_parser_module, only : tc_parser
    use :: ttgt_descriptor_module, only : ttgt_descriptor
    use :: ttgt_parser_module, only : ttgt_parser
    use :: tc_ttgt_module, only : tc_ttgt
    use :: unbuffered_ttgt_tc_driver_module, only : unbuffered_ttgt_tc_driver
    use :: buffered_ttgt_tc_driver_module, only : buffered_ttgt_tc_driver

    implicit none
end module tensor_contraction_dev
