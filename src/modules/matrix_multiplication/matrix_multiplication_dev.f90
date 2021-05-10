module matrix_multiplication_dev
    use :: matrix_multiplication_api

    use :: intrinsic_mm_driver_module, only : intrinsic_mm_driver
    use :: raw_mm_driver_module, only : raw_mm_driver
    use :: fortran_data_mm_driver_module, only : fortran_data_mm_driver
    use :: pointer_data_mm_driver_module, only : pointer_data_mm_driver

    implicit none
    public
end module matrix_multiplication_dev
