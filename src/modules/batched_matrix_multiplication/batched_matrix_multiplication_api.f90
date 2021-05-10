module batched_matrix_multiplication_api
    use :: batched_matrix_multiplication_module, only : batched_matrix_multiplication
    use :: bmm_factory_module, only : bmm_factory

    implicit none
    public

    class(bmm_factory), allocatable :: batched_matrix_multiplication_factory
contains
    subroutine bmm_initialize(factory)
        class(bmm_factory), intent(in) :: factory

        batched_matrix_multiplication_factory = factory
    end subroutine bmm_initialize

    subroutine bmm_finalize()

        if ( allocated(batched_matrix_multiplication_factory) ) then
            deallocate(batched_matrix_multiplication_factory)
        end if
    end subroutine bmm_finalize
end module batched_matrix_multiplication_api
