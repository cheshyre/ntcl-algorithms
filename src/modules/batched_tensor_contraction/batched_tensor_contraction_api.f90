module batched_tensor_contraction_api
    use :: batched_tensor_contraction_module, only : batched_tensor_contraction
    use :: btc_factory_module, only : btc_factory

    implicit none
    public

    class(btc_factory), allocatable :: batched_tensor_contraction_factory
contains
    subroutine btc_initialize(factory)
        class(btc_factory), intent(in) :: factory

        batched_tensor_contraction_factory = factory
    end subroutine btc_initialize

    subroutine btc_finalize()

        if ( allocated(batched_tensor_contraction_factory) ) then
            deallocate(batched_tensor_contraction_factory)
        end if
    end subroutine btc_finalize
end module batched_tensor_contraction_api
