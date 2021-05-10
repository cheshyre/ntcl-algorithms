module batched_tensor_permute_api
    use :: batched_tensor_permute_module, only : batched_tensor_permute
    use :: btp_factory_module, only : btp_factory

    implicit none
    public

    class(btp_factory), allocatable :: batched_tensor_permute_factory
contains
    subroutine btp_initialize(factory)
        class(btp_factory), intent(in) :: factory

        batched_tensor_permute_factory = factory
    end subroutine btp_initialize

    subroutine btp_finalize()

        if ( allocated(batched_tensor_permute_factory) ) then
            deallocate(batched_tensor_permute_factory)
        end if
    end subroutine btp_finalize
end module batched_tensor_permute_api
