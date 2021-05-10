module tensor_permute_api
    use :: tensor_permute_module, only : tensor_permute
    use :: permute_factory_module, only : permute_factory

    implicit none

    class(permute_factory), allocatable :: tensor_permute_factory
contains
    subroutine tp_initialize(factory)
        class(permute_factory), intent(in) :: factory

        tensor_permute_factory = factory
    end subroutine tp_initialize

    subroutine tp_finalize()

        if (allocated(tensor_permute_factory)) deallocate(tensor_permute_factory)
    end subroutine tp_finalize
end module tensor_permute_api
