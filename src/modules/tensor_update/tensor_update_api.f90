module tensor_update_api
    use :: scalar_inline_update_module, only : scalar_inline_update
    use :: update_factory_module, only : update_factory

    implicit none
    public

    class(update_factory), allocatable :: tensor_update_factory
contains
    subroutine tu_initialize(factory)
        class(update_factory), intent(in) :: factory

        tensor_update_factory = factory
    end subroutine tu_initialize

    subroutine tu_finalize()

        if ( allocated(tensor_update_factory)) deallocate(tensor_update_factory)
    end subroutine tu_finalize
end module tensor_update_api
