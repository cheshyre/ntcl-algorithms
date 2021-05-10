module tensor_contraction_api
    use :: tensor_contraction_module, only : tensor_contraction
    use :: contraction_factory_module, only : contraction_factory

    implicit none
    private

    public :: tensor_contraction
    public :: tensor_contraction_factory
    public :: tc_initialize
    public :: tc_finalize

    class(contraction_factory), allocatable :: tensor_contraction_factory
contains
    subroutine tc_initialize(factory)
        class(contraction_factory), intent(in) :: factory

        tensor_contraction_factory = factory
    end subroutine tc_initialize

    subroutine tc_finalize()

        if (allocated(tensor_contraction_factory)) deallocate(tensor_contraction_factory)
    end subroutine tc_finalize
end module tensor_contraction_api
