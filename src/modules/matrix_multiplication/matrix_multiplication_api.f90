module matrix_multiplication_api
    use :: matrix_multiplication_module, only : matrix_multiplication
    use :: mm_factory_module, only : mm_factory

    implicit none
    public

    class(mm_factory), allocatable :: matrix_multiplication_factory
contains
    subroutine mm_initialize(factory)
        class(mm_factory), intent(in) :: factory

        matrix_multiplication_factory = factory
    end subroutine mm_initialize

    subroutine mm_finalize()

        if ( allocated(matrix_multiplication_factory) ) then
            deallocate(matrix_multiplication_factory)
        end if
    end subroutine mm_finalize
end module matrix_multiplication_api
