module matrix_multiplication_module
    use :: tensor_api, only : &
            scalar, &
            matrix
    use :: data_api, only : stream

    implicit none
    private

    public :: matrix_multiplication

    type, abstract :: matrix_multiplication
    contains
        procedure(mm_interface), deferred :: mm
        procedure(sync_interface), deferred :: synchronize
        procedure(empty), deferred :: cleanup
    end type matrix_multiplication

    abstract interface
        subroutine mm_interface(this, dst, left, right, alpha, beta, astream)
            import :: matrix_multiplication
            import :: matrix
            import :: scalar
            import :: stream

            class(matrix_multiplication), intent(in) :: this
            type(matrix), intent(inout) :: dst
            type(matrix), intent(in) :: left, right
            type(scalar), intent(in), optional :: alpha, beta
            type(stream), intent(in), optional :: astream
        end subroutine mm_interface

        subroutine sync_interface(this, astream)
            import :: matrix_multiplication
            import :: stream

            class(matrix_multiplication), intent(in) :: this
            type(stream), intent(in), optional :: astream
        end subroutine sync_interface

        subroutine empty(this)
            import :: matrix_multiplication

            class(matrix_multiplication), intent(inout) :: this
        end subroutine empty
    end interface
end module matrix_multiplication_module
