module batched_matrix_multiplication_module
    use :: data_api, only : stream

    use :: tensor_api, only : &
            scalar, &
            matrix

    implicit none
    private

    public :: batched_matrix_multiplication

    type, abstract :: batched_matrix_multiplication
    contains
        generic :: bmm => &
                bmm_array, &
                bmm_single

        procedure(bmm_interface), deferred :: bmm_array
        procedure(bmm_single_interface), deferred :: bmm_single
        procedure(sync_interface), deferred :: synchronize
        procedure(empty), deferred :: cleanup
    end type batched_matrix_multiplication

    abstract interface
        subroutine bmm_interface(this, dst, left, right, alpha, beta, astream)
            import :: batched_matrix_multiplication
            import :: matrix
            import :: scalar
            import :: stream

            class(batched_matrix_multiplication), intent(inout) :: this
            type(matrix), dimension(:), intent(inout) :: dst
            type(matrix), dimension(:), intent(in) :: left, right
            type(scalar), intent(in), dimension(:), optional :: alpha, beta
            type(stream), intent(in), optional :: astream
        end subroutine bmm_interface

        subroutine bmm_single_interface(this, dst, left, right, alpha, beta, astream)
            import :: batched_matrix_multiplication
            import :: matrix
            import :: scalar
            import :: stream

            class(batched_matrix_multiplication), intent(inout) :: this
            type(matrix), dimension(:), intent(inout) :: dst
            type(matrix), dimension(:), intent(in) :: left, right
            type(scalar), intent(in) :: alpha, beta
            type(stream), intent(in), optional :: astream
        end subroutine bmm_single_interface

        subroutine sync_interface(this, astream)
            import :: batched_matrix_multiplication
            import :: stream

            class(batched_matrix_multiplication), intent(in) :: this
            type(stream), intent(in), optional :: astream
        end subroutine sync_interface

        subroutine empty(this)
            import :: batched_matrix_multiplication

            class(batched_matrix_multiplication), intent(inout) :: this
        end subroutine empty
    end interface
end module batched_matrix_multiplication_module
