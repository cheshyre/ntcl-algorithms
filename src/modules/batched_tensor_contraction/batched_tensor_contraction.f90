module batched_tensor_contraction_module
    use :: data_api, only : stream

    use :: tensor_api, only : &
            tensor_array_element, &
            scalar

    implicit none
    private

    public :: batched_tensor_contraction

    type, abstract :: batched_tensor_contraction
    contains
        procedure(contract_interface), deferred :: contract
        procedure(synchronize_interface), deferred :: synchronize
        procedure(empty), deferred :: cleanup
    end type batched_tensor_contraction

    abstract interface
        subroutine contract_interface(this, c, a, b, alpha, beta, astream)
            import :: batched_tensor_contraction
            import :: tensor_array_element
            import :: scalar
            import :: stream

            class(batched_tensor_contraction), intent(inout) :: this
            type(tensor_array_element), dimension(:), intent(inout) :: c
            type(tensor_array_element), dimension(:), intent(in) :: a, b
            type(scalar), dimension(:), intent(in), optional :: alpha, beta
            type(stream), intent(in), optional :: astream
        end subroutine contract_interface

        subroutine synchronize_interface(this, astream)
            import :: batched_tensor_contraction
            import :: stream

            class(batched_tensor_contraction), intent(in) :: this
            type(stream), intent(in), optional :: astream
        end subroutine synchronize_interface

        subroutine empty(this)
            import :: batched_tensor_contraction

            class(batched_tensor_contraction), intent(inout) :: this
        end subroutine empty
    end interface
end module batched_tensor_contraction_module
