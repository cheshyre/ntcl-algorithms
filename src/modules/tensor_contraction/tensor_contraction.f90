module tensor_contraction_module
    use :: data_api, only : stream
    use :: tensor_api, only : &
            tensor, &
            scalar

    implicit none
    private

    public :: tensor_contraction

    type, abstract :: tensor_contraction
    contains
        procedure(contract_interface), deferred :: contract
        procedure(synchronize_interface), deferred :: synchronize
        procedure(empty), deferred :: cleanup
    end type tensor_contraction

    abstract interface
        subroutine contract_interface(this, c, a, b, alpha, beta, astream)
            import :: tensor_contraction
            import :: tensor
            import :: scalar
            import :: stream

            class(tensor_contraction), intent(inout) :: this
            class(tensor), intent(inout) :: c
            class(tensor), intent(in) :: a, b
            type(scalar), intent(in), optional :: alpha, beta
            type(stream), intent(in), optional :: astream
        end subroutine contract_interface

        subroutine synchronize_interface(this, astream)
            import :: tensor_contraction
            import :: stream

            class(tensor_contraction), intent(in) :: this
            type(stream), intent(in), optional :: astream
        end subroutine synchronize_interface

        subroutine empty(this)
            import :: tensor_contraction

            class(tensor_contraction), intent(inout) :: this
        end subroutine empty
    end interface
end module tensor_contraction_module
