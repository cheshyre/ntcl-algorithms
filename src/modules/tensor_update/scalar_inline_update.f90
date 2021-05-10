module scalar_inline_update_module
    use :: data_api, only : stream

    use :: tensor_api, only : &
            scalar, &
            tensor

    implicit none
    private

    public :: scalar_inline_update

    type, abstract :: scalar_inline_update
    contains
        procedure(update_interface), deferred :: update
        procedure(sync_interface), deferred :: synchronize
        procedure(empty), deferred :: cleanup
    end type scalar_inline_update

    abstract interface
        subroutine update_interface(this, dst, alpha, astream)
            import :: scalar_inline_update
            import :: tensor
            import :: scalar
            import :: stream

            class(scalar_inline_update), intent(inout) :: this
            class(tensor), intent(inout) :: dst
            type(scalar), intent(in) :: alpha
            type(stream), intent(in), optional :: astream
        end subroutine update_interface

        subroutine sync_interface(this, astream)
            import :: scalar_inline_update
            import :: stream

            class(scalar_inline_update), intent(inout) :: this
            type(stream), intent(in), optional :: astream
        end subroutine sync_interface

        subroutine empty(this)
            import :: scalar_inline_update

            class(scalar_inline_update), intent(inout) :: this
        end subroutine empty
    end interface
end module scalar_inline_update_module
