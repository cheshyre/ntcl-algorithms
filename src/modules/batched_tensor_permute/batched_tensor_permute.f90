module batched_tensor_permute_module
    use :: data_api, only : stream

    use :: tensor_api, only : tensor_array_element

    implicit none
    private

    public :: batched_tensor_permute

    type, abstract :: batched_tensor_permute
    contains
        procedure(btp_interface), deferred :: btp
        procedure(sync_interface), deferred :: synchronize
        procedure(empty), deferred :: cleanup
    end type batched_tensor_permute

    abstract interface
        subroutine btp_interface(this, src, dst, perm, astream)
            import :: batched_tensor_permute
            import :: tensor_array_element
            import :: stream

            class(batched_tensor_permute), target, intent(inout) :: this
            class(tensor_array_element), dimension(:), intent(in) :: src
            class(tensor_array_element), dimension(:), intent(inout) :: dst
            integer, intent(in) :: perm(:)
            type(stream), intent(in), optional :: astream
        end subroutine btp_interface

        subroutine sync_interface(this, astream)
            import :: batched_tensor_permute
            import :: stream

            class(batched_tensor_permute), intent(in) :: this
            type(stream), intent(in), optional :: astream
        end subroutine sync_interface

        subroutine empty(this)
            import :: batched_tensor_permute

            class(batched_tensor_permute), intent(inout) :: this
        end subroutine empty
    end interface
end module batched_tensor_permute_module
