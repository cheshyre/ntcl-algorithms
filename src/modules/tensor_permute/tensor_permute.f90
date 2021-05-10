module tensor_permute_module
    use :: data_api, only : stream

    use :: tensor_api, only : &
            tensor, &
            matrix

    implicit none
    private

    public :: tensor_permute

    type, abstract :: tensor_permute
    contains
        procedure :: permute_tensor_with_checks => permute_tensor_with_checks
        procedure :: permute_matrix_without_perm => permute_matrix_without_perm
        procedure(permute_tensor_interface), deferred :: permute_tensor

        generic :: permute => &
                permute_tensor_with_checks, &
                permute_matrix_without_perm
        procedure(sync_interface), deferred :: synchronize
        procedure(empty), deferred :: cleanup
    end type tensor_permute

    abstract interface
        subroutine permute_tensor_interface(this, src, dst, perm, astream)
            import :: tensor_permute
            import :: tensor
            import :: stream

            class(tensor_permute), intent(inout) :: this
            class(tensor), intent(in) :: src
            class(tensor), intent(inout) :: dst
            integer, dimension(:), intent(in) :: perm
            type(stream), intent(in), optional :: astream
        end subroutine permute_tensor_interface

        subroutine sync_interface(this, astream)
            import :: tensor_permute
            import :: stream

            class(tensor_permute), intent(in) :: this
            type(stream), intent(in), optional :: astream
        end subroutine sync_interface

        subroutine empty(this)
            import :: tensor_permute

            class(tensor_permute), intent(inout) :: this
        end subroutine empty
    end interface
contains
    subroutine permute_tensor_with_checks(this, src, dst, perm, astream)
        class(tensor_permute), intent(inout) :: this
        class(tensor), intent(in) :: src
        class(tensor), intent(inout) :: dst
        integer, dimension(:), intent(in):: perm
        type(stream), intent(in), optional :: astream

        if ( .not. same_type_as(src, dst) ) &
                error stop "tensor_permute::permute_tensor_with_checks:Tensors are not of the same order."

        if( size(perm) < 2 ) then
            error stop "tensor_permute::permute_tensor_with_checks:Permute array must be >= 2 in size."
        end if

        call this%permute_tensor(src, dst, perm, astream)
    end subroutine permute_tensor_with_checks

    subroutine permute_matrix_without_perm(this, src, dst, astream)
        class(tensor_permute), intent(inout) :: this
        type(matrix), intent(in) :: src
        type(matrix), intent(inout) :: dst
        type(stream), intent(in), optional :: astream

        call this%permute(src, dst, [2,1], astream)
    end subroutine permute_matrix_without_perm
end module tensor_permute_module
