! Auto-generated. DO NOT MODIFY!
module raw_tensor_permute_module
    use :: tensor_api, only : &
            tensor, &
            matrix, &
            tensor_rank3, &
            tensor_rank4, &
            tensor_rank5

    use :: tensor_permute_module, only : tensor_permute

    implicit none
    private

    public :: raw_tensor_permute

    type, abstract, extends(tensor_permute) :: raw_tensor_permute
    contains
        procedure :: is_compatible => is_compatible
    end type raw_tensor_permute
contains
    logical function is_compatible(this, src, dst, perm)
        class(raw_tensor_permute), intent(in) :: this
        class(tensor), intent(in) :: src, dst
        integer, dimension(:), intent(in) :: perm

        is_compatible = same_type_as(src, dst)

        select type (src)
        type is ( matrix )
            is_compatible = is_compatible .and. size(perm) == 2
        type is ( tensor_rank3 )
            is_compatible = is_compatible .and. size(perm) == 3
        type is ( tensor_rank4 )
            is_compatible = is_compatible .and. size(perm) == 4
        type is ( tensor_rank5 )
            is_compatible = is_compatible .and. size(perm) == 5
        class default
            is_compatible = .false.
        end select

        if ( is_compatible ) is_compatible = all(dst%dims == src%dims(perm))
    end function is_compatible
end module raw_tensor_permute_module
