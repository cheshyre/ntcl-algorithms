! Auto-generated. DO NOT MODIFY!
module tensor_permute_loops_module
    use, intrinsic :: iso_fortran_env, only : &
            int64, &
            real64, &
            real32
    use :: data_api, only : &
            stream
    use :: tensor_api, only : &
            tensor, &
            tensor_fortran_converter, &
            matrix, &
            tensor_rank3, &
            tensor_rank4, &
            tensor_rank5, &
            dt_c128, &
            dt_c64, &
            dt_r64, &
            dt_r32
    use :: raw_tensor_permute_module, only : &
            raw_tensor_permute

    implicit none
    private

    public :: tensor_permute_loops

    type, extends(raw_tensor_permute) :: tensor_permute_loops
        type(tensor_fortran_converter) :: converter
    contains
        procedure :: permute_tensor => permute_tensor
        procedure :: permute_complex128_matrix => permute_complex128_matrix
        procedure :: permute_complex128_tensor_rank3 => permute_complex128_tensor_rank3
        procedure :: permute_complex128_tensor_rank4 => permute_complex128_tensor_rank4
        procedure :: permute_complex128_tensor_rank5 => permute_complex128_tensor_rank5
        procedure :: permute_complex64_matrix => permute_complex64_matrix
        procedure :: permute_complex64_tensor_rank3 => permute_complex64_tensor_rank3
        procedure :: permute_complex64_tensor_rank4 => permute_complex64_tensor_rank4
        procedure :: permute_complex64_tensor_rank5 => permute_complex64_tensor_rank5
        procedure :: permute_real64_matrix => permute_real64_matrix
        procedure :: permute_real64_tensor_rank3 => permute_real64_tensor_rank3
        procedure :: permute_real64_tensor_rank4 => permute_real64_tensor_rank4
        procedure :: permute_real64_tensor_rank5 => permute_real64_tensor_rank5
        procedure :: permute_real32_matrix => permute_real32_matrix
        procedure :: permute_real32_tensor_rank3 => permute_real32_tensor_rank3
        procedure :: permute_real32_tensor_rank4 => permute_real32_tensor_rank4
        procedure :: permute_real32_tensor_rank5 => permute_real32_tensor_rank5
        procedure :: cleanup => cleanup
        procedure :: synchronize => synchronize
    end type tensor_permute_loops

    interface tensor_permute_loops
        module procedure constructor
    end interface tensor_permute_loops
contains
    function constructor() result(this)
        type(tensor_permute_loops) :: this

        this%converter = tensor_fortran_converter()
    end function constructor

    subroutine permute_tensor(this, src, dst, perm, astream)
        class(tensor_permute_loops), intent(inout) :: this
        class(tensor), intent(in) :: src
        class(tensor), intent(inout) :: dst
        integer, dimension(:), intent(in) :: perm
        type(stream), intent(in), optional :: astream

        integer :: rank
         
        if ( .not. this%is_compatible(src, dst, perm) ) &
                error stop "tensor_permute_loops:Tensors are not compatible."
         
        select type(src)
        type is (matrix)
            select type(dst)
            type is (matrix)
                rank = 2
                select case ( dst%get_datatype() )
                case(dt_c128)
                    call this%permute_complex128_matrix(src, dst, perm(:2), astream)
                case(dt_c64)
                    call this%permute_complex64_matrix(src, dst, perm(:2), astream)
                case(dt_r64)
                    call this%permute_real64_matrix(src, dst, perm(:2), astream)
                case(dt_r32)
                    call this%permute_real32_matrix(src, dst, perm(:2), astream)
                end select
            end select
        type is (tensor_rank3)
            select type(dst)
            type is (tensor_rank3)
                rank = 3
                select case ( dst%get_datatype() )
                case(dt_c128)
                    call this%permute_complex128_tensor_rank3(src, dst, perm(:3), astream)
                case(dt_c64)
                    call this%permute_complex64_tensor_rank3(src, dst, perm(:3), astream)
                case(dt_r64)
                    call this%permute_real64_tensor_rank3(src, dst, perm(:3), astream)
                case(dt_r32)
                    call this%permute_real32_tensor_rank3(src, dst, perm(:3), astream)
                end select
            end select
        type is (tensor_rank4)
            select type(dst)
            type is (tensor_rank4)
                rank = 4
                select case ( dst%get_datatype() )
                case(dt_c128)
                    call this%permute_complex128_tensor_rank4(src, dst, perm(:4), astream)
                case(dt_c64)
                    call this%permute_complex64_tensor_rank4(src, dst, perm(:4), astream)
                case(dt_r64)
                    call this%permute_real64_tensor_rank4(src, dst, perm(:4), astream)
                case(dt_r32)
                    call this%permute_real32_tensor_rank4(src, dst, perm(:4), astream)
                end select
            end select
        type is (tensor_rank5)
            select type(dst)
            type is (tensor_rank5)
                rank = 5
                select case ( dst%get_datatype() )
                case(dt_c128)
                    call this%permute_complex128_tensor_rank5(src, dst, perm(:5), astream)
                case(dt_c64)
                    call this%permute_complex64_tensor_rank5(src, dst, perm(:5), astream)
                case(dt_r64)
                    call this%permute_real64_tensor_rank5(src, dst, perm(:5), astream)
                case(dt_r32)
                    call this%permute_real32_tensor_rank5(src, dst, perm(:5), astream)
                end select
            end select
        end select
    end subroutine permute_tensor

    subroutine permute_complex128_matrix(this, src, dst, perm, astream)
        class(tensor_permute_loops), intent(inout) :: this
        type(matrix), intent(in) :: src
        type(matrix), intent(inout) :: dst
        integer, dimension(2), intent(in) :: perm
        type(stream), intent(in), optional :: astream

        integer(int64) :: d1, d2
        complex(real64), dimension(:,:), pointer, contiguous :: tsrc, tdst
        integer(int64) :: sidx(2), didx(2)

        associate ( sdims => src%dims, ddims => dst%dims )
            call this%converter%secure_fortran_pointer(tsrc, src, astream)
            call this%converter%secure_fortran_pointer(tdst, dst, astream)

            do d2 = 1, sdims(2)
                sidx(2) = d2
                do d1 = 1, sdims(1)
                    sidx(1) = d1

                    didx = sidx(perm)
                    tdst(didx(1),didx(2)) = tsrc(sidx(1),sidx(2))
                end do
            end do

            call this%converter%update_remote_and_release_pointer(tdst, dst, astream)
            call this%converter%release_pointer(tsrc, src, astream)
        end associate
    end subroutine permute_complex128_matrix

    subroutine permute_complex128_tensor_rank3(this, src, dst, perm, astream)
        class(tensor_permute_loops), intent(inout) :: this
        type(tensor_rank3), intent(in) :: src
        type(tensor_rank3), intent(inout) :: dst
        integer, dimension(3), intent(in) :: perm
        type(stream), intent(in), optional :: astream

        integer(int64) :: d1, d2, d3
        complex(real64), dimension(:,:,:), pointer, contiguous :: tsrc, tdst
        integer(int64) :: sidx(3), didx(3)

        associate ( sdims => src%dims, ddims => dst%dims )
            call this%converter%secure_fortran_pointer(tsrc, src, astream)
            call this%converter%secure_fortran_pointer(tdst, dst, astream)

            do d3 = 1, sdims(3)
                sidx(3) = d3
                do d2 = 1, sdims(2)
                    sidx(2) = d2
                    do d1 = 1, sdims(1)
                        sidx(1) = d1

                        didx = sidx(perm)
                        tdst(didx(1),didx(2),didx(3)) = tsrc(sidx(1),sidx(2),sidx(3))
                    end do
                end do
            end do

            call this%converter%update_remote_and_release_pointer(tdst, dst, astream)
            call this%converter%release_pointer(tsrc, src, astream)
        end associate
    end subroutine permute_complex128_tensor_rank3

    subroutine permute_complex128_tensor_rank4(this, src, dst, perm, astream)
        class(tensor_permute_loops), intent(inout) :: this
        type(tensor_rank4), intent(in) :: src
        type(tensor_rank4), intent(inout) :: dst
        integer, dimension(4), intent(in) :: perm
        type(stream), intent(in), optional :: astream

        integer(int64) :: d1, d2, d3, d4
        complex(real64), dimension(:,:,:,:), pointer, contiguous :: tsrc, tdst
        integer(int64) :: sidx(4), didx(4)

        associate ( sdims => src%dims, ddims => dst%dims )
            call this%converter%secure_fortran_pointer(tsrc, src, astream)
            call this%converter%secure_fortran_pointer(tdst, dst, astream)

            do d4 = 1, sdims(4)
                sidx(4) = d4
                do d3 = 1, sdims(3)
                    sidx(3) = d3
                    do d2 = 1, sdims(2)
                        sidx(2) = d2
                        do d1 = 1, sdims(1)
                            sidx(1) = d1

                            didx = sidx(perm)
                            tdst(didx(1),didx(2),didx(3),didx(4)) = tsrc(sidx(1),sidx(2),sidx(3),sidx(4))
                        end do
                    end do
                end do
            end do

            call this%converter%update_remote_and_release_pointer(tdst, dst, astream)
            call this%converter%release_pointer(tsrc, src, astream)
        end associate
    end subroutine permute_complex128_tensor_rank4

    subroutine permute_complex128_tensor_rank5(this, src, dst, perm, astream)
        class(tensor_permute_loops), intent(inout) :: this
        type(tensor_rank5), intent(in) :: src
        type(tensor_rank5), intent(inout) :: dst
        integer, dimension(5), intent(in) :: perm
        type(stream), intent(in), optional :: astream

        integer(int64) :: d1, d2, d3, d4, d5
        complex(real64), dimension(:,:,:,:,:), pointer, contiguous :: tsrc, tdst
        integer(int64) :: sidx(5), didx(5)

        associate ( sdims => src%dims, ddims => dst%dims )
            call this%converter%secure_fortran_pointer(tsrc, src, astream)
            call this%converter%secure_fortran_pointer(tdst, dst, astream)

            do d5 = 1, sdims(5)
                sidx(5) = d5
                do d4 = 1, sdims(4)
                    sidx(4) = d4
                    do d3 = 1, sdims(3)
                        sidx(3) = d3
                        do d2 = 1, sdims(2)
                            sidx(2) = d2
                            do d1 = 1, sdims(1)
                                sidx(1) = d1

                                didx = sidx(perm)
                                tdst(didx(1),didx(2),didx(3),didx(4),didx(5)) = tsrc(sidx(1),sidx(2),sidx(3),sidx(4),sidx(5))
                            end do
                        end do
                    end do
                end do
            end do

            call this%converter%update_remote_and_release_pointer(tdst, dst, astream)
            call this%converter%release_pointer(tsrc, src, astream)
        end associate
    end subroutine permute_complex128_tensor_rank5

    subroutine permute_complex64_matrix(this, src, dst, perm, astream)
        class(tensor_permute_loops), intent(inout) :: this
        type(matrix), intent(in) :: src
        type(matrix), intent(inout) :: dst
        integer, dimension(2), intent(in) :: perm
        type(stream), intent(in), optional :: astream

        integer(int64) :: d1, d2
        complex(real32), dimension(:,:), pointer, contiguous :: tsrc, tdst
        integer(int64) :: sidx(2), didx(2)

        associate ( sdims => src%dims, ddims => dst%dims )
            call this%converter%secure_fortran_pointer(tsrc, src, astream)
            call this%converter%secure_fortran_pointer(tdst, dst, astream)

            do d2 = 1, sdims(2)
                sidx(2) = d2
                do d1 = 1, sdims(1)
                    sidx(1) = d1

                    didx = sidx(perm)
                    tdst(didx(1),didx(2)) = tsrc(sidx(1),sidx(2))
                end do
            end do

            call this%converter%update_remote_and_release_pointer(tdst, dst, astream)
            call this%converter%release_pointer(tsrc, src, astream)
        end associate
    end subroutine permute_complex64_matrix

    subroutine permute_complex64_tensor_rank3(this, src, dst, perm, astream)
        class(tensor_permute_loops), intent(inout) :: this
        type(tensor_rank3), intent(in) :: src
        type(tensor_rank3), intent(inout) :: dst
        integer, dimension(3), intent(in) :: perm
        type(stream), intent(in), optional :: astream

        integer(int64) :: d1, d2, d3
        complex(real32), dimension(:,:,:), pointer, contiguous :: tsrc, tdst
        integer(int64) :: sidx(3), didx(3)

        associate ( sdims => src%dims, ddims => dst%dims )
            call this%converter%secure_fortran_pointer(tsrc, src, astream)
            call this%converter%secure_fortran_pointer(tdst, dst, astream)

            do d3 = 1, sdims(3)
                sidx(3) = d3
                do d2 = 1, sdims(2)
                    sidx(2) = d2
                    do d1 = 1, sdims(1)
                        sidx(1) = d1

                        didx = sidx(perm)
                        tdst(didx(1),didx(2),didx(3)) = tsrc(sidx(1),sidx(2),sidx(3))
                    end do
                end do
            end do

            call this%converter%update_remote_and_release_pointer(tdst, dst, astream)
            call this%converter%release_pointer(tsrc, src, astream)
        end associate
    end subroutine permute_complex64_tensor_rank3

    subroutine permute_complex64_tensor_rank4(this, src, dst, perm, astream)
        class(tensor_permute_loops), intent(inout) :: this
        type(tensor_rank4), intent(in) :: src
        type(tensor_rank4), intent(inout) :: dst
        integer, dimension(4), intent(in) :: perm
        type(stream), intent(in), optional :: astream

        integer(int64) :: d1, d2, d3, d4
        complex(real32), dimension(:,:,:,:), pointer, contiguous :: tsrc, tdst
        integer(int64) :: sidx(4), didx(4)

        associate ( sdims => src%dims, ddims => dst%dims )
            call this%converter%secure_fortran_pointer(tsrc, src, astream)
            call this%converter%secure_fortran_pointer(tdst, dst, astream)

            do d4 = 1, sdims(4)
                sidx(4) = d4
                do d3 = 1, sdims(3)
                    sidx(3) = d3
                    do d2 = 1, sdims(2)
                        sidx(2) = d2
                        do d1 = 1, sdims(1)
                            sidx(1) = d1

                            didx = sidx(perm)
                            tdst(didx(1),didx(2),didx(3),didx(4)) = tsrc(sidx(1),sidx(2),sidx(3),sidx(4))
                        end do
                    end do
                end do
            end do

            call this%converter%update_remote_and_release_pointer(tdst, dst, astream)
            call this%converter%release_pointer(tsrc, src, astream)
        end associate
    end subroutine permute_complex64_tensor_rank4

    subroutine permute_complex64_tensor_rank5(this, src, dst, perm, astream)
        class(tensor_permute_loops), intent(inout) :: this
        type(tensor_rank5), intent(in) :: src
        type(tensor_rank5), intent(inout) :: dst
        integer, dimension(5), intent(in) :: perm
        type(stream), intent(in), optional :: astream

        integer(int64) :: d1, d2, d3, d4, d5
        complex(real32), dimension(:,:,:,:,:), pointer, contiguous :: tsrc, tdst
        integer(int64) :: sidx(5), didx(5)

        associate ( sdims => src%dims, ddims => dst%dims )
            call this%converter%secure_fortran_pointer(tsrc, src, astream)
            call this%converter%secure_fortran_pointer(tdst, dst, astream)

            do d5 = 1, sdims(5)
                sidx(5) = d5
                do d4 = 1, sdims(4)
                    sidx(4) = d4
                    do d3 = 1, sdims(3)
                        sidx(3) = d3
                        do d2 = 1, sdims(2)
                            sidx(2) = d2
                            do d1 = 1, sdims(1)
                                sidx(1) = d1

                                didx = sidx(perm)
                                tdst(didx(1),didx(2),didx(3),didx(4),didx(5)) = tsrc(sidx(1),sidx(2),sidx(3),sidx(4),sidx(5))
                            end do
                        end do
                    end do
                end do
            end do

            call this%converter%update_remote_and_release_pointer(tdst, dst, astream)
            call this%converter%release_pointer(tsrc, src, astream)
        end associate
    end subroutine permute_complex64_tensor_rank5

    subroutine permute_real64_matrix(this, src, dst, perm, astream)
        class(tensor_permute_loops), intent(inout) :: this
        type(matrix), intent(in) :: src
        type(matrix), intent(inout) :: dst
        integer, dimension(2), intent(in) :: perm
        type(stream), intent(in), optional :: astream

        integer(int64) :: d1, d2
        real(real64), dimension(:,:), pointer, contiguous :: tsrc, tdst
        integer(int64) :: sidx(2), didx(2)

        associate ( sdims => src%dims, ddims => dst%dims )
            call this%converter%secure_fortran_pointer(tsrc, src, astream)
            call this%converter%secure_fortran_pointer(tdst, dst, astream)

            do d2 = 1, sdims(2)
                sidx(2) = d2
                do d1 = 1, sdims(1)
                    sidx(1) = d1

                    didx = sidx(perm)
                    tdst(didx(1),didx(2)) = tsrc(sidx(1),sidx(2))
                end do
            end do

            call this%converter%update_remote_and_release_pointer(tdst, dst, astream)
            call this%converter%release_pointer(tsrc, src, astream)
        end associate
    end subroutine permute_real64_matrix

    subroutine permute_real64_tensor_rank3(this, src, dst, perm, astream)
        class(tensor_permute_loops), intent(inout) :: this
        type(tensor_rank3), intent(in) :: src
        type(tensor_rank3), intent(inout) :: dst
        integer, dimension(3), intent(in) :: perm
        type(stream), intent(in), optional :: astream

        integer(int64) :: d1, d2, d3
        real(real64), dimension(:,:,:), pointer, contiguous :: tsrc, tdst
        integer(int64) :: sidx(3), didx(3)

        associate ( sdims => src%dims, ddims => dst%dims )
            call this%converter%secure_fortran_pointer(tsrc, src, astream)
            call this%converter%secure_fortran_pointer(tdst, dst, astream)

            do d3 = 1, sdims(3)
                sidx(3) = d3
                do d2 = 1, sdims(2)
                    sidx(2) = d2
                    do d1 = 1, sdims(1)
                        sidx(1) = d1

                        didx = sidx(perm)
                        tdst(didx(1),didx(2),didx(3)) = tsrc(sidx(1),sidx(2),sidx(3))
                    end do
                end do
            end do

            call this%converter%update_remote_and_release_pointer(tdst, dst, astream)
            call this%converter%release_pointer(tsrc, src, astream)
        end associate
    end subroutine permute_real64_tensor_rank3

    subroutine permute_real64_tensor_rank4(this, src, dst, perm, astream)
        class(tensor_permute_loops), intent(inout) :: this
        type(tensor_rank4), intent(in) :: src
        type(tensor_rank4), intent(inout) :: dst
        integer, dimension(4), intent(in) :: perm
        type(stream), intent(in), optional :: astream

        integer(int64) :: d1, d2, d3, d4
        real(real64), dimension(:,:,:,:), pointer, contiguous :: tsrc, tdst
        integer(int64) :: sidx(4), didx(4)

        associate ( sdims => src%dims, ddims => dst%dims )
            call this%converter%secure_fortran_pointer(tsrc, src, astream)
            call this%converter%secure_fortran_pointer(tdst, dst, astream)

            do d4 = 1, sdims(4)
                sidx(4) = d4
                do d3 = 1, sdims(3)
                    sidx(3) = d3
                    do d2 = 1, sdims(2)
                        sidx(2) = d2
                        do d1 = 1, sdims(1)
                            sidx(1) = d1

                            didx = sidx(perm)
                            tdst(didx(1),didx(2),didx(3),didx(4)) = tsrc(sidx(1),sidx(2),sidx(3),sidx(4))
                        end do
                    end do
                end do
            end do

            call this%converter%update_remote_and_release_pointer(tdst, dst, astream)
            call this%converter%release_pointer(tsrc, src, astream)
        end associate
    end subroutine permute_real64_tensor_rank4

    subroutine permute_real64_tensor_rank5(this, src, dst, perm, astream)
        class(tensor_permute_loops), intent(inout) :: this
        type(tensor_rank5), intent(in) :: src
        type(tensor_rank5), intent(inout) :: dst
        integer, dimension(5), intent(in) :: perm
        type(stream), intent(in), optional :: astream

        integer(int64) :: d1, d2, d3, d4, d5
        real(real64), dimension(:,:,:,:,:), pointer, contiguous :: tsrc, tdst
        integer(int64) :: sidx(5), didx(5)

        associate ( sdims => src%dims, ddims => dst%dims )
            call this%converter%secure_fortran_pointer(tsrc, src, astream)
            call this%converter%secure_fortran_pointer(tdst, dst, astream)

            do d5 = 1, sdims(5)
                sidx(5) = d5
                do d4 = 1, sdims(4)
                    sidx(4) = d4
                    do d3 = 1, sdims(3)
                        sidx(3) = d3
                        do d2 = 1, sdims(2)
                            sidx(2) = d2
                            do d1 = 1, sdims(1)
                                sidx(1) = d1

                                didx = sidx(perm)
                                tdst(didx(1),didx(2),didx(3),didx(4),didx(5)) = tsrc(sidx(1),sidx(2),sidx(3),sidx(4),sidx(5))
                            end do
                        end do
                    end do
                end do
            end do

            call this%converter%update_remote_and_release_pointer(tdst, dst, astream)
            call this%converter%release_pointer(tsrc, src, astream)
        end associate
    end subroutine permute_real64_tensor_rank5

    subroutine permute_real32_matrix(this, src, dst, perm, astream)
        class(tensor_permute_loops), intent(inout) :: this
        type(matrix), intent(in) :: src
        type(matrix), intent(inout) :: dst
        integer, dimension(2), intent(in) :: perm
        type(stream), intent(in), optional :: astream

        integer(int64) :: d1, d2
        real(real32), dimension(:,:), pointer, contiguous :: tsrc, tdst
        integer(int64) :: sidx(2), didx(2)

        associate ( sdims => src%dims, ddims => dst%dims )
            call this%converter%secure_fortran_pointer(tsrc, src, astream)
            call this%converter%secure_fortran_pointer(tdst, dst, astream)

            do d2 = 1, sdims(2)
                sidx(2) = d2
                do d1 = 1, sdims(1)
                    sidx(1) = d1

                    didx = sidx(perm)
                    tdst(didx(1),didx(2)) = tsrc(sidx(1),sidx(2))
                end do
            end do

            call this%converter%update_remote_and_release_pointer(tdst, dst, astream)
            call this%converter%release_pointer(tsrc, src, astream)
        end associate
    end subroutine permute_real32_matrix

    subroutine permute_real32_tensor_rank3(this, src, dst, perm, astream)
        class(tensor_permute_loops), intent(inout) :: this
        type(tensor_rank3), intent(in) :: src
        type(tensor_rank3), intent(inout) :: dst
        integer, dimension(3), intent(in) :: perm
        type(stream), intent(in), optional :: astream

        integer(int64) :: d1, d2, d3
        real(real32), dimension(:,:,:), pointer, contiguous :: tsrc, tdst
        integer(int64) :: sidx(3), didx(3)

        associate ( sdims => src%dims, ddims => dst%dims )
            call this%converter%secure_fortran_pointer(tsrc, src, astream)
            call this%converter%secure_fortran_pointer(tdst, dst, astream)

            do d3 = 1, sdims(3)
                sidx(3) = d3
                do d2 = 1, sdims(2)
                    sidx(2) = d2
                    do d1 = 1, sdims(1)
                        sidx(1) = d1

                        didx = sidx(perm)
                        tdst(didx(1),didx(2),didx(3)) = tsrc(sidx(1),sidx(2),sidx(3))
                    end do
                end do
            end do

            call this%converter%update_remote_and_release_pointer(tdst, dst, astream)
            call this%converter%release_pointer(tsrc, src, astream)
        end associate
    end subroutine permute_real32_tensor_rank3

    subroutine permute_real32_tensor_rank4(this, src, dst, perm, astream)
        class(tensor_permute_loops), intent(inout) :: this
        type(tensor_rank4), intent(in) :: src
        type(tensor_rank4), intent(inout) :: dst
        integer, dimension(4), intent(in) :: perm
        type(stream), intent(in), optional :: astream

        integer(int64) :: d1, d2, d3, d4
        real(real32), dimension(:,:,:,:), pointer, contiguous :: tsrc, tdst
        integer(int64) :: sidx(4), didx(4)

        associate ( sdims => src%dims, ddims => dst%dims )
            call this%converter%secure_fortran_pointer(tsrc, src, astream)
            call this%converter%secure_fortran_pointer(tdst, dst, astream)

            do d4 = 1, sdims(4)
                sidx(4) = d4
                do d3 = 1, sdims(3)
                    sidx(3) = d3
                    do d2 = 1, sdims(2)
                        sidx(2) = d2
                        do d1 = 1, sdims(1)
                            sidx(1) = d1

                            didx = sidx(perm)
                            tdst(didx(1),didx(2),didx(3),didx(4)) = tsrc(sidx(1),sidx(2),sidx(3),sidx(4))
                        end do
                    end do
                end do
            end do

            call this%converter%update_remote_and_release_pointer(tdst, dst, astream)
            call this%converter%release_pointer(tsrc, src, astream)
        end associate
    end subroutine permute_real32_tensor_rank4

    subroutine permute_real32_tensor_rank5(this, src, dst, perm, astream)
        class(tensor_permute_loops), intent(inout) :: this
        type(tensor_rank5), intent(in) :: src
        type(tensor_rank5), intent(inout) :: dst
        integer, dimension(5), intent(in) :: perm
        type(stream), intent(in), optional :: astream

        integer(int64) :: d1, d2, d3, d4, d5
        real(real32), dimension(:,:,:,:,:), pointer, contiguous :: tsrc, tdst
        integer(int64) :: sidx(5), didx(5)

        associate ( sdims => src%dims, ddims => dst%dims )
            call this%converter%secure_fortran_pointer(tsrc, src, astream)
            call this%converter%secure_fortran_pointer(tdst, dst, astream)

            do d5 = 1, sdims(5)
                sidx(5) = d5
                do d4 = 1, sdims(4)
                    sidx(4) = d4
                    do d3 = 1, sdims(3)
                        sidx(3) = d3
                        do d2 = 1, sdims(2)
                            sidx(2) = d2
                            do d1 = 1, sdims(1)
                                sidx(1) = d1

                                didx = sidx(perm)
                                tdst(didx(1),didx(2),didx(3),didx(4),didx(5)) = tsrc(sidx(1),sidx(2),sidx(3),sidx(4),sidx(5))
                            end do
                        end do
                    end do
                end do
            end do

            call this%converter%update_remote_and_release_pointer(tdst, dst, astream)
            call this%converter%release_pointer(tsrc, src, astream)
        end associate
    end subroutine permute_real32_tensor_rank5

    subroutine synchronize(this, astream)
        class(tensor_permute_loops), intent(in) :: this
        type(stream), intent(in), optional :: astream

        continue
    end subroutine synchronize

    subroutine cleanup(this)
        class(tensor_permute_loops), intent(inout) :: this

        call this%converter%cleanup()
    end subroutine cleanup
end module tensor_permute_loops_module
