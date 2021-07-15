! Auto-generated. DO NOT MODIFY!
module tensor_permute_test_helper_module
    use, intrinsic :: iso_fortran_env, only : &
            int64, &
            real64, &
            real32
    use :: util_api, only : &
            string, &
            dictionary, &
            assert
    use :: data_api, only : &
            storage_helper, &
            concurrency_factory, &
            stream, &
            stream_handler
    use :: tensor_api, only : &
            tensor, &
            allocate_and_copy_tensor
    use :: algorithms_api, only : &
            tensor_permute, &
            tensor_permute_factory

    implicit none
    private

    public :: test_all
    public :: permute_real64_tensor_rank4_1_4_3_2
    public :: permute_real32_tensor_rank4_1_4_3_2

contains
    subroutine test_all(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        call test_permute_complex128_matrix_1_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_matrix_2_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank3_1_2_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank3_1_3_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank3_2_1_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank3_2_3_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank3_3_2_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank3_3_1_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank4_1_2_3_4( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank4_1_2_4_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank4_1_3_2_4( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank4_1_3_4_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank4_1_4_3_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank4_1_4_2_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank4_2_1_3_4( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank4_2_1_4_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank4_2_3_1_4( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank4_2_3_4_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank4_2_4_3_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank4_2_4_1_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank4_3_2_1_4( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank4_3_2_4_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank4_3_1_2_4( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank4_3_1_4_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank4_3_4_1_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank4_3_4_2_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank4_4_2_3_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank4_4_2_1_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank4_4_3_2_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank4_4_3_1_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank4_4_1_3_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank4_4_1_2_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank5_1_2_3_4_5( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank5_1_3_4_5_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank5_1_5_3_4_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank5_2_1_4_5_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank5_2_4_3_1_5( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank5_2_5_4_1_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank5_3_1_2_4_5( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank5_3_4_2_5_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank5_4_2_3_1_5( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank5_4_3_1_5_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank5_4_5_3_1_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank5_5_2_4_1_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank5_5_4_3_2_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex128_tensor_rank5_5_1_4_2_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_matrix_1_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_matrix_2_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank3_1_2_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank3_1_3_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank3_2_1_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank3_2_3_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank3_3_2_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank3_3_1_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank4_1_2_3_4( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank4_1_2_4_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank4_1_3_2_4( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank4_1_3_4_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank4_1_4_3_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank4_1_4_2_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank4_2_1_3_4( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank4_2_1_4_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank4_2_3_1_4( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank4_2_3_4_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank4_2_4_3_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank4_2_4_1_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank4_3_2_1_4( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank4_3_2_4_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank4_3_1_2_4( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank4_3_1_4_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank4_3_4_1_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank4_3_4_2_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank4_4_2_3_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank4_4_2_1_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank4_4_3_2_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank4_4_3_1_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank4_4_1_3_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank4_4_1_2_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank5_1_2_3_4_5( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank5_1_3_4_5_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank5_1_5_3_4_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank5_2_1_4_5_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank5_2_4_3_1_5( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank5_2_5_4_1_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank5_3_1_2_4_5( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank5_3_4_2_5_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank5_4_2_3_1_5( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank5_4_3_1_5_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank5_4_5_3_1_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank5_5_2_4_1_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank5_5_4_3_2_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_complex64_tensor_rank5_5_1_4_2_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_matrix_1_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_matrix_2_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank3_1_2_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank3_1_3_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank3_2_1_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank3_2_3_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank3_3_2_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank3_3_1_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank4_1_2_3_4( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank4_1_2_4_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank4_1_3_2_4( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank4_1_3_4_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank4_1_4_3_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank4_1_4_2_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank4_2_1_3_4( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank4_2_1_4_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank4_2_3_1_4( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank4_2_3_4_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank4_2_4_3_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank4_2_4_1_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank4_3_2_1_4( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank4_3_2_4_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank4_3_1_2_4( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank4_3_1_4_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank4_3_4_1_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank4_3_4_2_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank4_4_2_3_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank4_4_2_1_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank4_4_3_2_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank4_4_3_1_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank4_4_1_3_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank4_4_1_2_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank5_1_2_3_4_5( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank5_1_3_4_5_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank5_1_5_3_4_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank5_2_1_4_5_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank5_2_4_3_1_5( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank5_2_5_4_1_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank5_3_1_2_4_5( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank5_3_4_2_5_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank5_4_2_3_1_5( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank5_4_3_1_5_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank5_4_5_3_1_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank5_5_2_4_1_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank5_5_4_3_2_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real64_tensor_rank5_5_1_4_2_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_matrix_1_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_matrix_2_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank3_1_2_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank3_1_3_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank3_2_1_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank3_2_3_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank3_3_2_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank3_3_1_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank4_1_2_3_4( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank4_1_2_4_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank4_1_3_2_4( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank4_1_3_4_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank4_1_4_3_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank4_1_4_2_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank4_2_1_3_4( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank4_2_1_4_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank4_2_3_1_4( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank4_2_3_4_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank4_2_4_3_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank4_2_4_1_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank4_3_2_1_4( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank4_3_2_4_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank4_3_1_2_4( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank4_3_1_4_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank4_3_4_1_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank4_3_4_2_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank4_4_2_3_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank4_4_2_1_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank4_4_3_2_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank4_4_3_1_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank4_4_1_3_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank4_4_1_2_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank5_1_2_3_4_5( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank5_1_3_4_5_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank5_1_5_3_4_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank5_2_1_4_5_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank5_2_4_3_1_5( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank5_2_5_4_1_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank5_3_1_2_4_5( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank5_3_4_2_5_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank5_4_2_3_1_5( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank5_4_3_1_5_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank5_4_5_3_1_2( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank5_5_2_4_1_3( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank5_5_4_3_2_1( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_permute_real32_tensor_rank5_5_1_4_2_3( &
                assertion, prefix, driver, memtype, options, priorities)
    end subroutine test_all

    subroutine test_permute_complex128_matrix_1_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3) :: src
        complex(real64), dimension(2,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_matrix_1_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_matrix_1_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_matrix_1_2

    subroutine permute_complex128_matrix_1_2(dst, src)
        complex(real64), dimension(:,:), intent(inout) :: dst
        complex(real64), dimension(:,:), intent(in) :: src

        integer :: i1, i2

        do i2 = 1, size(src, 2)
            do i1 = 1, size(src, 1)
                dst(i1,i2) = src(i1,i2)
            end do
        end do
    end subroutine permute_complex128_matrix_1_2

    subroutine test_permute_complex128_matrix_2_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3) :: src
        complex(real64), dimension(3,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_matrix_2_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_matrix_2_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_matrix_2_1

    subroutine permute_complex128_matrix_2_1(dst, src)
        complex(real64), dimension(:,:), intent(inout) :: dst
        complex(real64), dimension(:,:), intent(in) :: src

        integer :: i1, i2

        do i2 = 1, size(src, 2)
            do i1 = 1, size(src, 1)
                dst(i2,i1) = src(i1,i2)
            end do
        end do
    end subroutine permute_complex128_matrix_2_1

    subroutine test_permute_complex128_tensor_rank3_1_2_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5) :: src
        complex(real64), dimension(2,3,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank3_1_2_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 2, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank3_1_2_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank3_1_2_3

    subroutine permute_complex128_tensor_rank3_1_2_3(dst, src)
        complex(real64), dimension(:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:), intent(in) :: src

        integer :: i1, i2, i3

        do i3 = 1, size(src, 3)
            do i2 = 1, size(src, 2)
                do i1 = 1, size(src, 1)
                    dst(i1,i2,i3) = src(i1,i2,i3)
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank3_1_2_3

    subroutine test_permute_complex128_tensor_rank3_1_3_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5) :: src
        complex(real64), dimension(2,5,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank3_1_3_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 3, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank3_1_3_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank3_1_3_2

    subroutine permute_complex128_tensor_rank3_1_3_2(dst, src)
        complex(real64), dimension(:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:), intent(in) :: src

        integer :: i1, i2, i3

        do i3 = 1, size(src, 3)
            do i2 = 1, size(src, 2)
                do i1 = 1, size(src, 1)
                    dst(i1,i3,i2) = src(i1,i2,i3)
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank3_1_3_2

    subroutine test_permute_complex128_tensor_rank3_2_1_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5) :: src
        complex(real64), dimension(3,2,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank3_2_1_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 1, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank3_2_1_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank3_2_1_3

    subroutine permute_complex128_tensor_rank3_2_1_3(dst, src)
        complex(real64), dimension(:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:), intent(in) :: src

        integer :: i1, i2, i3

        do i3 = 1, size(src, 3)
            do i2 = 1, size(src, 2)
                do i1 = 1, size(src, 1)
                    dst(i2,i1,i3) = src(i1,i2,i3)
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank3_2_1_3

    subroutine test_permute_complex128_tensor_rank3_2_3_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5) :: src
        complex(real64), dimension(3,5,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank3_2_3_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 3, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank3_2_3_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank3_2_3_1

    subroutine permute_complex128_tensor_rank3_2_3_1(dst, src)
        complex(real64), dimension(:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:), intent(in) :: src

        integer :: i1, i2, i3

        do i3 = 1, size(src, 3)
            do i2 = 1, size(src, 2)
                do i1 = 1, size(src, 1)
                    dst(i2,i3,i1) = src(i1,i2,i3)
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank3_2_3_1

    subroutine test_permute_complex128_tensor_rank3_3_2_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5) :: src
        complex(real64), dimension(5,3,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank3_3_2_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 2, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank3_3_2_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank3_3_2_1

    subroutine permute_complex128_tensor_rank3_3_2_1(dst, src)
        complex(real64), dimension(:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:), intent(in) :: src

        integer :: i1, i2, i3

        do i3 = 1, size(src, 3)
            do i2 = 1, size(src, 2)
                do i1 = 1, size(src, 1)
                    dst(i3,i2,i1) = src(i1,i2,i3)
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank3_3_2_1

    subroutine test_permute_complex128_tensor_rank3_3_1_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5) :: src
        complex(real64), dimension(5,2,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank3_3_1_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 1, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank3_3_1_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank3_3_1_2

    subroutine permute_complex128_tensor_rank3_3_1_2(dst, src)
        complex(real64), dimension(:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:), intent(in) :: src

        integer :: i1, i2, i3

        do i3 = 1, size(src, 3)
            do i2 = 1, size(src, 2)
                do i1 = 1, size(src, 1)
                    dst(i3,i1,i2) = src(i1,i2,i3)
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank3_3_1_2

    subroutine test_permute_complex128_tensor_rank4_1_2_3_4(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7) :: src
        complex(real64), dimension(2,3,5,7) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank4_1_2_3_4(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 2, 3, 4], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank4_1_2_3_4:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank4_1_2_3_4

    subroutine permute_complex128_tensor_rank4_1_2_3_4(dst, src)
        complex(real64), dimension(:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i1,i2,i3,i4) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank4_1_2_3_4

    subroutine test_permute_complex128_tensor_rank4_1_2_4_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7) :: src
        complex(real64), dimension(2,3,7,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank4_1_2_4_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 2, 4, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank4_1_2_4_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank4_1_2_4_3

    subroutine permute_complex128_tensor_rank4_1_2_4_3(dst, src)
        complex(real64), dimension(:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i1,i2,i4,i3) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank4_1_2_4_3

    subroutine test_permute_complex128_tensor_rank4_1_3_2_4(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7) :: src
        complex(real64), dimension(2,5,3,7) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank4_1_3_2_4(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 3, 2, 4], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank4_1_3_2_4:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank4_1_3_2_4

    subroutine permute_complex128_tensor_rank4_1_3_2_4(dst, src)
        complex(real64), dimension(:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i1,i3,i2,i4) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank4_1_3_2_4

    subroutine test_permute_complex128_tensor_rank4_1_3_4_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7) :: src
        complex(real64), dimension(2,5,7,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank4_1_3_4_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 3, 4, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank4_1_3_4_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank4_1_3_4_2

    subroutine permute_complex128_tensor_rank4_1_3_4_2(dst, src)
        complex(real64), dimension(:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i1,i3,i4,i2) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank4_1_3_4_2

    subroutine test_permute_complex128_tensor_rank4_1_4_3_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7) :: src
        complex(real64), dimension(2,7,5,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank4_1_4_3_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 4, 3, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank4_1_4_3_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank4_1_4_3_2

    subroutine permute_complex128_tensor_rank4_1_4_3_2(dst, src)
        complex(real64), dimension(:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i1,i4,i3,i2) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank4_1_4_3_2

    subroutine test_permute_complex128_tensor_rank4_1_4_2_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7) :: src
        complex(real64), dimension(2,7,3,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank4_1_4_2_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 4, 2, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank4_1_4_2_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank4_1_4_2_3

    subroutine permute_complex128_tensor_rank4_1_4_2_3(dst, src)
        complex(real64), dimension(:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i1,i4,i2,i3) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank4_1_4_2_3

    subroutine test_permute_complex128_tensor_rank4_2_1_3_4(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7) :: src
        complex(real64), dimension(3,2,5,7) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank4_2_1_3_4(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 1, 3, 4], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank4_2_1_3_4:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank4_2_1_3_4

    subroutine permute_complex128_tensor_rank4_2_1_3_4(dst, src)
        complex(real64), dimension(:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i2,i1,i3,i4) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank4_2_1_3_4

    subroutine test_permute_complex128_tensor_rank4_2_1_4_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7) :: src
        complex(real64), dimension(3,2,7,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank4_2_1_4_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 1, 4, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank4_2_1_4_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank4_2_1_4_3

    subroutine permute_complex128_tensor_rank4_2_1_4_3(dst, src)
        complex(real64), dimension(:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i2,i1,i4,i3) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank4_2_1_4_3

    subroutine test_permute_complex128_tensor_rank4_2_3_1_4(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7) :: src
        complex(real64), dimension(3,5,2,7) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank4_2_3_1_4(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 3, 1, 4], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank4_2_3_1_4:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank4_2_3_1_4

    subroutine permute_complex128_tensor_rank4_2_3_1_4(dst, src)
        complex(real64), dimension(:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i2,i3,i1,i4) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank4_2_3_1_4

    subroutine test_permute_complex128_tensor_rank4_2_3_4_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7) :: src
        complex(real64), dimension(3,5,7,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank4_2_3_4_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 3, 4, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank4_2_3_4_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank4_2_3_4_1

    subroutine permute_complex128_tensor_rank4_2_3_4_1(dst, src)
        complex(real64), dimension(:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i2,i3,i4,i1) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank4_2_3_4_1

    subroutine test_permute_complex128_tensor_rank4_2_4_3_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7) :: src
        complex(real64), dimension(3,7,5,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank4_2_4_3_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 4, 3, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank4_2_4_3_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank4_2_4_3_1

    subroutine permute_complex128_tensor_rank4_2_4_3_1(dst, src)
        complex(real64), dimension(:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i2,i4,i3,i1) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank4_2_4_3_1

    subroutine test_permute_complex128_tensor_rank4_2_4_1_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7) :: src
        complex(real64), dimension(3,7,2,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank4_2_4_1_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 4, 1, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank4_2_4_1_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank4_2_4_1_3

    subroutine permute_complex128_tensor_rank4_2_4_1_3(dst, src)
        complex(real64), dimension(:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i2,i4,i1,i3) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank4_2_4_1_3

    subroutine test_permute_complex128_tensor_rank4_3_2_1_4(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7) :: src
        complex(real64), dimension(5,3,2,7) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank4_3_2_1_4(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 2, 1, 4], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank4_3_2_1_4:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank4_3_2_1_4

    subroutine permute_complex128_tensor_rank4_3_2_1_4(dst, src)
        complex(real64), dimension(:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i3,i2,i1,i4) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank4_3_2_1_4

    subroutine test_permute_complex128_tensor_rank4_3_2_4_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7) :: src
        complex(real64), dimension(5,3,7,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank4_3_2_4_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 2, 4, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank4_3_2_4_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank4_3_2_4_1

    subroutine permute_complex128_tensor_rank4_3_2_4_1(dst, src)
        complex(real64), dimension(:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i3,i2,i4,i1) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank4_3_2_4_1

    subroutine test_permute_complex128_tensor_rank4_3_1_2_4(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7) :: src
        complex(real64), dimension(5,2,3,7) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank4_3_1_2_4(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 1, 2, 4], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank4_3_1_2_4:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank4_3_1_2_4

    subroutine permute_complex128_tensor_rank4_3_1_2_4(dst, src)
        complex(real64), dimension(:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i3,i1,i2,i4) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank4_3_1_2_4

    subroutine test_permute_complex128_tensor_rank4_3_1_4_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7) :: src
        complex(real64), dimension(5,2,7,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank4_3_1_4_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 1, 4, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank4_3_1_4_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank4_3_1_4_2

    subroutine permute_complex128_tensor_rank4_3_1_4_2(dst, src)
        complex(real64), dimension(:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i3,i1,i4,i2) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank4_3_1_4_2

    subroutine test_permute_complex128_tensor_rank4_3_4_1_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7) :: src
        complex(real64), dimension(5,7,2,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank4_3_4_1_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 4, 1, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank4_3_4_1_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank4_3_4_1_2

    subroutine permute_complex128_tensor_rank4_3_4_1_2(dst, src)
        complex(real64), dimension(:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i3,i4,i1,i2) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank4_3_4_1_2

    subroutine test_permute_complex128_tensor_rank4_3_4_2_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7) :: src
        complex(real64), dimension(5,7,3,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank4_3_4_2_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 4, 2, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank4_3_4_2_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank4_3_4_2_1

    subroutine permute_complex128_tensor_rank4_3_4_2_1(dst, src)
        complex(real64), dimension(:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i3,i4,i2,i1) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank4_3_4_2_1

    subroutine test_permute_complex128_tensor_rank4_4_2_3_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7) :: src
        complex(real64), dimension(7,3,5,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank4_4_2_3_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [4, 2, 3, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank4_4_2_3_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank4_4_2_3_1

    subroutine permute_complex128_tensor_rank4_4_2_3_1(dst, src)
        complex(real64), dimension(:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i4,i2,i3,i1) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank4_4_2_3_1

    subroutine test_permute_complex128_tensor_rank4_4_2_1_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7) :: src
        complex(real64), dimension(7,3,2,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank4_4_2_1_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [4, 2, 1, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank4_4_2_1_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank4_4_2_1_3

    subroutine permute_complex128_tensor_rank4_4_2_1_3(dst, src)
        complex(real64), dimension(:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i4,i2,i1,i3) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank4_4_2_1_3

    subroutine test_permute_complex128_tensor_rank4_4_3_2_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7) :: src
        complex(real64), dimension(7,5,3,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank4_4_3_2_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [4, 3, 2, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank4_4_3_2_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank4_4_3_2_1

    subroutine permute_complex128_tensor_rank4_4_3_2_1(dst, src)
        complex(real64), dimension(:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i4,i3,i2,i1) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank4_4_3_2_1

    subroutine test_permute_complex128_tensor_rank4_4_3_1_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7) :: src
        complex(real64), dimension(7,5,2,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank4_4_3_1_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [4, 3, 1, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank4_4_3_1_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank4_4_3_1_2

    subroutine permute_complex128_tensor_rank4_4_3_1_2(dst, src)
        complex(real64), dimension(:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i4,i3,i1,i2) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank4_4_3_1_2

    subroutine test_permute_complex128_tensor_rank4_4_1_3_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7) :: src
        complex(real64), dimension(7,2,5,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank4_4_1_3_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [4, 1, 3, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank4_4_1_3_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank4_4_1_3_2

    subroutine permute_complex128_tensor_rank4_4_1_3_2(dst, src)
        complex(real64), dimension(:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i4,i1,i3,i2) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank4_4_1_3_2

    subroutine test_permute_complex128_tensor_rank4_4_1_2_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7) :: src
        complex(real64), dimension(7,2,3,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank4_4_1_2_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [4, 1, 2, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank4_4_1_2_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank4_4_1_2_3

    subroutine permute_complex128_tensor_rank4_4_1_2_3(dst, src)
        complex(real64), dimension(:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i4,i1,i2,i3) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank4_4_1_2_3

    subroutine test_permute_complex128_tensor_rank5_1_2_3_4_5(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7,11) :: src
        complex(real64), dimension(2,3,5,7,11) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7,11) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank5_1_2_3_4_5(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 2, 3, 4, 5], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank5_1_2_3_4_5:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank5_1_2_3_4_5

    subroutine permute_complex128_tensor_rank5_1_2_3_4_5(dst, src)
        complex(real64), dimension(:,:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i1,i2,i3,i4,i5) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank5_1_2_3_4_5

    subroutine test_permute_complex128_tensor_rank5_1_3_4_5_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7,11) :: src
        complex(real64), dimension(2,5,7,11,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7,11) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank5_1_3_4_5_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 3, 4, 5, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank5_1_3_4_5_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank5_1_3_4_5_2

    subroutine permute_complex128_tensor_rank5_1_3_4_5_2(dst, src)
        complex(real64), dimension(:,:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i1,i3,i4,i5,i2) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank5_1_3_4_5_2

    subroutine test_permute_complex128_tensor_rank5_1_5_3_4_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7,11) :: src
        complex(real64), dimension(2,11,5,7,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7,11) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank5_1_5_3_4_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 5, 3, 4, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank5_1_5_3_4_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank5_1_5_3_4_2

    subroutine permute_complex128_tensor_rank5_1_5_3_4_2(dst, src)
        complex(real64), dimension(:,:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i1,i5,i3,i4,i2) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank5_1_5_3_4_2

    subroutine test_permute_complex128_tensor_rank5_2_1_4_5_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7,11) :: src
        complex(real64), dimension(3,2,7,11,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7,11) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank5_2_1_4_5_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 1, 4, 5, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank5_2_1_4_5_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank5_2_1_4_5_3

    subroutine permute_complex128_tensor_rank5_2_1_4_5_3(dst, src)
        complex(real64), dimension(:,:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i2,i1,i4,i5,i3) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank5_2_1_4_5_3

    subroutine test_permute_complex128_tensor_rank5_2_4_3_1_5(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7,11) :: src
        complex(real64), dimension(3,7,5,2,11) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7,11) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank5_2_4_3_1_5(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 4, 3, 1, 5], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank5_2_4_3_1_5:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank5_2_4_3_1_5

    subroutine permute_complex128_tensor_rank5_2_4_3_1_5(dst, src)
        complex(real64), dimension(:,:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i2,i4,i3,i1,i5) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank5_2_4_3_1_5

    subroutine test_permute_complex128_tensor_rank5_2_5_4_1_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7,11) :: src
        complex(real64), dimension(3,11,7,2,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7,11) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank5_2_5_4_1_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 5, 4, 1, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank5_2_5_4_1_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank5_2_5_4_1_3

    subroutine permute_complex128_tensor_rank5_2_5_4_1_3(dst, src)
        complex(real64), dimension(:,:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i2,i5,i4,i1,i3) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank5_2_5_4_1_3

    subroutine test_permute_complex128_tensor_rank5_3_1_2_4_5(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7,11) :: src
        complex(real64), dimension(5,2,3,7,11) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7,11) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank5_3_1_2_4_5(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 1, 2, 4, 5], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank5_3_1_2_4_5:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank5_3_1_2_4_5

    subroutine permute_complex128_tensor_rank5_3_1_2_4_5(dst, src)
        complex(real64), dimension(:,:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i3,i1,i2,i4,i5) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank5_3_1_2_4_5

    subroutine test_permute_complex128_tensor_rank5_3_4_2_5_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7,11) :: src
        complex(real64), dimension(5,7,3,11,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7,11) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank5_3_4_2_5_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 4, 2, 5, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank5_3_4_2_5_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank5_3_4_2_5_1

    subroutine permute_complex128_tensor_rank5_3_4_2_5_1(dst, src)
        complex(real64), dimension(:,:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i3,i4,i2,i5,i1) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank5_3_4_2_5_1

    subroutine test_permute_complex128_tensor_rank5_4_2_3_1_5(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7,11) :: src
        complex(real64), dimension(7,3,5,2,11) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7,11) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank5_4_2_3_1_5(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [4, 2, 3, 1, 5], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank5_4_2_3_1_5:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank5_4_2_3_1_5

    subroutine permute_complex128_tensor_rank5_4_2_3_1_5(dst, src)
        complex(real64), dimension(:,:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i4,i2,i3,i1,i5) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank5_4_2_3_1_5

    subroutine test_permute_complex128_tensor_rank5_4_3_1_5_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7,11) :: src
        complex(real64), dimension(7,5,2,11,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7,11) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank5_4_3_1_5_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [4, 3, 1, 5, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank5_4_3_1_5_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank5_4_3_1_5_2

    subroutine permute_complex128_tensor_rank5_4_3_1_5_2(dst, src)
        complex(real64), dimension(:,:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i4,i3,i1,i5,i2) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank5_4_3_1_5_2

    subroutine test_permute_complex128_tensor_rank5_4_5_3_1_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7,11) :: src
        complex(real64), dimension(7,11,5,2,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7,11) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank5_4_5_3_1_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [4, 5, 3, 1, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank5_4_5_3_1_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank5_4_5_3_1_2

    subroutine permute_complex128_tensor_rank5_4_5_3_1_2(dst, src)
        complex(real64), dimension(:,:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i4,i5,i3,i1,i2) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank5_4_5_3_1_2

    subroutine test_permute_complex128_tensor_rank5_5_2_4_1_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7,11) :: src
        complex(real64), dimension(11,3,7,2,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7,11) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank5_5_2_4_1_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [5, 2, 4, 1, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank5_5_2_4_1_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank5_5_2_4_1_3

    subroutine permute_complex128_tensor_rank5_5_2_4_1_3(dst, src)
        complex(real64), dimension(:,:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i5,i2,i4,i1,i3) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank5_5_2_4_1_3

    subroutine test_permute_complex128_tensor_rank5_5_4_3_2_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7,11) :: src
        complex(real64), dimension(11,7,5,3,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7,11) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank5_5_4_3_2_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [5, 4, 3, 2, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank5_5_4_3_2_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank5_5_4_3_2_1

    subroutine permute_complex128_tensor_rank5_5_4_3_2_1(dst, src)
        complex(real64), dimension(:,:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i5,i4,i3,i2,i1) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank5_5_4_3_2_1

    subroutine test_permute_complex128_tensor_rank5_5_1_4_2_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3,5,7,11) :: src
        complex(real64), dimension(11,2,7,3,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7,11) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex128_tensor_rank5_5_1_4_2_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [5, 1, 4, 2, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex128_tensor_rank5_5_1_4_2_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex128_tensor_rank5_5_1_4_2_3

    subroutine permute_complex128_tensor_rank5_5_1_4_2_3(dst, src)
        complex(real64), dimension(:,:,:,:,:), intent(inout) :: dst
        complex(real64), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i5,i1,i4,i2,i3) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_complex128_tensor_rank5_5_1_4_2_3

    subroutine test_permute_complex64_matrix_1_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3) :: src
        complex(real32), dimension(2,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_matrix_1_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_matrix_1_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_matrix_1_2

    subroutine permute_complex64_matrix_1_2(dst, src)
        complex(real32), dimension(:,:), intent(inout) :: dst
        complex(real32), dimension(:,:), intent(in) :: src

        integer :: i1, i2

        do i2 = 1, size(src, 2)
            do i1 = 1, size(src, 1)
                dst(i1,i2) = src(i1,i2)
            end do
        end do
    end subroutine permute_complex64_matrix_1_2

    subroutine test_permute_complex64_matrix_2_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3) :: src
        complex(real32), dimension(3,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_matrix_2_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_matrix_2_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_matrix_2_1

    subroutine permute_complex64_matrix_2_1(dst, src)
        complex(real32), dimension(:,:), intent(inout) :: dst
        complex(real32), dimension(:,:), intent(in) :: src

        integer :: i1, i2

        do i2 = 1, size(src, 2)
            do i1 = 1, size(src, 1)
                dst(i2,i1) = src(i1,i2)
            end do
        end do
    end subroutine permute_complex64_matrix_2_1

    subroutine test_permute_complex64_tensor_rank3_1_2_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5) :: src
        complex(real32), dimension(2,3,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank3_1_2_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 2, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank3_1_2_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank3_1_2_3

    subroutine permute_complex64_tensor_rank3_1_2_3(dst, src)
        complex(real32), dimension(:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:), intent(in) :: src

        integer :: i1, i2, i3

        do i3 = 1, size(src, 3)
            do i2 = 1, size(src, 2)
                do i1 = 1, size(src, 1)
                    dst(i1,i2,i3) = src(i1,i2,i3)
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank3_1_2_3

    subroutine test_permute_complex64_tensor_rank3_1_3_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5) :: src
        complex(real32), dimension(2,5,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank3_1_3_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 3, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank3_1_3_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank3_1_3_2

    subroutine permute_complex64_tensor_rank3_1_3_2(dst, src)
        complex(real32), dimension(:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:), intent(in) :: src

        integer :: i1, i2, i3

        do i3 = 1, size(src, 3)
            do i2 = 1, size(src, 2)
                do i1 = 1, size(src, 1)
                    dst(i1,i3,i2) = src(i1,i2,i3)
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank3_1_3_2

    subroutine test_permute_complex64_tensor_rank3_2_1_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5) :: src
        complex(real32), dimension(3,2,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank3_2_1_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 1, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank3_2_1_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank3_2_1_3

    subroutine permute_complex64_tensor_rank3_2_1_3(dst, src)
        complex(real32), dimension(:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:), intent(in) :: src

        integer :: i1, i2, i3

        do i3 = 1, size(src, 3)
            do i2 = 1, size(src, 2)
                do i1 = 1, size(src, 1)
                    dst(i2,i1,i3) = src(i1,i2,i3)
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank3_2_1_3

    subroutine test_permute_complex64_tensor_rank3_2_3_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5) :: src
        complex(real32), dimension(3,5,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank3_2_3_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 3, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank3_2_3_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank3_2_3_1

    subroutine permute_complex64_tensor_rank3_2_3_1(dst, src)
        complex(real32), dimension(:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:), intent(in) :: src

        integer :: i1, i2, i3

        do i3 = 1, size(src, 3)
            do i2 = 1, size(src, 2)
                do i1 = 1, size(src, 1)
                    dst(i2,i3,i1) = src(i1,i2,i3)
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank3_2_3_1

    subroutine test_permute_complex64_tensor_rank3_3_2_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5) :: src
        complex(real32), dimension(5,3,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank3_3_2_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 2, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank3_3_2_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank3_3_2_1

    subroutine permute_complex64_tensor_rank3_3_2_1(dst, src)
        complex(real32), dimension(:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:), intent(in) :: src

        integer :: i1, i2, i3

        do i3 = 1, size(src, 3)
            do i2 = 1, size(src, 2)
                do i1 = 1, size(src, 1)
                    dst(i3,i2,i1) = src(i1,i2,i3)
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank3_3_2_1

    subroutine test_permute_complex64_tensor_rank3_3_1_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5) :: src
        complex(real32), dimension(5,2,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank3_3_1_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 1, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank3_3_1_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank3_3_1_2

    subroutine permute_complex64_tensor_rank3_3_1_2(dst, src)
        complex(real32), dimension(:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:), intent(in) :: src

        integer :: i1, i2, i3

        do i3 = 1, size(src, 3)
            do i2 = 1, size(src, 2)
                do i1 = 1, size(src, 1)
                    dst(i3,i1,i2) = src(i1,i2,i3)
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank3_3_1_2

    subroutine test_permute_complex64_tensor_rank4_1_2_3_4(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7) :: src
        complex(real32), dimension(2,3,5,7) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank4_1_2_3_4(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 2, 3, 4], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank4_1_2_3_4:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank4_1_2_3_4

    subroutine permute_complex64_tensor_rank4_1_2_3_4(dst, src)
        complex(real32), dimension(:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i1,i2,i3,i4) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank4_1_2_3_4

    subroutine test_permute_complex64_tensor_rank4_1_2_4_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7) :: src
        complex(real32), dimension(2,3,7,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank4_1_2_4_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 2, 4, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank4_1_2_4_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank4_1_2_4_3

    subroutine permute_complex64_tensor_rank4_1_2_4_3(dst, src)
        complex(real32), dimension(:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i1,i2,i4,i3) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank4_1_2_4_3

    subroutine test_permute_complex64_tensor_rank4_1_3_2_4(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7) :: src
        complex(real32), dimension(2,5,3,7) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank4_1_3_2_4(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 3, 2, 4], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank4_1_3_2_4:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank4_1_3_2_4

    subroutine permute_complex64_tensor_rank4_1_3_2_4(dst, src)
        complex(real32), dimension(:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i1,i3,i2,i4) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank4_1_3_2_4

    subroutine test_permute_complex64_tensor_rank4_1_3_4_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7) :: src
        complex(real32), dimension(2,5,7,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank4_1_3_4_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 3, 4, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank4_1_3_4_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank4_1_3_4_2

    subroutine permute_complex64_tensor_rank4_1_3_4_2(dst, src)
        complex(real32), dimension(:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i1,i3,i4,i2) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank4_1_3_4_2

    subroutine test_permute_complex64_tensor_rank4_1_4_3_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7) :: src
        complex(real32), dimension(2,7,5,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank4_1_4_3_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 4, 3, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank4_1_4_3_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank4_1_4_3_2

    subroutine permute_complex64_tensor_rank4_1_4_3_2(dst, src)
        complex(real32), dimension(:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i1,i4,i3,i2) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank4_1_4_3_2

    subroutine test_permute_complex64_tensor_rank4_1_4_2_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7) :: src
        complex(real32), dimension(2,7,3,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank4_1_4_2_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 4, 2, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank4_1_4_2_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank4_1_4_2_3

    subroutine permute_complex64_tensor_rank4_1_4_2_3(dst, src)
        complex(real32), dimension(:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i1,i4,i2,i3) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank4_1_4_2_3

    subroutine test_permute_complex64_tensor_rank4_2_1_3_4(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7) :: src
        complex(real32), dimension(3,2,5,7) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank4_2_1_3_4(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 1, 3, 4], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank4_2_1_3_4:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank4_2_1_3_4

    subroutine permute_complex64_tensor_rank4_2_1_3_4(dst, src)
        complex(real32), dimension(:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i2,i1,i3,i4) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank4_2_1_3_4

    subroutine test_permute_complex64_tensor_rank4_2_1_4_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7) :: src
        complex(real32), dimension(3,2,7,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank4_2_1_4_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 1, 4, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank4_2_1_4_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank4_2_1_4_3

    subroutine permute_complex64_tensor_rank4_2_1_4_3(dst, src)
        complex(real32), dimension(:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i2,i1,i4,i3) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank4_2_1_4_3

    subroutine test_permute_complex64_tensor_rank4_2_3_1_4(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7) :: src
        complex(real32), dimension(3,5,2,7) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank4_2_3_1_4(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 3, 1, 4], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank4_2_3_1_4:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank4_2_3_1_4

    subroutine permute_complex64_tensor_rank4_2_3_1_4(dst, src)
        complex(real32), dimension(:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i2,i3,i1,i4) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank4_2_3_1_4

    subroutine test_permute_complex64_tensor_rank4_2_3_4_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7) :: src
        complex(real32), dimension(3,5,7,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank4_2_3_4_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 3, 4, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank4_2_3_4_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank4_2_3_4_1

    subroutine permute_complex64_tensor_rank4_2_3_4_1(dst, src)
        complex(real32), dimension(:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i2,i3,i4,i1) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank4_2_3_4_1

    subroutine test_permute_complex64_tensor_rank4_2_4_3_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7) :: src
        complex(real32), dimension(3,7,5,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank4_2_4_3_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 4, 3, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank4_2_4_3_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank4_2_4_3_1

    subroutine permute_complex64_tensor_rank4_2_4_3_1(dst, src)
        complex(real32), dimension(:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i2,i4,i3,i1) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank4_2_4_3_1

    subroutine test_permute_complex64_tensor_rank4_2_4_1_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7) :: src
        complex(real32), dimension(3,7,2,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank4_2_4_1_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 4, 1, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank4_2_4_1_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank4_2_4_1_3

    subroutine permute_complex64_tensor_rank4_2_4_1_3(dst, src)
        complex(real32), dimension(:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i2,i4,i1,i3) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank4_2_4_1_3

    subroutine test_permute_complex64_tensor_rank4_3_2_1_4(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7) :: src
        complex(real32), dimension(5,3,2,7) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank4_3_2_1_4(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 2, 1, 4], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank4_3_2_1_4:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank4_3_2_1_4

    subroutine permute_complex64_tensor_rank4_3_2_1_4(dst, src)
        complex(real32), dimension(:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i3,i2,i1,i4) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank4_3_2_1_4

    subroutine test_permute_complex64_tensor_rank4_3_2_4_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7) :: src
        complex(real32), dimension(5,3,7,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank4_3_2_4_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 2, 4, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank4_3_2_4_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank4_3_2_4_1

    subroutine permute_complex64_tensor_rank4_3_2_4_1(dst, src)
        complex(real32), dimension(:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i3,i2,i4,i1) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank4_3_2_4_1

    subroutine test_permute_complex64_tensor_rank4_3_1_2_4(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7) :: src
        complex(real32), dimension(5,2,3,7) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank4_3_1_2_4(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 1, 2, 4], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank4_3_1_2_4:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank4_3_1_2_4

    subroutine permute_complex64_tensor_rank4_3_1_2_4(dst, src)
        complex(real32), dimension(:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i3,i1,i2,i4) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank4_3_1_2_4

    subroutine test_permute_complex64_tensor_rank4_3_1_4_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7) :: src
        complex(real32), dimension(5,2,7,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank4_3_1_4_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 1, 4, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank4_3_1_4_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank4_3_1_4_2

    subroutine permute_complex64_tensor_rank4_3_1_4_2(dst, src)
        complex(real32), dimension(:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i3,i1,i4,i2) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank4_3_1_4_2

    subroutine test_permute_complex64_tensor_rank4_3_4_1_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7) :: src
        complex(real32), dimension(5,7,2,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank4_3_4_1_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 4, 1, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank4_3_4_1_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank4_3_4_1_2

    subroutine permute_complex64_tensor_rank4_3_4_1_2(dst, src)
        complex(real32), dimension(:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i3,i4,i1,i2) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank4_3_4_1_2

    subroutine test_permute_complex64_tensor_rank4_3_4_2_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7) :: src
        complex(real32), dimension(5,7,3,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank4_3_4_2_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 4, 2, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank4_3_4_2_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank4_3_4_2_1

    subroutine permute_complex64_tensor_rank4_3_4_2_1(dst, src)
        complex(real32), dimension(:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i3,i4,i2,i1) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank4_3_4_2_1

    subroutine test_permute_complex64_tensor_rank4_4_2_3_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7) :: src
        complex(real32), dimension(7,3,5,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank4_4_2_3_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [4, 2, 3, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank4_4_2_3_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank4_4_2_3_1

    subroutine permute_complex64_tensor_rank4_4_2_3_1(dst, src)
        complex(real32), dimension(:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i4,i2,i3,i1) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank4_4_2_3_1

    subroutine test_permute_complex64_tensor_rank4_4_2_1_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7) :: src
        complex(real32), dimension(7,3,2,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank4_4_2_1_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [4, 2, 1, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank4_4_2_1_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank4_4_2_1_3

    subroutine permute_complex64_tensor_rank4_4_2_1_3(dst, src)
        complex(real32), dimension(:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i4,i2,i1,i3) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank4_4_2_1_3

    subroutine test_permute_complex64_tensor_rank4_4_3_2_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7) :: src
        complex(real32), dimension(7,5,3,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank4_4_3_2_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [4, 3, 2, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank4_4_3_2_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank4_4_3_2_1

    subroutine permute_complex64_tensor_rank4_4_3_2_1(dst, src)
        complex(real32), dimension(:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i4,i3,i2,i1) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank4_4_3_2_1

    subroutine test_permute_complex64_tensor_rank4_4_3_1_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7) :: src
        complex(real32), dimension(7,5,2,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank4_4_3_1_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [4, 3, 1, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank4_4_3_1_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank4_4_3_1_2

    subroutine permute_complex64_tensor_rank4_4_3_1_2(dst, src)
        complex(real32), dimension(:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i4,i3,i1,i2) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank4_4_3_1_2

    subroutine test_permute_complex64_tensor_rank4_4_1_3_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7) :: src
        complex(real32), dimension(7,2,5,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank4_4_1_3_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [4, 1, 3, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank4_4_1_3_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank4_4_1_3_2

    subroutine permute_complex64_tensor_rank4_4_1_3_2(dst, src)
        complex(real32), dimension(:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i4,i1,i3,i2) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank4_4_1_3_2

    subroutine test_permute_complex64_tensor_rank4_4_1_2_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7) :: src
        complex(real32), dimension(7,2,3,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank4_4_1_2_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [4, 1, 2, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank4_4_1_2_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank4_4_1_2_3

    subroutine permute_complex64_tensor_rank4_4_1_2_3(dst, src)
        complex(real32), dimension(:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i4,i1,i2,i3) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank4_4_1_2_3

    subroutine test_permute_complex64_tensor_rank5_1_2_3_4_5(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7,11) :: src
        complex(real32), dimension(2,3,5,7,11) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7,11) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank5_1_2_3_4_5(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 2, 3, 4, 5], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank5_1_2_3_4_5:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank5_1_2_3_4_5

    subroutine permute_complex64_tensor_rank5_1_2_3_4_5(dst, src)
        complex(real32), dimension(:,:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i1,i2,i3,i4,i5) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank5_1_2_3_4_5

    subroutine test_permute_complex64_tensor_rank5_1_3_4_5_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7,11) :: src
        complex(real32), dimension(2,5,7,11,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7,11) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank5_1_3_4_5_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 3, 4, 5, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank5_1_3_4_5_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank5_1_3_4_5_2

    subroutine permute_complex64_tensor_rank5_1_3_4_5_2(dst, src)
        complex(real32), dimension(:,:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i1,i3,i4,i5,i2) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank5_1_3_4_5_2

    subroutine test_permute_complex64_tensor_rank5_1_5_3_4_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7,11) :: src
        complex(real32), dimension(2,11,5,7,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7,11) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank5_1_5_3_4_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 5, 3, 4, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank5_1_5_3_4_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank5_1_5_3_4_2

    subroutine permute_complex64_tensor_rank5_1_5_3_4_2(dst, src)
        complex(real32), dimension(:,:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i1,i5,i3,i4,i2) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank5_1_5_3_4_2

    subroutine test_permute_complex64_tensor_rank5_2_1_4_5_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7,11) :: src
        complex(real32), dimension(3,2,7,11,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7,11) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank5_2_1_4_5_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 1, 4, 5, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank5_2_1_4_5_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank5_2_1_4_5_3

    subroutine permute_complex64_tensor_rank5_2_1_4_5_3(dst, src)
        complex(real32), dimension(:,:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i2,i1,i4,i5,i3) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank5_2_1_4_5_3

    subroutine test_permute_complex64_tensor_rank5_2_4_3_1_5(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7,11) :: src
        complex(real32), dimension(3,7,5,2,11) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7,11) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank5_2_4_3_1_5(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 4, 3, 1, 5], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank5_2_4_3_1_5:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank5_2_4_3_1_5

    subroutine permute_complex64_tensor_rank5_2_4_3_1_5(dst, src)
        complex(real32), dimension(:,:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i2,i4,i3,i1,i5) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank5_2_4_3_1_5

    subroutine test_permute_complex64_tensor_rank5_2_5_4_1_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7,11) :: src
        complex(real32), dimension(3,11,7,2,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7,11) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank5_2_5_4_1_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 5, 4, 1, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank5_2_5_4_1_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank5_2_5_4_1_3

    subroutine permute_complex64_tensor_rank5_2_5_4_1_3(dst, src)
        complex(real32), dimension(:,:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i2,i5,i4,i1,i3) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank5_2_5_4_1_3

    subroutine test_permute_complex64_tensor_rank5_3_1_2_4_5(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7,11) :: src
        complex(real32), dimension(5,2,3,7,11) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7,11) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank5_3_1_2_4_5(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 1, 2, 4, 5], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank5_3_1_2_4_5:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank5_3_1_2_4_5

    subroutine permute_complex64_tensor_rank5_3_1_2_4_5(dst, src)
        complex(real32), dimension(:,:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i3,i1,i2,i4,i5) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank5_3_1_2_4_5

    subroutine test_permute_complex64_tensor_rank5_3_4_2_5_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7,11) :: src
        complex(real32), dimension(5,7,3,11,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7,11) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank5_3_4_2_5_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 4, 2, 5, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank5_3_4_2_5_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank5_3_4_2_5_1

    subroutine permute_complex64_tensor_rank5_3_4_2_5_1(dst, src)
        complex(real32), dimension(:,:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i3,i4,i2,i5,i1) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank5_3_4_2_5_1

    subroutine test_permute_complex64_tensor_rank5_4_2_3_1_5(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7,11) :: src
        complex(real32), dimension(7,3,5,2,11) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7,11) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank5_4_2_3_1_5(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [4, 2, 3, 1, 5], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank5_4_2_3_1_5:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank5_4_2_3_1_5

    subroutine permute_complex64_tensor_rank5_4_2_3_1_5(dst, src)
        complex(real32), dimension(:,:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i4,i2,i3,i1,i5) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank5_4_2_3_1_5

    subroutine test_permute_complex64_tensor_rank5_4_3_1_5_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7,11) :: src
        complex(real32), dimension(7,5,2,11,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7,11) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank5_4_3_1_5_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [4, 3, 1, 5, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank5_4_3_1_5_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank5_4_3_1_5_2

    subroutine permute_complex64_tensor_rank5_4_3_1_5_2(dst, src)
        complex(real32), dimension(:,:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i4,i3,i1,i5,i2) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank5_4_3_1_5_2

    subroutine test_permute_complex64_tensor_rank5_4_5_3_1_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7,11) :: src
        complex(real32), dimension(7,11,5,2,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7,11) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank5_4_5_3_1_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [4, 5, 3, 1, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank5_4_5_3_1_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank5_4_5_3_1_2

    subroutine permute_complex64_tensor_rank5_4_5_3_1_2(dst, src)
        complex(real32), dimension(:,:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i4,i5,i3,i1,i2) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank5_4_5_3_1_2

    subroutine test_permute_complex64_tensor_rank5_5_2_4_1_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7,11) :: src
        complex(real32), dimension(11,3,7,2,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7,11) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank5_5_2_4_1_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [5, 2, 4, 1, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank5_5_2_4_1_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank5_5_2_4_1_3

    subroutine permute_complex64_tensor_rank5_5_2_4_1_3(dst, src)
        complex(real32), dimension(:,:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i5,i2,i4,i1,i3) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank5_5_2_4_1_3

    subroutine test_permute_complex64_tensor_rank5_5_4_3_2_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7,11) :: src
        complex(real32), dimension(11,7,5,3,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7,11) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank5_5_4_3_2_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [5, 4, 3, 2, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank5_5_4_3_2_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank5_5_4_3_2_1

    subroutine permute_complex64_tensor_rank5_5_4_3_2_1(dst, src)
        complex(real32), dimension(:,:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i5,i4,i3,i2,i1) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank5_5_4_3_2_1

    subroutine test_permute_complex64_tensor_rank5_5_1_4_2_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3,5,7,11) :: src
        complex(real32), dimension(11,2,7,3,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        real, dimension(2,3,5,7,11) :: real_array, imag_array
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(real_array)
        call random_number(imag_array)

        src = cmplx(real_array, imag_array)
        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_complex64_tensor_rank5_5_1_4_2_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [5, 1, 4, 2, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_complex64_tensor_rank5_5_1_4_2_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_complex64_tensor_rank5_5_1_4_2_3

    subroutine permute_complex64_tensor_rank5_5_1_4_2_3(dst, src)
        complex(real32), dimension(:,:,:,:,:), intent(inout) :: dst
        complex(real32), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i5,i1,i4,i2,i3) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_complex64_tensor_rank5_5_1_4_2_3

    subroutine test_permute_real64_matrix_1_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3) :: src
        real(real64), dimension(2,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_matrix_1_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_matrix_1_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_matrix_1_2

    subroutine permute_real64_matrix_1_2(dst, src)
        real(real64), dimension(:,:), intent(inout) :: dst
        real(real64), dimension(:,:), intent(in) :: src

        integer :: i1, i2

        do i2 = 1, size(src, 2)
            do i1 = 1, size(src, 1)
                dst(i1,i2) = src(i1,i2)
            end do
        end do
    end subroutine permute_real64_matrix_1_2

    subroutine test_permute_real64_matrix_2_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3) :: src
        real(real64), dimension(3,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_matrix_2_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_matrix_2_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_matrix_2_1

    subroutine permute_real64_matrix_2_1(dst, src)
        real(real64), dimension(:,:), intent(inout) :: dst
        real(real64), dimension(:,:), intent(in) :: src

        integer :: i1, i2

        do i2 = 1, size(src, 2)
            do i1 = 1, size(src, 1)
                dst(i2,i1) = src(i1,i2)
            end do
        end do
    end subroutine permute_real64_matrix_2_1

    subroutine test_permute_real64_tensor_rank3_1_2_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5) :: src
        real(real64), dimension(2,3,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank3_1_2_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 2, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank3_1_2_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank3_1_2_3

    subroutine permute_real64_tensor_rank3_1_2_3(dst, src)
        real(real64), dimension(:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:), intent(in) :: src

        integer :: i1, i2, i3

        do i3 = 1, size(src, 3)
            do i2 = 1, size(src, 2)
                do i1 = 1, size(src, 1)
                    dst(i1,i2,i3) = src(i1,i2,i3)
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank3_1_2_3

    subroutine test_permute_real64_tensor_rank3_1_3_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5) :: src
        real(real64), dimension(2,5,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank3_1_3_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 3, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank3_1_3_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank3_1_3_2

    subroutine permute_real64_tensor_rank3_1_3_2(dst, src)
        real(real64), dimension(:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:), intent(in) :: src

        integer :: i1, i2, i3

        do i3 = 1, size(src, 3)
            do i2 = 1, size(src, 2)
                do i1 = 1, size(src, 1)
                    dst(i1,i3,i2) = src(i1,i2,i3)
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank3_1_3_2

    subroutine test_permute_real64_tensor_rank3_2_1_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5) :: src
        real(real64), dimension(3,2,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank3_2_1_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 1, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank3_2_1_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank3_2_1_3

    subroutine permute_real64_tensor_rank3_2_1_3(dst, src)
        real(real64), dimension(:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:), intent(in) :: src

        integer :: i1, i2, i3

        do i3 = 1, size(src, 3)
            do i2 = 1, size(src, 2)
                do i1 = 1, size(src, 1)
                    dst(i2,i1,i3) = src(i1,i2,i3)
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank3_2_1_3

    subroutine test_permute_real64_tensor_rank3_2_3_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5) :: src
        real(real64), dimension(3,5,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank3_2_3_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 3, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank3_2_3_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank3_2_3_1

    subroutine permute_real64_tensor_rank3_2_3_1(dst, src)
        real(real64), dimension(:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:), intent(in) :: src

        integer :: i1, i2, i3

        do i3 = 1, size(src, 3)
            do i2 = 1, size(src, 2)
                do i1 = 1, size(src, 1)
                    dst(i2,i3,i1) = src(i1,i2,i3)
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank3_2_3_1

    subroutine test_permute_real64_tensor_rank3_3_2_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5) :: src
        real(real64), dimension(5,3,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank3_3_2_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 2, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank3_3_2_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank3_3_2_1

    subroutine permute_real64_tensor_rank3_3_2_1(dst, src)
        real(real64), dimension(:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:), intent(in) :: src

        integer :: i1, i2, i3

        do i3 = 1, size(src, 3)
            do i2 = 1, size(src, 2)
                do i1 = 1, size(src, 1)
                    dst(i3,i2,i1) = src(i1,i2,i3)
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank3_3_2_1

    subroutine test_permute_real64_tensor_rank3_3_1_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5) :: src
        real(real64), dimension(5,2,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank3_3_1_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 1, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank3_3_1_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank3_3_1_2

    subroutine permute_real64_tensor_rank3_3_1_2(dst, src)
        real(real64), dimension(:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:), intent(in) :: src

        integer :: i1, i2, i3

        do i3 = 1, size(src, 3)
            do i2 = 1, size(src, 2)
                do i1 = 1, size(src, 1)
                    dst(i3,i1,i2) = src(i1,i2,i3)
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank3_3_1_2

    subroutine test_permute_real64_tensor_rank4_1_2_3_4(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7) :: src
        real(real64), dimension(2,3,5,7) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank4_1_2_3_4(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 2, 3, 4], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank4_1_2_3_4:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank4_1_2_3_4

    subroutine permute_real64_tensor_rank4_1_2_3_4(dst, src)
        real(real64), dimension(:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i1,i2,i3,i4) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank4_1_2_3_4

    subroutine test_permute_real64_tensor_rank4_1_2_4_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7) :: src
        real(real64), dimension(2,3,7,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank4_1_2_4_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 2, 4, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank4_1_2_4_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank4_1_2_4_3

    subroutine permute_real64_tensor_rank4_1_2_4_3(dst, src)
        real(real64), dimension(:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i1,i2,i4,i3) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank4_1_2_4_3

    subroutine test_permute_real64_tensor_rank4_1_3_2_4(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7) :: src
        real(real64), dimension(2,5,3,7) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank4_1_3_2_4(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 3, 2, 4], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank4_1_3_2_4:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank4_1_3_2_4

    subroutine permute_real64_tensor_rank4_1_3_2_4(dst, src)
        real(real64), dimension(:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i1,i3,i2,i4) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank4_1_3_2_4

    subroutine test_permute_real64_tensor_rank4_1_3_4_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7) :: src
        real(real64), dimension(2,5,7,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank4_1_3_4_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 3, 4, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank4_1_3_4_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank4_1_3_4_2

    subroutine permute_real64_tensor_rank4_1_3_4_2(dst, src)
        real(real64), dimension(:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i1,i3,i4,i2) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank4_1_3_4_2

    subroutine test_permute_real64_tensor_rank4_1_4_3_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7) :: src
        real(real64), dimension(2,7,5,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank4_1_4_3_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 4, 3, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank4_1_4_3_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank4_1_4_3_2

    subroutine permute_real64_tensor_rank4_1_4_3_2(dst, src)
        real(real64), dimension(:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i1,i4,i3,i2) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank4_1_4_3_2

    subroutine test_permute_real64_tensor_rank4_1_4_2_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7) :: src
        real(real64), dimension(2,7,3,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank4_1_4_2_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 4, 2, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank4_1_4_2_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank4_1_4_2_3

    subroutine permute_real64_tensor_rank4_1_4_2_3(dst, src)
        real(real64), dimension(:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i1,i4,i2,i3) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank4_1_4_2_3

    subroutine test_permute_real64_tensor_rank4_2_1_3_4(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7) :: src
        real(real64), dimension(3,2,5,7) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank4_2_1_3_4(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 1, 3, 4], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank4_2_1_3_4:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank4_2_1_3_4

    subroutine permute_real64_tensor_rank4_2_1_3_4(dst, src)
        real(real64), dimension(:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i2,i1,i3,i4) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank4_2_1_3_4

    subroutine test_permute_real64_tensor_rank4_2_1_4_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7) :: src
        real(real64), dimension(3,2,7,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank4_2_1_4_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 1, 4, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank4_2_1_4_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank4_2_1_4_3

    subroutine permute_real64_tensor_rank4_2_1_4_3(dst, src)
        real(real64), dimension(:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i2,i1,i4,i3) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank4_2_1_4_3

    subroutine test_permute_real64_tensor_rank4_2_3_1_4(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7) :: src
        real(real64), dimension(3,5,2,7) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank4_2_3_1_4(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 3, 1, 4], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank4_2_3_1_4:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank4_2_3_1_4

    subroutine permute_real64_tensor_rank4_2_3_1_4(dst, src)
        real(real64), dimension(:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i2,i3,i1,i4) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank4_2_3_1_4

    subroutine test_permute_real64_tensor_rank4_2_3_4_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7) :: src
        real(real64), dimension(3,5,7,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank4_2_3_4_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 3, 4, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank4_2_3_4_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank4_2_3_4_1

    subroutine permute_real64_tensor_rank4_2_3_4_1(dst, src)
        real(real64), dimension(:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i2,i3,i4,i1) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank4_2_3_4_1

    subroutine test_permute_real64_tensor_rank4_2_4_3_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7) :: src
        real(real64), dimension(3,7,5,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank4_2_4_3_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 4, 3, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank4_2_4_3_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank4_2_4_3_1

    subroutine permute_real64_tensor_rank4_2_4_3_1(dst, src)
        real(real64), dimension(:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i2,i4,i3,i1) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank4_2_4_3_1

    subroutine test_permute_real64_tensor_rank4_2_4_1_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7) :: src
        real(real64), dimension(3,7,2,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank4_2_4_1_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 4, 1, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank4_2_4_1_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank4_2_4_1_3

    subroutine permute_real64_tensor_rank4_2_4_1_3(dst, src)
        real(real64), dimension(:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i2,i4,i1,i3) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank4_2_4_1_3

    subroutine test_permute_real64_tensor_rank4_3_2_1_4(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7) :: src
        real(real64), dimension(5,3,2,7) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank4_3_2_1_4(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 2, 1, 4], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank4_3_2_1_4:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank4_3_2_1_4

    subroutine permute_real64_tensor_rank4_3_2_1_4(dst, src)
        real(real64), dimension(:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i3,i2,i1,i4) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank4_3_2_1_4

    subroutine test_permute_real64_tensor_rank4_3_2_4_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7) :: src
        real(real64), dimension(5,3,7,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank4_3_2_4_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 2, 4, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank4_3_2_4_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank4_3_2_4_1

    subroutine permute_real64_tensor_rank4_3_2_4_1(dst, src)
        real(real64), dimension(:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i3,i2,i4,i1) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank4_3_2_4_1

    subroutine test_permute_real64_tensor_rank4_3_1_2_4(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7) :: src
        real(real64), dimension(5,2,3,7) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank4_3_1_2_4(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 1, 2, 4], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank4_3_1_2_4:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank4_3_1_2_4

    subroutine permute_real64_tensor_rank4_3_1_2_4(dst, src)
        real(real64), dimension(:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i3,i1,i2,i4) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank4_3_1_2_4

    subroutine test_permute_real64_tensor_rank4_3_1_4_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7) :: src
        real(real64), dimension(5,2,7,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank4_3_1_4_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 1, 4, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank4_3_1_4_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank4_3_1_4_2

    subroutine permute_real64_tensor_rank4_3_1_4_2(dst, src)
        real(real64), dimension(:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i3,i1,i4,i2) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank4_3_1_4_2

    subroutine test_permute_real64_tensor_rank4_3_4_1_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7) :: src
        real(real64), dimension(5,7,2,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank4_3_4_1_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 4, 1, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank4_3_4_1_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank4_3_4_1_2

    subroutine permute_real64_tensor_rank4_3_4_1_2(dst, src)
        real(real64), dimension(:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i3,i4,i1,i2) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank4_3_4_1_2

    subroutine test_permute_real64_tensor_rank4_3_4_2_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7) :: src
        real(real64), dimension(5,7,3,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank4_3_4_2_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 4, 2, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank4_3_4_2_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank4_3_4_2_1

    subroutine permute_real64_tensor_rank4_3_4_2_1(dst, src)
        real(real64), dimension(:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i3,i4,i2,i1) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank4_3_4_2_1

    subroutine test_permute_real64_tensor_rank4_4_2_3_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7) :: src
        real(real64), dimension(7,3,5,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank4_4_2_3_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [4, 2, 3, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank4_4_2_3_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank4_4_2_3_1

    subroutine permute_real64_tensor_rank4_4_2_3_1(dst, src)
        real(real64), dimension(:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i4,i2,i3,i1) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank4_4_2_3_1

    subroutine test_permute_real64_tensor_rank4_4_2_1_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7) :: src
        real(real64), dimension(7,3,2,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank4_4_2_1_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [4, 2, 1, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank4_4_2_1_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank4_4_2_1_3

    subroutine permute_real64_tensor_rank4_4_2_1_3(dst, src)
        real(real64), dimension(:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i4,i2,i1,i3) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank4_4_2_1_3

    subroutine test_permute_real64_tensor_rank4_4_3_2_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7) :: src
        real(real64), dimension(7,5,3,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank4_4_3_2_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [4, 3, 2, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank4_4_3_2_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank4_4_3_2_1

    subroutine permute_real64_tensor_rank4_4_3_2_1(dst, src)
        real(real64), dimension(:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i4,i3,i2,i1) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank4_4_3_2_1

    subroutine test_permute_real64_tensor_rank4_4_3_1_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7) :: src
        real(real64), dimension(7,5,2,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank4_4_3_1_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [4, 3, 1, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank4_4_3_1_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank4_4_3_1_2

    subroutine permute_real64_tensor_rank4_4_3_1_2(dst, src)
        real(real64), dimension(:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i4,i3,i1,i2) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank4_4_3_1_2

    subroutine test_permute_real64_tensor_rank4_4_1_3_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7) :: src
        real(real64), dimension(7,2,5,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank4_4_1_3_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [4, 1, 3, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank4_4_1_3_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank4_4_1_3_2

    subroutine permute_real64_tensor_rank4_4_1_3_2(dst, src)
        real(real64), dimension(:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i4,i1,i3,i2) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank4_4_1_3_2

    subroutine test_permute_real64_tensor_rank4_4_1_2_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7) :: src
        real(real64), dimension(7,2,3,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank4_4_1_2_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [4, 1, 2, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank4_4_1_2_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank4_4_1_2_3

    subroutine permute_real64_tensor_rank4_4_1_2_3(dst, src)
        real(real64), dimension(:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i4,i1,i2,i3) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank4_4_1_2_3

    subroutine test_permute_real64_tensor_rank5_1_2_3_4_5(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7,11) :: src
        real(real64), dimension(2,3,5,7,11) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank5_1_2_3_4_5(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 2, 3, 4, 5], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank5_1_2_3_4_5:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank5_1_2_3_4_5

    subroutine permute_real64_tensor_rank5_1_2_3_4_5(dst, src)
        real(real64), dimension(:,:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i1,i2,i3,i4,i5) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank5_1_2_3_4_5

    subroutine test_permute_real64_tensor_rank5_1_3_4_5_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7,11) :: src
        real(real64), dimension(2,5,7,11,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank5_1_3_4_5_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 3, 4, 5, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank5_1_3_4_5_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank5_1_3_4_5_2

    subroutine permute_real64_tensor_rank5_1_3_4_5_2(dst, src)
        real(real64), dimension(:,:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i1,i3,i4,i5,i2) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank5_1_3_4_5_2

    subroutine test_permute_real64_tensor_rank5_1_5_3_4_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7,11) :: src
        real(real64), dimension(2,11,5,7,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank5_1_5_3_4_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 5, 3, 4, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank5_1_5_3_4_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank5_1_5_3_4_2

    subroutine permute_real64_tensor_rank5_1_5_3_4_2(dst, src)
        real(real64), dimension(:,:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i1,i5,i3,i4,i2) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank5_1_5_3_4_2

    subroutine test_permute_real64_tensor_rank5_2_1_4_5_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7,11) :: src
        real(real64), dimension(3,2,7,11,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank5_2_1_4_5_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 1, 4, 5, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank5_2_1_4_5_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank5_2_1_4_5_3

    subroutine permute_real64_tensor_rank5_2_1_4_5_3(dst, src)
        real(real64), dimension(:,:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i2,i1,i4,i5,i3) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank5_2_1_4_5_3

    subroutine test_permute_real64_tensor_rank5_2_4_3_1_5(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7,11) :: src
        real(real64), dimension(3,7,5,2,11) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank5_2_4_3_1_5(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 4, 3, 1, 5], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank5_2_4_3_1_5:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank5_2_4_3_1_5

    subroutine permute_real64_tensor_rank5_2_4_3_1_5(dst, src)
        real(real64), dimension(:,:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i2,i4,i3,i1,i5) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank5_2_4_3_1_5

    subroutine test_permute_real64_tensor_rank5_2_5_4_1_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7,11) :: src
        real(real64), dimension(3,11,7,2,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank5_2_5_4_1_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 5, 4, 1, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank5_2_5_4_1_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank5_2_5_4_1_3

    subroutine permute_real64_tensor_rank5_2_5_4_1_3(dst, src)
        real(real64), dimension(:,:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i2,i5,i4,i1,i3) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank5_2_5_4_1_3

    subroutine test_permute_real64_tensor_rank5_3_1_2_4_5(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7,11) :: src
        real(real64), dimension(5,2,3,7,11) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank5_3_1_2_4_5(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 1, 2, 4, 5], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank5_3_1_2_4_5:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank5_3_1_2_4_5

    subroutine permute_real64_tensor_rank5_3_1_2_4_5(dst, src)
        real(real64), dimension(:,:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i3,i1,i2,i4,i5) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank5_3_1_2_4_5

    subroutine test_permute_real64_tensor_rank5_3_4_2_5_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7,11) :: src
        real(real64), dimension(5,7,3,11,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank5_3_4_2_5_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 4, 2, 5, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank5_3_4_2_5_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank5_3_4_2_5_1

    subroutine permute_real64_tensor_rank5_3_4_2_5_1(dst, src)
        real(real64), dimension(:,:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i3,i4,i2,i5,i1) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank5_3_4_2_5_1

    subroutine test_permute_real64_tensor_rank5_4_2_3_1_5(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7,11) :: src
        real(real64), dimension(7,3,5,2,11) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank5_4_2_3_1_5(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [4, 2, 3, 1, 5], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank5_4_2_3_1_5:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank5_4_2_3_1_5

    subroutine permute_real64_tensor_rank5_4_2_3_1_5(dst, src)
        real(real64), dimension(:,:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i4,i2,i3,i1,i5) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank5_4_2_3_1_5

    subroutine test_permute_real64_tensor_rank5_4_3_1_5_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7,11) :: src
        real(real64), dimension(7,5,2,11,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank5_4_3_1_5_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [4, 3, 1, 5, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank5_4_3_1_5_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank5_4_3_1_5_2

    subroutine permute_real64_tensor_rank5_4_3_1_5_2(dst, src)
        real(real64), dimension(:,:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i4,i3,i1,i5,i2) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank5_4_3_1_5_2

    subroutine test_permute_real64_tensor_rank5_4_5_3_1_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7,11) :: src
        real(real64), dimension(7,11,5,2,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank5_4_5_3_1_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [4, 5, 3, 1, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank5_4_5_3_1_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank5_4_5_3_1_2

    subroutine permute_real64_tensor_rank5_4_5_3_1_2(dst, src)
        real(real64), dimension(:,:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i4,i5,i3,i1,i2) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank5_4_5_3_1_2

    subroutine test_permute_real64_tensor_rank5_5_2_4_1_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7,11) :: src
        real(real64), dimension(11,3,7,2,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank5_5_2_4_1_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [5, 2, 4, 1, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank5_5_2_4_1_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank5_5_2_4_1_3

    subroutine permute_real64_tensor_rank5_5_2_4_1_3(dst, src)
        real(real64), dimension(:,:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i5,i2,i4,i1,i3) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank5_5_2_4_1_3

    subroutine test_permute_real64_tensor_rank5_5_4_3_2_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7,11) :: src
        real(real64), dimension(11,7,5,3,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank5_5_4_3_2_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [5, 4, 3, 2, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank5_5_4_3_2_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank5_5_4_3_2_1

    subroutine permute_real64_tensor_rank5_5_4_3_2_1(dst, src)
        real(real64), dimension(:,:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i5,i4,i3,i2,i1) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank5_5_4_3_2_1

    subroutine test_permute_real64_tensor_rank5_5_1_4_2_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3,5,7,11) :: src
        real(real64), dimension(11,2,7,3,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real64_tensor_rank5_5_1_4_2_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [5, 1, 4, 2, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real64_tensor_rank5_5_1_4_2_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real64_tensor_rank5_5_1_4_2_3

    subroutine permute_real64_tensor_rank5_5_1_4_2_3(dst, src)
        real(real64), dimension(:,:,:,:,:), intent(inout) :: dst
        real(real64), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i5,i1,i4,i2,i3) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_real64_tensor_rank5_5_1_4_2_3

    subroutine test_permute_real32_matrix_1_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3) :: src
        real(real32), dimension(2,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_matrix_1_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_matrix_1_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_matrix_1_2

    subroutine permute_real32_matrix_1_2(dst, src)
        real(real32), dimension(:,:), intent(inout) :: dst
        real(real32), dimension(:,:), intent(in) :: src

        integer :: i1, i2

        do i2 = 1, size(src, 2)
            do i1 = 1, size(src, 1)
                dst(i1,i2) = src(i1,i2)
            end do
        end do
    end subroutine permute_real32_matrix_1_2

    subroutine test_permute_real32_matrix_2_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3) :: src
        real(real32), dimension(3,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_matrix_2_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_matrix_2_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_matrix_2_1

    subroutine permute_real32_matrix_2_1(dst, src)
        real(real32), dimension(:,:), intent(inout) :: dst
        real(real32), dimension(:,:), intent(in) :: src

        integer :: i1, i2

        do i2 = 1, size(src, 2)
            do i1 = 1, size(src, 1)
                dst(i2,i1) = src(i1,i2)
            end do
        end do
    end subroutine permute_real32_matrix_2_1

    subroutine test_permute_real32_tensor_rank3_1_2_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5) :: src
        real(real32), dimension(2,3,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank3_1_2_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 2, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank3_1_2_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank3_1_2_3

    subroutine permute_real32_tensor_rank3_1_2_3(dst, src)
        real(real32), dimension(:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:), intent(in) :: src

        integer :: i1, i2, i3

        do i3 = 1, size(src, 3)
            do i2 = 1, size(src, 2)
                do i1 = 1, size(src, 1)
                    dst(i1,i2,i3) = src(i1,i2,i3)
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank3_1_2_3

    subroutine test_permute_real32_tensor_rank3_1_3_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5) :: src
        real(real32), dimension(2,5,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank3_1_3_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 3, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank3_1_3_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank3_1_3_2

    subroutine permute_real32_tensor_rank3_1_3_2(dst, src)
        real(real32), dimension(:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:), intent(in) :: src

        integer :: i1, i2, i3

        do i3 = 1, size(src, 3)
            do i2 = 1, size(src, 2)
                do i1 = 1, size(src, 1)
                    dst(i1,i3,i2) = src(i1,i2,i3)
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank3_1_3_2

    subroutine test_permute_real32_tensor_rank3_2_1_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5) :: src
        real(real32), dimension(3,2,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank3_2_1_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 1, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank3_2_1_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank3_2_1_3

    subroutine permute_real32_tensor_rank3_2_1_3(dst, src)
        real(real32), dimension(:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:), intent(in) :: src

        integer :: i1, i2, i3

        do i3 = 1, size(src, 3)
            do i2 = 1, size(src, 2)
                do i1 = 1, size(src, 1)
                    dst(i2,i1,i3) = src(i1,i2,i3)
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank3_2_1_3

    subroutine test_permute_real32_tensor_rank3_2_3_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5) :: src
        real(real32), dimension(3,5,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank3_2_3_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 3, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank3_2_3_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank3_2_3_1

    subroutine permute_real32_tensor_rank3_2_3_1(dst, src)
        real(real32), dimension(:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:), intent(in) :: src

        integer :: i1, i2, i3

        do i3 = 1, size(src, 3)
            do i2 = 1, size(src, 2)
                do i1 = 1, size(src, 1)
                    dst(i2,i3,i1) = src(i1,i2,i3)
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank3_2_3_1

    subroutine test_permute_real32_tensor_rank3_3_2_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5) :: src
        real(real32), dimension(5,3,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank3_3_2_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 2, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank3_3_2_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank3_3_2_1

    subroutine permute_real32_tensor_rank3_3_2_1(dst, src)
        real(real32), dimension(:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:), intent(in) :: src

        integer :: i1, i2, i3

        do i3 = 1, size(src, 3)
            do i2 = 1, size(src, 2)
                do i1 = 1, size(src, 1)
                    dst(i3,i2,i1) = src(i1,i2,i3)
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank3_3_2_1

    subroutine test_permute_real32_tensor_rank3_3_1_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5) :: src
        real(real32), dimension(5,2,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank3_3_1_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 1, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank3_3_1_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank3_3_1_2

    subroutine permute_real32_tensor_rank3_3_1_2(dst, src)
        real(real32), dimension(:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:), intent(in) :: src

        integer :: i1, i2, i3

        do i3 = 1, size(src, 3)
            do i2 = 1, size(src, 2)
                do i1 = 1, size(src, 1)
                    dst(i3,i1,i2) = src(i1,i2,i3)
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank3_3_1_2

    subroutine test_permute_real32_tensor_rank4_1_2_3_4(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7) :: src
        real(real32), dimension(2,3,5,7) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank4_1_2_3_4(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 2, 3, 4], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank4_1_2_3_4:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank4_1_2_3_4

    subroutine permute_real32_tensor_rank4_1_2_3_4(dst, src)
        real(real32), dimension(:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i1,i2,i3,i4) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank4_1_2_3_4

    subroutine test_permute_real32_tensor_rank4_1_2_4_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7) :: src
        real(real32), dimension(2,3,7,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank4_1_2_4_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 2, 4, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank4_1_2_4_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank4_1_2_4_3

    subroutine permute_real32_tensor_rank4_1_2_4_3(dst, src)
        real(real32), dimension(:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i1,i2,i4,i3) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank4_1_2_4_3

    subroutine test_permute_real32_tensor_rank4_1_3_2_4(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7) :: src
        real(real32), dimension(2,5,3,7) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank4_1_3_2_4(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 3, 2, 4], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank4_1_3_2_4:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank4_1_3_2_4

    subroutine permute_real32_tensor_rank4_1_3_2_4(dst, src)
        real(real32), dimension(:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i1,i3,i2,i4) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank4_1_3_2_4

    subroutine test_permute_real32_tensor_rank4_1_3_4_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7) :: src
        real(real32), dimension(2,5,7,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank4_1_3_4_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 3, 4, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank4_1_3_4_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank4_1_3_4_2

    subroutine permute_real32_tensor_rank4_1_3_4_2(dst, src)
        real(real32), dimension(:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i1,i3,i4,i2) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank4_1_3_4_2

    subroutine test_permute_real32_tensor_rank4_1_4_3_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7) :: src
        real(real32), dimension(2,7,5,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank4_1_4_3_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 4, 3, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank4_1_4_3_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank4_1_4_3_2

    subroutine permute_real32_tensor_rank4_1_4_3_2(dst, src)
        real(real32), dimension(:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i1,i4,i3,i2) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank4_1_4_3_2

    subroutine test_permute_real32_tensor_rank4_1_4_2_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7) :: src
        real(real32), dimension(2,7,3,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank4_1_4_2_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 4, 2, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank4_1_4_2_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank4_1_4_2_3

    subroutine permute_real32_tensor_rank4_1_4_2_3(dst, src)
        real(real32), dimension(:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i1,i4,i2,i3) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank4_1_4_2_3

    subroutine test_permute_real32_tensor_rank4_2_1_3_4(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7) :: src
        real(real32), dimension(3,2,5,7) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank4_2_1_3_4(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 1, 3, 4], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank4_2_1_3_4:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank4_2_1_3_4

    subroutine permute_real32_tensor_rank4_2_1_3_4(dst, src)
        real(real32), dimension(:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i2,i1,i3,i4) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank4_2_1_3_4

    subroutine test_permute_real32_tensor_rank4_2_1_4_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7) :: src
        real(real32), dimension(3,2,7,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank4_2_1_4_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 1, 4, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank4_2_1_4_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank4_2_1_4_3

    subroutine permute_real32_tensor_rank4_2_1_4_3(dst, src)
        real(real32), dimension(:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i2,i1,i4,i3) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank4_2_1_4_3

    subroutine test_permute_real32_tensor_rank4_2_3_1_4(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7) :: src
        real(real32), dimension(3,5,2,7) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank4_2_3_1_4(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 3, 1, 4], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank4_2_3_1_4:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank4_2_3_1_4

    subroutine permute_real32_tensor_rank4_2_3_1_4(dst, src)
        real(real32), dimension(:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i2,i3,i1,i4) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank4_2_3_1_4

    subroutine test_permute_real32_tensor_rank4_2_3_4_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7) :: src
        real(real32), dimension(3,5,7,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank4_2_3_4_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 3, 4, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank4_2_3_4_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank4_2_3_4_1

    subroutine permute_real32_tensor_rank4_2_3_4_1(dst, src)
        real(real32), dimension(:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i2,i3,i4,i1) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank4_2_3_4_1

    subroutine test_permute_real32_tensor_rank4_2_4_3_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7) :: src
        real(real32), dimension(3,7,5,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank4_2_4_3_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 4, 3, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank4_2_4_3_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank4_2_4_3_1

    subroutine permute_real32_tensor_rank4_2_4_3_1(dst, src)
        real(real32), dimension(:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i2,i4,i3,i1) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank4_2_4_3_1

    subroutine test_permute_real32_tensor_rank4_2_4_1_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7) :: src
        real(real32), dimension(3,7,2,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank4_2_4_1_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 4, 1, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank4_2_4_1_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank4_2_4_1_3

    subroutine permute_real32_tensor_rank4_2_4_1_3(dst, src)
        real(real32), dimension(:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i2,i4,i1,i3) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank4_2_4_1_3

    subroutine test_permute_real32_tensor_rank4_3_2_1_4(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7) :: src
        real(real32), dimension(5,3,2,7) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank4_3_2_1_4(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 2, 1, 4], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank4_3_2_1_4:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank4_3_2_1_4

    subroutine permute_real32_tensor_rank4_3_2_1_4(dst, src)
        real(real32), dimension(:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i3,i2,i1,i4) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank4_3_2_1_4

    subroutine test_permute_real32_tensor_rank4_3_2_4_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7) :: src
        real(real32), dimension(5,3,7,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank4_3_2_4_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 2, 4, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank4_3_2_4_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank4_3_2_4_1

    subroutine permute_real32_tensor_rank4_3_2_4_1(dst, src)
        real(real32), dimension(:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i3,i2,i4,i1) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank4_3_2_4_1

    subroutine test_permute_real32_tensor_rank4_3_1_2_4(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7) :: src
        real(real32), dimension(5,2,3,7) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank4_3_1_2_4(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 1, 2, 4], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank4_3_1_2_4:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank4_3_1_2_4

    subroutine permute_real32_tensor_rank4_3_1_2_4(dst, src)
        real(real32), dimension(:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i3,i1,i2,i4) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank4_3_1_2_4

    subroutine test_permute_real32_tensor_rank4_3_1_4_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7) :: src
        real(real32), dimension(5,2,7,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank4_3_1_4_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 1, 4, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank4_3_1_4_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank4_3_1_4_2

    subroutine permute_real32_tensor_rank4_3_1_4_2(dst, src)
        real(real32), dimension(:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i3,i1,i4,i2) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank4_3_1_4_2

    subroutine test_permute_real32_tensor_rank4_3_4_1_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7) :: src
        real(real32), dimension(5,7,2,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank4_3_4_1_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 4, 1, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank4_3_4_1_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank4_3_4_1_2

    subroutine permute_real32_tensor_rank4_3_4_1_2(dst, src)
        real(real32), dimension(:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i3,i4,i1,i2) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank4_3_4_1_2

    subroutine test_permute_real32_tensor_rank4_3_4_2_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7) :: src
        real(real32), dimension(5,7,3,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank4_3_4_2_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 4, 2, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank4_3_4_2_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank4_3_4_2_1

    subroutine permute_real32_tensor_rank4_3_4_2_1(dst, src)
        real(real32), dimension(:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i3,i4,i2,i1) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank4_3_4_2_1

    subroutine test_permute_real32_tensor_rank4_4_2_3_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7) :: src
        real(real32), dimension(7,3,5,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank4_4_2_3_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [4, 2, 3, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank4_4_2_3_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank4_4_2_3_1

    subroutine permute_real32_tensor_rank4_4_2_3_1(dst, src)
        real(real32), dimension(:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i4,i2,i3,i1) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank4_4_2_3_1

    subroutine test_permute_real32_tensor_rank4_4_2_1_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7) :: src
        real(real32), dimension(7,3,2,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank4_4_2_1_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [4, 2, 1, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank4_4_2_1_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank4_4_2_1_3

    subroutine permute_real32_tensor_rank4_4_2_1_3(dst, src)
        real(real32), dimension(:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i4,i2,i1,i3) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank4_4_2_1_3

    subroutine test_permute_real32_tensor_rank4_4_3_2_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7) :: src
        real(real32), dimension(7,5,3,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank4_4_3_2_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [4, 3, 2, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank4_4_3_2_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank4_4_3_2_1

    subroutine permute_real32_tensor_rank4_4_3_2_1(dst, src)
        real(real32), dimension(:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i4,i3,i2,i1) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank4_4_3_2_1

    subroutine test_permute_real32_tensor_rank4_4_3_1_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7) :: src
        real(real32), dimension(7,5,2,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank4_4_3_1_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [4, 3, 1, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank4_4_3_1_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank4_4_3_1_2

    subroutine permute_real32_tensor_rank4_4_3_1_2(dst, src)
        real(real32), dimension(:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i4,i3,i1,i2) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank4_4_3_1_2

    subroutine test_permute_real32_tensor_rank4_4_1_3_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7) :: src
        real(real32), dimension(7,2,5,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank4_4_1_3_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [4, 1, 3, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank4_4_1_3_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank4_4_1_3_2

    subroutine permute_real32_tensor_rank4_4_1_3_2(dst, src)
        real(real32), dimension(:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i4,i1,i3,i2) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank4_4_1_3_2

    subroutine test_permute_real32_tensor_rank4_4_1_2_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7) :: src
        real(real32), dimension(7,2,3,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank4_4_1_2_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [4, 1, 2, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank4_4_1_2_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank4_4_1_2_3

    subroutine permute_real32_tensor_rank4_4_1_2_3(dst, src)
        real(real32), dimension(:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4

        do i4 = 1, size(src, 4)
            do i3 = 1, size(src, 3)
                do i2 = 1, size(src, 2)
                    do i1 = 1, size(src, 1)
                        dst(i4,i1,i2,i3) = src(i1,i2,i3,i4)
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank4_4_1_2_3

    subroutine test_permute_real32_tensor_rank5_1_2_3_4_5(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7,11) :: src
        real(real32), dimension(2,3,5,7,11) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank5_1_2_3_4_5(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 2, 3, 4, 5], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank5_1_2_3_4_5:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank5_1_2_3_4_5

    subroutine permute_real32_tensor_rank5_1_2_3_4_5(dst, src)
        real(real32), dimension(:,:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i1,i2,i3,i4,i5) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank5_1_2_3_4_5

    subroutine test_permute_real32_tensor_rank5_1_3_4_5_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7,11) :: src
        real(real32), dimension(2,5,7,11,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank5_1_3_4_5_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 3, 4, 5, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank5_1_3_4_5_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank5_1_3_4_5_2

    subroutine permute_real32_tensor_rank5_1_3_4_5_2(dst, src)
        real(real32), dimension(:,:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i1,i3,i4,i5,i2) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank5_1_3_4_5_2

    subroutine test_permute_real32_tensor_rank5_1_5_3_4_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7,11) :: src
        real(real32), dimension(2,11,5,7,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank5_1_5_3_4_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [1, 5, 3, 4, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank5_1_5_3_4_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank5_1_5_3_4_2

    subroutine permute_real32_tensor_rank5_1_5_3_4_2(dst, src)
        real(real32), dimension(:,:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i1,i5,i3,i4,i2) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank5_1_5_3_4_2

    subroutine test_permute_real32_tensor_rank5_2_1_4_5_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7,11) :: src
        real(real32), dimension(3,2,7,11,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank5_2_1_4_5_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 1, 4, 5, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank5_2_1_4_5_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank5_2_1_4_5_3

    subroutine permute_real32_tensor_rank5_2_1_4_5_3(dst, src)
        real(real32), dimension(:,:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i2,i1,i4,i5,i3) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank5_2_1_4_5_3

    subroutine test_permute_real32_tensor_rank5_2_4_3_1_5(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7,11) :: src
        real(real32), dimension(3,7,5,2,11) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank5_2_4_3_1_5(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 4, 3, 1, 5], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank5_2_4_3_1_5:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank5_2_4_3_1_5

    subroutine permute_real32_tensor_rank5_2_4_3_1_5(dst, src)
        real(real32), dimension(:,:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i2,i4,i3,i1,i5) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank5_2_4_3_1_5

    subroutine test_permute_real32_tensor_rank5_2_5_4_1_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7,11) :: src
        real(real32), dimension(3,11,7,2,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank5_2_5_4_1_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [2, 5, 4, 1, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank5_2_5_4_1_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank5_2_5_4_1_3

    subroutine permute_real32_tensor_rank5_2_5_4_1_3(dst, src)
        real(real32), dimension(:,:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i2,i5,i4,i1,i3) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank5_2_5_4_1_3

    subroutine test_permute_real32_tensor_rank5_3_1_2_4_5(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7,11) :: src
        real(real32), dimension(5,2,3,7,11) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank5_3_1_2_4_5(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 1, 2, 4, 5], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank5_3_1_2_4_5:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank5_3_1_2_4_5

    subroutine permute_real32_tensor_rank5_3_1_2_4_5(dst, src)
        real(real32), dimension(:,:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i3,i1,i2,i4,i5) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank5_3_1_2_4_5

    subroutine test_permute_real32_tensor_rank5_3_4_2_5_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7,11) :: src
        real(real32), dimension(5,7,3,11,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank5_3_4_2_5_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [3, 4, 2, 5, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank5_3_4_2_5_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank5_3_4_2_5_1

    subroutine permute_real32_tensor_rank5_3_4_2_5_1(dst, src)
        real(real32), dimension(:,:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i3,i4,i2,i5,i1) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank5_3_4_2_5_1

    subroutine test_permute_real32_tensor_rank5_4_2_3_1_5(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7,11) :: src
        real(real32), dimension(7,3,5,2,11) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank5_4_2_3_1_5(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [4, 2, 3, 1, 5], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank5_4_2_3_1_5:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank5_4_2_3_1_5

    subroutine permute_real32_tensor_rank5_4_2_3_1_5(dst, src)
        real(real32), dimension(:,:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i4,i2,i3,i1,i5) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank5_4_2_3_1_5

    subroutine test_permute_real32_tensor_rank5_4_3_1_5_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7,11) :: src
        real(real32), dimension(7,5,2,11,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank5_4_3_1_5_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [4, 3, 1, 5, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank5_4_3_1_5_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank5_4_3_1_5_2

    subroutine permute_real32_tensor_rank5_4_3_1_5_2(dst, src)
        real(real32), dimension(:,:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i4,i3,i1,i5,i2) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank5_4_3_1_5_2

    subroutine test_permute_real32_tensor_rank5_4_5_3_1_2(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7,11) :: src
        real(real32), dimension(7,11,5,2,3) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank5_4_5_3_1_2(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [4, 5, 3, 1, 2], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank5_4_5_3_1_2:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank5_4_5_3_1_2

    subroutine permute_real32_tensor_rank5_4_5_3_1_2(dst, src)
        real(real32), dimension(:,:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i4,i5,i3,i1,i2) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank5_4_5_3_1_2

    subroutine test_permute_real32_tensor_rank5_5_2_4_1_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7,11) :: src
        real(real32), dimension(11,3,7,2,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank5_5_2_4_1_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [5, 2, 4, 1, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank5_5_2_4_1_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank5_5_2_4_1_3

    subroutine permute_real32_tensor_rank5_5_2_4_1_3(dst, src)
        real(real32), dimension(:,:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i5,i2,i4,i1,i3) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank5_5_2_4_1_3

    subroutine test_permute_real32_tensor_rank5_5_4_3_2_1(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7,11) :: src
        real(real32), dimension(11,7,5,3,2) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank5_5_4_3_2_1(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [5, 4, 3, 2, 1], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank5_5_4_3_2_1:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank5_5_4_3_2_1

    subroutine permute_real32_tensor_rank5_5_4_3_2_1(dst, src)
        real(real32), dimension(:,:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i5,i4,i3,i2,i1) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank5_5_4_3_2_1

    subroutine test_permute_real32_tensor_rank5_5_1_4_2_3(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3,5,7,11) :: src
        real(real32), dimension(11,2,7,3,5) :: dst
        class(tensor), allocatable :: tsrc, tdst
        type(storage_helper) :: helper
        class(tensor_permute), allocatable :: p
        class(stream_handler), allocatable :: handler
        type(stream) :: astream

        call random_number(src)

        call allocate_and_copy_tensor(tsrc, src, memtype, options, priorities)
        call allocate_and_copy_tensor(tdst, dst, memtype, options, priorities)
        call tensor_permute_factory%create(p, driver, options, priorities)

        call permute_real32_tensor_rank5_5_1_4_2_3(dst, src)

        call concurrency_factory%create_default_stream_handler(handler, memtype, options, priorities)
        call handler%create(astream)

        call p%permute(tsrc, tdst, [5, 1, 4, 2, 3], astream)
        call p%synchronize(astream)

        call handler%destroy(astream)
        deallocate(handler)

        call assertion%equal(prefix//":permute_real32_tensor_rank5_5_1_4_2_3:equal", &
                helper%equal(tdst%storage, dst))

        call tsrc%cleanup(); call tdst%cleanup()
        deallocate(tsrc, tdst)

        call p%cleanup()
        deallocate(p)
    end subroutine test_permute_real32_tensor_rank5_5_1_4_2_3

    subroutine permute_real32_tensor_rank5_5_1_4_2_3(dst, src)
        real(real32), dimension(:,:,:,:,:), intent(inout) :: dst
        real(real32), dimension(:,:,:,:,:), intent(in) :: src

        integer :: i1, i2, i3, i4, i5

        do i5 = 1, size(src, 5)
            do i4 = 1, size(src, 4)
                do i3 = 1, size(src, 3)
                    do i2 = 1, size(src, 2)
                        do i1 = 1, size(src, 1)
                            dst(i5,i1,i4,i2,i3) = src(i1,i2,i3,i4,i5)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine permute_real32_tensor_rank5_5_1_4_2_3
end module tensor_permute_test_helper_module
