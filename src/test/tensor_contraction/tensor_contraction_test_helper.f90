! Auto-generated. DO NOT MODIFY!
module tensor_contraction_test_helper_module
    use, intrinsic :: iso_fortran_env, only : &
            int64, &
            real64, &
            real32
    use :: util_api, only : &
            string, &
            dictionary, &
            assert
    use :: data_api, only : &
            storage_helper
    use :: tensor_api, only : &
            tensor, &
            allocate_and_copy_tensor
    use :: algorithms_api, only : &
            tensor_contraction, &
            tensor_contraction_factory, &
            contract

    implicit none
    private

    public :: test_all

contains
    subroutine test_all(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        call test_contract_a_ai_i_complex128( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_i_a_ia_complex128( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_ai_a_i_complex128( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_ia_a_i_complex128( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_ai_ae_ei_complex128( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_ia_ae_ie_complex128( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_ai_aef_efi_complex128( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_ai_eaf_ife_complex128( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_abi_ea_eib_complex128( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_abi_bea_ei_complex128( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_ai_aeil_el_complex128( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_ab_ambe_em_complex128( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_abij_ab_ij_complex128( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_abij_abej_ei_complex128( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_abij_ijbk_ak_complex128( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_abij_abcd_cdij_complex128( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_abij_kbcj_acik_complex128( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract__ij_ij_complex128( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_ij__ij_complex128( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_a_ai_i_complex64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_i_a_ia_complex64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_ai_a_i_complex64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_ia_a_i_complex64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_ai_ae_ei_complex64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_ia_ae_ie_complex64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_ai_aef_efi_complex64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_ai_eaf_ife_complex64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_abi_ea_eib_complex64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_abi_bea_ei_complex64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_ai_aeil_el_complex64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_ab_ambe_em_complex64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_abij_ab_ij_complex64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_abij_abej_ei_complex64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_abij_ijbk_ak_complex64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_abij_abcd_cdij_complex64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_abij_kbcj_acik_complex64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract__ij_ij_complex64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_ij__ij_complex64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_a_ai_i_real64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_i_a_ia_real64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_ai_a_i_real64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_ia_a_i_real64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_ai_ae_ei_real64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_ia_ae_ie_real64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_ai_aef_efi_real64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_ai_eaf_ife_real64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_abi_ea_eib_real64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_abi_bea_ei_real64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_ai_aeil_el_real64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_ab_ambe_em_real64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_abij_ab_ij_real64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_abij_abej_ei_real64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_abij_ijbk_ak_real64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_abij_abcd_cdij_real64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_abij_kbcj_acik_real64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract__ij_ij_real64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_ij__ij_real64( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_a_ai_i_real32( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_i_a_ia_real32( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_ai_a_i_real32( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_ia_a_i_real32( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_ai_ae_ei_real32( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_ia_ae_ie_real32( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_ai_aef_efi_real32( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_ai_eaf_ife_real32( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_abi_ea_eib_real32( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_abi_bea_ei_real32( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_ai_aeil_el_real32( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_ab_ambe_em_real32( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_abij_ab_ij_real32( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_abij_abej_ei_real32( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_abij_ijbk_ak_real32( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_abij_abcd_cdij_real32( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_abij_kbcj_acik_real32( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract__ij_ij_real32( &
                assertion, prefix, driver, memtype, options, priorities)
        call test_contract_ij__ij_real32( &
                assertion, prefix, driver, memtype, options, priorities)
    end subroutine test_all

    subroutine test_contract_a_ai_i_complex128(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2) :: c
        complex(real64), dimension(2) :: c_easy
        complex(real64), dimension(2,3) :: a
        complex(real64), dimension(3) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real, dimension(2,3) :: a_real_array, a_imag_array
        real, dimension(3) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a)=a(a,i)*b(i)", &
                driver, options, priorities)

        call contract_a_ai_i_complex128(c, a, b)
        call contract(c_easy, a, b, "c(a)=a(a,i)*b(i)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_a_ai_i_complex128:equal", &
                helper%equal(tc%storage, c, (1.0d-12,1.0d-12)))

        call assertion%equal(prefix//":contract_a_ai_i_complex128-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0d-12,1.0d-12)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_a_ai_i_complex128

    subroutine contract_a_ai_i_complex128(c, a, b)
        complex(real64), dimension(:), intent(inout) :: c
        complex(real64), dimension(:,:), intent(in) :: a
        complex(real64), dimension(:), intent(in) :: b

        integer :: ia,ii

        do ia = 1, size(c, 1)
            do ii = 1, size(a, 2)
                c(ia) = c(ia) + &
                        a(ia,ii)*b(ii)
            end do
        end do
    end subroutine contract_a_ai_i_complex128

    subroutine test_contract_i_a_ia_complex128(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2) :: c
        complex(real64), dimension(2) :: c_easy
        complex(real64), dimension(3) :: a
        complex(real64), dimension(2,3) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real, dimension(3) :: a_real_array, a_imag_array
        real, dimension(2,3) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(i)=a(a)*b(i,a)", &
                driver, options, priorities)

        call contract_i_a_ia_complex128(c, a, b)
        call contract(c_easy, a, b, "c(i)=a(a)*b(i,a)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_i_a_ia_complex128:equal", &
                helper%equal(tc%storage, c, (1.0d-12,1.0d-12)))

        call assertion%equal(prefix//":contract_i_a_ia_complex128-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0d-12,1.0d-12)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_i_a_ia_complex128

    subroutine contract_i_a_ia_complex128(c, a, b)
        complex(real64), dimension(:), intent(inout) :: c
        complex(real64), dimension(:), intent(in) :: a
        complex(real64), dimension(:,:), intent(in) :: b

        integer :: ii,ia

        do ii = 1, size(c, 1)
            do ia = 1, size(a, 1)
                c(ii) = c(ii) + &
                        a(ia)*b(ii,ia)
            end do
        end do
    end subroutine contract_i_a_ia_complex128

    subroutine test_contract_ai_a_i_complex128(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3) :: c
        complex(real64), dimension(2,3) :: c_easy
        complex(real64), dimension(2) :: a
        complex(real64), dimension(3) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real, dimension(2) :: a_real_array, a_imag_array
        real, dimension(3) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,i)=a(a)*b(i)", &
                driver, options, priorities)

        call contract_ai_a_i_complex128(c, a, b)
        call contract(c_easy, a, b, "c(a,i)=a(a)*b(i)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_ai_a_i_complex128:equal", &
                helper%equal(tc%storage, c, (1.0d-12,1.0d-12)))

        call assertion%equal(prefix//":contract_ai_a_i_complex128-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0d-12,1.0d-12)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_ai_a_i_complex128

    subroutine contract_ai_a_i_complex128(c, a, b)
        complex(real64), dimension(:,:), intent(inout) :: c
        complex(real64), dimension(:), intent(in) :: a
        complex(real64), dimension(:), intent(in) :: b

        integer :: ia,ii

        do ia = 1, size(c, 1)
            do ii = 1, size(c, 2)
                c(ia,ii) = c(ia,ii) + &
                        a(ia)*b(ii)
            end do
        end do
    end subroutine contract_ai_a_i_complex128

    subroutine test_contract_ia_a_i_complex128(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(3,2) :: c
        complex(real64), dimension(3,2) :: c_easy
        complex(real64), dimension(2) :: a
        complex(real64), dimension(3) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real, dimension(2) :: a_real_array, a_imag_array
        real, dimension(3) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(i,a)=a(a)*b(i)", &
                driver, options, priorities)

        call contract_ia_a_i_complex128(c, a, b)
        call contract(c_easy, a, b, "c(i,a)=a(a)*b(i)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_ia_a_i_complex128:equal", &
                helper%equal(tc%storage, c, (1.0d-12,1.0d-12)))

        call assertion%equal(prefix//":contract_ia_a_i_complex128-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0d-12,1.0d-12)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_ia_a_i_complex128

    subroutine contract_ia_a_i_complex128(c, a, b)
        complex(real64), dimension(:,:), intent(inout) :: c
        complex(real64), dimension(:), intent(in) :: a
        complex(real64), dimension(:), intent(in) :: b

        integer :: ia,ii

        do ia = 1, size(c, 2)
            do ii = 1, size(c, 1)
                c(ii,ia) = c(ii,ia) + &
                        a(ia)*b(ii)
            end do
        end do
    end subroutine contract_ia_a_i_complex128

    subroutine test_contract_ai_ae_ei_complex128(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3) :: c
        complex(real64), dimension(2,3) :: c_easy
        complex(real64), dimension(2,5) :: a
        complex(real64), dimension(5,3) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real, dimension(2,5) :: a_real_array, a_imag_array
        real, dimension(5,3) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,i)=a(a,e)*b(e,i)", &
                driver, options, priorities)

        call contract_ai_ae_ei_complex128(c, a, b)
        call contract(c_easy, a, b, "c(a,i)=a(a,e)*b(e,i)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_ai_ae_ei_complex128:equal", &
                helper%equal(tc%storage, c, (1.0d-12,1.0d-12)))

        call assertion%equal(prefix//":contract_ai_ae_ei_complex128-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0d-12,1.0d-12)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_ai_ae_ei_complex128

    subroutine contract_ai_ae_ei_complex128(c, a, b)
        complex(real64), dimension(:,:), intent(inout) :: c
        complex(real64), dimension(:,:), intent(in) :: a
        complex(real64), dimension(:,:), intent(in) :: b

        integer :: ia,ii,ie

        do ia = 1, size(c, 1)
            do ii = 1, size(c, 2)
                do ie = 1, size(a, 2)
                    c(ia,ii) = c(ia,ii) + &
                            a(ia,ie)*b(ie,ii)
                end do
            end do
        end do
    end subroutine contract_ai_ae_ei_complex128

    subroutine test_contract_ia_ae_ie_complex128(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(3,2) :: c
        complex(real64), dimension(3,2) :: c_easy
        complex(real64), dimension(2,5) :: a
        complex(real64), dimension(3,5) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real, dimension(2,5) :: a_real_array, a_imag_array
        real, dimension(3,5) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(i,a)=a(a,e)*b(i,e)", &
                driver, options, priorities)

        call contract_ia_ae_ie_complex128(c, a, b)
        call contract(c_easy, a, b, "c(i,a)=a(a,e)*b(i,e)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_ia_ae_ie_complex128:equal", &
                helper%equal(tc%storage, c, (1.0d-12,1.0d-12)))

        call assertion%equal(prefix//":contract_ia_ae_ie_complex128-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0d-12,1.0d-12)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_ia_ae_ie_complex128

    subroutine contract_ia_ae_ie_complex128(c, a, b)
        complex(real64), dimension(:,:), intent(inout) :: c
        complex(real64), dimension(:,:), intent(in) :: a
        complex(real64), dimension(:,:), intent(in) :: b

        integer :: ia,ii,ie

        do ia = 1, size(c, 2)
            do ii = 1, size(c, 1)
                do ie = 1, size(a, 2)
                    c(ii,ia) = c(ii,ia) + &
                            a(ia,ie)*b(ii,ie)
                end do
            end do
        end do
    end subroutine contract_ia_ae_ie_complex128

    subroutine test_contract_ai_aef_efi_complex128(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3) :: c
        complex(real64), dimension(2,3) :: c_easy
        complex(real64), dimension(2,7,5) :: a
        complex(real64), dimension(7,5,3) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real, dimension(2,7,5) :: a_real_array, a_imag_array
        real, dimension(7,5,3) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,i)=a(a,e,f)*b(e,f,i)", &
                driver, options, priorities)

        call contract_ai_aef_efi_complex128(c, a, b)
        call contract(c_easy, a, b, "c(a,i)=a(a,e,f)*b(e,f,i)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_ai_aef_efi_complex128:equal", &
                helper%equal(tc%storage, c, (1.0d-12,1.0d-12)))

        call assertion%equal(prefix//":contract_ai_aef_efi_complex128-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0d-12,1.0d-12)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_ai_aef_efi_complex128

    subroutine contract_ai_aef_efi_complex128(c, a, b)
        complex(real64), dimension(:,:), intent(inout) :: c
        complex(real64), dimension(:,:,:), intent(in) :: a
        complex(real64), dimension(:,:,:), intent(in) :: b

        integer :: ia,ii,if,ie

        do ia = 1, size(c, 1)
            do ii = 1, size(c, 2)
                do if = 1, size(a, 3)
                    do ie = 1, size(a, 2)
                        c(ia,ii) = c(ia,ii) + &
                                a(ia,ie,if)*b(ie,if,ii)
                    end do
                end do
            end do
        end do
    end subroutine contract_ai_aef_efi_complex128

    subroutine test_contract_ai_eaf_ife_complex128(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3) :: c
        complex(real64), dimension(2,3) :: c_easy
        complex(real64), dimension(7,2,5) :: a
        complex(real64), dimension(3,5,7) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real, dimension(7,2,5) :: a_real_array, a_imag_array
        real, dimension(3,5,7) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,i)=a(e,a,f)*b(i,f,e)", &
                driver, options, priorities)

        call contract_ai_eaf_ife_complex128(c, a, b)
        call contract(c_easy, a, b, "c(a,i)=a(e,a,f)*b(i,f,e)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_ai_eaf_ife_complex128:equal", &
                helper%equal(tc%storage, c, (1.0d-12,1.0d-12)))

        call assertion%equal(prefix//":contract_ai_eaf_ife_complex128-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0d-12,1.0d-12)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_ai_eaf_ife_complex128

    subroutine contract_ai_eaf_ife_complex128(c, a, b)
        complex(real64), dimension(:,:), intent(inout) :: c
        complex(real64), dimension(:,:,:), intent(in) :: a
        complex(real64), dimension(:,:,:), intent(in) :: b

        integer :: ia,ii,if,ie

        do ia = 1, size(c, 1)
            do ii = 1, size(c, 2)
                do if = 1, size(a, 3)
                    do ie = 1, size(a, 1)
                        c(ia,ii) = c(ia,ii) + &
                                a(ie,ia,if)*b(ii,if,ie)
                    end do
                end do
            end do
        end do
    end subroutine contract_ai_eaf_ife_complex128

    subroutine test_contract_abi_ea_eib_complex128(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(3,2,5) :: c
        complex(real64), dimension(3,2,5) :: c_easy
        complex(real64), dimension(7,3) :: a
        complex(real64), dimension(7,5,2) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real, dimension(7,3) :: a_real_array, a_imag_array
        real, dimension(7,5,2) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,b,i)=a(e,a)*b(e,i,b)", &
                driver, options, priorities)

        call contract_abi_ea_eib_complex128(c, a, b)
        call contract(c_easy, a, b, "c(a,b,i)=a(e,a)*b(e,i,b)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_abi_ea_eib_complex128:equal", &
                helper%equal(tc%storage, c, (1.0d-12,1.0d-12)))

        call assertion%equal(prefix//":contract_abi_ea_eib_complex128-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0d-12,1.0d-12)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_abi_ea_eib_complex128

    subroutine contract_abi_ea_eib_complex128(c, a, b)
        complex(real64), dimension(:,:,:), intent(inout) :: c
        complex(real64), dimension(:,:), intent(in) :: a
        complex(real64), dimension(:,:,:), intent(in) :: b

        integer :: ib,ia,ii,ie

        do ib = 1, size(c, 2)
            do ia = 1, size(c, 1)
                do ii = 1, size(c, 3)
                    do ie = 1, size(a, 1)
                        c(ia,ib,ii) = c(ia,ib,ii) + &
                                a(ie,ia)*b(ie,ii,ib)
                    end do
                end do
            end do
        end do
    end subroutine contract_abi_ea_eib_complex128

    subroutine test_contract_abi_bea_ei_complex128(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(3,2,5) :: c
        complex(real64), dimension(3,2,5) :: c_easy
        complex(real64), dimension(2,7,3) :: a
        complex(real64), dimension(7,5) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real, dimension(2,7,3) :: a_real_array, a_imag_array
        real, dimension(7,5) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,b,i)=a(b,e,a)*b(e,i)", &
                driver, options, priorities)

        call contract_abi_bea_ei_complex128(c, a, b)
        call contract(c_easy, a, b, "c(a,b,i)=a(b,e,a)*b(e,i)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_abi_bea_ei_complex128:equal", &
                helper%equal(tc%storage, c, (1.0d-12,1.0d-12)))

        call assertion%equal(prefix//":contract_abi_bea_ei_complex128-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0d-12,1.0d-12)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_abi_bea_ei_complex128

    subroutine contract_abi_bea_ei_complex128(c, a, b)
        complex(real64), dimension(:,:,:), intent(inout) :: c
        complex(real64), dimension(:,:,:), intent(in) :: a
        complex(real64), dimension(:,:), intent(in) :: b

        integer :: ib,ia,ii,ie

        do ib = 1, size(c, 2)
            do ia = 1, size(c, 1)
                do ii = 1, size(c, 3)
                    do ie = 1, size(a, 2)
                        c(ia,ib,ii) = c(ia,ib,ii) + &
                                a(ib,ie,ia)*b(ie,ii)
                    end do
                end do
            end do
        end do
    end subroutine contract_abi_bea_ei_complex128

    subroutine test_contract_ai_aeil_el_complex128(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(2,3) :: c
        complex(real64), dimension(2,3) :: c_easy
        complex(real64), dimension(2,7,3,5) :: a
        complex(real64), dimension(7,5) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real, dimension(2,7,3,5) :: a_real_array, a_imag_array
        real, dimension(7,5) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,i)=a(a,e,i,l)*b(e,l)", &
                driver, options, priorities)

        call contract_ai_aeil_el_complex128(c, a, b)
        call contract(c_easy, a, b, "c(a,i)=a(a,e,i,l)*b(e,l)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_ai_aeil_el_complex128:equal", &
                helper%equal(tc%storage, c, (1.0d-12,1.0d-12)))

        call assertion%equal(prefix//":contract_ai_aeil_el_complex128-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0d-12,1.0d-12)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_ai_aeil_el_complex128

    subroutine contract_ai_aeil_el_complex128(c, a, b)
        complex(real64), dimension(:,:), intent(inout) :: c
        complex(real64), dimension(:,:,:,:), intent(in) :: a
        complex(real64), dimension(:,:), intent(in) :: b

        integer :: ia,ii,il,ie

        do ia = 1, size(c, 1)
            do ii = 1, size(c, 2)
                do il = 1, size(a, 4)
                    do ie = 1, size(a, 2)
                        c(ia,ii) = c(ia,ii) + &
                                a(ia,ie,ii,il)*b(ie,il)
                    end do
                end do
            end do
        end do
    end subroutine contract_ai_aeil_el_complex128

    subroutine test_contract_ab_ambe_em_complex128(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(3,2) :: c
        complex(real64), dimension(3,2) :: c_easy
        complex(real64), dimension(3,5,2,7) :: a
        complex(real64), dimension(7,5) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real, dimension(3,5,2,7) :: a_real_array, a_imag_array
        real, dimension(7,5) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,b)=a(a,m,b,e)*b(e,m)", &
                driver, options, priorities)

        call contract_ab_ambe_em_complex128(c, a, b)
        call contract(c_easy, a, b, "c(a,b)=a(a,m,b,e)*b(e,m)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_ab_ambe_em_complex128:equal", &
                helper%equal(tc%storage, c, (1.0d-12,1.0d-12)))

        call assertion%equal(prefix//":contract_ab_ambe_em_complex128-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0d-12,1.0d-12)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_ab_ambe_em_complex128

    subroutine contract_ab_ambe_em_complex128(c, a, b)
        complex(real64), dimension(:,:), intent(inout) :: c
        complex(real64), dimension(:,:,:,:), intent(in) :: a
        complex(real64), dimension(:,:), intent(in) :: b

        integer :: ib,ia,im,ie

        do ib = 1, size(c, 2)
            do ia = 1, size(c, 1)
                do im = 1, size(a, 2)
                    do ie = 1, size(a, 4)
                        c(ia,ib) = c(ia,ib) + &
                                a(ia,im,ib,ie)*b(ie,im)
                    end do
                end do
            end do
        end do
    end subroutine contract_ab_ambe_em_complex128

    subroutine test_contract_abij_ab_ij_complex128(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(5,3,7,2) :: c
        complex(real64), dimension(5,3,7,2) :: c_easy
        complex(real64), dimension(5,3) :: a
        complex(real64), dimension(7,2) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real, dimension(5,3) :: a_real_array, a_imag_array
        real, dimension(7,2) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,b,i,j)=a(a,b)*b(i,j)", &
                driver, options, priorities)

        call contract_abij_ab_ij_complex128(c, a, b)
        call contract(c_easy, a, b, "c(a,b,i,j)=a(a,b)*b(i,j)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_abij_ab_ij_complex128:equal", &
                helper%equal(tc%storage, c, (1.0d-12,1.0d-12)))

        call assertion%equal(prefix//":contract_abij_ab_ij_complex128-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0d-12,1.0d-12)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_abij_ab_ij_complex128

    subroutine contract_abij_ab_ij_complex128(c, a, b)
        complex(real64), dimension(:,:,:,:), intent(inout) :: c
        complex(real64), dimension(:,:), intent(in) :: a
        complex(real64), dimension(:,:), intent(in) :: b

        integer :: ij,ib,ia,ii

        do ij = 1, size(c, 4)
            do ib = 1, size(c, 2)
                do ia = 1, size(c, 1)
                    do ii = 1, size(c, 3)
                        c(ia,ib,ii,ij) = c(ia,ib,ii,ij) + &
                                a(ia,ib)*b(ii,ij)
                    end do
                end do
            end do
        end do
    end subroutine contract_abij_ab_ij_complex128

    subroutine test_contract_abij_abej_ei_complex128(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(5,3,7,2) :: c
        complex(real64), dimension(5,3,7,2) :: c_easy
        complex(real64), dimension(5,3,11,2) :: a
        complex(real64), dimension(11,7) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real, dimension(5,3,11,2) :: a_real_array, a_imag_array
        real, dimension(11,7) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,b,i,j)=a(a,b,e,j)*b(e,i)", &
                driver, options, priorities)

        call contract_abij_abej_ei_complex128(c, a, b)
        call contract(c_easy, a, b, "c(a,b,i,j)=a(a,b,e,j)*b(e,i)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_abij_abej_ei_complex128:equal", &
                helper%equal(tc%storage, c, (1.0d-12,1.0d-12)))

        call assertion%equal(prefix//":contract_abij_abej_ei_complex128-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0d-12,1.0d-12)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_abij_abej_ei_complex128

    subroutine contract_abij_abej_ei_complex128(c, a, b)
        complex(real64), dimension(:,:,:,:), intent(inout) :: c
        complex(real64), dimension(:,:,:,:), intent(in) :: a
        complex(real64), dimension(:,:), intent(in) :: b

        integer :: ij,ib,ia,ii,ie

        do ij = 1, size(c, 4)
            do ib = 1, size(c, 2)
                do ia = 1, size(c, 1)
                    do ii = 1, size(c, 3)
                        do ie = 1, size(a, 3)
                            c(ia,ib,ii,ij) = c(ia,ib,ii,ij) + &
                                    a(ia,ib,ie,ij)*b(ie,ii)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine contract_abij_abej_ei_complex128

    subroutine test_contract_abij_ijbk_ak_complex128(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(5,3,7,2) :: c
        complex(real64), dimension(5,3,7,2) :: c_easy
        complex(real64), dimension(7,2,3,11) :: a
        complex(real64), dimension(5,11) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real, dimension(7,2,3,11) :: a_real_array, a_imag_array
        real, dimension(5,11) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,b,i,j)=a(i,j,b,k)*b(a,k)", &
                driver, options, priorities)

        call contract_abij_ijbk_ak_complex128(c, a, b)
        call contract(c_easy, a, b, "c(a,b,i,j)=a(i,j,b,k)*b(a,k)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_abij_ijbk_ak_complex128:equal", &
                helper%equal(tc%storage, c, (1.0d-12,1.0d-12)))

        call assertion%equal(prefix//":contract_abij_ijbk_ak_complex128-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0d-12,1.0d-12)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_abij_ijbk_ak_complex128

    subroutine contract_abij_ijbk_ak_complex128(c, a, b)
        complex(real64), dimension(:,:,:,:), intent(inout) :: c
        complex(real64), dimension(:,:,:,:), intent(in) :: a
        complex(real64), dimension(:,:), intent(in) :: b

        integer :: ij,ib,ia,ii,ik

        do ij = 1, size(c, 4)
            do ib = 1, size(c, 2)
                do ia = 1, size(c, 1)
                    do ii = 1, size(c, 3)
                        do ik = 1, size(a, 4)
                            c(ia,ib,ii,ij) = c(ia,ib,ii,ij) + &
                                    a(ii,ij,ib,ik)*b(ia,ik)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine contract_abij_ijbk_ak_complex128

    subroutine test_contract_abij_abcd_cdij_complex128(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(5,3,7,2) :: c
        complex(real64), dimension(5,3,7,2) :: c_easy
        complex(real64), dimension(5,3,13,11) :: a
        complex(real64), dimension(13,11,7,2) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real, dimension(5,3,13,11) :: a_real_array, a_imag_array
        real, dimension(13,11,7,2) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,b,i,j)=a(a,b,c,d)*b(c,d,i,j)", &
                driver, options, priorities)

        call contract_abij_abcd_cdij_complex128(c, a, b)
        call contract(c_easy, a, b, "c(a,b,i,j)=a(a,b,c,d)*b(c,d,i,j)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_abij_abcd_cdij_complex128:equal", &
                helper%equal(tc%storage, c, (1.0d-12,1.0d-12)))

        call assertion%equal(prefix//":contract_abij_abcd_cdij_complex128-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0d-12,1.0d-12)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_abij_abcd_cdij_complex128

    subroutine contract_abij_abcd_cdij_complex128(c, a, b)
        complex(real64), dimension(:,:,:,:), intent(inout) :: c
        complex(real64), dimension(:,:,:,:), intent(in) :: a
        complex(real64), dimension(:,:,:,:), intent(in) :: b

        integer :: ij,ib,ia,ii,id,ic

        do ij = 1, size(c, 4)
            do ib = 1, size(c, 2)
                do ia = 1, size(c, 1)
                    do ii = 1, size(c, 3)
                        do id = 1, size(a, 4)
                            do ic = 1, size(a, 3)
                                c(ia,ib,ii,ij) = c(ia,ib,ii,ij) + &
                                        a(ia,ib,ic,id)*b(ic,id,ii,ij)
                            end do
                        end do
                    end do
                end do
            end do
        end do
    end subroutine contract_abij_abcd_cdij_complex128

    subroutine test_contract_abij_kbcj_acik_complex128(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(5,3,7,2) :: c
        complex(real64), dimension(5,3,7,2) :: c_easy
        complex(real64), dimension(11,3,13,2) :: a
        complex(real64), dimension(5,13,7,11) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real, dimension(11,3,13,2) :: a_real_array, a_imag_array
        real, dimension(5,13,7,11) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,b,i,j)=a(k,b,c,j)*b(a,c,i,k)", &
                driver, options, priorities)

        call contract_abij_kbcj_acik_complex128(c, a, b)
        call contract(c_easy, a, b, "c(a,b,i,j)=a(k,b,c,j)*b(a,c,i,k)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_abij_kbcj_acik_complex128:equal", &
                helper%equal(tc%storage, c, (1.0d-12,1.0d-12)))

        call assertion%equal(prefix//":contract_abij_kbcj_acik_complex128-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0d-12,1.0d-12)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_abij_kbcj_acik_complex128

    subroutine contract_abij_kbcj_acik_complex128(c, a, b)
        complex(real64), dimension(:,:,:,:), intent(inout) :: c
        complex(real64), dimension(:,:,:,:), intent(in) :: a
        complex(real64), dimension(:,:,:,:), intent(in) :: b

        integer :: ij,ib,ia,ii,ik,ic

        do ij = 1, size(c, 4)
            do ib = 1, size(c, 2)
                do ia = 1, size(c, 1)
                    do ii = 1, size(c, 3)
                        do ik = 1, size(a, 1)
                            do ic = 1, size(a, 3)
                                c(ia,ib,ii,ij) = c(ia,ib,ii,ij) + &
                                        a(ik,ib,ic,ij)*b(ia,ic,ii,ik)
                            end do
                        end do
                    end do
                end do
            end do
        end do
    end subroutine contract_abij_kbcj_acik_complex128

    subroutine test_contract__ij_ij_complex128(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64) :: c
        complex(real64) :: c_easy
        complex(real64), dimension(3,2) :: a
        complex(real64), dimension(3,2) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real, dimension(3,2) :: a_real_array, a_imag_array
        real, dimension(3,2) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c=a(i,j)*b(i,j)", &
                driver, options, priorities)

        call contract__ij_ij_complex128(c, a, b)
        call contract(c_easy, a, b, "c=a(i,j)*b(i,j)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract__ij_ij_complex128:equal", &
                helper%equal(tc%storage, c, (1.0d-12,1.0d-12)))

        call assertion%equal(prefix//":contract__ij_ij_complex128-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0d-12,1.0d-12)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract__ij_ij_complex128

    subroutine contract__ij_ij_complex128(c, a, b)
        complex(real64), intent(inout) :: c
        complex(real64), dimension(:,:), intent(in) :: a
        complex(real64), dimension(:,:), intent(in) :: b

        integer :: ij,ii

        do ij = 1, size(a, 2)
            do ii = 1, size(a, 1)
                c = c + &
                        a(ii,ij)*b(ii,ij)
            end do
        end do
    end subroutine contract__ij_ij_complex128

    subroutine test_contract_ij__ij_complex128(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real64), dimension(3,2) :: c
        complex(real64), dimension(3,2) :: c_easy
        complex(real64) :: a
        complex(real64), dimension(3,2) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real :: a_real_array, a_imag_array
        real, dimension(3,2) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(i,j)=a*b(i,j)", &
                driver, options, priorities)

        call contract_ij__ij_complex128(c, a, b)
        call contract(c_easy, a, b, "c(i,j)=a*b(i,j)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_ij__ij_complex128:equal", &
                helper%equal(tc%storage, c, (1.0d-12,1.0d-12)))

        call assertion%equal(prefix//":contract_ij__ij_complex128-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0d-12,1.0d-12)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_ij__ij_complex128

    subroutine contract_ij__ij_complex128(c, a, b)
        complex(real64), dimension(:,:), intent(inout) :: c
        complex(real64), intent(in) :: a
        complex(real64), dimension(:,:), intent(in) :: b

        integer :: ij,ii

        do ij = 1, size(c, 2)
            do ii = 1, size(c, 1)
                c(ii,ij) = c(ii,ij) + &
                        a*b(ii,ij)
            end do
        end do
    end subroutine contract_ij__ij_complex128

    subroutine test_contract_a_ai_i_complex64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2) :: c
        complex(real32), dimension(2) :: c_easy
        complex(real32), dimension(2,3) :: a
        complex(real32), dimension(3) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real, dimension(2,3) :: a_real_array, a_imag_array
        real, dimension(3) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a)=a(a,i)*b(i)", &
                driver, options, priorities)

        call contract_a_ai_i_complex64(c, a, b)
        call contract(c_easy, a, b, "c(a)=a(a,i)*b(i)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_a_ai_i_complex64:equal", &
                helper%equal(tc%storage, c, (1.0e-4,1.0e-4)))

        call assertion%equal(prefix//":contract_a_ai_i_complex64-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0e-4,1.0e-4)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_a_ai_i_complex64

    subroutine contract_a_ai_i_complex64(c, a, b)
        complex(real32), dimension(:), intent(inout) :: c
        complex(real32), dimension(:,:), intent(in) :: a
        complex(real32), dimension(:), intent(in) :: b

        integer :: ia,ii

        do ia = 1, size(c, 1)
            do ii = 1, size(a, 2)
                c(ia) = c(ia) + &
                        a(ia,ii)*b(ii)
            end do
        end do
    end subroutine contract_a_ai_i_complex64

    subroutine test_contract_i_a_ia_complex64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2) :: c
        complex(real32), dimension(2) :: c_easy
        complex(real32), dimension(3) :: a
        complex(real32), dimension(2,3) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real, dimension(3) :: a_real_array, a_imag_array
        real, dimension(2,3) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(i)=a(a)*b(i,a)", &
                driver, options, priorities)

        call contract_i_a_ia_complex64(c, a, b)
        call contract(c_easy, a, b, "c(i)=a(a)*b(i,a)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_i_a_ia_complex64:equal", &
                helper%equal(tc%storage, c, (1.0e-4,1.0e-4)))

        call assertion%equal(prefix//":contract_i_a_ia_complex64-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0e-4,1.0e-4)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_i_a_ia_complex64

    subroutine contract_i_a_ia_complex64(c, a, b)
        complex(real32), dimension(:), intent(inout) :: c
        complex(real32), dimension(:), intent(in) :: a
        complex(real32), dimension(:,:), intent(in) :: b

        integer :: ii,ia

        do ii = 1, size(c, 1)
            do ia = 1, size(a, 1)
                c(ii) = c(ii) + &
                        a(ia)*b(ii,ia)
            end do
        end do
    end subroutine contract_i_a_ia_complex64

    subroutine test_contract_ai_a_i_complex64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3) :: c
        complex(real32), dimension(2,3) :: c_easy
        complex(real32), dimension(2) :: a
        complex(real32), dimension(3) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real, dimension(2) :: a_real_array, a_imag_array
        real, dimension(3) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,i)=a(a)*b(i)", &
                driver, options, priorities)

        call contract_ai_a_i_complex64(c, a, b)
        call contract(c_easy, a, b, "c(a,i)=a(a)*b(i)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_ai_a_i_complex64:equal", &
                helper%equal(tc%storage, c, (1.0e-4,1.0e-4)))

        call assertion%equal(prefix//":contract_ai_a_i_complex64-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0e-4,1.0e-4)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_ai_a_i_complex64

    subroutine contract_ai_a_i_complex64(c, a, b)
        complex(real32), dimension(:,:), intent(inout) :: c
        complex(real32), dimension(:), intent(in) :: a
        complex(real32), dimension(:), intent(in) :: b

        integer :: ia,ii

        do ia = 1, size(c, 1)
            do ii = 1, size(c, 2)
                c(ia,ii) = c(ia,ii) + &
                        a(ia)*b(ii)
            end do
        end do
    end subroutine contract_ai_a_i_complex64

    subroutine test_contract_ia_a_i_complex64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(3,2) :: c
        complex(real32), dimension(3,2) :: c_easy
        complex(real32), dimension(2) :: a
        complex(real32), dimension(3) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real, dimension(2) :: a_real_array, a_imag_array
        real, dimension(3) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(i,a)=a(a)*b(i)", &
                driver, options, priorities)

        call contract_ia_a_i_complex64(c, a, b)
        call contract(c_easy, a, b, "c(i,a)=a(a)*b(i)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_ia_a_i_complex64:equal", &
                helper%equal(tc%storage, c, (1.0e-4,1.0e-4)))

        call assertion%equal(prefix//":contract_ia_a_i_complex64-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0e-4,1.0e-4)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_ia_a_i_complex64

    subroutine contract_ia_a_i_complex64(c, a, b)
        complex(real32), dimension(:,:), intent(inout) :: c
        complex(real32), dimension(:), intent(in) :: a
        complex(real32), dimension(:), intent(in) :: b

        integer :: ia,ii

        do ia = 1, size(c, 2)
            do ii = 1, size(c, 1)
                c(ii,ia) = c(ii,ia) + &
                        a(ia)*b(ii)
            end do
        end do
    end subroutine contract_ia_a_i_complex64

    subroutine test_contract_ai_ae_ei_complex64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3) :: c
        complex(real32), dimension(2,3) :: c_easy
        complex(real32), dimension(2,5) :: a
        complex(real32), dimension(5,3) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real, dimension(2,5) :: a_real_array, a_imag_array
        real, dimension(5,3) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,i)=a(a,e)*b(e,i)", &
                driver, options, priorities)

        call contract_ai_ae_ei_complex64(c, a, b)
        call contract(c_easy, a, b, "c(a,i)=a(a,e)*b(e,i)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_ai_ae_ei_complex64:equal", &
                helper%equal(tc%storage, c, (1.0e-4,1.0e-4)))

        call assertion%equal(prefix//":contract_ai_ae_ei_complex64-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0e-4,1.0e-4)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_ai_ae_ei_complex64

    subroutine contract_ai_ae_ei_complex64(c, a, b)
        complex(real32), dimension(:,:), intent(inout) :: c
        complex(real32), dimension(:,:), intent(in) :: a
        complex(real32), dimension(:,:), intent(in) :: b

        integer :: ia,ii,ie

        do ia = 1, size(c, 1)
            do ii = 1, size(c, 2)
                do ie = 1, size(a, 2)
                    c(ia,ii) = c(ia,ii) + &
                            a(ia,ie)*b(ie,ii)
                end do
            end do
        end do
    end subroutine contract_ai_ae_ei_complex64

    subroutine test_contract_ia_ae_ie_complex64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(3,2) :: c
        complex(real32), dimension(3,2) :: c_easy
        complex(real32), dimension(2,5) :: a
        complex(real32), dimension(3,5) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real, dimension(2,5) :: a_real_array, a_imag_array
        real, dimension(3,5) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(i,a)=a(a,e)*b(i,e)", &
                driver, options, priorities)

        call contract_ia_ae_ie_complex64(c, a, b)
        call contract(c_easy, a, b, "c(i,a)=a(a,e)*b(i,e)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_ia_ae_ie_complex64:equal", &
                helper%equal(tc%storage, c, (1.0e-4,1.0e-4)))

        call assertion%equal(prefix//":contract_ia_ae_ie_complex64-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0e-4,1.0e-4)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_ia_ae_ie_complex64

    subroutine contract_ia_ae_ie_complex64(c, a, b)
        complex(real32), dimension(:,:), intent(inout) :: c
        complex(real32), dimension(:,:), intent(in) :: a
        complex(real32), dimension(:,:), intent(in) :: b

        integer :: ia,ii,ie

        do ia = 1, size(c, 2)
            do ii = 1, size(c, 1)
                do ie = 1, size(a, 2)
                    c(ii,ia) = c(ii,ia) + &
                            a(ia,ie)*b(ii,ie)
                end do
            end do
        end do
    end subroutine contract_ia_ae_ie_complex64

    subroutine test_contract_ai_aef_efi_complex64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3) :: c
        complex(real32), dimension(2,3) :: c_easy
        complex(real32), dimension(2,7,5) :: a
        complex(real32), dimension(7,5,3) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real, dimension(2,7,5) :: a_real_array, a_imag_array
        real, dimension(7,5,3) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,i)=a(a,e,f)*b(e,f,i)", &
                driver, options, priorities)

        call contract_ai_aef_efi_complex64(c, a, b)
        call contract(c_easy, a, b, "c(a,i)=a(a,e,f)*b(e,f,i)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_ai_aef_efi_complex64:equal", &
                helper%equal(tc%storage, c, (1.0e-4,1.0e-4)))

        call assertion%equal(prefix//":contract_ai_aef_efi_complex64-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0e-4,1.0e-4)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_ai_aef_efi_complex64

    subroutine contract_ai_aef_efi_complex64(c, a, b)
        complex(real32), dimension(:,:), intent(inout) :: c
        complex(real32), dimension(:,:,:), intent(in) :: a
        complex(real32), dimension(:,:,:), intent(in) :: b

        integer :: ia,ii,if,ie

        do ia = 1, size(c, 1)
            do ii = 1, size(c, 2)
                do if = 1, size(a, 3)
                    do ie = 1, size(a, 2)
                        c(ia,ii) = c(ia,ii) + &
                                a(ia,ie,if)*b(ie,if,ii)
                    end do
                end do
            end do
        end do
    end subroutine contract_ai_aef_efi_complex64

    subroutine test_contract_ai_eaf_ife_complex64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3) :: c
        complex(real32), dimension(2,3) :: c_easy
        complex(real32), dimension(7,2,5) :: a
        complex(real32), dimension(3,5,7) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real, dimension(7,2,5) :: a_real_array, a_imag_array
        real, dimension(3,5,7) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,i)=a(e,a,f)*b(i,f,e)", &
                driver, options, priorities)

        call contract_ai_eaf_ife_complex64(c, a, b)
        call contract(c_easy, a, b, "c(a,i)=a(e,a,f)*b(i,f,e)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_ai_eaf_ife_complex64:equal", &
                helper%equal(tc%storage, c, (1.0e-4,1.0e-4)))

        call assertion%equal(prefix//":contract_ai_eaf_ife_complex64-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0e-4,1.0e-4)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_ai_eaf_ife_complex64

    subroutine contract_ai_eaf_ife_complex64(c, a, b)
        complex(real32), dimension(:,:), intent(inout) :: c
        complex(real32), dimension(:,:,:), intent(in) :: a
        complex(real32), dimension(:,:,:), intent(in) :: b

        integer :: ia,ii,if,ie

        do ia = 1, size(c, 1)
            do ii = 1, size(c, 2)
                do if = 1, size(a, 3)
                    do ie = 1, size(a, 1)
                        c(ia,ii) = c(ia,ii) + &
                                a(ie,ia,if)*b(ii,if,ie)
                    end do
                end do
            end do
        end do
    end subroutine contract_ai_eaf_ife_complex64

    subroutine test_contract_abi_ea_eib_complex64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(3,2,5) :: c
        complex(real32), dimension(3,2,5) :: c_easy
        complex(real32), dimension(7,3) :: a
        complex(real32), dimension(7,5,2) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real, dimension(7,3) :: a_real_array, a_imag_array
        real, dimension(7,5,2) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,b,i)=a(e,a)*b(e,i,b)", &
                driver, options, priorities)

        call contract_abi_ea_eib_complex64(c, a, b)
        call contract(c_easy, a, b, "c(a,b,i)=a(e,a)*b(e,i,b)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_abi_ea_eib_complex64:equal", &
                helper%equal(tc%storage, c, (1.0e-4,1.0e-4)))

        call assertion%equal(prefix//":contract_abi_ea_eib_complex64-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0e-4,1.0e-4)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_abi_ea_eib_complex64

    subroutine contract_abi_ea_eib_complex64(c, a, b)
        complex(real32), dimension(:,:,:), intent(inout) :: c
        complex(real32), dimension(:,:), intent(in) :: a
        complex(real32), dimension(:,:,:), intent(in) :: b

        integer :: ib,ia,ii,ie

        do ib = 1, size(c, 2)
            do ia = 1, size(c, 1)
                do ii = 1, size(c, 3)
                    do ie = 1, size(a, 1)
                        c(ia,ib,ii) = c(ia,ib,ii) + &
                                a(ie,ia)*b(ie,ii,ib)
                    end do
                end do
            end do
        end do
    end subroutine contract_abi_ea_eib_complex64

    subroutine test_contract_abi_bea_ei_complex64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(3,2,5) :: c
        complex(real32), dimension(3,2,5) :: c_easy
        complex(real32), dimension(2,7,3) :: a
        complex(real32), dimension(7,5) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real, dimension(2,7,3) :: a_real_array, a_imag_array
        real, dimension(7,5) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,b,i)=a(b,e,a)*b(e,i)", &
                driver, options, priorities)

        call contract_abi_bea_ei_complex64(c, a, b)
        call contract(c_easy, a, b, "c(a,b,i)=a(b,e,a)*b(e,i)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_abi_bea_ei_complex64:equal", &
                helper%equal(tc%storage, c, (1.0e-4,1.0e-4)))

        call assertion%equal(prefix//":contract_abi_bea_ei_complex64-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0e-4,1.0e-4)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_abi_bea_ei_complex64

    subroutine contract_abi_bea_ei_complex64(c, a, b)
        complex(real32), dimension(:,:,:), intent(inout) :: c
        complex(real32), dimension(:,:,:), intent(in) :: a
        complex(real32), dimension(:,:), intent(in) :: b

        integer :: ib,ia,ii,ie

        do ib = 1, size(c, 2)
            do ia = 1, size(c, 1)
                do ii = 1, size(c, 3)
                    do ie = 1, size(a, 2)
                        c(ia,ib,ii) = c(ia,ib,ii) + &
                                a(ib,ie,ia)*b(ie,ii)
                    end do
                end do
            end do
        end do
    end subroutine contract_abi_bea_ei_complex64

    subroutine test_contract_ai_aeil_el_complex64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(2,3) :: c
        complex(real32), dimension(2,3) :: c_easy
        complex(real32), dimension(2,7,3,5) :: a
        complex(real32), dimension(7,5) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real, dimension(2,7,3,5) :: a_real_array, a_imag_array
        real, dimension(7,5) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,i)=a(a,e,i,l)*b(e,l)", &
                driver, options, priorities)

        call contract_ai_aeil_el_complex64(c, a, b)
        call contract(c_easy, a, b, "c(a,i)=a(a,e,i,l)*b(e,l)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_ai_aeil_el_complex64:equal", &
                helper%equal(tc%storage, c, (1.0e-4,1.0e-4)))

        call assertion%equal(prefix//":contract_ai_aeil_el_complex64-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0e-4,1.0e-4)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_ai_aeil_el_complex64

    subroutine contract_ai_aeil_el_complex64(c, a, b)
        complex(real32), dimension(:,:), intent(inout) :: c
        complex(real32), dimension(:,:,:,:), intent(in) :: a
        complex(real32), dimension(:,:), intent(in) :: b

        integer :: ia,ii,il,ie

        do ia = 1, size(c, 1)
            do ii = 1, size(c, 2)
                do il = 1, size(a, 4)
                    do ie = 1, size(a, 2)
                        c(ia,ii) = c(ia,ii) + &
                                a(ia,ie,ii,il)*b(ie,il)
                    end do
                end do
            end do
        end do
    end subroutine contract_ai_aeil_el_complex64

    subroutine test_contract_ab_ambe_em_complex64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(3,2) :: c
        complex(real32), dimension(3,2) :: c_easy
        complex(real32), dimension(3,5,2,7) :: a
        complex(real32), dimension(7,5) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real, dimension(3,5,2,7) :: a_real_array, a_imag_array
        real, dimension(7,5) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,b)=a(a,m,b,e)*b(e,m)", &
                driver, options, priorities)

        call contract_ab_ambe_em_complex64(c, a, b)
        call contract(c_easy, a, b, "c(a,b)=a(a,m,b,e)*b(e,m)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_ab_ambe_em_complex64:equal", &
                helper%equal(tc%storage, c, (1.0e-4,1.0e-4)))

        call assertion%equal(prefix//":contract_ab_ambe_em_complex64-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0e-4,1.0e-4)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_ab_ambe_em_complex64

    subroutine contract_ab_ambe_em_complex64(c, a, b)
        complex(real32), dimension(:,:), intent(inout) :: c
        complex(real32), dimension(:,:,:,:), intent(in) :: a
        complex(real32), dimension(:,:), intent(in) :: b

        integer :: ib,ia,im,ie

        do ib = 1, size(c, 2)
            do ia = 1, size(c, 1)
                do im = 1, size(a, 2)
                    do ie = 1, size(a, 4)
                        c(ia,ib) = c(ia,ib) + &
                                a(ia,im,ib,ie)*b(ie,im)
                    end do
                end do
            end do
        end do
    end subroutine contract_ab_ambe_em_complex64

    subroutine test_contract_abij_ab_ij_complex64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(5,3,7,2) :: c
        complex(real32), dimension(5,3,7,2) :: c_easy
        complex(real32), dimension(5,3) :: a
        complex(real32), dimension(7,2) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real, dimension(5,3) :: a_real_array, a_imag_array
        real, dimension(7,2) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,b,i,j)=a(a,b)*b(i,j)", &
                driver, options, priorities)

        call contract_abij_ab_ij_complex64(c, a, b)
        call contract(c_easy, a, b, "c(a,b,i,j)=a(a,b)*b(i,j)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_abij_ab_ij_complex64:equal", &
                helper%equal(tc%storage, c, (1.0e-4,1.0e-4)))

        call assertion%equal(prefix//":contract_abij_ab_ij_complex64-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0e-4,1.0e-4)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_abij_ab_ij_complex64

    subroutine contract_abij_ab_ij_complex64(c, a, b)
        complex(real32), dimension(:,:,:,:), intent(inout) :: c
        complex(real32), dimension(:,:), intent(in) :: a
        complex(real32), dimension(:,:), intent(in) :: b

        integer :: ij,ib,ia,ii

        do ij = 1, size(c, 4)
            do ib = 1, size(c, 2)
                do ia = 1, size(c, 1)
                    do ii = 1, size(c, 3)
                        c(ia,ib,ii,ij) = c(ia,ib,ii,ij) + &
                                a(ia,ib)*b(ii,ij)
                    end do
                end do
            end do
        end do
    end subroutine contract_abij_ab_ij_complex64

    subroutine test_contract_abij_abej_ei_complex64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(5,3,7,2) :: c
        complex(real32), dimension(5,3,7,2) :: c_easy
        complex(real32), dimension(5,3,11,2) :: a
        complex(real32), dimension(11,7) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real, dimension(5,3,11,2) :: a_real_array, a_imag_array
        real, dimension(11,7) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,b,i,j)=a(a,b,e,j)*b(e,i)", &
                driver, options, priorities)

        call contract_abij_abej_ei_complex64(c, a, b)
        call contract(c_easy, a, b, "c(a,b,i,j)=a(a,b,e,j)*b(e,i)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_abij_abej_ei_complex64:equal", &
                helper%equal(tc%storage, c, (1.0e-4,1.0e-4)))

        call assertion%equal(prefix//":contract_abij_abej_ei_complex64-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0e-4,1.0e-4)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_abij_abej_ei_complex64

    subroutine contract_abij_abej_ei_complex64(c, a, b)
        complex(real32), dimension(:,:,:,:), intent(inout) :: c
        complex(real32), dimension(:,:,:,:), intent(in) :: a
        complex(real32), dimension(:,:), intent(in) :: b

        integer :: ij,ib,ia,ii,ie

        do ij = 1, size(c, 4)
            do ib = 1, size(c, 2)
                do ia = 1, size(c, 1)
                    do ii = 1, size(c, 3)
                        do ie = 1, size(a, 3)
                            c(ia,ib,ii,ij) = c(ia,ib,ii,ij) + &
                                    a(ia,ib,ie,ij)*b(ie,ii)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine contract_abij_abej_ei_complex64

    subroutine test_contract_abij_ijbk_ak_complex64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(5,3,7,2) :: c
        complex(real32), dimension(5,3,7,2) :: c_easy
        complex(real32), dimension(7,2,3,11) :: a
        complex(real32), dimension(5,11) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real, dimension(7,2,3,11) :: a_real_array, a_imag_array
        real, dimension(5,11) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,b,i,j)=a(i,j,b,k)*b(a,k)", &
                driver, options, priorities)

        call contract_abij_ijbk_ak_complex64(c, a, b)
        call contract(c_easy, a, b, "c(a,b,i,j)=a(i,j,b,k)*b(a,k)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_abij_ijbk_ak_complex64:equal", &
                helper%equal(tc%storage, c, (1.0e-4,1.0e-4)))

        call assertion%equal(prefix//":contract_abij_ijbk_ak_complex64-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0e-4,1.0e-4)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_abij_ijbk_ak_complex64

    subroutine contract_abij_ijbk_ak_complex64(c, a, b)
        complex(real32), dimension(:,:,:,:), intent(inout) :: c
        complex(real32), dimension(:,:,:,:), intent(in) :: a
        complex(real32), dimension(:,:), intent(in) :: b

        integer :: ij,ib,ia,ii,ik

        do ij = 1, size(c, 4)
            do ib = 1, size(c, 2)
                do ia = 1, size(c, 1)
                    do ii = 1, size(c, 3)
                        do ik = 1, size(a, 4)
                            c(ia,ib,ii,ij) = c(ia,ib,ii,ij) + &
                                    a(ii,ij,ib,ik)*b(ia,ik)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine contract_abij_ijbk_ak_complex64

    subroutine test_contract_abij_abcd_cdij_complex64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(5,3,7,2) :: c
        complex(real32), dimension(5,3,7,2) :: c_easy
        complex(real32), dimension(5,3,13,11) :: a
        complex(real32), dimension(13,11,7,2) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real, dimension(5,3,13,11) :: a_real_array, a_imag_array
        real, dimension(13,11,7,2) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,b,i,j)=a(a,b,c,d)*b(c,d,i,j)", &
                driver, options, priorities)

        call contract_abij_abcd_cdij_complex64(c, a, b)
        call contract(c_easy, a, b, "c(a,b,i,j)=a(a,b,c,d)*b(c,d,i,j)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_abij_abcd_cdij_complex64:equal", &
                helper%equal(tc%storage, c, (1.0e-4,1.0e-4)))

        call assertion%equal(prefix//":contract_abij_abcd_cdij_complex64-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0e-4,1.0e-4)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_abij_abcd_cdij_complex64

    subroutine contract_abij_abcd_cdij_complex64(c, a, b)
        complex(real32), dimension(:,:,:,:), intent(inout) :: c
        complex(real32), dimension(:,:,:,:), intent(in) :: a
        complex(real32), dimension(:,:,:,:), intent(in) :: b

        integer :: ij,ib,ia,ii,id,ic

        do ij = 1, size(c, 4)
            do ib = 1, size(c, 2)
                do ia = 1, size(c, 1)
                    do ii = 1, size(c, 3)
                        do id = 1, size(a, 4)
                            do ic = 1, size(a, 3)
                                c(ia,ib,ii,ij) = c(ia,ib,ii,ij) + &
                                        a(ia,ib,ic,id)*b(ic,id,ii,ij)
                            end do
                        end do
                    end do
                end do
            end do
        end do
    end subroutine contract_abij_abcd_cdij_complex64

    subroutine test_contract_abij_kbcj_acik_complex64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(5,3,7,2) :: c
        complex(real32), dimension(5,3,7,2) :: c_easy
        complex(real32), dimension(11,3,13,2) :: a
        complex(real32), dimension(5,13,7,11) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real, dimension(11,3,13,2) :: a_real_array, a_imag_array
        real, dimension(5,13,7,11) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,b,i,j)=a(k,b,c,j)*b(a,c,i,k)", &
                driver, options, priorities)

        call contract_abij_kbcj_acik_complex64(c, a, b)
        call contract(c_easy, a, b, "c(a,b,i,j)=a(k,b,c,j)*b(a,c,i,k)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_abij_kbcj_acik_complex64:equal", &
                helper%equal(tc%storage, c, (1.0e-4,1.0e-4)))

        call assertion%equal(prefix//":contract_abij_kbcj_acik_complex64-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0e-4,1.0e-4)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_abij_kbcj_acik_complex64

    subroutine contract_abij_kbcj_acik_complex64(c, a, b)
        complex(real32), dimension(:,:,:,:), intent(inout) :: c
        complex(real32), dimension(:,:,:,:), intent(in) :: a
        complex(real32), dimension(:,:,:,:), intent(in) :: b

        integer :: ij,ib,ia,ii,ik,ic

        do ij = 1, size(c, 4)
            do ib = 1, size(c, 2)
                do ia = 1, size(c, 1)
                    do ii = 1, size(c, 3)
                        do ik = 1, size(a, 1)
                            do ic = 1, size(a, 3)
                                c(ia,ib,ii,ij) = c(ia,ib,ii,ij) + &
                                        a(ik,ib,ic,ij)*b(ia,ic,ii,ik)
                            end do
                        end do
                    end do
                end do
            end do
        end do
    end subroutine contract_abij_kbcj_acik_complex64

    subroutine test_contract__ij_ij_complex64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32) :: c
        complex(real32) :: c_easy
        complex(real32), dimension(3,2) :: a
        complex(real32), dimension(3,2) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real, dimension(3,2) :: a_real_array, a_imag_array
        real, dimension(3,2) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c=a(i,j)*b(i,j)", &
                driver, options, priorities)

        call contract__ij_ij_complex64(c, a, b)
        call contract(c_easy, a, b, "c=a(i,j)*b(i,j)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract__ij_ij_complex64:equal", &
                helper%equal(tc%storage, c, (1.0e-4,1.0e-4)))

        call assertion%equal(prefix//":contract__ij_ij_complex64-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0e-4,1.0e-4)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract__ij_ij_complex64

    subroutine contract__ij_ij_complex64(c, a, b)
        complex(real32), intent(inout) :: c
        complex(real32), dimension(:,:), intent(in) :: a
        complex(real32), dimension(:,:), intent(in) :: b

        integer :: ij,ii

        do ij = 1, size(a, 2)
            do ii = 1, size(a, 1)
                c = c + &
                        a(ii,ij)*b(ii,ij)
            end do
        end do
    end subroutine contract__ij_ij_complex64

    subroutine test_contract_ij__ij_complex64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        complex(real32), dimension(3,2) :: c
        complex(real32), dimension(3,2) :: c_easy
        complex(real32) :: a
        complex(real32), dimension(3,2) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        real :: a_real_array, a_imag_array
        real, dimension(3,2) :: b_real_array, b_imag_array

        call random_number(a_real_array)
        call random_number(a_imag_array)
        call random_number(b_real_array)
        call random_number(b_imag_array)

        a = cmplx(a_real_array, a_imag_array)
        b = cmplx(b_real_array, b_imag_array)
        c = cmplx(0.0, 0.0)
        c_easy = cmplx(0.0, 0.0)

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(i,j)=a*b(i,j)", &
                driver, options, priorities)

        call contract_ij__ij_complex64(c, a, b)
        call contract(c_easy, a, b, "c(i,j)=a*b(i,j)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_ij__ij_complex64:equal", &
                helper%equal(tc%storage, c, (1.0e-4,1.0e-4)))

        call assertion%equal(prefix//":contract_ij__ij_complex64-easy:equal", &
                helper%equal(tc%storage, c_easy, (1.0e-4,1.0e-4)))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_ij__ij_complex64

    subroutine contract_ij__ij_complex64(c, a, b)
        complex(real32), dimension(:,:), intent(inout) :: c
        complex(real32), intent(in) :: a
        complex(real32), dimension(:,:), intent(in) :: b

        integer :: ij,ii

        do ij = 1, size(c, 2)
            do ii = 1, size(c, 1)
                c(ii,ij) = c(ii,ij) + &
                        a*b(ii,ij)
            end do
        end do
    end subroutine contract_ij__ij_complex64

    subroutine test_contract_a_ai_i_real64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2) :: c
        real(real64), dimension(2) :: c_easy
        real(real64), dimension(2,3) :: a
        real(real64), dimension(3) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a)=a(a,i)*b(i)", &
                driver, options, priorities)

        call contract_a_ai_i_real64(c, a, b)
        call contract(c_easy, a, b, "c(a)=a(a,i)*b(i)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_a_ai_i_real64:equal", &
                helper%equal(tc%storage, c, 1.0d-12))

        call assertion%equal(prefix//":contract_a_ai_i_real64-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0d-12))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_a_ai_i_real64

    subroutine contract_a_ai_i_real64(c, a, b)
        real(real64), dimension(:), intent(inout) :: c
        real(real64), dimension(:,:), intent(in) :: a
        real(real64), dimension(:), intent(in) :: b

        integer :: ia,ii

        do ia = 1, size(c, 1)
            do ii = 1, size(a, 2)
                c(ia) = c(ia) + &
                        a(ia,ii)*b(ii)
            end do
        end do
    end subroutine contract_a_ai_i_real64

    subroutine test_contract_i_a_ia_real64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2) :: c
        real(real64), dimension(2) :: c_easy
        real(real64), dimension(3) :: a
        real(real64), dimension(2,3) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(i)=a(a)*b(i,a)", &
                driver, options, priorities)

        call contract_i_a_ia_real64(c, a, b)
        call contract(c_easy, a, b, "c(i)=a(a)*b(i,a)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_i_a_ia_real64:equal", &
                helper%equal(tc%storage, c, 1.0d-12))

        call assertion%equal(prefix//":contract_i_a_ia_real64-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0d-12))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_i_a_ia_real64

    subroutine contract_i_a_ia_real64(c, a, b)
        real(real64), dimension(:), intent(inout) :: c
        real(real64), dimension(:), intent(in) :: a
        real(real64), dimension(:,:), intent(in) :: b

        integer :: ii,ia

        do ii = 1, size(c, 1)
            do ia = 1, size(a, 1)
                c(ii) = c(ii) + &
                        a(ia)*b(ii,ia)
            end do
        end do
    end subroutine contract_i_a_ia_real64

    subroutine test_contract_ai_a_i_real64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3) :: c
        real(real64), dimension(2,3) :: c_easy
        real(real64), dimension(2) :: a
        real(real64), dimension(3) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,i)=a(a)*b(i)", &
                driver, options, priorities)

        call contract_ai_a_i_real64(c, a, b)
        call contract(c_easy, a, b, "c(a,i)=a(a)*b(i)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_ai_a_i_real64:equal", &
                helper%equal(tc%storage, c, 1.0d-12))

        call assertion%equal(prefix//":contract_ai_a_i_real64-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0d-12))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_ai_a_i_real64

    subroutine contract_ai_a_i_real64(c, a, b)
        real(real64), dimension(:,:), intent(inout) :: c
        real(real64), dimension(:), intent(in) :: a
        real(real64), dimension(:), intent(in) :: b

        integer :: ia,ii

        do ia = 1, size(c, 1)
            do ii = 1, size(c, 2)
                c(ia,ii) = c(ia,ii) + &
                        a(ia)*b(ii)
            end do
        end do
    end subroutine contract_ai_a_i_real64

    subroutine test_contract_ia_a_i_real64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(3,2) :: c
        real(real64), dimension(3,2) :: c_easy
        real(real64), dimension(2) :: a
        real(real64), dimension(3) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(i,a)=a(a)*b(i)", &
                driver, options, priorities)

        call contract_ia_a_i_real64(c, a, b)
        call contract(c_easy, a, b, "c(i,a)=a(a)*b(i)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_ia_a_i_real64:equal", &
                helper%equal(tc%storage, c, 1.0d-12))

        call assertion%equal(prefix//":contract_ia_a_i_real64-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0d-12))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_ia_a_i_real64

    subroutine contract_ia_a_i_real64(c, a, b)
        real(real64), dimension(:,:), intent(inout) :: c
        real(real64), dimension(:), intent(in) :: a
        real(real64), dimension(:), intent(in) :: b

        integer :: ia,ii

        do ia = 1, size(c, 2)
            do ii = 1, size(c, 1)
                c(ii,ia) = c(ii,ia) + &
                        a(ia)*b(ii)
            end do
        end do
    end subroutine contract_ia_a_i_real64

    subroutine test_contract_ai_ae_ei_real64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3) :: c
        real(real64), dimension(2,3) :: c_easy
        real(real64), dimension(2,5) :: a
        real(real64), dimension(5,3) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,i)=a(a,e)*b(e,i)", &
                driver, options, priorities)

        call contract_ai_ae_ei_real64(c, a, b)
        call contract(c_easy, a, b, "c(a,i)=a(a,e)*b(e,i)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_ai_ae_ei_real64:equal", &
                helper%equal(tc%storage, c, 1.0d-12))

        call assertion%equal(prefix//":contract_ai_ae_ei_real64-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0d-12))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_ai_ae_ei_real64

    subroutine contract_ai_ae_ei_real64(c, a, b)
        real(real64), dimension(:,:), intent(inout) :: c
        real(real64), dimension(:,:), intent(in) :: a
        real(real64), dimension(:,:), intent(in) :: b

        integer :: ia,ii,ie

        do ia = 1, size(c, 1)
            do ii = 1, size(c, 2)
                do ie = 1, size(a, 2)
                    c(ia,ii) = c(ia,ii) + &
                            a(ia,ie)*b(ie,ii)
                end do
            end do
        end do
    end subroutine contract_ai_ae_ei_real64

    subroutine test_contract_ia_ae_ie_real64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(3,2) :: c
        real(real64), dimension(3,2) :: c_easy
        real(real64), dimension(2,5) :: a
        real(real64), dimension(3,5) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(i,a)=a(a,e)*b(i,e)", &
                driver, options, priorities)

        call contract_ia_ae_ie_real64(c, a, b)
        call contract(c_easy, a, b, "c(i,a)=a(a,e)*b(i,e)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_ia_ae_ie_real64:equal", &
                helper%equal(tc%storage, c, 1.0d-12))

        call assertion%equal(prefix//":contract_ia_ae_ie_real64-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0d-12))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_ia_ae_ie_real64

    subroutine contract_ia_ae_ie_real64(c, a, b)
        real(real64), dimension(:,:), intent(inout) :: c
        real(real64), dimension(:,:), intent(in) :: a
        real(real64), dimension(:,:), intent(in) :: b

        integer :: ia,ii,ie

        do ia = 1, size(c, 2)
            do ii = 1, size(c, 1)
                do ie = 1, size(a, 2)
                    c(ii,ia) = c(ii,ia) + &
                            a(ia,ie)*b(ii,ie)
                end do
            end do
        end do
    end subroutine contract_ia_ae_ie_real64

    subroutine test_contract_ai_aef_efi_real64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3) :: c
        real(real64), dimension(2,3) :: c_easy
        real(real64), dimension(2,7,5) :: a
        real(real64), dimension(7,5,3) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,i)=a(a,e,f)*b(e,f,i)", &
                driver, options, priorities)

        call contract_ai_aef_efi_real64(c, a, b)
        call contract(c_easy, a, b, "c(a,i)=a(a,e,f)*b(e,f,i)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_ai_aef_efi_real64:equal", &
                helper%equal(tc%storage, c, 1.0d-12))

        call assertion%equal(prefix//":contract_ai_aef_efi_real64-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0d-12))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_ai_aef_efi_real64

    subroutine contract_ai_aef_efi_real64(c, a, b)
        real(real64), dimension(:,:), intent(inout) :: c
        real(real64), dimension(:,:,:), intent(in) :: a
        real(real64), dimension(:,:,:), intent(in) :: b

        integer :: ia,ii,if,ie

        do ia = 1, size(c, 1)
            do ii = 1, size(c, 2)
                do if = 1, size(a, 3)
                    do ie = 1, size(a, 2)
                        c(ia,ii) = c(ia,ii) + &
                                a(ia,ie,if)*b(ie,if,ii)
                    end do
                end do
            end do
        end do
    end subroutine contract_ai_aef_efi_real64

    subroutine test_contract_ai_eaf_ife_real64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3) :: c
        real(real64), dimension(2,3) :: c_easy
        real(real64), dimension(7,2,5) :: a
        real(real64), dimension(3,5,7) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,i)=a(e,a,f)*b(i,f,e)", &
                driver, options, priorities)

        call contract_ai_eaf_ife_real64(c, a, b)
        call contract(c_easy, a, b, "c(a,i)=a(e,a,f)*b(i,f,e)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_ai_eaf_ife_real64:equal", &
                helper%equal(tc%storage, c, 1.0d-12))

        call assertion%equal(prefix//":contract_ai_eaf_ife_real64-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0d-12))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_ai_eaf_ife_real64

    subroutine contract_ai_eaf_ife_real64(c, a, b)
        real(real64), dimension(:,:), intent(inout) :: c
        real(real64), dimension(:,:,:), intent(in) :: a
        real(real64), dimension(:,:,:), intent(in) :: b

        integer :: ia,ii,if,ie

        do ia = 1, size(c, 1)
            do ii = 1, size(c, 2)
                do if = 1, size(a, 3)
                    do ie = 1, size(a, 1)
                        c(ia,ii) = c(ia,ii) + &
                                a(ie,ia,if)*b(ii,if,ie)
                    end do
                end do
            end do
        end do
    end subroutine contract_ai_eaf_ife_real64

    subroutine test_contract_abi_ea_eib_real64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(3,2,5) :: c
        real(real64), dimension(3,2,5) :: c_easy
        real(real64), dimension(7,3) :: a
        real(real64), dimension(7,5,2) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,b,i)=a(e,a)*b(e,i,b)", &
                driver, options, priorities)

        call contract_abi_ea_eib_real64(c, a, b)
        call contract(c_easy, a, b, "c(a,b,i)=a(e,a)*b(e,i,b)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_abi_ea_eib_real64:equal", &
                helper%equal(tc%storage, c, 1.0d-12))

        call assertion%equal(prefix//":contract_abi_ea_eib_real64-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0d-12))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_abi_ea_eib_real64

    subroutine contract_abi_ea_eib_real64(c, a, b)
        real(real64), dimension(:,:,:), intent(inout) :: c
        real(real64), dimension(:,:), intent(in) :: a
        real(real64), dimension(:,:,:), intent(in) :: b

        integer :: ib,ia,ii,ie

        do ib = 1, size(c, 2)
            do ia = 1, size(c, 1)
                do ii = 1, size(c, 3)
                    do ie = 1, size(a, 1)
                        c(ia,ib,ii) = c(ia,ib,ii) + &
                                a(ie,ia)*b(ie,ii,ib)
                    end do
                end do
            end do
        end do
    end subroutine contract_abi_ea_eib_real64

    subroutine test_contract_abi_bea_ei_real64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(3,2,5) :: c
        real(real64), dimension(3,2,5) :: c_easy
        real(real64), dimension(2,7,3) :: a
        real(real64), dimension(7,5) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,b,i)=a(b,e,a)*b(e,i)", &
                driver, options, priorities)

        call contract_abi_bea_ei_real64(c, a, b)
        call contract(c_easy, a, b, "c(a,b,i)=a(b,e,a)*b(e,i)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_abi_bea_ei_real64:equal", &
                helper%equal(tc%storage, c, 1.0d-12))

        call assertion%equal(prefix//":contract_abi_bea_ei_real64-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0d-12))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_abi_bea_ei_real64

    subroutine contract_abi_bea_ei_real64(c, a, b)
        real(real64), dimension(:,:,:), intent(inout) :: c
        real(real64), dimension(:,:,:), intent(in) :: a
        real(real64), dimension(:,:), intent(in) :: b

        integer :: ib,ia,ii,ie

        do ib = 1, size(c, 2)
            do ia = 1, size(c, 1)
                do ii = 1, size(c, 3)
                    do ie = 1, size(a, 2)
                        c(ia,ib,ii) = c(ia,ib,ii) + &
                                a(ib,ie,ia)*b(ie,ii)
                    end do
                end do
            end do
        end do
    end subroutine contract_abi_bea_ei_real64

    subroutine test_contract_ai_aeil_el_real64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(2,3) :: c
        real(real64), dimension(2,3) :: c_easy
        real(real64), dimension(2,7,3,5) :: a
        real(real64), dimension(7,5) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,i)=a(a,e,i,l)*b(e,l)", &
                driver, options, priorities)

        call contract_ai_aeil_el_real64(c, a, b)
        call contract(c_easy, a, b, "c(a,i)=a(a,e,i,l)*b(e,l)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_ai_aeil_el_real64:equal", &
                helper%equal(tc%storage, c, 1.0d-12))

        call assertion%equal(prefix//":contract_ai_aeil_el_real64-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0d-12))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_ai_aeil_el_real64

    subroutine contract_ai_aeil_el_real64(c, a, b)
        real(real64), dimension(:,:), intent(inout) :: c
        real(real64), dimension(:,:,:,:), intent(in) :: a
        real(real64), dimension(:,:), intent(in) :: b

        integer :: ia,ii,il,ie

        do ia = 1, size(c, 1)
            do ii = 1, size(c, 2)
                do il = 1, size(a, 4)
                    do ie = 1, size(a, 2)
                        c(ia,ii) = c(ia,ii) + &
                                a(ia,ie,ii,il)*b(ie,il)
                    end do
                end do
            end do
        end do
    end subroutine contract_ai_aeil_el_real64

    subroutine test_contract_ab_ambe_em_real64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(3,2) :: c
        real(real64), dimension(3,2) :: c_easy
        real(real64), dimension(3,5,2,7) :: a
        real(real64), dimension(7,5) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,b)=a(a,m,b,e)*b(e,m)", &
                driver, options, priorities)

        call contract_ab_ambe_em_real64(c, a, b)
        call contract(c_easy, a, b, "c(a,b)=a(a,m,b,e)*b(e,m)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_ab_ambe_em_real64:equal", &
                helper%equal(tc%storage, c, 1.0d-12))

        call assertion%equal(prefix//":contract_ab_ambe_em_real64-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0d-12))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_ab_ambe_em_real64

    subroutine contract_ab_ambe_em_real64(c, a, b)
        real(real64), dimension(:,:), intent(inout) :: c
        real(real64), dimension(:,:,:,:), intent(in) :: a
        real(real64), dimension(:,:), intent(in) :: b

        integer :: ib,ia,im,ie

        do ib = 1, size(c, 2)
            do ia = 1, size(c, 1)
                do im = 1, size(a, 2)
                    do ie = 1, size(a, 4)
                        c(ia,ib) = c(ia,ib) + &
                                a(ia,im,ib,ie)*b(ie,im)
                    end do
                end do
            end do
        end do
    end subroutine contract_ab_ambe_em_real64

    subroutine test_contract_abij_ab_ij_real64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(5,3,7,2) :: c
        real(real64), dimension(5,3,7,2) :: c_easy
        real(real64), dimension(5,3) :: a
        real(real64), dimension(7,2) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,b,i,j)=a(a,b)*b(i,j)", &
                driver, options, priorities)

        call contract_abij_ab_ij_real64(c, a, b)
        call contract(c_easy, a, b, "c(a,b,i,j)=a(a,b)*b(i,j)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_abij_ab_ij_real64:equal", &
                helper%equal(tc%storage, c, 1.0d-12))

        call assertion%equal(prefix//":contract_abij_ab_ij_real64-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0d-12))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_abij_ab_ij_real64

    subroutine contract_abij_ab_ij_real64(c, a, b)
        real(real64), dimension(:,:,:,:), intent(inout) :: c
        real(real64), dimension(:,:), intent(in) :: a
        real(real64), dimension(:,:), intent(in) :: b

        integer :: ij,ib,ia,ii

        do ij = 1, size(c, 4)
            do ib = 1, size(c, 2)
                do ia = 1, size(c, 1)
                    do ii = 1, size(c, 3)
                        c(ia,ib,ii,ij) = c(ia,ib,ii,ij) + &
                                a(ia,ib)*b(ii,ij)
                    end do
                end do
            end do
        end do
    end subroutine contract_abij_ab_ij_real64

    subroutine test_contract_abij_abej_ei_real64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(5,3,7,2) :: c
        real(real64), dimension(5,3,7,2) :: c_easy
        real(real64), dimension(5,3,11,2) :: a
        real(real64), dimension(11,7) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,b,i,j)=a(a,b,e,j)*b(e,i)", &
                driver, options, priorities)

        call contract_abij_abej_ei_real64(c, a, b)
        call contract(c_easy, a, b, "c(a,b,i,j)=a(a,b,e,j)*b(e,i)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_abij_abej_ei_real64:equal", &
                helper%equal(tc%storage, c, 1.0d-12))

        call assertion%equal(prefix//":contract_abij_abej_ei_real64-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0d-12))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_abij_abej_ei_real64

    subroutine contract_abij_abej_ei_real64(c, a, b)
        real(real64), dimension(:,:,:,:), intent(inout) :: c
        real(real64), dimension(:,:,:,:), intent(in) :: a
        real(real64), dimension(:,:), intent(in) :: b

        integer :: ij,ib,ia,ii,ie

        do ij = 1, size(c, 4)
            do ib = 1, size(c, 2)
                do ia = 1, size(c, 1)
                    do ii = 1, size(c, 3)
                        do ie = 1, size(a, 3)
                            c(ia,ib,ii,ij) = c(ia,ib,ii,ij) + &
                                    a(ia,ib,ie,ij)*b(ie,ii)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine contract_abij_abej_ei_real64

    subroutine test_contract_abij_ijbk_ak_real64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(5,3,7,2) :: c
        real(real64), dimension(5,3,7,2) :: c_easy
        real(real64), dimension(7,2,3,11) :: a
        real(real64), dimension(5,11) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,b,i,j)=a(i,j,b,k)*b(a,k)", &
                driver, options, priorities)

        call contract_abij_ijbk_ak_real64(c, a, b)
        call contract(c_easy, a, b, "c(a,b,i,j)=a(i,j,b,k)*b(a,k)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_abij_ijbk_ak_real64:equal", &
                helper%equal(tc%storage, c, 1.0d-12))

        call assertion%equal(prefix//":contract_abij_ijbk_ak_real64-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0d-12))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_abij_ijbk_ak_real64

    subroutine contract_abij_ijbk_ak_real64(c, a, b)
        real(real64), dimension(:,:,:,:), intent(inout) :: c
        real(real64), dimension(:,:,:,:), intent(in) :: a
        real(real64), dimension(:,:), intent(in) :: b

        integer :: ij,ib,ia,ii,ik

        do ij = 1, size(c, 4)
            do ib = 1, size(c, 2)
                do ia = 1, size(c, 1)
                    do ii = 1, size(c, 3)
                        do ik = 1, size(a, 4)
                            c(ia,ib,ii,ij) = c(ia,ib,ii,ij) + &
                                    a(ii,ij,ib,ik)*b(ia,ik)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine contract_abij_ijbk_ak_real64

    subroutine test_contract_abij_abcd_cdij_real64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(5,3,7,2) :: c
        real(real64), dimension(5,3,7,2) :: c_easy
        real(real64), dimension(5,3,13,11) :: a
        real(real64), dimension(13,11,7,2) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,b,i,j)=a(a,b,c,d)*b(c,d,i,j)", &
                driver, options, priorities)

        call contract_abij_abcd_cdij_real64(c, a, b)
        call contract(c_easy, a, b, "c(a,b,i,j)=a(a,b,c,d)*b(c,d,i,j)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_abij_abcd_cdij_real64:equal", &
                helper%equal(tc%storage, c, 1.0d-12))

        call assertion%equal(prefix//":contract_abij_abcd_cdij_real64-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0d-12))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_abij_abcd_cdij_real64

    subroutine contract_abij_abcd_cdij_real64(c, a, b)
        real(real64), dimension(:,:,:,:), intent(inout) :: c
        real(real64), dimension(:,:,:,:), intent(in) :: a
        real(real64), dimension(:,:,:,:), intent(in) :: b

        integer :: ij,ib,ia,ii,id,ic

        do ij = 1, size(c, 4)
            do ib = 1, size(c, 2)
                do ia = 1, size(c, 1)
                    do ii = 1, size(c, 3)
                        do id = 1, size(a, 4)
                            do ic = 1, size(a, 3)
                                c(ia,ib,ii,ij) = c(ia,ib,ii,ij) + &
                                        a(ia,ib,ic,id)*b(ic,id,ii,ij)
                            end do
                        end do
                    end do
                end do
            end do
        end do
    end subroutine contract_abij_abcd_cdij_real64

    subroutine test_contract_abij_kbcj_acik_real64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(5,3,7,2) :: c
        real(real64), dimension(5,3,7,2) :: c_easy
        real(real64), dimension(11,3,13,2) :: a
        real(real64), dimension(5,13,7,11) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,b,i,j)=a(k,b,c,j)*b(a,c,i,k)", &
                driver, options, priorities)

        call contract_abij_kbcj_acik_real64(c, a, b)
        call contract(c_easy, a, b, "c(a,b,i,j)=a(k,b,c,j)*b(a,c,i,k)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_abij_kbcj_acik_real64:equal", &
                helper%equal(tc%storage, c, 1.0d-12))

        call assertion%equal(prefix//":contract_abij_kbcj_acik_real64-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0d-12))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_abij_kbcj_acik_real64

    subroutine contract_abij_kbcj_acik_real64(c, a, b)
        real(real64), dimension(:,:,:,:), intent(inout) :: c
        real(real64), dimension(:,:,:,:), intent(in) :: a
        real(real64), dimension(:,:,:,:), intent(in) :: b

        integer :: ij,ib,ia,ii,ik,ic

        do ij = 1, size(c, 4)
            do ib = 1, size(c, 2)
                do ia = 1, size(c, 1)
                    do ii = 1, size(c, 3)
                        do ik = 1, size(a, 1)
                            do ic = 1, size(a, 3)
                                c(ia,ib,ii,ij) = c(ia,ib,ii,ij) + &
                                        a(ik,ib,ic,ij)*b(ia,ic,ii,ik)
                            end do
                        end do
                    end do
                end do
            end do
        end do
    end subroutine contract_abij_kbcj_acik_real64

    subroutine test_contract__ij_ij_real64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64) :: c
        real(real64) :: c_easy
        real(real64), dimension(3,2) :: a
        real(real64), dimension(3,2) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c=a(i,j)*b(i,j)", &
                driver, options, priorities)

        call contract__ij_ij_real64(c, a, b)
        call contract(c_easy, a, b, "c=a(i,j)*b(i,j)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract__ij_ij_real64:equal", &
                helper%equal(tc%storage, c, 1.0d-12))

        call assertion%equal(prefix//":contract__ij_ij_real64-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0d-12))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract__ij_ij_real64

    subroutine contract__ij_ij_real64(c, a, b)
        real(real64), intent(inout) :: c
        real(real64), dimension(:,:), intent(in) :: a
        real(real64), dimension(:,:), intent(in) :: b

        integer :: ij,ii

        do ij = 1, size(a, 2)
            do ii = 1, size(a, 1)
                c = c + &
                        a(ii,ij)*b(ii,ij)
            end do
        end do
    end subroutine contract__ij_ij_real64

    subroutine test_contract_ij__ij_real64(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real64), dimension(3,2) :: c
        real(real64), dimension(3,2) :: c_easy
        real(real64) :: a
        real(real64), dimension(3,2) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(i,j)=a*b(i,j)", &
                driver, options, priorities)

        call contract_ij__ij_real64(c, a, b)
        call contract(c_easy, a, b, "c(i,j)=a*b(i,j)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_ij__ij_real64:equal", &
                helper%equal(tc%storage, c, 1.0d-12))

        call assertion%equal(prefix//":contract_ij__ij_real64-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0d-12))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_ij__ij_real64

    subroutine contract_ij__ij_real64(c, a, b)
        real(real64), dimension(:,:), intent(inout) :: c
        real(real64), intent(in) :: a
        real(real64), dimension(:,:), intent(in) :: b

        integer :: ij,ii

        do ij = 1, size(c, 2)
            do ii = 1, size(c, 1)
                c(ii,ij) = c(ii,ij) + &
                        a*b(ii,ij)
            end do
        end do
    end subroutine contract_ij__ij_real64

    subroutine test_contract_a_ai_i_real32(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2) :: c
        real(real32), dimension(2) :: c_easy
        real(real32), dimension(2,3) :: a
        real(real32), dimension(3) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a)=a(a,i)*b(i)", &
                driver, options, priorities)

        call contract_a_ai_i_real32(c, a, b)
        call contract(c_easy, a, b, "c(a)=a(a,i)*b(i)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_a_ai_i_real32:equal", &
                helper%equal(tc%storage, c, 1.0e-4))

        call assertion%equal(prefix//":contract_a_ai_i_real32-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0e-4))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_a_ai_i_real32

    subroutine contract_a_ai_i_real32(c, a, b)
        real(real32), dimension(:), intent(inout) :: c
        real(real32), dimension(:,:), intent(in) :: a
        real(real32), dimension(:), intent(in) :: b

        integer :: ia,ii

        do ia = 1, size(c, 1)
            do ii = 1, size(a, 2)
                c(ia) = c(ia) + &
                        a(ia,ii)*b(ii)
            end do
        end do
    end subroutine contract_a_ai_i_real32

    subroutine test_contract_i_a_ia_real32(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2) :: c
        real(real32), dimension(2) :: c_easy
        real(real32), dimension(3) :: a
        real(real32), dimension(2,3) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(i)=a(a)*b(i,a)", &
                driver, options, priorities)

        call contract_i_a_ia_real32(c, a, b)
        call contract(c_easy, a, b, "c(i)=a(a)*b(i,a)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_i_a_ia_real32:equal", &
                helper%equal(tc%storage, c, 1.0e-4))

        call assertion%equal(prefix//":contract_i_a_ia_real32-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0e-4))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_i_a_ia_real32

    subroutine contract_i_a_ia_real32(c, a, b)
        real(real32), dimension(:), intent(inout) :: c
        real(real32), dimension(:), intent(in) :: a
        real(real32), dimension(:,:), intent(in) :: b

        integer :: ii,ia

        do ii = 1, size(c, 1)
            do ia = 1, size(a, 1)
                c(ii) = c(ii) + &
                        a(ia)*b(ii,ia)
            end do
        end do
    end subroutine contract_i_a_ia_real32

    subroutine test_contract_ai_a_i_real32(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3) :: c
        real(real32), dimension(2,3) :: c_easy
        real(real32), dimension(2) :: a
        real(real32), dimension(3) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,i)=a(a)*b(i)", &
                driver, options, priorities)

        call contract_ai_a_i_real32(c, a, b)
        call contract(c_easy, a, b, "c(a,i)=a(a)*b(i)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_ai_a_i_real32:equal", &
                helper%equal(tc%storage, c, 1.0e-4))

        call assertion%equal(prefix//":contract_ai_a_i_real32-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0e-4))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_ai_a_i_real32

    subroutine contract_ai_a_i_real32(c, a, b)
        real(real32), dimension(:,:), intent(inout) :: c
        real(real32), dimension(:), intent(in) :: a
        real(real32), dimension(:), intent(in) :: b

        integer :: ia,ii

        do ia = 1, size(c, 1)
            do ii = 1, size(c, 2)
                c(ia,ii) = c(ia,ii) + &
                        a(ia)*b(ii)
            end do
        end do
    end subroutine contract_ai_a_i_real32

    subroutine test_contract_ia_a_i_real32(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(3,2) :: c
        real(real32), dimension(3,2) :: c_easy
        real(real32), dimension(2) :: a
        real(real32), dimension(3) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(i,a)=a(a)*b(i)", &
                driver, options, priorities)

        call contract_ia_a_i_real32(c, a, b)
        call contract(c_easy, a, b, "c(i,a)=a(a)*b(i)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_ia_a_i_real32:equal", &
                helper%equal(tc%storage, c, 1.0e-4))

        call assertion%equal(prefix//":contract_ia_a_i_real32-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0e-4))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_ia_a_i_real32

    subroutine contract_ia_a_i_real32(c, a, b)
        real(real32), dimension(:,:), intent(inout) :: c
        real(real32), dimension(:), intent(in) :: a
        real(real32), dimension(:), intent(in) :: b

        integer :: ia,ii

        do ia = 1, size(c, 2)
            do ii = 1, size(c, 1)
                c(ii,ia) = c(ii,ia) + &
                        a(ia)*b(ii)
            end do
        end do
    end subroutine contract_ia_a_i_real32

    subroutine test_contract_ai_ae_ei_real32(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3) :: c
        real(real32), dimension(2,3) :: c_easy
        real(real32), dimension(2,5) :: a
        real(real32), dimension(5,3) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,i)=a(a,e)*b(e,i)", &
                driver, options, priorities)

        call contract_ai_ae_ei_real32(c, a, b)
        call contract(c_easy, a, b, "c(a,i)=a(a,e)*b(e,i)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_ai_ae_ei_real32:equal", &
                helper%equal(tc%storage, c, 1.0e-4))

        call assertion%equal(prefix//":contract_ai_ae_ei_real32-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0e-4))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_ai_ae_ei_real32

    subroutine contract_ai_ae_ei_real32(c, a, b)
        real(real32), dimension(:,:), intent(inout) :: c
        real(real32), dimension(:,:), intent(in) :: a
        real(real32), dimension(:,:), intent(in) :: b

        integer :: ia,ii,ie

        do ia = 1, size(c, 1)
            do ii = 1, size(c, 2)
                do ie = 1, size(a, 2)
                    c(ia,ii) = c(ia,ii) + &
                            a(ia,ie)*b(ie,ii)
                end do
            end do
        end do
    end subroutine contract_ai_ae_ei_real32

    subroutine test_contract_ia_ae_ie_real32(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(3,2) :: c
        real(real32), dimension(3,2) :: c_easy
        real(real32), dimension(2,5) :: a
        real(real32), dimension(3,5) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(i,a)=a(a,e)*b(i,e)", &
                driver, options, priorities)

        call contract_ia_ae_ie_real32(c, a, b)
        call contract(c_easy, a, b, "c(i,a)=a(a,e)*b(i,e)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_ia_ae_ie_real32:equal", &
                helper%equal(tc%storage, c, 1.0e-4))

        call assertion%equal(prefix//":contract_ia_ae_ie_real32-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0e-4))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_ia_ae_ie_real32

    subroutine contract_ia_ae_ie_real32(c, a, b)
        real(real32), dimension(:,:), intent(inout) :: c
        real(real32), dimension(:,:), intent(in) :: a
        real(real32), dimension(:,:), intent(in) :: b

        integer :: ia,ii,ie

        do ia = 1, size(c, 2)
            do ii = 1, size(c, 1)
                do ie = 1, size(a, 2)
                    c(ii,ia) = c(ii,ia) + &
                            a(ia,ie)*b(ii,ie)
                end do
            end do
        end do
    end subroutine contract_ia_ae_ie_real32

    subroutine test_contract_ai_aef_efi_real32(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3) :: c
        real(real32), dimension(2,3) :: c_easy
        real(real32), dimension(2,7,5) :: a
        real(real32), dimension(7,5,3) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,i)=a(a,e,f)*b(e,f,i)", &
                driver, options, priorities)

        call contract_ai_aef_efi_real32(c, a, b)
        call contract(c_easy, a, b, "c(a,i)=a(a,e,f)*b(e,f,i)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_ai_aef_efi_real32:equal", &
                helper%equal(tc%storage, c, 1.0e-4))

        call assertion%equal(prefix//":contract_ai_aef_efi_real32-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0e-4))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_ai_aef_efi_real32

    subroutine contract_ai_aef_efi_real32(c, a, b)
        real(real32), dimension(:,:), intent(inout) :: c
        real(real32), dimension(:,:,:), intent(in) :: a
        real(real32), dimension(:,:,:), intent(in) :: b

        integer :: ia,ii,if,ie

        do ia = 1, size(c, 1)
            do ii = 1, size(c, 2)
                do if = 1, size(a, 3)
                    do ie = 1, size(a, 2)
                        c(ia,ii) = c(ia,ii) + &
                                a(ia,ie,if)*b(ie,if,ii)
                    end do
                end do
            end do
        end do
    end subroutine contract_ai_aef_efi_real32

    subroutine test_contract_ai_eaf_ife_real32(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3) :: c
        real(real32), dimension(2,3) :: c_easy
        real(real32), dimension(7,2,5) :: a
        real(real32), dimension(3,5,7) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,i)=a(e,a,f)*b(i,f,e)", &
                driver, options, priorities)

        call contract_ai_eaf_ife_real32(c, a, b)
        call contract(c_easy, a, b, "c(a,i)=a(e,a,f)*b(i,f,e)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_ai_eaf_ife_real32:equal", &
                helper%equal(tc%storage, c, 1.0e-4))

        call assertion%equal(prefix//":contract_ai_eaf_ife_real32-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0e-4))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_ai_eaf_ife_real32

    subroutine contract_ai_eaf_ife_real32(c, a, b)
        real(real32), dimension(:,:), intent(inout) :: c
        real(real32), dimension(:,:,:), intent(in) :: a
        real(real32), dimension(:,:,:), intent(in) :: b

        integer :: ia,ii,if,ie

        do ia = 1, size(c, 1)
            do ii = 1, size(c, 2)
                do if = 1, size(a, 3)
                    do ie = 1, size(a, 1)
                        c(ia,ii) = c(ia,ii) + &
                                a(ie,ia,if)*b(ii,if,ie)
                    end do
                end do
            end do
        end do
    end subroutine contract_ai_eaf_ife_real32

    subroutine test_contract_abi_ea_eib_real32(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(3,2,5) :: c
        real(real32), dimension(3,2,5) :: c_easy
        real(real32), dimension(7,3) :: a
        real(real32), dimension(7,5,2) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,b,i)=a(e,a)*b(e,i,b)", &
                driver, options, priorities)

        call contract_abi_ea_eib_real32(c, a, b)
        call contract(c_easy, a, b, "c(a,b,i)=a(e,a)*b(e,i,b)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_abi_ea_eib_real32:equal", &
                helper%equal(tc%storage, c, 1.0e-4))

        call assertion%equal(prefix//":contract_abi_ea_eib_real32-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0e-4))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_abi_ea_eib_real32

    subroutine contract_abi_ea_eib_real32(c, a, b)
        real(real32), dimension(:,:,:), intent(inout) :: c
        real(real32), dimension(:,:), intent(in) :: a
        real(real32), dimension(:,:,:), intent(in) :: b

        integer :: ib,ia,ii,ie

        do ib = 1, size(c, 2)
            do ia = 1, size(c, 1)
                do ii = 1, size(c, 3)
                    do ie = 1, size(a, 1)
                        c(ia,ib,ii) = c(ia,ib,ii) + &
                                a(ie,ia)*b(ie,ii,ib)
                    end do
                end do
            end do
        end do
    end subroutine contract_abi_ea_eib_real32

    subroutine test_contract_abi_bea_ei_real32(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(3,2,5) :: c
        real(real32), dimension(3,2,5) :: c_easy
        real(real32), dimension(2,7,3) :: a
        real(real32), dimension(7,5) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,b,i)=a(b,e,a)*b(e,i)", &
                driver, options, priorities)

        call contract_abi_bea_ei_real32(c, a, b)
        call contract(c_easy, a, b, "c(a,b,i)=a(b,e,a)*b(e,i)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_abi_bea_ei_real32:equal", &
                helper%equal(tc%storage, c, 1.0e-4))

        call assertion%equal(prefix//":contract_abi_bea_ei_real32-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0e-4))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_abi_bea_ei_real32

    subroutine contract_abi_bea_ei_real32(c, a, b)
        real(real32), dimension(:,:,:), intent(inout) :: c
        real(real32), dimension(:,:,:), intent(in) :: a
        real(real32), dimension(:,:), intent(in) :: b

        integer :: ib,ia,ii,ie

        do ib = 1, size(c, 2)
            do ia = 1, size(c, 1)
                do ii = 1, size(c, 3)
                    do ie = 1, size(a, 2)
                        c(ia,ib,ii) = c(ia,ib,ii) + &
                                a(ib,ie,ia)*b(ie,ii)
                    end do
                end do
            end do
        end do
    end subroutine contract_abi_bea_ei_real32

    subroutine test_contract_ai_aeil_el_real32(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(2,3) :: c
        real(real32), dimension(2,3) :: c_easy
        real(real32), dimension(2,7,3,5) :: a
        real(real32), dimension(7,5) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,i)=a(a,e,i,l)*b(e,l)", &
                driver, options, priorities)

        call contract_ai_aeil_el_real32(c, a, b)
        call contract(c_easy, a, b, "c(a,i)=a(a,e,i,l)*b(e,l)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_ai_aeil_el_real32:equal", &
                helper%equal(tc%storage, c, 1.0e-4))

        call assertion%equal(prefix//":contract_ai_aeil_el_real32-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0e-4))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_ai_aeil_el_real32

    subroutine contract_ai_aeil_el_real32(c, a, b)
        real(real32), dimension(:,:), intent(inout) :: c
        real(real32), dimension(:,:,:,:), intent(in) :: a
        real(real32), dimension(:,:), intent(in) :: b

        integer :: ia,ii,il,ie

        do ia = 1, size(c, 1)
            do ii = 1, size(c, 2)
                do il = 1, size(a, 4)
                    do ie = 1, size(a, 2)
                        c(ia,ii) = c(ia,ii) + &
                                a(ia,ie,ii,il)*b(ie,il)
                    end do
                end do
            end do
        end do
    end subroutine contract_ai_aeil_el_real32

    subroutine test_contract_ab_ambe_em_real32(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(3,2) :: c
        real(real32), dimension(3,2) :: c_easy
        real(real32), dimension(3,5,2,7) :: a
        real(real32), dimension(7,5) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,b)=a(a,m,b,e)*b(e,m)", &
                driver, options, priorities)

        call contract_ab_ambe_em_real32(c, a, b)
        call contract(c_easy, a, b, "c(a,b)=a(a,m,b,e)*b(e,m)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_ab_ambe_em_real32:equal", &
                helper%equal(tc%storage, c, 1.0e-4))

        call assertion%equal(prefix//":contract_ab_ambe_em_real32-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0e-4))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_ab_ambe_em_real32

    subroutine contract_ab_ambe_em_real32(c, a, b)
        real(real32), dimension(:,:), intent(inout) :: c
        real(real32), dimension(:,:,:,:), intent(in) :: a
        real(real32), dimension(:,:), intent(in) :: b

        integer :: ib,ia,im,ie

        do ib = 1, size(c, 2)
            do ia = 1, size(c, 1)
                do im = 1, size(a, 2)
                    do ie = 1, size(a, 4)
                        c(ia,ib) = c(ia,ib) + &
                                a(ia,im,ib,ie)*b(ie,im)
                    end do
                end do
            end do
        end do
    end subroutine contract_ab_ambe_em_real32

    subroutine test_contract_abij_ab_ij_real32(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(5,3,7,2) :: c
        real(real32), dimension(5,3,7,2) :: c_easy
        real(real32), dimension(5,3) :: a
        real(real32), dimension(7,2) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,b,i,j)=a(a,b)*b(i,j)", &
                driver, options, priorities)

        call contract_abij_ab_ij_real32(c, a, b)
        call contract(c_easy, a, b, "c(a,b,i,j)=a(a,b)*b(i,j)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_abij_ab_ij_real32:equal", &
                helper%equal(tc%storage, c, 1.0e-4))

        call assertion%equal(prefix//":contract_abij_ab_ij_real32-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0e-4))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_abij_ab_ij_real32

    subroutine contract_abij_ab_ij_real32(c, a, b)
        real(real32), dimension(:,:,:,:), intent(inout) :: c
        real(real32), dimension(:,:), intent(in) :: a
        real(real32), dimension(:,:), intent(in) :: b

        integer :: ij,ib,ia,ii

        do ij = 1, size(c, 4)
            do ib = 1, size(c, 2)
                do ia = 1, size(c, 1)
                    do ii = 1, size(c, 3)
                        c(ia,ib,ii,ij) = c(ia,ib,ii,ij) + &
                                a(ia,ib)*b(ii,ij)
                    end do
                end do
            end do
        end do
    end subroutine contract_abij_ab_ij_real32

    subroutine test_contract_abij_abej_ei_real32(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(5,3,7,2) :: c
        real(real32), dimension(5,3,7,2) :: c_easy
        real(real32), dimension(5,3,11,2) :: a
        real(real32), dimension(11,7) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,b,i,j)=a(a,b,e,j)*b(e,i)", &
                driver, options, priorities)

        call contract_abij_abej_ei_real32(c, a, b)
        call contract(c_easy, a, b, "c(a,b,i,j)=a(a,b,e,j)*b(e,i)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_abij_abej_ei_real32:equal", &
                helper%equal(tc%storage, c, 1.0e-4))

        call assertion%equal(prefix//":contract_abij_abej_ei_real32-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0e-4))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_abij_abej_ei_real32

    subroutine contract_abij_abej_ei_real32(c, a, b)
        real(real32), dimension(:,:,:,:), intent(inout) :: c
        real(real32), dimension(:,:,:,:), intent(in) :: a
        real(real32), dimension(:,:), intent(in) :: b

        integer :: ij,ib,ia,ii,ie

        do ij = 1, size(c, 4)
            do ib = 1, size(c, 2)
                do ia = 1, size(c, 1)
                    do ii = 1, size(c, 3)
                        do ie = 1, size(a, 3)
                            c(ia,ib,ii,ij) = c(ia,ib,ii,ij) + &
                                    a(ia,ib,ie,ij)*b(ie,ii)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine contract_abij_abej_ei_real32

    subroutine test_contract_abij_ijbk_ak_real32(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(5,3,7,2) :: c
        real(real32), dimension(5,3,7,2) :: c_easy
        real(real32), dimension(7,2,3,11) :: a
        real(real32), dimension(5,11) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,b,i,j)=a(i,j,b,k)*b(a,k)", &
                driver, options, priorities)

        call contract_abij_ijbk_ak_real32(c, a, b)
        call contract(c_easy, a, b, "c(a,b,i,j)=a(i,j,b,k)*b(a,k)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_abij_ijbk_ak_real32:equal", &
                helper%equal(tc%storage, c, 1.0e-4))

        call assertion%equal(prefix//":contract_abij_ijbk_ak_real32-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0e-4))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_abij_ijbk_ak_real32

    subroutine contract_abij_ijbk_ak_real32(c, a, b)
        real(real32), dimension(:,:,:,:), intent(inout) :: c
        real(real32), dimension(:,:,:,:), intent(in) :: a
        real(real32), dimension(:,:), intent(in) :: b

        integer :: ij,ib,ia,ii,ik

        do ij = 1, size(c, 4)
            do ib = 1, size(c, 2)
                do ia = 1, size(c, 1)
                    do ii = 1, size(c, 3)
                        do ik = 1, size(a, 4)
                            c(ia,ib,ii,ij) = c(ia,ib,ii,ij) + &
                                    a(ii,ij,ib,ik)*b(ia,ik)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine contract_abij_ijbk_ak_real32

    subroutine test_contract_abij_abcd_cdij_real32(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(5,3,7,2) :: c
        real(real32), dimension(5,3,7,2) :: c_easy
        real(real32), dimension(5,3,13,11) :: a
        real(real32), dimension(13,11,7,2) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,b,i,j)=a(a,b,c,d)*b(c,d,i,j)", &
                driver, options, priorities)

        call contract_abij_abcd_cdij_real32(c, a, b)
        call contract(c_easy, a, b, "c(a,b,i,j)=a(a,b,c,d)*b(c,d,i,j)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_abij_abcd_cdij_real32:equal", &
                helper%equal(tc%storage, c, 1.0e-4))

        call assertion%equal(prefix//":contract_abij_abcd_cdij_real32-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0e-4))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_abij_abcd_cdij_real32

    subroutine contract_abij_abcd_cdij_real32(c, a, b)
        real(real32), dimension(:,:,:,:), intent(inout) :: c
        real(real32), dimension(:,:,:,:), intent(in) :: a
        real(real32), dimension(:,:,:,:), intent(in) :: b

        integer :: ij,ib,ia,ii,id,ic

        do ij = 1, size(c, 4)
            do ib = 1, size(c, 2)
                do ia = 1, size(c, 1)
                    do ii = 1, size(c, 3)
                        do id = 1, size(a, 4)
                            do ic = 1, size(a, 3)
                                c(ia,ib,ii,ij) = c(ia,ib,ii,ij) + &
                                        a(ia,ib,ic,id)*b(ic,id,ii,ij)
                            end do
                        end do
                    end do
                end do
            end do
        end do
    end subroutine contract_abij_abcd_cdij_real32

    subroutine test_contract_abij_kbcj_acik_real32(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(5,3,7,2) :: c
        real(real32), dimension(5,3,7,2) :: c_easy
        real(real32), dimension(11,3,13,2) :: a
        real(real32), dimension(5,13,7,11) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(a,b,i,j)=a(k,b,c,j)*b(a,c,i,k)", &
                driver, options, priorities)

        call contract_abij_kbcj_acik_real32(c, a, b)
        call contract(c_easy, a, b, "c(a,b,i,j)=a(k,b,c,j)*b(a,c,i,k)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_abij_kbcj_acik_real32:equal", &
                helper%equal(tc%storage, c, 1.0e-4))

        call assertion%equal(prefix//":contract_abij_kbcj_acik_real32-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0e-4))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_abij_kbcj_acik_real32

    subroutine contract_abij_kbcj_acik_real32(c, a, b)
        real(real32), dimension(:,:,:,:), intent(inout) :: c
        real(real32), dimension(:,:,:,:), intent(in) :: a
        real(real32), dimension(:,:,:,:), intent(in) :: b

        integer :: ij,ib,ia,ii,ik,ic

        do ij = 1, size(c, 4)
            do ib = 1, size(c, 2)
                do ia = 1, size(c, 1)
                    do ii = 1, size(c, 3)
                        do ik = 1, size(a, 1)
                            do ic = 1, size(a, 3)
                                c(ia,ib,ii,ij) = c(ia,ib,ii,ij) + &
                                        a(ik,ib,ic,ij)*b(ia,ic,ii,ik)
                            end do
                        end do
                    end do
                end do
            end do
        end do
    end subroutine contract_abij_kbcj_acik_real32

    subroutine test_contract__ij_ij_real32(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32) :: c
        real(real32) :: c_easy
        real(real32), dimension(3,2) :: a
        real(real32), dimension(3,2) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c=a(i,j)*b(i,j)", &
                driver, options, priorities)

        call contract__ij_ij_real32(c, a, b)
        call contract(c_easy, a, b, "c=a(i,j)*b(i,j)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract__ij_ij_real32:equal", &
                helper%equal(tc%storage, c, 1.0e-4))

        call assertion%equal(prefix//":contract__ij_ij_real32-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0e-4))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract__ij_ij_real32

    subroutine contract__ij_ij_real32(c, a, b)
        real(real32), intent(inout) :: c
        real(real32), dimension(:,:), intent(in) :: a
        real(real32), dimension(:,:), intent(in) :: b

        integer :: ij,ii

        do ij = 1, size(a, 2)
            do ii = 1, size(a, 1)
                c = c + &
                        a(ii,ij)*b(ii,ij)
            end do
        end do
    end subroutine contract__ij_ij_real32

    subroutine test_contract_ij__ij_real32(assertion, prefix, driver, memtype, options, priorities)
        type(assert), intent(inout) :: assertion
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in), optional :: driver
        character(len=*), intent(in), optional :: memtype
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities

        real(real32), dimension(3,2) :: c
        real(real32), dimension(3,2) :: c_easy
        real(real32) :: a
        real(real32), dimension(3,2) :: b
        class(tensor), allocatable :: ta, tb, tc
        type(storage_helper) :: helper
        class(tensor_contraction), allocatable :: t

        call random_number(a)
        call random_number(b)
        c = 0.0
        c_easy = 0.0

        call allocate_and_copy_tensor(ta, a, memtype, options, priorities)
        call allocate_and_copy_tensor(tb, b, memtype, options, priorities)
        call allocate_and_copy_tensor(tc, c, memtype, options, priorities)

        call tensor_contraction_factory%create(t, "c(i,j)=a*b(i,j)", &
                driver, options, priorities)

        call contract_ij__ij_real32(c, a, b)
        call contract(c_easy, a, b, "c(i,j)=a*b(i,j)", options=options, priorities=priorities)

        call t%contract(tc, ta, tb)

        call assertion%equal(prefix//":contract_ij__ij_real32:equal", &
                helper%equal(tc%storage, c, 1.0e-4))

        call assertion%equal(prefix//":contract_ij__ij_real32-easy:equal", &
                helper%equal(tc%storage, c_easy, 1.0e-4))

        call ta%cleanup(); call tb%cleanup(); call tc%cleanup()
        deallocate(ta, tb, tc)

        call t%cleanup()
        deallocate(t)
    end subroutine test_contract_ij__ij_real32

    subroutine contract_ij__ij_real32(c, a, b)
        real(real32), dimension(:,:), intent(inout) :: c
        real(real32), intent(in) :: a
        real(real32), dimension(:,:), intent(in) :: b

        integer :: ij,ii

        do ij = 1, size(c, 2)
            do ii = 1, size(c, 1)
                c(ii,ij) = c(ii,ij) + &
                        a*b(ii,ij)
            end do
        end do
    end subroutine contract_ij__ij_real32
end module tensor_contraction_test_helper_module
