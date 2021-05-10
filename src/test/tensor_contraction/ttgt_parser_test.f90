module ttgt_parser_test_module
    use :: util_api, only : &
            assert, &
            string

    use :: algorithms_dev, only : &
            ttgt_descriptor, &
            ttgt_parser

    implicit none
    private

    public :: ttgt_parser_test

    type :: ttgt_parser_test
    contains
        procedure :: run => run
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type ttgt_parser_test

    interface ttgt_parser_test
        module procedure constructor
    end interface ttgt_parser_test
contains
    function constructor() result(this)
        type(ttgt_parser_test) :: this

        call this%clear()
    end function constructor

    subroutine run(this, assertion)
        class(ttgt_parser_test), intent(in) :: this
        type(assert), intent(inout) :: assertion

        type(ttgt_parser) :: parser
        type(string) :: contraction, strategy, ablock
        type(ttgt_descriptor) :: descr
        integer, dimension(0) :: empty

        call assertion%equal("ttgt_parser::Test complete", .true.)

        contraction = "c(m,n)=a(m,k)*b(k,n)"
        parser = ttgt_parser(contraction)

        call assertion%equal("ttgt_parser::"//contraction%char_array//":index_chars", &
            parser%parser%unique_indices == "mnk")
        call assertion%equal("ttgt_parser::"//contraction%char_array//":c_indices", &
            parser%parser%c == "mn")
        call assertion%equal("ttgt_parser::"//contraction%char_array//":a_indices", &
            parser%parser%a == "mk")
        call assertion%equal("ttgt_parser::"//contraction%char_array//":b_indices", &
            parser%parser%b == "kn")

        call assertion%equal("ttgt_parser::get_block(m, n):", &
                parser%get_block(string("m"), string("n")) == "(m,n)")
        call assertion%equal("ttgt_parser::get_block(mi, nj):", &
                parser%get_block(string("mi"), string("nj")) == "(mi,nj)")

        call assertion%equal("ttgt_parser::find_common_indices(mi, mj):", &
                parser%find_common_indices(string("mi"), string("mj")) == "m")

        contraction = "c(m,n)=a(m,k)*b(k,n)"
        parser = ttgt_parser(contraction)
        strategy = parser%get_default_strategy()
        call assertion%equal("ttgt_parser::"//contraction%char_array//":strategy", &
                strategy == "c(m,n)=a(m,k)*b(k,n)")

        contraction = "c(m,n)=a(m,k)*b(n,k)"
        parser = ttgt_parser(contraction)
        strategy = parser%get_default_strategy()
        call assertion%equal("ttgt_parser::"//contraction%char_array//":strategy", &
                strategy == "c(m,n)=a(m,k)*b(k,n)")

        contraction = "c(m,n,i)=a(k,i,m)*b(n,k)"
        parser = ttgt_parser(contraction)
        strategy = parser%get_default_strategy()
        call assertion%equal("ttgt_parser::"//contraction%char_array//":strategy", &
                strategy == "c(mi,n)=a(mi,k)*b(k,n)")

        contraction = "c(m,n)=a(m,k)*b(k,n)"
        parser = ttgt_parser(contraction)
        descr = parser%get_descriptor(string("c(m,n)=a(m,k)*b(k,n)"))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":descriptor:m allocated", &
                allocated(descr%m))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":descriptor:m", &
                all(descr%m==[1]))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":descriptor:n allocated", &
                allocated(descr%n))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":descriptor:n", &
                all(descr%n==[2]))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":descriptor:k allocated", &
                allocated(descr%k))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":descriptor:k", &
                all(descr%k==[3]))
        call assertion%equal("ttgt_parser::"//contraction%char_array//"::switch_ab", &
                .not. descr%switch_ab)

        contraction = "c(m,n,i)=a(k,i,m)*b(n,k)"
        parser = ttgt_parser(contraction)
        descr = parser%get_descriptor(string("c(mi,n)=a(mi,k)*b(k,n)"))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":descriptor:m allocated", &
                allocated(descr%m))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":descriptor:m", &
                all(descr%m==[1,3]))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":descriptor:n allocated", &
                allocated(descr%n))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":descriptor:n", &
                all(descr%n==[2]))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":descriptor:k allocated", &
                allocated(descr%k))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":descriptor:k", &
                all(descr%k==[4]))
        call assertion%equal("ttgt_parser::"//contraction%char_array//"::switch_ab", &
                .not. descr%switch_ab)

        contraction = "c(m,n,i,j,l)=a(k,f,l,m,e)*b(j,i,e,n,k,f)"
        parser = ttgt_parser(contraction)
        strategy = parser%get_default_strategy()
        call assertion%equal("ttgt_parser::"//contraction%char_array//":strategy", &
                strategy == "c(ml,nij)=a(ml,kfe)*b(kfe,nij)")
        descr = parser%get_descriptor(string("c(ml,nij)=a(ml,kfe)*b(kfe,nij)"))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":descriptor:m allocated", &
                allocated(descr%m))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":descriptor:m", &
                all(descr%m==[1,5]))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":descriptor:n allocated", &
                allocated(descr%n))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":descriptor:n", &
                all(descr%n==[2,3,4]))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":descriptor:k allocated", &
                allocated(descr%k))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":descriptor:k", &
                all(descr%k==[6,7,8]))
        call assertion%equal("ttgt_parser::"//contraction%char_array//"::switch_ab", &
                .not. descr%switch_ab)

        contraction = "c(m,n,i)=a(k,i,m)*b(n,k)"
        parser = ttgt_parser(contraction)
        descr = parser%get_descriptor(string("c(n,mi)=b(n,k)*a(k,mi)"))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":switch:descriptor:m allocated", &
                allocated(descr%m))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":switch:descriptor:m", &
                all(descr%m==[2]))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":switch:descriptor:n allocated", &
                allocated(descr%n))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":switch:descriptor:n", &
                all(descr%n==[1,3]))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":switch:descriptor:k allocated", &
                allocated(descr%k))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":switch:descriptor:k", &
                all(descr%k==[4]))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":switch:switch_ab", &
                descr%switch_ab)

        contraction = "c(m,n)=a(m,n)*b"
        parser = ttgt_parser(contraction)
        strategy = parser%get_default_strategy()
        call assertion%equal("ttgt_parser::"//contraction%char_array//":strategy", &
                strategy == "c(mn)=a(mn)*b")
        descr = parser%get_descriptor(string("c(mn)=a(mn)*b"))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":descriptor:m allocated", &
                allocated(descr%m))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":descriptor:m", &
                all(descr%m==[1,2]))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":descriptor:n allocated", &
                allocated(descr%n))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":descriptor:n", &
                all(descr%n==empty))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":descriptor:k allocated", &
                allocated(descr%k))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":descriptor:k", &
                all(descr%k==empty))
        call assertion%equal("ttgt_parser::"//contraction%char_array//"::switch_ab", &
                .not. descr%switch_ab)

        contraction = "c=a(m,n)*b(m,n)"
        parser = ttgt_parser(contraction)
        strategy = parser%get_default_strategy()
        call assertion%equal("ttgt_parser::"//contraction%char_array//":strategy", &
                strategy == "c=a(mn)*b(mn)")
        descr = parser%get_descriptor(string("c=a(nm)*b(nm)"))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":descriptor:m allocated", &
                allocated(descr%m))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":descriptor:m", &
                all(descr%m==empty))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":descriptor:n allocated", &
                allocated(descr%n))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":descriptor:n", &
                all(descr%n==empty))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":descriptor:k allocated", &
                allocated(descr%k))
        call assertion%equal("ttgt_parser::"//contraction%char_array//":descriptor:k", &
                all(descr%k==[2,1]))
        call assertion%equal("ttgt_parser::"//contraction%char_array//"::switch_ab", &
                .not. descr%switch_ab)
    end subroutine run

    subroutine cleanup(this)
        class(ttgt_parser_test), intent(inout) :: this

        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(ttgt_parser_test), intent(inout) :: this
    end subroutine clear
end module ttgt_parser_test_module
