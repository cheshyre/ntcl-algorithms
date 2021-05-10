module algorithms_initializer
    use :: data_initializer, only : &
            data_initialize, &
            data_finalize

    use :: algorithms_dev, only : &
            mm_initialize, &
            mm_finalize, &
            tp_initialize, &
            tp_finalize, &
            tc_initialize, &
            tc_finalize, &
            tu_initialize, &
            tu_finalize, &
            bmm_initialize, &
            bmm_finalize, &
            btp_initialize, &
            btp_finalize, &
            btc_initialize, &
            btc_finalize, &
            default_mm_factory, &
            default_permute_factory, &
            default_contraction_factory, &
            default_update_factory, &
            default_bmm_factory, &
            default_btp_factory, &
            default_btc_factory

    implicit none
    public

contains
    subroutine algorithms_initialize()

        call data_initialize()

        call mm_initialize(default_mm_factory())
        call tp_initialize(default_permute_factory())
        call tc_initialize(default_contraction_factory())
        call tu_initialize(default_update_factory())
        call bmm_initialize(default_bmm_factory())
        call btp_initialize(default_btp_factory())
        call btc_initialize(default_btc_factory())
    end subroutine

    subroutine algorithms_finalize()

        call btc_finalize()
        call btp_finalize()
        call bmm_finalize()
        call tu_finalize()
        call tc_finalize()
        call tp_finalize()
        call mm_finalize()

        call data_finalize()
    end subroutine algorithms_finalize
end module algorithms_initializer
