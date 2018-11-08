module cc

    implicit none

contains

    subroutine solve_cc(io, froz, occ_a, occ_b, orbs, &
            shift, itol, &
            FockR, FockB, IntR, IntB, IntM, &
            is_rhf, diis_space, restart, p_space, &
            e_cor)

        use, intrinsic :: iso_fortran_env, only: output_unit
        use const
        use diis
        use utils, only: residuum
        ! [TODO] optimize terms
        !    use hmatrix
        !    use contract

        implicit none

        integer, intent(in) :: io
        integer, intent(in) :: froz, occ_a, occ_b, orbs
        real(kind=8), intent(in) :: shift
        integer, intent(in) :: itol
        logical, intent(in) :: is_rhf, restart
        integer, intent(in) :: diis_space
        real(kind=8), intent(inout) :: e_cor

        integer, intent(in) :: p_space(occ_a+1:orbs,occ_a+1:orbs,occ_a+1:orbs,froz+1:occ_a,froz+1:occ_a,froz+1:occ_a)

        integer :: t_size

        integer, parameter :: maxiter = 1000

        real(kind=8) :: e_cor_new, energy_diff, prev_time
        real(kind=8) :: res
        real(kind=8) :: conv(3) = 1

        integer :: iter

        real(kind=8) :: E1A,E1B,E2A,E2B,E2C,E1A1A,E1B1B,E1A1B

        ! [TODO] clean this vars
        integer :: K1A,K1B,K2A,K2B,K2C,K3A,K3B
        integer :: K3C,K3D
        integer :: K1,K2,K3,K4,i0

        ! Indices
        integer :: i,j,k,l,m,n
        integer :: a,b,c,d,e,f

        character(len=200) :: test_env
        integer :: test_env_i

        real(kind=8) :: FockR(orbs,orbs)
        real(kind=8) :: FockB(orbs,orbs)
        real(kind=8) :: IntR(froz+1:orbs,froz+1:orbs,froz+1:orbs,froz+1:orbs)
        real(kind=8) :: IntB(froz+1:orbs,froz+1:orbs,froz+1:orbs,froz+1:orbs)
        real(kind=8) :: IntM(froz+1:orbs,froz+1:orbs,froz+1:orbs,froz+1:orbs)

        real(kind=8), allocatable :: t(:)
        real(kind=8), allocatable :: V1(:,:)
        real(kind=8), allocatable :: V2(:,:,:,:)
        real(kind=8), allocatable :: V3(:,:,:,:,:,:)

        call get_environment_variable("MKL_NUM_THREADS", test_env)
        if (trim(test_env) /= '') then
            read(test_env, *) test_env_i
            write(io, '(a,i4)') 'MKL_NUM_THREADS', test_env_i
        endif

        K1 = occ_a-froz
        K3 = orbs-occ_a !OCC
        K2 = occ_b-froz
        K4 = orbs-occ_b !VIR

        t_size = 0
        K1A = t_size+1
        t_size = t_size+K1*K3
        K1B = t_size+1
        t_size = t_size+K2*K4
        K2A = t_size+1
        t_size = t_size+K1*K1*K3*K3
        K2B = t_size+1
        t_size = t_size+K2*K2*K4*K4
        K2C = t_size+1
        t_size = t_size+K1*K2*K3*K4
        K3A = t_size+1
        t_size = t_size+K3*K3*K3*K1*K1*K1
        K3B = t_size+1
        t_size = t_size+K4*K4*K4*K2*K2*K2
        K3C = t_size+1
        t_size = t_size+K3*K4*K3*K1*K2*K1
        K3D = t_size+1
        t_size = t_size+K4*K4*K3*K2*K2*K1

        ! Open temporary files to store T vector amplitudes
        open(t_unit,file='t_vec.bin',form='unformatted')
        call init_t_vecs(t_vecs_unit, t_size)

        allocate(t(t_size))
        t=0.0d0

        E1A=0.0d0
        E1B=0.0d0
        E2A=0.0d0
        E2B=0.0d0
        E2C=0.0d0
        E1A1A=0.0d0
        E1B1B=0.0d0
        E1A1B=0.0d0

        call print_header(io)
        write(io, '(a)') ''
        call print_date(io, '  Starting CC(P) calculation on')
        write(io, '(a)') ''

        ! Output formatting
        call print_iter_head(io)

        ! Start main CC loop
        call cpu_time(prev_time)

        do iter=1, maxiter

            call update_clusters(froz, occ_a, occ_b ,orbs, &
                fockr, fockb, intr, intb, intm, t, t_size, &
                p_space)


            call calculate_energy(occ_a,occ_b,orbs,froz, &
                FockR,FockB,IntR,IntB,IntM,t(K1A),t(K1B),t(K2A),t(K2B), &
                t(K2C),E1A,E1B,E2A,E2B,E2C,E1A1A,E1A1B,E1B1B)

            e_cor_new = e1a+e1b+e2a+e2b+e2c+e1a1a+e1a1b+e1b1b
            energy_diff = e_cor-e_cor_new

            ! Convergence information. Currently using a moving average. This can change
            conv(1) = conv(2)
            conv(2) = conv(3)
            conv(3) = energy_diff

            e_cor = e_cor_new

            ! Write T vector on a file
            rewind(t_unit)
            write(t_unit) t

            ! Write T vecs
            call write_t_vecs(t_vecs_unit, iter, diis_space, t)

            ! Calculate residuum
            res = residuum(iter, diis_space, t_size, t_vecs_unit)

            ! Do DIIS
            if (mod(iter, diis_space + 1) == 0) then
                write(io, '(a)') '      DIIS cycle'
                call calc_diis(t_vecs_unit, diis_space, t_size, t)
            endif

            call print_iteration(io, iter, e_cor_new, energy_diff, res, prev_time)
            call cpu_time(prev_time)

            ! Check for convergence
            if (dabs(conv(1)) < 1.0d1**(-itol) .and. dabs(conv(2)) <1.0d1**(-itol) .and. dabs(conv(3)) < 1.0d1**(-itol)) exit

            ! Abort if out of iterations
            if (iter == maxiter) write(io, '(a)') 'FAILED TO CONVERGE.'
        enddo

        close(t_vecs_unit, status='delete')
        close(t_unit)

        write(io, '(a)') ''
        call print_date(io, '  CC(P) calculation finished on:')

        deallocate(t)

    end subroutine solve_cc

end module cc
