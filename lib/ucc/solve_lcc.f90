module lcc

    implicit none

contains

    subroutine solve_lcc(io, froz, occ_a, occ_b, orbs, &
            fockr, fockb, intr, intb, intm, &
            ecor_cc, itol, maxiter, diis_space, &
            p_space)

        use const
        use diis

        implicit none

        integer, intent(in) :: io
        integer, intent(in) :: froz, occ_a, occ_b ,orbs
        integer, intent(in) :: maxiter, itol
        integer, intent(in) :: diis_space
        real(kind=8), intent(in) :: ecor_cc

        integer :: k1, k2, k3, k4,  kkk
        integer :: k1a, k1b, k2a, k2b, k2c, k3a, k3b, k3c, k3d
        integer :: l2b, l2c, L3b, l3c, l3d
        integer :: i0
        integer :: i, j, k, l
        integer :: a, b, c, d
        integer :: iter

        real(kind=8) :: FockR(orbs, orbs), FockB(orbs, orbs)
        real(kind=8) :: IntR(froz+1:orbs, froz+1:orbs, froz+1:orbs, froz+1:orbs)
        real(kind=8) :: IntB(froz+1:orbs, froz+1:orbs, froz+1:orbs, froz+1:orbs)
        real(kind=8) :: IntM(froz+1:orbs, froz+1:orbs, froz+1:orbs, froz+1:orbs)

        real(kind=8) :: prev_time
        real(kind=8) :: ecor, ecor_new, energy_diff
        real(kind=8), allocatable :: t(:), t0(:), l_vec(:)
        real(kind=8), allocatable :: t3B(:,:,:,:,:,:)


        real(kind=8), allocatable :: H1A(:,:)
        real(kind=8), allocatable :: H1B(:,:)
        real(kind=8), allocatable :: H2A(:,:,:,:)
        real(kind=8), allocatable :: H2B(:,:,:,:)
        real(kind=8), allocatable :: H2C(:,:,:,:)

        integer :: p_space(occ_a+1:orbs,occ_a+1:orbs,occ_a+1:orbs,froz+1:occ_a,froz+1:occ_a,froz+1:occ_a)
        real(kind=8) :: res, shift


        shift = 0.0d0

        !call antisymmetrize_p_space(froz, occ_a, occ_b, orbs, p_space)

        allocate(H1A(froz+1:orbs,froz+1:orbs),H1B(froz+1:orbs,froz+1:orbs))
        allocate(H2A(froz+1:orbs,froz+1:orbs,froz+1:orbs,froz+1:orbs))
        allocate(H2B(froz+1:orbs,froz+1:orbs,froz+1:orbs,froz+1:orbs))
        allocate(H2C(froz+1:orbs,froz+1:orbs,froz+1:orbs,froz+1:orbs))
        H1A=0.0d0
        H1B=0.0d0
        H2A=0.0d0
        H2B=0.0d0
        H2C=0.0d0

        open(hbar_unit, file='hbar.bin', form='unformatted')
        rewind(hbar_unit)
        read(hbar_unit) ((H1A(i,j),i=froz+1,orbs),j=froz+1,orbs)
        read(hbar_unit) ((H1B(i,j),i=froz+1,orbs),j=froz+1,orbs)
        read(hbar_unit) ((((H2A(i,j,k,l),i=froz+1,orbs),j=froz+1,orbs),k=froz+1,orbs),l=froz+1,orbs)
        read(hbar_unit) ((((H2B(i,j,k,l),i=froz+1,orbs),j=froz+1,orbs),k=froz+1,orbs),l=froz+1,orbs)
        read(hbar_unit) ((((H2C(i,j,k,l),i=froz+1,orbs),j=froz+1,orbs),k=froz+1,orbs),l=froz+1,orbs)
        close(hbar_unit)

        do i=froz+1,occ_a
            do j=froz+1,occ_a
                do a=occ_a+1,orbs
                    do b=occ_a+1,orbs
                        H2A(b,a,j,i)=IntR(b,a,j,i)
                    enddo
                enddo
            enddo
        enddo

        do i=froz+1,occ_a
            do j=froz+1,occ_b
                do a=occ_a+1,orbs
                    do b=occ_b+1,orbs
                        H2B(b,a,j,i)=IntM(b,a,j,i)
                    enddo
                enddo
            enddo
        enddo

        do i=froz+1,occ_b
            do j=froz+1,occ_b
                do a=occ_b+1,orbs
                    do b=occ_b+1,orbs
                        H2C(b,a,j,i)=IntB(b,a,j,i)
                    enddo
                enddo
            enddo
        enddo

        K1=occ_a-froz
        K3=orbs-occ_a !OCC
        K2=occ_b-froz
        K4=orbs-occ_b !VIR
        KKK=0
        K1A =KKK+1
        KKK=KKK+K1*K3
        K1B =KKK+1
        KKK=KKK+K2*K4
        K2A =KKK+1
        KKK=KKK+K1*K1*K3*K3
        K2B =KKK+1
        KKK=KKK+K1*K2*K3*K4
        K2C =KKK+1
        KKK=KKK+K2*K2*K4*K4
        K3A=KKK+1
        KKK=KKK+K3*K3*K3*K1*K1*K1
        K3B=KKK+1
        KKK=KKK+K4*K3*K3*K2*K1*K1
        K3C=KKK+1
        KKK=KKK+K4*K4*K3*K2*K2*K1
        K3D=KKK+1
        KKK=KKK+K4*K4*K4*K2*K2*K2
        L2C=K2B
        L2B=L2C+K2*K2*K4*K4
        L3D=K3B
        L3B=L3D+K4*K4*K4*K2*K2*K2
        L3C=L3B+K3*K4*K3*K1*K2*K1

        allocate(t(kkk))
        t = 0.0d0
        open(t_unit, file='t_vec.bin', form='unformatted')
        read(t_unit) t
        close(t_unit)

        allocate(t0(KKK))
        t0=t
        do i=K2A,K2B-1
            t(i)=-t0(i)
        enddo
        do i=K2B,K2C-1
            t(i)=-t0(L2B-K2B+i)
        enddo
        do i=K2C,K3A-1
            t(i)=-t0(L2C-K2C+i)
        enddo
        do i=K3A,K3B-1
            t(i)=-t0(i)
        enddo
        do i=K3C,K3D-1
            t(i)=-t0(L3C-K3C+i)
        enddo
        do i=K3D,KKK
            t(i)=-t0(L3D-K3D+i)
        enddo
        allocate(t3B(occ_a+1:orbs,occ_b+1:orbs,occ_a+1:orbs,froz+1:occ_a,froz+1:occ_b,froz+1:occ_a))
        i0=L3B-1
        do i=froz+1,occ_a
            do j=froz+1,occ_b
                do k=froz+1,occ_a
                    do a=occ_a+1,orbs
                        do b=occ_b+1,orbs
                            do c=occ_a+1,orbs
                                i0=i0+1
                                t3B(c,b,a,k,j,i)=t0(i0)
                            enddo
                        enddo
                    enddo
                enddo
            enddo
        enddo
        deallocate(t0)
        i0=K3B-1
        do i=froz+1,occ_a
            do k=froz+1,occ_a
                do j=froz+1,occ_b
                    do a=occ_a+1,orbs
                        do c=occ_a+1,orbs
                            do b=occ_b+1,orbs
                                i0=i0+1
                                t(i0)=-t3B(c,b,a,k,j,i)
                            enddo
                        enddo
                    enddo
                enddo
            enddo
        enddo
        deallocate(t3B)


        open(l_unit, file='l_vec.bin', form='unformatted')
        open(l_vecs_unit, file='l_vecs.bin', form='unformatted', recl=kkk*8, access='direct')
        write(l_unit) t


        write(io, '(a)') ''
        call print_date(io, '  Starting L-CC calculation on')
        write(io, '(a)') ''
        !call print_amps(l_unit, io, froz, occ_a, occ_b, orbs, kkk, 5.0d-2)
        call print_iter_head(io)
        call cpu_time(prev_time)

        do iter=1, maxiter

            call update_lcc(io, froz,occ_a,occ_b,orbs,l_unit,res,ecor_new,shift, &
                K1,K2,K3,K4,K1A,K1B,K2A,K2B,K2C,K3A,K3B,K3C,K3D,KKK, &
                FockR,FockB,IntR,IntB,IntM,H1A,H1B,H2A,H2B,H2C,t, &
                p_space)

            energy_diff = ecor - ecor_new
            ecor = ecor_new

            call print_iteration(io, iter, ecor + ecor_cc, energy_diff, res, prev_time)
            call cpu_time(prev_time)

            ! Write T vecs
            allocate(l_vec(kkk))
            rewind(l_unit)
            read(l_unit) l_vec
            call write_t_vecs(l_vecs_unit, iter, diis_space, l_vec)
            deallocate(l_vec)

            ! Do DIIS
            if (mod(iter, diis_space + 1) == 0) then
                write(io, '(a)') '      DIIS cycle'
                allocate(l_vec(kkk))
                rewind(l_unit)
                read(l_unit) l_vec

                call calc_diis(l_vecs_unit, diis_space, kkk, l_vec)

                rewind(l_unit)
                write(l_unit) l_vec
                deallocate(l_vec)
            endif

            !call print_amps(l_unit, io, froz, occ_a, occ_b, orbs, kkk, 5.0d-2)

            if (dabs(ecor) < 1.0d1**(-itol)) exit

            !if (iter == maxiter) call abort_cc('Calculation failed to converge.')
            if (iter == maxiter) write(io, '(a)') 'FAILED TO CONVERGE.'

        enddo

        write(io, '(a)') ''
        call print_date(io, '  L-CC calculation finished on:')
        write(io, '(a)') ''

        deallocate(H1A)
        deallocate(H1B)
        deallocate(H2A)
        deallocate(H2B)
        deallocate(H2C)

        close(l_vecs_unit, status='delete')
        close(l_unit)

    end subroutine solve_lcc

end module lcc
