module ccp3

contains

    subroutine correct_cc(io, froz, occ_a, occ_b, orbs, &
            fockr, fockb, intr, intb, intm, &
            ccpq_energy, p_space)

        use const, only: hbar_unit

        implicit none

        integer, intent(in) :: io
        integer, intent(in) :: froz, occ_a, occ_b, orbs
        real(kind=8), intent(inout) :: ccpq_energy(4)
        !integer :: hbar_unit

        integer :: a,b,c,d,e,f
        integer :: i, j, k, m
        integer :: i0
        integer :: nroot, iroot
        integer :: k1a, k1b, k2a, k2b, k2c, k3a, k3b, k3c, k3d, kkk
        integer :: l2b, l2c, l3b, l3c, l3d
        integer :: k1, k2, k3, k4

        integer, parameter :: il = 441

        real(kind=8) :: res,PP,shift,LHM,R0,ECC
        real(kind=8) :: DA,DB,DC,DD

        real(kind=8) :: FockR(orbs, orbs), FockB(orbs, orbs)
        real(kind=8) :: IntR(froz+1:orbs, froz+1:orbs, froz+1:orbs, froz+1:orbs)
        real(kind=8) :: IntB(froz+1:orbs, froz+1:orbs, froz+1:orbs, froz+1:orbs)
        real(kind=8) :: IntM(froz+1:orbs, froz+1:orbs, froz+1:orbs, froz+1:orbs)
        real(kind=8), allocatable :: t3B(:,:,:,:,:,:)

        real(kind=8), allocatable :: H1A(:,:)
        real(kind=8), allocatable :: H1B(:,:)
        real(kind=8), allocatable :: H2A(:,:,:,:)
        real(kind=8), allocatable :: H2B(:,:,:,:)
        real(kind=8), allocatable :: H2C(:,:,:,:)

        real(kind=8) :: E23A
        real(kind=8) :: E23B
        real(kind=8) :: E23C
        real(kind=8) :: E23D
        real(kind=8), allocatable :: l(:),ECor(:),r(:), t(:), t0(:), t_test(:)
        real(kind=8), allocatable :: LH3(:,:,:,:,:,:)
        real(kind=8), allocatable :: MM3(:,:,:,:,:,:)
        real(kind=8), allocatable :: D3A1(:,:,:)
        real(kind=8), allocatable :: D3A2(:,:,:)
        real(kind=8), allocatable :: D3B1(:,:,:)
        real(kind=8), allocatable :: D3B2(:,:,:)
        real(kind=8), allocatable :: D3C1(:,:,:)
        real(kind=8), allocatable :: D3C2(:,:,:)
        real(kind=8), allocatable :: D3D1(:,:,:)
        real(kind=8), allocatable :: D3D2(:,:,:)

        integer :: p_space(occ_a+1:orbs,occ_a+1:orbs,occ_a+1:orbs,froz+1:occ_a,froz+1:occ_a,froz+1:occ_a)

        integer, parameter :: t_unit = 389

        iroot = 0
        pp = 0.0d0
        r0 = 1.0d0

        !hbar_unit = 535
        !open(hbar_unit,file='h8_hbar.bin',form='unformatted')

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
        read(hbar_unit) ((((H2A(i,j,k,m),i=froz+1,orbs),j=froz+1,orbs),k=froz+1,orbs),m=froz+1,orbs)
        read(hbar_unit) ((((H2B(i,j,k,m),i=froz+1,orbs),j=froz+1,orbs),k=froz+1,orbs),m=froz+1,orbs)
        read(hbar_unit) ((((H2C(i,j,k,m),i=froz+1,orbs),j=froz+1,orbs),k=froz+1,orbs),m=froz+1,orbs)
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

        e23a = 0.0d0
        e23b = 0.0d0
        e23c = 0.0d0
        e23d = 0.0d0


        allocate(D3A1(occ_a+1:orbs,froz+1:occ_a,froz+1:occ_a))
        allocate(D3A2(occ_a+1:orbs,occ_a+1:orbs,froz+1:occ_a))
        allocate(D3B1(occ_a+1:orbs,froz+1:occ_b,froz+1:occ_a))
        allocate(D3B2(occ_b+1:orbs,occ_a+1:orbs,froz+1:occ_a))
        allocate(D3C1(occ_b+1:orbs,froz+1:occ_b,froz+1:occ_a))
        allocate(D3C2(occ_b+1:orbs,occ_a+1:orbs,froz+1:occ_b))
        allocate(D3D1(occ_b+1:orbs,froz+1:occ_b,froz+1:occ_b))
        allocate(D3D2(occ_b+1:orbs,occ_b+1:orbs,froz+1:occ_b))
        D3A1=0.0d0
        D3A2=0.0d0
        D3B1=0.0d0
        D3B2=0.0d0
        D3C1=0.0d0
        D3C2=0.0d0
        D3D1=0.0d0
        D3D2=0.0d0

        call Cal_D3(froz,occ_a,occ_b,orbs, &
            H2A,H2B,H2C,t(K2A),t(K2B),t(K2C), &
            D3A1,D3A2,D3B1,D3B2,D3C1,D3C2,D3D1,D3D2)

        E23A=0.0d0
        E23B=0.0d0
        E23C=0.0d0
        E23D=0.0d0

        ! Start loop over roots (iroot > 0 are excited states)
        open(il, file='l_vec.bin', form='unformatted')
        allocate(l(KKK))
        l=0.0d0
        read(iL)l
        !      read(iL0,rec=iroot+1)(l(i),i=1,K3A-1)
        !write(io,'(a)') 'Largest L amplitudes'
        !call print_amps(il, io, froz, occ_a, occ_b, orbs, kkk, 5.0d-2)
        close(il)

        PP=0.0d0
        R0=1.0d0

        ! Start T3A correction
        allocate(LH3(occ_a+1:orbs,occ_a+1:orbs,occ_a+1:orbs,froz+1:occ_a,froz+1:occ_a,froz+1:occ_a))
        LH3=0.0d0
        call L3A_update_cor(froz,occ_a,occ_b,orbs,LH3, &
            K1,K2,K3,K4, &
            FockR,FockB,IntR,IntB,IntM, &
            H1A,H1B,H2A,H2B,H2C, &
            t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),t(K3A),t(K3B),t(K3C),t(K3D), &
            l(K1A),l(K1B),l(K2A),l(K2B),l(K2C),l(K3A),l(K3B),l(K3C),l(K3D))

        allocate(MM3(occ_a+1:orbs,occ_a+1:orbs,occ_a+1:orbs,froz+1:occ_a,froz+1:occ_a,froz+1:occ_a))
        MM3=0.0d0

        if(dabs(R0).gt.1.0d-12)then
            call t3A_update_cor(froz,occ_a,occ_b,orbs,MM3, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
                t(K3A),t(K3B),t(K3C),t(K3D))
            MM3=R0*MM3
        endif

        ! Zero amplitudes outside of the p_space
        call if0(froz,occ_a,occ_b,orbs, &
            froz,occ_a,froz,occ_a,froz,occ_a,occ_a,orbs,occ_a,orbs,occ_a,orbs,LH3,p_space)
        call if0(froz,occ_a,occ_b,orbs, &
            froz,occ_a,froz,occ_a,froz,occ_a,occ_a,orbs,occ_a,orbs,occ_a,orbs,MM3,p_space)

        ! Perform correction
        do i=froz+1,occ_a-2
            do j=i+1,occ_a-1
                do k=j+1,occ_a
                    do a=occ_a+1,orbs-2
                        do b=a+1,orbs-1
                            do c=b+1,orbs
                                if(p_space(c,b,a,k,j,i) == 1) then
                                    cycle
                                endif

                                DA=PP+FockR(i,i)+FocKR(j,j)+FockR(k,k) &
                                    -FockR(a,a)-FockR(b,b)-FockR(c,c)
                                DB=PP+H1A(i,i)+H1A(j,j)+H1A(k,k) &
                                    -H1A(a,a)-H1A(b,b)-H1A(c,c)
                                DC=DB+H2A(a,i,a,i)+H2A(b,i,b,i)+H2A(c,i,c,i) &
                                    +H2A(a,j,a,j)+H2A(b,j,b,j)+H2A(c,j,c,j) &
                                    +H2A(a,k,a,k)+H2A(b,k,b,k)+H2A(c,k,c,k) &
                                    -H2A(j,i,j,i)-H2A(k,i,k,i)-H2A(k,j,k,j) &
                                    -H2A(b,a,b,a)-H2A(c,a,c,a)-H2A(c,b,c,b)
                                DD=DC+D3A1(a,j,i)+D3A1(a,k,i)+D3A1(a,k,j) &
                                    +D3A1(b,j,i)+D3A1(b,k,i)+D3A1(b,k,j) &
                                    +D3A1(c,j,i)+D3A1(c,k,i)+D3A1(c,k,j) &
                                    -D3A2(b,a,i)-D3A2(b,a,j)-D3A2(b,a,k) &
                                    -D3A2(c,a,i)-D3A2(c,a,j)-D3A2(c,a,k) &
                                    -D3A2(c,b,i)-D3A2(c,b,j)-D3A2(c,b,k)
                                LHM=LH3(c,b,a,k,j,i)*MM3(c,b,a,k,j,i)
                                E23A=E23A+LHM/DA
                                E23B=E23B+LHM/DB
                                E23C=E23C+LHM/DC
                                E23D=E23D+LHM/DD
                            enddo
                        enddo
                    enddo
                enddo
            enddo
        enddo
        deallocate(LH3,MM3)

        ! Start T3B correction
        allocate(LH3(occ_b+1:orbs,occ_a+1:orbs,occ_a+1:orbs,froz+1:occ_b,froz+1:occ_a,froz+1:occ_a))
        LH3=0.0d0
        call L3B_update_cor(froz,occ_a,occ_b,orbs,LH3, &
            K1,K2,K3,K4, &
            FockR,FockB,IntR,IntB,IntM, &
            H1A,H1B,H2A,H2B,H2C, &
            t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),t(K3A),t(K3B),t(K3C),t(K3D), &
            l(K1A),l(K1B),l(K2A),l(K2B),l(K2C),l(K3A),l(K3B),l(K3C),l(K3D))

        allocate(MM3(occ_b+1:orbs,occ_a+1:orbs,occ_a+1:orbs,froz+1:occ_b,froz+1:occ_a,froz+1:occ_a))
        MM3=0.0d0
        if(dabs(R0).gt.1.0d-12)then
            call t3B_update_cor(froz,occ_a,occ_b,orbs,MM3, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
                t(K3A),t(K3B),t(K3C),t(K3D))
            MM3=R0*MM3
        endif

        call if0(froz,occ_a,occ_b,orbs, &
            froz,occ_a,froz,occ_a,froz,occ_b,occ_a,orbs,occ_a,orbs,occ_b,orbs,LH3,p_space)
        call if0(froz,occ_a,occ_b,orbs, &
            froz,occ_a,froz,occ_a,froz,occ_b,occ_a,orbs,occ_a,orbs,occ_b,orbs,MM3,p_space)

        do i=froz+1,occ_a-1
            do j=i+1,occ_a
                do k=froz+1,occ_b
                    do a=occ_a+1,orbs-1
                        do b=a+1,orbs
                            do c=occ_b+1,orbs
                                if(p_space(c,b,a,k,j,i) == 1) then
                                    cycle
                                endif

                                DA=PP+FockR(i,i)+FocKR(j,j)+FockB(k,k) &
                                    -FockR(a,a)-FockR(b,b)-FockB(c,c)
                                DB=PP+H1A(i,i)+H1A(j,j)+H1B(k,k) &
                                    -H1A(a,a)-H1A(b,b)-H1B(c,c)
                                DC=DB+H2A(a,i,a,i)+H2A(b,i,b,i)+H2B(c,i,c,i) &
                                    +H2A(a,j,a,j)+H2A(b,j,b,j)+H2B(c,j,c,j) &
                                    +H2B(k,a,k,a)+H2B(k,b,k,b)+H2C(c,k,c,k) &
                                    -H2A(j,i,j,i)-H2B(k,i,k,i)-H2B(k,j,k,j) &
                                    -H2A(b,a,b,a)-H2B(c,a,c,a)-H2B(c,b,c,b)
                                DD=DC+D3A1(a,j,i)+D3B1(a,k,i)+D3B1(a,k,j) &
                                    +D3A1(b,j,i)+D3B1(b,k,i)+D3B1(b,k,j) &
                                    +D3C1(c,k,i)+D3C1(c,k,j) &
                                    -D3A2(b,a,i)-D3A2(b,a,j) &
                                    -D3B2(c,a,i)-D3B2(c,a,j)-D3C2(c,a,k) &
                                    -D3B2(c,b,i)-D3B2(c,b,j)-D3C2(c,b,k)
                                LHM=LH3(c,b,a,k,j,i)*MM3(c,b,a,k,j,i)
                                E23A=E23A+LHM/DA!*0.25
                                E23B=E23B+LHM/DB!*0.25
                                E23C=E23C+LHM/DC!*0.25
                                E23D=E23D+LHM/DD!*0.25
                            enddo
                        enddo
                    enddo
                enddo
            enddo
        enddo
        deallocate(LH3,MM3)

        ! Start T3C correction
        allocate(LH3(occ_b+1:orbs,occ_b+1:orbs,occ_a+1:orbs,froz+1:occ_b,froz+1:occ_b,froz+1:occ_a))
        LH3=0.0d0
        call L3C_update_cor(froz,occ_a,occ_b,orbs,LH3, &
            K1,K2,K3,K4, &
            FockR,FockB,IntR,IntB,IntM, &
            H1A,H1B,H2A,H2B,H2C, &
            t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),t(K3A),t(K3B),t(K3C),t(K3D), &
            l(K1A),l(K1B),l(K2A),l(K2B),l(K2C),l(K3A),l(K3B),l(K3C),l(K3D))

        allocate(MM3(occ_b+1:orbs,occ_b+1:orbs,occ_a+1:orbs,froz+1:occ_b,froz+1:occ_b,froz+1:occ_a))
        MM3=0.0d0
        if(dabs(R0).gt.1.0d-12)then
            call t3C_update_cor(froz,occ_a,occ_b,orbs,MM3, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
                t(K3A),t(K3B),t(K3C),t(K3D))
            MM3=R0*MM3
        endif

        call if0(froz,occ_a,occ_b,orbs, &
            froz,occ_a,froz,occ_b,froz,occ_b,occ_a,orbs,occ_b,orbs,occ_b,orbs,LH3,p_space)
        call if0(froz,occ_a,occ_b,orbs, &
            froz,occ_a,froz,occ_b,froz,occ_b,occ_a,orbs,occ_b,orbs,occ_b,orbs,MM3,p_space)

        do i=froz+1,occ_a
            do j=froz+1,occ_b-1
                do k=j+1,occ_b
                    do a=occ_a+1,orbs
                        do b=occ_b+1,orbs-1
                            do c=b+1,orbs
                                if(p_space(c,b,a,k,j,i) == 1) then
                                    cycle
                                endif

                                DA=PP+FockR(i,i)+FocKB(j,j)+FockB(k,k) &
                                    -FockR(a,a)-FockB(b,b)-FockB(c,c)
                                DB=PP+H1A(i,i)+H1B(j,j)+H1B(k,k) &
                                    -H1A(a,a)-H1B(b,b)-H1B(c,c)
                                DC=DB+H2A(a,i,a,i)+H2B(b,i,b,i)+H2B(c,i,c,i) &
                                    +H2B(j,a,j,a)+H2C(b,j,b,j)+H2C(c,j,c,j) &
                                    +H2B(k,a,k,a)+H2C(b,k,b,k)+H2C(c,k,c,k) &
                                    -H2B(j,i,j,i)-H2B(k,i,k,i)-H2C(k,j,k,j) &
                                    -H2B(b,a,b,a)-H2B(c,a,c,a)-H2C(c,b,c,b)
                                DD=DC+D3B1(a,j,i)+D3B1(a,k,i) &
                                    +D3C1(b,j,i)+D3C1(b,k,i)+D3D1(b,k,j) &
                                    +D3C1(c,j,i)+D3C1(c,k,i)+D3D1(c,k,j) &
                                    -D3B2(b,a,i)-D3C2(b,a,j)-D3C2(b,a,k) &
                                    -D3B2(c,a,i)-D3C2(c,a,j)-D3C2(c,a,k) &
                                    -D3D2(c,b,j)-D3D2(c,b,k)
                                LHM=LH3(c,b,a,k,j,i)*MM3(c,b,a,k,j,i)
                                E23A=E23A+LHM/DA!*0.25
                                E23B=E23B+LHM/DB!*0.25
                                E23C=E23C+LHM/DC!*0.25
                                E23D=E23D+LHM/DD!*0.25
                            enddo
                        enddo
                    enddo
                enddo
            enddo
        enddo
        deallocate(LH3,MM3)

        ! Start T3D correction
        allocate(LH3(occ_b+1:orbs,occ_b+1:orbs,occ_b+1:orbs,froz+1:occ_b,froz+1:occ_b,froz+1:occ_b))
        LH3=0.0d0
        call L3D_update_cor(froz,occ_a,occ_b,orbs,LH3, &
            K1,K2,K3,K4, &
            FockR,FockB,IntR,IntB,IntM, &
            H1A,H1B,H2A,H2B,H2C, &
            t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
            t(K3A),t(K3B),t(K3C),t(K3D), &
            l(K1A),l(K1B),l(K2A),l(K2B),l(K2C), &
            l(K3A),l(K3B),l(K3C),l(K3D))

        allocate(MM3(occ_b+1:orbs,occ_b+1:orbs,occ_b+1:orbs,froz+1:occ_b,froz+1:occ_b,froz+1:occ_b))
        MM3=0.0d0
        if(dabs(R0).gt.1.0d-12)then
            call t3D_update_cor(froz,occ_a,occ_b,orbs,MM3, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
                t(K3A),t(K3B),t(K3C),t(K3D))
        endif

        call if0(froz,occ_a,occ_b,orbs, &
            froz,occ_b,froz,occ_b,froz,occ_b,occ_b,orbs,occ_b,orbs,occ_b,orbs,LH3,p_space)
        call if0(froz,occ_a,occ_b,orbs, &
            froz,occ_b,froz,occ_b,froz,occ_b,occ_b,orbs,occ_b,orbs,occ_b,orbs,MM3,p_space)

        do i=froz+1,occ_b-2
            do j=i+1,occ_b-1
                do k=j+1,occ_b
                    do a=occ_b+1,orbs-2
                        do b=a+1,orbs-1
                            do c=b+1,orbs
                                if(p_space(c,b,a,k,j,i) == 1) then
                                    cycle
                                endif
                                DA=PP+FockB(i,i)+FocKB(j,j)+FockB(k,k) &
                                    -FockB(a,a)-FockB(b,b)-FockB(c,c)
                                DB=PP+H1B(i,i)+H1B(j,j)+H1B(k,k) &
                                    -H1B(a,a)-H1B(b,b)-H1B(c,c)
                                DC=DB+H2C(a,i,a,i)+H2C(b,i,b,i)+H2C(c,i,c,i) &
                                    +H2C(a,j,a,j)+H2C(b,j,b,j)+H2C(c,j,c,j) &
                                    +H2C(a,k,a,k)+H2C(b,k,b,k)+H2C(c,k,c,k) &
                                    -H2C(j,i,j,i)-H2C(k,i,k,i)-H2C(k,j,k,j) &
                                    -H2C(b,a,b,a)-H2C(c,a,c,a)-H2C(c,b,c,b)
                                DD=DC+D3D1(a,j,i)+D3D1(a,k,i)+D3D1(a,k,j) &
                                    +D3D1(b,j,i)+D3D1(b,k,i)+D3D1(b,k,j) &
                                    +D3D1(c,j,i)+D3D1(c,k,i)+D3D1(c,k,j) &
                                    -D3D2(b,a,i)-D3D2(b,a,j)-D3D2(b,a,k) &
                                    -D3D2(c,a,i)-D3D2(c,a,j)-D3D2(c,a,k) &
                                    -D3D2(c,b,i)-D3D2(c,b,j)-D3D2(c,b,k)
                                LHM=LH3(c,b,a,k,j,i)*MM3(c,b,a,k,j,i)
                                E23A=E23A+LHM/DA
                                E23B=E23B+LHM/DB
                                E23C=E23C+LHM/DC
                                E23D=E23D+LHM/DD
                            enddo
                        enddo
                    enddo
                enddo
            enddo
        enddo
        deallocate(LH3,MM3)

        deallocate(l)

        ccpq_energy(1) = e23a
        ccpq_energy(2) = e23b
        ccpq_energy(3) = e23c
        ccpq_energy(4) = e23d

        !EeV=27.21138386
        !Ekcal=627.509469

    end subroutine correct_cc

    subroutine if0(froz,occ_a,occ_b,orbs, &
            K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,LH3, &
            p_space)
        integer, intent(in) :: froz, occ_a, occ_b, orbs
        integer, intent(in) :: k1, k2, k3, k4, k5, k6
        integer, intent(in) :: l1, l2, l3, l4, l5, l6

        real(kind=8), intent(inout) :: LH3(K6+1:L6,K5+1:L5,K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1)

        integer, intent(in) :: p_space(occ_a+1:orbs,occ_a+1:orbs,occ_a+1:orbs,froz+1:occ_a,froz+1:occ_a,froz+1:occ_a)
        integer :: i, j, k, a, b, c

        do i=K1+1,L1
            do j=K2+1,L2
                do k=K3+1,L3
                    do a=K4+1,L4
                        do b=K5+1,L5
                            do c=K6+1,L6
                                if(p_space(a,b,c,i,j,k) /= 1) then
                                    cycle
                                endif
                                LH3(c,b,a,k,j,i)=0.0d0
                            enddo
                        enddo
                    enddo
                enddo
            enddo
        enddo
    end subroutine if0

end module ccp3
