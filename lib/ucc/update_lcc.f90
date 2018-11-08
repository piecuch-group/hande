subroutine update_lcc(io, N0,N1,N2,N3,iL,res,e_cor_new, shift, &
        K1,K2,K3,K4,K1A,K1B,K2A,K2B,K2C,K3A,K3B,K3C,K3D,KKK, &
        FockR,FockB,IntR,IntB,IntM,H1A,H1B,H2A,H2B,H2C,t, &
        p_space)

    implicit none

    integer :: i, j, k, a, b, c
    integer, intent(in) :: io
    integer, intent(in) :: n0, n1, n2, n3
    integer, intent(in) :: il
    integer, intent(in) :: K1,K2,K3,K4,K1A,K1B,K2A,K2B,K2C,K3A,K3B,K3C,K3D,KKK

    real(kind=8), intent(inout) :: res, e_cor_new
    real(kind=8), intent(in) :: shift

    real*8 FockR(N3,N3)
    real*8 FockB(N3,N3)
    real*8 IntR(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
    real*8 IntB(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
    real*8 IntM(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
    real*8 H1A(N0+1:N3,N0+1:N3)
    real*8 H1B(N0+1:N3,N0+1:N3)
    real*8 H2A(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
    real*8 H2B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
    real*8 H2C(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
    real*8 t(KKK)
    real*8,allocatable::LH(:),l(:)

    integer p_space(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)

    integer, parameter :: iroot = 0

    allocate(LH(KKK),L(KKK))
    LH=0.0d0
    l=0.0d0
    rewind(iL)
    read(iL) L

    call L_input(N0,N1,N2,N3,iroot, &
        H1A,H1B,H2A,H2B,H2C,l(K1A),l(K1B),l(K2A),l(K2B),l(K2C), &
        LH(K1A),LH(K1B),LH(K2A),LH(K2B),LH(K2C),LH(K3A),LH(K3B),LH(K3C), &
        LH(K3D))

    call L1A_update(N0,N1,N2,N3,LH(K1A), &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        H1A,H1B,H2A,H2B,H2C, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),t(K3A),t(K3B),t(K3C),t(K3D), &
        l(K1A),l(K1B),l(K2A),l(K2B),l(K2C),l(K3A),l(K3B),l(K3C),l(K3D))

    call L1B_update(N0,N1,N2,N3,LH(K1B), &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        H1A,H1B,H2A,H2B,H2C, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),t(K3A),t(K3B),t(K3C),t(K3D), &
        l(K1A),l(K1B),l(K2A),l(K2B),l(K2C),l(K3A),l(K3B),l(K3C),l(K3D))

    call L2A_update(N0,N1,N2,N3,LH(K2A), &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        H1A,H1B,H2A,H2B,H2C, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),t(K3A),t(K3B),t(K3C),t(K3D), &
        l(K1A),l(K1B),l(K2A),l(K2B),l(K2C),l(K3A),l(K3B),l(K3C),l(K3D))

    call L2B_update(N0,N1,N2,N3,LH(K2B), &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        H1A,H1B,H2A,H2B,H2C, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),t(K3A),t(K3B),t(K3C),t(K3D), &
        l(K1A),l(K1B),l(K2A),l(K2B),l(K2C),l(K3A),l(K3B),l(K3C),l(K3D))

    call L2C_update(N0,N1,N2,N3,LH(K2C), &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        H1A,H1B,H2A,H2B,H2C, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),t(K3A),t(K3B),t(K3C),t(K3D), &
        l(K1A),l(K1B),l(K2A),l(K2B),l(K2C),l(K3A),l(K3B),l(K3C),l(K3D))

    call L3A_update(N0,N1,N2,N3,LH(K3A), &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        H1A,H1B,H2A,H2B,H2C, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),t(K3A),t(K3B),t(K3C),t(K3D), &
        l(K1A),l(K1B),l(K2A),l(K2B),l(K2C),l(K3A),l(K3B),l(K3C),l(K3D))

    call L3B_update(N0,N1,N2,N3,LH(K3B), &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        H1A,H1B,H2A,H2B,H2C, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),t(K3A),t(K3B),t(K3C),t(K3D), &
        l(K1A),l(K1B),l(K2A),l(K2B),l(K2C),l(K3A),l(K3B),l(K3C),l(K3D))

    call L3C_update(N0,N1,N2,N3,LH(K3C), &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        H1A,H1B,H2A,H2B,H2C, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),t(K3A),t(K3B),t(K3C),t(K3D), &
        l(K1A),l(K1B),l(K2A),l(K2B),l(K2C),l(K3A),l(K3B),l(K3C),l(K3D))

    call L3D_update(N0,N1,N2,N3,LH(K3D), &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        H1A,H1B,H2A,H2B,H2C, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),t(K3A),t(K3B),t(K3C),t(K3D), &
        l(K1A),l(K1B),l(K2A),l(K2B),l(K2C),l(K3A),l(K3B),l(K3C),l(K3D))

    call if0(N0,N1,N2,N3,KKK,K3A,LH,p_space)
    call L_update (N0,N1,N2,N3,shift,H1A,H1B,0.0d0, &
        l(K1A),l(K1B),l(K2A),l(K2B),l(K2C),l(K3A),l(K3B),l(K3C),l(K3D), &
        LH(K1A),LH(K1B),LH(K2A),LH(K2B),LH(K2C),LH(K3A),LH(K3B),LH(K3C), &
        LH(K3D))
    call if0(N0,N1,N2,N3,KKK,K3A,L,p_space)

    e_cor_new = 0.0d0
    do i=1,KKK
        e_cor_new = e_cor_new + LH(i)*LH(i)
    enddo
    e_cor_new = dsqrt(e_cor_new)

    rewind(il)
    read(iL) LH
    rewind(il)
    write(iL) L

    L=L-LH
    res = 0.0d0

    do i=1,KKK
        res =  res + L(i)*L(i)
    enddo
    res = dsqrt(res)

    deallocate(LH,L)

end subroutine update_lcc

subroutine if0(N0,N1,N2,N3,KKK,K3A,LH,p_space)

    implicit none
    integer, intent(in) :: n0, n1, n2 ,n3
    integer, intent(in) :: k3a
    integer, intent(in) :: kkk
    real(kind=8), intent(inout) :: LH(KKK)
    integer, intent(in) :: p_space(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)
    integer :: i, j, k, a, b, c

    integer :: nact, i0
    i0=K3A-1
    do i=N0+1,N1
        do j=N0+1,N1
            do k=N0+1,N1
                do a=N1+1,N3
                    do b=N1+1,N3
                        do c=N1+1,N3
                            i0=i0+1
                            if(p_space(a,b,c,i,j,k) == 1) then
                                cycle
                            endif
                            LH(i0)=0.0d0
                        enddo
                    enddo
                enddo
            enddo
        enddo
    enddo
    do i=N0+1,N1
        do j=N0+1,N1
            do k=N0+1,N2
                do a=N1+1,N3
                    do b=N1+1,N3
                        do c=N2+1,N3
                            i0=i0+1
                            if(p_space(a,b,c,i,j,k) == 1) then
                                cycle
                            endif
                            LH(i0)=0.0d0
                        enddo
                    enddo
                enddo
            enddo
        enddo
    enddo
    do i=N0+1,N1
        do j=N0+1,N2
            do k=N0+1,N2
                do a=N1+1,N3
                    do b=N2+1,N3
                        do c=N2+1,N3
                            i0=i0+1
                            if(p_space(a,b,c,i,j,k) == 1) then
                                cycle
                            endif
                            LH(i0)=0.0d0
                        enddo
                    enddo
                enddo
            enddo
        enddo
    enddo
    do i=N0+1,N2
        do j=N0+1,N2
            do k=N0+1,N2
                do a=N2+1,N3
                    do b=N2+1,N3
                        do c=N2+1,N3
                            i0=i0+1
                            if(p_space(a,b,c,i,j,k) == 1) then
                                cycle
                            endif
                            LH(i0)=0.0d0
                        enddo
                    enddo
                enddo
            enddo
        enddo
    enddo

end subroutine if0
