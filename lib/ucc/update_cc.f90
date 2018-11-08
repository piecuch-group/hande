subroutine update_clusters(n0, n1, n2, n3, &
        fockr, fockb, intr, intb, intm, t, t_size, &
        p_space)

    implicit none

    integer, intent(in) :: n0, n1, n2 ,n3, t_size

    real(kind=8), intent(inout) :: t(t_size)
    integer, intent(in) :: p_space(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)

    real(kind=8), intent(in) :: FockR(N3,N3)
    real(kind=8), intent(in) :: FockB(N3,N3)
    real(kind=8), intent(in) :: IntR(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
    real(kind=8), intent(in) :: IntB(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
    real(kind=8), intent(in) :: IntM(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)

    real(kind=8), allocatable :: V1(:,:)
    real(kind=8), allocatable :: V2(:,:,:,:)
    real(kind=8), allocatable :: V3(:,:,:,:,:,:)


    real(kind=8) :: shift
    integer :: k1, k2, k3, k4
    integer :: k1a, k1b, k2a, k2b, k2c, k3a, k3b, k3c, k3d
    integer :: kkk0

    integer :: i
    logical :: ifrhf

    ifrhf = .false.

    shift = 0.0d0
    K1=N1-N0
    K3=N3-N1 !OCC
    K2=N2-N0
    K4=N3-N2 !VIR
    KKK0=0
    K1A =KKK0+1
    KKK0=KKK0+K1*K3
    K1B =KKK0+1
    KKK0=KKK0+K2*K4
    K2A =KKK0+1
    KKK0=KKK0+K1*K1*K3*K3
    K2B =KKK0+1
    KKK0=KKK0+K2*K2*K4*K4
    K2C =KKK0+1
    KKK0=KKK0+K1*K2*K3*K4
    K3A =KKK0+1
    KKK0=KKK0+K3*K3*K3*K1*K1*K1
    K3B =KKK0+1
    KKK0=KKK0+K4*K4*K4*K2*K2*K2
    K3C=KKK0+1
    KKK0=KKK0+K3*K4*K3*K1*K2*K1
    K3D=KKK0+1
    KKK0=KKK0+K4*K4*K3*K2*K2*K1

    allocate(V2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
    V2=0.0d0
    call sumx2143(N0,N3,N2,N3,N1,N3,N0,N2,N0,N1,V2,IntM,-1.000)

    call t2C_update(N0,N1,N2,N3,K1,K2,K3,K4,shift,V2, &
        FockR,FockB,IntR,IntB,IntM,t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
        t(K3A),t(K3B),t(K3C),t(K3D))
    deallocate(V2)

    allocate(V2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
    V2=0.0d0
    call sumx2143(N0,N3,N1,N3,N1,N3,N0,N1,N0,N1,V2,IntR,-1.000)

    call t2A_update(N0,N1,N2,N3,K1,K2,K3,K4,shift,V2, &
        FockR,FockB,IntR,IntB,IntM,t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
        t(K3A),t(K3B),t(K3C),t(K3D))
    deallocate(V2)

    if(ifRHF)then
        do i=K2A,K2B-1
            t(i+K2B-K2A)=t(i)
        enddo
    else
        allocate(V2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
        V2=0.0d0
        call sumx2143(N0,N3,N2,N3,N2,N3,N0,N2,N0,N2,V2,IntB,-1.000)

        call t2B_update(N0,N1,N2,N3,K1,K2,K3,K4,shift,V2, &
            FockR,FockB,IntR,IntB,IntM,t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
            t(K3A),t(K3B),t(K3C),t(K3D))
        deallocate(V2)

    endif

    allocate(V1(N1+1:N3,N0+1:N1))
    V1=0.0d0
    call sumx12(0,N3,N1,N3,N0,N1,V1,FockR, 1.000)

    call t1A_update(N0,N1,N2,N3,K1,K2,K3,K4,shift,V1, &
        FockR,FockB,IntR,IntB,IntM,t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
        t(K3A),t(K3B),t(K3C),t(K3D))
    deallocate(V1)

    if(ifRHF)then
        do i=K1A,K1B-1
            t(i+K1B-K1A)=t(i)
        enddo
    else
        allocate(V1(N2+1:N3,N0+1:N2))
        V1=0.0d0
        call sumx12(0,N3,N2,N3,N0,N2,V1,FockB, 1.000)

        call t1B_update(N0,N1,N2,N3,K1,K2,K3,K4,shift,V1, &
            FockR,FockB,IntR,IntB,IntM,t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
            t(K3A),t(K3B),t(K3C),t(K3D))
        deallocate(V1)

    endif

    allocate(V3(K3,K3,K3,K1,K1,K1))
    V3=0.0d0

    call t3A_update(N0,N1,N2,N3,K1,K2,K3,K4,shift,V3, &
        FockR,FockB,IntR,IntB,IntM,t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
        t(K3A),t(K3B),t(K3C),t(K3D),p_space)
    deallocate(V3)

    if(ifRHF)then
        do i=K3A,K3B-1
            t(i+K3B-K3A)=t(i)
        enddo
    else
        allocate(V3(K4,K4,K4,K2,K2,K2))
        V3=0.0d0

        call t3B_update(N0,N1,N2,N3,K1,K2,K3,K4,shift,V3, &
            FockR,FockB,IntR,IntB,IntM,t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
            t(K3A),t(K3B),t(K3C),t(K3D),p_space)
        deallocate(V3)
    endif

    allocate(V3(K3,K4,K3,K1,K2,K1))
    V3=0.0d0

    call t3C_update(N0,N1,N2,N3,K1,K2,K3,K4,shift,V3, &
        FockR,FockB,IntR,IntB,IntM,t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
        t(K3A),t(K3B),t(K3C),t(K3D),p_space)
    deallocate(V3)

    if(ifRHF)then
        do i=K3C,K3D-1
            t(i+K3B-K3C)=t(i)
        enddo
    else
        allocate(V3(K4,K4,K3,K2,K2,K1))
        V3=0.0d0

        call t3D_update(N0,N1,N2,N3,K1,K2,K3,K4,shift,V3, &
            FockR,FockB,IntR,IntB,IntM,t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
            t(K3A),t(K3B),t(K3C),t(K3D),p_space)
        deallocate(V3)
    endif

end subroutine update_clusters
