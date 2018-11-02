subroutine DIIS(io,iDIIS,icoe,KKK0,t0,ncoe)
    implicit none
    integer, intent(in)         :: icoe(iDIIS+2)
    integer, intent(in)         :: io
    integer(kind=8), intent(in) :: KKK0, iDIIS, ncoe
    real(kind=8), intent(inout) :: t0(KKK0)
    ! Local variables
    real(kind=8), allocatable   :: t(:), t1(:), t2(:), B(:,:), L(:)
    real(kind=8), allocatable   :: pivot(:)
    real(kind=8)                :: dotproduct, ddot
    real(kind=8)                :: Tim0,CPUTim
    integer(kind=4)             :: Wall0, TIME, info
    integer                     :: i, ii, i0, ic, istep, j, j0, k
    integer                     :: KKK

    Tim0=CPUTim(0)
    Wall0=TIME()
    ! I think this is trying to deal with the poorly designed system of files for t
    do i=1,iDIIS
        i0=i
        if (i.gt.iDIIS / 2) then
            i0 = i + 1
        endif
        ic = icoe(i0)
        rewind(ic)
        ic = icoe(i0 + 1)
        rewind(ic)
    enddo
    ! Allocate overlap matrix
    allocate(B(iDIIS + 1,iDIIS + 1))
    B=0.0d0
    do istep=1,ncoe
        do i=1,iDIIS
            i0=i
            if (i.gt.iDIIS/2) then
                i0=i+1
            endif
            ic=icoe(i0)
            read(ic)KKK
            if (i.eq.1) then
                allocate(t(KKK),t1(KKK),t2(KKK))
            endif
            read(ic) t1
            backspace(ic)
            backspace(ic)
            ic=icoe(i0+1)
            read(ic)KKK
            read(ic)t2
            backspace(ic)
            backspace(ic)
            t=t2-t1 ! Set residuum vector
            do j=1,iDIIS
                j0=j
                if (j0.gt.iDIIS/2) then
                    j0=j0+1
                endif
                ic=icoe(j0)
                read(ic)KKK
                read(ic)t1
                if(i.ne.iDIIS)then
                    backspace(ic)
                    backspace(ic)
                endif
                ic=icoe(j0+1)
                read(ic)KKK
                read(ic)t2
                if (i.ne.iDIIS.or.(j.ne.iDIIS.and.j.ne.iDIIS/2)) then
                    backspace(ic)
                    backspace(ic)
                endif
                ! Calculate overlap matrix B
                t1=t2-t1 ! Set residuum vector
                dotproduct = ddot(KKK, t, 1, t1, 1)
                B(i,j) = B(i,j) + dotproduct
            enddo
        enddo
        deallocate(t,t1,t2)
    enddo

    do i=1,iDIIS+1
        B(i,iDIIS+1) = -1.0d0
        B(iDIIS+1,i) = -1.0d0
    enddo

    B(iDIIS + 1, iDIIS + 1) = 0.0d0

    allocate(L(iDIIS + 1)) ! Allocate lambda and c
    L=0.0d0
    L(iDIIS+1) = -1.0d0
    info = 0
    allocate(pivot(iDIIS + 1))
    call dgesv(iDIIS + 1, 1, B, iDIIS + 1, pivot, L, iDIIS + 1, info)
    ! Changed if(ifs) to if(.not. ifs) might not work!
    if(info == 0) then
        t0=0.0d0
        do i=1,iDIIS
            i0=i
            if(i0.gt.iDIIS/2)i0=i0+1
            ic=icoe(i0)
            rewind(ic)
            ii=0
            do j=1,ncoe
                read(ic)KKK
                allocate(t(KKK))
                read(ic)t
                do k=1,KKK
                    t0(ii+k)=t0(ii+k)+L(i)*t(k)
                enddo
                deallocate(t)
                ii=ii+KKK
            enddo
        enddo
    else
        write(io,'(a)') 'Error during DGESV excecution.'
    endif
    call NJ_cputim(io,Tim0)
    call NJ_walltim(io,Wall0)
end subroutine DIIS
