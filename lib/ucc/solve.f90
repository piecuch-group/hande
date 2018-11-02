       subroutine solve_cc(io,N1,N2,N3,N0,ECor,ERef, &
               shift,itol,ifr, &
               FockR,FockB,IntR,IntB,IntM,ifRHF,icoe,iDIIS, &
               p_space)
               !p_space_a, p_space_b, p_space_c, p_space_d)

        implicit none
        integer itol,ifS,ifD,ifT,ifQ,ll,if3AB,if3CD,if3EF,ifFT,ist
        integer N1,N2,N3,N0,ifCCSD,ncycle0,kk,ifNT,if1b,ifr,io
        logical ifRHF
        integer iDIIS,icoe(iDIIS+2),nDIIS,ii,jj,ic
        integer KKK0,KKK,ncoe
        integer K1A,K1B,K2A,K2B,K2C,K3A,K3B
        integer K3C,K3D
        integer K1,K2,K3,K4,i0,M1,M2

        real*8 ET,ET1,ET2,ET3,ET4,ENN,dE1,dE2,ECor1

        integer (kind=4) Wall0,Nmin,Wall1
        real(kind=8) Tim0,Tim1,Sec,CPUTim,Tim2
        external CPUTim

        real(kind=4) :: values(2)
        real(kind=4) :: time

        real*8 E1A,E1B,E2A,E2B,E2C,E1A1A,E1B1B,E1A1B
        real*8 ECor,ECorNew,ECCSD,dE,dE0
        real*8 ERef,shift,shift1
        integer i,j,k,l,m,n
        integer a,b,c,d,e,f
        integer NCycle

        character(len=200) :: test_env
        integer :: test_env_i

        real*8 FockR(N3,N3)
        real*8 FockB(N3,N3)
        real*8 IntR(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
        real*8 IntB(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
        real*8 IntM(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)

! allocatable arrays

       real*8,allocatable::t(:)
       real*8,allocatable::V1(:,:)
       real*8,allocatable::V2(:,:,:,:)
       real*8,allocatable::V3(:,:,:,:,:,:)
       integer :: p_space(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)
       !integer :: p_space_b(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2)
       !integer :: p_space_c(N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N1)
       !integer :: p_space_d(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1)

! allocate array

      call get_environment_variable("MKL_NUM_THREADS", test_env)
      if (trim(test_env) /= '') then
          read(test_env, *) test_env_i
          write(io, '(a,i4)') 'MKL_NUM_THREADS', test_env_i
      endif


      K1=N1-N0;K3=N3-N1 !OCC
      K2=N2-N0;K4=N3-N2 !VIR
      KKK0=0
      K1A =KKK0+1;KKK0=KKK0+K1*K3
      K1B =KKK0+1;KKK0=KKK0+K2*K4
      K2A =KKK0+1;KKK0=KKK0+K1*K1*K3*K3
      K2B =KKK0+1;KKK0=KKK0+K2*K2*K4*K4
      K2C =KKK0+1;KKK0=KKK0+K1*K2*K3*K4
      K3A =KKK0+1;KKK0=KKK0+K3*K3*K3*K1*K1*K1
      K3B =KKK0+1;KKK0=KKK0+K4*K4*K4*K2*K2*K2
      K3C=KKK0+1;KKK0=KKK0+K3*K4*K3*K1*K2*K1
      K3D=KKK0+1;KKK0=KKK0+K4*K4*K3*K2*K2*K1

      allocate(t(KKK0))
      t=0.0d0

      Tim0=CPUTim(0)
      call etime(values, time)
      Wall0= int(time, kind=4)
      write(io,*) '+++ ENTER UCCSDt0 PROGRAM +++'
      call NJ_date(io,'UCCSDt0 calculation begin from:')

        do i=1,N3;do j=1,N3
         if (dabs(FockR(i,j)).lt.1.0d-10)FockR(i,j)=0.0d0
         if (dabs(FockB(i,j)).lt.1.0d-10)FockB(i,j)=0.0d0
        enddo;enddo
        do i=N0+1,N3;do j=N0+1,N3;do k=N0+1,N3;do l=N0+1,N3
         if (dabs(IntR(i,j,k,l)).lt.1.0d0-10)IntR(i,j,k,l)=0.0d0
         if (dabs(IntB(i,j,k,l)).lt.1.0d0-10)IntB(i,j,k,l)=0.0d0
         if (dabs(IntM(i,j,k,l)).lt.1.0d0-10)IntM(i,j,k,l)=0.0d0
        enddo;enddo;enddo;enddo


       E1A=0.0d0;E1B=0.0d0
       E2A=0.0d0;E2B=0.0d0;E2C=0.0d0
       E1A1A=0.0d0;E1B1B=0.0d0;E1A1B=0.0d0
       ECor=0.0d0;ECornew=0.0d0

        ncycle0=0;ist=0

!       if(ifr.eq.333)goto 333
!       rewind(ifr)
!       read(ifr,err=444,end=444) &
!      ist,ECor,E1A,E1B,E2A,E2B,E2C,E1A1A,E1B1B,E1A1B
!       if(ist.ne.0)then
!        read(ifr,err=444,end=444)t
!       else
!        read(ifr,err=444,end=444)(t(i),i=1,K3A-1)
!       endif
!       write(io,*)'Energy of initia guess'
!       write(io,*)'ECor= ',ECor
!       write(io,*)'E1A=   ',E1A
!       write(io,*)'E1B=   ',E1B
!       write(io,*)'E2A=   ',E2A
!       write(io,*)'E2B=   ',E2B
!       write(io,*)'E2C=   ',E2C
!       write(io,*)'E1A1A= ',E1A1A
!       write(io,*)'E1A1B= ',E1A1B
!       write(io,*)'E1B1B= ',E1B1B
!       goto 333
!444    write(io,*)'err to restart'
       t=0.0d0
       E1A=0.0d0;E1B=0.0d0
       E2A=0.0d0;E2B=0.0d0;E2C=0.0d0
       E1A1A=0.0d0;E1B1B=0.0d0;E1A1B=0.0d0
       ECor=0.0d0;ECornew=0.0d0
333    continue


      !write(io,*)'All t3',KKK0-K3A

      !call if0(N0,N1,N2,N3,M1,M2,KKK,K3A,t,p_space)
       write(io,'(/,a4,2(a15),a16)') 'It.',  'E (Corr)','dE', 'CPU Time'
       write(io,'(50("-"))')

       ist=1
       ii=0;jj=0
       ll=0;dE0=0.0d0;kk=0;dE1=0.0d0;dE2=0.0d0
!      jj=5
       do 1000 Ncycle=1,100000

        Tim1=CPUTim(0)
        if(iDIIS.eq.0)then
         jj=1
         goto 5000
        endif
        ii=ii+1;jj=jj+1
        if(ii.gt.10)ii=ii-10
        if(jj.le.5)goto 5000
        jj=1
        if(Ncycle.le.Ncycle0+10)then
         write(io,*)'initial DIIS'
         nDIIS=0
         goto 2000
        endif
        nDIIS=nDIIS+1
        write(io, '(6x,a)') 'DIIS cycle'
        call DIIS(io,iDIIS,icoe,KKK0,t,ncoe)

      Sec=CPUTim(0)-Tim1
      NMin=Sec/60
      Sec=Sec-NMin*60
      !write(io,'(a,i5,'' min'',f5.1,'' s'')') &
     !'DIIS',NMin,Sec
      Tim2=CPUTim(0)
        goto 2000
5000    continue

       allocate(V2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       V2=0.0d0
       call sumx2143(N0,N3,N2,N3,N1,N3,N0,N2,N0,N1,V2,IntM,-1.000)

       call t2C_update(N0,N1,N2,N3,K1,K2,K3,K4,shift,V2, &
      FockR,FockB,IntR,IntB,IntM,t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
      t(K3A),t(K3B),t(K3C),t(K3D))
       deallocate(V2)

      Sec=CPUTim(0)-Tim1
      NMin=Sec/60
      Sec=Sec-NMin*60
      !write(io,'(a,i5,'' min'',f5.1,'' s'')')  't2C',NMin,Sec
      Tim2=CPUTim(0)

       allocate(V2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       V2=0.0d0
       call sumx2143(N0,N3,N1,N3,N1,N3,N0,N1,N0,N1,V2,IntR,-1.000)

       call t2A_update(N0,N1,N2,N3,K1,K2,K3,K4,shift,V2, &
      FockR,FockB,IntR,IntB,IntM,t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
      t(K3A),t(K3B),t(K3C),t(K3D))
       deallocate(V2)

      Sec=CPUTim(0)-Tim2
      NMin=Sec/60
      Sec=Sec-NMin*60
      !write(io,'(a,i5,'' min'',f5.1,'' s'')')  't2A',NMin,Sec
      Tim2=CPUTim(0)

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

      Sec=CPUTim(0)-Tim2
      NMin=Sec/60
      Sec=Sec-NMin*60
      !write(io,'(a,i5,'' min'',f5.1,'' s'')')  't2B',NMin,Sec
      Tim2=CPUTim(0)
      endif

       allocate(V1(N1+1:N3,N0+1:N1))
       V1=0.0d0
       call sumx12(0,N3,N1,N3,N0,N1,V1,FockR, 1.000)

       call t1A_update(N0,N1,N2,N3,K1,K2,K3,K4,shift,V1, &
      FockR,FockB,IntR,IntB,IntM,t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
      t(K3A),t(K3B),t(K3C),t(K3D))
       deallocate(V1)

      Sec=CPUTim(0)-Tim2
      NMin=Sec/60
      Sec=Sec-NMin*60
      !write(io,'(a,i5,'' min'',f5.1,'' s'')')  't1A',NMin,Sec
      Tim2=CPUTim(0)

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

      Sec=CPUTim(0)-Tim2
      NMin=Sec/60
      Sec=Sec-NMin*60
      !write(io,'(a,i5,'' min'',f5.1,'' s'')')  't1B',NMin,Sec
      Tim2=CPUTim(0)
      endif
      !goto 2000

       allocate(V3(K3,K3,K3,K1,K1,K1))
       V3=0.0d0

       call t3A_update(N0,N1,N2,N3,K1,K2,K3,K4,shift,V3, &
      FockR,FockB,IntR,IntB,IntM,t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
      t(K3A),t(K3B),t(K3C),t(K3D),p_space)
       deallocate(V3)
      !call if0A(N0,N1,N2,N3,M1,M2,KKK,K3A,t,p_space)

      Sec=CPUTim(0)-Tim2
      NMin=Sec/60
      Sec=Sec-NMin*60
      !write(io,'(a,i5,'' min'',f5.1,'' s'')')  't3A',NMin,Sec
      Tim2=CPUTim(0)

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
      !call if0B(N0,N1,N2,N3,M1,M2,KKK,K3B,t,p_space)

      Sec=CPUTim(0)-Tim2
      NMin=Sec/60
      Sec=Sec-NMin*60
      !write(io,'(a,i5,'' min'',f5.1,'' s'')')  't3B',NMin,Sec
      Tim2=CPUTim(0)
      endif

       allocate(V3(K3,K4,K3,K1,K2,K1))
       V3=0.0d0

       call t3C_update(N0,N1,N2,N3,K1,K2,K3,K4,shift,V3, &
      FockR,FockB,IntR,IntB,IntM,t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
      t(K3A),t(K3B),t(K3C),t(K3D),p_space)
       deallocate(V3)
      !call if0C(N0,N1,N2,N3,M1,M2,KKK,K3C,t,p_space)

      Sec=CPUTim(0)-Tim2
      NMin=Sec/60
      Sec=Sec-NMin*60
      !write(io,'(a,i5,'' min'',f5.1,'' s'')')  't3C',NMin,Sec
      Tim2=CPUTim(0)

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
      !call if0D(N0,N1,N2,N3,M1,M2,KKK,K3D,t,p_space)

      Sec=CPUTim(0)-Tim2
      NMin=Sec/60
      Sec=Sec-NMin*60
      !write(io,'(a,i5,'' min'',f5.1,'' s'')')  't3D',NMin,Sec
      Tim2=CPUTim(0)
      endif

2000   continue

      !call if0(N0,N1,N2,N3,M1,M2,KKK,K3A,t,p_space)

       call Cal_E(N1,N2,N3,N0, &
      FockR,FockB,IntR,IntB,IntM,t(K1A),t(K1B),t(K2A),t(K2B), &
      t(K2C),E1A,E1B,E2A,E2B,E2C,E1A1A,E1A1B,E1B1B)

       ECorNew=E1A+E1B+E2A+E2B+E2C+E1A1A+E1A1B+E1B1B
       dE=ECor-ECorNew

!stop
      Sec=CPUTim(0)-Tim1
      NMin=Sec/60
      Sec=Sec-NMin*60
      write(io,'(i4,2(f15.10),i5,'' min'',f5.1,'' s'')') NCycle-Ncycle0,ECorNew,de,NMin,Sec

      ! Shift error vectors
      if(iDIIS.gt.0)then
        ic=icoe(1)
        do i=1,iDIIS+1
         icoe(i)=icoe(i+1)
        enddo
        icoe(iDIIS+2)=ic
        call write_coe(ic,KKK0,t,ncoe)
       endif

       rewind(ifr)
       write(ifr)ist,ECorNew,E1A,E1B,E2A,E2B,E2C,E1A1A,E1B1B,E1A1B,KKK0
!      write(ifr)(t(i),i=1,KKK0)
       write(ifr)t
      !call count_t3(io,N0,N1,N2,N3,M1,M2,t(K3A),t(K3B),t(K3C),t(K3D))

        if(Ncycle.gt.ncycle0+1.and.jj.eq.1)then
         if(iDIIS.gt.0)then
          dE2=ECorNeW-ECor1
          !write(io,*)'err between DIIS',dE2
         endif
      if (dabs(dE2).le.10.0d0**(-itol) &
          .and.dabs(dE).le.10.0d0**(-itol) &
          .and.dabs(dE)-dabs(dE0).le.10.0d0**(-itol) &
          .and.dabs(dE2)-dabs(dE1).le.10.0d0**(-itol))then
               ECor=ECorNew;dE0=dE
               ECor1=ECor;dE1=dE2
      call NJ_date(io,'UCCSDt0 calculation over on:')
      call NJ_cputim(io,Tim0)
      call NJ_walltim(io,Wall0)
                        goto 4000
                        return
                endif
         ECor=ECorNew;dE0=dE
         ECor1=ECor;dE1=dE2
        endif
        ECor=ECorNew;dE0=dE
        ll=ll+1
1000   continue
       stop 'out of cycle'

! deallocate array

4000   deallocate(t)

       return

       end

      subroutine count_t3(io,N0,N1,N2,N3,M1,M2,t3A,t3B,t3C,t3D)
       real*8 t3A(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)
       real*8 t3B(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2)
       real*8 t3C(N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N1)
       real*8 t3D(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1)
      integer a,b,c
       itol=10
        i0=0
        i1=0
       do i=N0+1,N1-2
       do j=i+1,N1-1
       do k=j+1,N1
       do a=N1+1,N3-2
       do b=a+1,N3-1
       do c=b+1,N3
         i1=i1+1
         if(dabs(t3A(c,b,a,k,j,i)).gt.10.0d0**(-itol))i0=i0+1
        enddo;enddo;enddo
        enddo;enddo;enddo
       do i=N0+1,N2-2
       do j=i+1,N2-1
       do k=j+1,N2
       do a=N2+1,N3-2
       do b=a+1,N3-1
       do c=b+1,N3
         i1=i1+1
         if(dabs(t3B(c,b,a,k,j,i)).gt.10.0d0**(-itol))i0=i0+1
        enddo;enddo;enddo
        enddo;enddo;enddo
       do i=N0+1,N1-1
       do j=N0+1,N2
       do k=i+1,N1
       do a=N1+1,N3-1
       do b=N2+1,N3
       do c=a+1,N3
         i1=i1+1
         if(dabs(t3C(c,b,a,k,j,i)).gt.10.0d0**(-itol))i0=i0+1
        enddo;enddo;enddo
        enddo;enddo;enddo
       do i=N0+1,N1
       do j=N0+1,N2-1
       do k=j+1,N2
       do a=N1+1,N3
       do b=N2+1,N3-1
       do c=b+1,N3
         i1=i1+1
         if(dabs(t3D(c,b,a,k,j,i)).gt.10.0d0**(-itol))i0=i0+1
        enddo;enddo;enddo
        enddo;enddo;enddo
      !write(io,*)'Unique  t3',i1
      !write(io,*)'Unique nonzero t3',i0
       end

      subroutine if0(N0,N1,N2,N3,M1,M2,KKK,K3A,t,p_space)
      real*8 t(KKK)
      integer p_space(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)
      integer a,b,c
        i0=K3A-1
        do i=N0+1,N1;do j=N0+1,N1;do k=N0+1,N1
        do a=N1+1,N3;do b=N1+1,N3;do c=N1+1,N3
         i0=i0+1
         if (p_space(a,b,c,i,j,k) == 1) then
             cycle
         endif
         t(i0)=0.0d0
        enddo;enddo;enddo
        enddo;enddo;enddo
        do i=N0+1,N2;do j=N0+1,N2;do k=N0+1,N2
        do a=N2+1,N3;do b=N2+1,N3;do c=N2+1,N3
         i0=i0+1
         if(p_space(a,b,c,i,j,k) == 1) then
             cycle
         endif
         t(i0)=0.0d0
        enddo;enddo;enddo
        enddo;enddo;enddo
        do i=N0+1,N1;do j=N0+1,N2;do k=N0+1,N1
        do a=N1+1,N3;do b=N2+1,N3;do c=N1+1,N3
         i0=i0+1
         if(p_space(a,b,c,i,j,k) == 1) then
             cycle
         endif
         t(i0)=0.0d0
        enddo;enddo;enddo
        enddo;enddo;enddo
        do i=N0+1,N1;do j=N0+1,N2;do k=N0+1,N2
        do a=N1+1,N3;do b=N2+1,N3;do c=N2+1,N3
         i0=i0+1
         if(p_space(a,b,c,i,j,k) == 1) then
             cycle
         endif
         t(i0)=0.0d0
        enddo;enddo;enddo
        enddo;enddo;enddo
    end subroutine if0

      subroutine if0A(N0,N1,N2,N3,M1,M2,KKK,K3A,t,p_space)
      real*8 t(KKK)
      integer p_space(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)
      integer a,b,c
        i0=K3A-1
        do i=N0+1,N1;do j=N0+1,N1;do k=N0+1,N1
        do a=N1+1,N3;do b=N1+1,N3;do c=N1+1,N3
         i0=i0+1
         if(p_space(a,b,c,i,j,k) == 1) then
             cycle
         endif
         t(i0)=0.0d0
        enddo;enddo;enddo
        enddo;enddo;enddo
    end subroutine if0a

      subroutine if0B(N0,N1,N2,N3,M1,M2,KKK,K3B,t,p_space)
      real*8 t(KKK)
      integer p_space(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)
      integer a,b,c
        i0=K3B-1
        do i=N0+1,N2;do j=N0+1,N2;do k=N0+1,N2
        do a=N2+1,N3;do b=N2+1,N3;do c=N2+1,N3
         i0=i0+1
         if(p_space(a,b,c,i,j,k) == 1) then
             cycle
         endif
         t(i0)=0.0d0
        enddo;enddo;enddo
        enddo;enddo;enddo
    end subroutine if0b

      subroutine if0C(N0,N1,N2,N3,M1,M2,KKK,K3C,t,p_space)
      real*8 t(KKK)
      integer p_space(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)
      integer a,b,c
        i0=K3C-1
        do i=N0+1,N1;do j=N0+1,N2;do k=N0+1,N1
        do a=N1+1,N3;do b=N2+1,N3;do c=N1+1,N3
         i0=i0+1
         if(p_space(a,b,c,i,j,k) == 1) then
             cycle
         endif
         t(i0)=0.0d0
        enddo;enddo;enddo
        enddo;enddo;enddo
    end subroutine if0c

      subroutine if0D(N0,N1,N2,N3,M1,M2,KKK,K3D,t,p_space)
      real*8 t(KKK)
      integer p_space(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)
      integer a,b,c
        NACT=1
        i0=K3D-1
        do i=N0+1,N1;do j=N0+1,N2;do k=N0+1,N2
        do a=N1+1,N3;do b=N2+1,N3;do c=N2+1,N3
         i0=i0+1
         if(p_space(a,b,c,i,j,k) == 1) then
             cycle
         endif
         t(i0)=0.0d0
        enddo;enddo;enddo
        enddo;enddo;enddo
    end subroutine if0d

