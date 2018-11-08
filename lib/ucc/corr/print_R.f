       subroutine print_RA(io,N0,N1,N2,N3,KKK,r,PP)
C
       real*8 PP
       real*8 r(KKK)
       integer a,b,c,d
C
!      PP=1.0d-1
       i0=0
!3A
       do i=N0+1,N1;do j=N0+1,N1;do k=N0+1,N1
       do a=N1+1,N3;do b=N1+1,N3;do c=N1+1,N3
        i0=i0+1
        if(dabs(r(i0)).lt.PP)cycle
        write(io,'(i4,a,i4,a,i4,a,i4,a,i4,a,i4,a,e19.12)')
     &  i,'A  ',j,'A  ',k,'A  --> ',a,'A  ',b,'A  ',c,'A  :',r(i0)
       enddo;enddo;enddo;enddo;enddo;enddo
C
       end
       subroutine print_RB(io,N0,N1,N2,N3,KKK,r,PP)
C
       real*8 PP
       real*8 r(KKK)
       integer a,b,c,d
C
!      PP=1.0d-1
       i0=0
!3B
       do i=N0+1,N1;do j=N0+1,N1;do k=N0+1,N2
       do a=N1+1,N3;do b=N1+1,N3;do c=N2+1,N3
        i0=i0+1
        if(dabs(r(i0)).lt.PP)cycle
        write(io,'(i4,a,i4,a,i4,a,i4,a,i4,a,i4,a,e19.12)')
     &  i,'A  ',j,'A  ',k,'B  --> ',a,'A  ',b,'A  ',c,'B  :',r(i0)
       enddo;enddo;enddo;enddo;enddo;enddo
C
       end
       subroutine print_RC(io,N0,N1,N2,N3,KKK,r,PP)
C
       real*8 PP
       real*8 r(KKK)
       integer a,b,c,d
C
!      PP=1.0d-1
       i0=0
!3C
       do i=N0+1,N1;do j=N0+1,N2;do k=N0+1,N2
       do a=N1+1,N3;do b=N2+1,N3;do c=N2+1,N3
        i0=i0+1
        if(dabs(r(i0)).lt.PP)cycle
        write(io,'(i4,a,i4,a,i4,a,i4,a,i4,a,i4,a,e19.12)')
     &  i,'A  ',j,'B  ',k,'B  --> ',a,'A  ',b,'B  ',c,'B  :',r(i0)
       enddo;enddo;enddo;enddo;enddo;enddo
C
       end
       subroutine print_RD(io,N0,N1,N2,N3,KKK,r,PP)
C
       real*8 PP
       real*8 r(KKK)
       integer a,b,c,d
C
!      PP=1.0d-1
       i0=0
!3D
       do i=N0+1,N2;do j=N0+1,N2;do k=N0+1,N2
       do a=N2+1,N3;do b=N2+1,N3;do c=N2+1,N3
        i0=i0+1
        if(dabs(r(i0)).lt.PP)cycle
        write(io,'(i4,a,i4,a,i4,a,i4,a,i4,a,i4,a,e19.12)')
     &  i,'B  ',j,'B  ',k,'B  --> ',a,'B  ',b,'B  ',c,'B  :',r(i0)
       enddo;enddo;enddo;enddo;enddo;enddo
C
       end
C
       subroutine print_R(iR,io,iter,N0,N1,N2,N3,KKK,PP)
C
       real*8 PP
       real*8,allocatable:: r(:)
       integer a,b,c,d
C
!      PP=1.0d-1
       allocate(r(KKK))
       r=0.0d0
       read(iR,rec=iter)r
       i0=0
!1A
       do i=N0+1,N1;do a=N1+1,N3
        i0=i0+1
        if(dabs(r(i0)).lt.PP)cycle
        write(io,'(i4,a,i4,a,e19.12)')i,'A  --> ',a,'A  :',r(i0)
       enddo;enddo
!1B
       do i=N0+1,N2;do a=N2+1,N3
        i0=i0+1
        if(dabs(r(i0)).lt.PP)cycle
        write(io,'(i4,a,i4,a,e19.12)')i,'B  --> ',a,'B  :',r(i0)
       enddo;enddo
!2A
       do i=N0+1,N1;do j=N0+1,N1
       do a=N1+1,N3;do b=N1+1,N3
        i0=i0+1
        if(dabs(r(i0)).lt.PP)cycle
        write(io,'(i4,a,i4,a,i4,a,i4,a,e19.12)')
     &  i,'A  ',j,'A  --> ',a,'A  ',b,'A  :',r(i0)
       enddo;enddo;enddo;enddo
!2B
       do i=N0+1,N1;do j=N0+1,N2
       do a=N1+1,N3;do b=N2+1,N3
        i0=i0+1
        if(dabs(r(i0)).lt.PP)cycle
        write(io,'(i4,a,i4,a,i4,a,i4,a,e19.12)')
     &  i,'A  ',j,'B  --> ',a,'A  ',b,'B  :',r(i0)
       enddo;enddo;enddo;enddo
!2C
       do i=N0+1,N2;do j=N0+1,N2
       do a=N2+1,N3;do b=N2+1,N3
        i0=i0+1
        if(dabs(r(i0)).lt.PP)cycle
        write(io,'(i4,a,i4,a,i4,a,i4,a,e19.12)')
     &  i,'B  ',j,'B  --> ',a,'B  ',b,'B  :',r(i0)
       enddo;enddo;enddo;enddo
C
       end
C
       subroutine print_INI(io,N0,N1,N2,N3,M3,M4,r,KKK,PP)
C
       real*8 PP
       real*8 r(KKK)
       integer a,b,c,d
C
       i0=0
!1A
       do i=N0+1,N1;do a=N1+1,N3
        i0=i0+1
        if(dabs(r(i0)).lt.PP)cycle
        write(io,'(i4,a,i4,a,e19.12)')i,'A  --> ',a,'A  :',r(i0)
       enddo;enddo
!1B
       do i=N0+1,N2;do a=N2+1,N3
        i0=i0+1
        if(dabs(r(i0)).lt.PP)cycle
        write(io,'(i4,a,i4,a,e19.12)')i,'B  --> ',a,'B  :',r(i0)
       enddo;enddo
!2A
       do i=N0+1,N1;do j=N0+1,N1
       do a=N1+1,N3;do b=N1+1,N3
        i0=i0+1
        if(dabs(r(i0)).lt.PP)cycle
        write(io,'(i4,a,i4,a,i4,a,i4,a,e19.12)')
     &  i,'A  ',j,'A  --> ',a,'A  ',b,'A  :',r(i0)
       enddo;enddo;enddo;enddo
!2B
       do i=N0+1,N1;do j=N0+1,N2
       do a=N1+1,N3;do b=N2+1,N3
        i0=i0+1
        if(dabs(r(i0)).lt.PP)cycle
        write(io,'(i4,a,i4,a,i4,a,i4,a,e19.12)')
     &  i,'A  ',j,'B  --> ',a,'A  ',b,'B  :',r(i0)
       enddo;enddo;enddo;enddo
!2C
       do i=N0+1,N2;do j=N0+1,N2
       do a=N2+1,N3;do b=N2+1,N3
        i0=i0+1
        if(dabs(r(i0)).lt.PP)cycle
        write(io,'(i4,a,i4,a,i4,a,i4,a,e19.12)')
     &  i,'B  ',j,'B  --> ',a,'B  ',b,'B  :',r(i0)
       enddo;enddo;enddo;enddo
      if(i0.ne.KKK)stop 'i0 !=KKK in print_INI'
C
       end
C
       subroutine print_INI0(iR,io,iter,N0,N1,N2,N3,KKK,PP)
C
       real*8 PP
       real*8,allocatable:: r(:)
       integer a,b,c,d
C
!      PP=1.0d-1
       allocate(r(KKK))
       r=0.0d0
       read(iR,rec=iter)r
!      read(iR,rec=iter)(r(i),i=1,KKK)
       i0=0
!1A
       do i=N0+1,N1;do a=N1+1,N3
        i0=i0+1
        if(dabs(r(i0)).lt.PP)cycle
        write(io,'(i4,a,i4,a,e19.12)')i,'A  --> ',a,'A  :',r(i0)
       enddo;enddo
!1B
       do i=N0+1,N2;do a=N2+1,N3
        i0=i0+1
        if(dabs(r(i0)).lt.PP)cycle
        write(io,'(i4,a,i4,a,e19.12)')i,'B  --> ',a,'B  :',r(i0)
       enddo;enddo
      if(i0.ne.KKK)stop 'i0 !=KKK in print_INI'
C
       end
