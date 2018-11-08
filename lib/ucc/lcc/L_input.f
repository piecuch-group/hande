       subroutine L_input(N0,N1,N2,N3,iroot,
     & H1A,H1B,H2A,H2B,H2C,l1A,l1B,l2A,l2B,l2C,
     & V1A,V1B,V2A,V2B,V2C,V3A,V3B,V3C,V3D)
C
       integer a,b,c
       real*8 H1A(N0+1:N3,N0+1:N3)
       real*8 H1B(N0+1:N3,N0+1:N3)
       real*8 H2A(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 H2B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 H2C(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 l1A(N1+1:N3,N0+1:N1)
       real*8 l1B(N2+1:N3,N0+1:N2)
       real*8 l2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 l2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 l2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
       real*8 V1A(N1+1:N3,N0+1:N1)
       real*8 V1B(N2+1:N3,N0+1:N2)
       real*8 V2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 V2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 V2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
       real*8 V3A(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)
       real*8 V3B(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1)
       real*8 V3C(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1)
       real*8 V3D(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2)
C
       do i=N0+1,N1;do j=N0+1,N1
       do a=N1+1,N3;do b=N1+1,N3
        V2A(b,a,j,i)=-H1A(b,i)*l1A(a,j)
     &               +H1A(a,i)*l1A(b,j)
     &               +H1A(b,j)*l1A(a,i)
     &               -H1A(a,j)*l1A(b,i)
       enddo;enddo;enddo;enddo
C
       do i=N0+1,N1;do j=N0+1,N2
       do a=N1+1,N3;do b=N2+1,N3
        V2B(b,a,j,i)=+H1B(b,j)*l1A(a,i)
     &               +H1A(a,i)*l1B(b,j)
       enddo;enddo;enddo;enddo
C
       do i=N0+1,N2;do j=N0+1,N2
       do a=N2+1,N3;do b=N2+1,N3
        V2C(b,a,j,i)=-H1B(b,i)*l1B(a,j)
     &               +H1B(a,i)*l1B(b,j)
     &               +H1B(b,j)*l1B(a,i)
     &               -H1B(a,j)*l1B(b,i)
       enddo;enddo;enddo;enddo
C
       do i=N0+1,N1;do j=N0+1,N1;do k=N0+1,N1
       do a=N1+1,N3;do b=N1+1,N3;do c=N1+1,N3
        V3A(c,b,a,k,j,i)=+H1A(c,i)*l2A(b,a,k,j)
     &                   -H1A(b,i)*l2A(c,a,k,j)
     &                   +H1A(a,i)*l2A(c,b,k,j)
     &                   -H1A(c,j)*l2A(b,a,k,i)
     &                   +H1A(b,j)*l2A(c,a,k,i)
     &                   -H1A(a,j)*l2A(c,b,k,i)
     &                   +H1A(c,k)*l2A(b,a,j,i)
     &                   -H1A(b,k)*l2A(c,a,j,i)
     &                   +H1A(a,k)*l2A(c,b,j,i)
     &                   +l1A(c,i)*H2A(b,a,k,j)
     &                   -l1A(b,i)*H2A(c,a,k,j)
     &                   +l1A(a,i)*H2A(c,b,k,j)
     &                   -l1A(c,j)*H2A(b,a,k,i)
     &                   +l1A(b,j)*H2A(c,a,k,i)
     &                   -l1A(a,j)*H2A(c,b,k,i)
     &                   +l1A(c,k)*H2A(b,a,j,i)
     &                   -l1A(b,k)*H2A(c,a,j,i)
     &                   +l1A(a,k)*H2A(c,b,j,i)
       enddo;enddo;enddo;enddo;enddo;enddo
C
       do i=N0+1,N1;do j=N0+1,N1;do k=N0+1,N2
       do a=N1+1,N3;do b=N1+1,N3;do c=N2+1,N3
        V3B(c,b,a,k,j,i)=+H1B(c,k)*l2A(b,a,j,i)
     &                   -H1A(b,i)*l2B(c,a,k,j)
     &                   +H1A(a,i)*l2B(c,b,k,j)
     &                   +H1A(b,j)*l2B(c,a,k,i)
     &                   -H1A(a,j)*l2B(c,b,k,i)
     &                   +l1B(c,k)*H2A(b,a,j,i)
     &                   -l1A(b,i)*H2B(c,a,k,j)
     &                   +l1A(a,i)*H2B(c,b,k,j)
     &                   +l1A(b,j)*H2B(c,a,k,i)
     &                   -l1A(a,j)*H2B(c,b,k,i)
       enddo;enddo;enddo;enddo;enddo;enddo
C
       do i=N0+1,N1;do j=N0+1,N2;do k=N0+1,N2
       do a=N1+1,N3;do b=N2+1,N3;do c=N2+1,N3
        V3C(c,b,a,k,j,i)=-H1B(c,j)*l2B(b,a,k,i)
     &                   +H1B(b,j)*l2B(c,a,k,i)
     &                   +H1B(c,k)*l2B(b,a,j,i)
     &                   -H1B(b,k)*l2B(c,a,j,i)
     &                   +H1A(a,i)*l2C(c,b,k,j)
     &                   -l1B(c,j)*H2B(b,a,k,i)
     &                   +l1B(b,j)*H2B(c,a,k,i)
     &                   +l1B(c,k)*H2B(b,a,j,i)
     &                   -l1B(b,k)*H2B(c,a,j,i)
     &                   +l1A(a,i)*H2C(c,b,k,j)
       enddo;enddo;enddo;enddo;enddo;enddo
C
       do i=N0+1,N2;do j=N0+1,N2;do k=N0+1,N2
       do a=N2+1,N3;do b=N2+1,N3;do c=N2+1,N3
        V3D(c,b,a,k,j,i)=+H1B(c,i)*l2C(b,a,k,j)
     &                   -H1B(b,i)*l2C(c,a,k,j)
     &                   +H1B(a,i)*l2C(c,b,k,j)
     &                   -H1B(c,j)*l2C(b,a,k,i)
     &                   +H1B(b,j)*l2C(c,a,k,i)
     &                   -H1B(a,j)*l2C(c,b,k,i)
     &                   +H1B(c,k)*l2C(b,a,j,i)
     &                   -H1B(b,k)*l2C(c,a,j,i)
     &                   +H1B(a,k)*l2C(c,b,j,i)
     &                   +l1B(c,i)*H2C(b,a,k,j)
     &                   -l1B(b,i)*H2C(c,a,k,j)
     &                   +l1B(a,i)*H2C(c,b,k,j)
     &                   -l1B(c,j)*H2C(b,a,k,i)
     &                   +l1B(b,j)*H2C(c,a,k,i)
     &                   -l1B(a,j)*H2C(c,b,k,i)
     &                   +l1B(c,k)*H2C(b,a,j,i)
     &                   -l1B(b,k)*H2C(c,a,j,i)
     &                   +l1B(a,k)*H2C(c,b,j,i)
       enddo;enddo;enddo;enddo;enddo;enddo
C
       if(iroot.ne.0)return
       do i=N0+1,N1;do a=N1+1,N3
        V1A(a,i)=H1A(a,i)
       enddo;enddo
       do i=N0+1,N2;do a=N2+1,N3
        V1B(a,i)=H1B(a,i)
       enddo;enddo
       do i=N0+1,N1;do j=N0+1,N1;do a=N1+1,N3;do b=N1+1,N3
        V2A(b,a,j,i)=V2A(b,a,j,i)+H2A(b,a,j,i)
       enddo;enddo;enddo;enddo
       do i=N0+1,N1;do j=N0+1,N2;do a=N1+1,N3;do b=N2+1,N3
        V2B(b,a,j,i)=V2B(b,a,j,i)+H2B(b,a,j,i)
       enddo;enddo;enddo;enddo
       do i=N0+1,N2;do j=N0+1,N2;do a=N2+1,N3;do b=N2+1,N3
        V2C(b,a,j,i)=V2C(b,a,j,i)+H2C(b,a,j,i)
       enddo;enddo;enddo;enddo
C
       end
