       subroutine L_update(N0,N1,N2,N3,shift,H1A,H1B,ECor,
     & l1A,l1B,l2A,l2B,l2C,l3A,l3B,l3C,l3D,
     & V1A,V1B,V2A,V2B,V2C,V3A,V3B,V3C,V3D)
C
       integer a,b,c
       real*8 shift,PP,ECor
       real*8 H1A(N0+1:N3,N0+1:N3)
       real*8 H1B(N0+1:N3,N0+1:N3)
       real*8 l1A(N1+1:N3,N0+1:N1)
       real*8 l1B(N2+1:N3,N0+1:N2)
       real*8 l2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 l2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 l2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
       real*8 l3A(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)
       real*8 l3B(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1)
       real*8 l3C(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1)
       real*8 l3D(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2)
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
       do i=N0+1,N1;do a=N1+1,N3
         PP=H1A(a,a)-H1A(i,i) -ECor+shift
         l1A(a,i)=l1A(a,i)-V1A(a,i)/PP
       enddo;enddo
       do i=N0+1,N2;do a=N2+1,N3
         PP=H1B(a,a)-H1B(i,i) -ECor+shift
         l1B(a,i)=l1B(a,i)-V1B(a,i)/PP
       enddo;enddo
       do i=N0+1,N1;do j=N0+1,N1
       do a=N1+1,N3;do b=N1+1,N3
         PP=H1A(b,b)+H1A(a,a)-H1A(j,j)-H1A(i,i) -ECor+shift
         l2A(b,a,j,i)= l2A(b,a,j,i)-V2A(b,a,j,i)/PP
       enddo;enddo;enddo;enddo
       do i=N0+1,N1;do j=N0+1,N2
       do a=N1+1,N3;do b=N2+1,N3
         PP=H1B(b,b)+H1A(a,a)-H1B(j,j)-H1A(i,i) -ECor+shift
         l2B(b,a,j,i)= l2B(b,a,j,i)-V2B(b,a,j,i)/PP
       enddo;enddo;enddo;enddo
       do i=N0+1,N2;do j=N0+1,N2
       do a=N2+1,N3;do b=N2+1,N3
         PP=H1B(b,b)+H1B(a,a)-H1B(j,j)-H1B(i,i) -ECor+shift
         l2C(b,a,j,i)= l2C(b,a,j,i)-V2C(b,a,j,i)/PP
       enddo;enddo;enddo;enddo
       do i=N0+1,N1;do j=N0+1,N1;do k=N0+1,N1
       do a=N1+1,N3;do b=N1+1,N3;do c=N1+1,N3
         PP=H1A(c,c)+H1A(b,b)+H1A(a,a)-H1A(k,k)-H1A(j,j)-H1A(i,i)
     &      -ECor+shift
         l3A(c,b,a,k,j,i)= l3A(c,b,a,k,j,i)-V3A(c,b,a,k,j,i)/PP
       enddo;enddo;enddo;enddo;enddo;enddo
       do i=N0+1,N1;do j=N0+1,N1;do k=N0+1,N2
       do a=N1+1,N3;do b=N1+1,N3;do c=N2+1,N3
         PP=H1B(c,c)+H1A(b,b)+H1A(a,a)-H1B(k,k)-H1A(j,j)-H1A(i,i)
     &      -ECor+shift
         l3B(c,b,a,k,j,i)= l3B(c,b,a,k,j,i)-V3B(c,b,a,k,j,i)/PP
       enddo;enddo;enddo;enddo;enddo;enddo
       do i=N0+1,N1;do j=N0+1,N2;do k=N0+1,N2
       do a=N1+1,N3;do b=N2+1,N3;do c=N2+1,N3
         PP=H1B(c,c)+H1B(b,b)+H1A(a,a)-H1B(k,k)-H1B(j,j)-H1A(i,i)
     &      -ECor+shift
         l3C(c,b,a,k,j,i)= l3C(c,b,a,k,j,i)-V3C(c,b,a,k,j,i)/PP
       enddo;enddo;enddo;enddo;enddo;enddo
       do i=N0+1,N2;do j=N0+1,N2;do k=N0+1,N2
       do a=N2+1,N3;do b=N2+1,N3;do c=N2+1,N3
         PP=H1B(c,c)+H1B(b,b)+H1B(a,a)-H1B(k,k)-H1B(j,j)-H1B(i,i)
     &      -ECor+shift
         l3D(c,b,a,k,j,i)= l3D(c,b,a,k,j,i)-V3D(c,b,a,k,j,i)/PP
       enddo;enddo;enddo;enddo;enddo;enddo
C
       end
C
       subroutine L_update1(N0,N1,N2,N3,shift,H1A,H1B,ECor,
     & l1A,l1B,l2A,l2B,l2C,l3A,l3B,l3C,l3D,
     & V1A,V1B,V2A,V2B,V2C,V3A,V3B,V3C,V3D)
C
       integer a,b,c
       real*8 shift,PP,ECor
       real*8 H1A(N0+1:N3,N0+1:N3)
       real*8 H1B(N0+1:N3,N0+1:N3)
       real*8 l1A(N1+1:N3,N0+1:N1)
       real*8 l1B(N2+1:N3,N0+1:N2)
       real*8 l2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 l2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 l2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
       real*8 l3A(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)
       real*8 l3B(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1)
       real*8 l3C(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1)
       real*8 l3D(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2)
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
       do i=N0+1,N1;do a=N1+1,N3
         PP=H1A(a,a)-H1A(i,i)-ECor+shift
         l1A(a,i)=l1A(a,i)-V1A(a,i)/PP
       enddo;enddo
       do i=N0+1,N2;do a=N2+1,N3
         PP=H1B(a,a)-H1B(i,i)-ECor+shift
         l1B(a,i)=l1B(a,i)-V1B(a,i)/PP
       enddo;enddo
       do i=N0+1,N1-1;do j=i+1,N1
       do a=N1+1,N3-1;do b=a+1,N3
         PP=H1A(b,b)+H1A(a,a)-H1A(j,j)-H1A(i,i)-ECor+shift
         l2A(b,a,j,i)= l2A(b,a,j,i)-V2A(b,a,j,i)/PP
         l2A(b,a,i,j)=-l2A(b,a,j,i)
         l2A(a,b,j,i)=-l2A(b,a,j,i)
         l2A(a,b,i,j)= l2A(b,a,j,i)
       enddo;enddo;enddo;enddo
       do i=N0+1,N1;do j=N0+1,N2
       do a=N1+1,N3;do b=N2+1,N3
         PP=H1B(b,b)+H1A(a,a)-H1B(j,j)-H1A(i,i)-ECor+shift
         l2B(b,a,j,i)= l2B(b,a,j,i)-V2B(b,a,j,i)/PP
       enddo;enddo;enddo;enddo
       do i=N0+1,N2-1;do j=i+1,N2
       do a=N2+1,N3-1;do b=a+1,N3
         PP=H1B(b,b)+H1B(a,a)-H1B(j,j)-H1B(i,i)-ECor+shift
         l2C(b,a,j,i)= l2C(b,a,j,i)-V2C(b,a,j,i)/PP
         l2C(b,a,i,j)=-l2C(b,a,j,i)
         l2C(a,b,j,i)=-l2C(b,a,j,i)
         l2C(a,b,i,j)= l2C(b,a,j,i)
       enddo;enddo;enddo;enddo
       do i=N0+1,N1-2;do j=i+1,N1-1;do k=j+1,N1
       do a=N1+1,N3-2;do b=a+1,N3-1;do c=b+1,N3
         PP=H1A(c,c)+H1A(b,b)+H1A(a,a)-H1A(k,k)-H1A(j,j)-H1A(i,i)
     &      -ECor+shift
         l3A(c,b,a,k,j,i)= l3A(c,b,a,k,j,i)-V3A(c,b,a,k,j,i)/PP
         l3A(c,b,a,k,i,j)=-l3A(c,b,a,k,j,i)
         l3A(c,b,a,i,j,k)=-l3A(c,b,a,k,j,i)
         l3A(c,b,a,i,k,j)= l3A(c,b,a,k,j,i)
         l3A(c,b,a,j,k,i)=-l3A(c,b,a,k,j,i)
         l3A(c,b,a,j,i,k)= l3A(c,b,a,k,j,i)
         l3A(c,a,b,k,j,i)=-l3A(c,b,a,k,j,i)
         l3A(c,a,b,k,i,j)= l3A(c,b,a,k,j,i)
         l3A(c,a,b,i,j,k)= l3A(c,b,a,k,j,i)
         l3A(c,a,b,i,k,j)=-l3A(c,b,a,k,j,i)
         l3A(c,a,b,j,k,i)= l3A(c,b,a,k,j,i)
         l3A(c,a,b,j,i,k)=-l3A(c,b,a,k,j,i)
         l3A(a,b,c,k,j,i)=-l3A(c,b,a,k,j,i)
         l3A(a,b,c,k,i,j)= l3A(c,b,a,k,j,i)
         l3A(a,b,c,i,j,k)= l3A(c,b,a,k,j,i)
         l3A(a,b,c,i,k,j)=-l3A(c,b,a,k,j,i)
         l3A(a,b,c,j,k,i)= l3A(c,b,a,k,j,i)
         l3A(a,b,c,j,i,k)=-l3A(c,b,a,k,j,i)
         l3A(a,c,b,k,j,i)= l3A(c,b,a,k,j,i)
         l3A(a,c,b,k,i,j)=-l3A(c,b,a,k,j,i)
         l3A(a,c,b,i,j,k)=-l3A(c,b,a,k,j,i)
         l3A(a,c,b,i,k,j)= l3A(c,b,a,k,j,i)
         l3A(a,c,b,j,k,i)=-l3A(c,b,a,k,j,i)
         l3A(a,c,b,j,i,k)= l3A(c,b,a,k,j,i)
         l3A(b,c,a,k,j,i)=-l3A(c,b,a,k,j,i)
         l3A(b,c,a,k,i,j)= l3A(c,b,a,k,j,i)
         l3A(b,c,a,i,j,k)= l3A(c,b,a,k,j,i)
         l3A(b,c,a,i,k,j)=-l3A(c,b,a,k,j,i)
         l3A(b,c,a,j,k,i)= l3A(c,b,a,k,j,i)
         l3A(b,c,a,j,i,k)=-l3A(c,b,a,k,j,i)
         l3A(b,a,c,k,j,i)= l3A(c,b,a,k,j,i)
         l3A(b,a,c,k,i,j)=-l3A(c,b,a,k,j,i)
         l3A(b,a,c,i,j,k)=-l3A(c,b,a,k,j,i)
         l3A(b,a,c,i,k,j)= l3A(c,b,a,k,j,i)
         l3A(b,a,c,j,k,i)=-l3A(c,b,a,k,j,i)
         l3A(b,a,c,j,i,k)= l3A(c,b,a,k,j,i)
       enddo;enddo;enddo;enddo;enddo;enddo
       do i=N0+1,N1-1;do j=i+1,N1;do k=N0+1,N2
       do a=N1+1,N3-1;do b=a+1,N3;do c=N2+1,N3
         PP=H1B(c,c)+H1A(b,b)+H1A(a,a)-H1B(k,k)-H1A(j,j)-H1A(i,i)
     &      -ECor+shift
         l3B(c,b,a,k,j,i)= l3B(c,b,a,k,j,i)-V3B(c,b,a,k,j,i)/PP
         l3B(c,b,a,k,i,j)=-l3B(c,b,a,k,j,i)
         l3B(c,a,b,k,j,i)=-l3B(c,b,a,k,j,i)
         l3B(c,a,b,k,i,j)= l3B(c,b,a,k,j,i)
       enddo;enddo;enddo;enddo;enddo;enddo
       do i=N0+1,N1;do j=N0+1,N2-1;do k=j+1,N2
       do a=N1+1,N3;do b=N2+1,N3-1;do c=b+1,N3
         PP=H1B(c,c)+H1B(b,b)+H1A(a,a)-H1B(k,k)-H1B(j,j)-H1A(i,i)
     &      -ECor+shift
         l3C(c,b,a,k,j,i)= l3C(c,b,a,k,j,i)-V3C(c,b,a,k,j,i)/PP
         l3C(c,b,a,j,k,i)=-l3C(c,b,a,k,j,i)
         l3C(b,c,a,k,j,i)=-l3C(c,b,a,k,j,i)
         l3C(b,c,a,j,k,i)= l3C(c,b,a,k,j,i)
       enddo;enddo;enddo;enddo;enddo;enddo
       do i=N0+1,N2-2;do j=i+1,N2-1;do k=j+1,N2
       do a=N2+1,N3-2;do b=a+1,N3-1;do c=b+1,N3
         PP=H1B(c,c)+H1B(b,b)+H1B(a,a)-H1B(k,k)-H1B(j,j)-H1B(i,i)
     &      -ECor+shift
         l3D(c,b,a,k,j,i)= l3D(c,b,a,k,j,i)-V3D(c,b,a,k,j,i)/PP
         l3D(c,b,a,k,i,j)=-l3D(c,b,a,k,j,i)
         l3D(c,b,a,i,j,k)=-l3D(c,b,a,k,j,i)
         l3D(c,b,a,i,k,j)= l3D(c,b,a,k,j,i)
         l3D(c,b,a,j,k,i)=-l3D(c,b,a,k,j,i)
         l3D(c,b,a,j,i,k)= l3D(c,b,a,k,j,i)
         l3D(c,a,b,k,j,i)=-l3D(c,b,a,k,j,i)
         l3D(c,a,b,k,i,j)= l3D(c,b,a,k,j,i)
         l3D(c,a,b,i,j,k)= l3D(c,b,a,k,j,i)
         l3D(c,a,b,i,k,j)=-l3D(c,b,a,k,j,i)
         l3D(c,a,b,j,k,i)= l3D(c,b,a,k,j,i)
         l3D(c,a,b,j,i,k)=-l3D(c,b,a,k,j,i)
         l3D(a,b,c,k,j,i)=-l3D(c,b,a,k,j,i)
         l3D(a,b,c,k,i,j)= l3D(c,b,a,k,j,i)
         l3D(a,b,c,i,j,k)= l3D(c,b,a,k,j,i)
         l3D(a,b,c,i,k,j)=-l3D(c,b,a,k,j,i)
         l3D(a,b,c,j,k,i)= l3D(c,b,a,k,j,i)
         l3D(a,b,c,j,i,k)=-l3D(c,b,a,k,j,i)
         l3D(a,c,b,k,j,i)= l3D(c,b,a,k,j,i)
         l3D(a,c,b,k,i,j)=-l3D(c,b,a,k,j,i)
         l3D(a,c,b,i,j,k)=-l3D(c,b,a,k,j,i)
         l3D(a,c,b,i,k,j)= l3D(c,b,a,k,j,i)
         l3D(a,c,b,j,k,i)=-l3D(c,b,a,k,j,i)
         l3D(a,c,b,j,i,k)= l3D(c,b,a,k,j,i)
         l3D(b,c,a,k,j,i)=-l3D(c,b,a,k,j,i)
         l3D(b,c,a,k,i,j)= l3D(c,b,a,k,j,i)
         l3D(b,c,a,i,j,k)= l3D(c,b,a,k,j,i)
         l3D(b,c,a,i,k,j)=-l3D(c,b,a,k,j,i)
         l3D(b,c,a,j,k,i)= l3D(c,b,a,k,j,i)
         l3D(b,c,a,j,i,k)=-l3D(c,b,a,k,j,i)
         l3D(b,a,c,k,j,i)= l3D(c,b,a,k,j,i)
         l3D(b,a,c,k,i,j)=-l3D(c,b,a,k,j,i)
         l3D(b,a,c,i,j,k)=-l3D(c,b,a,k,j,i)
         l3D(b,a,c,i,k,j)= l3D(c,b,a,k,j,i)
         l3D(b,a,c,j,k,i)=-l3D(c,b,a,k,j,i)
         l3D(b,a,c,j,i,k)= l3D(c,b,a,k,j,i)
       enddo;enddo;enddo;enddo;enddo;enddo
C
       end
