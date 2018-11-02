      subroutine DGEMMY(K1,K2,K3,A,B,C)
      implicit none
      integer K1,K2,K3,i,j,k
      real*8 A(K3,K1),B(K3,K2),C(K2,K1),PP
C
      do i=1,K1;do j=1,K2
       PP=0.0d0
       do k=1,K3
        PP=PP+A(k,i)*B(k,j)
	write(2234,*)k,A(k,i),B(k,j)
       enddo
       C(j,i)=PP
	write(2234,*)j,i,PP
      enddo;enddo
C
      end
      subroutine DGEMMX(K1,K2,K3,A,B,C)
      implicit none
      integer K1,K2,K3,i,j,k
      real*8 A(K3,K1),B(K3,K2),C(K2,K1),PP
C
	write(234,*)'DGEMX'
      do i=1,K1;do j=1,K2
       PP=0.0d0
       do k=1,K3
        PP=PP+A(k,i)*B(k,j)
	write(234,*)k,A(k,i),B(k,j)
       enddo
       C(j,i)=PP
	write(234,*)j,i,PP
      enddo;enddo
C
      end
C
      subroutine jungemm(K1,K2,K3,A,B,C)
      implicit none
      integer K1,K2,K3,i,j,k
      real*8 A(K3,K1),B(K3,K2),C(K2,K1)
C
      call dgemm('t', 'n', K2, K1, K3, 1.0d0, B, K3, A, K3, 0.0d0, 
     &     C, K2)
C
      end
C
      subroutine jungemm1(K1,K3,A,B,C)
      implicit none
      integer K1,K3,i,j,k
      real*8 A(K3,K1),B(K3),C(K1)
C
      call dgemv('t', K3, K1, 1.0d0, A, K3, B, 1, 0.0d0, C, 1)
C
      end
C
      subroutine jungemm2(K2,K3,A,B,C)
      implicit none
      integer K2,K3,i,j,k
      real*8 A(K3),B(K3,K2),C(K2)
C
      call dgemv('t', K3, K2, 1.0d0, B, K3, A, 1, 0.0d0, C, 1)
C
      end
C
      subroutine DGEMM0(K1,K2,K3,A,B,C)
      implicit none
      integer K1,K2,K3,i,j,k
      real*8 A(K3,K1),B(K3,K2),C(K2,K1),PP
C
	print*,'DGEMM'
      do i=1,K1;do j=1,K2
       PP=0.0d0
       do k=1,K3
        PP=PP+A(k,i)*B(k,j)
	print*,i,j,k,A(k,i),B(k,j)
       enddo
       C(j,i)=PP
	print*,j,i,PP
      enddo;enddo
C
      end
C
