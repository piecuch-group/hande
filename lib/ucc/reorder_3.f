       subroutine VtoHR123(M1,N1,M2,N2,M3,N3,
     & K1,L1,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
        B(I1,I2,I3)=A(I1,I2,I3)
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder123(M1,N1,M2,N2,M3,N3,
     & K1,L1,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
        B(I1,I2,I3)=A(I1,I2,I3)
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder132(M1,N1,M2,N2,M3,N3,
     & K1,L1,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
        B(I1,I3,I2)=A(I1,I2,I3)
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder213(M1,N1,M2,N2,M3,N3,
     & K2,L2,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
        B(I2,I1,I3)=A(I1,I2,I3)
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder231(M1,N1,M2,N2,M3,N3,
     & K2,L2,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
        B(I2,I3,I1)=A(I1,I2,I3)
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder312(M1,N1,M2,N2,M3,N3,
     & K3,L3,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
        B(I3,I1,I2)=A(I1,I2,I3)
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder321(M1,N1,M2,N2,M3,N3,
     & K3,L3,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
        B(I3,I2,I1)=A(I1,I2,I3)
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
