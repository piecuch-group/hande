       subroutine VtoHR12(M1,N1,M2,N2,
     & K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2)
       real*8 B(K1+1:L1,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2)
       do I1=M1+1,N1
       do I2=M2+1,N2
        B(I1,I2)=A(I1,I2)
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine VtoHR21(M1,N1,M2,N2,
     & K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2)
       real*8 B(K2+1:L2,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2)
       do I1=M1+1,N1
       do I2=M2+1,N2
        B(I2,I1)=-A(I1,I2)
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder12(M1,N1,M2,N2,
     & K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2)
       real*8 B(K1+1:L1,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2)
       do I1=K1+1,L1
       do I2=K2+1,L2
        B(I1,I2)=A(I1,I2)
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder21(M1,N1,M2,N2,
     & K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2)
       real*8 B(K2+1:L2,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2)
       do I1=K1+1,L1
       do I2=K2+1,L2
        B(I2,I1)=A(I1,I2)
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
