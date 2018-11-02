       subroutine VtoHR1234(M1,N1,M2,N2,M3,N3,M4,N4,
     & K1,L1,K2,L2,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4)
       do I1=M1+1,N1
       do I2=M2+1,N2
       do I3=M3+1,N3
       do I4=M4+1,N4
        B(I1,I2,I3,I4)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine VtoHR1243(M1,N1,M2,N2,M3,N3,M4,N4,
     & K1,L1,K2,L2,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4)
       do I1=M1+1,N1
       do I2=M2+1,N2
       do I3=M3+1,N3
       do I4=M4+1,N4
        B(I1,I2,I4,I3)=-A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine VtoHR1324(M1,N1,M2,N2,M3,N3,M4,N4,
     & K1,L1,K3,L3,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4)
       do I1=M1+1,N1
       do I2=M2+1,N2
       do I3=M3+1,N3
       do I4=M4+1,N4
        B(I1,I3,I2,I4)=-A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine VtoHR1432(M1,N1,M2,N2,M3,N3,M4,N4,
     & K1,L1,K2,L2,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4)
       do I1=M1+1,N1
       do I2=M2+1,N2
       do I3=M3+1,N3
       do I4=M4+1,N4
        B(I1,I4,I3,I2)=-A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine VtoHR2134(M1,N1,M2,N2,M3,N3,M4,N4,
     & K2,L2,K1,L1,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4)
       do I1=M1+1,N1
       do I2=M2+1,N2
       do I3=M3+1,N3
       do I4=M4+1,N4
        B(I2,I1,I3,I4)=-A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine VtoHR2143(M1,N1,M2,N2,M3,N3,M4,N4,
     & K2,L2,K1,L1,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4)
       do I1=M1+1,N1
       do I2=M2+1,N2
       do I3=M3+1,N3
       do I4=M4+1,N4
        B(I2,I1,I4,I3)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine VtoHR3214(M1,N1,M2,N2,M3,N3,M4,N4,
     & K3,L3,K2,L2,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4)
       do I1=M1+1,N1
       do I2=M2+1,N2
       do I3=M3+1,N3
       do I4=M4+1,N4
        B(I3,I2,I1,I4)=-A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder1234(M1,N1,M2,N2,M3,N3,M4,N4,
     & K1,L1,K2,L2,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I1,I2,I3,I4)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder1243(M1,N1,M2,N2,M3,N3,M4,N4,
     & K1,L1,K2,L2,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I1,I2,I4,I3)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder1324(M1,N1,M2,N2,M3,N3,M4,N4,
     & K1,L1,K3,L3,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I1,I3,I2,I4)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder1342(M1,N1,M2,N2,M3,N3,M4,N4,
     & K1,L1,K3,L3,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I1,I3,I4,I2)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder1423(M1,N1,M2,N2,M3,N3,M4,N4,
     & K1,L1,K4,L4,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I1,I4,I2,I3)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder1432(M1,N1,M2,N2,M3,N3,M4,N4,
     & K1,L1,K4,L4,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I1,I4,I3,I2)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder2134(M1,N1,M2,N2,M3,N3,M4,N4,
     & K2,L2,K1,L1,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I2,I1,I3,I4)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder2143(M1,N1,M2,N2,M3,N3,M4,N4,
     & K2,L2,K1,L1,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I2,I1,I4,I3)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder2314(M1,N1,M2,N2,M3,N3,M4,N4,
     & K2,L2,K3,L3,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I2,I3,I1,I4)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder2341(M1,N1,M2,N2,M3,N3,M4,N4,
     & K2,L2,K3,L3,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I2,I3,I4,I1)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder2413(M1,N1,M2,N2,M3,N3,M4,N4,
     & K2,L2,K4,L4,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I2,I4,I1,I3)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder2431(M1,N1,M2,N2,M3,N3,M4,N4,
     & K2,L2,K4,L4,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I2,I4,I3,I1)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder3124(M1,N1,M2,N2,M3,N3,M4,N4,
     & K3,L3,K1,L1,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I3,I1,I2,I4)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder3142(M1,N1,M2,N2,M3,N3,M4,N4,
     & K3,L3,K1,L1,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I3,I1,I4,I2)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder3214(M1,N1,M2,N2,M3,N3,M4,N4,
     & K3,L3,K2,L2,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I3,I2,I1,I4)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder3241(M1,N1,M2,N2,M3,N3,M4,N4,
     & K3,L3,K2,L2,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I3,I2,I4,I1)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder3412(M1,N1,M2,N2,M3,N3,M4,N4,
     & K3,L3,K4,L4,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I3,I4,I1,I2)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder3421(M1,N1,M2,N2,M3,N3,M4,N4,
     & K3,L3,K4,L4,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I3,I4,I2,I1)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder4123(M1,N1,M2,N2,M3,N3,M4,N4,
     & K4,L4,K1,L1,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I4,I1,I2,I3)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder4132(M1,N1,M2,N2,M3,N3,M4,N4,
     & K4,L4,K1,L1,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I4,I1,I3,I2)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder4213(M1,N1,M2,N2,M3,N3,M4,N4,
     & K4,L4,K2,L2,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I4,I2,I1,I3)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder4231(M1,N1,M2,N2,M3,N3,M4,N4,
     & K4,L4,K2,L2,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I4,I2,I3,I1)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder4312(M1,N1,M2,N2,M3,N3,M4,N4,
     & K4,L4,K3,L3,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I4,I3,I1,I2)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder4321(M1,N1,M2,N2,M3,N3,M4,N4,
     & K4,L4,K3,L3,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I4,I3,I2,I1)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
