       subroutine VtoHR12345(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder12345(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder12354(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K1,L1,K2,L2,K3,L3,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K5+1:L5,K4+1:L4)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I1,I2,I3,I5,I4)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder12435(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K1,L1,K2,L2,K4,L4,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3,K5+1:L5)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I1,I2,I4,I3,I5)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder12453(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K1,L1,K2,L2,K4,L4,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K5+1:L5,K3+1:L3)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I1,I2,I4,I5,I3)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder12534(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K1,L1,K2,L2,K5,L5,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K1+1:L1,K2+1:L2,K5+1:L5,K3+1:L3,K4+1:L4)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I1,I2,I5,I3,I4)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder12543(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K1,L1,K2,L2,K5,L5,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K1+1:L1,K2+1:L2,K5+1:L5,K4+1:L4,K3+1:L3)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I1,I2,I5,I4,I3)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder13245(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K1,L1,K3,L3,K2,L2,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4,K5+1:L5)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I1,I3,I2,I4,I5)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder13254(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K1,L1,K3,L3,K2,L2,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K5+1:L5,K4+1:L4)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I1,I3,I2,I5,I4)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder13425(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K1,L1,K3,L3,K4,L4,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2,K5+1:L5)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I1,I3,I4,I2,I5)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder13452(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K1,L1,K3,L3,K4,L4,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K5+1:L5,K2+1:L2)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I1,I3,I4,I5,I2)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder13524(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K1,L1,K3,L3,K5,L5,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K1+1:L1,K3+1:L3,K5+1:L5,K2+1:L2,K4+1:L4)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I1,I3,I5,I2,I4)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder13542(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K1,L1,K3,L3,K5,L5,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K1+1:L1,K3+1:L3,K5+1:L5,K4+1:L4,K2+1:L2)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I1,I3,I5,I4,I2)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder14235(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K1,L1,K4,L4,K2,L2,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3,K5+1:L5)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I1,I4,I2,I3,I5)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder14253(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K1,L1,K4,L4,K2,L2,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K5+1:L5,K3+1:L3)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I1,I4,I2,I5,I3)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder14325(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K1,L1,K4,L4,K3,L3,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2,K5+1:L5)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I1,I4,I3,I2,I5)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder14352(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K1,L1,K4,L4,K3,L3,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K5+1:L5,K2+1:L2)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I1,I4,I3,I5,I2)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder14523(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K1,L1,K4,L4,K5,L5,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K1+1:L1,K4+1:L4,K5+1:L5,K2+1:L2,K3+1:L3)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I1,I4,I5,I2,I3)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder14532(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K1,L1,K4,L4,K5,L5,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K1+1:L1,K4+1:L4,K5+1:L5,K3+1:L3,K2+1:L2)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I1,I4,I5,I3,I2)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder15234(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K1,L1,K5,L5,K2,L2,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K1+1:L1,K5+1:L5,K2+1:L2,K3+1:L3,K4+1:L4)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I1,I5,I2,I3,I4)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder15243(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K1,L1,K5,L5,K2,L2,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K1+1:L1,K5+1:L5,K2+1:L2,K4+1:L4,K3+1:L3)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I1,I5,I2,I4,I3)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder15324(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K1,L1,K5,L5,K3,L3,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K1+1:L1,K5+1:L5,K3+1:L3,K2+1:L2,K4+1:L4)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I1,I5,I3,I2,I4)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder15342(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K1,L1,K5,L5,K3,L3,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K1+1:L1,K5+1:L5,K3+1:L3,K4+1:L4,K2+1:L2)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I1,I5,I3,I4,I2)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder15423(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K1,L1,K5,L5,K4,L4,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K1+1:L1,K5+1:L5,K4+1:L4,K2+1:L2,K3+1:L3)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I1,I5,I4,I2,I3)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder15432(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K1,L1,K5,L5,K4,L4,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K1+1:L1,K5+1:L5,K4+1:L4,K3+1:L3,K2+1:L2)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I1,I5,I4,I3,I2)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder21345(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K2,L2,K1,L1,K3,L3,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4,K5+1:L5)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I2,I1,I3,I4,I5)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder21354(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K2,L2,K1,L1,K3,L3,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K5+1:L5,K4+1:L4)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I2,I1,I3,I5,I4)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder21435(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K2,L2,K1,L1,K4,L4,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3,K5+1:L5)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I2,I1,I4,I3,I5)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder21453(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K2,L2,K1,L1,K4,L4,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K5+1:L5,K3+1:L3)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I2,I1,I4,I5,I3)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder21534(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K2,L2,K1,L1,K5,L5,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K2+1:L2,K1+1:L1,K5+1:L5,K3+1:L3,K4+1:L4)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I2,I1,I5,I3,I4)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder21543(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K2,L2,K1,L1,K5,L5,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K2+1:L2,K1+1:L1,K5+1:L5,K4+1:L4,K3+1:L3)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I2,I1,I5,I4,I3)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder23145(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K2,L2,K3,L3,K1,L1,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I2,I3,I1,I4,I5)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder23154(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K2,L2,K3,L3,K1,L1,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K5+1:L5,K4+1:L4)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I2,I3,I1,I5,I4)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder23415(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K2,L2,K3,L3,K4,L4,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1,K5+1:L5)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I2,I3,I4,I1,I5)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder23451(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K2,L2,K3,L3,K4,L4,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K1+1:L1)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I2,I3,I4,I5,I1)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder23514(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K2,L2,K3,L3,K5,L5,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K2+1:L2,K3+1:L3,K5+1:L5,K1+1:L1,K4+1:L4)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I2,I3,I5,I1,I4)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder23541(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K2,L2,K3,L3,K5,L5,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K2+1:L2,K3+1:L3,K5+1:L5,K4+1:L4,K1+1:L1)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I2,I3,I5,I4,I1)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder24135(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K2,L2,K4,L4,K1,L1,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3,K5+1:L5)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I2,I4,I1,I3,I5)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder24153(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K2,L2,K4,L4,K1,L1,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K5+1:L5,K3+1:L3)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I2,I4,I1,I5,I3)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder24315(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K2,L2,K4,L4,K3,L3,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1,K5+1:L5)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I2,I4,I3,I1,I5)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder24351(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K2,L2,K4,L4,K3,L3,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K5+1:L5,K1+1:L1)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I2,I4,I3,I5,I1)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder24513(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K2,L2,K4,L4,K5,L5,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K2+1:L2,K4+1:L4,K5+1:L5,K1+1:L1,K3+1:L3)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I2,I4,I5,I1,I3)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder24531(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K2,L2,K4,L4,K5,L5,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K2+1:L2,K4+1:L4,K5+1:L5,K3+1:L3,K1+1:L1)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I2,I4,I5,I3,I1)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder25134(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K2,L2,K5,L5,K1,L1,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K2+1:L2,K5+1:L5,K1+1:L1,K3+1:L3,K4+1:L4)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I2,I5,I1,I3,I4)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder25143(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K2,L2,K5,L5,K1,L1,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K2+1:L2,K5+1:L5,K1+1:L1,K4+1:L4,K3+1:L3)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I2,I5,I1,I4,I3)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder25314(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K2,L2,K5,L5,K3,L3,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K2+1:L2,K5+1:L5,K3+1:L3,K1+1:L1,K4+1:L4)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I2,I5,I3,I1,I4)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder25341(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K2,L2,K5,L5,K3,L3,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K2+1:L2,K5+1:L5,K3+1:L3,K4+1:L4,K1+1:L1)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I2,I5,I3,I4,I1)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder25413(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K2,L2,K5,L5,K4,L4,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K2+1:L2,K5+1:L5,K4+1:L4,K1+1:L1,K3+1:L3)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I2,I5,I4,I1,I3)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder25431(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K2,L2,K5,L5,K4,L4,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K2+1:L2,K5+1:L5,K4+1:L4,K3+1:L3,K1+1:L1)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I2,I5,I4,I3,I1)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder31245(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K3,L3,K1,L1,K2,L2,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4,K5+1:L5)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I3,I1,I2,I4,I5)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder31254(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K3,L3,K1,L1,K2,L2,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K5+1:L5,K4+1:L4)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I3,I1,I2,I5,I4)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder31425(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K3,L3,K1,L1,K4,L4,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2,K5+1:L5)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I3,I1,I4,I2,I5)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder31452(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K3,L3,K1,L1,K4,L4,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5,K2+1:L2)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I3,I1,I4,I5,I2)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder31524(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K3,L3,K1,L1,K5,L5,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K3+1:L3,K1+1:L1,K5+1:L5,K2+1:L2,K4+1:L4)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I3,I1,I5,I2,I4)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder31542(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K3,L3,K1,L1,K5,L5,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K3+1:L3,K1+1:L1,K5+1:L5,K4+1:L4,K2+1:L2)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I3,I1,I5,I4,I2)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder32145(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K3,L3,K2,L2,K1,L1,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4,K5+1:L5)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I3,I2,I1,I4,I5)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder32154(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K3,L3,K2,L2,K1,L1,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K5+1:L5,K4+1:L4)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I3,I2,I1,I5,I4)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder32415(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K3,L3,K2,L2,K4,L4,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1,K5+1:L5)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I3,I2,I4,I1,I5)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder32451(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K3,L3,K2,L2,K4,L4,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K5+1:L5,K1+1:L1)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I3,I2,I4,I5,I1)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder32514(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K3,L3,K2,L2,K5,L5,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K3+1:L3,K2+1:L2,K5+1:L5,K1+1:L1,K4+1:L4)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I3,I2,I5,I1,I4)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder32541(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K3,L3,K2,L2,K5,L5,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K3+1:L3,K2+1:L2,K5+1:L5,K4+1:L4,K1+1:L1)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I3,I2,I5,I4,I1)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder34125(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K3,L3,K4,L4,K1,L1,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2,K5+1:L5)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I3,I4,I1,I2,I5)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder34152(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K3,L3,K4,L4,K1,L1,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K5+1:L5,K2+1:L2)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I3,I4,I1,I5,I2)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder34215(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K3,L3,K4,L4,K2,L2,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1,K5+1:L5)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I3,I4,I2,I1,I5)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder34251(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K3,L3,K4,L4,K2,L2,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K5+1:L5,K1+1:L1)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I3,I4,I2,I5,I1)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder34512(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K3,L3,K4,L4,K5,L5,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K3+1:L3,K4+1:L4,K5+1:L5,K1+1:L1,K2+1:L2)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I3,I4,I5,I1,I2)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder34521(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K3,L3,K4,L4,K5,L5,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K3+1:L3,K4+1:L4,K5+1:L5,K2+1:L2,K1+1:L1)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I3,I4,I5,I2,I1)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder35124(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K3,L3,K5,L5,K1,L1,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K3+1:L3,K5+1:L5,K1+1:L1,K2+1:L2,K4+1:L4)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I3,I5,I1,I2,I4)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder35142(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K3,L3,K5,L5,K1,L1,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K3+1:L3,K5+1:L5,K1+1:L1,K4+1:L4,K2+1:L2)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I3,I5,I1,I4,I2)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder35214(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K3,L3,K5,L5,K2,L2,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K3+1:L3,K5+1:L5,K2+1:L2,K1+1:L1,K4+1:L4)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I3,I5,I2,I1,I4)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder35241(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K3,L3,K5,L5,K2,L2,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K3+1:L3,K5+1:L5,K2+1:L2,K4+1:L4,K1+1:L1)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I3,I5,I2,I4,I1)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder35412(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K3,L3,K5,L5,K4,L4,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K3+1:L3,K5+1:L5,K4+1:L4,K1+1:L1,K2+1:L2)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I3,I5,I4,I1,I2)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder35421(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K3,L3,K5,L5,K4,L4,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K3+1:L3,K5+1:L5,K4+1:L4,K2+1:L2,K1+1:L1)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I3,I5,I4,I2,I1)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder41235(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K4,L4,K1,L1,K2,L2,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3,K5+1:L5)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I4,I1,I2,I3,I5)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder41253(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K4,L4,K1,L1,K2,L2,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K5+1:L5,K3+1:L3)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I4,I1,I2,I5,I3)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder41325(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K4,L4,K1,L1,K3,L3,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2,K5+1:L5)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I4,I1,I3,I2,I5)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder41352(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K4,L4,K1,L1,K3,L3,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K5+1:L5,K2+1:L2)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I4,I1,I3,I5,I2)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder41523(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K4,L4,K1,L1,K5,L5,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K4+1:L4,K1+1:L1,K5+1:L5,K2+1:L2,K3+1:L3)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I4,I1,I5,I2,I3)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder41532(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K4,L4,K1,L1,K5,L5,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K4+1:L4,K1+1:L1,K5+1:L5,K3+1:L3,K2+1:L2)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I4,I1,I5,I3,I2)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder42135(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K4,L4,K2,L2,K1,L1,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3,K5+1:L5)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I4,I2,I1,I3,I5)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder42153(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K4,L4,K2,L2,K1,L1,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K5+1:L5,K3+1:L3)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I4,I2,I1,I5,I3)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder42315(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K4,L4,K2,L2,K3,L3,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1,K5+1:L5)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I4,I2,I3,I1,I5)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder42351(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K4,L4,K2,L2,K3,L3,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K5+1:L5,K1+1:L1)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I4,I2,I3,I5,I1)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder42513(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K4,L4,K2,L2,K5,L5,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K4+1:L4,K2+1:L2,K5+1:L5,K1+1:L1,K3+1:L3)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I4,I2,I5,I1,I3)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder42531(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K4,L4,K2,L2,K5,L5,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K4+1:L4,K2+1:L2,K5+1:L5,K3+1:L3,K1+1:L1)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I4,I2,I5,I3,I1)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder43125(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K4,L4,K3,L3,K1,L1,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2,K5+1:L5)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I4,I3,I1,I2,I5)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder43152(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K4,L4,K3,L3,K1,L1,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K5+1:L5,K2+1:L2)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I4,I3,I1,I5,I2)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder43215(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K4,L4,K3,L3,K2,L2,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1,K5+1:L5)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I4,I3,I2,I1,I5)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder43251(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K4,L4,K3,L3,K2,L2,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K5+1:L5,K1+1:L1)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I4,I3,I2,I5,I1)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder43512(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K4,L4,K3,L3,K5,L5,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K4+1:L4,K3+1:L3,K5+1:L5,K1+1:L1,K2+1:L2)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I4,I3,I5,I1,I2)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder43521(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K4,L4,K3,L3,K5,L5,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K4+1:L4,K3+1:L3,K5+1:L5,K2+1:L2,K1+1:L1)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I4,I3,I5,I2,I1)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder45123(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K4,L4,K5,L5,K1,L1,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K4+1:L4,K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I4,I5,I1,I2,I3)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder45132(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K4,L4,K5,L5,K1,L1,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K4+1:L4,K5+1:L5,K1+1:L1,K3+1:L3,K2+1:L2)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I4,I5,I1,I3,I2)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder45213(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K4,L4,K5,L5,K2,L2,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K4+1:L4,K5+1:L5,K2+1:L2,K1+1:L1,K3+1:L3)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I4,I5,I2,I1,I3)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder45231(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K4,L4,K5,L5,K2,L2,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K4+1:L4,K5+1:L5,K2+1:L2,K3+1:L3,K1+1:L1)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I4,I5,I2,I3,I1)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder45312(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K4,L4,K5,L5,K3,L3,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K4+1:L4,K5+1:L5,K3+1:L3,K1+1:L1,K2+1:L2)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I4,I5,I3,I1,I2)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder45321(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K4,L4,K5,L5,K3,L3,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K4+1:L4,K5+1:L5,K3+1:L3,K2+1:L2,K1+1:L1)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I4,I5,I3,I2,I1)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder51234(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K5,L5,K1,L1,K2,L2,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I5,I1,I2,I3,I4)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder51243(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K5,L5,K1,L1,K2,L2,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K5+1:L5,K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I5,I1,I2,I4,I3)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder51324(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K5,L5,K1,L1,K3,L3,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K5+1:L5,K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I5,I1,I3,I2,I4)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder51342(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K5,L5,K1,L1,K3,L3,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K5+1:L5,K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I5,I1,I3,I4,I2)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder51423(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K5,L5,K1,L1,K4,L4,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K5+1:L5,K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I5,I1,I4,I2,I3)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder51432(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K5,L5,K1,L1,K4,L4,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K5+1:L5,K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I5,I1,I4,I3,I2)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder52134(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K5,L5,K2,L2,K1,L1,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K5+1:L5,K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I5,I2,I1,I3,I4)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder52143(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K5,L5,K2,L2,K1,L1,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K5+1:L5,K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I5,I2,I1,I4,I3)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder52314(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K5,L5,K2,L2,K3,L3,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K5+1:L5,K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I5,I2,I3,I1,I4)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder52341(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K5,L5,K2,L2,K3,L3,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K5+1:L5,K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I5,I2,I3,I4,I1)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder52413(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K5,L5,K2,L2,K4,L4,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K5+1:L5,K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I5,I2,I4,I1,I3)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder52431(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K5,L5,K2,L2,K4,L4,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K5+1:L5,K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I5,I2,I4,I3,I1)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder53124(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K5,L5,K3,L3,K1,L1,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K5+1:L5,K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I5,I3,I1,I2,I4)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder53142(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K5,L5,K3,L3,K1,L1,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K5+1:L5,K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I5,I3,I1,I4,I2)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder53214(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K5,L5,K3,L3,K2,L2,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K5+1:L5,K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I5,I3,I2,I1,I4)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder53241(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K5,L5,K3,L3,K2,L2,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K5+1:L5,K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I5,I3,I2,I4,I1)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder53412(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K5,L5,K3,L3,K4,L4,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K5+1:L5,K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I5,I3,I4,I1,I2)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder53421(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K5,L5,K3,L3,K4,L4,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K5+1:L5,K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I5,I3,I4,I2,I1)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder54123(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K5,L5,K4,L4,K1,L1,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K5+1:L5,K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I5,I4,I1,I2,I3)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder54132(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K5,L5,K4,L4,K1,L1,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K5+1:L5,K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I5,I4,I1,I3,I2)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder54213(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K5,L5,K4,L4,K2,L2,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K5+1:L5,K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I5,I4,I2,I1,I3)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder54231(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K5,L5,K4,L4,K2,L2,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K5+1:L5,K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I5,I4,I2,I3,I1)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder54312(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K5,L5,K4,L4,K3,L3,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K5+1:L5,K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I5,I4,I3,I1,I2)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder54321(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,
     & K5,L5,K4,L4,K3,L3,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5)
       real*8 B(K5+1:L5,K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1)
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        B(I5,I4,I3,I2,I1)=A(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
