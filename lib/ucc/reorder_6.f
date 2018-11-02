       subroutine eorder463125(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K3,L3,K1,L1,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K3+1:L3,K1+1:L1,K2+1:L2,K5+1:L5)
       real*8 C
C
	print*,'reorder'
	print*,M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6
	print*,K4,L4,K6,L6,K3,L3,K1,L1,K2,L2,K5,L5
	print*,A
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I3,I1,I2,I5)=A(I1,I2,I3,I4,I5,I6)
	print*,I1,I2,I3,I4,I5,I6
	print*,A(I1,I2,I3,I4,I5,I6),B(I4,I6,I3,I1,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
	print*,B
C
!$OMP END PARALLEL DO
       end

       subroutine VtoHR123456(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=M1+1,N1
       do I2=M2+1,N2
       do I3=M3+1,N3
       do I4=M4+1,N4
       do I5=M5+1,N5
       do I6=M6+1,N6
        B(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder123456(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder123465(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K3,L3,K4,L4,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K6+1:L6,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I3,I4,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder123546(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K3,L3,K5,L5,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K5+1:L5,K4+1:L4,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I3,I5,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder123564(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K3,L3,K5,L5,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K5+1:L5,K6+1:L6,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I3,I5,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder123645(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K3,L3,K6,L6,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K6+1:L6,K4+1:L4,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I3,I6,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder123654(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K3,L3,K6,L6,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K6+1:L6,K5+1:L5,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I3,I6,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder124356(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K4,L4,K3,L3,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3,K5+1:L5,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I4,I3,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder124365(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K4,L4,K3,L3,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3,K6+1:L6,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I4,I3,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder124536(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K4,L4,K5,L5,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K5+1:L5,K3+1:L3,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I4,I5,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder124563(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K4,L4,K5,L5,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K5+1:L5,K6+1:L6,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I4,I5,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder124635(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K4,L4,K6,L6,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K6+1:L6,K3+1:L3,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I4,I6,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder124653(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K4,L4,K6,L6,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K6+1:L6,K5+1:L5,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I4,I6,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder125346(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K5,L5,K3,L3,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K5+1:L5,K3+1:L3,K4+1:L4,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I5,I3,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder125364(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K5,L5,K3,L3,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K5+1:L5,K3+1:L3,K6+1:L6,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I5,I3,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder125436(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K5,L5,K4,L4,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K5+1:L5,K4+1:L4,K3+1:L3,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I5,I4,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder125463(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K5,L5,K4,L4,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K5+1:L5,K4+1:L4,K6+1:L6,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I5,I4,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder125634(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K5,L5,K6,L6,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K5+1:L5,K6+1:L6,K3+1:L3,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I5,I6,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder125643(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K5,L5,K6,L6,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K5+1:L5,K6+1:L6,K4+1:L4,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I5,I6,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder126345(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K6,L6,K3,L3,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K6+1:L6,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I6,I3,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder126354(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K6,L6,K3,L3,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K6+1:L6,K3+1:L3,K5+1:L5,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I6,I3,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder126435(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K6,L6,K4,L4,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K6+1:L6,K4+1:L4,K3+1:L3,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I6,I4,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder126453(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K6,L6,K4,L4,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K6+1:L6,K4+1:L4,K5+1:L5,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I6,I4,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder126534(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K6,L6,K5,L5,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K6+1:L6,K5+1:L5,K3+1:L3,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I6,I5,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder126543(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K6,L6,K5,L5,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K6+1:L6,K5+1:L5,K4+1:L4,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I6,I5,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder132456(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K2,L2,K4,L4,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I2,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder132465(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K2,L2,K4,L4,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4,K6+1:L6,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I2,I4,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder132546(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K2,L2,K5,L5,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K5+1:L5,K4+1:L4,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I2,I5,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder132564(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K2,L2,K5,L5,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K5+1:L5,K6+1:L6,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I2,I5,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder132645(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K2,L2,K6,L6,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K6+1:L6,K4+1:L4,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I2,I6,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder132654(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K2,L2,K6,L6,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K6+1:L6,K5+1:L5,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I2,I6,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder134256(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K4,L4,K2,L2,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2,K5+1:L5,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I4,I2,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder134265(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K4,L4,K2,L2,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2,K6+1:L6,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I4,I2,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder134526(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K4,L4,K5,L5,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K5+1:L5,K2+1:L2,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I4,I5,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder134562(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K4,L4,K5,L5,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I4,I5,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder134625(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K4,L4,K6,L6,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K6+1:L6,K2+1:L2,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I4,I6,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder134652(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K4,L4,K6,L6,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K6+1:L6,K5+1:L5,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I4,I6,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder135246(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K5,L5,K2,L2,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K5+1:L5,K2+1:L2,K4+1:L4,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I5,I2,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder135264(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K5,L5,K2,L2,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K5+1:L5,K2+1:L2,K6+1:L6,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I5,I2,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder135426(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K5,L5,K4,L4,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K5+1:L5,K4+1:L4,K2+1:L2,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I5,I4,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder135462(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K5,L5,K4,L4,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K5+1:L5,K4+1:L4,K6+1:L6,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I5,I4,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder135624(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K5,L5,K6,L6,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K5+1:L5,K6+1:L6,K2+1:L2,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I5,I6,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder135642(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K5,L5,K6,L6,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K5+1:L5,K6+1:L6,K4+1:L4,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I5,I6,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder136245(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K6,L6,K2,L2,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K6+1:L6,K2+1:L2,K4+1:L4,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I6,I2,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder136254(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K6,L6,K2,L2,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K6+1:L6,K2+1:L2,K5+1:L5,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I6,I2,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder136425(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K6,L6,K4,L4,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K6+1:L6,K4+1:L4,K2+1:L2,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I6,I4,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder136452(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K6,L6,K4,L4,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K6+1:L6,K4+1:L4,K5+1:L5,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I6,I4,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder136524(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K6,L6,K5,L5,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K6+1:L6,K5+1:L5,K2+1:L2,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I6,I5,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder136542(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K6,L6,K5,L5,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K6+1:L6,K5+1:L5,K4+1:L4,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I6,I5,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder142356(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K2,L2,K3,L3,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3,K5+1:L5,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I2,I3,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder142365(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K2,L2,K3,L3,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3,K6+1:L6,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I2,I3,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder142536(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K2,L2,K5,L5,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K5+1:L5,K3+1:L3,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I2,I5,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder142563(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K2,L2,K5,L5,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K5+1:L5,K6+1:L6,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I2,I5,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder142635(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K2,L2,K6,L6,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K6+1:L6,K3+1:L3,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I2,I6,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder142653(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K2,L2,K6,L6,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K6+1:L6,K5+1:L5,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I2,I6,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder143256(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K3,L3,K2,L2,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2,K5+1:L5,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I3,I2,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder143265(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K3,L3,K2,L2,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2,K6+1:L6,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I3,I2,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder143526(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K3,L3,K5,L5,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K5+1:L5,K2+1:L2,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I3,I5,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder143562(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K3,L3,K5,L5,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K5+1:L5,K6+1:L6,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I3,I5,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder143625(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K3,L3,K6,L6,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K6+1:L6,K2+1:L2,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I3,I6,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder143652(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K3,L3,K6,L6,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K6+1:L6,K5+1:L5,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I3,I6,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder145236(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K5,L5,K2,L2,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K5+1:L5,K2+1:L2,K3+1:L3,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I5,I2,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder145263(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K5,L5,K2,L2,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K5+1:L5,K2+1:L2,K6+1:L6,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I5,I2,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder145326(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K5,L5,K3,L3,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K5+1:L5,K3+1:L3,K2+1:L2,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I5,I3,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder145362(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K5,L5,K3,L3,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K5+1:L5,K3+1:L3,K6+1:L6,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I5,I3,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder145623(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K5,L5,K6,L6,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K5+1:L5,K6+1:L6,K2+1:L2,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I5,I6,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder145632(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K5,L5,K6,L6,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K5+1:L5,K6+1:L6,K3+1:L3,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I5,I6,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder146235(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K6,L6,K2,L2,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K6+1:L6,K2+1:L2,K3+1:L3,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I6,I2,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder146253(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K6,L6,K2,L2,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K6+1:L6,K2+1:L2,K5+1:L5,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I6,I2,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder146325(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K6,L6,K3,L3,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K6+1:L6,K3+1:L3,K2+1:L2,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I6,I3,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder146352(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K6,L6,K3,L3,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K6+1:L6,K3+1:L3,K5+1:L5,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I6,I3,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder146523(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K6,L6,K5,L5,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K6+1:L6,K5+1:L5,K2+1:L2,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I6,I5,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder146532(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K6,L6,K5,L5,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K6+1:L6,K5+1:L5,K3+1:L3,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I6,I5,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder152346(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K2,L2,K3,L3,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K2+1:L2,K3+1:L3,K4+1:L4,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I2,I3,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder152364(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K2,L2,K3,L3,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K2+1:L2,K3+1:L3,K6+1:L6,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I2,I3,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder152436(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K2,L2,K4,L4,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K2+1:L2,K4+1:L4,K3+1:L3,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I2,I4,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder152463(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K2,L2,K4,L4,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K2+1:L2,K4+1:L4,K6+1:L6,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I2,I4,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder152634(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K2,L2,K6,L6,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K2+1:L2,K6+1:L6,K3+1:L3,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I2,I6,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder152643(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K2,L2,K6,L6,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K2+1:L2,K6+1:L6,K4+1:L4,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I2,I6,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder153246(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K3,L3,K2,L2,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K3+1:L3,K2+1:L2,K4+1:L4,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I3,I2,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder153264(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K3,L3,K2,L2,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K3+1:L3,K2+1:L2,K6+1:L6,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I3,I2,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder153426(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K3,L3,K4,L4,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K3+1:L3,K4+1:L4,K2+1:L2,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I3,I4,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder153462(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K3,L3,K4,L4,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K3+1:L3,K4+1:L4,K6+1:L6,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I3,I4,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder153624(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K3,L3,K6,L6,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K3+1:L3,K6+1:L6,K2+1:L2,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I3,I6,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder153642(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K3,L3,K6,L6,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K3+1:L3,K6+1:L6,K4+1:L4,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I3,I6,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder154236(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K4,L4,K2,L2,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K4+1:L4,K2+1:L2,K3+1:L3,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I4,I2,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder154263(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K4,L4,K2,L2,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K4+1:L4,K2+1:L2,K6+1:L6,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I4,I2,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder154326(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K4,L4,K3,L3,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K4+1:L4,K3+1:L3,K2+1:L2,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I4,I3,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder154362(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K4,L4,K3,L3,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K4+1:L4,K3+1:L3,K6+1:L6,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I4,I3,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder154623(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K4,L4,K6,L6,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K4+1:L4,K6+1:L6,K2+1:L2,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I4,I6,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder154632(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K4,L4,K6,L6,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K4+1:L4,K6+1:L6,K3+1:L3,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I4,I6,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder156234(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K6,L6,K2,L2,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K6+1:L6,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I6,I2,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder156243(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K6,L6,K2,L2,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K6+1:L6,K2+1:L2,K4+1:L4,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I6,I2,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder156324(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K6,L6,K3,L3,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K6+1:L6,K3+1:L3,K2+1:L2,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I6,I3,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder156342(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K6,L6,K3,L3,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K6+1:L6,K3+1:L3,K4+1:L4,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I6,I3,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder156423(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K6,L6,K4,L4,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K6+1:L6,K4+1:L4,K2+1:L2,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I6,I4,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder156432(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K6,L6,K4,L4,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K6+1:L6,K4+1:L4,K3+1:L3,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I6,I4,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder162345(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K2,L2,K3,L3,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder162354(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K2,L2,K3,L3,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K2+1:L2,K3+1:L3,K5+1:L5,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I2,I3,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder162435(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K2,L2,K4,L4,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K2+1:L2,K4+1:L4,K3+1:L3,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I2,I4,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder162453(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K2,L2,K4,L4,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K2+1:L2,K4+1:L4,K5+1:L5,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I2,I4,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder162534(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K2,L2,K5,L5,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K2+1:L2,K5+1:L5,K3+1:L3,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I2,I5,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder162543(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K2,L2,K5,L5,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K2+1:L2,K5+1:L5,K4+1:L4,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I2,I5,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder163245(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K3,L3,K2,L2,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K3+1:L3,K2+1:L2,K4+1:L4,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I3,I2,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder163254(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K3,L3,K2,L2,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K3+1:L3,K2+1:L2,K5+1:L5,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I3,I2,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder163425(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K3,L3,K4,L4,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K3+1:L3,K4+1:L4,K2+1:L2,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I3,I4,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder163452(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K3,L3,K4,L4,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K3+1:L3,K4+1:L4,K5+1:L5,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I3,I4,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder163524(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K3,L3,K5,L5,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K3+1:L3,K5+1:L5,K2+1:L2,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I3,I5,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder163542(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K3,L3,K5,L5,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K3+1:L3,K5+1:L5,K4+1:L4,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I3,I5,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder164235(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K4,L4,K2,L2,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K4+1:L4,K2+1:L2,K3+1:L3,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I4,I2,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder164253(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K4,L4,K2,L2,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K4+1:L4,K2+1:L2,K5+1:L5,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I4,I2,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder164325(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K4,L4,K3,L3,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K4+1:L4,K3+1:L3,K2+1:L2,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I4,I3,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder164352(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K4,L4,K3,L3,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K4+1:L4,K3+1:L3,K5+1:L5,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I4,I3,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder164523(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K4,L4,K5,L5,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K4+1:L4,K5+1:L5,K2+1:L2,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I4,I5,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder164532(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K4,L4,K5,L5,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K4+1:L4,K5+1:L5,K3+1:L3,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I4,I5,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder165234(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K5,L5,K2,L2,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K5+1:L5,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I5,I2,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder165243(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K5,L5,K2,L2,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K5+1:L5,K2+1:L2,K4+1:L4,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I5,I2,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder165324(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K5,L5,K3,L3,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K5+1:L5,K3+1:L3,K2+1:L2,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I5,I3,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder165342(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K5,L5,K3,L3,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K5+1:L5,K3+1:L3,K4+1:L4,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I5,I3,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder165423(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K5,L5,K4,L4,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K5+1:L5,K4+1:L4,K2+1:L2,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I5,I4,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder165432(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K5,L5,K4,L4,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K5+1:L5,K4+1:L4,K3+1:L3,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I5,I4,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder213456(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K3,L3,K4,L4,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder213465(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K3,L3,K4,L4,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4,K6+1:L6,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I3,I4,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder213546(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K3,L3,K5,L5,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K5+1:L5,K4+1:L4,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I3,I5,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder213564(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K3,L3,K5,L5,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K5+1:L5,K6+1:L6,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I3,I5,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder213645(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K3,L3,K6,L6,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K6+1:L6,K4+1:L4,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I3,I6,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder213654(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K3,L3,K6,L6,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K6+1:L6,K5+1:L5,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I3,I6,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder214356(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K4,L4,K3,L3,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3,K5+1:L5,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I4,I3,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder214365(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K4,L4,K3,L3,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3,K6+1:L6,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I4,I3,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder214536(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K4,L4,K5,L5,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K5+1:L5,K3+1:L3,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I4,I5,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder214563(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K4,L4,K5,L5,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K5+1:L5,K6+1:L6,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I4,I5,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder214635(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K4,L4,K6,L6,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K6+1:L6,K3+1:L3,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I4,I6,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder214653(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K4,L4,K6,L6,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K6+1:L6,K5+1:L5,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I4,I6,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder215346(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K5,L5,K3,L3,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K5+1:L5,K3+1:L3,K4+1:L4,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I5,I3,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder215364(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K5,L5,K3,L3,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K5+1:L5,K3+1:L3,K6+1:L6,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I5,I3,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder215436(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K5,L5,K4,L4,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K5+1:L5,K4+1:L4,K3+1:L3,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I5,I4,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder215463(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K5,L5,K4,L4,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K5+1:L5,K4+1:L4,K6+1:L6,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I5,I4,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder215634(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K5,L5,K6,L6,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K5+1:L5,K6+1:L6,K3+1:L3,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I5,I6,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder215643(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K5,L5,K6,L6,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K5+1:L5,K6+1:L6,K4+1:L4,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I5,I6,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder216345(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K6,L6,K3,L3,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K6+1:L6,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I6,I3,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder216354(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K6,L6,K3,L3,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K6+1:L6,K3+1:L3,K5+1:L5,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I6,I3,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder216435(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K6,L6,K4,L4,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K6+1:L6,K4+1:L4,K3+1:L3,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I6,I4,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder216453(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K6,L6,K4,L4,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K6+1:L6,K4+1:L4,K5+1:L5,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I6,I4,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder216534(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K6,L6,K5,L5,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K6+1:L6,K5+1:L5,K3+1:L3,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I6,I5,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder216543(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K6,L6,K5,L5,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K6+1:L6,K5+1:L5,K4+1:L4,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I6,I5,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder231456(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K1,L1,K4,L4,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I1,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder231465(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K1,L1,K4,L4,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4,K6+1:L6,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I1,I4,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder231546(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K1,L1,K5,L5,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K5+1:L5,K4+1:L4,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I1,I5,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder231564(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K1,L1,K5,L5,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K5+1:L5,K6+1:L6,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I1,I5,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder231645(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K1,L1,K6,L6,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K6+1:L6,K4+1:L4,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I1,I6,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder231654(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K1,L1,K6,L6,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K6+1:L6,K5+1:L5,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I1,I6,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder234156(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K4,L4,K1,L1,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1,K5+1:L5,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I4,I1,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder234165(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K4,L4,K1,L1,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1,K6+1:L6,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I4,I1,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder234516(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K4,L4,K5,L5,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K1+1:L1,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I4,I5,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder234561(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I4,I5,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder234615(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K4,L4,K6,L6,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K6+1:L6,K1+1:L1,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I4,I6,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder234651(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K4,L4,K6,L6,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K6+1:L6,K5+1:L5,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I4,I6,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder235146(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K5,L5,K1,L1,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K5+1:L5,K1+1:L1,K4+1:L4,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I5,I1,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder235164(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K5,L5,K1,L1,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K5+1:L5,K1+1:L1,K6+1:L6,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I5,I1,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder235416(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K5,L5,K4,L4,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K5+1:L5,K4+1:L4,K1+1:L1,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I5,I4,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder235461(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K5,L5,K4,L4,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K5+1:L5,K4+1:L4,K6+1:L6,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I5,I4,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder235614(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K5,L5,K6,L6,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K5+1:L5,K6+1:L6,K1+1:L1,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I5,I6,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder235641(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K5,L5,K6,L6,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K5+1:L5,K6+1:L6,K4+1:L4,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I5,I6,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder236145(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K6,L6,K1,L1,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K6+1:L6,K1+1:L1,K4+1:L4,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I6,I1,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder236154(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K6,L6,K1,L1,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K6+1:L6,K1+1:L1,K5+1:L5,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I6,I1,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder236415(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K6,L6,K4,L4,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K6+1:L6,K4+1:L4,K1+1:L1,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I6,I4,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder236451(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K6,L6,K4,L4,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K6+1:L6,K4+1:L4,K5+1:L5,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I6,I4,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder236514(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K6,L6,K5,L5,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K6+1:L6,K5+1:L5,K1+1:L1,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I6,I5,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder236541(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K6,L6,K5,L5,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K6+1:L6,K5+1:L5,K4+1:L4,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I6,I5,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder241356(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K1,L1,K3,L3,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3,K5+1:L5,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I1,I3,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder241365(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K1,L1,K3,L3,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3,K6+1:L6,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I1,I3,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder241536(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K1,L1,K5,L5,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K5+1:L5,K3+1:L3,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I1,I5,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder241563(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K1,L1,K5,L5,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K5+1:L5,K6+1:L6,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I1,I5,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder241635(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K1,L1,K6,L6,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K6+1:L6,K3+1:L3,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I1,I6,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder241653(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K1,L1,K6,L6,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K6+1:L6,K5+1:L5,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I1,I6,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder243156(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K3,L3,K1,L1,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1,K5+1:L5,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I3,I1,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder243165(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K3,L3,K1,L1,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1,K6+1:L6,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I3,I1,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder243516(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K3,L3,K5,L5,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K5+1:L5,K1+1:L1,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I3,I5,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder243561(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K3,L3,K5,L5,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K5+1:L5,K6+1:L6,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I3,I5,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder243615(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K3,L3,K6,L6,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K6+1:L6,K1+1:L1,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I3,I6,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder243651(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K3,L3,K6,L6,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K6+1:L6,K5+1:L5,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I3,I6,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder245136(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K5,L5,K1,L1,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K5+1:L5,K1+1:L1,K3+1:L3,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I5,I1,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder245163(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K5,L5,K1,L1,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K5+1:L5,K1+1:L1,K6+1:L6,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I5,I1,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder245316(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K5,L5,K3,L3,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K5+1:L5,K3+1:L3,K1+1:L1,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I5,I3,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder245361(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K5,L5,K3,L3,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K5+1:L5,K3+1:L3,K6+1:L6,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I5,I3,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder245613(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K5,L5,K6,L6,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K5+1:L5,K6+1:L6,K1+1:L1,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I5,I6,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder245631(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K5,L5,K6,L6,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K5+1:L5,K6+1:L6,K3+1:L3,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I5,I6,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder246135(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K6,L6,K1,L1,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K6+1:L6,K1+1:L1,K3+1:L3,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I6,I1,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder246153(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K6,L6,K1,L1,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K6+1:L6,K1+1:L1,K5+1:L5,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I6,I1,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder246315(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K6,L6,K3,L3,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K6+1:L6,K3+1:L3,K1+1:L1,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I6,I3,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder246351(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K6,L6,K3,L3,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K6+1:L6,K3+1:L3,K5+1:L5,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I6,I3,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder246513(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K6,L6,K5,L5,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K6+1:L6,K5+1:L5,K1+1:L1,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I6,I5,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder246531(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K6,L6,K5,L5,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K6+1:L6,K5+1:L5,K3+1:L3,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I6,I5,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder251346(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K1,L1,K3,L3,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K1+1:L1,K3+1:L3,K4+1:L4,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I1,I3,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder251364(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K1,L1,K3,L3,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K1+1:L1,K3+1:L3,K6+1:L6,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I1,I3,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder251436(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K1,L1,K4,L4,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K1+1:L1,K4+1:L4,K3+1:L3,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I1,I4,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder251463(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K1,L1,K4,L4,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K1+1:L1,K4+1:L4,K6+1:L6,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I1,I4,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder251634(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K1,L1,K6,L6,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K1+1:L1,K6+1:L6,K3+1:L3,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I1,I6,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder251643(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K1,L1,K6,L6,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K1+1:L1,K6+1:L6,K4+1:L4,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I1,I6,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder253146(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K3,L3,K1,L1,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K3+1:L3,K1+1:L1,K4+1:L4,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I3,I1,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder253164(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K3,L3,K1,L1,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K3+1:L3,K1+1:L1,K6+1:L6,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I3,I1,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder253416(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K3,L3,K4,L4,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K3+1:L3,K4+1:L4,K1+1:L1,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I3,I4,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder253461(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K3,L3,K4,L4,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K3+1:L3,K4+1:L4,K6+1:L6,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I3,I4,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder253614(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K3,L3,K6,L6,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K3+1:L3,K6+1:L6,K1+1:L1,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I3,I6,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder253641(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K3,L3,K6,L6,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K3+1:L3,K6+1:L6,K4+1:L4,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I3,I6,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder254136(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K4,L4,K1,L1,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K4+1:L4,K1+1:L1,K3+1:L3,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I4,I1,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder254163(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K4,L4,K1,L1,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K4+1:L4,K1+1:L1,K6+1:L6,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I4,I1,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder254316(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K4,L4,K3,L3,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K4+1:L4,K3+1:L3,K1+1:L1,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I4,I3,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder254361(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K4,L4,K3,L3,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K4+1:L4,K3+1:L3,K6+1:L6,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I4,I3,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder254613(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K4,L4,K6,L6,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K4+1:L4,K6+1:L6,K1+1:L1,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I4,I6,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder254631(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K4,L4,K6,L6,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K4+1:L4,K6+1:L6,K3+1:L3,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I4,I6,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder256134(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K6,L6,K1,L1,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K6+1:L6,K1+1:L1,K3+1:L3,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I6,I1,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder256143(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K6,L6,K1,L1,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K6+1:L6,K1+1:L1,K4+1:L4,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I6,I1,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder256314(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K6,L6,K3,L3,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K6+1:L6,K3+1:L3,K1+1:L1,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I6,I3,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder256341(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K6,L6,K3,L3,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K6+1:L6,K3+1:L3,K4+1:L4,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I6,I3,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder256413(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K6,L6,K4,L4,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K6+1:L6,K4+1:L4,K1+1:L1,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I6,I4,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder256431(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K6,L6,K4,L4,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K6+1:L6,K4+1:L4,K3+1:L3,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I6,I4,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder261345(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K1,L1,K3,L3,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K1+1:L1,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I1,I3,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder261354(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K1,L1,K3,L3,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K1+1:L1,K3+1:L3,K5+1:L5,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I1,I3,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder261435(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K1,L1,K4,L4,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K1+1:L1,K4+1:L4,K3+1:L3,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I1,I4,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder261453(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K1,L1,K4,L4,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K1+1:L1,K4+1:L4,K5+1:L5,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I1,I4,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder261534(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K1,L1,K5,L5,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K1+1:L1,K5+1:L5,K3+1:L3,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I1,I5,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder261543(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K1,L1,K5,L5,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K1+1:L1,K5+1:L5,K4+1:L4,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I1,I5,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder263145(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K3,L3,K1,L1,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I3,I1,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder263154(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K3,L3,K1,L1,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K3+1:L3,K1+1:L1,K5+1:L5,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I3,I1,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder263415(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K3,L3,K4,L4,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K3+1:L3,K4+1:L4,K1+1:L1,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I3,I4,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder263451(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K3,L3,K4,L4,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K3+1:L3,K4+1:L4,K5+1:L5,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I3,I4,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder263514(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K3,L3,K5,L5,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K3+1:L3,K5+1:L5,K1+1:L1,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I3,I5,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder263541(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K3,L3,K5,L5,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K3+1:L3,K5+1:L5,K4+1:L4,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I3,I5,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder264135(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K4,L4,K1,L1,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K4+1:L4,K1+1:L1,K3+1:L3,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I4,I1,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder264153(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K4,L4,K1,L1,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K4+1:L4,K1+1:L1,K5+1:L5,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I4,I1,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder264315(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K4,L4,K3,L3,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K4+1:L4,K3+1:L3,K1+1:L1,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I4,I3,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder264351(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K4,L4,K3,L3,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K4+1:L4,K3+1:L3,K5+1:L5,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I4,I3,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder264513(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K4,L4,K5,L5,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K4+1:L4,K5+1:L5,K1+1:L1,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I4,I5,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder264531(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K4,L4,K5,L5,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K4+1:L4,K5+1:L5,K3+1:L3,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I4,I5,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder265134(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K5,L5,K1,L1,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K5+1:L5,K1+1:L1,K3+1:L3,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I5,I1,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder265143(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K5,L5,K1,L1,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K5+1:L5,K1+1:L1,K4+1:L4,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I5,I1,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder265314(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K5,L5,K3,L3,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K5+1:L5,K3+1:L3,K1+1:L1,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I5,I3,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder265341(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K5,L5,K3,L3,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K5+1:L5,K3+1:L3,K4+1:L4,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I5,I3,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder265413(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K5,L5,K4,L4,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K5+1:L5,K4+1:L4,K1+1:L1,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I5,I4,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder265431(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K5,L5,K4,L4,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K5+1:L5,K4+1:L4,K3+1:L3,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I5,I4,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder312456(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K2,L2,K4,L4,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I2,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder312465(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K2,L2,K4,L4,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4,K6+1:L6,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I2,I4,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder312546(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K2,L2,K5,L5,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K5+1:L5,K4+1:L4,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I2,I5,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder312564(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K2,L2,K5,L5,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K5+1:L5,K6+1:L6,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I2,I5,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder312645(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K2,L2,K6,L6,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K6+1:L6,K4+1:L4,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I2,I6,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder312654(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K2,L2,K6,L6,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K6+1:L6,K5+1:L5,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I2,I6,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder314256(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K4,L4,K2,L2,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2,K5+1:L5,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I4,I2,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder314265(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K4,L4,K2,L2,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2,K6+1:L6,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I4,I2,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder314526(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K4,L4,K5,L5,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5,K2+1:L2,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I4,I5,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder314562(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K4,L4,K5,L5,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5,K6+1:L6,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I4,I5,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder314625(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K4,L4,K6,L6,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K6+1:L6,K2+1:L2,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I4,I6,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder314652(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K4,L4,K6,L6,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K6+1:L6,K5+1:L5,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I4,I6,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder315246(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K5,L5,K2,L2,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K5+1:L5,K2+1:L2,K4+1:L4,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I5,I2,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder315264(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K5,L5,K2,L2,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K5+1:L5,K2+1:L2,K6+1:L6,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I5,I2,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder315426(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K5,L5,K4,L4,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K5+1:L5,K4+1:L4,K2+1:L2,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I5,I4,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder315462(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K5,L5,K4,L4,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K5+1:L5,K4+1:L4,K6+1:L6,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I5,I4,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder315624(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K5,L5,K6,L6,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K5+1:L5,K6+1:L6,K2+1:L2,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I5,I6,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder315642(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K5,L5,K6,L6,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K5+1:L5,K6+1:L6,K4+1:L4,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I5,I6,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder316245(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K6,L6,K2,L2,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K6+1:L6,K2+1:L2,K4+1:L4,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I6,I2,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder316254(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K6,L6,K2,L2,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K6+1:L6,K2+1:L2,K5+1:L5,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I6,I2,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder316425(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K6,L6,K4,L4,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K6+1:L6,K4+1:L4,K2+1:L2,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I6,I4,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder316452(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K6,L6,K4,L4,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K6+1:L6,K4+1:L4,K5+1:L5,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I6,I4,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder316524(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K6,L6,K5,L5,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K6+1:L6,K5+1:L5,K2+1:L2,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I6,I5,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder316542(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K6,L6,K5,L5,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K6+1:L6,K5+1:L5,K4+1:L4,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I6,I5,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder321456(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K1,L1,K4,L4,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I1,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder321465(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K1,L1,K4,L4,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4,K6+1:L6,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I1,I4,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder321546(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K1,L1,K5,L5,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K5+1:L5,K4+1:L4,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I1,I5,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder321564(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K1,L1,K5,L5,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K5+1:L5,K6+1:L6,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I1,I5,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder321645(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K1,L1,K6,L6,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K6+1:L6,K4+1:L4,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I1,I6,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder321654(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K1,L1,K6,L6,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K6+1:L6,K5+1:L5,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I1,I6,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder324156(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K4,L4,K1,L1,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1,K5+1:L5,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I4,I1,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder324165(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K4,L4,K1,L1,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1,K6+1:L6,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I4,I1,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder324516(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K4,L4,K5,L5,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K5+1:L5,K1+1:L1,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I4,I5,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder324561(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K4,L4,K5,L5,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K5+1:L5,K6+1:L6,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I4,I5,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder324615(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K4,L4,K6,L6,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K6+1:L6,K1+1:L1,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I4,I6,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder324651(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K4,L4,K6,L6,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K6+1:L6,K5+1:L5,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I4,I6,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder325146(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K5,L5,K1,L1,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K5+1:L5,K1+1:L1,K4+1:L4,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I5,I1,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder325164(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K5,L5,K1,L1,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K5+1:L5,K1+1:L1,K6+1:L6,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I5,I1,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder325416(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K5,L5,K4,L4,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K5+1:L5,K4+1:L4,K1+1:L1,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I5,I4,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder325461(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K5,L5,K4,L4,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K5+1:L5,K4+1:L4,K6+1:L6,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I5,I4,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder325614(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K5,L5,K6,L6,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K5+1:L5,K6+1:L6,K1+1:L1,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I5,I6,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder325641(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K5,L5,K6,L6,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K5+1:L5,K6+1:L6,K4+1:L4,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I5,I6,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder326145(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K6,L6,K1,L1,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K6+1:L6,K1+1:L1,K4+1:L4,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I6,I1,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder326154(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K6,L6,K1,L1,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K6+1:L6,K1+1:L1,K5+1:L5,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I6,I1,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder326415(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K6,L6,K4,L4,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K6+1:L6,K4+1:L4,K1+1:L1,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I6,I4,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder326451(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K6,L6,K4,L4,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K6+1:L6,K4+1:L4,K5+1:L5,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I6,I4,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder326514(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K6,L6,K5,L5,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K6+1:L6,K5+1:L5,K1+1:L1,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I6,I5,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder326541(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K6,L6,K5,L5,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K6+1:L6,K5+1:L5,K4+1:L4,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I6,I5,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder341256(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K1,L1,K2,L2,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2,K5+1:L5,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I1,I2,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder341265(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K1,L1,K2,L2,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2,K6+1:L6,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I1,I2,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder341526(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K1,L1,K5,L5,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K5+1:L5,K2+1:L2,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I1,I5,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder341562(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K1,L1,K5,L5,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K5+1:L5,K6+1:L6,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I1,I5,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder341625(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K1,L1,K6,L6,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K6+1:L6,K2+1:L2,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I1,I6,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder341652(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K1,L1,K6,L6,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K6+1:L6,K5+1:L5,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I1,I6,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder342156(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K2,L2,K1,L1,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1,K5+1:L5,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I2,I1,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder342165(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K2,L2,K1,L1,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1,K6+1:L6,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I2,I1,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder342516(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K2,L2,K5,L5,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K5+1:L5,K1+1:L1,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I2,I5,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder342561(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K2,L2,K5,L5,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K5+1:L5,K6+1:L6,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I2,I5,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder342615(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K2,L2,K6,L6,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K6+1:L6,K1+1:L1,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I2,I6,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder342651(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K2,L2,K6,L6,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K6+1:L6,K5+1:L5,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I2,I6,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder345126(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K5,L5,K1,L1,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K5+1:L5,K1+1:L1,K2+1:L2,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I5,I1,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder345162(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K5,L5,K1,L1,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K5+1:L5,K1+1:L1,K6+1:L6,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I5,I1,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder345216(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K5,L5,K2,L2,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K5+1:L5,K2+1:L2,K1+1:L1,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I5,I2,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder345261(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K5,L5,K2,L2,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K5+1:L5,K2+1:L2,K6+1:L6,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I5,I2,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder345612(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K5,L5,K6,L6,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6,K1+1:L1,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I5,I6,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder345621(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K5,L5,K6,L6,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6,K2+1:L2,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I5,I6,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder346125(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K6,L6,K1,L1,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K6+1:L6,K1+1:L1,K2+1:L2,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I6,I1,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder346152(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K6,L6,K1,L1,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K6+1:L6,K1+1:L1,K5+1:L5,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I6,I1,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder346215(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K6,L6,K2,L2,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K6+1:L6,K2+1:L2,K1+1:L1,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I6,I2,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder346251(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K6,L6,K2,L2,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K6+1:L6,K2+1:L2,K5+1:L5,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I6,I2,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder346512(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K6,L6,K5,L5,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K6+1:L6,K5+1:L5,K1+1:L1,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I6,I5,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder346521(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K6,L6,K5,L5,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K6+1:L6,K5+1:L5,K2+1:L2,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I6,I5,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder351246(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K1,L1,K2,L2,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K1+1:L1,K2+1:L2,K4+1:L4,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I1,I2,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder351264(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K1,L1,K2,L2,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K1+1:L1,K2+1:L2,K6+1:L6,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I1,I2,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder351426(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K1,L1,K4,L4,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K1+1:L1,K4+1:L4,K2+1:L2,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I1,I4,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder351462(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K1,L1,K4,L4,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K1+1:L1,K4+1:L4,K6+1:L6,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I1,I4,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder351624(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K1,L1,K6,L6,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K1+1:L1,K6+1:L6,K2+1:L2,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I1,I6,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder351642(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K1,L1,K6,L6,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K1+1:L1,K6+1:L6,K4+1:L4,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I1,I6,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder352146(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K2,L2,K1,L1,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K2+1:L2,K1+1:L1,K4+1:L4,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I2,I1,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder352164(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K2,L2,K1,L1,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K2+1:L2,K1+1:L1,K6+1:L6,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I2,I1,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder352416(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K2,L2,K4,L4,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K2+1:L2,K4+1:L4,K1+1:L1,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I2,I4,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder352461(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K2,L2,K4,L4,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K2+1:L2,K4+1:L4,K6+1:L6,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I2,I4,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder352614(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K2,L2,K6,L6,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K2+1:L2,K6+1:L6,K1+1:L1,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I2,I6,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder352641(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K2,L2,K6,L6,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K2+1:L2,K6+1:L6,K4+1:L4,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I2,I6,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder354126(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K4,L4,K1,L1,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K4+1:L4,K1+1:L1,K2+1:L2,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I4,I1,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder354162(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K4,L4,K1,L1,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K4+1:L4,K1+1:L1,K6+1:L6,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I4,I1,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder354216(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K4,L4,K2,L2,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K4+1:L4,K2+1:L2,K1+1:L1,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I4,I2,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder354261(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K4,L4,K2,L2,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K4+1:L4,K2+1:L2,K6+1:L6,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I4,I2,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder354612(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K4,L4,K6,L6,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K4+1:L4,K6+1:L6,K1+1:L1,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I4,I6,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder354621(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K4,L4,K6,L6,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K4+1:L4,K6+1:L6,K2+1:L2,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I4,I6,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder356124(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K6,L6,K1,L1,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K6+1:L6,K1+1:L1,K2+1:L2,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I6,I1,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder356142(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K6,L6,K1,L1,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K6+1:L6,K1+1:L1,K4+1:L4,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I6,I1,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder356214(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K6,L6,K2,L2,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K6+1:L6,K2+1:L2,K1+1:L1,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I6,I2,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder356241(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K6,L6,K2,L2,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K6+1:L6,K2+1:L2,K4+1:L4,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I6,I2,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder356412(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K6,L6,K4,L4,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K6+1:L6,K4+1:L4,K1+1:L1,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I6,I4,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder356421(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K6,L6,K4,L4,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K6+1:L6,K4+1:L4,K2+1:L2,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I6,I4,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder361245(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K1,L1,K2,L2,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K1+1:L1,K2+1:L2,K4+1:L4,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I1,I2,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder361254(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K1,L1,K2,L2,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K1+1:L1,K2+1:L2,K5+1:L5,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I1,I2,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder361425(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K1,L1,K4,L4,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K1+1:L1,K4+1:L4,K2+1:L2,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I1,I4,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder361452(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K1,L1,K4,L4,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K1+1:L1,K4+1:L4,K5+1:L5,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I1,I4,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder361524(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K1,L1,K5,L5,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K1+1:L1,K5+1:L5,K2+1:L2,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I1,I5,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder361542(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K1,L1,K5,L5,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K1+1:L1,K5+1:L5,K4+1:L4,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I1,I5,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder362145(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K2,L2,K1,L1,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K2+1:L2,K1+1:L1,K4+1:L4,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I2,I1,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder362154(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K2,L2,K1,L1,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K2+1:L2,K1+1:L1,K5+1:L5,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I2,I1,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder362415(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K2,L2,K4,L4,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K2+1:L2,K4+1:L4,K1+1:L1,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I2,I4,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder362451(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K2,L2,K4,L4,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K2+1:L2,K4+1:L4,K5+1:L5,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I2,I4,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder362514(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K2,L2,K5,L5,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K2+1:L2,K5+1:L5,K1+1:L1,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I2,I5,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder362541(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K2,L2,K5,L5,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K2+1:L2,K5+1:L5,K4+1:L4,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I2,I5,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder364125(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K4,L4,K1,L1,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K4+1:L4,K1+1:L1,K2+1:L2,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I4,I1,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder364152(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K4,L4,K1,L1,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K4+1:L4,K1+1:L1,K5+1:L5,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I4,I1,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder364215(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K4,L4,K2,L2,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K4+1:L4,K2+1:L2,K1+1:L1,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I4,I2,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder364251(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K4,L4,K2,L2,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K4+1:L4,K2+1:L2,K5+1:L5,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I4,I2,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder364512(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K4,L4,K5,L5,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K4+1:L4,K5+1:L5,K1+1:L1,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I4,I5,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder364521(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K4,L4,K5,L5,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K4+1:L4,K5+1:L5,K2+1:L2,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I4,I5,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder365124(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K5,L5,K1,L1,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K5+1:L5,K1+1:L1,K2+1:L2,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I5,I1,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder365142(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K5,L5,K1,L1,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K5+1:L5,K1+1:L1,K4+1:L4,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I5,I1,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder365214(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K5,L5,K2,L2,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K5+1:L5,K2+1:L2,K1+1:L1,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I5,I2,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder365241(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K5,L5,K2,L2,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K5+1:L5,K2+1:L2,K4+1:L4,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I5,I2,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder365412(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K5,L5,K4,L4,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K5+1:L5,K4+1:L4,K1+1:L1,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I5,I4,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder365421(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K5,L5,K4,L4,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K5+1:L5,K4+1:L4,K2+1:L2,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I5,I4,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder412356(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K2,L2,K3,L3,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3,K5+1:L5,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I2,I3,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder412365(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K2,L2,K3,L3,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3,K6+1:L6,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I2,I3,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder412536(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K2,L2,K5,L5,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K5+1:L5,K3+1:L3,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I2,I5,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder412563(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K2,L2,K5,L5,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K5+1:L5,K6+1:L6,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I2,I5,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder412635(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K2,L2,K6,L6,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K6+1:L6,K3+1:L3,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I2,I6,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder412653(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K2,L2,K6,L6,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K6+1:L6,K5+1:L5,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I2,I6,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder413256(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K3,L3,K2,L2,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2,K5+1:L5,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I3,I2,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder413265(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K3,L3,K2,L2,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2,K6+1:L6,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I3,I2,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder413526(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K3,L3,K5,L5,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K5+1:L5,K2+1:L2,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I3,I5,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder413562(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K3,L3,K5,L5,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K5+1:L5,K6+1:L6,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I3,I5,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder413625(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K3,L3,K6,L6,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K6+1:L6,K2+1:L2,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I3,I6,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder413652(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K3,L3,K6,L6,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K6+1:L6,K5+1:L5,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I3,I6,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder415236(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K5,L5,K2,L2,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K5+1:L5,K2+1:L2,K3+1:L3,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I5,I2,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder415263(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K5,L5,K2,L2,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K5+1:L5,K2+1:L2,K6+1:L6,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I5,I2,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder415326(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K5,L5,K3,L3,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K5+1:L5,K3+1:L3,K2+1:L2,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I5,I3,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder415362(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K5,L5,K3,L3,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K5+1:L5,K3+1:L3,K6+1:L6,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I5,I3,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder415623(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K5,L5,K6,L6,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K5+1:L5,K6+1:L6,K2+1:L2,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I5,I6,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder415632(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K5,L5,K6,L6,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K5+1:L5,K6+1:L6,K3+1:L3,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I5,I6,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder416235(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K6,L6,K2,L2,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K6+1:L6,K2+1:L2,K3+1:L3,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I6,I2,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder416253(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K6,L6,K2,L2,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K6+1:L6,K2+1:L2,K5+1:L5,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I6,I2,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder416325(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K6,L6,K3,L3,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K6+1:L6,K3+1:L3,K2+1:L2,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I6,I3,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder416352(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K6,L6,K3,L3,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K6+1:L6,K3+1:L3,K5+1:L5,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I6,I3,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder416523(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K6,L6,K5,L5,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K6+1:L6,K5+1:L5,K2+1:L2,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I6,I5,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder416532(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K6,L6,K5,L5,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K6+1:L6,K5+1:L5,K3+1:L3,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I6,I5,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder421356(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K1,L1,K3,L3,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3,K5+1:L5,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I1,I3,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder421365(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K1,L1,K3,L3,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3,K6+1:L6,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I1,I3,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder421536(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K1,L1,K5,L5,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K5+1:L5,K3+1:L3,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I1,I5,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder421563(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K1,L1,K5,L5,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K5+1:L5,K6+1:L6,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I1,I5,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder421635(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K1,L1,K6,L6,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K6+1:L6,K3+1:L3,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I1,I6,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder421653(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K1,L1,K6,L6,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K6+1:L6,K5+1:L5,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I1,I6,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder423156(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K3,L3,K1,L1,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1,K5+1:L5,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I3,I1,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder423165(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K3,L3,K1,L1,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1,K6+1:L6,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I3,I1,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder423516(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K3,L3,K5,L5,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K5+1:L5,K1+1:L1,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I3,I5,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder423561(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K3,L3,K5,L5,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K5+1:L5,K6+1:L6,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I3,I5,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder423615(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K3,L3,K6,L6,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K6+1:L6,K1+1:L1,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I3,I6,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder423651(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K3,L3,K6,L6,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K6+1:L6,K5+1:L5,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I3,I6,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder425136(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K5,L5,K1,L1,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K5+1:L5,K1+1:L1,K3+1:L3,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I5,I1,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder425163(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K5,L5,K1,L1,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K5+1:L5,K1+1:L1,K6+1:L6,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I5,I1,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder425316(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K5,L5,K3,L3,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K5+1:L5,K3+1:L3,K1+1:L1,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I5,I3,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder425361(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K5,L5,K3,L3,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K5+1:L5,K3+1:L3,K6+1:L6,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I5,I3,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder425613(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K5,L5,K6,L6,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K5+1:L5,K6+1:L6,K1+1:L1,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I5,I6,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder425631(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K5,L5,K6,L6,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K5+1:L5,K6+1:L6,K3+1:L3,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I5,I6,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder426135(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K6,L6,K1,L1,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K6+1:L6,K1+1:L1,K3+1:L3,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I6,I1,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder426153(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K6,L6,K1,L1,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K6+1:L6,K1+1:L1,K5+1:L5,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I6,I1,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder426315(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K6,L6,K3,L3,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K6+1:L6,K3+1:L3,K1+1:L1,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I6,I3,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder426351(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K6,L6,K3,L3,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K6+1:L6,K3+1:L3,K5+1:L5,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I6,I3,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder426513(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K6,L6,K5,L5,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K6+1:L6,K5+1:L5,K1+1:L1,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I6,I5,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder426531(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K6,L6,K5,L5,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K6+1:L6,K5+1:L5,K3+1:L3,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I6,I5,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder431256(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K1,L1,K2,L2,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2,K5+1:L5,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I1,I2,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder431265(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K1,L1,K2,L2,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2,K6+1:L6,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I1,I2,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder431526(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K1,L1,K5,L5,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K5+1:L5,K2+1:L2,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I1,I5,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder431562(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K1,L1,K5,L5,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K5+1:L5,K6+1:L6,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I1,I5,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder431625(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K1,L1,K6,L6,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K6+1:L6,K2+1:L2,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I1,I6,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder431652(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K1,L1,K6,L6,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K6+1:L6,K5+1:L5,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I1,I6,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder432156(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K2,L2,K1,L1,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1,K5+1:L5,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I2,I1,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder432165(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K2,L2,K1,L1,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1,K6+1:L6,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I2,I1,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder432516(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K2,L2,K5,L5,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K5+1:L5,K1+1:L1,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I2,I5,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder432561(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K2,L2,K5,L5,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K5+1:L5,K6+1:L6,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I2,I5,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder432615(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K2,L2,K6,L6,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K6+1:L6,K1+1:L1,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I2,I6,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder432651(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K2,L2,K6,L6,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K6+1:L6,K5+1:L5,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I2,I6,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder435126(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K5,L5,K1,L1,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K5+1:L5,K1+1:L1,K2+1:L2,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I5,I1,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder435162(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K5,L5,K1,L1,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K5+1:L5,K1+1:L1,K6+1:L6,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I5,I1,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder435216(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K5,L5,K2,L2,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K5+1:L5,K2+1:L2,K1+1:L1,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I5,I2,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder435261(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K5,L5,K2,L2,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K5+1:L5,K2+1:L2,K6+1:L6,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I5,I2,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder435612(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K5,L5,K6,L6,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K5+1:L5,K6+1:L6,K1+1:L1,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I5,I6,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder435621(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K5,L5,K6,L6,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K5+1:L5,K6+1:L6,K2+1:L2,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I5,I6,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder436125(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K6,L6,K1,L1,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K6+1:L6,K1+1:L1,K2+1:L2,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I6,I1,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder436152(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K6,L6,K1,L1,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K6+1:L6,K1+1:L1,K5+1:L5,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I6,I1,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder436215(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K6,L6,K2,L2,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K6+1:L6,K2+1:L2,K1+1:L1,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I6,I2,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder436251(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K6,L6,K2,L2,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K6+1:L6,K2+1:L2,K5+1:L5,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I6,I2,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder436512(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K6,L6,K5,L5,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K6+1:L6,K5+1:L5,K1+1:L1,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I6,I5,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder436521(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K6,L6,K5,L5,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K6+1:L6,K5+1:L5,K2+1:L2,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I6,I5,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder451236(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K1,L1,K2,L2,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I1,I2,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder451263(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K1,L1,K2,L2,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K1+1:L1,K2+1:L2,K6+1:L6,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I1,I2,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder451326(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K1,L1,K3,L3,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K1+1:L1,K3+1:L3,K2+1:L2,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I1,I3,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder451362(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K1,L1,K3,L3,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K1+1:L1,K3+1:L3,K6+1:L6,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I1,I3,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder451623(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K1,L1,K6,L6,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K1+1:L1,K6+1:L6,K2+1:L2,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I1,I6,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder451632(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K1,L1,K6,L6,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K1+1:L1,K6+1:L6,K3+1:L3,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I1,I6,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder452136(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K2,L2,K1,L1,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K2+1:L2,K1+1:L1,K3+1:L3,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I2,I1,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder452163(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K2,L2,K1,L1,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K2+1:L2,K1+1:L1,K6+1:L6,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I2,I1,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder452316(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K2,L2,K3,L3,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K2+1:L2,K3+1:L3,K1+1:L1,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I2,I3,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder452361(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K2,L2,K3,L3,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K2+1:L2,K3+1:L3,K6+1:L6,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I2,I3,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder452613(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K2,L2,K6,L6,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K2+1:L2,K6+1:L6,K1+1:L1,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I2,I6,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder452631(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K2,L2,K6,L6,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K2+1:L2,K6+1:L6,K3+1:L3,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I2,I6,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder453126(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K3,L3,K1,L1,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K3+1:L3,K1+1:L1,K2+1:L2,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I3,I1,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder453162(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K3,L3,K1,L1,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K3+1:L3,K1+1:L1,K6+1:L6,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I3,I1,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder453216(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K3,L3,K2,L2,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K3+1:L3,K2+1:L2,K1+1:L1,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I3,I2,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder453261(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K3,L3,K2,L2,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K3+1:L3,K2+1:L2,K6+1:L6,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I3,I2,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder453612(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K3,L3,K6,L6,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K3+1:L3,K6+1:L6,K1+1:L1,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I3,I6,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder453621(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K3,L3,K6,L6,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K3+1:L3,K6+1:L6,K2+1:L2,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I3,I6,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder456123(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K6,L6,K1,L1,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K6+1:L6,K1+1:L1,K2+1:L2,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I6,I1,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder456132(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K6,L6,K1,L1,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K6+1:L6,K1+1:L1,K3+1:L3,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I6,I1,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder456213(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K6,L6,K2,L2,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K6+1:L6,K2+1:L2,K1+1:L1,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I6,I2,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder456231(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K6,L6,K2,L2,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K6+1:L6,K2+1:L2,K3+1:L3,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I6,I2,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder456312(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K6,L6,K3,L3,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K6+1:L6,K3+1:L3,K1+1:L1,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I6,I3,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder456321(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K6,L6,K3,L3,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K6+1:L6,K3+1:L3,K2+1:L2,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I6,I3,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder461235(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K1,L1,K2,L2,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K1+1:L1,K2+1:L2,K3+1:L3,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I1,I2,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder461253(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K1,L1,K2,L2,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K1+1:L1,K2+1:L2,K5+1:L5,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I1,I2,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder461325(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K1,L1,K3,L3,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K1+1:L1,K3+1:L3,K2+1:L2,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I1,I3,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder461352(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K1,L1,K3,L3,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K1+1:L1,K3+1:L3,K5+1:L5,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I1,I3,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder461523(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K1,L1,K5,L5,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K1+1:L1,K5+1:L5,K2+1:L2,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I1,I5,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder461532(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K1,L1,K5,L5,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K1+1:L1,K5+1:L5,K3+1:L3,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I1,I5,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder462135(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K2,L2,K1,L1,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K2+1:L2,K1+1:L1,K3+1:L3,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I2,I1,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder462153(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K2,L2,K1,L1,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K2+1:L2,K1+1:L1,K5+1:L5,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I2,I1,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder462315(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K2,L2,K3,L3,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K2+1:L2,K3+1:L3,K1+1:L1,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I2,I3,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder462351(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K2,L2,K3,L3,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K2+1:L2,K3+1:L3,K5+1:L5,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I2,I3,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder462513(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K2,L2,K5,L5,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K2+1:L2,K5+1:L5,K1+1:L1,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I2,I5,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder462531(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K2,L2,K5,L5,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K2+1:L2,K5+1:L5,K3+1:L3,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I2,I5,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder463125(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K3,L3,K1,L1,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K3+1:L3,K1+1:L1,K2+1:L2,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I3,I1,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder463152(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K3,L3,K1,L1,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K3+1:L3,K1+1:L1,K5+1:L5,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I3,I1,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder463215(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K3,L3,K2,L2,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K3+1:L3,K2+1:L2,K1+1:L1,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I3,I2,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder463251(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K3,L3,K2,L2,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K3+1:L3,K2+1:L2,K5+1:L5,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I3,I2,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder463512(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K3,L3,K5,L5,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K3+1:L3,K5+1:L5,K1+1:L1,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I3,I5,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder463521(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K3,L3,K5,L5,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K3+1:L3,K5+1:L5,K2+1:L2,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I3,I5,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder465123(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K5,L5,K1,L1,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I5,I1,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder465132(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K5,L5,K1,L1,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K5+1:L5,K1+1:L1,K3+1:L3,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I5,I1,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder465213(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K5,L5,K2,L2,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K5+1:L5,K2+1:L2,K1+1:L1,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I5,I2,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder465231(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K5,L5,K2,L2,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K5+1:L5,K2+1:L2,K3+1:L3,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I5,I2,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder465312(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K5,L5,K3,L3,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K5+1:L5,K3+1:L3,K1+1:L1,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I5,I3,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder465321(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K5,L5,K3,L3,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K5+1:L5,K3+1:L3,K2+1:L2,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I5,I3,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder512346(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K2,L2,K3,L3,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I2,I3,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder512364(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K2,L2,K3,L3,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3,K6+1:L6,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I2,I3,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder512436(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K2,L2,K4,L4,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I2,I4,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder512463(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K2,L2,K4,L4,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K2+1:L2,K4+1:L4,K6+1:L6,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I2,I4,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder512634(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K2,L2,K6,L6,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K2+1:L2,K6+1:L6,K3+1:L3,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I2,I6,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder512643(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K2,L2,K6,L6,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K2+1:L2,K6+1:L6,K4+1:L4,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I2,I6,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder513246(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K3,L3,K2,L2,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I3,I2,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder513264(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K3,L3,K2,L2,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K3+1:L3,K2+1:L2,K6+1:L6,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I3,I2,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder513426(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K3,L3,K4,L4,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I3,I4,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder513462(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K3,L3,K4,L4,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K3+1:L3,K4+1:L4,K6+1:L6,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I3,I4,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder513624(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K3,L3,K6,L6,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K3+1:L3,K6+1:L6,K2+1:L2,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I3,I6,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder513642(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K3,L3,K6,L6,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K3+1:L3,K6+1:L6,K4+1:L4,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I3,I6,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder514236(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K4,L4,K2,L2,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I4,I2,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder514263(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K4,L4,K2,L2,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K4+1:L4,K2+1:L2,K6+1:L6,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I4,I2,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder514326(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K4,L4,K3,L3,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I4,I3,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder514362(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K4,L4,K3,L3,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K4+1:L4,K3+1:L3,K6+1:L6,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I4,I3,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder514623(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K4,L4,K6,L6,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K4+1:L4,K6+1:L6,K2+1:L2,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I4,I6,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder514632(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K4,L4,K6,L6,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K4+1:L4,K6+1:L6,K3+1:L3,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I4,I6,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder516234(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K6,L6,K2,L2,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K6+1:L6,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I6,I2,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder516243(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K6,L6,K2,L2,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K6+1:L6,K2+1:L2,K4+1:L4,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I6,I2,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder516324(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K6,L6,K3,L3,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K6+1:L6,K3+1:L3,K2+1:L2,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I6,I3,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder516342(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K6,L6,K3,L3,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K6+1:L6,K3+1:L3,K4+1:L4,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I6,I3,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder516423(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K6,L6,K4,L4,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K6+1:L6,K4+1:L4,K2+1:L2,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I6,I4,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder516432(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K6,L6,K4,L4,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K6+1:L6,K4+1:L4,K3+1:L3,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I6,I4,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder521346(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K1,L1,K3,L3,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I1,I3,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder521364(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K1,L1,K3,L3,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K1+1:L1,K3+1:L3,K6+1:L6,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I1,I3,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder521436(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K1,L1,K4,L4,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I1,I4,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder521463(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K1,L1,K4,L4,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K1+1:L1,K4+1:L4,K6+1:L6,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I1,I4,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder521634(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K1,L1,K6,L6,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K1+1:L1,K6+1:L6,K3+1:L3,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I1,I6,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder521643(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K1,L1,K6,L6,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K1+1:L1,K6+1:L6,K4+1:L4,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I1,I6,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder523146(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K3,L3,K1,L1,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I3,I1,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder523164(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K3,L3,K1,L1,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K3+1:L3,K1+1:L1,K6+1:L6,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I3,I1,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder523416(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K3,L3,K4,L4,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I3,I4,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder523461(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K3,L3,K4,L4,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K3+1:L3,K4+1:L4,K6+1:L6,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I3,I4,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder523614(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K3,L3,K6,L6,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K3+1:L3,K6+1:L6,K1+1:L1,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I3,I6,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder523641(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K3,L3,K6,L6,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K3+1:L3,K6+1:L6,K4+1:L4,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I3,I6,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder524136(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K4,L4,K1,L1,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I4,I1,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder524163(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K4,L4,K1,L1,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K4+1:L4,K1+1:L1,K6+1:L6,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I4,I1,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder524316(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K4,L4,K3,L3,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I4,I3,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder524361(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K4,L4,K3,L3,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K4+1:L4,K3+1:L3,K6+1:L6,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I4,I3,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder524613(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K4,L4,K6,L6,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K4+1:L4,K6+1:L6,K1+1:L1,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I4,I6,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder524631(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K4,L4,K6,L6,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K4+1:L4,K6+1:L6,K3+1:L3,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I4,I6,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder526134(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K6,L6,K1,L1,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K6+1:L6,K1+1:L1,K3+1:L3,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I6,I1,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder526143(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K6,L6,K1,L1,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K6+1:L6,K1+1:L1,K4+1:L4,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I6,I1,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder526314(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K6,L6,K3,L3,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K6+1:L6,K3+1:L3,K1+1:L1,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I6,I3,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder526341(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K6,L6,K3,L3,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K6+1:L6,K3+1:L3,K4+1:L4,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I6,I3,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder526413(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K6,L6,K4,L4,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K6+1:L6,K4+1:L4,K1+1:L1,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I6,I4,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder526431(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K6,L6,K4,L4,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K6+1:L6,K4+1:L4,K3+1:L3,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I6,I4,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder531246(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K1,L1,K2,L2,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I1,I2,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder531264(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K1,L1,K2,L2,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K1+1:L1,K2+1:L2,K6+1:L6,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I1,I2,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder531426(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K1,L1,K4,L4,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I1,I4,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder531462(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K1,L1,K4,L4,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K1+1:L1,K4+1:L4,K6+1:L6,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I1,I4,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder531624(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K1,L1,K6,L6,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K1+1:L1,K6+1:L6,K2+1:L2,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I1,I6,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder531642(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K1,L1,K6,L6,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K1+1:L1,K6+1:L6,K4+1:L4,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I1,I6,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder532146(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K2,L2,K1,L1,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I2,I1,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder532164(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K2,L2,K1,L1,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K2+1:L2,K1+1:L1,K6+1:L6,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I2,I1,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder532416(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K2,L2,K4,L4,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I2,I4,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder532461(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K2,L2,K4,L4,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K2+1:L2,K4+1:L4,K6+1:L6,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I2,I4,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder532614(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K2,L2,K6,L6,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K2+1:L2,K6+1:L6,K1+1:L1,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I2,I6,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder532641(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K2,L2,K6,L6,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K2+1:L2,K6+1:L6,K4+1:L4,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I2,I6,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder534126(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K4,L4,K1,L1,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I4,I1,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder534162(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K4,L4,K1,L1,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K4+1:L4,K1+1:L1,K6+1:L6,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I4,I1,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder534216(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K4,L4,K2,L2,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I4,I2,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder534261(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K4,L4,K2,L2,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K4+1:L4,K2+1:L2,K6+1:L6,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I4,I2,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder534612(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K4,L4,K6,L6,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K4+1:L4,K6+1:L6,K1+1:L1,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I4,I6,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder534621(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K4,L4,K6,L6,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K4+1:L4,K6+1:L6,K2+1:L2,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I4,I6,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder536124(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K6,L6,K1,L1,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K6+1:L6,K1+1:L1,K2+1:L2,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I6,I1,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder536142(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K6,L6,K1,L1,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K6+1:L6,K1+1:L1,K4+1:L4,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I6,I1,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder536214(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K6,L6,K2,L2,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K6+1:L6,K2+1:L2,K1+1:L1,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I6,I2,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder536241(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K6,L6,K2,L2,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K6+1:L6,K2+1:L2,K4+1:L4,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I6,I2,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder536412(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K6,L6,K4,L4,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K6+1:L6,K4+1:L4,K1+1:L1,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I6,I4,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder536421(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K6,L6,K4,L4,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K6+1:L6,K4+1:L4,K2+1:L2,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I6,I4,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder541236(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K1,L1,K2,L2,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I1,I2,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder541263(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K1,L1,K2,L2,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K1+1:L1,K2+1:L2,K6+1:L6,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I1,I2,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder541326(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K1,L1,K3,L3,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I1,I3,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder541362(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K1,L1,K3,L3,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K1+1:L1,K3+1:L3,K6+1:L6,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I1,I3,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder541623(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K1,L1,K6,L6,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K1+1:L1,K6+1:L6,K2+1:L2,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I1,I6,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder541632(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K1,L1,K6,L6,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K1+1:L1,K6+1:L6,K3+1:L3,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I1,I6,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder542136(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K2,L2,K1,L1,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I2,I1,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder542163(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K2,L2,K1,L1,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K2+1:L2,K1+1:L1,K6+1:L6,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I2,I1,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder542316(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K2,L2,K3,L3,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I2,I3,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder542361(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K2,L2,K3,L3,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K2+1:L2,K3+1:L3,K6+1:L6,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I2,I3,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder542613(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K2,L2,K6,L6,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K2+1:L2,K6+1:L6,K1+1:L1,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I2,I6,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder542631(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K2,L2,K6,L6,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K2+1:L2,K6+1:L6,K3+1:L3,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I2,I6,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder543126(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K3,L3,K1,L1,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I3,I1,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder543162(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K3,L3,K1,L1,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K3+1:L3,K1+1:L1,K6+1:L6,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I3,I1,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder543216(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K3,L3,K2,L2,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1,K6+1:L6)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I3,I2,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder543261(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K3,L3,K2,L2,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K3+1:L3,K2+1:L2,K6+1:L6,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I3,I2,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder543612(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K3,L3,K6,L6,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K3+1:L3,K6+1:L6,K1+1:L1,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I3,I6,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder543621(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K3,L3,K6,L6,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K3+1:L3,K6+1:L6,K2+1:L2,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I3,I6,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder546123(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K6,L6,K1,L1,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K6+1:L6,K1+1:L1,K2+1:L2,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I6,I1,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder546132(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K6,L6,K1,L1,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K6+1:L6,K1+1:L1,K3+1:L3,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I6,I1,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder546213(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K6,L6,K2,L2,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K6+1:L6,K2+1:L2,K1+1:L1,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I6,I2,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder546231(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K6,L6,K2,L2,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K6+1:L6,K2+1:L2,K3+1:L3,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I6,I2,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder546312(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K6,L6,K3,L3,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K6+1:L6,K3+1:L3,K1+1:L1,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I6,I3,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder546321(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K6,L6,K3,L3,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K6+1:L6,K3+1:L3,K2+1:L2,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I6,I3,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder561234(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K1,L1,K2,L2,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I1,I2,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder561243(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K1,L1,K2,L2,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I1,I2,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder561324(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K1,L1,K3,L3,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I1,I3,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder561342(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K1,L1,K3,L3,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I1,I3,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder561423(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K1,L1,K4,L4,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I1,I4,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder561432(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K1,L1,K4,L4,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I1,I4,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder562134(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K2,L2,K1,L1,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I2,I1,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder562143(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K2,L2,K1,L1,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I2,I1,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder562314(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K2,L2,K3,L3,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I2,I3,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder562341(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K2,L2,K3,L3,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I2,I3,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder562413(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K2,L2,K4,L4,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I2,I4,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder562431(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K2,L2,K4,L4,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I2,I4,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder563124(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K3,L3,K1,L1,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I3,I1,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder563142(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K3,L3,K1,L1,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I3,I1,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder563214(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K3,L3,K2,L2,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I3,I2,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder563241(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K3,L3,K2,L2,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I3,I2,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder563412(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K3,L3,K4,L4,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I3,I4,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder563421(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K3,L3,K4,L4,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I3,I4,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder564123(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K4,L4,K1,L1,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I4,I1,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder564132(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K4,L4,K1,L1,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I4,I1,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder564213(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K4,L4,K2,L2,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I4,I2,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder564231(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K4,L4,K2,L2,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I4,I2,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder564312(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K4,L4,K3,L3,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I4,I3,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder564321(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K4,L4,K3,L3,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I4,I3,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder612345(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder612354(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K2,L2,K3,L3,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K2+1:L2,K3+1:L3,K5+1:L5,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I2,I3,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder612435(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K2,L2,K4,L4,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I2,I4,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder612453(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K2,L2,K4,L4,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K2+1:L2,K4+1:L4,K5+1:L5,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I2,I4,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder612534(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K2,L2,K5,L5,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K2+1:L2,K5+1:L5,K3+1:L3,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I2,I5,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder612543(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K2,L2,K5,L5,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K2+1:L2,K5+1:L5,K4+1:L4,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I2,I5,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder613245(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K3,L3,K2,L2,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I3,I2,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder613254(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K3,L3,K2,L2,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K3+1:L3,K2+1:L2,K5+1:L5,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I3,I2,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder613425(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K3,L3,K4,L4,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I3,I4,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder613452(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K3,L3,K4,L4,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K3+1:L3,K4+1:L4,K5+1:L5,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I3,I4,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder613524(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K3,L3,K5,L5,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K3+1:L3,K5+1:L5,K2+1:L2,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I3,I5,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder613542(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K3,L3,K5,L5,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K3+1:L3,K5+1:L5,K4+1:L4,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I3,I5,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder614235(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K4,L4,K2,L2,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I4,I2,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder614253(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K4,L4,K2,L2,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K4+1:L4,K2+1:L2,K5+1:L5,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I4,I2,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder614325(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K4,L4,K3,L3,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I4,I3,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder614352(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K4,L4,K3,L3,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K4+1:L4,K3+1:L3,K5+1:L5,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I4,I3,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder614523(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K4,L4,K5,L5,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K4+1:L4,K5+1:L5,K2+1:L2,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I4,I5,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder614532(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K4,L4,K5,L5,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K4+1:L4,K5+1:L5,K3+1:L3,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I4,I5,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder615234(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K5,L5,K2,L2,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K5+1:L5,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I5,I2,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder615243(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K5,L5,K2,L2,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K5+1:L5,K2+1:L2,K4+1:L4,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I5,I2,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder615324(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K5,L5,K3,L3,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K5+1:L5,K3+1:L3,K2+1:L2,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I5,I3,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder615342(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K5,L5,K3,L3,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K5+1:L5,K3+1:L3,K4+1:L4,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I5,I3,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder615423(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K5,L5,K4,L4,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K5+1:L5,K4+1:L4,K2+1:L2,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I5,I4,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder615432(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K5,L5,K4,L4,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K5+1:L5,K4+1:L4,K3+1:L3,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I5,I4,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder621345(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K1,L1,K3,L3,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I1,I3,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder621354(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K1,L1,K3,L3,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K1+1:L1,K3+1:L3,K5+1:L5,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I1,I3,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder621435(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K1,L1,K4,L4,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I1,I4,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder621453(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K1,L1,K4,L4,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K1+1:L1,K4+1:L4,K5+1:L5,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I1,I4,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder621534(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K1,L1,K5,L5,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K1+1:L1,K5+1:L5,K3+1:L3,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I1,I5,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder621543(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K1,L1,K5,L5,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K1+1:L1,K5+1:L5,K4+1:L4,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I1,I5,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder623145(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K3,L3,K1,L1,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I3,I1,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder623154(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K3,L3,K1,L1,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K3+1:L3,K1+1:L1,K5+1:L5,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I3,I1,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder623415(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K3,L3,K4,L4,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I3,I4,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder623451(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K3,L3,K4,L4,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I3,I4,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder623514(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K3,L3,K5,L5,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K3+1:L3,K5+1:L5,K1+1:L1,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I3,I5,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder623541(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K3,L3,K5,L5,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K3+1:L3,K5+1:L5,K4+1:L4,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I3,I5,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder624135(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K4,L4,K1,L1,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I4,I1,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder624153(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K4,L4,K1,L1,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K4+1:L4,K1+1:L1,K5+1:L5,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I4,I1,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder624315(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K4,L4,K3,L3,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I4,I3,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder624351(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K4,L4,K3,L3,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K4+1:L4,K3+1:L3,K5+1:L5,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I4,I3,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder624513(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K4,L4,K5,L5,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K4+1:L4,K5+1:L5,K1+1:L1,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I4,I5,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder624531(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K4,L4,K5,L5,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K4+1:L4,K5+1:L5,K3+1:L3,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I4,I5,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder625134(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K5,L5,K1,L1,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K5+1:L5,K1+1:L1,K3+1:L3,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I5,I1,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder625143(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K5,L5,K1,L1,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K5+1:L5,K1+1:L1,K4+1:L4,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I5,I1,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder625314(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K5,L5,K3,L3,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K5+1:L5,K3+1:L3,K1+1:L1,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I5,I3,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder625341(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K5,L5,K3,L3,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K5+1:L5,K3+1:L3,K4+1:L4,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I5,I3,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder625413(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K5,L5,K4,L4,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K5+1:L5,K4+1:L4,K1+1:L1,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I5,I4,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder625431(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K5,L5,K4,L4,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K5+1:L5,K4+1:L4,K3+1:L3,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I5,I4,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder631245(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K1,L1,K2,L2,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I1,I2,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder631254(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K1,L1,K2,L2,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K1+1:L1,K2+1:L2,K5+1:L5,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I1,I2,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder631425(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K1,L1,K4,L4,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I1,I4,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder631452(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K1,L1,K4,L4,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I1,I4,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder631524(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K1,L1,K5,L5,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K1+1:L1,K5+1:L5,K2+1:L2,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I1,I5,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder631542(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K1,L1,K5,L5,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K1+1:L1,K5+1:L5,K4+1:L4,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I1,I5,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder632145(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K2,L2,K1,L1,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I2,I1,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder632154(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K2,L2,K1,L1,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K2+1:L2,K1+1:L1,K5+1:L5,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I2,I1,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder632415(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K2,L2,K4,L4,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I2,I4,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder632451(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K2,L2,K4,L4,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K2+1:L2,K4+1:L4,K5+1:L5,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I2,I4,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder632514(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K2,L2,K5,L5,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K2+1:L2,K5+1:L5,K1+1:L1,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I2,I5,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder632541(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K2,L2,K5,L5,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K2+1:L2,K5+1:L5,K4+1:L4,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I2,I5,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder634125(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K4,L4,K1,L1,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I4,I1,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder634152(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K4,L4,K1,L1,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K4+1:L4,K1+1:L1,K5+1:L5,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I4,I1,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder634215(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K4,L4,K2,L2,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I4,I2,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder634251(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K4,L4,K2,L2,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K4+1:L4,K2+1:L2,K5+1:L5,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I4,I2,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder634512(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K4,L4,K5,L5,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K4+1:L4,K5+1:L5,K1+1:L1,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I4,I5,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder634521(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K4,L4,K5,L5,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K4+1:L4,K5+1:L5,K2+1:L2,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I4,I5,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder635124(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K5,L5,K1,L1,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K5+1:L5,K1+1:L1,K2+1:L2,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I5,I1,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder635142(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K5,L5,K1,L1,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K5+1:L5,K1+1:L1,K4+1:L4,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I5,I1,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder635214(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K5,L5,K2,L2,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K5+1:L5,K2+1:L2,K1+1:L1,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I5,I2,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder635241(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K5,L5,K2,L2,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K5+1:L5,K2+1:L2,K4+1:L4,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I5,I2,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder635412(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K5,L5,K4,L4,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K5+1:L5,K4+1:L4,K1+1:L1,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I5,I4,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder635421(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K5,L5,K4,L4,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K5+1:L5,K4+1:L4,K2+1:L2,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I5,I4,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder641235(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K1,L1,K2,L2,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I1,I2,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder641253(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K1,L1,K2,L2,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K1+1:L1,K2+1:L2,K5+1:L5,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I1,I2,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder641325(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K1,L1,K3,L3,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I1,I3,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder641352(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K1,L1,K3,L3,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K1+1:L1,K3+1:L3,K5+1:L5,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I1,I3,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder641523(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K1,L1,K5,L5,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K1+1:L1,K5+1:L5,K2+1:L2,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I1,I5,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder641532(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K1,L1,K5,L5,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K1+1:L1,K5+1:L5,K3+1:L3,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I1,I5,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder642135(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K2,L2,K1,L1,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I2,I1,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder642153(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K2,L2,K1,L1,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K2+1:L2,K1+1:L1,K5+1:L5,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I2,I1,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder642315(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K2,L2,K3,L3,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I2,I3,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder642351(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K2,L2,K3,L3,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K2+1:L2,K3+1:L3,K5+1:L5,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I2,I3,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder642513(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K2,L2,K5,L5,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K2+1:L2,K5+1:L5,K1+1:L1,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I2,I5,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder642531(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K2,L2,K5,L5,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K2+1:L2,K5+1:L5,K3+1:L3,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I2,I5,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder643125(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K3,L3,K1,L1,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I3,I1,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder643152(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K3,L3,K1,L1,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K3+1:L3,K1+1:L1,K5+1:L5,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I3,I1,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder643215(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K3,L3,K2,L2,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1,K5+1:L5)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I3,I2,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder643251(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K3,L3,K2,L2,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K3+1:L3,K2+1:L2,K5+1:L5,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I3,I2,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder643512(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K3,L3,K5,L5,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K3+1:L3,K5+1:L5,K1+1:L1,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I3,I5,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder643521(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K3,L3,K5,L5,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K3+1:L3,K5+1:L5,K2+1:L2,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I3,I5,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder645123(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K5,L5,K1,L1,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I5,I1,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder645132(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K5,L5,K1,L1,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K5+1:L5,K1+1:L1,K3+1:L3,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I5,I1,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder645213(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K5,L5,K2,L2,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K5+1:L5,K2+1:L2,K1+1:L1,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I5,I2,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder645231(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K5,L5,K2,L2,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K5+1:L5,K2+1:L2,K3+1:L3,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I5,I2,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder645312(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K5,L5,K3,L3,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K5+1:L5,K3+1:L3,K1+1:L1,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I5,I3,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder645321(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K5,L5,K3,L3,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K5+1:L5,K3+1:L3,K2+1:L2,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I5,I3,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder651234(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K1,L1,K2,L2,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I1,I2,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder651243(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K1,L1,K2,L2,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I1,I2,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder651324(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K1,L1,K3,L3,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I1,I3,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder651342(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K1,L1,K3,L3,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I1,I3,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder651423(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K1,L1,K4,L4,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I1,I4,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder651432(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K1,L1,K4,L4,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I1,I4,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder652134(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K2,L2,K1,L1,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I2,I1,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder652143(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K2,L2,K1,L1,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I2,I1,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder652314(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K2,L2,K3,L3,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I2,I3,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder652341(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K2,L2,K3,L3,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I2,I3,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder652413(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K2,L2,K4,L4,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I2,I4,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder652431(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K2,L2,K4,L4,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I2,I4,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder653124(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K3,L3,K1,L1,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I3,I1,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder653142(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K3,L3,K1,L1,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I3,I1,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder653214(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K3,L3,K2,L2,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I3,I2,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder653241(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K3,L3,K2,L2,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I3,I2,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder653412(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K3,L3,K4,L4,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I3,I4,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder653421(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K3,L3,K4,L4,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I3,I4,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder654123(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K4,L4,K1,L1,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I4,I1,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder654132(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K4,L4,K1,L1,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I4,I1,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder654213(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K4,L4,K2,L2,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I4,I2,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder654231(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K4,L4,K2,L2,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I4,I2,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder654312(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K4,L4,K3,L3,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I4,I3,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
       subroutine reorder654321(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K4,L4,K3,L3,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1)
       real*8 C
C
!$OMP PARALLEL DO PRIVATE(I1,I2,I3,I4,I5,I6)
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I4,I3,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
!$OMP END PARALLEL DO
       end
C
