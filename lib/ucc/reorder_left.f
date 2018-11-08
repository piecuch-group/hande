       subroutine relef12(M1,N1,M2,N2,
     & K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2)
       real*8 B(K1+1:L1,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
        B(I1,I2)=A(I1,I2)
       enddo
       enddo
C
       end
C
       subroutine relef21(M1,N1,M2,N2,
     & K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2)
       real*8 B(K2+1:L2,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
        B(I2,I1)=A(I1,I2)
       enddo
       enddo
C
       end
C
       subroutine relef1234(M1,N1,M2,N2,M3,N3,M4,N4,
     & K1,L1,K2,L2,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef1243(M1,N1,M2,N2,M3,N3,M4,N4,
     & K1,L1,K2,L2,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef1324(M1,N1,M2,N2,M3,N3,M4,N4,
     & K1,L1,K3,L3,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef1342(M1,N1,M2,N2,M3,N3,M4,N4,
     & K1,L1,K3,L3,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef1423(M1,N1,M2,N2,M3,N3,M4,N4,
     & K1,L1,K4,L4,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef1432(M1,N1,M2,N2,M3,N3,M4,N4,
     & K1,L1,K4,L4,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef2134(M1,N1,M2,N2,M3,N3,M4,N4,
     & K2,L2,K1,L1,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef2143(M1,N1,M2,N2,M3,N3,M4,N4,
     & K2,L2,K1,L1,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef2314(M1,N1,M2,N2,M3,N3,M4,N4,
     & K2,L2,K3,L3,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef2341(M1,N1,M2,N2,M3,N3,M4,N4,
     & K2,L2,K3,L3,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef2413(M1,N1,M2,N2,M3,N3,M4,N4,
     & K2,L2,K4,L4,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef2431(M1,N1,M2,N2,M3,N3,M4,N4,
     & K2,L2,K4,L4,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef3124(M1,N1,M2,N2,M3,N3,M4,N4,
     & K3,L3,K1,L1,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef3142(M1,N1,M2,N2,M3,N3,M4,N4,
     & K3,L3,K1,L1,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef3214(M1,N1,M2,N2,M3,N3,M4,N4,
     & K3,L3,K2,L2,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef3241(M1,N1,M2,N2,M3,N3,M4,N4,
     & K3,L3,K2,L2,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef3412(M1,N1,M2,N2,M3,N3,M4,N4,
     & K3,L3,K4,L4,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef3421(M1,N1,M2,N2,M3,N3,M4,N4,
     & K3,L3,K4,L4,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef4123(M1,N1,M2,N2,M3,N3,M4,N4,
     & K4,L4,K1,L1,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef4132(M1,N1,M2,N2,M3,N3,M4,N4,
     & K4,L4,K1,L1,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef4213(M1,N1,M2,N2,M3,N3,M4,N4,
     & K4,L4,K2,L2,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef4231(M1,N1,M2,N2,M3,N3,M4,N4,
     & K4,L4,K2,L2,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef4312(M1,N1,M2,N2,M3,N3,M4,N4,
     & K4,L4,K3,L3,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef4321(M1,N1,M2,N2,M3,N3,M4,N4,
     & K4,L4,K3,L3,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef123456(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef123465(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K3,L3,K4,L4,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K6+1:L6,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef123546(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K3,L3,K5,L5,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K5+1:L5,K4+1:L4,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef123564(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K3,L3,K5,L5,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K5+1:L5,K6+1:L6,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef123645(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K3,L3,K6,L6,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K6+1:L6,K4+1:L4,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef123654(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K3,L3,K6,L6,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K6+1:L6,K5+1:L5,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef124356(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K4,L4,K3,L3,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3,K5+1:L5,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef124365(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K4,L4,K3,L3,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3,K6+1:L6,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef124536(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K4,L4,K5,L5,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K5+1:L5,K3+1:L3,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef124563(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K4,L4,K5,L5,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K5+1:L5,K6+1:L6,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef124635(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K4,L4,K6,L6,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K6+1:L6,K3+1:L3,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef124653(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K4,L4,K6,L6,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K6+1:L6,K5+1:L5,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef125346(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K5,L5,K3,L3,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K5+1:L5,K3+1:L3,K4+1:L4,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef125364(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K5,L5,K3,L3,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K5+1:L5,K3+1:L3,K6+1:L6,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef125436(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K5,L5,K4,L4,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K5+1:L5,K4+1:L4,K3+1:L3,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef125463(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K5,L5,K4,L4,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K5+1:L5,K4+1:L4,K6+1:L6,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef125634(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K5,L5,K6,L6,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K5+1:L5,K6+1:L6,K3+1:L3,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef125643(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K5,L5,K6,L6,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K5+1:L5,K6+1:L6,K4+1:L4,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef126345(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K6,L6,K3,L3,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K6+1:L6,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef126354(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K6,L6,K3,L3,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K6+1:L6,K3+1:L3,K5+1:L5,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef126435(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K6,L6,K4,L4,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K6+1:L6,K4+1:L4,K3+1:L3,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef126453(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K6,L6,K4,L4,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K6+1:L6,K4+1:L4,K5+1:L5,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef126534(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K6,L6,K5,L5,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K6+1:L6,K5+1:L5,K3+1:L3,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef126543(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K6,L6,K5,L5,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K6+1:L6,K5+1:L5,K4+1:L4,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef132456(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K2,L2,K4,L4,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef132465(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K2,L2,K4,L4,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4,K6+1:L6,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef132546(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K2,L2,K5,L5,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K5+1:L5,K4+1:L4,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef132564(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K2,L2,K5,L5,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K5+1:L5,K6+1:L6,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef132645(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K2,L2,K6,L6,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K6+1:L6,K4+1:L4,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef132654(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K2,L2,K6,L6,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K6+1:L6,K5+1:L5,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef134256(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K4,L4,K2,L2,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2,K5+1:L5,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef134265(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K4,L4,K2,L2,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2,K6+1:L6,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef134526(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K4,L4,K5,L5,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K5+1:L5,K2+1:L2,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef134562(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K4,L4,K5,L5,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef134625(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K4,L4,K6,L6,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K6+1:L6,K2+1:L2,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef134652(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K4,L4,K6,L6,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K6+1:L6,K5+1:L5,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef135246(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K5,L5,K2,L2,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K5+1:L5,K2+1:L2,K4+1:L4,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef135264(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K5,L5,K2,L2,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K5+1:L5,K2+1:L2,K6+1:L6,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef135426(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K5,L5,K4,L4,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K5+1:L5,K4+1:L4,K2+1:L2,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef135462(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K5,L5,K4,L4,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K5+1:L5,K4+1:L4,K6+1:L6,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef135624(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K5,L5,K6,L6,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K5+1:L5,K6+1:L6,K2+1:L2,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef135642(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K5,L5,K6,L6,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K5+1:L5,K6+1:L6,K4+1:L4,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef136245(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K6,L6,K2,L2,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K6+1:L6,K2+1:L2,K4+1:L4,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef136254(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K6,L6,K2,L2,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K6+1:L6,K2+1:L2,K5+1:L5,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef136425(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K6,L6,K4,L4,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K6+1:L6,K4+1:L4,K2+1:L2,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef136452(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K6,L6,K4,L4,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K6+1:L6,K4+1:L4,K5+1:L5,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef136524(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K6,L6,K5,L5,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K6+1:L6,K5+1:L5,K2+1:L2,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef136542(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K6,L6,K5,L5,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K6+1:L6,K5+1:L5,K4+1:L4,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef142356(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K2,L2,K3,L3,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3,K5+1:L5,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef142365(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K2,L2,K3,L3,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3,K6+1:L6,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef142536(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K2,L2,K5,L5,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K5+1:L5,K3+1:L3,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef142563(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K2,L2,K5,L5,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K5+1:L5,K6+1:L6,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef142635(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K2,L2,K6,L6,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K6+1:L6,K3+1:L3,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef142653(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K2,L2,K6,L6,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K6+1:L6,K5+1:L5,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef143256(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K3,L3,K2,L2,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2,K5+1:L5,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef143265(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K3,L3,K2,L2,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2,K6+1:L6,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef143526(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K3,L3,K5,L5,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K5+1:L5,K2+1:L2,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef143562(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K3,L3,K5,L5,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K5+1:L5,K6+1:L6,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef143625(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K3,L3,K6,L6,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K6+1:L6,K2+1:L2,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef143652(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K3,L3,K6,L6,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K6+1:L6,K5+1:L5,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef145236(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K5,L5,K2,L2,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K5+1:L5,K2+1:L2,K3+1:L3,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef145263(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K5,L5,K2,L2,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K5+1:L5,K2+1:L2,K6+1:L6,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef145326(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K5,L5,K3,L3,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K5+1:L5,K3+1:L3,K2+1:L2,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef145362(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K5,L5,K3,L3,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K5+1:L5,K3+1:L3,K6+1:L6,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef145623(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K5,L5,K6,L6,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K5+1:L5,K6+1:L6,K2+1:L2,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef145632(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K5,L5,K6,L6,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K5+1:L5,K6+1:L6,K3+1:L3,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef146235(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K6,L6,K2,L2,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K6+1:L6,K2+1:L2,K3+1:L3,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef146253(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K6,L6,K2,L2,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K6+1:L6,K2+1:L2,K5+1:L5,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef146325(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K6,L6,K3,L3,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K6+1:L6,K3+1:L3,K2+1:L2,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef146352(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K6,L6,K3,L3,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K6+1:L6,K3+1:L3,K5+1:L5,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef146523(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K6,L6,K5,L5,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K6+1:L6,K5+1:L5,K2+1:L2,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef146532(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K6,L6,K5,L5,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K6+1:L6,K5+1:L5,K3+1:L3,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef152346(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K2,L2,K3,L3,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K2+1:L2,K3+1:L3,K4+1:L4,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef152364(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K2,L2,K3,L3,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K2+1:L2,K3+1:L3,K6+1:L6,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef152436(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K2,L2,K4,L4,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K2+1:L2,K4+1:L4,K3+1:L3,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef152463(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K2,L2,K4,L4,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K2+1:L2,K4+1:L4,K6+1:L6,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef152634(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K2,L2,K6,L6,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K2+1:L2,K6+1:L6,K3+1:L3,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef152643(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K2,L2,K6,L6,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K2+1:L2,K6+1:L6,K4+1:L4,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef153246(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K3,L3,K2,L2,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K3+1:L3,K2+1:L2,K4+1:L4,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef153264(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K3,L3,K2,L2,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K3+1:L3,K2+1:L2,K6+1:L6,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef153426(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K3,L3,K4,L4,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K3+1:L3,K4+1:L4,K2+1:L2,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef153462(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K3,L3,K4,L4,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K3+1:L3,K4+1:L4,K6+1:L6,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef153624(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K3,L3,K6,L6,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K3+1:L3,K6+1:L6,K2+1:L2,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef153642(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K3,L3,K6,L6,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K3+1:L3,K6+1:L6,K4+1:L4,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef154236(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K4,L4,K2,L2,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K4+1:L4,K2+1:L2,K3+1:L3,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef154263(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K4,L4,K2,L2,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K4+1:L4,K2+1:L2,K6+1:L6,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef154326(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K4,L4,K3,L3,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K4+1:L4,K3+1:L3,K2+1:L2,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef154362(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K4,L4,K3,L3,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K4+1:L4,K3+1:L3,K6+1:L6,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef154623(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K4,L4,K6,L6,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K4+1:L4,K6+1:L6,K2+1:L2,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef154632(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K4,L4,K6,L6,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K4+1:L4,K6+1:L6,K3+1:L3,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef156234(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K6,L6,K2,L2,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K6+1:L6,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef156243(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K6,L6,K2,L2,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K6+1:L6,K2+1:L2,K4+1:L4,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef156324(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K6,L6,K3,L3,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K6+1:L6,K3+1:L3,K2+1:L2,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef156342(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K6,L6,K3,L3,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K6+1:L6,K3+1:L3,K4+1:L4,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef156423(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K6,L6,K4,L4,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K6+1:L6,K4+1:L4,K2+1:L2,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef156432(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K6,L6,K4,L4,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K6+1:L6,K4+1:L4,K3+1:L3,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef162345(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K2,L2,K3,L3,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef162354(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K2,L2,K3,L3,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K2+1:L2,K3+1:L3,K5+1:L5,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef162435(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K2,L2,K4,L4,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K2+1:L2,K4+1:L4,K3+1:L3,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef162453(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K2,L2,K4,L4,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K2+1:L2,K4+1:L4,K5+1:L5,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef162534(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K2,L2,K5,L5,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K2+1:L2,K5+1:L5,K3+1:L3,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef162543(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K2,L2,K5,L5,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K2+1:L2,K5+1:L5,K4+1:L4,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef163245(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K3,L3,K2,L2,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K3+1:L3,K2+1:L2,K4+1:L4,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef163254(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K3,L3,K2,L2,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K3+1:L3,K2+1:L2,K5+1:L5,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef163425(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K3,L3,K4,L4,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K3+1:L3,K4+1:L4,K2+1:L2,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef163452(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K3,L3,K4,L4,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K3+1:L3,K4+1:L4,K5+1:L5,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef163524(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K3,L3,K5,L5,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K3+1:L3,K5+1:L5,K2+1:L2,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef163542(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K3,L3,K5,L5,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K3+1:L3,K5+1:L5,K4+1:L4,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef164235(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K4,L4,K2,L2,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K4+1:L4,K2+1:L2,K3+1:L3,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef164253(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K4,L4,K2,L2,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K4+1:L4,K2+1:L2,K5+1:L5,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef164325(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K4,L4,K3,L3,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K4+1:L4,K3+1:L3,K2+1:L2,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef164352(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K4,L4,K3,L3,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K4+1:L4,K3+1:L3,K5+1:L5,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef164523(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K4,L4,K5,L5,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K4+1:L4,K5+1:L5,K2+1:L2,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef164532(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K4,L4,K5,L5,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K4+1:L4,K5+1:L5,K3+1:L3,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef165234(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K5,L5,K2,L2,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K5+1:L5,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef165243(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K5,L5,K2,L2,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K5+1:L5,K2+1:L2,K4+1:L4,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef165324(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K5,L5,K3,L3,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K5+1:L5,K3+1:L3,K2+1:L2,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef165342(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K5,L5,K3,L3,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K5+1:L5,K3+1:L3,K4+1:L4,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef165423(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K5,L5,K4,L4,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K5+1:L5,K4+1:L4,K2+1:L2,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef165432(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K5,L5,K4,L4,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K5+1:L5,K4+1:L4,K3+1:L3,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef213456(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K3,L3,K4,L4,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef213465(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K3,L3,K4,L4,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4,K6+1:L6,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef213546(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K3,L3,K5,L5,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K5+1:L5,K4+1:L4,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef213564(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K3,L3,K5,L5,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K5+1:L5,K6+1:L6,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef213645(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K3,L3,K6,L6,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K6+1:L6,K4+1:L4,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef213654(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K3,L3,K6,L6,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K6+1:L6,K5+1:L5,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef214356(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K4,L4,K3,L3,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3,K5+1:L5,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef214365(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K4,L4,K3,L3,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3,K6+1:L6,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef214536(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K4,L4,K5,L5,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K5+1:L5,K3+1:L3,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef214563(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K4,L4,K5,L5,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K5+1:L5,K6+1:L6,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef214635(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K4,L4,K6,L6,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K6+1:L6,K3+1:L3,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef214653(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K4,L4,K6,L6,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K6+1:L6,K5+1:L5,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef215346(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K5,L5,K3,L3,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K5+1:L5,K3+1:L3,K4+1:L4,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef215364(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K5,L5,K3,L3,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K5+1:L5,K3+1:L3,K6+1:L6,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef215436(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K5,L5,K4,L4,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K5+1:L5,K4+1:L4,K3+1:L3,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef215463(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K5,L5,K4,L4,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K5+1:L5,K4+1:L4,K6+1:L6,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef215634(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K5,L5,K6,L6,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K5+1:L5,K6+1:L6,K3+1:L3,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef215643(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K5,L5,K6,L6,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K5+1:L5,K6+1:L6,K4+1:L4,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef216345(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K6,L6,K3,L3,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K6+1:L6,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef216354(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K6,L6,K3,L3,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K6+1:L6,K3+1:L3,K5+1:L5,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef216435(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K6,L6,K4,L4,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K6+1:L6,K4+1:L4,K3+1:L3,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef216453(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K6,L6,K4,L4,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K6+1:L6,K4+1:L4,K5+1:L5,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef216534(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K6,L6,K5,L5,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K6+1:L6,K5+1:L5,K3+1:L3,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef216543(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K6,L6,K5,L5,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K6+1:L6,K5+1:L5,K4+1:L4,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef231456(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K1,L1,K4,L4,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef231465(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K1,L1,K4,L4,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4,K6+1:L6,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef231546(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K1,L1,K5,L5,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K5+1:L5,K4+1:L4,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef231564(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K1,L1,K5,L5,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K5+1:L5,K6+1:L6,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef231645(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K1,L1,K6,L6,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K6+1:L6,K4+1:L4,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef231654(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K1,L1,K6,L6,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K6+1:L6,K5+1:L5,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef234156(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K4,L4,K1,L1,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1,K5+1:L5,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef234165(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K4,L4,K1,L1,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1,K6+1:L6,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef234516(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K4,L4,K5,L5,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K1+1:L1,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef234561(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef234615(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K4,L4,K6,L6,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K6+1:L6,K1+1:L1,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef234651(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K4,L4,K6,L6,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K6+1:L6,K5+1:L5,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef235146(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K5,L5,K1,L1,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K5+1:L5,K1+1:L1,K4+1:L4,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef235164(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K5,L5,K1,L1,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K5+1:L5,K1+1:L1,K6+1:L6,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef235416(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K5,L5,K4,L4,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K5+1:L5,K4+1:L4,K1+1:L1,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef235461(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K5,L5,K4,L4,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K5+1:L5,K4+1:L4,K6+1:L6,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef235614(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K5,L5,K6,L6,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K5+1:L5,K6+1:L6,K1+1:L1,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef235641(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K5,L5,K6,L6,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K5+1:L5,K6+1:L6,K4+1:L4,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef236145(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K6,L6,K1,L1,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K6+1:L6,K1+1:L1,K4+1:L4,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef236154(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K6,L6,K1,L1,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K6+1:L6,K1+1:L1,K5+1:L5,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef236415(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K6,L6,K4,L4,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K6+1:L6,K4+1:L4,K1+1:L1,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef236451(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K6,L6,K4,L4,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K6+1:L6,K4+1:L4,K5+1:L5,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef236514(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K6,L6,K5,L5,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K6+1:L6,K5+1:L5,K1+1:L1,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef236541(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K6,L6,K5,L5,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K6+1:L6,K5+1:L5,K4+1:L4,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef241356(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K1,L1,K3,L3,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3,K5+1:L5,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef241365(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K1,L1,K3,L3,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3,K6+1:L6,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef241536(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K1,L1,K5,L5,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K5+1:L5,K3+1:L3,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef241563(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K1,L1,K5,L5,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K5+1:L5,K6+1:L6,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef241635(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K1,L1,K6,L6,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K6+1:L6,K3+1:L3,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef241653(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K1,L1,K6,L6,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K6+1:L6,K5+1:L5,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef243156(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K3,L3,K1,L1,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1,K5+1:L5,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef243165(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K3,L3,K1,L1,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1,K6+1:L6,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef243516(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K3,L3,K5,L5,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K5+1:L5,K1+1:L1,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef243561(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K3,L3,K5,L5,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K5+1:L5,K6+1:L6,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef243615(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K3,L3,K6,L6,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K6+1:L6,K1+1:L1,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef243651(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K3,L3,K6,L6,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K6+1:L6,K5+1:L5,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef245136(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K5,L5,K1,L1,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K5+1:L5,K1+1:L1,K3+1:L3,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef245163(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K5,L5,K1,L1,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K5+1:L5,K1+1:L1,K6+1:L6,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef245316(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K5,L5,K3,L3,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K5+1:L5,K3+1:L3,K1+1:L1,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef245361(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K5,L5,K3,L3,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K5+1:L5,K3+1:L3,K6+1:L6,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef245613(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K5,L5,K6,L6,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K5+1:L5,K6+1:L6,K1+1:L1,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef245631(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K5,L5,K6,L6,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K5+1:L5,K6+1:L6,K3+1:L3,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef246135(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K6,L6,K1,L1,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K6+1:L6,K1+1:L1,K3+1:L3,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef246153(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K6,L6,K1,L1,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K6+1:L6,K1+1:L1,K5+1:L5,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef246315(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K6,L6,K3,L3,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K6+1:L6,K3+1:L3,K1+1:L1,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef246351(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K6,L6,K3,L3,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K6+1:L6,K3+1:L3,K5+1:L5,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef246513(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K6,L6,K5,L5,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K6+1:L6,K5+1:L5,K1+1:L1,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef246531(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K6,L6,K5,L5,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K6+1:L6,K5+1:L5,K3+1:L3,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef251346(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K1,L1,K3,L3,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K1+1:L1,K3+1:L3,K4+1:L4,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef251364(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K1,L1,K3,L3,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K1+1:L1,K3+1:L3,K6+1:L6,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef251436(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K1,L1,K4,L4,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K1+1:L1,K4+1:L4,K3+1:L3,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef251463(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K1,L1,K4,L4,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K1+1:L1,K4+1:L4,K6+1:L6,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef251634(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K1,L1,K6,L6,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K1+1:L1,K6+1:L6,K3+1:L3,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef251643(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K1,L1,K6,L6,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K1+1:L1,K6+1:L6,K4+1:L4,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef253146(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K3,L3,K1,L1,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K3+1:L3,K1+1:L1,K4+1:L4,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef253164(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K3,L3,K1,L1,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K3+1:L3,K1+1:L1,K6+1:L6,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef253416(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K3,L3,K4,L4,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K3+1:L3,K4+1:L4,K1+1:L1,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef253461(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K3,L3,K4,L4,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K3+1:L3,K4+1:L4,K6+1:L6,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef253614(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K3,L3,K6,L6,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K3+1:L3,K6+1:L6,K1+1:L1,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef253641(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K3,L3,K6,L6,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K3+1:L3,K6+1:L6,K4+1:L4,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef254136(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K4,L4,K1,L1,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K4+1:L4,K1+1:L1,K3+1:L3,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef254163(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K4,L4,K1,L1,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K4+1:L4,K1+1:L1,K6+1:L6,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef254316(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K4,L4,K3,L3,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K4+1:L4,K3+1:L3,K1+1:L1,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef254361(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K4,L4,K3,L3,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K4+1:L4,K3+1:L3,K6+1:L6,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef254613(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K4,L4,K6,L6,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K4+1:L4,K6+1:L6,K1+1:L1,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef254631(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K4,L4,K6,L6,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K4+1:L4,K6+1:L6,K3+1:L3,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef256134(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K6,L6,K1,L1,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K6+1:L6,K1+1:L1,K3+1:L3,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef256143(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K6,L6,K1,L1,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K6+1:L6,K1+1:L1,K4+1:L4,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef256314(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K6,L6,K3,L3,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K6+1:L6,K3+1:L3,K1+1:L1,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef256341(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K6,L6,K3,L3,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K6+1:L6,K3+1:L3,K4+1:L4,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef256413(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K6,L6,K4,L4,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K6+1:L6,K4+1:L4,K1+1:L1,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef256431(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K6,L6,K4,L4,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K6+1:L6,K4+1:L4,K3+1:L3,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef261345(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K1,L1,K3,L3,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K1+1:L1,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef261354(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K1,L1,K3,L3,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K1+1:L1,K3+1:L3,K5+1:L5,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef261435(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K1,L1,K4,L4,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K1+1:L1,K4+1:L4,K3+1:L3,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef261453(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K1,L1,K4,L4,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K1+1:L1,K4+1:L4,K5+1:L5,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef261534(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K1,L1,K5,L5,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K1+1:L1,K5+1:L5,K3+1:L3,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef261543(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K1,L1,K5,L5,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K1+1:L1,K5+1:L5,K4+1:L4,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef263145(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K3,L3,K1,L1,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef263154(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K3,L3,K1,L1,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K3+1:L3,K1+1:L1,K5+1:L5,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef263415(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K3,L3,K4,L4,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K3+1:L3,K4+1:L4,K1+1:L1,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef263451(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K3,L3,K4,L4,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K3+1:L3,K4+1:L4,K5+1:L5,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef263514(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K3,L3,K5,L5,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K3+1:L3,K5+1:L5,K1+1:L1,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef263541(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K3,L3,K5,L5,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K3+1:L3,K5+1:L5,K4+1:L4,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef264135(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K4,L4,K1,L1,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K4+1:L4,K1+1:L1,K3+1:L3,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef264153(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K4,L4,K1,L1,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K4+1:L4,K1+1:L1,K5+1:L5,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef264315(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K4,L4,K3,L3,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K4+1:L4,K3+1:L3,K1+1:L1,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef264351(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K4,L4,K3,L3,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K4+1:L4,K3+1:L3,K5+1:L5,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef264513(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K4,L4,K5,L5,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K4+1:L4,K5+1:L5,K1+1:L1,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef264531(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K4,L4,K5,L5,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K4+1:L4,K5+1:L5,K3+1:L3,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef265134(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K5,L5,K1,L1,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K5+1:L5,K1+1:L1,K3+1:L3,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef265143(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K5,L5,K1,L1,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K5+1:L5,K1+1:L1,K4+1:L4,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef265314(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K5,L5,K3,L3,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K5+1:L5,K3+1:L3,K1+1:L1,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef265341(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K5,L5,K3,L3,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K5+1:L5,K3+1:L3,K4+1:L4,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef265413(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K5,L5,K4,L4,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K5+1:L5,K4+1:L4,K1+1:L1,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef265431(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K5,L5,K4,L4,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K5+1:L5,K4+1:L4,K3+1:L3,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef312456(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K2,L2,K4,L4,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef312465(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K2,L2,K4,L4,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4,K6+1:L6,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef312546(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K2,L2,K5,L5,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K5+1:L5,K4+1:L4,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef312564(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K2,L2,K5,L5,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K5+1:L5,K6+1:L6,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef312645(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K2,L2,K6,L6,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K6+1:L6,K4+1:L4,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef312654(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K2,L2,K6,L6,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K6+1:L6,K5+1:L5,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef314256(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K4,L4,K2,L2,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2,K5+1:L5,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef314265(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K4,L4,K2,L2,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2,K6+1:L6,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef314526(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K4,L4,K5,L5,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5,K2+1:L2,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef314562(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K4,L4,K5,L5,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5,K6+1:L6,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef314625(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K4,L4,K6,L6,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K6+1:L6,K2+1:L2,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef314652(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K4,L4,K6,L6,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K6+1:L6,K5+1:L5,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef315246(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K5,L5,K2,L2,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K5+1:L5,K2+1:L2,K4+1:L4,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef315264(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K5,L5,K2,L2,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K5+1:L5,K2+1:L2,K6+1:L6,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef315426(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K5,L5,K4,L4,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K5+1:L5,K4+1:L4,K2+1:L2,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef315462(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K5,L5,K4,L4,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K5+1:L5,K4+1:L4,K6+1:L6,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef315624(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K5,L5,K6,L6,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K5+1:L5,K6+1:L6,K2+1:L2,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef315642(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K5,L5,K6,L6,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K5+1:L5,K6+1:L6,K4+1:L4,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef316245(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K6,L6,K2,L2,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K6+1:L6,K2+1:L2,K4+1:L4,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef316254(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K6,L6,K2,L2,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K6+1:L6,K2+1:L2,K5+1:L5,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef316425(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K6,L6,K4,L4,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K6+1:L6,K4+1:L4,K2+1:L2,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef316452(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K6,L6,K4,L4,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K6+1:L6,K4+1:L4,K5+1:L5,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef316524(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K6,L6,K5,L5,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K6+1:L6,K5+1:L5,K2+1:L2,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef316542(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K6,L6,K5,L5,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K6+1:L6,K5+1:L5,K4+1:L4,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef321456(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K1,L1,K4,L4,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef321465(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K1,L1,K4,L4,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4,K6+1:L6,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef321546(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K1,L1,K5,L5,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K5+1:L5,K4+1:L4,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef321564(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K1,L1,K5,L5,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K5+1:L5,K6+1:L6,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef321645(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K1,L1,K6,L6,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K6+1:L6,K4+1:L4,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef321654(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K1,L1,K6,L6,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K6+1:L6,K5+1:L5,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef324156(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K4,L4,K1,L1,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1,K5+1:L5,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef324165(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K4,L4,K1,L1,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1,K6+1:L6,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef324516(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K4,L4,K5,L5,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K5+1:L5,K1+1:L1,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef324561(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K4,L4,K5,L5,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K5+1:L5,K6+1:L6,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef324615(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K4,L4,K6,L6,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K6+1:L6,K1+1:L1,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef324651(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K4,L4,K6,L6,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K6+1:L6,K5+1:L5,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef325146(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K5,L5,K1,L1,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K5+1:L5,K1+1:L1,K4+1:L4,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef325164(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K5,L5,K1,L1,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K5+1:L5,K1+1:L1,K6+1:L6,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef325416(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K5,L5,K4,L4,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K5+1:L5,K4+1:L4,K1+1:L1,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef325461(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K5,L5,K4,L4,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K5+1:L5,K4+1:L4,K6+1:L6,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef325614(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K5,L5,K6,L6,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K5+1:L5,K6+1:L6,K1+1:L1,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef325641(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K5,L5,K6,L6,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K5+1:L5,K6+1:L6,K4+1:L4,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef326145(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K6,L6,K1,L1,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K6+1:L6,K1+1:L1,K4+1:L4,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef326154(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K6,L6,K1,L1,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K6+1:L6,K1+1:L1,K5+1:L5,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef326415(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K6,L6,K4,L4,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K6+1:L6,K4+1:L4,K1+1:L1,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef326451(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K6,L6,K4,L4,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K6+1:L6,K4+1:L4,K5+1:L5,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef326514(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K6,L6,K5,L5,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K6+1:L6,K5+1:L5,K1+1:L1,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef326541(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K6,L6,K5,L5,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K6+1:L6,K5+1:L5,K4+1:L4,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef341256(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K1,L1,K2,L2,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2,K5+1:L5,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef341265(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K1,L1,K2,L2,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2,K6+1:L6,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef341526(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K1,L1,K5,L5,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K5+1:L5,K2+1:L2,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef341562(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K1,L1,K5,L5,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K5+1:L5,K6+1:L6,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef341625(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K1,L1,K6,L6,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K6+1:L6,K2+1:L2,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef341652(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K1,L1,K6,L6,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K6+1:L6,K5+1:L5,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef342156(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K2,L2,K1,L1,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1,K5+1:L5,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef342165(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K2,L2,K1,L1,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1,K6+1:L6,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef342516(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K2,L2,K5,L5,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K5+1:L5,K1+1:L1,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef342561(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K2,L2,K5,L5,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K5+1:L5,K6+1:L6,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef342615(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K2,L2,K6,L6,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K6+1:L6,K1+1:L1,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef342651(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K2,L2,K6,L6,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K6+1:L6,K5+1:L5,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef345126(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K5,L5,K1,L1,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K5+1:L5,K1+1:L1,K2+1:L2,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef345162(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K5,L5,K1,L1,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K5+1:L5,K1+1:L1,K6+1:L6,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef345216(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K5,L5,K2,L2,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K5+1:L5,K2+1:L2,K1+1:L1,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef345261(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K5,L5,K2,L2,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K5+1:L5,K2+1:L2,K6+1:L6,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef345612(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K5,L5,K6,L6,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6,K1+1:L1,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef345621(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K5,L5,K6,L6,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6,K2+1:L2,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef346125(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K6,L6,K1,L1,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K6+1:L6,K1+1:L1,K2+1:L2,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef346152(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K6,L6,K1,L1,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K6+1:L6,K1+1:L1,K5+1:L5,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef346215(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K6,L6,K2,L2,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K6+1:L6,K2+1:L2,K1+1:L1,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef346251(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K6,L6,K2,L2,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K6+1:L6,K2+1:L2,K5+1:L5,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef346512(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K6,L6,K5,L5,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K6+1:L6,K5+1:L5,K1+1:L1,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef346521(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K6,L6,K5,L5,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K6+1:L6,K5+1:L5,K2+1:L2,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef351246(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K1,L1,K2,L2,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K1+1:L1,K2+1:L2,K4+1:L4,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef351264(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K1,L1,K2,L2,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K1+1:L1,K2+1:L2,K6+1:L6,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef351426(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K1,L1,K4,L4,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K1+1:L1,K4+1:L4,K2+1:L2,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef351462(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K1,L1,K4,L4,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K1+1:L1,K4+1:L4,K6+1:L6,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef351624(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K1,L1,K6,L6,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K1+1:L1,K6+1:L6,K2+1:L2,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef351642(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K1,L1,K6,L6,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K1+1:L1,K6+1:L6,K4+1:L4,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef352146(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K2,L2,K1,L1,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K2+1:L2,K1+1:L1,K4+1:L4,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef352164(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K2,L2,K1,L1,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K2+1:L2,K1+1:L1,K6+1:L6,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef352416(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K2,L2,K4,L4,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K2+1:L2,K4+1:L4,K1+1:L1,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef352461(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K2,L2,K4,L4,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K2+1:L2,K4+1:L4,K6+1:L6,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef352614(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K2,L2,K6,L6,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K2+1:L2,K6+1:L6,K1+1:L1,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef352641(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K2,L2,K6,L6,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K2+1:L2,K6+1:L6,K4+1:L4,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef354126(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K4,L4,K1,L1,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K4+1:L4,K1+1:L1,K2+1:L2,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef354162(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K4,L4,K1,L1,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K4+1:L4,K1+1:L1,K6+1:L6,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef354216(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K4,L4,K2,L2,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K4+1:L4,K2+1:L2,K1+1:L1,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef354261(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K4,L4,K2,L2,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K4+1:L4,K2+1:L2,K6+1:L6,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef354612(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K4,L4,K6,L6,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K4+1:L4,K6+1:L6,K1+1:L1,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef354621(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K4,L4,K6,L6,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K4+1:L4,K6+1:L6,K2+1:L2,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef356124(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K6,L6,K1,L1,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K6+1:L6,K1+1:L1,K2+1:L2,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef356142(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K6,L6,K1,L1,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K6+1:L6,K1+1:L1,K4+1:L4,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef356214(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K6,L6,K2,L2,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K6+1:L6,K2+1:L2,K1+1:L1,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef356241(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K6,L6,K2,L2,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K6+1:L6,K2+1:L2,K4+1:L4,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef356412(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K6,L6,K4,L4,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K6+1:L6,K4+1:L4,K1+1:L1,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef356421(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K6,L6,K4,L4,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K6+1:L6,K4+1:L4,K2+1:L2,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef361245(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K1,L1,K2,L2,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K1+1:L1,K2+1:L2,K4+1:L4,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef361254(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K1,L1,K2,L2,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K1+1:L1,K2+1:L2,K5+1:L5,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef361425(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K1,L1,K4,L4,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K1+1:L1,K4+1:L4,K2+1:L2,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef361452(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K1,L1,K4,L4,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K1+1:L1,K4+1:L4,K5+1:L5,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef361524(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K1,L1,K5,L5,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K1+1:L1,K5+1:L5,K2+1:L2,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef361542(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K1,L1,K5,L5,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K1+1:L1,K5+1:L5,K4+1:L4,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef362145(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K2,L2,K1,L1,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K2+1:L2,K1+1:L1,K4+1:L4,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef362154(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K2,L2,K1,L1,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K2+1:L2,K1+1:L1,K5+1:L5,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef362415(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K2,L2,K4,L4,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K2+1:L2,K4+1:L4,K1+1:L1,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef362451(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K2,L2,K4,L4,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K2+1:L2,K4+1:L4,K5+1:L5,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef362514(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K2,L2,K5,L5,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K2+1:L2,K5+1:L5,K1+1:L1,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef362541(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K2,L2,K5,L5,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K2+1:L2,K5+1:L5,K4+1:L4,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef364125(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K4,L4,K1,L1,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K4+1:L4,K1+1:L1,K2+1:L2,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef364152(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K4,L4,K1,L1,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K4+1:L4,K1+1:L1,K5+1:L5,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef364215(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K4,L4,K2,L2,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K4+1:L4,K2+1:L2,K1+1:L1,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef364251(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K4,L4,K2,L2,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K4+1:L4,K2+1:L2,K5+1:L5,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef364512(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K4,L4,K5,L5,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K4+1:L4,K5+1:L5,K1+1:L1,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef364521(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K4,L4,K5,L5,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K4+1:L4,K5+1:L5,K2+1:L2,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef365124(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K5,L5,K1,L1,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K5+1:L5,K1+1:L1,K2+1:L2,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef365142(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K5,L5,K1,L1,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K5+1:L5,K1+1:L1,K4+1:L4,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef365214(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K5,L5,K2,L2,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K5+1:L5,K2+1:L2,K1+1:L1,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef365241(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K5,L5,K2,L2,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K5+1:L5,K2+1:L2,K4+1:L4,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef365412(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K5,L5,K4,L4,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K5+1:L5,K4+1:L4,K1+1:L1,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef365421(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K5,L5,K4,L4,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K5+1:L5,K4+1:L4,K2+1:L2,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef412356(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K2,L2,K3,L3,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3,K5+1:L5,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef412365(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K2,L2,K3,L3,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3,K6+1:L6,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef412536(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K2,L2,K5,L5,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K5+1:L5,K3+1:L3,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef412563(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K2,L2,K5,L5,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K5+1:L5,K6+1:L6,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef412635(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K2,L2,K6,L6,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K6+1:L6,K3+1:L3,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef412653(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K2,L2,K6,L6,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K6+1:L6,K5+1:L5,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef413256(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K3,L3,K2,L2,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2,K5+1:L5,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef413265(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K3,L3,K2,L2,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2,K6+1:L6,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef413526(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K3,L3,K5,L5,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K5+1:L5,K2+1:L2,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef413562(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K3,L3,K5,L5,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K5+1:L5,K6+1:L6,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef413625(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K3,L3,K6,L6,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K6+1:L6,K2+1:L2,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef413652(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K3,L3,K6,L6,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K6+1:L6,K5+1:L5,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef415236(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K5,L5,K2,L2,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K5+1:L5,K2+1:L2,K3+1:L3,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef415263(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K5,L5,K2,L2,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K5+1:L5,K2+1:L2,K6+1:L6,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef415326(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K5,L5,K3,L3,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K5+1:L5,K3+1:L3,K2+1:L2,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef415362(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K5,L5,K3,L3,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K5+1:L5,K3+1:L3,K6+1:L6,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef415623(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K5,L5,K6,L6,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K5+1:L5,K6+1:L6,K2+1:L2,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef415632(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K5,L5,K6,L6,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K5+1:L5,K6+1:L6,K3+1:L3,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef416235(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K6,L6,K2,L2,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K6+1:L6,K2+1:L2,K3+1:L3,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef416253(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K6,L6,K2,L2,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K6+1:L6,K2+1:L2,K5+1:L5,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef416325(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K6,L6,K3,L3,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K6+1:L6,K3+1:L3,K2+1:L2,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef416352(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K6,L6,K3,L3,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K6+1:L6,K3+1:L3,K5+1:L5,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef416523(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K6,L6,K5,L5,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K6+1:L6,K5+1:L5,K2+1:L2,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef416532(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K6,L6,K5,L5,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K6+1:L6,K5+1:L5,K3+1:L3,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef421356(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K1,L1,K3,L3,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3,K5+1:L5,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef421365(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K1,L1,K3,L3,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3,K6+1:L6,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef421536(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K1,L1,K5,L5,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K5+1:L5,K3+1:L3,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef421563(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K1,L1,K5,L5,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K5+1:L5,K6+1:L6,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef421635(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K1,L1,K6,L6,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K6+1:L6,K3+1:L3,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef421653(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K1,L1,K6,L6,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K6+1:L6,K5+1:L5,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef423156(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K3,L3,K1,L1,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1,K5+1:L5,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef423165(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K3,L3,K1,L1,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1,K6+1:L6,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef423516(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K3,L3,K5,L5,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K5+1:L5,K1+1:L1,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef423561(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K3,L3,K5,L5,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K5+1:L5,K6+1:L6,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef423615(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K3,L3,K6,L6,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K6+1:L6,K1+1:L1,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef423651(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K3,L3,K6,L6,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K6+1:L6,K5+1:L5,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef425136(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K5,L5,K1,L1,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K5+1:L5,K1+1:L1,K3+1:L3,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef425163(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K5,L5,K1,L1,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K5+1:L5,K1+1:L1,K6+1:L6,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef425316(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K5,L5,K3,L3,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K5+1:L5,K3+1:L3,K1+1:L1,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef425361(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K5,L5,K3,L3,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K5+1:L5,K3+1:L3,K6+1:L6,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef425613(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K5,L5,K6,L6,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K5+1:L5,K6+1:L6,K1+1:L1,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef425631(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K5,L5,K6,L6,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K5+1:L5,K6+1:L6,K3+1:L3,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef426135(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K6,L6,K1,L1,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K6+1:L6,K1+1:L1,K3+1:L3,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef426153(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K6,L6,K1,L1,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K6+1:L6,K1+1:L1,K5+1:L5,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef426315(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K6,L6,K3,L3,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K6+1:L6,K3+1:L3,K1+1:L1,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef426351(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K6,L6,K3,L3,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K6+1:L6,K3+1:L3,K5+1:L5,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef426513(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K6,L6,K5,L5,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K6+1:L6,K5+1:L5,K1+1:L1,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef426531(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K6,L6,K5,L5,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K6+1:L6,K5+1:L5,K3+1:L3,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef431256(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K1,L1,K2,L2,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2,K5+1:L5,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef431265(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K1,L1,K2,L2,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2,K6+1:L6,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef431526(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K1,L1,K5,L5,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K5+1:L5,K2+1:L2,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef431562(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K1,L1,K5,L5,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K5+1:L5,K6+1:L6,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef431625(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K1,L1,K6,L6,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K6+1:L6,K2+1:L2,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef431652(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K1,L1,K6,L6,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K6+1:L6,K5+1:L5,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef432156(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K2,L2,K1,L1,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1,K5+1:L5,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef432165(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K2,L2,K1,L1,K6,L6,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1,K6+1:L6,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef432516(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K2,L2,K5,L5,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K5+1:L5,K1+1:L1,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef432561(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K2,L2,K5,L5,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K5+1:L5,K6+1:L6,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef432615(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K2,L2,K6,L6,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K6+1:L6,K1+1:L1,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef432651(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K2,L2,K6,L6,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K6+1:L6,K5+1:L5,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef435126(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K5,L5,K1,L1,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K5+1:L5,K1+1:L1,K2+1:L2,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef435162(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K5,L5,K1,L1,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K5+1:L5,K1+1:L1,K6+1:L6,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef435216(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K5,L5,K2,L2,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K5+1:L5,K2+1:L2,K1+1:L1,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef435261(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K5,L5,K2,L2,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K5+1:L5,K2+1:L2,K6+1:L6,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef435612(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K5,L5,K6,L6,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K5+1:L5,K6+1:L6,K1+1:L1,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef435621(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K5,L5,K6,L6,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K5+1:L5,K6+1:L6,K2+1:L2,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef436125(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K6,L6,K1,L1,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K6+1:L6,K1+1:L1,K2+1:L2,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef436152(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K6,L6,K1,L1,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K6+1:L6,K1+1:L1,K5+1:L5,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef436215(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K6,L6,K2,L2,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K6+1:L6,K2+1:L2,K1+1:L1,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef436251(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K6,L6,K2,L2,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K6+1:L6,K2+1:L2,K5+1:L5,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef436512(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K6,L6,K5,L5,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K6+1:L6,K5+1:L5,K1+1:L1,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef436521(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K6,L6,K5,L5,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K6+1:L6,K5+1:L5,K2+1:L2,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef451236(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K1,L1,K2,L2,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef451263(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K1,L1,K2,L2,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K1+1:L1,K2+1:L2,K6+1:L6,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef451326(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K1,L1,K3,L3,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K1+1:L1,K3+1:L3,K2+1:L2,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef451362(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K1,L1,K3,L3,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K1+1:L1,K3+1:L3,K6+1:L6,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef451623(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K1,L1,K6,L6,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K1+1:L1,K6+1:L6,K2+1:L2,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef451632(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K1,L1,K6,L6,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K1+1:L1,K6+1:L6,K3+1:L3,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef452136(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K2,L2,K1,L1,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K2+1:L2,K1+1:L1,K3+1:L3,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef452163(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K2,L2,K1,L1,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K2+1:L2,K1+1:L1,K6+1:L6,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef452316(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K2,L2,K3,L3,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K2+1:L2,K3+1:L3,K1+1:L1,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef452361(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K2,L2,K3,L3,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K2+1:L2,K3+1:L3,K6+1:L6,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef452613(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K2,L2,K6,L6,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K2+1:L2,K6+1:L6,K1+1:L1,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef452631(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K2,L2,K6,L6,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K2+1:L2,K6+1:L6,K3+1:L3,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef453126(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K3,L3,K1,L1,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K3+1:L3,K1+1:L1,K2+1:L2,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef453162(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K3,L3,K1,L1,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K3+1:L3,K1+1:L1,K6+1:L6,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef453216(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K3,L3,K2,L2,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K3+1:L3,K2+1:L2,K1+1:L1,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef453261(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K3,L3,K2,L2,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K3+1:L3,K2+1:L2,K6+1:L6,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef453612(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K3,L3,K6,L6,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K3+1:L3,K6+1:L6,K1+1:L1,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef453621(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K3,L3,K6,L6,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K3+1:L3,K6+1:L6,K2+1:L2,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef456123(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K6,L6,K1,L1,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K6+1:L6,K1+1:L1,K2+1:L2,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef456132(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K6,L6,K1,L1,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K6+1:L6,K1+1:L1,K3+1:L3,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef456213(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K6,L6,K2,L2,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K6+1:L6,K2+1:L2,K1+1:L1,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef456231(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K6,L6,K2,L2,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K6+1:L6,K2+1:L2,K3+1:L3,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef456312(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K6,L6,K3,L3,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K6+1:L6,K3+1:L3,K1+1:L1,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef456321(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K6,L6,K3,L3,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K6+1:L6,K3+1:L3,K2+1:L2,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef461235(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K1,L1,K2,L2,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K1+1:L1,K2+1:L2,K3+1:L3,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef461253(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K1,L1,K2,L2,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K1+1:L1,K2+1:L2,K5+1:L5,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef461325(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K1,L1,K3,L3,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K1+1:L1,K3+1:L3,K2+1:L2,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef461352(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K1,L1,K3,L3,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K1+1:L1,K3+1:L3,K5+1:L5,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef461523(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K1,L1,K5,L5,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K1+1:L1,K5+1:L5,K2+1:L2,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef461532(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K1,L1,K5,L5,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K1+1:L1,K5+1:L5,K3+1:L3,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef462135(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K2,L2,K1,L1,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K2+1:L2,K1+1:L1,K3+1:L3,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef462153(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K2,L2,K1,L1,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K2+1:L2,K1+1:L1,K5+1:L5,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef462315(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K2,L2,K3,L3,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K2+1:L2,K3+1:L3,K1+1:L1,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef462351(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K2,L2,K3,L3,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K2+1:L2,K3+1:L3,K5+1:L5,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef462513(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K2,L2,K5,L5,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K2+1:L2,K5+1:L5,K1+1:L1,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef462531(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K2,L2,K5,L5,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K2+1:L2,K5+1:L5,K3+1:L3,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef463125(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K3,L3,K1,L1,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K3+1:L3,K1+1:L1,K2+1:L2,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef463152(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K3,L3,K1,L1,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K3+1:L3,K1+1:L1,K5+1:L5,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef463215(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K3,L3,K2,L2,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K3+1:L3,K2+1:L2,K1+1:L1,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef463251(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K3,L3,K2,L2,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K3+1:L3,K2+1:L2,K5+1:L5,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef463512(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K3,L3,K5,L5,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K3+1:L3,K5+1:L5,K1+1:L1,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef463521(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K3,L3,K5,L5,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K3+1:L3,K5+1:L5,K2+1:L2,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef465123(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K5,L5,K1,L1,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef465132(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K5,L5,K1,L1,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K5+1:L5,K1+1:L1,K3+1:L3,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef465213(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K5,L5,K2,L2,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K5+1:L5,K2+1:L2,K1+1:L1,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef465231(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K5,L5,K2,L2,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K5+1:L5,K2+1:L2,K3+1:L3,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef465312(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K5,L5,K3,L3,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K5+1:L5,K3+1:L3,K1+1:L1,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef465321(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K5,L5,K3,L3,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K5+1:L5,K3+1:L3,K2+1:L2,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef512346(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K2,L2,K3,L3,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef512364(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K2,L2,K3,L3,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3,K6+1:L6,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef512436(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K2,L2,K4,L4,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef512463(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K2,L2,K4,L4,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K2+1:L2,K4+1:L4,K6+1:L6,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef512634(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K2,L2,K6,L6,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K2+1:L2,K6+1:L6,K3+1:L3,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef512643(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K2,L2,K6,L6,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K2+1:L2,K6+1:L6,K4+1:L4,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef513246(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K3,L3,K2,L2,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef513264(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K3,L3,K2,L2,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K3+1:L3,K2+1:L2,K6+1:L6,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef513426(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K3,L3,K4,L4,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef513462(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K3,L3,K4,L4,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K3+1:L3,K4+1:L4,K6+1:L6,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef513624(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K3,L3,K6,L6,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K3+1:L3,K6+1:L6,K2+1:L2,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef513642(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K3,L3,K6,L6,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K3+1:L3,K6+1:L6,K4+1:L4,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef514236(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K4,L4,K2,L2,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef514263(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K4,L4,K2,L2,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K4+1:L4,K2+1:L2,K6+1:L6,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef514326(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K4,L4,K3,L3,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef514362(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K4,L4,K3,L3,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K4+1:L4,K3+1:L3,K6+1:L6,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef514623(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K4,L4,K6,L6,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K4+1:L4,K6+1:L6,K2+1:L2,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef514632(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K4,L4,K6,L6,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K4+1:L4,K6+1:L6,K3+1:L3,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef516234(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K6,L6,K2,L2,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K6+1:L6,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef516243(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K6,L6,K2,L2,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K6+1:L6,K2+1:L2,K4+1:L4,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef516324(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K6,L6,K3,L3,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K6+1:L6,K3+1:L3,K2+1:L2,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef516342(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K6,L6,K3,L3,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K6+1:L6,K3+1:L3,K4+1:L4,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef516423(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K6,L6,K4,L4,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K6+1:L6,K4+1:L4,K2+1:L2,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef516432(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K6,L6,K4,L4,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K6+1:L6,K4+1:L4,K3+1:L3,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef521346(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K1,L1,K3,L3,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef521364(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K1,L1,K3,L3,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K1+1:L1,K3+1:L3,K6+1:L6,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef521436(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K1,L1,K4,L4,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef521463(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K1,L1,K4,L4,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K1+1:L1,K4+1:L4,K6+1:L6,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef521634(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K1,L1,K6,L6,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K1+1:L1,K6+1:L6,K3+1:L3,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef521643(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K1,L1,K6,L6,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K1+1:L1,K6+1:L6,K4+1:L4,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef523146(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K3,L3,K1,L1,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef523164(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K3,L3,K1,L1,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K3+1:L3,K1+1:L1,K6+1:L6,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef523416(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K3,L3,K4,L4,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef523461(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K3,L3,K4,L4,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K3+1:L3,K4+1:L4,K6+1:L6,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef523614(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K3,L3,K6,L6,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K3+1:L3,K6+1:L6,K1+1:L1,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef523641(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K3,L3,K6,L6,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K3+1:L3,K6+1:L6,K4+1:L4,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef524136(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K4,L4,K1,L1,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef524163(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K4,L4,K1,L1,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K4+1:L4,K1+1:L1,K6+1:L6,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef524316(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K4,L4,K3,L3,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef524361(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K4,L4,K3,L3,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K4+1:L4,K3+1:L3,K6+1:L6,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef524613(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K4,L4,K6,L6,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K4+1:L4,K6+1:L6,K1+1:L1,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef524631(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K4,L4,K6,L6,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K4+1:L4,K6+1:L6,K3+1:L3,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef526134(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K6,L6,K1,L1,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K6+1:L6,K1+1:L1,K3+1:L3,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef526143(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K6,L6,K1,L1,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K6+1:L6,K1+1:L1,K4+1:L4,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef526314(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K6,L6,K3,L3,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K6+1:L6,K3+1:L3,K1+1:L1,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef526341(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K6,L6,K3,L3,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K6+1:L6,K3+1:L3,K4+1:L4,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef526413(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K6,L6,K4,L4,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K6+1:L6,K4+1:L4,K1+1:L1,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef526431(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K6,L6,K4,L4,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K6+1:L6,K4+1:L4,K3+1:L3,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef531246(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K1,L1,K2,L2,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef531264(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K1,L1,K2,L2,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K1+1:L1,K2+1:L2,K6+1:L6,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef531426(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K1,L1,K4,L4,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef531462(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K1,L1,K4,L4,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K1+1:L1,K4+1:L4,K6+1:L6,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef531624(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K1,L1,K6,L6,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K1+1:L1,K6+1:L6,K2+1:L2,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef531642(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K1,L1,K6,L6,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K1+1:L1,K6+1:L6,K4+1:L4,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef532146(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K2,L2,K1,L1,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef532164(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K2,L2,K1,L1,K6,L6,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K2+1:L2,K1+1:L1,K6+1:L6,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef532416(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K2,L2,K4,L4,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef532461(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K2,L2,K4,L4,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K2+1:L2,K4+1:L4,K6+1:L6,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef532614(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K2,L2,K6,L6,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K2+1:L2,K6+1:L6,K1+1:L1,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef532641(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K2,L2,K6,L6,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K2+1:L2,K6+1:L6,K4+1:L4,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef534126(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K4,L4,K1,L1,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef534162(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K4,L4,K1,L1,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K4+1:L4,K1+1:L1,K6+1:L6,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef534216(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K4,L4,K2,L2,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef534261(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K4,L4,K2,L2,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K4+1:L4,K2+1:L2,K6+1:L6,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef534612(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K4,L4,K6,L6,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K4+1:L4,K6+1:L6,K1+1:L1,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef534621(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K4,L4,K6,L6,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K4+1:L4,K6+1:L6,K2+1:L2,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef536124(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K6,L6,K1,L1,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K6+1:L6,K1+1:L1,K2+1:L2,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef536142(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K6,L6,K1,L1,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K6+1:L6,K1+1:L1,K4+1:L4,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef536214(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K6,L6,K2,L2,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K6+1:L6,K2+1:L2,K1+1:L1,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef536241(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K6,L6,K2,L2,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K6+1:L6,K2+1:L2,K4+1:L4,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef536412(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K6,L6,K4,L4,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K6+1:L6,K4+1:L4,K1+1:L1,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef536421(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K6,L6,K4,L4,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K6+1:L6,K4+1:L4,K2+1:L2,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef541236(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K1,L1,K2,L2,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef541263(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K1,L1,K2,L2,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K1+1:L1,K2+1:L2,K6+1:L6,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef541326(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K1,L1,K3,L3,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef541362(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K1,L1,K3,L3,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K1+1:L1,K3+1:L3,K6+1:L6,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef541623(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K1,L1,K6,L6,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K1+1:L1,K6+1:L6,K2+1:L2,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef541632(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K1,L1,K6,L6,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K1+1:L1,K6+1:L6,K3+1:L3,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef542136(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K2,L2,K1,L1,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef542163(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K2,L2,K1,L1,K6,L6,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K2+1:L2,K1+1:L1,K6+1:L6,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef542316(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K2,L2,K3,L3,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef542361(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K2,L2,K3,L3,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K2+1:L2,K3+1:L3,K6+1:L6,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef542613(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K2,L2,K6,L6,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K2+1:L2,K6+1:L6,K1+1:L1,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef542631(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K2,L2,K6,L6,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K2+1:L2,K6+1:L6,K3+1:L3,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef543126(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K3,L3,K1,L1,K2,L2,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef543162(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K3,L3,K1,L1,K6,L6,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K3+1:L3,K1+1:L1,K6+1:L6,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef543216(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K3,L3,K2,L2,K1,L1,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1,K6+1:L6)
       real*8 C
C
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
       end
C
       subroutine relef543261(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K3,L3,K2,L2,K6,L6,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K3+1:L3,K2+1:L2,K6+1:L6,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef543612(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K3,L3,K6,L6,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K3+1:L3,K6+1:L6,K1+1:L1,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef543621(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K3,L3,K6,L6,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K3+1:L3,K6+1:L6,K2+1:L2,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef546123(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K6,L6,K1,L1,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K6+1:L6,K1+1:L1,K2+1:L2,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef546132(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K6,L6,K1,L1,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K6+1:L6,K1+1:L1,K3+1:L3,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef546213(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K6,L6,K2,L2,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K6+1:L6,K2+1:L2,K1+1:L1,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef546231(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K6,L6,K2,L2,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K6+1:L6,K2+1:L2,K3+1:L3,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef546312(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K6,L6,K3,L3,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K6+1:L6,K3+1:L3,K1+1:L1,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef546321(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K6,L6,K3,L3,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K6+1:L6,K3+1:L3,K2+1:L2,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef561234(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K1,L1,K2,L2,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef561243(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K1,L1,K2,L2,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef561324(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K1,L1,K3,L3,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef561342(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K1,L1,K3,L3,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef561423(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K1,L1,K4,L4,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef561432(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K1,L1,K4,L4,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef562134(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K2,L2,K1,L1,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef562143(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K2,L2,K1,L1,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef562314(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K2,L2,K3,L3,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef562341(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K2,L2,K3,L3,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef562413(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K2,L2,K4,L4,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef562431(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K2,L2,K4,L4,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef563124(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K3,L3,K1,L1,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef563142(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K3,L3,K1,L1,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef563214(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K3,L3,K2,L2,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef563241(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K3,L3,K2,L2,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef563412(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K3,L3,K4,L4,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef563421(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K3,L3,K4,L4,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef564123(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K4,L4,K1,L1,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef564132(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K4,L4,K1,L1,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef564213(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K4,L4,K2,L2,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef564231(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K4,L4,K2,L2,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef564312(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K4,L4,K3,L3,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef564321(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K4,L4,K3,L3,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef612345(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef612354(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K2,L2,K3,L3,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K2+1:L2,K3+1:L3,K5+1:L5,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef612435(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K2,L2,K4,L4,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef612453(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K2,L2,K4,L4,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K2+1:L2,K4+1:L4,K5+1:L5,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef612534(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K2,L2,K5,L5,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K2+1:L2,K5+1:L5,K3+1:L3,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef612543(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K2,L2,K5,L5,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K2+1:L2,K5+1:L5,K4+1:L4,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef613245(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K3,L3,K2,L2,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef613254(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K3,L3,K2,L2,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K3+1:L3,K2+1:L2,K5+1:L5,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef613425(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K3,L3,K4,L4,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef613452(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K3,L3,K4,L4,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K3+1:L3,K4+1:L4,K5+1:L5,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef613524(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K3,L3,K5,L5,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K3+1:L3,K5+1:L5,K2+1:L2,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef613542(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K3,L3,K5,L5,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K3+1:L3,K5+1:L5,K4+1:L4,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef614235(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K4,L4,K2,L2,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef614253(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K4,L4,K2,L2,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K4+1:L4,K2+1:L2,K5+1:L5,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef614325(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K4,L4,K3,L3,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef614352(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K4,L4,K3,L3,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K4+1:L4,K3+1:L3,K5+1:L5,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef614523(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K4,L4,K5,L5,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K4+1:L4,K5+1:L5,K2+1:L2,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef614532(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K4,L4,K5,L5,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K4+1:L4,K5+1:L5,K3+1:L3,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef615234(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K5,L5,K2,L2,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K5+1:L5,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef615243(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K5,L5,K2,L2,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K5+1:L5,K2+1:L2,K4+1:L4,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef615324(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K5,L5,K3,L3,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K5+1:L5,K3+1:L3,K2+1:L2,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef615342(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K5,L5,K3,L3,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K5+1:L5,K3+1:L3,K4+1:L4,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef615423(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K5,L5,K4,L4,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K5+1:L5,K4+1:L4,K2+1:L2,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef615432(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K5,L5,K4,L4,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K5+1:L5,K4+1:L4,K3+1:L3,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef621345(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K1,L1,K3,L3,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef621354(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K1,L1,K3,L3,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K1+1:L1,K3+1:L3,K5+1:L5,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef621435(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K1,L1,K4,L4,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef621453(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K1,L1,K4,L4,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K1+1:L1,K4+1:L4,K5+1:L5,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef621534(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K1,L1,K5,L5,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K1+1:L1,K5+1:L5,K3+1:L3,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef621543(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K1,L1,K5,L5,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K1+1:L1,K5+1:L5,K4+1:L4,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef623145(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K3,L3,K1,L1,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef623154(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K3,L3,K1,L1,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K3+1:L3,K1+1:L1,K5+1:L5,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef623415(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K3,L3,K4,L4,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef623451(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K3,L3,K4,L4,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef623514(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K3,L3,K5,L5,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K3+1:L3,K5+1:L5,K1+1:L1,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef623541(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K3,L3,K5,L5,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K3+1:L3,K5+1:L5,K4+1:L4,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef624135(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K4,L4,K1,L1,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef624153(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K4,L4,K1,L1,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K4+1:L4,K1+1:L1,K5+1:L5,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef624315(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K4,L4,K3,L3,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef624351(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K4,L4,K3,L3,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K4+1:L4,K3+1:L3,K5+1:L5,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef624513(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K4,L4,K5,L5,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K4+1:L4,K5+1:L5,K1+1:L1,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef624531(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K4,L4,K5,L5,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K4+1:L4,K5+1:L5,K3+1:L3,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef625134(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K5,L5,K1,L1,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K5+1:L5,K1+1:L1,K3+1:L3,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef625143(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K5,L5,K1,L1,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K5+1:L5,K1+1:L1,K4+1:L4,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef625314(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K5,L5,K3,L3,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K5+1:L5,K3+1:L3,K1+1:L1,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef625341(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K5,L5,K3,L3,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K5+1:L5,K3+1:L3,K4+1:L4,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef625413(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K5,L5,K4,L4,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K5+1:L5,K4+1:L4,K1+1:L1,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef625431(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K5,L5,K4,L4,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K5+1:L5,K4+1:L4,K3+1:L3,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef631245(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K1,L1,K2,L2,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef631254(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K1,L1,K2,L2,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K1+1:L1,K2+1:L2,K5+1:L5,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef631425(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K1,L1,K4,L4,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef631452(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K1,L1,K4,L4,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef631524(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K1,L1,K5,L5,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K1+1:L1,K5+1:L5,K2+1:L2,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef631542(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K1,L1,K5,L5,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K1+1:L1,K5+1:L5,K4+1:L4,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef632145(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K2,L2,K1,L1,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef632154(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K2,L2,K1,L1,K5,L5,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K2+1:L2,K1+1:L1,K5+1:L5,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef632415(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K2,L2,K4,L4,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef632451(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K2,L2,K4,L4,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K2+1:L2,K4+1:L4,K5+1:L5,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef632514(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K2,L2,K5,L5,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K2+1:L2,K5+1:L5,K1+1:L1,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef632541(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K2,L2,K5,L5,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K2+1:L2,K5+1:L5,K4+1:L4,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef634125(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K4,L4,K1,L1,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef634152(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K4,L4,K1,L1,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K4+1:L4,K1+1:L1,K5+1:L5,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef634215(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K4,L4,K2,L2,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef634251(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K4,L4,K2,L2,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K4+1:L4,K2+1:L2,K5+1:L5,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef634512(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K4,L4,K5,L5,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K4+1:L4,K5+1:L5,K1+1:L1,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef634521(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K4,L4,K5,L5,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K4+1:L4,K5+1:L5,K2+1:L2,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef635124(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K5,L5,K1,L1,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K5+1:L5,K1+1:L1,K2+1:L2,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef635142(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K5,L5,K1,L1,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K5+1:L5,K1+1:L1,K4+1:L4,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef635214(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K5,L5,K2,L2,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K5+1:L5,K2+1:L2,K1+1:L1,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef635241(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K5,L5,K2,L2,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K5+1:L5,K2+1:L2,K4+1:L4,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef635412(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K5,L5,K4,L4,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K5+1:L5,K4+1:L4,K1+1:L1,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef635421(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K5,L5,K4,L4,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K5+1:L5,K4+1:L4,K2+1:L2,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef641235(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K1,L1,K2,L2,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef641253(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K1,L1,K2,L2,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K1+1:L1,K2+1:L2,K5+1:L5,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef641325(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K1,L1,K3,L3,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef641352(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K1,L1,K3,L3,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K1+1:L1,K3+1:L3,K5+1:L5,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef641523(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K1,L1,K5,L5,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K1+1:L1,K5+1:L5,K2+1:L2,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef641532(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K1,L1,K5,L5,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K1+1:L1,K5+1:L5,K3+1:L3,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef642135(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K2,L2,K1,L1,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef642153(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K2,L2,K1,L1,K5,L5,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K2+1:L2,K1+1:L1,K5+1:L5,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef642315(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K2,L2,K3,L3,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef642351(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K2,L2,K3,L3,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K2+1:L2,K3+1:L3,K5+1:L5,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef642513(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K2,L2,K5,L5,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K2+1:L2,K5+1:L5,K1+1:L1,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef642531(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K2,L2,K5,L5,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K2+1:L2,K5+1:L5,K3+1:L3,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef643125(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K3,L3,K1,L1,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef643152(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K3,L3,K1,L1,K5,L5,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K3+1:L3,K1+1:L1,K5+1:L5,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef643215(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K3,L3,K2,L2,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1,K5+1:L5)
       real*8 C
C
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
       end
C
       subroutine relef643251(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K3,L3,K2,L2,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K3+1:L3,K2+1:L2,K5+1:L5,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef643512(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K3,L3,K5,L5,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K3+1:L3,K5+1:L5,K1+1:L1,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef643521(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K3,L3,K5,L5,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K3+1:L3,K5+1:L5,K2+1:L2,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef645123(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K5,L5,K1,L1,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef645132(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K5,L5,K1,L1,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K5+1:L5,K1+1:L1,K3+1:L3,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef645213(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K5,L5,K2,L2,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K5+1:L5,K2+1:L2,K1+1:L1,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef645231(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K5,L5,K2,L2,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K5+1:L5,K2+1:L2,K3+1:L3,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef645312(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K5,L5,K3,L3,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K5+1:L5,K3+1:L3,K1+1:L1,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef645321(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K5,L5,K3,L3,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K5+1:L5,K3+1:L3,K2+1:L2,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef651234(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K1,L1,K2,L2,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef651243(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K1,L1,K2,L2,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef651324(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K1,L1,K3,L3,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef651342(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K1,L1,K3,L3,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef651423(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K1,L1,K4,L4,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef651432(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K1,L1,K4,L4,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef652134(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K2,L2,K1,L1,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef652143(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K2,L2,K1,L1,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef652314(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K2,L2,K3,L3,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef652341(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K2,L2,K3,L3,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef652413(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K2,L2,K4,L4,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef652431(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K2,L2,K4,L4,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef653124(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K3,L3,K1,L1,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef653142(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K3,L3,K1,L1,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef653214(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K3,L3,K2,L2,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4)
       real*8 C
C
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
       end
C
       subroutine relef653241(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K3,L3,K2,L2,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef653412(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K3,L3,K4,L4,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef653421(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K3,L3,K4,L4,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef654123(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K4,L4,K1,L1,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef654132(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K4,L4,K1,L1,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef654213(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K4,L4,K2,L2,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3)
       real*8 C
C
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
       end
C
       subroutine relef654231(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K4,L4,K2,L2,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1)
       real*8 C
C
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
       end
C
       subroutine relef654312(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K4,L4,K3,L3,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2)
       real*8 C
C
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
       end
C
       subroutine relef654321(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K4,L4,K3,L3,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1)
       real*8 C
C
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
       end
C
