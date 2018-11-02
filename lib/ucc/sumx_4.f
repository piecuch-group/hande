       subroutine sumx1234(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx1243(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I1,I2,I4,I3)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx1324(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I1,I3,I2,I4)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx1342(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I1,I3,I4,I2)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx1423(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I1,I4,I2,I3)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx1432(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I1,I4,I3,I2)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx2134(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I2,I1,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx2143(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I2,I1,I4,I3)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx2314(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I2,I3,I1,I4)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx2341(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I2,I3,I4,I1)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx2413(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I2,I4,I1,I3)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx2431(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I2,I4,I3,I1)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx3124(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I3,I1,I2,I4)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx3142(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I3,I1,I4,I2)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx3214(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I3,I2,I1,I4)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx3241(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I3,I2,I4,I1)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx3412(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I3,I4,I1,I2)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx3421(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I3,I4,I2,I1)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx4123(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I4,I1,I2,I3)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx4132(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I4,I1,I3,I2)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx4213(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I4,I2,I1,I3)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx4231(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I4,I2,I3,I1)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx4312(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I4,I3,I1,I2)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx4321(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I4,I3,I2,I1)
       enddo
       enddo
       enddo
       enddo
C
       end
C
