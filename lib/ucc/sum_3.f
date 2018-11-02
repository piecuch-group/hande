       subroutine sum123(K1,L1,K2,L2,K3,L3,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
        A(I1,I2,I3)=A(I1,I2,I3)+C*B(I1,I2,I3)
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum132(K1,L1,K2,L2,K3,L3,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
        A(I1,I2,I3)=A(I1,I2,I3)+C*B(I1,I3,I2)
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum213(K1,L1,K2,L2,K3,L3,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
        A(I1,I2,I3)=A(I1,I2,I3)+C*B(I2,I1,I3)
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum231(K1,L1,K2,L2,K3,L3,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
        A(I1,I2,I3)=A(I1,I2,I3)+C*B(I2,I3,I1)
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum312(K1,L1,K2,L2,K3,L3,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
        A(I1,I2,I3)=A(I1,I2,I3)+C*B(I3,I1,I2)
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum321(K1,L1,K2,L2,K3,L3,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
        A(I1,I2,I3)=A(I1,I2,I3)+C*B(I3,I2,I1)
       enddo
       enddo
       enddo
C
       end
C
