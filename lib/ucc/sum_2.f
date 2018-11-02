       subroutine sum12(K1,L1,K2,L2,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2)
       real*8 B(K1+1:L1,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
        A(I1,I2)=A(I1,I2)+C*B(I1,I2)
       enddo
       enddo
C
       end
C
       subroutine sum21(K1,L1,K2,L2,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2)
       real*8 B(K2+1:L2,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
        A(I1,I2)=A(I1,I2)+C*B(I2,I1)
       enddo
       enddo
C
       end
C
