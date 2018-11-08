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
       subroutine sum1234(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
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
       subroutine sum1243(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3)
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
       subroutine sum1324(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4)
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
       subroutine sum1342(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2)
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
       subroutine sum1423(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3)
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
       subroutine sum1432(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2)
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
       subroutine sum2134(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4)
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
       subroutine sum2143(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3)
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
       subroutine sum2314(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4)
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
       subroutine sum2341(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1)
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
       subroutine sum2413(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3)
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
       subroutine sum2431(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1)
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
       subroutine sum3124(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4)
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
       subroutine sum3142(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2)
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
       subroutine sum3214(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4)
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
       subroutine sum3241(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1)
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
       subroutine sum3412(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2)
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
       subroutine sum3421(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1)
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
       subroutine sum4123(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3)
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
       subroutine sum4132(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2)
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
       subroutine sum4213(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3)
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
       subroutine sum4231(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1)
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
       subroutine sum4312(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2)
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
       subroutine sum4321(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1)
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
       subroutine sum12345(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum12354(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K5+1:L5,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I1,I2,I3,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum12435(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I1,I2,I4,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum12453(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K5+1:L5,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I1,I2,I4,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum12534(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K1+1:L1,K2+1:L2,K5+1:L5,K3+1:L3,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I1,I2,I5,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum12543(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K1+1:L1,K2+1:L2,K5+1:L5,K4+1:L4,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I1,I2,I5,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum13245(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I1,I3,I2,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum13254(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K5+1:L5,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I1,I3,I2,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum13425(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I1,I3,I4,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum13452(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K5+1:L5,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I1,I3,I4,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum13524(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K1+1:L1,K3+1:L3,K5+1:L5,K2+1:L2,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I1,I3,I5,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum13542(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K1+1:L1,K3+1:L3,K5+1:L5,K4+1:L4,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I1,I3,I5,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum14235(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I1,I4,I2,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum14253(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K5+1:L5,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I1,I4,I2,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum14325(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I1,I4,I3,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum14352(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K5+1:L5,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I1,I4,I3,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum14523(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K1+1:L1,K4+1:L4,K5+1:L5,K2+1:L2,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I1,I4,I5,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum14532(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K1+1:L1,K4+1:L4,K5+1:L5,K3+1:L3,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I1,I4,I5,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum15234(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K1+1:L1,K5+1:L5,K2+1:L2,K3+1:L3,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I1,I5,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum15243(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K1+1:L1,K5+1:L5,K2+1:L2,K4+1:L4,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I1,I5,I2,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum15324(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K1+1:L1,K5+1:L5,K3+1:L3,K2+1:L2,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I1,I5,I3,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum15342(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K1+1:L1,K5+1:L5,K3+1:L3,K4+1:L4,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I1,I5,I3,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum15423(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K1+1:L1,K5+1:L5,K4+1:L4,K2+1:L2,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I1,I5,I4,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum15432(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K1+1:L1,K5+1:L5,K4+1:L4,K3+1:L3,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I1,I5,I4,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum21345(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I2,I1,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum21354(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K5+1:L5,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I2,I1,I3,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum21435(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I2,I1,I4,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum21453(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K5+1:L5,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I2,I1,I4,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum21534(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K2+1:L2,K1+1:L1,K5+1:L5,K3+1:L3,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I2,I1,I5,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum21543(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K2+1:L2,K1+1:L1,K5+1:L5,K4+1:L4,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I2,I1,I5,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum23145(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I2,I3,I1,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum23154(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K5+1:L5,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I2,I3,I1,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum23415(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I2,I3,I4,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum23451(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I2,I3,I4,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum23514(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K2+1:L2,K3+1:L3,K5+1:L5,K1+1:L1,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I2,I3,I5,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum23541(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K2+1:L2,K3+1:L3,K5+1:L5,K4+1:L4,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I2,I3,I5,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum24135(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I2,I4,I1,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum24153(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K5+1:L5,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I2,I4,I1,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum24315(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I2,I4,I3,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum24351(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K5+1:L5,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I2,I4,I3,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum24513(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K2+1:L2,K4+1:L4,K5+1:L5,K1+1:L1,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I2,I4,I5,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum24531(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K2+1:L2,K4+1:L4,K5+1:L5,K3+1:L3,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I2,I4,I5,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum25134(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K2+1:L2,K5+1:L5,K1+1:L1,K3+1:L3,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I2,I5,I1,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum25143(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K2+1:L2,K5+1:L5,K1+1:L1,K4+1:L4,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I2,I5,I1,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum25314(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K2+1:L2,K5+1:L5,K3+1:L3,K1+1:L1,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I2,I5,I3,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum25341(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K2+1:L2,K5+1:L5,K3+1:L3,K4+1:L4,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I2,I5,I3,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum25413(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K2+1:L2,K5+1:L5,K4+1:L4,K1+1:L1,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I2,I5,I4,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum25431(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K2+1:L2,K5+1:L5,K4+1:L4,K3+1:L3,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I2,I5,I4,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum31245(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I3,I1,I2,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum31254(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K5+1:L5,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I3,I1,I2,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum31425(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I3,I1,I4,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum31452(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I3,I1,I4,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum31524(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K3+1:L3,K1+1:L1,K5+1:L5,K2+1:L2,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I3,I1,I5,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum31542(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K3+1:L3,K1+1:L1,K5+1:L5,K4+1:L4,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I3,I1,I5,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum32145(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I3,I2,I1,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum32154(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K5+1:L5,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I3,I2,I1,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum32415(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I3,I2,I4,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum32451(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K5+1:L5,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I3,I2,I4,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum32514(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K3+1:L3,K2+1:L2,K5+1:L5,K1+1:L1,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I3,I2,I5,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum32541(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K3+1:L3,K2+1:L2,K5+1:L5,K4+1:L4,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I3,I2,I5,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum34125(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I3,I4,I1,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum34152(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K5+1:L5,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I3,I4,I1,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum34215(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I3,I4,I2,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum34251(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K5+1:L5,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I3,I4,I2,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum34512(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K3+1:L3,K4+1:L4,K5+1:L5,K1+1:L1,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I3,I4,I5,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum34521(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K3+1:L3,K4+1:L4,K5+1:L5,K2+1:L2,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I3,I4,I5,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum35124(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K3+1:L3,K5+1:L5,K1+1:L1,K2+1:L2,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I3,I5,I1,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum35142(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K3+1:L3,K5+1:L5,K1+1:L1,K4+1:L4,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I3,I5,I1,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum35214(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K3+1:L3,K5+1:L5,K2+1:L2,K1+1:L1,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I3,I5,I2,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum35241(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K3+1:L3,K5+1:L5,K2+1:L2,K4+1:L4,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I3,I5,I2,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum35412(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K3+1:L3,K5+1:L5,K4+1:L4,K1+1:L1,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I3,I5,I4,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum35421(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K3+1:L3,K5+1:L5,K4+1:L4,K2+1:L2,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I3,I5,I4,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum41235(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I4,I1,I2,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum41253(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K5+1:L5,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I4,I1,I2,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum41325(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I4,I1,I3,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum41352(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K5+1:L5,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I4,I1,I3,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum41523(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K4+1:L4,K1+1:L1,K5+1:L5,K2+1:L2,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I4,I1,I5,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum41532(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K4+1:L4,K1+1:L1,K5+1:L5,K3+1:L3,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I4,I1,I5,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum42135(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I4,I2,I1,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum42153(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K5+1:L5,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I4,I2,I1,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum42315(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I4,I2,I3,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum42351(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K5+1:L5,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I4,I2,I3,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum42513(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K4+1:L4,K2+1:L2,K5+1:L5,K1+1:L1,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I4,I2,I5,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum42531(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K4+1:L4,K2+1:L2,K5+1:L5,K3+1:L3,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I4,I2,I5,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum43125(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I4,I3,I1,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum43152(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K5+1:L5,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I4,I3,I1,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum43215(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I4,I3,I2,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum43251(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K5+1:L5,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I4,I3,I2,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum43512(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K4+1:L4,K3+1:L3,K5+1:L5,K1+1:L1,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I4,I3,I5,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum43521(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K4+1:L4,K3+1:L3,K5+1:L5,K2+1:L2,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I4,I3,I5,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum45123(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K4+1:L4,K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I4,I5,I1,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum45132(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K4+1:L4,K5+1:L5,K1+1:L1,K3+1:L3,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I4,I5,I1,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum45213(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K4+1:L4,K5+1:L5,K2+1:L2,K1+1:L1,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I4,I5,I2,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum45231(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K4+1:L4,K5+1:L5,K2+1:L2,K3+1:L3,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I4,I5,I2,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum45312(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K4+1:L4,K5+1:L5,K3+1:L3,K1+1:L1,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I4,I5,I3,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum45321(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K4+1:L4,K5+1:L5,K3+1:L3,K2+1:L2,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I4,I5,I3,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum51234(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I5,I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum51243(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K5+1:L5,K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I5,I1,I2,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum51324(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K5+1:L5,K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I5,I1,I3,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum51342(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K5+1:L5,K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I5,I1,I3,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum51423(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K5+1:L5,K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I5,I1,I4,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum51432(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K5+1:L5,K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I5,I1,I4,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum52134(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K5+1:L5,K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I5,I2,I1,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum52143(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K5+1:L5,K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I5,I2,I1,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum52314(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K5+1:L5,K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I5,I2,I3,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum52341(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K5+1:L5,K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I5,I2,I3,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum52413(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K5+1:L5,K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I5,I2,I4,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum52431(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K5+1:L5,K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I5,I2,I4,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum53124(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K5+1:L5,K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I5,I3,I1,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum53142(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K5+1:L5,K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I5,I3,I1,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum53214(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K5+1:L5,K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I5,I3,I2,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum53241(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K5+1:L5,K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I5,I3,I2,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum53412(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K5+1:L5,K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I5,I3,I4,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum53421(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K5+1:L5,K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I5,I3,I4,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum54123(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K5+1:L5,K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I5,I4,I1,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum54132(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K5+1:L5,K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I5,I4,I1,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum54213(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K5+1:L5,K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I5,I4,I2,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum54231(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K5+1:L5,K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I5,I4,I2,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum54312(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K5+1:L5,K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I5,I4,I3,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum54321(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 B(K5+1:L5,K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
        A(I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5)+C*B(I5,I4,I3,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum123456(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum123465(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K6+1:L6,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I3,I4,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum123546(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K5+1:L5,K4+1:L4,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I3,I5,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum123564(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K5+1:L5,K6+1:L6,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I3,I5,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum123645(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K6+1:L6,K4+1:L4,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I3,I6,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum123654(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K6+1:L6,K5+1:L5,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I3,I6,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum124356(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3,K5+1:L5,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I4,I3,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum124365(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3,K6+1:L6,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I4,I3,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum124536(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K5+1:L5,K3+1:L3,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I4,I5,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum124563(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K5+1:L5,K6+1:L6,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I4,I5,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum124635(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K6+1:L6,K3+1:L3,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I4,I6,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum124653(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K6+1:L6,K5+1:L5,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I4,I6,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum125346(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K5+1:L5,K3+1:L3,K4+1:L4,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I5,I3,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum125364(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K5+1:L5,K3+1:L3,K6+1:L6,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I5,I3,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum125436(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K5+1:L5,K4+1:L4,K3+1:L3,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I5,I4,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum125463(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K5+1:L5,K4+1:L4,K6+1:L6,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I5,I4,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum125634(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K5+1:L5,K6+1:L6,K3+1:L3,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I5,I6,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum125643(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K5+1:L5,K6+1:L6,K4+1:L4,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I5,I6,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum126345(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K6+1:L6,K3+1:L3,K4+1:L4,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I6,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum126354(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K6+1:L6,K3+1:L3,K5+1:L5,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I6,I3,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum126435(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K6+1:L6,K4+1:L4,K3+1:L3,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I6,I4,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum126453(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K6+1:L6,K4+1:L4,K5+1:L5,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I6,I4,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum126534(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K6+1:L6,K5+1:L5,K3+1:L3,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I6,I5,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum126543(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K6+1:L6,K5+1:L5,K4+1:L4,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I6,I5,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum132456(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4,K5+1:L5,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I2,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum132465(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4,K6+1:L6,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I2,I4,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum132546(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K5+1:L5,K4+1:L4,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I2,I5,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum132564(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K5+1:L5,K6+1:L6,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I2,I5,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum132645(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K6+1:L6,K4+1:L4,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I2,I6,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum132654(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K6+1:L6,K5+1:L5,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I2,I6,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum134256(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2,K5+1:L5,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I4,I2,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum134265(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2,K6+1:L6,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I4,I2,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum134526(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K5+1:L5,K2+1:L2,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I4,I5,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum134562(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I4,I5,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum134625(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K6+1:L6,K2+1:L2,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I4,I6,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum134652(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K6+1:L6,K5+1:L5,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I4,I6,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum135246(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K5+1:L5,K2+1:L2,K4+1:L4,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I5,I2,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum135264(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K5+1:L5,K2+1:L2,K6+1:L6,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I5,I2,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum135426(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K5+1:L5,K4+1:L4,K2+1:L2,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I5,I4,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum135462(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K5+1:L5,K4+1:L4,K6+1:L6,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I5,I4,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum135624(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K5+1:L5,K6+1:L6,K2+1:L2,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I5,I6,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum135642(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K5+1:L5,K6+1:L6,K4+1:L4,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I5,I6,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum136245(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K6+1:L6,K2+1:L2,K4+1:L4,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I6,I2,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum136254(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K6+1:L6,K2+1:L2,K5+1:L5,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I6,I2,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum136425(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K6+1:L6,K4+1:L4,K2+1:L2,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I6,I4,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum136452(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K6+1:L6,K4+1:L4,K5+1:L5,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I6,I4,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum136524(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K6+1:L6,K5+1:L5,K2+1:L2,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I6,I5,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum136542(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K6+1:L6,K5+1:L5,K4+1:L4,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I6,I5,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum142356(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3,K5+1:L5,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I2,I3,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum142365(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3,K6+1:L6,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I2,I3,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum142536(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K5+1:L5,K3+1:L3,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I2,I5,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum142563(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K5+1:L5,K6+1:L6,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I2,I5,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum142635(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K6+1:L6,K3+1:L3,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I2,I6,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum142653(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K6+1:L6,K5+1:L5,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I2,I6,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum143256(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2,K5+1:L5,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I3,I2,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum143265(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2,K6+1:L6,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I3,I2,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum143526(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K5+1:L5,K2+1:L2,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I3,I5,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum143562(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K5+1:L5,K6+1:L6,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I3,I5,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum143625(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K6+1:L6,K2+1:L2,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I3,I6,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum143652(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K6+1:L6,K5+1:L5,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I3,I6,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum145236(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K5+1:L5,K2+1:L2,K3+1:L3,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I5,I2,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum145263(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K5+1:L5,K2+1:L2,K6+1:L6,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I5,I2,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum145326(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K5+1:L5,K3+1:L3,K2+1:L2,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I5,I3,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum145362(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K5+1:L5,K3+1:L3,K6+1:L6,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I5,I3,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum145623(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K5+1:L5,K6+1:L6,K2+1:L2,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I5,I6,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum145632(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K5+1:L5,K6+1:L6,K3+1:L3,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I5,I6,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum146235(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K6+1:L6,K2+1:L2,K3+1:L3,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I6,I2,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum146253(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K6+1:L6,K2+1:L2,K5+1:L5,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I6,I2,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum146325(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K6+1:L6,K3+1:L3,K2+1:L2,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I6,I3,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum146352(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K6+1:L6,K3+1:L3,K5+1:L5,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I6,I3,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum146523(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K6+1:L6,K5+1:L5,K2+1:L2,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I6,I5,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum146532(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K6+1:L6,K5+1:L5,K3+1:L3,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I6,I5,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum152346(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K2+1:L2,K3+1:L3,K4+1:L4,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I2,I3,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum152364(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K2+1:L2,K3+1:L3,K6+1:L6,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I2,I3,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum152436(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K2+1:L2,K4+1:L4,K3+1:L3,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I2,I4,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum152463(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K2+1:L2,K4+1:L4,K6+1:L6,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I2,I4,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum152634(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K2+1:L2,K6+1:L6,K3+1:L3,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I2,I6,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum152643(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K2+1:L2,K6+1:L6,K4+1:L4,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I2,I6,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum153246(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K3+1:L3,K2+1:L2,K4+1:L4,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I3,I2,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum153264(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K3+1:L3,K2+1:L2,K6+1:L6,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I3,I2,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum153426(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K3+1:L3,K4+1:L4,K2+1:L2,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I3,I4,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum153462(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K3+1:L3,K4+1:L4,K6+1:L6,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I3,I4,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum153624(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K3+1:L3,K6+1:L6,K2+1:L2,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I3,I6,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum153642(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K3+1:L3,K6+1:L6,K4+1:L4,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I3,I6,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum154236(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K4+1:L4,K2+1:L2,K3+1:L3,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I4,I2,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum154263(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K4+1:L4,K2+1:L2,K6+1:L6,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I4,I2,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum154326(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K4+1:L4,K3+1:L3,K2+1:L2,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I4,I3,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum154362(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K4+1:L4,K3+1:L3,K6+1:L6,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I4,I3,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum154623(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K4+1:L4,K6+1:L6,K2+1:L2,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I4,I6,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum154632(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K4+1:L4,K6+1:L6,K3+1:L3,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I4,I6,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum156234(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K6+1:L6,K2+1:L2,K3+1:L3,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I6,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum156243(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K6+1:L6,K2+1:L2,K4+1:L4,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I6,I2,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum156324(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K6+1:L6,K3+1:L3,K2+1:L2,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I6,I3,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum156342(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K6+1:L6,K3+1:L3,K4+1:L4,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I6,I3,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum156423(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K6+1:L6,K4+1:L4,K2+1:L2,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I6,I4,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum156432(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K6+1:L6,K4+1:L4,K3+1:L3,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I6,I4,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum162345(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum162354(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K2+1:L2,K3+1:L3,K5+1:L5,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I2,I3,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum162435(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K2+1:L2,K4+1:L4,K3+1:L3,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I2,I4,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum162453(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K2+1:L2,K4+1:L4,K5+1:L5,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I2,I4,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum162534(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K2+1:L2,K5+1:L5,K3+1:L3,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I2,I5,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum162543(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K2+1:L2,K5+1:L5,K4+1:L4,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I2,I5,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum163245(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K3+1:L3,K2+1:L2,K4+1:L4,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I3,I2,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum163254(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K3+1:L3,K2+1:L2,K5+1:L5,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I3,I2,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum163425(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K3+1:L3,K4+1:L4,K2+1:L2,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I3,I4,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum163452(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K3+1:L3,K4+1:L4,K5+1:L5,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I3,I4,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum163524(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K3+1:L3,K5+1:L5,K2+1:L2,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I3,I5,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum163542(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K3+1:L3,K5+1:L5,K4+1:L4,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I3,I5,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum164235(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K4+1:L4,K2+1:L2,K3+1:L3,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I4,I2,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum164253(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K4+1:L4,K2+1:L2,K5+1:L5,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I4,I2,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum164325(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K4+1:L4,K3+1:L3,K2+1:L2,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I4,I3,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum164352(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K4+1:L4,K3+1:L3,K5+1:L5,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I4,I3,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum164523(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K4+1:L4,K5+1:L5,K2+1:L2,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I4,I5,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum164532(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K4+1:L4,K5+1:L5,K3+1:L3,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I4,I5,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum165234(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K5+1:L5,K2+1:L2,K3+1:L3,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I5,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum165243(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K5+1:L5,K2+1:L2,K4+1:L4,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I5,I2,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum165324(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K5+1:L5,K3+1:L3,K2+1:L2,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I5,I3,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum165342(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K5+1:L5,K3+1:L3,K4+1:L4,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I5,I3,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum165423(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K5+1:L5,K4+1:L4,K2+1:L2,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I5,I4,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum165432(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K5+1:L5,K4+1:L4,K3+1:L3,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I5,I4,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum213456(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum213465(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4,K6+1:L6,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I3,I4,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum213546(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K5+1:L5,K4+1:L4,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I3,I5,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum213564(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K5+1:L5,K6+1:L6,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I3,I5,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum213645(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K6+1:L6,K4+1:L4,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I3,I6,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum213654(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K6+1:L6,K5+1:L5,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I3,I6,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum214356(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3,K5+1:L5,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I4,I3,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum214365(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3,K6+1:L6,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I4,I3,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum214536(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K5+1:L5,K3+1:L3,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I4,I5,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum214563(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K5+1:L5,K6+1:L6,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I4,I5,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum214635(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K6+1:L6,K3+1:L3,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I4,I6,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum214653(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K6+1:L6,K5+1:L5,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I4,I6,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum215346(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K5+1:L5,K3+1:L3,K4+1:L4,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I5,I3,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum215364(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K5+1:L5,K3+1:L3,K6+1:L6,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I5,I3,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum215436(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K5+1:L5,K4+1:L4,K3+1:L3,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I5,I4,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum215463(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K5+1:L5,K4+1:L4,K6+1:L6,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I5,I4,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum215634(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K5+1:L5,K6+1:L6,K3+1:L3,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I5,I6,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum215643(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K5+1:L5,K6+1:L6,K4+1:L4,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I5,I6,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum216345(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K6+1:L6,K3+1:L3,K4+1:L4,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I6,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum216354(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K6+1:L6,K3+1:L3,K5+1:L5,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I6,I3,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum216435(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K6+1:L6,K4+1:L4,K3+1:L3,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I6,I4,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum216453(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K6+1:L6,K4+1:L4,K5+1:L5,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I6,I4,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum216534(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K6+1:L6,K5+1:L5,K3+1:L3,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I6,I5,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum216543(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K6+1:L6,K5+1:L5,K4+1:L4,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I6,I5,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum231456(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I1,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum231465(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4,K6+1:L6,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I1,I4,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum231546(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K5+1:L5,K4+1:L4,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I1,I5,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum231564(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K5+1:L5,K6+1:L6,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I1,I5,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum231645(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K6+1:L6,K4+1:L4,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I1,I6,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum231654(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K6+1:L6,K5+1:L5,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I1,I6,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum234156(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1,K5+1:L5,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I4,I1,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum234165(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1,K6+1:L6,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I4,I1,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum234516(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K1+1:L1,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I4,I5,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum234561(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I4,I5,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum234615(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K6+1:L6,K1+1:L1,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I4,I6,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum234651(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K6+1:L6,K5+1:L5,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I4,I6,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum235146(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K5+1:L5,K1+1:L1,K4+1:L4,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I5,I1,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum235164(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K5+1:L5,K1+1:L1,K6+1:L6,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I5,I1,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum235416(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K5+1:L5,K4+1:L4,K1+1:L1,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I5,I4,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum235461(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K5+1:L5,K4+1:L4,K6+1:L6,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I5,I4,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum235614(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K5+1:L5,K6+1:L6,K1+1:L1,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I5,I6,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum235641(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K5+1:L5,K6+1:L6,K4+1:L4,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I5,I6,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum236145(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K6+1:L6,K1+1:L1,K4+1:L4,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I6,I1,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum236154(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K6+1:L6,K1+1:L1,K5+1:L5,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I6,I1,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum236415(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K6+1:L6,K4+1:L4,K1+1:L1,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I6,I4,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum236451(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K6+1:L6,K4+1:L4,K5+1:L5,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I6,I4,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum236514(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K6+1:L6,K5+1:L5,K1+1:L1,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I6,I5,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum236541(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K6+1:L6,K5+1:L5,K4+1:L4,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I6,I5,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum241356(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3,K5+1:L5,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I1,I3,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum241365(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3,K6+1:L6,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I1,I3,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum241536(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K5+1:L5,K3+1:L3,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I1,I5,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum241563(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K5+1:L5,K6+1:L6,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I1,I5,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum241635(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K6+1:L6,K3+1:L3,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I1,I6,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum241653(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K6+1:L6,K5+1:L5,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I1,I6,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum243156(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1,K5+1:L5,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I3,I1,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum243165(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1,K6+1:L6,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I3,I1,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum243516(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K5+1:L5,K1+1:L1,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I3,I5,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum243561(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K5+1:L5,K6+1:L6,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I3,I5,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum243615(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K6+1:L6,K1+1:L1,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I3,I6,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum243651(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K6+1:L6,K5+1:L5,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I3,I6,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum245136(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K5+1:L5,K1+1:L1,K3+1:L3,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I5,I1,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum245163(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K5+1:L5,K1+1:L1,K6+1:L6,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I5,I1,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum245316(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K5+1:L5,K3+1:L3,K1+1:L1,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I5,I3,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum245361(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K5+1:L5,K3+1:L3,K6+1:L6,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I5,I3,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum245613(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K5+1:L5,K6+1:L6,K1+1:L1,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I5,I6,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum245631(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K5+1:L5,K6+1:L6,K3+1:L3,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I5,I6,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum246135(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K6+1:L6,K1+1:L1,K3+1:L3,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I6,I1,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum246153(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K6+1:L6,K1+1:L1,K5+1:L5,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I6,I1,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum246315(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K6+1:L6,K3+1:L3,K1+1:L1,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I6,I3,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum246351(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K6+1:L6,K3+1:L3,K5+1:L5,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I6,I3,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum246513(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K6+1:L6,K5+1:L5,K1+1:L1,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I6,I5,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum246531(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K6+1:L6,K5+1:L5,K3+1:L3,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I6,I5,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum251346(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K1+1:L1,K3+1:L3,K4+1:L4,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I1,I3,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum251364(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K1+1:L1,K3+1:L3,K6+1:L6,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I1,I3,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum251436(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K1+1:L1,K4+1:L4,K3+1:L3,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I1,I4,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum251463(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K1+1:L1,K4+1:L4,K6+1:L6,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I1,I4,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum251634(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K1+1:L1,K6+1:L6,K3+1:L3,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I1,I6,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum251643(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K1+1:L1,K6+1:L6,K4+1:L4,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I1,I6,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum253146(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K3+1:L3,K1+1:L1,K4+1:L4,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I3,I1,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum253164(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K3+1:L3,K1+1:L1,K6+1:L6,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I3,I1,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum253416(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K3+1:L3,K4+1:L4,K1+1:L1,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I3,I4,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum253461(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K3+1:L3,K4+1:L4,K6+1:L6,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I3,I4,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum253614(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K3+1:L3,K6+1:L6,K1+1:L1,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I3,I6,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum253641(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K3+1:L3,K6+1:L6,K4+1:L4,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I3,I6,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum254136(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K4+1:L4,K1+1:L1,K3+1:L3,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I4,I1,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum254163(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K4+1:L4,K1+1:L1,K6+1:L6,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I4,I1,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum254316(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K4+1:L4,K3+1:L3,K1+1:L1,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I4,I3,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum254361(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K4+1:L4,K3+1:L3,K6+1:L6,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I4,I3,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum254613(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K4+1:L4,K6+1:L6,K1+1:L1,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I4,I6,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum254631(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K4+1:L4,K6+1:L6,K3+1:L3,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I4,I6,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum256134(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K6+1:L6,K1+1:L1,K3+1:L3,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I6,I1,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum256143(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K6+1:L6,K1+1:L1,K4+1:L4,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I6,I1,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum256314(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K6+1:L6,K3+1:L3,K1+1:L1,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I6,I3,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum256341(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K6+1:L6,K3+1:L3,K4+1:L4,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I6,I3,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum256413(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K6+1:L6,K4+1:L4,K1+1:L1,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I6,I4,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum256431(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K6+1:L6,K4+1:L4,K3+1:L3,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I6,I4,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum261345(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K1+1:L1,K3+1:L3,K4+1:L4,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I1,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum261354(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K1+1:L1,K3+1:L3,K5+1:L5,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I1,I3,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum261435(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K1+1:L1,K4+1:L4,K3+1:L3,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I1,I4,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum261453(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K1+1:L1,K4+1:L4,K5+1:L5,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I1,I4,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum261534(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K1+1:L1,K5+1:L5,K3+1:L3,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I1,I5,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum261543(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K1+1:L1,K5+1:L5,K4+1:L4,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I1,I5,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum263145(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I3,I1,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum263154(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K3+1:L3,K1+1:L1,K5+1:L5,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I3,I1,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum263415(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K3+1:L3,K4+1:L4,K1+1:L1,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I3,I4,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum263451(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K3+1:L3,K4+1:L4,K5+1:L5,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I3,I4,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum263514(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K3+1:L3,K5+1:L5,K1+1:L1,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I3,I5,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum263541(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K3+1:L3,K5+1:L5,K4+1:L4,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I3,I5,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum264135(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K4+1:L4,K1+1:L1,K3+1:L3,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I4,I1,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum264153(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K4+1:L4,K1+1:L1,K5+1:L5,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I4,I1,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum264315(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K4+1:L4,K3+1:L3,K1+1:L1,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I4,I3,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum264351(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K4+1:L4,K3+1:L3,K5+1:L5,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I4,I3,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum264513(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K4+1:L4,K5+1:L5,K1+1:L1,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I4,I5,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum264531(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K4+1:L4,K5+1:L5,K3+1:L3,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I4,I5,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum265134(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K5+1:L5,K1+1:L1,K3+1:L3,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I5,I1,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum265143(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K5+1:L5,K1+1:L1,K4+1:L4,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I5,I1,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum265314(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K5+1:L5,K3+1:L3,K1+1:L1,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I5,I3,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum265341(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K5+1:L5,K3+1:L3,K4+1:L4,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I5,I3,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum265413(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K5+1:L5,K4+1:L4,K1+1:L1,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I5,I4,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum265431(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K5+1:L5,K4+1:L4,K3+1:L3,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I5,I4,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum312456(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4,K5+1:L5,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I2,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum312465(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4,K6+1:L6,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I2,I4,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum312546(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K5+1:L5,K4+1:L4,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I2,I5,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum312564(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K5+1:L5,K6+1:L6,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I2,I5,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum312645(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K6+1:L6,K4+1:L4,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I2,I6,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum312654(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K6+1:L6,K5+1:L5,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I2,I6,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum314256(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2,K5+1:L5,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I4,I2,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum314265(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2,K6+1:L6,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I4,I2,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum314526(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5,K2+1:L2,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I4,I5,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum314562(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5,K6+1:L6,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I4,I5,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum314625(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K6+1:L6,K2+1:L2,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I4,I6,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum314652(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K6+1:L6,K5+1:L5,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I4,I6,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum315246(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K5+1:L5,K2+1:L2,K4+1:L4,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I5,I2,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum315264(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K5+1:L5,K2+1:L2,K6+1:L6,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I5,I2,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum315426(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K5+1:L5,K4+1:L4,K2+1:L2,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I5,I4,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum315462(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K5+1:L5,K4+1:L4,K6+1:L6,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I5,I4,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum315624(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K5+1:L5,K6+1:L6,K2+1:L2,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I5,I6,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum315642(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K5+1:L5,K6+1:L6,K4+1:L4,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I5,I6,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum316245(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K6+1:L6,K2+1:L2,K4+1:L4,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I6,I2,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum316254(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K6+1:L6,K2+1:L2,K5+1:L5,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I6,I2,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum316425(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K6+1:L6,K4+1:L4,K2+1:L2,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I6,I4,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum316452(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K6+1:L6,K4+1:L4,K5+1:L5,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I6,I4,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum316524(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K6+1:L6,K5+1:L5,K2+1:L2,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I6,I5,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum316542(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K6+1:L6,K5+1:L5,K4+1:L4,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I6,I5,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum321456(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4,K5+1:L5,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I1,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum321465(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4,K6+1:L6,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I1,I4,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum321546(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K5+1:L5,K4+1:L4,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I1,I5,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum321564(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K5+1:L5,K6+1:L6,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I1,I5,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum321645(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K6+1:L6,K4+1:L4,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I1,I6,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum321654(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K6+1:L6,K5+1:L5,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I1,I6,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum324156(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1,K5+1:L5,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I4,I1,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum324165(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1,K6+1:L6,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I4,I1,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum324516(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K5+1:L5,K1+1:L1,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I4,I5,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum324561(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K5+1:L5,K6+1:L6,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I4,I5,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum324615(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K6+1:L6,K1+1:L1,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I4,I6,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum324651(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K6+1:L6,K5+1:L5,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I4,I6,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum325146(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K5+1:L5,K1+1:L1,K4+1:L4,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I5,I1,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum325164(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K5+1:L5,K1+1:L1,K6+1:L6,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I5,I1,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum325416(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K5+1:L5,K4+1:L4,K1+1:L1,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I5,I4,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum325461(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K5+1:L5,K4+1:L4,K6+1:L6,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I5,I4,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum325614(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K5+1:L5,K6+1:L6,K1+1:L1,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I5,I6,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum325641(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K5+1:L5,K6+1:L6,K4+1:L4,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I5,I6,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum326145(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K6+1:L6,K1+1:L1,K4+1:L4,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I6,I1,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum326154(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K6+1:L6,K1+1:L1,K5+1:L5,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I6,I1,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum326415(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K6+1:L6,K4+1:L4,K1+1:L1,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I6,I4,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum326451(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K6+1:L6,K4+1:L4,K5+1:L5,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I6,I4,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum326514(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K6+1:L6,K5+1:L5,K1+1:L1,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I6,I5,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum326541(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K6+1:L6,K5+1:L5,K4+1:L4,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I6,I5,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum341256(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2,K5+1:L5,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I1,I2,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum341265(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2,K6+1:L6,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I1,I2,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum341526(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K5+1:L5,K2+1:L2,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I1,I5,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum341562(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K5+1:L5,K6+1:L6,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I1,I5,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum341625(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K6+1:L6,K2+1:L2,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I1,I6,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum341652(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K6+1:L6,K5+1:L5,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I1,I6,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum342156(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1,K5+1:L5,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I2,I1,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum342165(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1,K6+1:L6,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I2,I1,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum342516(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K5+1:L5,K1+1:L1,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I2,I5,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum342561(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K5+1:L5,K6+1:L6,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I2,I5,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum342615(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K6+1:L6,K1+1:L1,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I2,I6,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum342651(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K6+1:L6,K5+1:L5,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I2,I6,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum345126(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K5+1:L5,K1+1:L1,K2+1:L2,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I5,I1,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum345162(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K5+1:L5,K1+1:L1,K6+1:L6,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I5,I1,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum345216(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K5+1:L5,K2+1:L2,K1+1:L1,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I5,I2,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum345261(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K5+1:L5,K2+1:L2,K6+1:L6,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I5,I2,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum345612(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6,K1+1:L1,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I5,I6,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum345621(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6,K2+1:L2,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I5,I6,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum346125(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K6+1:L6,K1+1:L1,K2+1:L2,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I6,I1,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum346152(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K6+1:L6,K1+1:L1,K5+1:L5,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I6,I1,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum346215(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K6+1:L6,K2+1:L2,K1+1:L1,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I6,I2,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum346251(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K6+1:L6,K2+1:L2,K5+1:L5,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I6,I2,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum346512(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K6+1:L6,K5+1:L5,K1+1:L1,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I6,I5,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum346521(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K6+1:L6,K5+1:L5,K2+1:L2,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I6,I5,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum351246(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K1+1:L1,K2+1:L2,K4+1:L4,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I1,I2,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum351264(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K1+1:L1,K2+1:L2,K6+1:L6,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I1,I2,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum351426(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K1+1:L1,K4+1:L4,K2+1:L2,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I1,I4,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum351462(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K1+1:L1,K4+1:L4,K6+1:L6,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I1,I4,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum351624(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K1+1:L1,K6+1:L6,K2+1:L2,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I1,I6,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum351642(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K1+1:L1,K6+1:L6,K4+1:L4,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I1,I6,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum352146(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K2+1:L2,K1+1:L1,K4+1:L4,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I2,I1,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum352164(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K2+1:L2,K1+1:L1,K6+1:L6,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I2,I1,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum352416(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K2+1:L2,K4+1:L4,K1+1:L1,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I2,I4,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum352461(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K2+1:L2,K4+1:L4,K6+1:L6,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I2,I4,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum352614(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K2+1:L2,K6+1:L6,K1+1:L1,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I2,I6,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum352641(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K2+1:L2,K6+1:L6,K4+1:L4,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I2,I6,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum354126(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K4+1:L4,K1+1:L1,K2+1:L2,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I4,I1,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum354162(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K4+1:L4,K1+1:L1,K6+1:L6,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I4,I1,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum354216(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K4+1:L4,K2+1:L2,K1+1:L1,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I4,I2,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum354261(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K4+1:L4,K2+1:L2,K6+1:L6,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I4,I2,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum354612(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K4+1:L4,K6+1:L6,K1+1:L1,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I4,I6,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum354621(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K4+1:L4,K6+1:L6,K2+1:L2,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I4,I6,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum356124(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K6+1:L6,K1+1:L1,K2+1:L2,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I6,I1,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum356142(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K6+1:L6,K1+1:L1,K4+1:L4,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I6,I1,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum356214(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K6+1:L6,K2+1:L2,K1+1:L1,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I6,I2,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum356241(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K6+1:L6,K2+1:L2,K4+1:L4,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I6,I2,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum356412(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K6+1:L6,K4+1:L4,K1+1:L1,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I6,I4,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum356421(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K6+1:L6,K4+1:L4,K2+1:L2,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I6,I4,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum361245(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K1+1:L1,K2+1:L2,K4+1:L4,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I1,I2,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum361254(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K1+1:L1,K2+1:L2,K5+1:L5,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I1,I2,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum361425(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K1+1:L1,K4+1:L4,K2+1:L2,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I1,I4,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum361452(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K1+1:L1,K4+1:L4,K5+1:L5,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I1,I4,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum361524(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K1+1:L1,K5+1:L5,K2+1:L2,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I1,I5,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum361542(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K1+1:L1,K5+1:L5,K4+1:L4,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I1,I5,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum362145(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K2+1:L2,K1+1:L1,K4+1:L4,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I2,I1,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum362154(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K2+1:L2,K1+1:L1,K5+1:L5,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I2,I1,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum362415(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K2+1:L2,K4+1:L4,K1+1:L1,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I2,I4,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum362451(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K2+1:L2,K4+1:L4,K5+1:L5,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I2,I4,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum362514(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K2+1:L2,K5+1:L5,K1+1:L1,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I2,I5,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum362541(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K2+1:L2,K5+1:L5,K4+1:L4,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I2,I5,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum364125(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K4+1:L4,K1+1:L1,K2+1:L2,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I4,I1,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum364152(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K4+1:L4,K1+1:L1,K5+1:L5,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I4,I1,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum364215(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K4+1:L4,K2+1:L2,K1+1:L1,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I4,I2,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum364251(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K4+1:L4,K2+1:L2,K5+1:L5,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I4,I2,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum364512(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K4+1:L4,K5+1:L5,K1+1:L1,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I4,I5,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum364521(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K4+1:L4,K5+1:L5,K2+1:L2,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I4,I5,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum365124(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K5+1:L5,K1+1:L1,K2+1:L2,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I5,I1,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum365142(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K5+1:L5,K1+1:L1,K4+1:L4,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I5,I1,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum365214(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K5+1:L5,K2+1:L2,K1+1:L1,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I5,I2,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum365241(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K5+1:L5,K2+1:L2,K4+1:L4,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I5,I2,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum365412(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K5+1:L5,K4+1:L4,K1+1:L1,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I5,I4,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum365421(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K5+1:L5,K4+1:L4,K2+1:L2,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I5,I4,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum412356(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3,K5+1:L5,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I2,I3,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum412365(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3,K6+1:L6,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I2,I3,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum412536(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K5+1:L5,K3+1:L3,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I2,I5,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum412563(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K5+1:L5,K6+1:L6,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I2,I5,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum412635(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K6+1:L6,K3+1:L3,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I2,I6,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum412653(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K6+1:L6,K5+1:L5,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I2,I6,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum413256(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2,K5+1:L5,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I3,I2,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum413265(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2,K6+1:L6,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I3,I2,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum413526(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K5+1:L5,K2+1:L2,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I3,I5,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum413562(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K5+1:L5,K6+1:L6,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I3,I5,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum413625(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K6+1:L6,K2+1:L2,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I3,I6,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum413652(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K6+1:L6,K5+1:L5,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I3,I6,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum415236(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K5+1:L5,K2+1:L2,K3+1:L3,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I5,I2,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum415263(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K5+1:L5,K2+1:L2,K6+1:L6,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I5,I2,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum415326(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K5+1:L5,K3+1:L3,K2+1:L2,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I5,I3,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum415362(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K5+1:L5,K3+1:L3,K6+1:L6,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I5,I3,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum415623(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K5+1:L5,K6+1:L6,K2+1:L2,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I5,I6,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum415632(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K5+1:L5,K6+1:L6,K3+1:L3,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I5,I6,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum416235(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K6+1:L6,K2+1:L2,K3+1:L3,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I6,I2,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum416253(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K6+1:L6,K2+1:L2,K5+1:L5,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I6,I2,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum416325(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K6+1:L6,K3+1:L3,K2+1:L2,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I6,I3,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum416352(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K6+1:L6,K3+1:L3,K5+1:L5,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I6,I3,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum416523(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K6+1:L6,K5+1:L5,K2+1:L2,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I6,I5,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum416532(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K6+1:L6,K5+1:L5,K3+1:L3,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I6,I5,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum421356(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3,K5+1:L5,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I1,I3,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum421365(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3,K6+1:L6,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I1,I3,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum421536(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K5+1:L5,K3+1:L3,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I1,I5,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum421563(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K5+1:L5,K6+1:L6,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I1,I5,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum421635(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K6+1:L6,K3+1:L3,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I1,I6,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum421653(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K6+1:L6,K5+1:L5,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I1,I6,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum423156(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1,K5+1:L5,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I3,I1,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum423165(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1,K6+1:L6,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I3,I1,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum423516(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K5+1:L5,K1+1:L1,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I3,I5,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum423561(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K5+1:L5,K6+1:L6,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I3,I5,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum423615(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K6+1:L6,K1+1:L1,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I3,I6,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum423651(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K6+1:L6,K5+1:L5,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I3,I6,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum425136(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K5+1:L5,K1+1:L1,K3+1:L3,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I5,I1,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum425163(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K5+1:L5,K1+1:L1,K6+1:L6,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I5,I1,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum425316(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K5+1:L5,K3+1:L3,K1+1:L1,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I5,I3,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum425361(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K5+1:L5,K3+1:L3,K6+1:L6,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I5,I3,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum425613(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K5+1:L5,K6+1:L6,K1+1:L1,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I5,I6,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum425631(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K5+1:L5,K6+1:L6,K3+1:L3,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I5,I6,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum426135(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K6+1:L6,K1+1:L1,K3+1:L3,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I6,I1,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum426153(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K6+1:L6,K1+1:L1,K5+1:L5,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I6,I1,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum426315(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K6+1:L6,K3+1:L3,K1+1:L1,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I6,I3,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum426351(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K6+1:L6,K3+1:L3,K5+1:L5,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I6,I3,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum426513(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K6+1:L6,K5+1:L5,K1+1:L1,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I6,I5,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum426531(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K6+1:L6,K5+1:L5,K3+1:L3,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I6,I5,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum431256(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2,K5+1:L5,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I1,I2,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum431265(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2,K6+1:L6,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I1,I2,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum431526(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K5+1:L5,K2+1:L2,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I1,I5,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum431562(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K5+1:L5,K6+1:L6,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I1,I5,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum431625(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K6+1:L6,K2+1:L2,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I1,I6,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum431652(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K6+1:L6,K5+1:L5,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I1,I6,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum432156(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1,K5+1:L5,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I2,I1,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum432165(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1,K6+1:L6,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I2,I1,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum432516(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K5+1:L5,K1+1:L1,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I2,I5,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum432561(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K5+1:L5,K6+1:L6,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I2,I5,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum432615(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K6+1:L6,K1+1:L1,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I2,I6,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum432651(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K6+1:L6,K5+1:L5,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I2,I6,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum435126(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K5+1:L5,K1+1:L1,K2+1:L2,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I5,I1,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum435162(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K5+1:L5,K1+1:L1,K6+1:L6,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I5,I1,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum435216(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K5+1:L5,K2+1:L2,K1+1:L1,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I5,I2,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum435261(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K5+1:L5,K2+1:L2,K6+1:L6,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I5,I2,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum435612(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K5+1:L5,K6+1:L6,K1+1:L1,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I5,I6,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum435621(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K5+1:L5,K6+1:L6,K2+1:L2,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I5,I6,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum436125(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K6+1:L6,K1+1:L1,K2+1:L2,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I6,I1,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum436152(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K6+1:L6,K1+1:L1,K5+1:L5,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I6,I1,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum436215(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K6+1:L6,K2+1:L2,K1+1:L1,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I6,I2,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum436251(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K6+1:L6,K2+1:L2,K5+1:L5,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I6,I2,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum436512(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K6+1:L6,K5+1:L5,K1+1:L1,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I6,I5,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum436521(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K6+1:L6,K5+1:L5,K2+1:L2,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I6,I5,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum451236(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I1,I2,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum451263(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K1+1:L1,K2+1:L2,K6+1:L6,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I1,I2,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum451326(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K1+1:L1,K3+1:L3,K2+1:L2,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I1,I3,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum451362(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K1+1:L1,K3+1:L3,K6+1:L6,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I1,I3,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum451623(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K1+1:L1,K6+1:L6,K2+1:L2,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I1,I6,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum451632(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K1+1:L1,K6+1:L6,K3+1:L3,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I1,I6,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum452136(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K2+1:L2,K1+1:L1,K3+1:L3,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I2,I1,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum452163(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K2+1:L2,K1+1:L1,K6+1:L6,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I2,I1,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum452316(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K2+1:L2,K3+1:L3,K1+1:L1,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I2,I3,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum452361(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K2+1:L2,K3+1:L3,K6+1:L6,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I2,I3,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum452613(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K2+1:L2,K6+1:L6,K1+1:L1,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I2,I6,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum452631(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K2+1:L2,K6+1:L6,K3+1:L3,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I2,I6,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum453126(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K3+1:L3,K1+1:L1,K2+1:L2,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I3,I1,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum453162(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K3+1:L3,K1+1:L1,K6+1:L6,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I3,I1,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum453216(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K3+1:L3,K2+1:L2,K1+1:L1,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I3,I2,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum453261(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K3+1:L3,K2+1:L2,K6+1:L6,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I3,I2,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum453612(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K3+1:L3,K6+1:L6,K1+1:L1,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I3,I6,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum453621(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K3+1:L3,K6+1:L6,K2+1:L2,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I3,I6,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum456123(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K6+1:L6,K1+1:L1,K2+1:L2,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I6,I1,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum456132(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K6+1:L6,K1+1:L1,K3+1:L3,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I6,I1,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum456213(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K6+1:L6,K2+1:L2,K1+1:L1,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I6,I2,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum456231(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K6+1:L6,K2+1:L2,K3+1:L3,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I6,I2,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum456312(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K6+1:L6,K3+1:L3,K1+1:L1,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I6,I3,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum456321(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K6+1:L6,K3+1:L3,K2+1:L2,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I6,I3,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum461235(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K1+1:L1,K2+1:L2,K3+1:L3,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I1,I2,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum461253(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K1+1:L1,K2+1:L2,K5+1:L5,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I1,I2,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum461325(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K1+1:L1,K3+1:L3,K2+1:L2,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I1,I3,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum461352(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K1+1:L1,K3+1:L3,K5+1:L5,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I1,I3,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum461523(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K1+1:L1,K5+1:L5,K2+1:L2,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I1,I5,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum461532(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K1+1:L1,K5+1:L5,K3+1:L3,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I1,I5,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum462135(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K2+1:L2,K1+1:L1,K3+1:L3,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I2,I1,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum462153(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K2+1:L2,K1+1:L1,K5+1:L5,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I2,I1,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum462315(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K2+1:L2,K3+1:L3,K1+1:L1,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I2,I3,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum462351(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K2+1:L2,K3+1:L3,K5+1:L5,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I2,I3,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum462513(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K2+1:L2,K5+1:L5,K1+1:L1,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I2,I5,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum462531(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K2+1:L2,K5+1:L5,K3+1:L3,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I2,I5,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum463125(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K3+1:L3,K1+1:L1,K2+1:L2,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I3,I1,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum463152(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K3+1:L3,K1+1:L1,K5+1:L5,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I3,I1,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum463215(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K3+1:L3,K2+1:L2,K1+1:L1,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I3,I2,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum463251(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K3+1:L3,K2+1:L2,K5+1:L5,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I3,I2,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum463512(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K3+1:L3,K5+1:L5,K1+1:L1,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I3,I5,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum463521(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K3+1:L3,K5+1:L5,K2+1:L2,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I3,I5,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum465123(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I5,I1,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum465132(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K5+1:L5,K1+1:L1,K3+1:L3,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I5,I1,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum465213(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K5+1:L5,K2+1:L2,K1+1:L1,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I5,I2,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum465231(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K5+1:L5,K2+1:L2,K3+1:L3,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I5,I2,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum465312(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K5+1:L5,K3+1:L3,K1+1:L1,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I5,I3,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum465321(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K5+1:L5,K3+1:L3,K2+1:L2,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I5,I3,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum512346(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I2,I3,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum512364(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3,K6+1:L6,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I2,I3,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum512436(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I2,I4,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum512463(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K2+1:L2,K4+1:L4,K6+1:L6,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I2,I4,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum512634(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K2+1:L2,K6+1:L6,K3+1:L3,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I2,I6,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum512643(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K2+1:L2,K6+1:L6,K4+1:L4,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I2,I6,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum513246(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I3,I2,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum513264(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K3+1:L3,K2+1:L2,K6+1:L6,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I3,I2,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum513426(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I3,I4,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum513462(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K3+1:L3,K4+1:L4,K6+1:L6,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I3,I4,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum513624(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K3+1:L3,K6+1:L6,K2+1:L2,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I3,I6,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum513642(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K3+1:L3,K6+1:L6,K4+1:L4,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I3,I6,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum514236(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I4,I2,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum514263(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K4+1:L4,K2+1:L2,K6+1:L6,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I4,I2,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum514326(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I4,I3,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum514362(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K4+1:L4,K3+1:L3,K6+1:L6,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I4,I3,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum514623(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K4+1:L4,K6+1:L6,K2+1:L2,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I4,I6,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum514632(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K4+1:L4,K6+1:L6,K3+1:L3,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I4,I6,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum516234(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K6+1:L6,K2+1:L2,K3+1:L3,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I6,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum516243(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K6+1:L6,K2+1:L2,K4+1:L4,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I6,I2,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum516324(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K6+1:L6,K3+1:L3,K2+1:L2,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I6,I3,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum516342(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K6+1:L6,K3+1:L3,K4+1:L4,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I6,I3,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum516423(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K6+1:L6,K4+1:L4,K2+1:L2,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I6,I4,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum516432(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K6+1:L6,K4+1:L4,K3+1:L3,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I6,I4,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum521346(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I1,I3,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum521364(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K1+1:L1,K3+1:L3,K6+1:L6,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I1,I3,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum521436(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I1,I4,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum521463(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K1+1:L1,K4+1:L4,K6+1:L6,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I1,I4,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum521634(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K1+1:L1,K6+1:L6,K3+1:L3,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I1,I6,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum521643(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K1+1:L1,K6+1:L6,K4+1:L4,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I1,I6,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum523146(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I3,I1,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum523164(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K3+1:L3,K1+1:L1,K6+1:L6,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I3,I1,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum523416(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I3,I4,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum523461(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K3+1:L3,K4+1:L4,K6+1:L6,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I3,I4,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum523614(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K3+1:L3,K6+1:L6,K1+1:L1,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I3,I6,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum523641(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K3+1:L3,K6+1:L6,K4+1:L4,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I3,I6,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum524136(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I4,I1,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum524163(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K4+1:L4,K1+1:L1,K6+1:L6,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I4,I1,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum524316(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I4,I3,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum524361(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K4+1:L4,K3+1:L3,K6+1:L6,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I4,I3,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum524613(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K4+1:L4,K6+1:L6,K1+1:L1,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I4,I6,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum524631(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K4+1:L4,K6+1:L6,K3+1:L3,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I4,I6,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum526134(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K6+1:L6,K1+1:L1,K3+1:L3,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I6,I1,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum526143(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K6+1:L6,K1+1:L1,K4+1:L4,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I6,I1,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum526314(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K6+1:L6,K3+1:L3,K1+1:L1,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I6,I3,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum526341(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K6+1:L6,K3+1:L3,K4+1:L4,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I6,I3,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum526413(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K6+1:L6,K4+1:L4,K1+1:L1,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I6,I4,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum526431(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K6+1:L6,K4+1:L4,K3+1:L3,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I6,I4,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum531246(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I1,I2,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum531264(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K1+1:L1,K2+1:L2,K6+1:L6,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I1,I2,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum531426(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I1,I4,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum531462(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K1+1:L1,K4+1:L4,K6+1:L6,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I1,I4,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum531624(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K1+1:L1,K6+1:L6,K2+1:L2,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I1,I6,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum531642(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K1+1:L1,K6+1:L6,K4+1:L4,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I1,I6,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum532146(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I2,I1,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum532164(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K2+1:L2,K1+1:L1,K6+1:L6,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I2,I1,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum532416(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I2,I4,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum532461(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K2+1:L2,K4+1:L4,K6+1:L6,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I2,I4,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum532614(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K2+1:L2,K6+1:L6,K1+1:L1,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I2,I6,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum532641(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K2+1:L2,K6+1:L6,K4+1:L4,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I2,I6,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum534126(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I4,I1,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum534162(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K4+1:L4,K1+1:L1,K6+1:L6,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I4,I1,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum534216(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I4,I2,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum534261(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K4+1:L4,K2+1:L2,K6+1:L6,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I4,I2,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum534612(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K4+1:L4,K6+1:L6,K1+1:L1,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I4,I6,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum534621(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K4+1:L4,K6+1:L6,K2+1:L2,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I4,I6,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum536124(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K6+1:L6,K1+1:L1,K2+1:L2,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I6,I1,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum536142(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K6+1:L6,K1+1:L1,K4+1:L4,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I6,I1,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum536214(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K6+1:L6,K2+1:L2,K1+1:L1,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I6,I2,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum536241(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K6+1:L6,K2+1:L2,K4+1:L4,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I6,I2,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum536412(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K6+1:L6,K4+1:L4,K1+1:L1,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I6,I4,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum536421(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K6+1:L6,K4+1:L4,K2+1:L2,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I6,I4,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum541236(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I1,I2,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum541263(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K1+1:L1,K2+1:L2,K6+1:L6,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I1,I2,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum541326(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I1,I3,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum541362(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K1+1:L1,K3+1:L3,K6+1:L6,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I1,I3,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum541623(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K1+1:L1,K6+1:L6,K2+1:L2,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I1,I6,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum541632(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K1+1:L1,K6+1:L6,K3+1:L3,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I1,I6,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum542136(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I2,I1,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum542163(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K2+1:L2,K1+1:L1,K6+1:L6,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I2,I1,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum542316(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I2,I3,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum542361(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K2+1:L2,K3+1:L3,K6+1:L6,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I2,I3,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum542613(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K2+1:L2,K6+1:L6,K1+1:L1,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I2,I6,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum542631(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K2+1:L2,K6+1:L6,K3+1:L3,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I2,I6,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum543126(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I3,I1,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum543162(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K3+1:L3,K1+1:L1,K6+1:L6,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I3,I1,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum543216(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1,K6+1:L6)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I3,I2,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum543261(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K3+1:L3,K2+1:L2,K6+1:L6,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I3,I2,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum543612(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K3+1:L3,K6+1:L6,K1+1:L1,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I3,I6,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum543621(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K3+1:L3,K6+1:L6,K2+1:L2,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I3,I6,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum546123(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K6+1:L6,K1+1:L1,K2+1:L2,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I6,I1,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum546132(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K6+1:L6,K1+1:L1,K3+1:L3,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I6,I1,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum546213(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K6+1:L6,K2+1:L2,K1+1:L1,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I6,I2,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum546231(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K6+1:L6,K2+1:L2,K3+1:L3,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I6,I2,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum546312(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K6+1:L6,K3+1:L3,K1+1:L1,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I6,I3,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum546321(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K6+1:L6,K3+1:L3,K2+1:L2,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I6,I3,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum561234(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum561243(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I1,I2,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum561324(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I1,I3,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum561342(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I1,I3,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum561423(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I1,I4,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum561432(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I1,I4,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum562134(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I2,I1,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum562143(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I2,I1,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum562314(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I2,I3,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum562341(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I2,I3,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum562413(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I2,I4,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum562431(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I2,I4,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum563124(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I3,I1,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum563142(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I3,I1,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum563214(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I3,I2,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum563241(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I3,I2,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum563412(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I3,I4,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum563421(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I3,I4,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum564123(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I4,I1,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum564132(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I4,I1,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum564213(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I4,I2,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum564231(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I4,I2,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum564312(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I4,I3,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum564321(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I4,I3,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum612345(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum612354(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K2+1:L2,K3+1:L3,K5+1:L5,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I2,I3,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum612435(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I2,I4,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum612453(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K2+1:L2,K4+1:L4,K5+1:L5,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I2,I4,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum612534(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K2+1:L2,K5+1:L5,K3+1:L3,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I2,I5,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum612543(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K2+1:L2,K5+1:L5,K4+1:L4,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I2,I5,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum613245(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I3,I2,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum613254(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K3+1:L3,K2+1:L2,K5+1:L5,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I3,I2,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum613425(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I3,I4,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum613452(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K3+1:L3,K4+1:L4,K5+1:L5,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I3,I4,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum613524(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K3+1:L3,K5+1:L5,K2+1:L2,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I3,I5,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum613542(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K3+1:L3,K5+1:L5,K4+1:L4,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I3,I5,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum614235(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I4,I2,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum614253(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K4+1:L4,K2+1:L2,K5+1:L5,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I4,I2,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum614325(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I4,I3,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum614352(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K4+1:L4,K3+1:L3,K5+1:L5,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I4,I3,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum614523(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K4+1:L4,K5+1:L5,K2+1:L2,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I4,I5,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum614532(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K4+1:L4,K5+1:L5,K3+1:L3,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I4,I5,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum615234(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K5+1:L5,K2+1:L2,K3+1:L3,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I5,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum615243(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K5+1:L5,K2+1:L2,K4+1:L4,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I5,I2,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum615324(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K5+1:L5,K3+1:L3,K2+1:L2,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I5,I3,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum615342(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K5+1:L5,K3+1:L3,K4+1:L4,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I5,I3,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum615423(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K5+1:L5,K4+1:L4,K2+1:L2,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I5,I4,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum615432(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K5+1:L5,K4+1:L4,K3+1:L3,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I5,I4,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum621345(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I1,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum621354(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K1+1:L1,K3+1:L3,K5+1:L5,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I1,I3,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum621435(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I1,I4,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum621453(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K1+1:L1,K4+1:L4,K5+1:L5,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I1,I4,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum621534(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K1+1:L1,K5+1:L5,K3+1:L3,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I1,I5,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum621543(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K1+1:L1,K5+1:L5,K4+1:L4,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I1,I5,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum623145(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I3,I1,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum623154(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K3+1:L3,K1+1:L1,K5+1:L5,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I3,I1,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum623415(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I3,I4,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum623451(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I3,I4,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum623514(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K3+1:L3,K5+1:L5,K1+1:L1,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I3,I5,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum623541(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K3+1:L3,K5+1:L5,K4+1:L4,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I3,I5,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum624135(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I4,I1,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum624153(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K4+1:L4,K1+1:L1,K5+1:L5,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I4,I1,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum624315(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I4,I3,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum624351(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K4+1:L4,K3+1:L3,K5+1:L5,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I4,I3,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum624513(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K4+1:L4,K5+1:L5,K1+1:L1,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I4,I5,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum624531(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K4+1:L4,K5+1:L5,K3+1:L3,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I4,I5,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum625134(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K5+1:L5,K1+1:L1,K3+1:L3,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I5,I1,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum625143(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K5+1:L5,K1+1:L1,K4+1:L4,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I5,I1,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum625314(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K5+1:L5,K3+1:L3,K1+1:L1,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I5,I3,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum625341(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K5+1:L5,K3+1:L3,K4+1:L4,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I5,I3,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum625413(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K5+1:L5,K4+1:L4,K1+1:L1,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I5,I4,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum625431(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K5+1:L5,K4+1:L4,K3+1:L3,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I5,I4,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum631245(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I1,I2,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum631254(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K1+1:L1,K2+1:L2,K5+1:L5,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I1,I2,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum631425(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I1,I4,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum631452(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I1,I4,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum631524(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K1+1:L1,K5+1:L5,K2+1:L2,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I1,I5,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum631542(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K1+1:L1,K5+1:L5,K4+1:L4,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I1,I5,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum632145(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I2,I1,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum632154(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K2+1:L2,K1+1:L1,K5+1:L5,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I2,I1,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum632415(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I2,I4,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum632451(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K2+1:L2,K4+1:L4,K5+1:L5,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I2,I4,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum632514(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K2+1:L2,K5+1:L5,K1+1:L1,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I2,I5,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum632541(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K2+1:L2,K5+1:L5,K4+1:L4,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I2,I5,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum634125(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I4,I1,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum634152(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K4+1:L4,K1+1:L1,K5+1:L5,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I4,I1,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum634215(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I4,I2,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum634251(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K4+1:L4,K2+1:L2,K5+1:L5,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I4,I2,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum634512(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K4+1:L4,K5+1:L5,K1+1:L1,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I4,I5,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum634521(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K4+1:L4,K5+1:L5,K2+1:L2,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I4,I5,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum635124(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K5+1:L5,K1+1:L1,K2+1:L2,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I5,I1,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum635142(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K5+1:L5,K1+1:L1,K4+1:L4,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I5,I1,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum635214(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K5+1:L5,K2+1:L2,K1+1:L1,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I5,I2,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum635241(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K5+1:L5,K2+1:L2,K4+1:L4,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I5,I2,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum635412(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K5+1:L5,K4+1:L4,K1+1:L1,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I5,I4,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum635421(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K5+1:L5,K4+1:L4,K2+1:L2,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I5,I4,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum641235(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I1,I2,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum641253(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K1+1:L1,K2+1:L2,K5+1:L5,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I1,I2,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum641325(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I1,I3,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum641352(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K1+1:L1,K3+1:L3,K5+1:L5,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I1,I3,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum641523(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K1+1:L1,K5+1:L5,K2+1:L2,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I1,I5,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum641532(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K1+1:L1,K5+1:L5,K3+1:L3,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I1,I5,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum642135(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I2,I1,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum642153(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K2+1:L2,K1+1:L1,K5+1:L5,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I2,I1,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum642315(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I2,I3,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum642351(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K2+1:L2,K3+1:L3,K5+1:L5,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I2,I3,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum642513(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K2+1:L2,K5+1:L5,K1+1:L1,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I2,I5,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum642531(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K2+1:L2,K5+1:L5,K3+1:L3,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I2,I5,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum643125(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I3,I1,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum643152(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K3+1:L3,K1+1:L1,K5+1:L5,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I3,I1,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum643215(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1,K5+1:L5)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I3,I2,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum643251(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K3+1:L3,K2+1:L2,K5+1:L5,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I3,I2,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum643512(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K3+1:L3,K5+1:L5,K1+1:L1,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I3,I5,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum643521(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K3+1:L3,K5+1:L5,K2+1:L2,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I3,I5,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum645123(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I5,I1,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum645132(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K5+1:L5,K1+1:L1,K3+1:L3,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I5,I1,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum645213(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K5+1:L5,K2+1:L2,K1+1:L1,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I5,I2,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum645231(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K5+1:L5,K2+1:L2,K3+1:L3,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I5,I2,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum645312(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K5+1:L5,K3+1:L3,K1+1:L1,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I5,I3,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum645321(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K5+1:L5,K3+1:L3,K2+1:L2,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I5,I3,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum651234(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum651243(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I1,I2,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum651324(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I1,I3,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum651342(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I1,I3,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum651423(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I1,I4,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum651432(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I1,I4,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum652134(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I2,I1,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum652143(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I2,I1,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum652314(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I2,I3,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum652341(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I2,I3,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum652413(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I2,I4,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum652431(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I2,I4,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum653124(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I3,I1,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum653142(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I3,I1,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum653214(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I3,I2,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum653241(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I3,I2,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum653412(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I3,I4,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum653421(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I3,I4,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum654123(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I4,I1,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum654132(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I4,I1,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum654213(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I4,I2,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum654231(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I4,I2,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum654312(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I4,I3,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sum654321(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1)
       real C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I4,I3,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx12(N0,N3,K1,L1,K2,L2,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2)
       real*8 B(N0+1:N3,N0+1:N3)
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
       subroutine sumx21(N0,N3,K1,L1,K2,L2,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2)
       real*8 B(N0+1:N3,N0+1:N3)
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
       subroutine sumx123456(L1,L2,L3,L4,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L3,L4,L5,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx123465(L1,L2,L3,L4,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L3,L4,L6,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I3,I4,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx123546(L1,L2,L3,L5,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L3,L5,L4,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I3,I5,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx123564(L1,L2,L3,L5,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L3,L5,L6,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I3,I5,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx123645(L1,L2,L3,L6,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L3,L6,L4,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I3,I6,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx123654(L1,L2,L3,L6,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L3,L6,L5,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I3,I6,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx124356(L1,L2,L4,L3,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L4,L3,L5,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I4,I3,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx124365(L1,L2,L4,L3,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L4,L3,L6,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I4,I3,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx124536(L1,L2,L4,L5,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L4,L5,L3,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I4,I5,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx124563(L1,L2,L4,L5,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L4,L5,L6,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I4,I5,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx124635(L1,L2,L4,L6,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L4,L6,L3,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I4,I6,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx124653(L1,L2,L4,L6,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L4,L6,L5,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I4,I6,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx125346(L1,L2,L5,L3,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L5,L3,L4,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I5,I3,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx125364(L1,L2,L5,L3,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L5,L3,L6,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I5,I3,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx125436(L1,L2,L5,L4,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L5,L4,L3,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I5,I4,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx125463(L1,L2,L5,L4,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L5,L4,L6,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I5,I4,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx125634(L1,L2,L5,L6,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L5,L6,L3,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I5,I6,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx125643(L1,L2,L5,L6,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L5,L6,L4,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I5,I6,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx126345(L1,L2,L6,L3,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L6,L3,L4,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I6,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx126354(L1,L2,L6,L3,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L6,L3,L5,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I6,I3,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx126435(L1,L2,L6,L4,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L6,L4,L3,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I6,I4,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx126453(L1,L2,L6,L4,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L6,L4,L5,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I6,I4,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx126534(L1,L2,L6,L5,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L6,L5,L3,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I6,I5,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx126543(L1,L2,L6,L5,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L6,L5,L4,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I6,I5,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx132456(L1,L3,L2,L4,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L2,L4,L5,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I2,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx132465(L1,L3,L2,L4,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L2,L4,L6,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I2,I4,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx132546(L1,L3,L2,L5,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L2,L5,L4,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I2,I5,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx132564(L1,L3,L2,L5,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L2,L5,L6,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I2,I5,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx132645(L1,L3,L2,L6,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L2,L6,L4,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I2,I6,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx132654(L1,L3,L2,L6,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L2,L6,L5,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I2,I6,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx134256(L1,L3,L4,L2,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L4,L2,L5,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I4,I2,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx134265(L1,L3,L4,L2,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L4,L2,L6,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I4,I2,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx134526(L1,L3,L4,L5,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L4,L5,L2,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I4,I5,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx134562(L1,L3,L4,L5,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L4,L5,L6,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I4,I5,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx134625(L1,L3,L4,L6,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L4,L6,L2,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I4,I6,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx134652(L1,L3,L4,L6,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L4,L6,L5,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I4,I6,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx135246(L1,L3,L5,L2,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L5,L2,L4,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I5,I2,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx135264(L1,L3,L5,L2,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L5,L2,L6,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I5,I2,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx135426(L1,L3,L5,L4,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L5,L4,L2,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I5,I4,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx135462(L1,L3,L5,L4,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L5,L4,L6,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I5,I4,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx135624(L1,L3,L5,L6,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L5,L6,L2,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I5,I6,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx135642(L1,L3,L5,L6,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L5,L6,L4,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I5,I6,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx136245(L1,L3,L6,L2,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L6,L2,L4,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I6,I2,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx136254(L1,L3,L6,L2,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L6,L2,L5,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I6,I2,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx136425(L1,L3,L6,L4,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L6,L4,L2,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I6,I4,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx136452(L1,L3,L6,L4,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L6,L4,L5,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I6,I4,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx136524(L1,L3,L6,L5,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L6,L5,L2,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I6,I5,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx136542(L1,L3,L6,L5,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L6,L5,L4,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I6,I5,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx142356(L1,L4,L2,L3,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L2,L3,L5,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I2,I3,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx142365(L1,L4,L2,L3,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L2,L3,L6,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I2,I3,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx142536(L1,L4,L2,L5,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L2,L5,L3,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I2,I5,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx142563(L1,L4,L2,L5,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L2,L5,L6,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I2,I5,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx142635(L1,L4,L2,L6,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L2,L6,L3,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I2,I6,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx142653(L1,L4,L2,L6,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L2,L6,L5,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I2,I6,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx143256(L1,L4,L3,L2,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L3,L2,L5,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I3,I2,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx143265(L1,L4,L3,L2,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L3,L2,L6,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I3,I2,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx143526(L1,L4,L3,L5,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L3,L5,L2,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I3,I5,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx143562(L1,L4,L3,L5,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L3,L5,L6,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I3,I5,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx143625(L1,L4,L3,L6,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L3,L6,L2,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I3,I6,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx143652(L1,L4,L3,L6,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L3,L6,L5,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I3,I6,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx145236(L1,L4,L5,L2,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L5,L2,L3,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I5,I2,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx145263(L1,L4,L5,L2,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L5,L2,L6,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I5,I2,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx145326(L1,L4,L5,L3,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L5,L3,L2,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I5,I3,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx145362(L1,L4,L5,L3,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L5,L3,L6,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I5,I3,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx145623(L1,L4,L5,L6,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L5,L6,L2,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I5,I6,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx145632(L1,L4,L5,L6,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L5,L6,L3,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I5,I6,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx146235(L1,L4,L6,L2,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L6,L2,L3,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I6,I2,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx146253(L1,L4,L6,L2,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L6,L2,L5,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I6,I2,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx146325(L1,L4,L6,L3,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L6,L3,L2,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I6,I3,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx146352(L1,L4,L6,L3,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L6,L3,L5,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I6,I3,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx146523(L1,L4,L6,L5,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L6,L5,L2,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I6,I5,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx146532(L1,L4,L6,L5,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L6,L5,L3,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I6,I5,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx152346(L1,L5,L2,L3,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L2,L3,L4,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I2,I3,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx152364(L1,L5,L2,L3,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L2,L3,L6,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I2,I3,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx152436(L1,L5,L2,L4,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L2,L4,L3,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I2,I4,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx152463(L1,L5,L2,L4,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L2,L4,L6,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I2,I4,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx152634(L1,L5,L2,L6,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L2,L6,L3,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I2,I6,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx152643(L1,L5,L2,L6,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L2,L6,L4,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I2,I6,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx153246(L1,L5,L3,L2,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L3,L2,L4,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I3,I2,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx153264(L1,L5,L3,L2,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L3,L2,L6,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I3,I2,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx153426(L1,L5,L3,L4,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L3,L4,L2,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I3,I4,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx153462(L1,L5,L3,L4,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L3,L4,L6,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I3,I4,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx153624(L1,L5,L3,L6,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L3,L6,L2,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I3,I6,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx153642(L1,L5,L3,L6,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L3,L6,L4,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I3,I6,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx154236(L1,L5,L4,L2,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L4,L2,L3,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I4,I2,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx154263(L1,L5,L4,L2,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L4,L2,L6,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I4,I2,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx154326(L1,L5,L4,L3,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L4,L3,L2,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I4,I3,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx154362(L1,L5,L4,L3,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L4,L3,L6,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I4,I3,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx154623(L1,L5,L4,L6,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L4,L6,L2,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I4,I6,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx154632(L1,L5,L4,L6,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L4,L6,L3,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I4,I6,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx156234(L1,L5,L6,L2,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L6,L2,L3,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I6,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx156243(L1,L5,L6,L2,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L6,L2,L4,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I6,I2,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx156324(L1,L5,L6,L3,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L6,L3,L2,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I6,I3,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx156342(L1,L5,L6,L3,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L6,L3,L4,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I6,I3,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx156423(L1,L5,L6,L4,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L6,L4,L2,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I6,I4,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx156432(L1,L5,L6,L4,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L6,L4,L3,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I6,I4,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx162345(L1,L6,L2,L3,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L2,L3,L4,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx162354(L1,L6,L2,L3,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L2,L3,L5,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I2,I3,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx162435(L1,L6,L2,L4,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L2,L4,L3,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I2,I4,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx162453(L1,L6,L2,L4,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L2,L4,L5,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I2,I4,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx162534(L1,L6,L2,L5,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L2,L5,L3,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I2,I5,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx162543(L1,L6,L2,L5,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L2,L5,L4,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I2,I5,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx163245(L1,L6,L3,L2,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L3,L2,L4,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I3,I2,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx163254(L1,L6,L3,L2,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L3,L2,L5,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I3,I2,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx163425(L1,L6,L3,L4,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L3,L4,L2,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I3,I4,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx163452(L1,L6,L3,L4,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L3,L4,L5,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I3,I4,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx163524(L1,L6,L3,L5,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L3,L5,L2,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I3,I5,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx163542(L1,L6,L3,L5,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L3,L5,L4,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I3,I5,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx164235(L1,L6,L4,L2,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L4,L2,L3,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I4,I2,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx164253(L1,L6,L4,L2,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L4,L2,L5,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I4,I2,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx164325(L1,L6,L4,L3,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L4,L3,L2,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I4,I3,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx164352(L1,L6,L4,L3,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L4,L3,L5,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I4,I3,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx164523(L1,L6,L4,L5,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L4,L5,L2,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I4,I5,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx164532(L1,L6,L4,L5,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L4,L5,L3,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I4,I5,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx165234(L1,L6,L5,L2,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L5,L2,L3,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I5,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx165243(L1,L6,L5,L2,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L5,L2,L4,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I5,I2,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx165324(L1,L6,L5,L3,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L5,L3,L2,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I5,I3,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx165342(L1,L6,L5,L3,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L5,L3,L4,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I5,I3,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx165423(L1,L6,L5,L4,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L5,L4,L2,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I5,I4,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx165432(L1,L6,L5,L4,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L5,L4,L3,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I5,I4,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx213456(L2,L1,L3,L4,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L3,L4,L5,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx213465(L2,L1,L3,L4,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L3,L4,L6,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I3,I4,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx213546(L2,L1,L3,L5,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L3,L5,L4,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I3,I5,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx213564(L2,L1,L3,L5,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L3,L5,L6,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I3,I5,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx213645(L2,L1,L3,L6,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L3,L6,L4,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I3,I6,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx213654(L2,L1,L3,L6,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L3,L6,L5,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I3,I6,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx214356(L2,L1,L4,L3,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L4,L3,L5,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I4,I3,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx214365(L2,L1,L4,L3,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L4,L3,L6,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I4,I3,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx214536(L2,L1,L4,L5,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L4,L5,L3,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I4,I5,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx214563(L2,L1,L4,L5,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L4,L5,L6,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I4,I5,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx214635(L2,L1,L4,L6,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L4,L6,L3,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I4,I6,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx214653(L2,L1,L4,L6,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L4,L6,L5,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I4,I6,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx215346(L2,L1,L5,L3,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L5,L3,L4,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I5,I3,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx215364(L2,L1,L5,L3,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L5,L3,L6,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I5,I3,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx215436(L2,L1,L5,L4,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L5,L4,L3,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I5,I4,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx215463(L2,L1,L5,L4,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L5,L4,L6,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I5,I4,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx215634(L2,L1,L5,L6,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L5,L6,L3,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I5,I6,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx215643(L2,L1,L5,L6,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L5,L6,L4,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I5,I6,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx216345(L2,L1,L6,L3,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L6,L3,L4,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I6,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx216354(L2,L1,L6,L3,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L6,L3,L5,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I6,I3,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx216435(L2,L1,L6,L4,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L6,L4,L3,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I6,I4,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx216453(L2,L1,L6,L4,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L6,L4,L5,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I6,I4,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx216534(L2,L1,L6,L5,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L6,L5,L3,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I6,I5,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx216543(L2,L1,L6,L5,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L6,L5,L4,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I6,I5,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx231456(L2,L3,L1,L4,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L1,L4,L5,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I1,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx231465(L2,L3,L1,L4,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L1,L4,L6,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I1,I4,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx231546(L2,L3,L1,L5,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L1,L5,L4,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I1,I5,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx231564(L2,L3,L1,L5,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L1,L5,L6,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I1,I5,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx231645(L2,L3,L1,L6,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L1,L6,L4,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I1,I6,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx231654(L2,L3,L1,L6,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L1,L6,L5,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I1,I6,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx234156(L2,L3,L4,L1,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L4,L1,L5,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I4,I1,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx234165(L2,L3,L4,L1,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L4,L1,L6,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I4,I1,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx234516(L2,L3,L4,L5,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L4,L5,L1,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I4,I5,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx234561(L2,L3,L4,L5,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L4,L5,L6,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I4,I5,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx234615(L2,L3,L4,L6,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L4,L6,L1,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I4,I6,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx234651(L2,L3,L4,L6,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L4,L6,L5,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I4,I6,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx235146(L2,L3,L5,L1,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L5,L1,L4,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I5,I1,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx235164(L2,L3,L5,L1,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L5,L1,L6,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I5,I1,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx235416(L2,L3,L5,L4,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L5,L4,L1,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I5,I4,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx235461(L2,L3,L5,L4,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L5,L4,L6,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I5,I4,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx235614(L2,L3,L5,L6,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L5,L6,L1,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I5,I6,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx235641(L2,L3,L5,L6,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L5,L6,L4,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I5,I6,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx236145(L2,L3,L6,L1,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L6,L1,L4,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I6,I1,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx236154(L2,L3,L6,L1,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L6,L1,L5,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I6,I1,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx236415(L2,L3,L6,L4,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L6,L4,L1,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I6,I4,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx236451(L2,L3,L6,L4,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L6,L4,L5,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I6,I4,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx236514(L2,L3,L6,L5,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L6,L5,L1,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I6,I5,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx236541(L2,L3,L6,L5,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L6,L5,L4,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I6,I5,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx241356(L2,L4,L1,L3,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L1,L3,L5,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I1,I3,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx241365(L2,L4,L1,L3,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L1,L3,L6,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I1,I3,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx241536(L2,L4,L1,L5,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L1,L5,L3,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I1,I5,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx241563(L2,L4,L1,L5,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L1,L5,L6,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I1,I5,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx241635(L2,L4,L1,L6,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L1,L6,L3,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I1,I6,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx241653(L2,L4,L1,L6,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L1,L6,L5,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I1,I6,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx243156(L2,L4,L3,L1,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L3,L1,L5,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I3,I1,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx243165(L2,L4,L3,L1,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L3,L1,L6,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I3,I1,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx243516(L2,L4,L3,L5,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L3,L5,L1,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I3,I5,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx243561(L2,L4,L3,L5,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L3,L5,L6,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I3,I5,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx243615(L2,L4,L3,L6,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L3,L6,L1,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I3,I6,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx243651(L2,L4,L3,L6,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L3,L6,L5,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I3,I6,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx245136(L2,L4,L5,L1,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L5,L1,L3,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I5,I1,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx245163(L2,L4,L5,L1,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L5,L1,L6,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I5,I1,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx245316(L2,L4,L5,L3,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L5,L3,L1,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I5,I3,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx245361(L2,L4,L5,L3,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L5,L3,L6,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I5,I3,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx245613(L2,L4,L5,L6,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L5,L6,L1,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I5,I6,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx245631(L2,L4,L5,L6,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L5,L6,L3,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I5,I6,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx246135(L2,L4,L6,L1,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L6,L1,L3,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I6,I1,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx246153(L2,L4,L6,L1,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L6,L1,L5,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I6,I1,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx246315(L2,L4,L6,L3,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L6,L3,L1,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I6,I3,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx246351(L2,L4,L6,L3,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L6,L3,L5,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I6,I3,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx246513(L2,L4,L6,L5,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L6,L5,L1,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I6,I5,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx246531(L2,L4,L6,L5,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L6,L5,L3,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I6,I5,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx251346(L2,L5,L1,L3,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L1,L3,L4,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I1,I3,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx251364(L2,L5,L1,L3,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L1,L3,L6,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I1,I3,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx251436(L2,L5,L1,L4,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L1,L4,L3,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I1,I4,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx251463(L2,L5,L1,L4,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L1,L4,L6,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I1,I4,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx251634(L2,L5,L1,L6,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L1,L6,L3,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I1,I6,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx251643(L2,L5,L1,L6,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L1,L6,L4,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I1,I6,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx253146(L2,L5,L3,L1,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L3,L1,L4,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I3,I1,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx253164(L2,L5,L3,L1,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L3,L1,L6,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I3,I1,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx253416(L2,L5,L3,L4,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L3,L4,L1,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I3,I4,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx253461(L2,L5,L3,L4,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L3,L4,L6,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I3,I4,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx253614(L2,L5,L3,L6,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L3,L6,L1,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I3,I6,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx253641(L2,L5,L3,L6,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L3,L6,L4,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I3,I6,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx254136(L2,L5,L4,L1,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L4,L1,L3,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I4,I1,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx254163(L2,L5,L4,L1,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L4,L1,L6,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I4,I1,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx254316(L2,L5,L4,L3,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L4,L3,L1,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I4,I3,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx254361(L2,L5,L4,L3,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L4,L3,L6,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I4,I3,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx254613(L2,L5,L4,L6,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L4,L6,L1,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I4,I6,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx254631(L2,L5,L4,L6,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L4,L6,L3,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I4,I6,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx256134(L2,L5,L6,L1,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L6,L1,L3,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I6,I1,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx256143(L2,L5,L6,L1,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L6,L1,L4,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I6,I1,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx256314(L2,L5,L6,L3,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L6,L3,L1,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I6,I3,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx256341(L2,L5,L6,L3,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L6,L3,L4,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I6,I3,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx256413(L2,L5,L6,L4,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L6,L4,L1,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I6,I4,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx256431(L2,L5,L6,L4,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L6,L4,L3,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I6,I4,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx261345(L2,L6,L1,L3,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L1,L3,L4,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I1,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx261354(L2,L6,L1,L3,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L1,L3,L5,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I1,I3,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx261435(L2,L6,L1,L4,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L1,L4,L3,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I1,I4,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx261453(L2,L6,L1,L4,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L1,L4,L5,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I1,I4,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx261534(L2,L6,L1,L5,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L1,L5,L3,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I1,I5,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx261543(L2,L6,L1,L5,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L1,L5,L4,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I1,I5,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx263145(L2,L6,L3,L1,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L3,L1,L4,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I3,I1,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx263154(L2,L6,L3,L1,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L3,L1,L5,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I3,I1,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx263415(L2,L6,L3,L4,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L3,L4,L1,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I3,I4,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx263451(L2,L6,L3,L4,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L3,L4,L5,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I3,I4,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx263514(L2,L6,L3,L5,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L3,L5,L1,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I3,I5,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx263541(L2,L6,L3,L5,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L3,L5,L4,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I3,I5,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx264135(L2,L6,L4,L1,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L4,L1,L3,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I4,I1,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx264153(L2,L6,L4,L1,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L4,L1,L5,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I4,I1,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx264315(L2,L6,L4,L3,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L4,L3,L1,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I4,I3,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx264351(L2,L6,L4,L3,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L4,L3,L5,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I4,I3,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx264513(L2,L6,L4,L5,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L4,L5,L1,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I4,I5,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx264531(L2,L6,L4,L5,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L4,L5,L3,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I4,I5,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx265134(L2,L6,L5,L1,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L5,L1,L3,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I5,I1,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx265143(L2,L6,L5,L1,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L5,L1,L4,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I5,I1,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx265314(L2,L6,L5,L3,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L5,L3,L1,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I5,I3,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx265341(L2,L6,L5,L3,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L5,L3,L4,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I5,I3,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx265413(L2,L6,L5,L4,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L5,L4,L1,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I5,I4,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx265431(L2,L6,L5,L4,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L5,L4,L3,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I5,I4,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx312456(L3,L1,L2,L4,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L2,L4,L5,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I2,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx312465(L3,L1,L2,L4,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L2,L4,L6,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I2,I4,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx312546(L3,L1,L2,L5,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L2,L5,L4,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I2,I5,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx312564(L3,L1,L2,L5,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L2,L5,L6,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I2,I5,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx312645(L3,L1,L2,L6,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L2,L6,L4,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I2,I6,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx312654(L3,L1,L2,L6,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L2,L6,L5,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I2,I6,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx314256(L3,L1,L4,L2,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L4,L2,L5,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I4,I2,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx314265(L3,L1,L4,L2,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L4,L2,L6,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I4,I2,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx314526(L3,L1,L4,L5,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L4,L5,L2,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I4,I5,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx314562(L3,L1,L4,L5,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L4,L5,L6,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I4,I5,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx314625(L3,L1,L4,L6,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L4,L6,L2,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I4,I6,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx314652(L3,L1,L4,L6,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L4,L6,L5,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I4,I6,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx315246(L3,L1,L5,L2,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L5,L2,L4,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I5,I2,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx315264(L3,L1,L5,L2,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L5,L2,L6,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I5,I2,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx315426(L3,L1,L5,L4,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L5,L4,L2,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I5,I4,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx315462(L3,L1,L5,L4,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L5,L4,L6,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I5,I4,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx315624(L3,L1,L5,L6,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L5,L6,L2,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I5,I6,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx315642(L3,L1,L5,L6,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L5,L6,L4,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I5,I6,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx316245(L3,L1,L6,L2,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L6,L2,L4,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I6,I2,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx316254(L3,L1,L6,L2,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L6,L2,L5,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I6,I2,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx316425(L3,L1,L6,L4,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L6,L4,L2,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I6,I4,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx316452(L3,L1,L6,L4,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L6,L4,L5,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I6,I4,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx316524(L3,L1,L6,L5,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L6,L5,L2,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I6,I5,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx316542(L3,L1,L6,L5,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L6,L5,L4,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I6,I5,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx321456(L3,L2,L1,L4,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L1,L4,L5,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I1,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx321465(L3,L2,L1,L4,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L1,L4,L6,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I1,I4,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx321546(L3,L2,L1,L5,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L1,L5,L4,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I1,I5,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx321564(L3,L2,L1,L5,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L1,L5,L6,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I1,I5,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx321645(L3,L2,L1,L6,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L1,L6,L4,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I1,I6,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx321654(L3,L2,L1,L6,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L1,L6,L5,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I1,I6,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx324156(L3,L2,L4,L1,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L4,L1,L5,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I4,I1,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx324165(L3,L2,L4,L1,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L4,L1,L6,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I4,I1,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx324516(L3,L2,L4,L5,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L4,L5,L1,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I4,I5,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx324561(L3,L2,L4,L5,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L4,L5,L6,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I4,I5,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx324615(L3,L2,L4,L6,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L4,L6,L1,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I4,I6,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx324651(L3,L2,L4,L6,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L4,L6,L5,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I4,I6,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx325146(L3,L2,L5,L1,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L5,L1,L4,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I5,I1,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx325164(L3,L2,L5,L1,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L5,L1,L6,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I5,I1,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx325416(L3,L2,L5,L4,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L5,L4,L1,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I5,I4,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx325461(L3,L2,L5,L4,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L5,L4,L6,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I5,I4,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx325614(L3,L2,L5,L6,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L5,L6,L1,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I5,I6,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx325641(L3,L2,L5,L6,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L5,L6,L4,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I5,I6,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx326145(L3,L2,L6,L1,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L6,L1,L4,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I6,I1,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx326154(L3,L2,L6,L1,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L6,L1,L5,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I6,I1,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx326415(L3,L2,L6,L4,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L6,L4,L1,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I6,I4,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx326451(L3,L2,L6,L4,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L6,L4,L5,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I6,I4,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx326514(L3,L2,L6,L5,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L6,L5,L1,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I6,I5,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx326541(L3,L2,L6,L5,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L6,L5,L4,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I6,I5,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx341256(L3,L4,L1,L2,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L1,L2,L5,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I1,I2,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx341265(L3,L4,L1,L2,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L1,L2,L6,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I1,I2,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx341526(L3,L4,L1,L5,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L1,L5,L2,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I1,I5,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx341562(L3,L4,L1,L5,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L1,L5,L6,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I1,I5,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx341625(L3,L4,L1,L6,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L1,L6,L2,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I1,I6,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx341652(L3,L4,L1,L6,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L1,L6,L5,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I1,I6,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx342156(L3,L4,L2,L1,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L2,L1,L5,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I2,I1,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx342165(L3,L4,L2,L1,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L2,L1,L6,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I2,I1,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx342516(L3,L4,L2,L5,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L2,L5,L1,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I2,I5,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx342561(L3,L4,L2,L5,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L2,L5,L6,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I2,I5,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx342615(L3,L4,L2,L6,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L2,L6,L1,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I2,I6,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx342651(L3,L4,L2,L6,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L2,L6,L5,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I2,I6,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx345126(L3,L4,L5,L1,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L5,L1,L2,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I5,I1,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx345162(L3,L4,L5,L1,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L5,L1,L6,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I5,I1,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx345216(L3,L4,L5,L2,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L5,L2,L1,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I5,I2,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx345261(L3,L4,L5,L2,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L5,L2,L6,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I5,I2,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx345612(L3,L4,L5,L6,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L5,L6,L1,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I5,I6,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx345621(L3,L4,L5,L6,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L5,L6,L2,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I5,I6,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx346125(L3,L4,L6,L1,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L6,L1,L2,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I6,I1,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx346152(L3,L4,L6,L1,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L6,L1,L5,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I6,I1,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx346215(L3,L4,L6,L2,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L6,L2,L1,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I6,I2,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx346251(L3,L4,L6,L2,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L6,L2,L5,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I6,I2,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx346512(L3,L4,L6,L5,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L6,L5,L1,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I6,I5,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx346521(L3,L4,L6,L5,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L6,L5,L2,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I6,I5,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx351246(L3,L5,L1,L2,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L1,L2,L4,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I1,I2,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx351264(L3,L5,L1,L2,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L1,L2,L6,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I1,I2,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx351426(L3,L5,L1,L4,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L1,L4,L2,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I1,I4,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx351462(L3,L5,L1,L4,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L1,L4,L6,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I1,I4,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx351624(L3,L5,L1,L6,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L1,L6,L2,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I1,I6,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx351642(L3,L5,L1,L6,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L1,L6,L4,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I1,I6,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx352146(L3,L5,L2,L1,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L2,L1,L4,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I2,I1,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx352164(L3,L5,L2,L1,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L2,L1,L6,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I2,I1,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx352416(L3,L5,L2,L4,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L2,L4,L1,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I2,I4,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx352461(L3,L5,L2,L4,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L2,L4,L6,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I2,I4,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx352614(L3,L5,L2,L6,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L2,L6,L1,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I2,I6,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx352641(L3,L5,L2,L6,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L2,L6,L4,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I2,I6,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx354126(L3,L5,L4,L1,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L4,L1,L2,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I4,I1,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx354162(L3,L5,L4,L1,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L4,L1,L6,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I4,I1,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx354216(L3,L5,L4,L2,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L4,L2,L1,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I4,I2,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx354261(L3,L5,L4,L2,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L4,L2,L6,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I4,I2,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx354612(L3,L5,L4,L6,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L4,L6,L1,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I4,I6,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx354621(L3,L5,L4,L6,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L4,L6,L2,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I4,I6,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx356124(L3,L5,L6,L1,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L6,L1,L2,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I6,I1,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx356142(L3,L5,L6,L1,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L6,L1,L4,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I6,I1,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx356214(L3,L5,L6,L2,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L6,L2,L1,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I6,I2,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx356241(L3,L5,L6,L2,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L6,L2,L4,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I6,I2,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx356412(L3,L5,L6,L4,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L6,L4,L1,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I6,I4,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx356421(L3,L5,L6,L4,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L6,L4,L2,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I6,I4,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx361245(L3,L6,L1,L2,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L1,L2,L4,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I1,I2,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx361254(L3,L6,L1,L2,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L1,L2,L5,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I1,I2,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx361425(L3,L6,L1,L4,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L1,L4,L2,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I1,I4,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx361452(L3,L6,L1,L4,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L1,L4,L5,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I1,I4,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx361524(L3,L6,L1,L5,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L1,L5,L2,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I1,I5,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx361542(L3,L6,L1,L5,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L1,L5,L4,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I1,I5,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx362145(L3,L6,L2,L1,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L2,L1,L4,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I2,I1,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx362154(L3,L6,L2,L1,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L2,L1,L5,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I2,I1,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx362415(L3,L6,L2,L4,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L2,L4,L1,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I2,I4,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx362451(L3,L6,L2,L4,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L2,L4,L5,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I2,I4,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx362514(L3,L6,L2,L5,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L2,L5,L1,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I2,I5,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx362541(L3,L6,L2,L5,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L2,L5,L4,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I2,I5,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx364125(L3,L6,L4,L1,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L4,L1,L2,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I4,I1,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx364152(L3,L6,L4,L1,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L4,L1,L5,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I4,I1,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx364215(L3,L6,L4,L2,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L4,L2,L1,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I4,I2,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx364251(L3,L6,L4,L2,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L4,L2,L5,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I4,I2,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx364512(L3,L6,L4,L5,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L4,L5,L1,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I4,I5,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx364521(L3,L6,L4,L5,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L4,L5,L2,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I4,I5,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx365124(L3,L6,L5,L1,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L5,L1,L2,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I5,I1,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx365142(L3,L6,L5,L1,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L5,L1,L4,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I5,I1,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx365214(L3,L6,L5,L2,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L5,L2,L1,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I5,I2,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx365241(L3,L6,L5,L2,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L5,L2,L4,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I5,I2,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx365412(L3,L6,L5,L4,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L5,L4,L1,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I5,I4,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx365421(L3,L6,L5,L4,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L5,L4,L2,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I5,I4,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx412356(L4,L1,L2,L3,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L2,L3,L5,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I2,I3,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx412365(L4,L1,L2,L3,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L2,L3,L6,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I2,I3,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx412536(L4,L1,L2,L5,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L2,L5,L3,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I2,I5,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx412563(L4,L1,L2,L5,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L2,L5,L6,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I2,I5,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx412635(L4,L1,L2,L6,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L2,L6,L3,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I2,I6,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx412653(L4,L1,L2,L6,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L2,L6,L5,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I2,I6,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx413256(L4,L1,L3,L2,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L3,L2,L5,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I3,I2,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx413265(L4,L1,L3,L2,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L3,L2,L6,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I3,I2,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx413526(L4,L1,L3,L5,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L3,L5,L2,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I3,I5,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx413562(L4,L1,L3,L5,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L3,L5,L6,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I3,I5,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx413625(L4,L1,L3,L6,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L3,L6,L2,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I3,I6,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx413652(L4,L1,L3,L6,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L3,L6,L5,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I3,I6,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx415236(L4,L1,L5,L2,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L5,L2,L3,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I5,I2,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx415263(L4,L1,L5,L2,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L5,L2,L6,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I5,I2,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx415326(L4,L1,L5,L3,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L5,L3,L2,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I5,I3,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx415362(L4,L1,L5,L3,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L5,L3,L6,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I5,I3,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx415623(L4,L1,L5,L6,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L5,L6,L2,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I5,I6,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx415632(L4,L1,L5,L6,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L5,L6,L3,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I5,I6,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx416235(L4,L1,L6,L2,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L6,L2,L3,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I6,I2,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx416253(L4,L1,L6,L2,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L6,L2,L5,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I6,I2,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx416325(L4,L1,L6,L3,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L6,L3,L2,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I6,I3,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx416352(L4,L1,L6,L3,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L6,L3,L5,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I6,I3,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx416523(L4,L1,L6,L5,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L6,L5,L2,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I6,I5,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx416532(L4,L1,L6,L5,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L6,L5,L3,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I6,I5,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx421356(L4,L2,L1,L3,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L1,L3,L5,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I1,I3,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx421365(L4,L2,L1,L3,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L1,L3,L6,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I1,I3,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx421536(L4,L2,L1,L5,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L1,L5,L3,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I1,I5,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx421563(L4,L2,L1,L5,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L1,L5,L6,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I1,I5,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx421635(L4,L2,L1,L6,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L1,L6,L3,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I1,I6,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx421653(L4,L2,L1,L6,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L1,L6,L5,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I1,I6,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx423156(L4,L2,L3,L1,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L3,L1,L5,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I3,I1,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx423165(L4,L2,L3,L1,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L3,L1,L6,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I3,I1,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx423516(L4,L2,L3,L5,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L3,L5,L1,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I3,I5,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx423561(L4,L2,L3,L5,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L3,L5,L6,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I3,I5,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx423615(L4,L2,L3,L6,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L3,L6,L1,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I3,I6,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx423651(L4,L2,L3,L6,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L3,L6,L5,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I3,I6,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx425136(L4,L2,L5,L1,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L5,L1,L3,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I5,I1,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx425163(L4,L2,L5,L1,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L5,L1,L6,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I5,I1,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx425316(L4,L2,L5,L3,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L5,L3,L1,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I5,I3,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx425361(L4,L2,L5,L3,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L5,L3,L6,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I5,I3,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx425613(L4,L2,L5,L6,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L5,L6,L1,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I5,I6,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx425631(L4,L2,L5,L6,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L5,L6,L3,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I5,I6,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx426135(L4,L2,L6,L1,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L6,L1,L3,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I6,I1,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx426153(L4,L2,L6,L1,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L6,L1,L5,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I6,I1,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx426315(L4,L2,L6,L3,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L6,L3,L1,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I6,I3,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx426351(L4,L2,L6,L3,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L6,L3,L5,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I6,I3,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx426513(L4,L2,L6,L5,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L6,L5,L1,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I6,I5,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx426531(L4,L2,L6,L5,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L6,L5,L3,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I6,I5,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx431256(L4,L3,L1,L2,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L1,L2,L5,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I1,I2,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx431265(L4,L3,L1,L2,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L1,L2,L6,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I1,I2,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx431526(L4,L3,L1,L5,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L1,L5,L2,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I1,I5,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx431562(L4,L3,L1,L5,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L1,L5,L6,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I1,I5,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx431625(L4,L3,L1,L6,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L1,L6,L2,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I1,I6,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx431652(L4,L3,L1,L6,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L1,L6,L5,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I1,I6,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx432156(L4,L3,L2,L1,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L2,L1,L5,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I2,I1,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx432165(L4,L3,L2,L1,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L2,L1,L6,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I2,I1,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx432516(L4,L3,L2,L5,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L2,L5,L1,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I2,I5,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx432561(L4,L3,L2,L5,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L2,L5,L6,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I2,I5,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx432615(L4,L3,L2,L6,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L2,L6,L1,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I2,I6,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx432651(L4,L3,L2,L6,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L2,L6,L5,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I2,I6,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx435126(L4,L3,L5,L1,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L5,L1,L2,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I5,I1,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx435162(L4,L3,L5,L1,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L5,L1,L6,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I5,I1,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx435216(L4,L3,L5,L2,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L5,L2,L1,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I5,I2,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx435261(L4,L3,L5,L2,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L5,L2,L6,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I5,I2,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx435612(L4,L3,L5,L6,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L5,L6,L1,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I5,I6,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx435621(L4,L3,L5,L6,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L5,L6,L2,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I5,I6,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx436125(L4,L3,L6,L1,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L6,L1,L2,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I6,I1,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx436152(L4,L3,L6,L1,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L6,L1,L5,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I6,I1,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx436215(L4,L3,L6,L2,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L6,L2,L1,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I6,I2,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx436251(L4,L3,L6,L2,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L6,L2,L5,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I6,I2,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx436512(L4,L3,L6,L5,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L6,L5,L1,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I6,I5,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx436521(L4,L3,L6,L5,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L6,L5,L2,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I6,I5,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx451236(L4,L5,L1,L2,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L1,L2,L3,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I1,I2,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx451263(L4,L5,L1,L2,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L1,L2,L6,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I1,I2,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx451326(L4,L5,L1,L3,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L1,L3,L2,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I1,I3,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx451362(L4,L5,L1,L3,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L1,L3,L6,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I1,I3,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx451623(L4,L5,L1,L6,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L1,L6,L2,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I1,I6,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx451632(L4,L5,L1,L6,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L1,L6,L3,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I1,I6,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx452136(L4,L5,L2,L1,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L2,L1,L3,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I2,I1,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx452163(L4,L5,L2,L1,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L2,L1,L6,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I2,I1,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx452316(L4,L5,L2,L3,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L2,L3,L1,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I2,I3,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx452361(L4,L5,L2,L3,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L2,L3,L6,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I2,I3,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx452613(L4,L5,L2,L6,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L2,L6,L1,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I2,I6,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx452631(L4,L5,L2,L6,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L2,L6,L3,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I2,I6,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx453126(L4,L5,L3,L1,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L3,L1,L2,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I3,I1,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx453162(L4,L5,L3,L1,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L3,L1,L6,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I3,I1,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx453216(L4,L5,L3,L2,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L3,L2,L1,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I3,I2,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx453261(L4,L5,L3,L2,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L3,L2,L6,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I3,I2,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx453612(L4,L5,L3,L6,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L3,L6,L1,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I3,I6,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx453621(L4,L5,L3,L6,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L3,L6,L2,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I3,I6,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx456123(L4,L5,L6,L1,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L6,L1,L2,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I6,I1,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx456132(L4,L5,L6,L1,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L6,L1,L3,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I6,I1,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx456213(L4,L5,L6,L2,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L6,L2,L1,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I6,I2,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx456231(L4,L5,L6,L2,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L6,L2,L3,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I6,I2,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx456312(L4,L5,L6,L3,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L6,L3,L1,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I6,I3,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx456321(L4,L5,L6,L3,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L6,L3,L2,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I6,I3,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx461235(L4,L6,L1,L2,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L1,L2,L3,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I1,I2,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx461253(L4,L6,L1,L2,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L1,L2,L5,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I1,I2,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx461325(L4,L6,L1,L3,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L1,L3,L2,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I1,I3,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx461352(L4,L6,L1,L3,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L1,L3,L5,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I1,I3,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx461523(L4,L6,L1,L5,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L1,L5,L2,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I1,I5,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx461532(L4,L6,L1,L5,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L1,L5,L3,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I1,I5,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx462135(L4,L6,L2,L1,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L2,L1,L3,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I2,I1,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx462153(L4,L6,L2,L1,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L2,L1,L5,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I2,I1,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx462315(L4,L6,L2,L3,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L2,L3,L1,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I2,I3,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx462351(L4,L6,L2,L3,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L2,L3,L5,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I2,I3,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx462513(L4,L6,L2,L5,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L2,L5,L1,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I2,I5,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx462531(L4,L6,L2,L5,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L2,L5,L3,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I2,I5,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx463125(L4,L6,L3,L1,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L3,L1,L2,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I3,I1,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx463152(L4,L6,L3,L1,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L3,L1,L5,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I3,I1,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx463215(L4,L6,L3,L2,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L3,L2,L1,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I3,I2,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx463251(L4,L6,L3,L2,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L3,L2,L5,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I3,I2,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx463512(L4,L6,L3,L5,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L3,L5,L1,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I3,I5,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx463521(L4,L6,L3,L5,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L3,L5,L2,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I3,I5,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx465123(L4,L6,L5,L1,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L5,L1,L2,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I5,I1,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx465132(L4,L6,L5,L1,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L5,L1,L3,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I5,I1,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx465213(L4,L6,L5,L2,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L5,L2,L1,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I5,I2,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx465231(L4,L6,L5,L2,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L5,L2,L3,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I5,I2,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx465312(L4,L6,L5,L3,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L5,L3,L1,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I5,I3,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx465321(L4,L6,L5,L3,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L5,L3,L2,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I5,I3,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx512346(L5,L1,L2,L3,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L2,L3,L4,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I2,I3,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx512364(L5,L1,L2,L3,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L2,L3,L6,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I2,I3,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx512436(L5,L1,L2,L4,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L2,L4,L3,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I2,I4,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx512463(L5,L1,L2,L4,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L2,L4,L6,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I2,I4,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx512634(L5,L1,L2,L6,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L2,L6,L3,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I2,I6,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx512643(L5,L1,L2,L6,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L2,L6,L4,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I2,I6,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx513246(L5,L1,L3,L2,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L3,L2,L4,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I3,I2,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx513264(L5,L1,L3,L2,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L3,L2,L6,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I3,I2,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx513426(L5,L1,L3,L4,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L3,L4,L2,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I3,I4,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx513462(L5,L1,L3,L4,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L3,L4,L6,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I3,I4,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx513624(L5,L1,L3,L6,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L3,L6,L2,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I3,I6,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx513642(L5,L1,L3,L6,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L3,L6,L4,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I3,I6,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx514236(L5,L1,L4,L2,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L4,L2,L3,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I4,I2,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx514263(L5,L1,L4,L2,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L4,L2,L6,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I4,I2,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx514326(L5,L1,L4,L3,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L4,L3,L2,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I4,I3,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx514362(L5,L1,L4,L3,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L4,L3,L6,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I4,I3,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx514623(L5,L1,L4,L6,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L4,L6,L2,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I4,I6,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx514632(L5,L1,L4,L6,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L4,L6,L3,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I4,I6,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx516234(L5,L1,L6,L2,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L6,L2,L3,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I6,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx516243(L5,L1,L6,L2,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L6,L2,L4,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I6,I2,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx516324(L5,L1,L6,L3,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L6,L3,L2,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I6,I3,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx516342(L5,L1,L6,L3,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L6,L3,L4,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I6,I3,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx516423(L5,L1,L6,L4,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L6,L4,L2,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I6,I4,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx516432(L5,L1,L6,L4,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L6,L4,L3,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I6,I4,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx521346(L5,L2,L1,L3,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L1,L3,L4,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I1,I3,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx521364(L5,L2,L1,L3,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L1,L3,L6,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I1,I3,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx521436(L5,L2,L1,L4,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L1,L4,L3,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I1,I4,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx521463(L5,L2,L1,L4,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L1,L4,L6,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I1,I4,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx521634(L5,L2,L1,L6,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L1,L6,L3,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I1,I6,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx521643(L5,L2,L1,L6,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L1,L6,L4,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I1,I6,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx523146(L5,L2,L3,L1,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L3,L1,L4,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I3,I1,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx523164(L5,L2,L3,L1,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L3,L1,L6,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I3,I1,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx523416(L5,L2,L3,L4,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L3,L4,L1,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I3,I4,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx523461(L5,L2,L3,L4,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L3,L4,L6,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I3,I4,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx523614(L5,L2,L3,L6,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L3,L6,L1,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I3,I6,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx523641(L5,L2,L3,L6,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L3,L6,L4,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I3,I6,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx524136(L5,L2,L4,L1,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L4,L1,L3,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I4,I1,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx524163(L5,L2,L4,L1,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L4,L1,L6,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I4,I1,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx524316(L5,L2,L4,L3,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L4,L3,L1,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I4,I3,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx524361(L5,L2,L4,L3,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L4,L3,L6,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I4,I3,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx524613(L5,L2,L4,L6,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L4,L6,L1,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I4,I6,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx524631(L5,L2,L4,L6,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L4,L6,L3,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I4,I6,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx526134(L5,L2,L6,L1,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L6,L1,L3,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I6,I1,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx526143(L5,L2,L6,L1,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L6,L1,L4,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I6,I1,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx526314(L5,L2,L6,L3,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L6,L3,L1,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I6,I3,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx526341(L5,L2,L6,L3,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L6,L3,L4,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I6,I3,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx526413(L5,L2,L6,L4,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L6,L4,L1,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I6,I4,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx526431(L5,L2,L6,L4,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L6,L4,L3,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I6,I4,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx531246(L5,L3,L1,L2,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L1,L2,L4,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I1,I2,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx531264(L5,L3,L1,L2,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L1,L2,L6,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I1,I2,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx531426(L5,L3,L1,L4,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L1,L4,L2,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I1,I4,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx531462(L5,L3,L1,L4,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L1,L4,L6,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I1,I4,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx531624(L5,L3,L1,L6,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L1,L6,L2,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I1,I6,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx531642(L5,L3,L1,L6,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L1,L6,L4,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I1,I6,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx532146(L5,L3,L2,L1,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L2,L1,L4,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I2,I1,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx532164(L5,L3,L2,L1,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L2,L1,L6,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I2,I1,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx532416(L5,L3,L2,L4,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L2,L4,L1,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I2,I4,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx532461(L5,L3,L2,L4,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L2,L4,L6,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I2,I4,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx532614(L5,L3,L2,L6,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L2,L6,L1,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I2,I6,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx532641(L5,L3,L2,L6,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L2,L6,L4,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I2,I6,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx534126(L5,L3,L4,L1,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L4,L1,L2,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I4,I1,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx534162(L5,L3,L4,L1,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L4,L1,L6,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I4,I1,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx534216(L5,L3,L4,L2,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L4,L2,L1,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I4,I2,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx534261(L5,L3,L4,L2,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L4,L2,L6,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I4,I2,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx534612(L5,L3,L4,L6,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L4,L6,L1,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I4,I6,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx534621(L5,L3,L4,L6,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L4,L6,L2,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I4,I6,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx536124(L5,L3,L6,L1,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L6,L1,L2,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I6,I1,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx536142(L5,L3,L6,L1,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L6,L1,L4,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I6,I1,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx536214(L5,L3,L6,L2,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L6,L2,L1,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I6,I2,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx536241(L5,L3,L6,L2,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L6,L2,L4,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I6,I2,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx536412(L5,L3,L6,L4,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L6,L4,L1,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I6,I4,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx536421(L5,L3,L6,L4,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L6,L4,L2,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I6,I4,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx541236(L5,L4,L1,L2,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L1,L2,L3,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I1,I2,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx541263(L5,L4,L1,L2,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L1,L2,L6,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I1,I2,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx541326(L5,L4,L1,L3,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L1,L3,L2,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I1,I3,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx541362(L5,L4,L1,L3,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L1,L3,L6,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I1,I3,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx541623(L5,L4,L1,L6,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L1,L6,L2,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I1,I6,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx541632(L5,L4,L1,L6,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L1,L6,L3,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I1,I6,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx542136(L5,L4,L2,L1,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L2,L1,L3,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I2,I1,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx542163(L5,L4,L2,L1,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L2,L1,L6,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I2,I1,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx542316(L5,L4,L2,L3,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L2,L3,L1,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I2,I3,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx542361(L5,L4,L2,L3,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L2,L3,L6,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I2,I3,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx542613(L5,L4,L2,L6,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L2,L6,L1,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I2,I6,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx542631(L5,L4,L2,L6,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L2,L6,L3,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I2,I6,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx543126(L5,L4,L3,L1,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L3,L1,L2,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I3,I1,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx543162(L5,L4,L3,L1,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L3,L1,L6,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I3,I1,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx543216(L5,L4,L3,L2,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L3,L2,L1,L6)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I3,I2,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx543261(L5,L4,L3,L2,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L3,L2,L6,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I3,I2,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx543612(L5,L4,L3,L6,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L3,L6,L1,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I3,I6,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx543621(L5,L4,L3,L6,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L3,L6,L2,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I3,I6,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx546123(L5,L4,L6,L1,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L6,L1,L2,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I6,I1,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx546132(L5,L4,L6,L1,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L6,L1,L3,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I6,I1,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx546213(L5,L4,L6,L2,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L6,L2,L1,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I6,I2,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx546231(L5,L4,L6,L2,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L6,L2,L3,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I6,I2,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx546312(L5,L4,L6,L3,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L6,L3,L1,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I6,I3,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx546321(L5,L4,L6,L3,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L6,L3,L2,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I6,I3,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx561234(L5,L6,L1,L2,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L1,L2,L3,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx561243(L5,L6,L1,L2,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L1,L2,L4,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I1,I2,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx561324(L5,L6,L1,L3,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L1,L3,L2,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I1,I3,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx561342(L5,L6,L1,L3,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L1,L3,L4,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I1,I3,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx561423(L5,L6,L1,L4,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L1,L4,L2,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I1,I4,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx561432(L5,L6,L1,L4,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L1,L4,L3,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I1,I4,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx562134(L5,L6,L2,L1,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L2,L1,L3,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I2,I1,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx562143(L5,L6,L2,L1,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L2,L1,L4,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I2,I1,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx562314(L5,L6,L2,L3,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L2,L3,L1,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I2,I3,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx562341(L5,L6,L2,L3,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L2,L3,L4,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I2,I3,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx562413(L5,L6,L2,L4,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L2,L4,L1,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I2,I4,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx562431(L5,L6,L2,L4,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L2,L4,L3,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I2,I4,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx563124(L5,L6,L3,L1,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L3,L1,L2,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I3,I1,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx563142(L5,L6,L3,L1,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L3,L1,L4,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I3,I1,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx563214(L5,L6,L3,L2,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L3,L2,L1,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I3,I2,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx563241(L5,L6,L3,L2,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L3,L2,L4,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I3,I2,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx563412(L5,L6,L3,L4,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L3,L4,L1,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I3,I4,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx563421(L5,L6,L3,L4,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L3,L4,L2,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I3,I4,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx564123(L5,L6,L4,L1,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L4,L1,L2,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I4,I1,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx564132(L5,L6,L4,L1,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L4,L1,L3,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I4,I1,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx564213(L5,L6,L4,L2,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L4,L2,L1,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I4,I2,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx564231(L5,L6,L4,L2,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L4,L2,L3,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I4,I2,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx564312(L5,L6,L4,L3,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L4,L3,L1,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I4,I3,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx564321(L5,L6,L4,L3,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L4,L3,L2,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I4,I3,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx612345(L6,L1,L2,L3,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L2,L3,L4,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx612354(L6,L1,L2,L3,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L2,L3,L5,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I2,I3,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx612435(L6,L1,L2,L4,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L2,L4,L3,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I2,I4,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx612453(L6,L1,L2,L4,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L2,L4,L5,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I2,I4,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx612534(L6,L1,L2,L5,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L2,L5,L3,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I2,I5,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx612543(L6,L1,L2,L5,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L2,L5,L4,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I2,I5,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx613245(L6,L1,L3,L2,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L3,L2,L4,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I3,I2,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx613254(L6,L1,L3,L2,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L3,L2,L5,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I3,I2,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx613425(L6,L1,L3,L4,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L3,L4,L2,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I3,I4,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx613452(L6,L1,L3,L4,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L3,L4,L5,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I3,I4,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx613524(L6,L1,L3,L5,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L3,L5,L2,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I3,I5,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx613542(L6,L1,L3,L5,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L3,L5,L4,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I3,I5,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx614235(L6,L1,L4,L2,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L4,L2,L3,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I4,I2,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx614253(L6,L1,L4,L2,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L4,L2,L5,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I4,I2,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx614325(L6,L1,L4,L3,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L4,L3,L2,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I4,I3,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx614352(L6,L1,L4,L3,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L4,L3,L5,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I4,I3,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx614523(L6,L1,L4,L5,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L4,L5,L2,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I4,I5,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx614532(L6,L1,L4,L5,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L4,L5,L3,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I4,I5,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx615234(L6,L1,L5,L2,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L5,L2,L3,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I5,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx615243(L6,L1,L5,L2,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L5,L2,L4,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I5,I2,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx615324(L6,L1,L5,L3,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L5,L3,L2,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I5,I3,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx615342(L6,L1,L5,L3,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L5,L3,L4,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I5,I3,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx615423(L6,L1,L5,L4,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L5,L4,L2,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I5,I4,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx615432(L6,L1,L5,L4,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L5,L4,L3,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I5,I4,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx621345(L6,L2,L1,L3,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L1,L3,L4,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I1,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx621354(L6,L2,L1,L3,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L1,L3,L5,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I1,I3,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx621435(L6,L2,L1,L4,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L1,L4,L3,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I1,I4,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx621453(L6,L2,L1,L4,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L1,L4,L5,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I1,I4,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx621534(L6,L2,L1,L5,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L1,L5,L3,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I1,I5,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx621543(L6,L2,L1,L5,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L1,L5,L4,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I1,I5,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx623145(L6,L2,L3,L1,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L3,L1,L4,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I3,I1,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx623154(L6,L2,L3,L1,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L3,L1,L5,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I3,I1,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx623415(L6,L2,L3,L4,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L3,L4,L1,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I3,I4,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx623451(L6,L2,L3,L4,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L3,L4,L5,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I3,I4,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx623514(L6,L2,L3,L5,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L3,L5,L1,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I3,I5,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx623541(L6,L2,L3,L5,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L3,L5,L4,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I3,I5,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx624135(L6,L2,L4,L1,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L4,L1,L3,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I4,I1,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx624153(L6,L2,L4,L1,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L4,L1,L5,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I4,I1,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx624315(L6,L2,L4,L3,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L4,L3,L1,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I4,I3,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx624351(L6,L2,L4,L3,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L4,L3,L5,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I4,I3,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx624513(L6,L2,L4,L5,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L4,L5,L1,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I4,I5,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx624531(L6,L2,L4,L5,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L4,L5,L3,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I4,I5,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx625134(L6,L2,L5,L1,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L5,L1,L3,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I5,I1,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx625143(L6,L2,L5,L1,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L5,L1,L4,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I5,I1,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx625314(L6,L2,L5,L3,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L5,L3,L1,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I5,I3,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx625341(L6,L2,L5,L3,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L5,L3,L4,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I5,I3,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx625413(L6,L2,L5,L4,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L5,L4,L1,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I5,I4,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx625431(L6,L2,L5,L4,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L5,L4,L3,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I5,I4,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx631245(L6,L3,L1,L2,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L1,L2,L4,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I1,I2,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx631254(L6,L3,L1,L2,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L1,L2,L5,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I1,I2,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx631425(L6,L3,L1,L4,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L1,L4,L2,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I1,I4,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx631452(L6,L3,L1,L4,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L1,L4,L5,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I1,I4,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx631524(L6,L3,L1,L5,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L1,L5,L2,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I1,I5,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx631542(L6,L3,L1,L5,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L1,L5,L4,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I1,I5,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx632145(L6,L3,L2,L1,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L2,L1,L4,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I2,I1,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx632154(L6,L3,L2,L1,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L2,L1,L5,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I2,I1,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx632415(L6,L3,L2,L4,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L2,L4,L1,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I2,I4,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx632451(L6,L3,L2,L4,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L2,L4,L5,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I2,I4,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx632514(L6,L3,L2,L5,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L2,L5,L1,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I2,I5,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx632541(L6,L3,L2,L5,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L2,L5,L4,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I2,I5,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx634125(L6,L3,L4,L1,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L4,L1,L2,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I4,I1,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx634152(L6,L3,L4,L1,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L4,L1,L5,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I4,I1,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx634215(L6,L3,L4,L2,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L4,L2,L1,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I4,I2,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx634251(L6,L3,L4,L2,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L4,L2,L5,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I4,I2,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx634512(L6,L3,L4,L5,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L4,L5,L1,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I4,I5,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx634521(L6,L3,L4,L5,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L4,L5,L2,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I4,I5,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx635124(L6,L3,L5,L1,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L5,L1,L2,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I5,I1,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx635142(L6,L3,L5,L1,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L5,L1,L4,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I5,I1,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx635214(L6,L3,L5,L2,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L5,L2,L1,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I5,I2,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx635241(L6,L3,L5,L2,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L5,L2,L4,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I5,I2,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx635412(L6,L3,L5,L4,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L5,L4,L1,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I5,I4,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx635421(L6,L3,L5,L4,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L5,L4,L2,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I5,I4,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx641235(L6,L4,L1,L2,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L1,L2,L3,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I1,I2,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx641253(L6,L4,L1,L2,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L1,L2,L5,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I1,I2,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx641325(L6,L4,L1,L3,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L1,L3,L2,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I1,I3,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx641352(L6,L4,L1,L3,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L1,L3,L5,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I1,I3,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx641523(L6,L4,L1,L5,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L1,L5,L2,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I1,I5,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx641532(L6,L4,L1,L5,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L1,L5,L3,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I1,I5,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx642135(L6,L4,L2,L1,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L2,L1,L3,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I2,I1,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx642153(L6,L4,L2,L1,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L2,L1,L5,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I2,I1,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx642315(L6,L4,L2,L3,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L2,L3,L1,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I2,I3,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx642351(L6,L4,L2,L3,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L2,L3,L5,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I2,I3,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx642513(L6,L4,L2,L5,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L2,L5,L1,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I2,I5,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx642531(L6,L4,L2,L5,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L2,L5,L3,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I2,I5,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx643125(L6,L4,L3,L1,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L3,L1,L2,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I3,I1,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx643152(L6,L4,L3,L1,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L3,L1,L5,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I3,I1,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx643215(L6,L4,L3,L2,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L3,L2,L1,L5)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I3,I2,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx643251(L6,L4,L3,L2,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L3,L2,L5,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I3,I2,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx643512(L6,L4,L3,L5,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L3,L5,L1,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I3,I5,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx643521(L6,L4,L3,L5,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L3,L5,L2,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I3,I5,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx645123(L6,L4,L5,L1,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L5,L1,L2,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I5,I1,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx645132(L6,L4,L5,L1,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L5,L1,L3,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I5,I1,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx645213(L6,L4,L5,L2,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L5,L2,L1,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I5,I2,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx645231(L6,L4,L5,L2,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L5,L2,L3,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I5,I2,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx645312(L6,L4,L5,L3,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L5,L3,L1,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I5,I3,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx645321(L6,L4,L5,L3,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L5,L3,L2,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I5,I3,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx651234(L6,L5,L1,L2,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L1,L2,L3,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx651243(L6,L5,L1,L2,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L1,L2,L4,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I1,I2,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx651324(L6,L5,L1,L3,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L1,L3,L2,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I1,I3,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx651342(L6,L5,L1,L3,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L1,L3,L4,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I1,I3,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx651423(L6,L5,L1,L4,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L1,L4,L2,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I1,I4,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx651432(L6,L5,L1,L4,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L1,L4,L3,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I1,I4,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx652134(L6,L5,L2,L1,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L2,L1,L3,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I2,I1,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx652143(L6,L5,L2,L1,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L2,L1,L4,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I2,I1,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx652314(L6,L5,L2,L3,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L2,L3,L1,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I2,I3,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx652341(L6,L5,L2,L3,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L2,L3,L4,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I2,I3,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx652413(L6,L5,L2,L4,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L2,L4,L1,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I2,I4,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx652431(L6,L5,L2,L4,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L2,L4,L3,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I2,I4,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx653124(L6,L5,L3,L1,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L3,L1,L2,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I3,I1,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx653142(L6,L5,L3,L1,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L3,L1,L4,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I3,I1,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx653214(L6,L5,L3,L2,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L3,L2,L1,L4)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I3,I2,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx653241(L6,L5,L3,L2,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L3,L2,L4,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I3,I2,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx653412(L6,L5,L3,L4,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L3,L4,L1,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I3,I4,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx653421(L6,L5,L3,L4,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L3,L4,L2,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I3,I4,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx654123(L6,L5,L4,L1,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L4,L1,L2,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I4,I1,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx654132(L6,L5,L4,L1,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L4,L1,L3,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I4,I1,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx654213(L6,L5,L4,L2,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L4,L2,L1,L3)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I4,I2,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx654231(L6,L5,L4,L2,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L4,L2,L3,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I4,I2,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx654312(L6,L5,L4,L3,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L4,L3,L1,L2)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I4,I3,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine sumx654321(L6,L5,L4,L3,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L4,L3,L2,L1)
       real C
C
       do I1=1,K1
       do I2=1,K2
       do I3=1,K3
       do I4=1,K4
       do I5=1,K5
       do I6=1,K6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I4,I3,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
