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
