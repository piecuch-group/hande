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
