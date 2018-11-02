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
