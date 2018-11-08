       subroutine slx12(N0,N3,K1,L1,K2,L2,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2)
       real*8 B(N0+1:N3,N0+1:N3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
        A(I1,I2)=A(I1,I2)+C*B(I1,I2)
       enddo
       enddo
C
       end
C
       subroutine slx21(N0,N3,K1,L1,K2,L2,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2)
       real*8 B(N0+1:N3,N0+1:N3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
        A(I1,I2)=A(I1,I2)+C*B(I2,I1)
       enddo
       enddo
C
       end
C
       subroutine slx1234(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 C
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
       subroutine slx1243(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 C
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
       subroutine slx1324(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 C
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
       subroutine slx1342(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 C
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
       subroutine slx1423(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 C
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
       subroutine slx1432(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 C
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
       subroutine slx2134(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 C
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
       subroutine slx2143(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 C
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
       subroutine slx2314(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 C
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
       subroutine slx2341(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 C
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
       subroutine slx2413(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 C
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
       subroutine slx2431(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 C
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
       subroutine slx3124(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 C
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
       subroutine slx3142(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 C
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
       subroutine slx3214(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 C
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
       subroutine slx3241(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 C
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
       subroutine slx3412(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 C
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
       subroutine slx3421(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 C
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
       subroutine slx4123(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 C
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
       subroutine slx4132(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 C
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
       subroutine slx4213(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 C
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
       subroutine slx4231(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 C
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
       subroutine slx4312(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 C
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
       subroutine slx4321(N0,N3,K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 C
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
       subroutine slx123456(L1,L2,L3,L4,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L3,L4,L5,L6)
       real*8 C
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
       subroutine slx123465(L1,L2,L3,L4,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L3,L4,L6,L5)
       real*8 C
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
       subroutine slx123546(L1,L2,L3,L5,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L3,L5,L4,L6)
       real*8 C
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
       subroutine slx123564(L1,L2,L3,L5,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L3,L5,L6,L4)
       real*8 C
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
       subroutine slx123645(L1,L2,L3,L6,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L3,L6,L4,L5)
       real*8 C
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
       subroutine slx123654(L1,L2,L3,L6,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L3,L6,L5,L4)
       real*8 C
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
       subroutine slx124356(L1,L2,L4,L3,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L4,L3,L5,L6)
       real*8 C
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
       subroutine slx124365(L1,L2,L4,L3,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L4,L3,L6,L5)
       real*8 C
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
       subroutine slx124536(L1,L2,L4,L5,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L4,L5,L3,L6)
       real*8 C
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
       subroutine slx124563(L1,L2,L4,L5,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L4,L5,L6,L3)
       real*8 C
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
       subroutine slx124635(L1,L2,L4,L6,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L4,L6,L3,L5)
       real*8 C
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
       subroutine slx124653(L1,L2,L4,L6,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L4,L6,L5,L3)
       real*8 C
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
       subroutine slx125346(L1,L2,L5,L3,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L5,L3,L4,L6)
       real*8 C
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
       subroutine slx125364(L1,L2,L5,L3,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L5,L3,L6,L4)
       real*8 C
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
       subroutine slx125436(L1,L2,L5,L4,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L5,L4,L3,L6)
       real*8 C
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
       subroutine slx125463(L1,L2,L5,L4,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L5,L4,L6,L3)
       real*8 C
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
       subroutine slx125634(L1,L2,L5,L6,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L5,L6,L3,L4)
       real*8 C
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
       subroutine slx125643(L1,L2,L5,L6,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L5,L6,L4,L3)
       real*8 C
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
       subroutine slx126345(L1,L2,L6,L3,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L6,L3,L4,L5)
       real*8 C
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
       subroutine slx126354(L1,L2,L6,L3,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L6,L3,L5,L4)
       real*8 C
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
       subroutine slx126435(L1,L2,L6,L4,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L6,L4,L3,L5)
       real*8 C
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
       subroutine slx126453(L1,L2,L6,L4,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L6,L4,L5,L3)
       real*8 C
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
       subroutine slx126534(L1,L2,L6,L5,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L6,L5,L3,L4)
       real*8 C
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
       subroutine slx126543(L1,L2,L6,L5,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L2,L6,L5,L4,L3)
       real*8 C
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
       subroutine slx132456(L1,L3,L2,L4,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L2,L4,L5,L6)
       real*8 C
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
       subroutine slx132465(L1,L3,L2,L4,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L2,L4,L6,L5)
       real*8 C
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
       subroutine slx132546(L1,L3,L2,L5,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L2,L5,L4,L6)
       real*8 C
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
       subroutine slx132564(L1,L3,L2,L5,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L2,L5,L6,L4)
       real*8 C
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
       subroutine slx132645(L1,L3,L2,L6,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L2,L6,L4,L5)
       real*8 C
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
       subroutine slx132654(L1,L3,L2,L6,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L2,L6,L5,L4)
       real*8 C
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
       subroutine slx134256(L1,L3,L4,L2,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L4,L2,L5,L6)
       real*8 C
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
       subroutine slx134265(L1,L3,L4,L2,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L4,L2,L6,L5)
       real*8 C
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
       subroutine slx134526(L1,L3,L4,L5,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L4,L5,L2,L6)
       real*8 C
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
       subroutine slx134562(L1,L3,L4,L5,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L4,L5,L6,L2)
       real*8 C
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
       subroutine slx134625(L1,L3,L4,L6,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L4,L6,L2,L5)
       real*8 C
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
       subroutine slx134652(L1,L3,L4,L6,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L4,L6,L5,L2)
       real*8 C
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
       subroutine slx135246(L1,L3,L5,L2,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L5,L2,L4,L6)
       real*8 C
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
       subroutine slx135264(L1,L3,L5,L2,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L5,L2,L6,L4)
       real*8 C
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
       subroutine slx135426(L1,L3,L5,L4,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L5,L4,L2,L6)
       real*8 C
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
       subroutine slx135462(L1,L3,L5,L4,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L5,L4,L6,L2)
       real*8 C
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
       subroutine slx135624(L1,L3,L5,L6,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L5,L6,L2,L4)
       real*8 C
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
       subroutine slx135642(L1,L3,L5,L6,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L5,L6,L4,L2)
       real*8 C
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
       subroutine slx136245(L1,L3,L6,L2,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L6,L2,L4,L5)
       real*8 C
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
       subroutine slx136254(L1,L3,L6,L2,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L6,L2,L5,L4)
       real*8 C
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
       subroutine slx136425(L1,L3,L6,L4,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L6,L4,L2,L5)
       real*8 C
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
       subroutine slx136452(L1,L3,L6,L4,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L6,L4,L5,L2)
       real*8 C
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
       subroutine slx136524(L1,L3,L6,L5,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L6,L5,L2,L4)
       real*8 C
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
       subroutine slx136542(L1,L3,L6,L5,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L3,L6,L5,L4,L2)
       real*8 C
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
       subroutine slx142356(L1,L4,L2,L3,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L2,L3,L5,L6)
       real*8 C
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
       subroutine slx142365(L1,L4,L2,L3,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L2,L3,L6,L5)
       real*8 C
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
       subroutine slx142536(L1,L4,L2,L5,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L2,L5,L3,L6)
       real*8 C
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
       subroutine slx142563(L1,L4,L2,L5,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L2,L5,L6,L3)
       real*8 C
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
       subroutine slx142635(L1,L4,L2,L6,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L2,L6,L3,L5)
       real*8 C
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
       subroutine slx142653(L1,L4,L2,L6,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L2,L6,L5,L3)
       real*8 C
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
       subroutine slx143256(L1,L4,L3,L2,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L3,L2,L5,L6)
       real*8 C
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
       subroutine slx143265(L1,L4,L3,L2,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L3,L2,L6,L5)
       real*8 C
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
       subroutine slx143526(L1,L4,L3,L5,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L3,L5,L2,L6)
       real*8 C
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
       subroutine slx143562(L1,L4,L3,L5,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L3,L5,L6,L2)
       real*8 C
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
       subroutine slx143625(L1,L4,L3,L6,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L3,L6,L2,L5)
       real*8 C
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
       subroutine slx143652(L1,L4,L3,L6,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L3,L6,L5,L2)
       real*8 C
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
       subroutine slx145236(L1,L4,L5,L2,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L5,L2,L3,L6)
       real*8 C
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
       subroutine slx145263(L1,L4,L5,L2,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L5,L2,L6,L3)
       real*8 C
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
       subroutine slx145326(L1,L4,L5,L3,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L5,L3,L2,L6)
       real*8 C
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
       subroutine slx145362(L1,L4,L5,L3,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L5,L3,L6,L2)
       real*8 C
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
       subroutine slx145623(L1,L4,L5,L6,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L5,L6,L2,L3)
       real*8 C
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
       subroutine slx145632(L1,L4,L5,L6,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L5,L6,L3,L2)
       real*8 C
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
       subroutine slx146235(L1,L4,L6,L2,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L6,L2,L3,L5)
       real*8 C
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
       subroutine slx146253(L1,L4,L6,L2,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L6,L2,L5,L3)
       real*8 C
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
       subroutine slx146325(L1,L4,L6,L3,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L6,L3,L2,L5)
       real*8 C
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
       subroutine slx146352(L1,L4,L6,L3,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L6,L3,L5,L2)
       real*8 C
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
       subroutine slx146523(L1,L4,L6,L5,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L6,L5,L2,L3)
       real*8 C
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
       subroutine slx146532(L1,L4,L6,L5,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L4,L6,L5,L3,L2)
       real*8 C
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
       subroutine slx152346(L1,L5,L2,L3,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L2,L3,L4,L6)
       real*8 C
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
       subroutine slx152364(L1,L5,L2,L3,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L2,L3,L6,L4)
       real*8 C
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
       subroutine slx152436(L1,L5,L2,L4,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L2,L4,L3,L6)
       real*8 C
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
       subroutine slx152463(L1,L5,L2,L4,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L2,L4,L6,L3)
       real*8 C
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
       subroutine slx152634(L1,L5,L2,L6,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L2,L6,L3,L4)
       real*8 C
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
       subroutine slx152643(L1,L5,L2,L6,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L2,L6,L4,L3)
       real*8 C
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
       subroutine slx153246(L1,L5,L3,L2,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L3,L2,L4,L6)
       real*8 C
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
       subroutine slx153264(L1,L5,L3,L2,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L3,L2,L6,L4)
       real*8 C
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
       subroutine slx153426(L1,L5,L3,L4,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L3,L4,L2,L6)
       real*8 C
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
       subroutine slx153462(L1,L5,L3,L4,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L3,L4,L6,L2)
       real*8 C
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
       subroutine slx153624(L1,L5,L3,L6,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L3,L6,L2,L4)
       real*8 C
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
       subroutine slx153642(L1,L5,L3,L6,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L3,L6,L4,L2)
       real*8 C
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
       subroutine slx154236(L1,L5,L4,L2,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L4,L2,L3,L6)
       real*8 C
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
       subroutine slx154263(L1,L5,L4,L2,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L4,L2,L6,L3)
       real*8 C
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
       subroutine slx154326(L1,L5,L4,L3,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L4,L3,L2,L6)
       real*8 C
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
       subroutine slx154362(L1,L5,L4,L3,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L4,L3,L6,L2)
       real*8 C
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
       subroutine slx154623(L1,L5,L4,L6,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L4,L6,L2,L3)
       real*8 C
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
       subroutine slx154632(L1,L5,L4,L6,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L4,L6,L3,L2)
       real*8 C
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
       subroutine slx156234(L1,L5,L6,L2,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L6,L2,L3,L4)
       real*8 C
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
       subroutine slx156243(L1,L5,L6,L2,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L6,L2,L4,L3)
       real*8 C
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
       subroutine slx156324(L1,L5,L6,L3,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L6,L3,L2,L4)
       real*8 C
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
       subroutine slx156342(L1,L5,L6,L3,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L6,L3,L4,L2)
       real*8 C
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
       subroutine slx156423(L1,L5,L6,L4,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L6,L4,L2,L3)
       real*8 C
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
       subroutine slx156432(L1,L5,L6,L4,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L5,L6,L4,L3,L2)
       real*8 C
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
       subroutine slx162345(L1,L6,L2,L3,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L2,L3,L4,L5)
       real*8 C
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
       subroutine slx162354(L1,L6,L2,L3,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L2,L3,L5,L4)
       real*8 C
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
       subroutine slx162435(L1,L6,L2,L4,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L2,L4,L3,L5)
       real*8 C
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
       subroutine slx162453(L1,L6,L2,L4,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L2,L4,L5,L3)
       real*8 C
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
       subroutine slx162534(L1,L6,L2,L5,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L2,L5,L3,L4)
       real*8 C
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
       subroutine slx162543(L1,L6,L2,L5,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L2,L5,L4,L3)
       real*8 C
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
       subroutine slx163245(L1,L6,L3,L2,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L3,L2,L4,L5)
       real*8 C
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
       subroutine slx163254(L1,L6,L3,L2,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L3,L2,L5,L4)
       real*8 C
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
       subroutine slx163425(L1,L6,L3,L4,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L3,L4,L2,L5)
       real*8 C
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
       subroutine slx163452(L1,L6,L3,L4,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L3,L4,L5,L2)
       real*8 C
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
       subroutine slx163524(L1,L6,L3,L5,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L3,L5,L2,L4)
       real*8 C
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
       subroutine slx163542(L1,L6,L3,L5,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L3,L5,L4,L2)
       real*8 C
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
       subroutine slx164235(L1,L6,L4,L2,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L4,L2,L3,L5)
       real*8 C
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
       subroutine slx164253(L1,L6,L4,L2,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L4,L2,L5,L3)
       real*8 C
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
       subroutine slx164325(L1,L6,L4,L3,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L4,L3,L2,L5)
       real*8 C
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
       subroutine slx164352(L1,L6,L4,L3,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L4,L3,L5,L2)
       real*8 C
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
       subroutine slx164523(L1,L6,L4,L5,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L4,L5,L2,L3)
       real*8 C
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
       subroutine slx164532(L1,L6,L4,L5,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L4,L5,L3,L2)
       real*8 C
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
       subroutine slx165234(L1,L6,L5,L2,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L5,L2,L3,L4)
       real*8 C
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
       subroutine slx165243(L1,L6,L5,L2,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L5,L2,L4,L3)
       real*8 C
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
       subroutine slx165324(L1,L6,L5,L3,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L5,L3,L2,L4)
       real*8 C
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
       subroutine slx165342(L1,L6,L5,L3,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L5,L3,L4,L2)
       real*8 C
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
       subroutine slx165423(L1,L6,L5,L4,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L5,L4,L2,L3)
       real*8 C
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
       subroutine slx165432(L1,L6,L5,L4,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L1,L6,L5,L4,L3,L2)
       real*8 C
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
       subroutine slx213456(L2,L1,L3,L4,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L3,L4,L5,L6)
       real*8 C
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
       subroutine slx213465(L2,L1,L3,L4,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L3,L4,L6,L5)
       real*8 C
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
       subroutine slx213546(L2,L1,L3,L5,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L3,L5,L4,L6)
       real*8 C
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
       subroutine slx213564(L2,L1,L3,L5,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L3,L5,L6,L4)
       real*8 C
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
       subroutine slx213645(L2,L1,L3,L6,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L3,L6,L4,L5)
       real*8 C
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
       subroutine slx213654(L2,L1,L3,L6,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L3,L6,L5,L4)
       real*8 C
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
       subroutine slx214356(L2,L1,L4,L3,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L4,L3,L5,L6)
       real*8 C
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
       subroutine slx214365(L2,L1,L4,L3,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L4,L3,L6,L5)
       real*8 C
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
       subroutine slx214536(L2,L1,L4,L5,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L4,L5,L3,L6)
       real*8 C
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
       subroutine slx214563(L2,L1,L4,L5,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L4,L5,L6,L3)
       real*8 C
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
       subroutine slx214635(L2,L1,L4,L6,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L4,L6,L3,L5)
       real*8 C
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
       subroutine slx214653(L2,L1,L4,L6,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L4,L6,L5,L3)
       real*8 C
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
       subroutine slx215346(L2,L1,L5,L3,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L5,L3,L4,L6)
       real*8 C
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
       subroutine slx215364(L2,L1,L5,L3,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L5,L3,L6,L4)
       real*8 C
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
       subroutine slx215436(L2,L1,L5,L4,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L5,L4,L3,L6)
       real*8 C
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
       subroutine slx215463(L2,L1,L5,L4,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L5,L4,L6,L3)
       real*8 C
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
       subroutine slx215634(L2,L1,L5,L6,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L5,L6,L3,L4)
       real*8 C
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
       subroutine slx215643(L2,L1,L5,L6,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L5,L6,L4,L3)
       real*8 C
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
       subroutine slx216345(L2,L1,L6,L3,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L6,L3,L4,L5)
       real*8 C
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
       subroutine slx216354(L2,L1,L6,L3,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L6,L3,L5,L4)
       real*8 C
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
       subroutine slx216435(L2,L1,L6,L4,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L6,L4,L3,L5)
       real*8 C
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
       subroutine slx216453(L2,L1,L6,L4,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L6,L4,L5,L3)
       real*8 C
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
       subroutine slx216534(L2,L1,L6,L5,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L6,L5,L3,L4)
       real*8 C
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
       subroutine slx216543(L2,L1,L6,L5,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L1,L6,L5,L4,L3)
       real*8 C
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
       subroutine slx231456(L2,L3,L1,L4,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L1,L4,L5,L6)
       real*8 C
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
       subroutine slx231465(L2,L3,L1,L4,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L1,L4,L6,L5)
       real*8 C
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
       subroutine slx231546(L2,L3,L1,L5,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L1,L5,L4,L6)
       real*8 C
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
       subroutine slx231564(L2,L3,L1,L5,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L1,L5,L6,L4)
       real*8 C
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
       subroutine slx231645(L2,L3,L1,L6,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L1,L6,L4,L5)
       real*8 C
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
       subroutine slx231654(L2,L3,L1,L6,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L1,L6,L5,L4)
       real*8 C
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
       subroutine slx234156(L2,L3,L4,L1,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L4,L1,L5,L6)
       real*8 C
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
       subroutine slx234165(L2,L3,L4,L1,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L4,L1,L6,L5)
       real*8 C
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
       subroutine slx234516(L2,L3,L4,L5,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L4,L5,L1,L6)
       real*8 C
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
       subroutine slx234561(L2,L3,L4,L5,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L4,L5,L6,L1)
       real*8 C
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
       subroutine slx234615(L2,L3,L4,L6,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L4,L6,L1,L5)
       real*8 C
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
       subroutine slx234651(L2,L3,L4,L6,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L4,L6,L5,L1)
       real*8 C
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
       subroutine slx235146(L2,L3,L5,L1,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L5,L1,L4,L6)
       real*8 C
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
       subroutine slx235164(L2,L3,L5,L1,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L5,L1,L6,L4)
       real*8 C
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
       subroutine slx235416(L2,L3,L5,L4,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L5,L4,L1,L6)
       real*8 C
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
       subroutine slx235461(L2,L3,L5,L4,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L5,L4,L6,L1)
       real*8 C
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
       subroutine slx235614(L2,L3,L5,L6,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L5,L6,L1,L4)
       real*8 C
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
       subroutine slx235641(L2,L3,L5,L6,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L5,L6,L4,L1)
       real*8 C
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
       subroutine slx236145(L2,L3,L6,L1,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L6,L1,L4,L5)
       real*8 C
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
       subroutine slx236154(L2,L3,L6,L1,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L6,L1,L5,L4)
       real*8 C
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
       subroutine slx236415(L2,L3,L6,L4,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L6,L4,L1,L5)
       real*8 C
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
       subroutine slx236451(L2,L3,L6,L4,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L6,L4,L5,L1)
       real*8 C
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
       subroutine slx236514(L2,L3,L6,L5,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L6,L5,L1,L4)
       real*8 C
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
       subroutine slx236541(L2,L3,L6,L5,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L3,L6,L5,L4,L1)
       real*8 C
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
       subroutine slx241356(L2,L4,L1,L3,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L1,L3,L5,L6)
       real*8 C
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
       subroutine slx241365(L2,L4,L1,L3,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L1,L3,L6,L5)
       real*8 C
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
       subroutine slx241536(L2,L4,L1,L5,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L1,L5,L3,L6)
       real*8 C
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
       subroutine slx241563(L2,L4,L1,L5,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L1,L5,L6,L3)
       real*8 C
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
       subroutine slx241635(L2,L4,L1,L6,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L1,L6,L3,L5)
       real*8 C
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
       subroutine slx241653(L2,L4,L1,L6,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L1,L6,L5,L3)
       real*8 C
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
       subroutine slx243156(L2,L4,L3,L1,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L3,L1,L5,L6)
       real*8 C
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
       subroutine slx243165(L2,L4,L3,L1,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L3,L1,L6,L5)
       real*8 C
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
       subroutine slx243516(L2,L4,L3,L5,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L3,L5,L1,L6)
       real*8 C
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
       subroutine slx243561(L2,L4,L3,L5,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L3,L5,L6,L1)
       real*8 C
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
       subroutine slx243615(L2,L4,L3,L6,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L3,L6,L1,L5)
       real*8 C
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
       subroutine slx243651(L2,L4,L3,L6,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L3,L6,L5,L1)
       real*8 C
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
       subroutine slx245136(L2,L4,L5,L1,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L5,L1,L3,L6)
       real*8 C
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
       subroutine slx245163(L2,L4,L5,L1,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L5,L1,L6,L3)
       real*8 C
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
       subroutine slx245316(L2,L4,L5,L3,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L5,L3,L1,L6)
       real*8 C
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
       subroutine slx245361(L2,L4,L5,L3,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L5,L3,L6,L1)
       real*8 C
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
       subroutine slx245613(L2,L4,L5,L6,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L5,L6,L1,L3)
       real*8 C
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
       subroutine slx245631(L2,L4,L5,L6,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L5,L6,L3,L1)
       real*8 C
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
       subroutine slx246135(L2,L4,L6,L1,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L6,L1,L3,L5)
       real*8 C
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
       subroutine slx246153(L2,L4,L6,L1,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L6,L1,L5,L3)
       real*8 C
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
       subroutine slx246315(L2,L4,L6,L3,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L6,L3,L1,L5)
       real*8 C
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
       subroutine slx246351(L2,L4,L6,L3,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L6,L3,L5,L1)
       real*8 C
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
       subroutine slx246513(L2,L4,L6,L5,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L6,L5,L1,L3)
       real*8 C
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
       subroutine slx246531(L2,L4,L6,L5,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L4,L6,L5,L3,L1)
       real*8 C
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
       subroutine slx251346(L2,L5,L1,L3,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L1,L3,L4,L6)
       real*8 C
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
       subroutine slx251364(L2,L5,L1,L3,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L1,L3,L6,L4)
       real*8 C
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
       subroutine slx251436(L2,L5,L1,L4,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L1,L4,L3,L6)
       real*8 C
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
       subroutine slx251463(L2,L5,L1,L4,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L1,L4,L6,L3)
       real*8 C
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
       subroutine slx251634(L2,L5,L1,L6,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L1,L6,L3,L4)
       real*8 C
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
       subroutine slx251643(L2,L5,L1,L6,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L1,L6,L4,L3)
       real*8 C
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
       subroutine slx253146(L2,L5,L3,L1,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L3,L1,L4,L6)
       real*8 C
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
       subroutine slx253164(L2,L5,L3,L1,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L3,L1,L6,L4)
       real*8 C
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
       subroutine slx253416(L2,L5,L3,L4,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L3,L4,L1,L6)
       real*8 C
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
       subroutine slx253461(L2,L5,L3,L4,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L3,L4,L6,L1)
       real*8 C
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
       subroutine slx253614(L2,L5,L3,L6,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L3,L6,L1,L4)
       real*8 C
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
       subroutine slx253641(L2,L5,L3,L6,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L3,L6,L4,L1)
       real*8 C
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
       subroutine slx254136(L2,L5,L4,L1,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L4,L1,L3,L6)
       real*8 C
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
       subroutine slx254163(L2,L5,L4,L1,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L4,L1,L6,L3)
       real*8 C
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
       subroutine slx254316(L2,L5,L4,L3,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L4,L3,L1,L6)
       real*8 C
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
       subroutine slx254361(L2,L5,L4,L3,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L4,L3,L6,L1)
       real*8 C
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
       subroutine slx254613(L2,L5,L4,L6,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L4,L6,L1,L3)
       real*8 C
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
       subroutine slx254631(L2,L5,L4,L6,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L4,L6,L3,L1)
       real*8 C
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
       subroutine slx256134(L2,L5,L6,L1,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L6,L1,L3,L4)
       real*8 C
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
       subroutine slx256143(L2,L5,L6,L1,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L6,L1,L4,L3)
       real*8 C
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
       subroutine slx256314(L2,L5,L6,L3,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L6,L3,L1,L4)
       real*8 C
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
       subroutine slx256341(L2,L5,L6,L3,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L6,L3,L4,L1)
       real*8 C
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
       subroutine slx256413(L2,L5,L6,L4,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L6,L4,L1,L3)
       real*8 C
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
       subroutine slx256431(L2,L5,L6,L4,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L5,L6,L4,L3,L1)
       real*8 C
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
       subroutine slx261345(L2,L6,L1,L3,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L1,L3,L4,L5)
       real*8 C
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
       subroutine slx261354(L2,L6,L1,L3,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L1,L3,L5,L4)
       real*8 C
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
       subroutine slx261435(L2,L6,L1,L4,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L1,L4,L3,L5)
       real*8 C
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
       subroutine slx261453(L2,L6,L1,L4,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L1,L4,L5,L3)
       real*8 C
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
       subroutine slx261534(L2,L6,L1,L5,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L1,L5,L3,L4)
       real*8 C
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
       subroutine slx261543(L2,L6,L1,L5,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L1,L5,L4,L3)
       real*8 C
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
       subroutine slx263145(L2,L6,L3,L1,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L3,L1,L4,L5)
       real*8 C
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
       subroutine slx263154(L2,L6,L3,L1,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L3,L1,L5,L4)
       real*8 C
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
       subroutine slx263415(L2,L6,L3,L4,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L3,L4,L1,L5)
       real*8 C
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
       subroutine slx263451(L2,L6,L3,L4,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L3,L4,L5,L1)
       real*8 C
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
       subroutine slx263514(L2,L6,L3,L5,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L3,L5,L1,L4)
       real*8 C
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
       subroutine slx263541(L2,L6,L3,L5,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L3,L5,L4,L1)
       real*8 C
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
       subroutine slx264135(L2,L6,L4,L1,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L4,L1,L3,L5)
       real*8 C
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
       subroutine slx264153(L2,L6,L4,L1,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L4,L1,L5,L3)
       real*8 C
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
       subroutine slx264315(L2,L6,L4,L3,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L4,L3,L1,L5)
       real*8 C
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
       subroutine slx264351(L2,L6,L4,L3,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L4,L3,L5,L1)
       real*8 C
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
       subroutine slx264513(L2,L6,L4,L5,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L4,L5,L1,L3)
       real*8 C
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
       subroutine slx264531(L2,L6,L4,L5,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L4,L5,L3,L1)
       real*8 C
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
       subroutine slx265134(L2,L6,L5,L1,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L5,L1,L3,L4)
       real*8 C
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
       subroutine slx265143(L2,L6,L5,L1,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L5,L1,L4,L3)
       real*8 C
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
       subroutine slx265314(L2,L6,L5,L3,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L5,L3,L1,L4)
       real*8 C
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
       subroutine slx265341(L2,L6,L5,L3,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L5,L3,L4,L1)
       real*8 C
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
       subroutine slx265413(L2,L6,L5,L4,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L5,L4,L1,L3)
       real*8 C
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
       subroutine slx265431(L2,L6,L5,L4,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L2,L6,L5,L4,L3,L1)
       real*8 C
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
       subroutine slx312456(L3,L1,L2,L4,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L2,L4,L5,L6)
       real*8 C
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
       subroutine slx312465(L3,L1,L2,L4,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L2,L4,L6,L5)
       real*8 C
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
       subroutine slx312546(L3,L1,L2,L5,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L2,L5,L4,L6)
       real*8 C
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
       subroutine slx312564(L3,L1,L2,L5,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L2,L5,L6,L4)
       real*8 C
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
       subroutine slx312645(L3,L1,L2,L6,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L2,L6,L4,L5)
       real*8 C
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
       subroutine slx312654(L3,L1,L2,L6,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L2,L6,L5,L4)
       real*8 C
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
       subroutine slx314256(L3,L1,L4,L2,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L4,L2,L5,L6)
       real*8 C
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
       subroutine slx314265(L3,L1,L4,L2,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L4,L2,L6,L5)
       real*8 C
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
       subroutine slx314526(L3,L1,L4,L5,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L4,L5,L2,L6)
       real*8 C
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
       subroutine slx314562(L3,L1,L4,L5,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L4,L5,L6,L2)
       real*8 C
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
       subroutine slx314625(L3,L1,L4,L6,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L4,L6,L2,L5)
       real*8 C
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
       subroutine slx314652(L3,L1,L4,L6,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L4,L6,L5,L2)
       real*8 C
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
       subroutine slx315246(L3,L1,L5,L2,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L5,L2,L4,L6)
       real*8 C
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
       subroutine slx315264(L3,L1,L5,L2,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L5,L2,L6,L4)
       real*8 C
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
       subroutine slx315426(L3,L1,L5,L4,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L5,L4,L2,L6)
       real*8 C
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
       subroutine slx315462(L3,L1,L5,L4,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L5,L4,L6,L2)
       real*8 C
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
       subroutine slx315624(L3,L1,L5,L6,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L5,L6,L2,L4)
       real*8 C
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
       subroutine slx315642(L3,L1,L5,L6,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L5,L6,L4,L2)
       real*8 C
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
       subroutine slx316245(L3,L1,L6,L2,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L6,L2,L4,L5)
       real*8 C
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
       subroutine slx316254(L3,L1,L6,L2,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L6,L2,L5,L4)
       real*8 C
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
       subroutine slx316425(L3,L1,L6,L4,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L6,L4,L2,L5)
       real*8 C
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
       subroutine slx316452(L3,L1,L6,L4,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L6,L4,L5,L2)
       real*8 C
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
       subroutine slx316524(L3,L1,L6,L5,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L6,L5,L2,L4)
       real*8 C
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
       subroutine slx316542(L3,L1,L6,L5,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L1,L6,L5,L4,L2)
       real*8 C
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
       subroutine slx321456(L3,L2,L1,L4,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L1,L4,L5,L6)
       real*8 C
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
       subroutine slx321465(L3,L2,L1,L4,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L1,L4,L6,L5)
       real*8 C
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
       subroutine slx321546(L3,L2,L1,L5,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L1,L5,L4,L6)
       real*8 C
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
       subroutine slx321564(L3,L2,L1,L5,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L1,L5,L6,L4)
       real*8 C
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
       subroutine slx321645(L3,L2,L1,L6,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L1,L6,L4,L5)
       real*8 C
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
       subroutine slx321654(L3,L2,L1,L6,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L1,L6,L5,L4)
       real*8 C
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
       subroutine slx324156(L3,L2,L4,L1,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L4,L1,L5,L6)
       real*8 C
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
       subroutine slx324165(L3,L2,L4,L1,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L4,L1,L6,L5)
       real*8 C
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
       subroutine slx324516(L3,L2,L4,L5,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L4,L5,L1,L6)
       real*8 C
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
       subroutine slx324561(L3,L2,L4,L5,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L4,L5,L6,L1)
       real*8 C
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
       subroutine slx324615(L3,L2,L4,L6,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L4,L6,L1,L5)
       real*8 C
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
       subroutine slx324651(L3,L2,L4,L6,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L4,L6,L5,L1)
       real*8 C
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
       subroutine slx325146(L3,L2,L5,L1,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L5,L1,L4,L6)
       real*8 C
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
       subroutine slx325164(L3,L2,L5,L1,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L5,L1,L6,L4)
       real*8 C
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
       subroutine slx325416(L3,L2,L5,L4,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L5,L4,L1,L6)
       real*8 C
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
       subroutine slx325461(L3,L2,L5,L4,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L5,L4,L6,L1)
       real*8 C
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
       subroutine slx325614(L3,L2,L5,L6,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L5,L6,L1,L4)
       real*8 C
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
       subroutine slx325641(L3,L2,L5,L6,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L5,L6,L4,L1)
       real*8 C
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
       subroutine slx326145(L3,L2,L6,L1,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L6,L1,L4,L5)
       real*8 C
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
       subroutine slx326154(L3,L2,L6,L1,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L6,L1,L5,L4)
       real*8 C
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
       subroutine slx326415(L3,L2,L6,L4,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L6,L4,L1,L5)
       real*8 C
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
       subroutine slx326451(L3,L2,L6,L4,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L6,L4,L5,L1)
       real*8 C
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
       subroutine slx326514(L3,L2,L6,L5,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L6,L5,L1,L4)
       real*8 C
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
       subroutine slx326541(L3,L2,L6,L5,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L2,L6,L5,L4,L1)
       real*8 C
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
       subroutine slx341256(L3,L4,L1,L2,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L1,L2,L5,L6)
       real*8 C
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
       subroutine slx341265(L3,L4,L1,L2,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L1,L2,L6,L5)
       real*8 C
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
       subroutine slx341526(L3,L4,L1,L5,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L1,L5,L2,L6)
       real*8 C
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
       subroutine slx341562(L3,L4,L1,L5,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L1,L5,L6,L2)
       real*8 C
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
       subroutine slx341625(L3,L4,L1,L6,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L1,L6,L2,L5)
       real*8 C
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
       subroutine slx341652(L3,L4,L1,L6,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L1,L6,L5,L2)
       real*8 C
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
       subroutine slx342156(L3,L4,L2,L1,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L2,L1,L5,L6)
       real*8 C
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
       subroutine slx342165(L3,L4,L2,L1,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L2,L1,L6,L5)
       real*8 C
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
       subroutine slx342516(L3,L4,L2,L5,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L2,L5,L1,L6)
       real*8 C
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
       subroutine slx342561(L3,L4,L2,L5,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L2,L5,L6,L1)
       real*8 C
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
       subroutine slx342615(L3,L4,L2,L6,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L2,L6,L1,L5)
       real*8 C
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
       subroutine slx342651(L3,L4,L2,L6,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L2,L6,L5,L1)
       real*8 C
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
       subroutine slx345126(L3,L4,L5,L1,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L5,L1,L2,L6)
       real*8 C
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
       subroutine slx345162(L3,L4,L5,L1,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L5,L1,L6,L2)
       real*8 C
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
       subroutine slx345216(L3,L4,L5,L2,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L5,L2,L1,L6)
       real*8 C
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
       subroutine slx345261(L3,L4,L5,L2,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L5,L2,L6,L1)
       real*8 C
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
       subroutine slx345612(L3,L4,L5,L6,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L5,L6,L1,L2)
       real*8 C
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
       subroutine slx345621(L3,L4,L5,L6,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L5,L6,L2,L1)
       real*8 C
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
       subroutine slx346125(L3,L4,L6,L1,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L6,L1,L2,L5)
       real*8 C
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
       subroutine slx346152(L3,L4,L6,L1,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L6,L1,L5,L2)
       real*8 C
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
       subroutine slx346215(L3,L4,L6,L2,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L6,L2,L1,L5)
       real*8 C
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
       subroutine slx346251(L3,L4,L6,L2,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L6,L2,L5,L1)
       real*8 C
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
       subroutine slx346512(L3,L4,L6,L5,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L6,L5,L1,L2)
       real*8 C
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
       subroutine slx346521(L3,L4,L6,L5,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L4,L6,L5,L2,L1)
       real*8 C
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
       subroutine slx351246(L3,L5,L1,L2,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L1,L2,L4,L6)
       real*8 C
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
       subroutine slx351264(L3,L5,L1,L2,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L1,L2,L6,L4)
       real*8 C
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
       subroutine slx351426(L3,L5,L1,L4,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L1,L4,L2,L6)
       real*8 C
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
       subroutine slx351462(L3,L5,L1,L4,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L1,L4,L6,L2)
       real*8 C
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
       subroutine slx351624(L3,L5,L1,L6,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L1,L6,L2,L4)
       real*8 C
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
       subroutine slx351642(L3,L5,L1,L6,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L1,L6,L4,L2)
       real*8 C
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
       subroutine slx352146(L3,L5,L2,L1,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L2,L1,L4,L6)
       real*8 C
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
       subroutine slx352164(L3,L5,L2,L1,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L2,L1,L6,L4)
       real*8 C
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
       subroutine slx352416(L3,L5,L2,L4,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L2,L4,L1,L6)
       real*8 C
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
       subroutine slx352461(L3,L5,L2,L4,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L2,L4,L6,L1)
       real*8 C
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
       subroutine slx352614(L3,L5,L2,L6,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L2,L6,L1,L4)
       real*8 C
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
       subroutine slx352641(L3,L5,L2,L6,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L2,L6,L4,L1)
       real*8 C
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
       subroutine slx354126(L3,L5,L4,L1,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L4,L1,L2,L6)
       real*8 C
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
       subroutine slx354162(L3,L5,L4,L1,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L4,L1,L6,L2)
       real*8 C
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
       subroutine slx354216(L3,L5,L4,L2,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L4,L2,L1,L6)
       real*8 C
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
       subroutine slx354261(L3,L5,L4,L2,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L4,L2,L6,L1)
       real*8 C
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
       subroutine slx354612(L3,L5,L4,L6,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L4,L6,L1,L2)
       real*8 C
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
       subroutine slx354621(L3,L5,L4,L6,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L4,L6,L2,L1)
       real*8 C
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
       subroutine slx356124(L3,L5,L6,L1,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L6,L1,L2,L4)
       real*8 C
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
       subroutine slx356142(L3,L5,L6,L1,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L6,L1,L4,L2)
       real*8 C
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
       subroutine slx356214(L3,L5,L6,L2,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L6,L2,L1,L4)
       real*8 C
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
       subroutine slx356241(L3,L5,L6,L2,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L6,L2,L4,L1)
       real*8 C
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
       subroutine slx356412(L3,L5,L6,L4,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L6,L4,L1,L2)
       real*8 C
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
       subroutine slx356421(L3,L5,L6,L4,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L5,L6,L4,L2,L1)
       real*8 C
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
       subroutine slx361245(L3,L6,L1,L2,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L1,L2,L4,L5)
       real*8 C
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
       subroutine slx361254(L3,L6,L1,L2,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L1,L2,L5,L4)
       real*8 C
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
       subroutine slx361425(L3,L6,L1,L4,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L1,L4,L2,L5)
       real*8 C
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
       subroutine slx361452(L3,L6,L1,L4,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L1,L4,L5,L2)
       real*8 C
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
       subroutine slx361524(L3,L6,L1,L5,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L1,L5,L2,L4)
       real*8 C
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
       subroutine slx361542(L3,L6,L1,L5,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L1,L5,L4,L2)
       real*8 C
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
       subroutine slx362145(L3,L6,L2,L1,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L2,L1,L4,L5)
       real*8 C
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
       subroutine slx362154(L3,L6,L2,L1,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L2,L1,L5,L4)
       real*8 C
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
       subroutine slx362415(L3,L6,L2,L4,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L2,L4,L1,L5)
       real*8 C
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
       subroutine slx362451(L3,L6,L2,L4,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L2,L4,L5,L1)
       real*8 C
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
       subroutine slx362514(L3,L6,L2,L5,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L2,L5,L1,L4)
       real*8 C
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
       subroutine slx362541(L3,L6,L2,L5,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L2,L5,L4,L1)
       real*8 C
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
       subroutine slx364125(L3,L6,L4,L1,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L4,L1,L2,L5)
       real*8 C
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
       subroutine slx364152(L3,L6,L4,L1,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L4,L1,L5,L2)
       real*8 C
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
       subroutine slx364215(L3,L6,L4,L2,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L4,L2,L1,L5)
       real*8 C
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
       subroutine slx364251(L3,L6,L4,L2,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L4,L2,L5,L1)
       real*8 C
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
       subroutine slx364512(L3,L6,L4,L5,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L4,L5,L1,L2)
       real*8 C
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
       subroutine slx364521(L3,L6,L4,L5,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L4,L5,L2,L1)
       real*8 C
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
       subroutine slx365124(L3,L6,L5,L1,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L5,L1,L2,L4)
       real*8 C
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
       subroutine slx365142(L3,L6,L5,L1,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L5,L1,L4,L2)
       real*8 C
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
       subroutine slx365214(L3,L6,L5,L2,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L5,L2,L1,L4)
       real*8 C
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
       subroutine slx365241(L3,L6,L5,L2,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L5,L2,L4,L1)
       real*8 C
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
       subroutine slx365412(L3,L6,L5,L4,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L5,L4,L1,L2)
       real*8 C
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
       subroutine slx365421(L3,L6,L5,L4,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L3,L6,L5,L4,L2,L1)
       real*8 C
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
       subroutine slx412356(L4,L1,L2,L3,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L2,L3,L5,L6)
       real*8 C
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
       subroutine slx412365(L4,L1,L2,L3,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L2,L3,L6,L5)
       real*8 C
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
       subroutine slx412536(L4,L1,L2,L5,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L2,L5,L3,L6)
       real*8 C
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
       subroutine slx412563(L4,L1,L2,L5,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L2,L5,L6,L3)
       real*8 C
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
       subroutine slx412635(L4,L1,L2,L6,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L2,L6,L3,L5)
       real*8 C
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
       subroutine slx412653(L4,L1,L2,L6,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L2,L6,L5,L3)
       real*8 C
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
       subroutine slx413256(L4,L1,L3,L2,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L3,L2,L5,L6)
       real*8 C
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
       subroutine slx413265(L4,L1,L3,L2,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L3,L2,L6,L5)
       real*8 C
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
       subroutine slx413526(L4,L1,L3,L5,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L3,L5,L2,L6)
       real*8 C
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
       subroutine slx413562(L4,L1,L3,L5,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L3,L5,L6,L2)
       real*8 C
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
       subroutine slx413625(L4,L1,L3,L6,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L3,L6,L2,L5)
       real*8 C
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
       subroutine slx413652(L4,L1,L3,L6,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L3,L6,L5,L2)
       real*8 C
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
       subroutine slx415236(L4,L1,L5,L2,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L5,L2,L3,L6)
       real*8 C
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
       subroutine slx415263(L4,L1,L5,L2,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L5,L2,L6,L3)
       real*8 C
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
       subroutine slx415326(L4,L1,L5,L3,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L5,L3,L2,L6)
       real*8 C
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
       subroutine slx415362(L4,L1,L5,L3,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L5,L3,L6,L2)
       real*8 C
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
       subroutine slx415623(L4,L1,L5,L6,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L5,L6,L2,L3)
       real*8 C
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
       subroutine slx415632(L4,L1,L5,L6,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L5,L6,L3,L2)
       real*8 C
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
       subroutine slx416235(L4,L1,L6,L2,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L6,L2,L3,L5)
       real*8 C
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
       subroutine slx416253(L4,L1,L6,L2,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L6,L2,L5,L3)
       real*8 C
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
       subroutine slx416325(L4,L1,L6,L3,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L6,L3,L2,L5)
       real*8 C
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
       subroutine slx416352(L4,L1,L6,L3,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L6,L3,L5,L2)
       real*8 C
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
       subroutine slx416523(L4,L1,L6,L5,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L6,L5,L2,L3)
       real*8 C
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
       subroutine slx416532(L4,L1,L6,L5,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L1,L6,L5,L3,L2)
       real*8 C
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
       subroutine slx421356(L4,L2,L1,L3,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L1,L3,L5,L6)
       real*8 C
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
       subroutine slx421365(L4,L2,L1,L3,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L1,L3,L6,L5)
       real*8 C
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
       subroutine slx421536(L4,L2,L1,L5,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L1,L5,L3,L6)
       real*8 C
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
       subroutine slx421563(L4,L2,L1,L5,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L1,L5,L6,L3)
       real*8 C
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
       subroutine slx421635(L4,L2,L1,L6,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L1,L6,L3,L5)
       real*8 C
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
       subroutine slx421653(L4,L2,L1,L6,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L1,L6,L5,L3)
       real*8 C
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
       subroutine slx423156(L4,L2,L3,L1,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L3,L1,L5,L6)
       real*8 C
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
       subroutine slx423165(L4,L2,L3,L1,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L3,L1,L6,L5)
       real*8 C
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
       subroutine slx423516(L4,L2,L3,L5,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L3,L5,L1,L6)
       real*8 C
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
       subroutine slx423561(L4,L2,L3,L5,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L3,L5,L6,L1)
       real*8 C
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
       subroutine slx423615(L4,L2,L3,L6,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L3,L6,L1,L5)
       real*8 C
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
       subroutine slx423651(L4,L2,L3,L6,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L3,L6,L5,L1)
       real*8 C
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
       subroutine slx425136(L4,L2,L5,L1,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L5,L1,L3,L6)
       real*8 C
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
       subroutine slx425163(L4,L2,L5,L1,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L5,L1,L6,L3)
       real*8 C
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
       subroutine slx425316(L4,L2,L5,L3,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L5,L3,L1,L6)
       real*8 C
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
       subroutine slx425361(L4,L2,L5,L3,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L5,L3,L6,L1)
       real*8 C
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
       subroutine slx425613(L4,L2,L5,L6,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L5,L6,L1,L3)
       real*8 C
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
       subroutine slx425631(L4,L2,L5,L6,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L5,L6,L3,L1)
       real*8 C
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
       subroutine slx426135(L4,L2,L6,L1,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L6,L1,L3,L5)
       real*8 C
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
       subroutine slx426153(L4,L2,L6,L1,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L6,L1,L5,L3)
       real*8 C
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
       subroutine slx426315(L4,L2,L6,L3,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L6,L3,L1,L5)
       real*8 C
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
       subroutine slx426351(L4,L2,L6,L3,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L6,L3,L5,L1)
       real*8 C
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
       subroutine slx426513(L4,L2,L6,L5,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L6,L5,L1,L3)
       real*8 C
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
       subroutine slx426531(L4,L2,L6,L5,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L2,L6,L5,L3,L1)
       real*8 C
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
       subroutine slx431256(L4,L3,L1,L2,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L1,L2,L5,L6)
       real*8 C
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
       subroutine slx431265(L4,L3,L1,L2,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L1,L2,L6,L5)
       real*8 C
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
       subroutine slx431526(L4,L3,L1,L5,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L1,L5,L2,L6)
       real*8 C
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
       subroutine slx431562(L4,L3,L1,L5,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L1,L5,L6,L2)
       real*8 C
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
       subroutine slx431625(L4,L3,L1,L6,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L1,L6,L2,L5)
       real*8 C
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
       subroutine slx431652(L4,L3,L1,L6,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L1,L6,L5,L2)
       real*8 C
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
       subroutine slx432156(L4,L3,L2,L1,L5,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L2,L1,L5,L6)
       real*8 C
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
       subroutine slx432165(L4,L3,L2,L1,L6,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L2,L1,L6,L5)
       real*8 C
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
       subroutine slx432516(L4,L3,L2,L5,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L2,L5,L1,L6)
       real*8 C
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
       subroutine slx432561(L4,L3,L2,L5,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L2,L5,L6,L1)
       real*8 C
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
       subroutine slx432615(L4,L3,L2,L6,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L2,L6,L1,L5)
       real*8 C
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
       subroutine slx432651(L4,L3,L2,L6,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L2,L6,L5,L1)
       real*8 C
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
       subroutine slx435126(L4,L3,L5,L1,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L5,L1,L2,L6)
       real*8 C
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
       subroutine slx435162(L4,L3,L5,L1,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L5,L1,L6,L2)
       real*8 C
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
       subroutine slx435216(L4,L3,L5,L2,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L5,L2,L1,L6)
       real*8 C
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
       subroutine slx435261(L4,L3,L5,L2,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L5,L2,L6,L1)
       real*8 C
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
       subroutine slx435612(L4,L3,L5,L6,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L5,L6,L1,L2)
       real*8 C
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
       subroutine slx435621(L4,L3,L5,L6,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L5,L6,L2,L1)
       real*8 C
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
       subroutine slx436125(L4,L3,L6,L1,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L6,L1,L2,L5)
       real*8 C
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
       subroutine slx436152(L4,L3,L6,L1,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L6,L1,L5,L2)
       real*8 C
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
       subroutine slx436215(L4,L3,L6,L2,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L6,L2,L1,L5)
       real*8 C
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
       subroutine slx436251(L4,L3,L6,L2,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L6,L2,L5,L1)
       real*8 C
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
       subroutine slx436512(L4,L3,L6,L5,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L6,L5,L1,L2)
       real*8 C
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
       subroutine slx436521(L4,L3,L6,L5,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L3,L6,L5,L2,L1)
       real*8 C
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
       subroutine slx451236(L4,L5,L1,L2,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L1,L2,L3,L6)
       real*8 C
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
       subroutine slx451263(L4,L5,L1,L2,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L1,L2,L6,L3)
       real*8 C
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
       subroutine slx451326(L4,L5,L1,L3,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L1,L3,L2,L6)
       real*8 C
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
       subroutine slx451362(L4,L5,L1,L3,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L1,L3,L6,L2)
       real*8 C
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
       subroutine slx451623(L4,L5,L1,L6,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L1,L6,L2,L3)
       real*8 C
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
       subroutine slx451632(L4,L5,L1,L6,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L1,L6,L3,L2)
       real*8 C
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
       subroutine slx452136(L4,L5,L2,L1,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L2,L1,L3,L6)
       real*8 C
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
       subroutine slx452163(L4,L5,L2,L1,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L2,L1,L6,L3)
       real*8 C
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
       subroutine slx452316(L4,L5,L2,L3,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L2,L3,L1,L6)
       real*8 C
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
       subroutine slx452361(L4,L5,L2,L3,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L2,L3,L6,L1)
       real*8 C
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
       subroutine slx452613(L4,L5,L2,L6,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L2,L6,L1,L3)
       real*8 C
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
       subroutine slx452631(L4,L5,L2,L6,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L2,L6,L3,L1)
       real*8 C
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
       subroutine slx453126(L4,L5,L3,L1,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L3,L1,L2,L6)
       real*8 C
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
       subroutine slx453162(L4,L5,L3,L1,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L3,L1,L6,L2)
       real*8 C
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
       subroutine slx453216(L4,L5,L3,L2,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L3,L2,L1,L6)
       real*8 C
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
       subroutine slx453261(L4,L5,L3,L2,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L3,L2,L6,L1)
       real*8 C
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
       subroutine slx453612(L4,L5,L3,L6,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L3,L6,L1,L2)
       real*8 C
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
       subroutine slx453621(L4,L5,L3,L6,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L3,L6,L2,L1)
       real*8 C
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
       subroutine slx456123(L4,L5,L6,L1,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L6,L1,L2,L3)
       real*8 C
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
       subroutine slx456132(L4,L5,L6,L1,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L6,L1,L3,L2)
       real*8 C
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
       subroutine slx456213(L4,L5,L6,L2,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L6,L2,L1,L3)
       real*8 C
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
       subroutine slx456231(L4,L5,L6,L2,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L6,L2,L3,L1)
       real*8 C
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
       subroutine slx456312(L4,L5,L6,L3,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L6,L3,L1,L2)
       real*8 C
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
       subroutine slx456321(L4,L5,L6,L3,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L5,L6,L3,L2,L1)
       real*8 C
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
       subroutine slx461235(L4,L6,L1,L2,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L1,L2,L3,L5)
       real*8 C
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
       subroutine slx461253(L4,L6,L1,L2,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L1,L2,L5,L3)
       real*8 C
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
       subroutine slx461325(L4,L6,L1,L3,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L1,L3,L2,L5)
       real*8 C
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
       subroutine slx461352(L4,L6,L1,L3,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L1,L3,L5,L2)
       real*8 C
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
       subroutine slx461523(L4,L6,L1,L5,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L1,L5,L2,L3)
       real*8 C
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
       subroutine slx461532(L4,L6,L1,L5,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L1,L5,L3,L2)
       real*8 C
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
       subroutine slx462135(L4,L6,L2,L1,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L2,L1,L3,L5)
       real*8 C
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
       subroutine slx462153(L4,L6,L2,L1,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L2,L1,L5,L3)
       real*8 C
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
       subroutine slx462315(L4,L6,L2,L3,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L2,L3,L1,L5)
       real*8 C
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
       subroutine slx462351(L4,L6,L2,L3,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L2,L3,L5,L1)
       real*8 C
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
       subroutine slx462513(L4,L6,L2,L5,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L2,L5,L1,L3)
       real*8 C
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
       subroutine slx462531(L4,L6,L2,L5,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L2,L5,L3,L1)
       real*8 C
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
       subroutine slx463125(L4,L6,L3,L1,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L3,L1,L2,L5)
       real*8 C
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
       subroutine slx463152(L4,L6,L3,L1,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L3,L1,L5,L2)
       real*8 C
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
       subroutine slx463215(L4,L6,L3,L2,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L3,L2,L1,L5)
       real*8 C
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
       subroutine slx463251(L4,L6,L3,L2,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L3,L2,L5,L1)
       real*8 C
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
       subroutine slx463512(L4,L6,L3,L5,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L3,L5,L1,L2)
       real*8 C
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
       subroutine slx463521(L4,L6,L3,L5,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L3,L5,L2,L1)
       real*8 C
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
       subroutine slx465123(L4,L6,L5,L1,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L5,L1,L2,L3)
       real*8 C
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
       subroutine slx465132(L4,L6,L5,L1,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L5,L1,L3,L2)
       real*8 C
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
       subroutine slx465213(L4,L6,L5,L2,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L5,L2,L1,L3)
       real*8 C
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
       subroutine slx465231(L4,L6,L5,L2,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L5,L2,L3,L1)
       real*8 C
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
       subroutine slx465312(L4,L6,L5,L3,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L5,L3,L1,L2)
       real*8 C
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
       subroutine slx465321(L4,L6,L5,L3,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L4,L6,L5,L3,L2,L1)
       real*8 C
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
       subroutine slx512346(L5,L1,L2,L3,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L2,L3,L4,L6)
       real*8 C
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
       subroutine slx512364(L5,L1,L2,L3,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L2,L3,L6,L4)
       real*8 C
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
       subroutine slx512436(L5,L1,L2,L4,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L2,L4,L3,L6)
       real*8 C
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
       subroutine slx512463(L5,L1,L2,L4,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L2,L4,L6,L3)
       real*8 C
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
       subroutine slx512634(L5,L1,L2,L6,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L2,L6,L3,L4)
       real*8 C
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
       subroutine slx512643(L5,L1,L2,L6,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L2,L6,L4,L3)
       real*8 C
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
       subroutine slx513246(L5,L1,L3,L2,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L3,L2,L4,L6)
       real*8 C
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
       subroutine slx513264(L5,L1,L3,L2,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L3,L2,L6,L4)
       real*8 C
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
       subroutine slx513426(L5,L1,L3,L4,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L3,L4,L2,L6)
       real*8 C
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
       subroutine slx513462(L5,L1,L3,L4,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L3,L4,L6,L2)
       real*8 C
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
       subroutine slx513624(L5,L1,L3,L6,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L3,L6,L2,L4)
       real*8 C
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
       subroutine slx513642(L5,L1,L3,L6,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L3,L6,L4,L2)
       real*8 C
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
       subroutine slx514236(L5,L1,L4,L2,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L4,L2,L3,L6)
       real*8 C
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
       subroutine slx514263(L5,L1,L4,L2,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L4,L2,L6,L3)
       real*8 C
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
       subroutine slx514326(L5,L1,L4,L3,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L4,L3,L2,L6)
       real*8 C
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
       subroutine slx514362(L5,L1,L4,L3,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L4,L3,L6,L2)
       real*8 C
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
       subroutine slx514623(L5,L1,L4,L6,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L4,L6,L2,L3)
       real*8 C
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
       subroutine slx514632(L5,L1,L4,L6,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L4,L6,L3,L2)
       real*8 C
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
       subroutine slx516234(L5,L1,L6,L2,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L6,L2,L3,L4)
       real*8 C
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
       subroutine slx516243(L5,L1,L6,L2,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L6,L2,L4,L3)
       real*8 C
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
       subroutine slx516324(L5,L1,L6,L3,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L6,L3,L2,L4)
       real*8 C
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
       subroutine slx516342(L5,L1,L6,L3,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L6,L3,L4,L2)
       real*8 C
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
       subroutine slx516423(L5,L1,L6,L4,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L6,L4,L2,L3)
       real*8 C
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
       subroutine slx516432(L5,L1,L6,L4,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L1,L6,L4,L3,L2)
       real*8 C
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
       subroutine slx521346(L5,L2,L1,L3,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L1,L3,L4,L6)
       real*8 C
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
       subroutine slx521364(L5,L2,L1,L3,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L1,L3,L6,L4)
       real*8 C
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
       subroutine slx521436(L5,L2,L1,L4,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L1,L4,L3,L6)
       real*8 C
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
       subroutine slx521463(L5,L2,L1,L4,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L1,L4,L6,L3)
       real*8 C
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
       subroutine slx521634(L5,L2,L1,L6,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L1,L6,L3,L4)
       real*8 C
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
       subroutine slx521643(L5,L2,L1,L6,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L1,L6,L4,L3)
       real*8 C
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
       subroutine slx523146(L5,L2,L3,L1,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L3,L1,L4,L6)
       real*8 C
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
       subroutine slx523164(L5,L2,L3,L1,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L3,L1,L6,L4)
       real*8 C
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
       subroutine slx523416(L5,L2,L3,L4,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L3,L4,L1,L6)
       real*8 C
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
       subroutine slx523461(L5,L2,L3,L4,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L3,L4,L6,L1)
       real*8 C
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
       subroutine slx523614(L5,L2,L3,L6,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L3,L6,L1,L4)
       real*8 C
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
       subroutine slx523641(L5,L2,L3,L6,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L3,L6,L4,L1)
       real*8 C
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
       subroutine slx524136(L5,L2,L4,L1,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L4,L1,L3,L6)
       real*8 C
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
       subroutine slx524163(L5,L2,L4,L1,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L4,L1,L6,L3)
       real*8 C
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
       subroutine slx524316(L5,L2,L4,L3,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L4,L3,L1,L6)
       real*8 C
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
       subroutine slx524361(L5,L2,L4,L3,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L4,L3,L6,L1)
       real*8 C
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
       subroutine slx524613(L5,L2,L4,L6,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L4,L6,L1,L3)
       real*8 C
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
       subroutine slx524631(L5,L2,L4,L6,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L4,L6,L3,L1)
       real*8 C
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
       subroutine slx526134(L5,L2,L6,L1,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L6,L1,L3,L4)
       real*8 C
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
       subroutine slx526143(L5,L2,L6,L1,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L6,L1,L4,L3)
       real*8 C
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
       subroutine slx526314(L5,L2,L6,L3,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L6,L3,L1,L4)
       real*8 C
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
       subroutine slx526341(L5,L2,L6,L3,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L6,L3,L4,L1)
       real*8 C
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
       subroutine slx526413(L5,L2,L6,L4,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L6,L4,L1,L3)
       real*8 C
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
       subroutine slx526431(L5,L2,L6,L4,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L2,L6,L4,L3,L1)
       real*8 C
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
       subroutine slx531246(L5,L3,L1,L2,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L1,L2,L4,L6)
       real*8 C
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
       subroutine slx531264(L5,L3,L1,L2,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L1,L2,L6,L4)
       real*8 C
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
       subroutine slx531426(L5,L3,L1,L4,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L1,L4,L2,L6)
       real*8 C
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
       subroutine slx531462(L5,L3,L1,L4,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L1,L4,L6,L2)
       real*8 C
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
       subroutine slx531624(L5,L3,L1,L6,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L1,L6,L2,L4)
       real*8 C
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
       subroutine slx531642(L5,L3,L1,L6,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L1,L6,L4,L2)
       real*8 C
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
       subroutine slx532146(L5,L3,L2,L1,L4,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L2,L1,L4,L6)
       real*8 C
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
       subroutine slx532164(L5,L3,L2,L1,L6,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L2,L1,L6,L4)
       real*8 C
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
       subroutine slx532416(L5,L3,L2,L4,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L2,L4,L1,L6)
       real*8 C
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
       subroutine slx532461(L5,L3,L2,L4,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L2,L4,L6,L1)
       real*8 C
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
       subroutine slx532614(L5,L3,L2,L6,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L2,L6,L1,L4)
       real*8 C
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
       subroutine slx532641(L5,L3,L2,L6,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L2,L6,L4,L1)
       real*8 C
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
       subroutine slx534126(L5,L3,L4,L1,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L4,L1,L2,L6)
       real*8 C
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
       subroutine slx534162(L5,L3,L4,L1,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L4,L1,L6,L2)
       real*8 C
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
       subroutine slx534216(L5,L3,L4,L2,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L4,L2,L1,L6)
       real*8 C
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
       subroutine slx534261(L5,L3,L4,L2,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L4,L2,L6,L1)
       real*8 C
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
       subroutine slx534612(L5,L3,L4,L6,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L4,L6,L1,L2)
       real*8 C
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
       subroutine slx534621(L5,L3,L4,L6,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L4,L6,L2,L1)
       real*8 C
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
       subroutine slx536124(L5,L3,L6,L1,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L6,L1,L2,L4)
       real*8 C
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
       subroutine slx536142(L5,L3,L6,L1,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L6,L1,L4,L2)
       real*8 C
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
       subroutine slx536214(L5,L3,L6,L2,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L6,L2,L1,L4)
       real*8 C
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
       subroutine slx536241(L5,L3,L6,L2,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L6,L2,L4,L1)
       real*8 C
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
       subroutine slx536412(L5,L3,L6,L4,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L6,L4,L1,L2)
       real*8 C
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
       subroutine slx536421(L5,L3,L6,L4,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L3,L6,L4,L2,L1)
       real*8 C
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
       subroutine slx541236(L5,L4,L1,L2,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L1,L2,L3,L6)
       real*8 C
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
       subroutine slx541263(L5,L4,L1,L2,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L1,L2,L6,L3)
       real*8 C
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
       subroutine slx541326(L5,L4,L1,L3,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L1,L3,L2,L6)
       real*8 C
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
       subroutine slx541362(L5,L4,L1,L3,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L1,L3,L6,L2)
       real*8 C
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
       subroutine slx541623(L5,L4,L1,L6,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L1,L6,L2,L3)
       real*8 C
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
       subroutine slx541632(L5,L4,L1,L6,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L1,L6,L3,L2)
       real*8 C
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
       subroutine slx542136(L5,L4,L2,L1,L3,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L2,L1,L3,L6)
       real*8 C
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
       subroutine slx542163(L5,L4,L2,L1,L6,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L2,L1,L6,L3)
       real*8 C
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
       subroutine slx542316(L5,L4,L2,L3,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L2,L3,L1,L6)
       real*8 C
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
       subroutine slx542361(L5,L4,L2,L3,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L2,L3,L6,L1)
       real*8 C
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
       subroutine slx542613(L5,L4,L2,L6,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L2,L6,L1,L3)
       real*8 C
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
       subroutine slx542631(L5,L4,L2,L6,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L2,L6,L3,L1)
       real*8 C
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
       subroutine slx543126(L5,L4,L3,L1,L2,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L3,L1,L2,L6)
       real*8 C
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
       subroutine slx543162(L5,L4,L3,L1,L6,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L3,L1,L6,L2)
       real*8 C
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
       subroutine slx543216(L5,L4,L3,L2,L1,L6,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L3,L2,L1,L6)
       real*8 C
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
       subroutine slx543261(L5,L4,L3,L2,L6,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L3,L2,L6,L1)
       real*8 C
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
       subroutine slx543612(L5,L4,L3,L6,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L3,L6,L1,L2)
       real*8 C
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
       subroutine slx543621(L5,L4,L3,L6,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L3,L6,L2,L1)
       real*8 C
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
       subroutine slx546123(L5,L4,L6,L1,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L6,L1,L2,L3)
       real*8 C
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
       subroutine slx546132(L5,L4,L6,L1,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L6,L1,L3,L2)
       real*8 C
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
       subroutine slx546213(L5,L4,L6,L2,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L6,L2,L1,L3)
       real*8 C
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
       subroutine slx546231(L5,L4,L6,L2,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L6,L2,L3,L1)
       real*8 C
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
       subroutine slx546312(L5,L4,L6,L3,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L6,L3,L1,L2)
       real*8 C
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
       subroutine slx546321(L5,L4,L6,L3,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L4,L6,L3,L2,L1)
       real*8 C
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
       subroutine slx561234(L5,L6,L1,L2,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L1,L2,L3,L4)
       real*8 C
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
       subroutine slx561243(L5,L6,L1,L2,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L1,L2,L4,L3)
       real*8 C
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
       subroutine slx561324(L5,L6,L1,L3,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L1,L3,L2,L4)
       real*8 C
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
       subroutine slx561342(L5,L6,L1,L3,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L1,L3,L4,L2)
       real*8 C
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
       subroutine slx561423(L5,L6,L1,L4,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L1,L4,L2,L3)
       real*8 C
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
       subroutine slx561432(L5,L6,L1,L4,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L1,L4,L3,L2)
       real*8 C
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
       subroutine slx562134(L5,L6,L2,L1,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L2,L1,L3,L4)
       real*8 C
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
       subroutine slx562143(L5,L6,L2,L1,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L2,L1,L4,L3)
       real*8 C
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
       subroutine slx562314(L5,L6,L2,L3,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L2,L3,L1,L4)
       real*8 C
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
       subroutine slx562341(L5,L6,L2,L3,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L2,L3,L4,L1)
       real*8 C
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
       subroutine slx562413(L5,L6,L2,L4,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L2,L4,L1,L3)
       real*8 C
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
       subroutine slx562431(L5,L6,L2,L4,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L2,L4,L3,L1)
       real*8 C
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
       subroutine slx563124(L5,L6,L3,L1,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L3,L1,L2,L4)
       real*8 C
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
       subroutine slx563142(L5,L6,L3,L1,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L3,L1,L4,L2)
       real*8 C
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
       subroutine slx563214(L5,L6,L3,L2,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L3,L2,L1,L4)
       real*8 C
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
       subroutine slx563241(L5,L6,L3,L2,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L3,L2,L4,L1)
       real*8 C
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
       subroutine slx563412(L5,L6,L3,L4,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L3,L4,L1,L2)
       real*8 C
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
       subroutine slx563421(L5,L6,L3,L4,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L3,L4,L2,L1)
       real*8 C
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
       subroutine slx564123(L5,L6,L4,L1,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L4,L1,L2,L3)
       real*8 C
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
       subroutine slx564132(L5,L6,L4,L1,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L4,L1,L3,L2)
       real*8 C
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
       subroutine slx564213(L5,L6,L4,L2,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L4,L2,L1,L3)
       real*8 C
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
       subroutine slx564231(L5,L6,L4,L2,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L4,L2,L3,L1)
       real*8 C
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
       subroutine slx564312(L5,L6,L4,L3,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L4,L3,L1,L2)
       real*8 C
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
       subroutine slx564321(L5,L6,L4,L3,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L5,L6,L4,L3,L2,L1)
       real*8 C
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
       subroutine slx612345(L6,L1,L2,L3,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L2,L3,L4,L5)
       real*8 C
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
       subroutine slx612354(L6,L1,L2,L3,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L2,L3,L5,L4)
       real*8 C
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
       subroutine slx612435(L6,L1,L2,L4,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L2,L4,L3,L5)
       real*8 C
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
       subroutine slx612453(L6,L1,L2,L4,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L2,L4,L5,L3)
       real*8 C
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
       subroutine slx612534(L6,L1,L2,L5,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L2,L5,L3,L4)
       real*8 C
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
       subroutine slx612543(L6,L1,L2,L5,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L2,L5,L4,L3)
       real*8 C
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
       subroutine slx613245(L6,L1,L3,L2,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L3,L2,L4,L5)
       real*8 C
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
       subroutine slx613254(L6,L1,L3,L2,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L3,L2,L5,L4)
       real*8 C
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
       subroutine slx613425(L6,L1,L3,L4,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L3,L4,L2,L5)
       real*8 C
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
       subroutine slx613452(L6,L1,L3,L4,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L3,L4,L5,L2)
       real*8 C
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
       subroutine slx613524(L6,L1,L3,L5,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L3,L5,L2,L4)
       real*8 C
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
       subroutine slx613542(L6,L1,L3,L5,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L3,L5,L4,L2)
       real*8 C
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
       subroutine slx614235(L6,L1,L4,L2,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L4,L2,L3,L5)
       real*8 C
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
       subroutine slx614253(L6,L1,L4,L2,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L4,L2,L5,L3)
       real*8 C
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
       subroutine slx614325(L6,L1,L4,L3,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L4,L3,L2,L5)
       real*8 C
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
       subroutine slx614352(L6,L1,L4,L3,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L4,L3,L5,L2)
       real*8 C
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
       subroutine slx614523(L6,L1,L4,L5,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L4,L5,L2,L3)
       real*8 C
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
       subroutine slx614532(L6,L1,L4,L5,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L4,L5,L3,L2)
       real*8 C
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
       subroutine slx615234(L6,L1,L5,L2,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L5,L2,L3,L4)
       real*8 C
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
       subroutine slx615243(L6,L1,L5,L2,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L5,L2,L4,L3)
       real*8 C
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
       subroutine slx615324(L6,L1,L5,L3,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L5,L3,L2,L4)
       real*8 C
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
       subroutine slx615342(L6,L1,L5,L3,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L5,L3,L4,L2)
       real*8 C
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
       subroutine slx615423(L6,L1,L5,L4,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L5,L4,L2,L3)
       real*8 C
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
       subroutine slx615432(L6,L1,L5,L4,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L1,L5,L4,L3,L2)
       real*8 C
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
       subroutine slx621345(L6,L2,L1,L3,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L1,L3,L4,L5)
       real*8 C
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
       subroutine slx621354(L6,L2,L1,L3,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L1,L3,L5,L4)
       real*8 C
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
       subroutine slx621435(L6,L2,L1,L4,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L1,L4,L3,L5)
       real*8 C
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
       subroutine slx621453(L6,L2,L1,L4,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L1,L4,L5,L3)
       real*8 C
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
       subroutine slx621534(L6,L2,L1,L5,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L1,L5,L3,L4)
       real*8 C
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
       subroutine slx621543(L6,L2,L1,L5,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L1,L5,L4,L3)
       real*8 C
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
       subroutine slx623145(L6,L2,L3,L1,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L3,L1,L4,L5)
       real*8 C
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
       subroutine slx623154(L6,L2,L3,L1,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L3,L1,L5,L4)
       real*8 C
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
       subroutine slx623415(L6,L2,L3,L4,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L3,L4,L1,L5)
       real*8 C
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
       subroutine slx623451(L6,L2,L3,L4,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L3,L4,L5,L1)
       real*8 C
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
       subroutine slx623514(L6,L2,L3,L5,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L3,L5,L1,L4)
       real*8 C
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
       subroutine slx623541(L6,L2,L3,L5,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L3,L5,L4,L1)
       real*8 C
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
       subroutine slx624135(L6,L2,L4,L1,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L4,L1,L3,L5)
       real*8 C
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
       subroutine slx624153(L6,L2,L4,L1,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L4,L1,L5,L3)
       real*8 C
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
       subroutine slx624315(L6,L2,L4,L3,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L4,L3,L1,L5)
       real*8 C
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
       subroutine slx624351(L6,L2,L4,L3,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L4,L3,L5,L1)
       real*8 C
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
       subroutine slx624513(L6,L2,L4,L5,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L4,L5,L1,L3)
       real*8 C
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
       subroutine slx624531(L6,L2,L4,L5,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L4,L5,L3,L1)
       real*8 C
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
       subroutine slx625134(L6,L2,L5,L1,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L5,L1,L3,L4)
       real*8 C
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
       subroutine slx625143(L6,L2,L5,L1,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L5,L1,L4,L3)
       real*8 C
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
       subroutine slx625314(L6,L2,L5,L3,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L5,L3,L1,L4)
       real*8 C
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
       subroutine slx625341(L6,L2,L5,L3,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L5,L3,L4,L1)
       real*8 C
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
       subroutine slx625413(L6,L2,L5,L4,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L5,L4,L1,L3)
       real*8 C
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
       subroutine slx625431(L6,L2,L5,L4,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L2,L5,L4,L3,L1)
       real*8 C
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
       subroutine slx631245(L6,L3,L1,L2,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L1,L2,L4,L5)
       real*8 C
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
       subroutine slx631254(L6,L3,L1,L2,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L1,L2,L5,L4)
       real*8 C
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
       subroutine slx631425(L6,L3,L1,L4,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L1,L4,L2,L5)
       real*8 C
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
       subroutine slx631452(L6,L3,L1,L4,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L1,L4,L5,L2)
       real*8 C
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
       subroutine slx631524(L6,L3,L1,L5,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L1,L5,L2,L4)
       real*8 C
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
       subroutine slx631542(L6,L3,L1,L5,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L1,L5,L4,L2)
       real*8 C
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
       subroutine slx632145(L6,L3,L2,L1,L4,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L2,L1,L4,L5)
       real*8 C
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
       subroutine slx632154(L6,L3,L2,L1,L5,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L2,L1,L5,L4)
       real*8 C
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
       subroutine slx632415(L6,L3,L2,L4,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L2,L4,L1,L5)
       real*8 C
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
       subroutine slx632451(L6,L3,L2,L4,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L2,L4,L5,L1)
       real*8 C
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
       subroutine slx632514(L6,L3,L2,L5,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L2,L5,L1,L4)
       real*8 C
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
       subroutine slx632541(L6,L3,L2,L5,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L2,L5,L4,L1)
       real*8 C
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
       subroutine slx634125(L6,L3,L4,L1,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L4,L1,L2,L5)
       real*8 C
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
       subroutine slx634152(L6,L3,L4,L1,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L4,L1,L5,L2)
       real*8 C
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
       subroutine slx634215(L6,L3,L4,L2,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L4,L2,L1,L5)
       real*8 C
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
       subroutine slx634251(L6,L3,L4,L2,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L4,L2,L5,L1)
       real*8 C
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
       subroutine slx634512(L6,L3,L4,L5,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L4,L5,L1,L2)
       real*8 C
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
       subroutine slx634521(L6,L3,L4,L5,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L4,L5,L2,L1)
       real*8 C
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
       subroutine slx635124(L6,L3,L5,L1,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L5,L1,L2,L4)
       real*8 C
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
       subroutine slx635142(L6,L3,L5,L1,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L5,L1,L4,L2)
       real*8 C
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
       subroutine slx635214(L6,L3,L5,L2,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L5,L2,L1,L4)
       real*8 C
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
       subroutine slx635241(L6,L3,L5,L2,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L5,L2,L4,L1)
       real*8 C
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
       subroutine slx635412(L6,L3,L5,L4,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L5,L4,L1,L2)
       real*8 C
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
       subroutine slx635421(L6,L3,L5,L4,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L3,L5,L4,L2,L1)
       real*8 C
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
       subroutine slx641235(L6,L4,L1,L2,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L1,L2,L3,L5)
       real*8 C
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
       subroutine slx641253(L6,L4,L1,L2,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L1,L2,L5,L3)
       real*8 C
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
       subroutine slx641325(L6,L4,L1,L3,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L1,L3,L2,L5)
       real*8 C
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
       subroutine slx641352(L6,L4,L1,L3,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L1,L3,L5,L2)
       real*8 C
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
       subroutine slx641523(L6,L4,L1,L5,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L1,L5,L2,L3)
       real*8 C
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
       subroutine slx641532(L6,L4,L1,L5,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L1,L5,L3,L2)
       real*8 C
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
       subroutine slx642135(L6,L4,L2,L1,L3,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L2,L1,L3,L5)
       real*8 C
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
       subroutine slx642153(L6,L4,L2,L1,L5,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L2,L1,L5,L3)
       real*8 C
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
       subroutine slx642315(L6,L4,L2,L3,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L2,L3,L1,L5)
       real*8 C
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
       subroutine slx642351(L6,L4,L2,L3,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L2,L3,L5,L1)
       real*8 C
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
       subroutine slx642513(L6,L4,L2,L5,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L2,L5,L1,L3)
       real*8 C
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
       subroutine slx642531(L6,L4,L2,L5,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L2,L5,L3,L1)
       real*8 C
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
       subroutine slx643125(L6,L4,L3,L1,L2,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L3,L1,L2,L5)
       real*8 C
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
       subroutine slx643152(L6,L4,L3,L1,L5,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L3,L1,L5,L2)
       real*8 C
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
       subroutine slx643215(L6,L4,L3,L2,L1,L5,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L3,L2,L1,L5)
       real*8 C
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
       subroutine slx643251(L6,L4,L3,L2,L5,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L3,L2,L5,L1)
       real*8 C
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
       subroutine slx643512(L6,L4,L3,L5,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L3,L5,L1,L2)
       real*8 C
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
       subroutine slx643521(L6,L4,L3,L5,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L3,L5,L2,L1)
       real*8 C
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
       subroutine slx645123(L6,L4,L5,L1,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L5,L1,L2,L3)
       real*8 C
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
       subroutine slx645132(L6,L4,L5,L1,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L5,L1,L3,L2)
       real*8 C
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
       subroutine slx645213(L6,L4,L5,L2,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L5,L2,L1,L3)
       real*8 C
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
       subroutine slx645231(L6,L4,L5,L2,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L5,L2,L3,L1)
       real*8 C
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
       subroutine slx645312(L6,L4,L5,L3,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L5,L3,L1,L2)
       real*8 C
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
       subroutine slx645321(L6,L4,L5,L3,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L4,L5,L3,L2,L1)
       real*8 C
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
       subroutine slx651234(L6,L5,L1,L2,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L1,L2,L3,L4)
       real*8 C
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
       subroutine slx651243(L6,L5,L1,L2,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L1,L2,L4,L3)
       real*8 C
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
       subroutine slx651324(L6,L5,L1,L3,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L1,L3,L2,L4)
       real*8 C
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
       subroutine slx651342(L6,L5,L1,L3,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L1,L3,L4,L2)
       real*8 C
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
       subroutine slx651423(L6,L5,L1,L4,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L1,L4,L2,L3)
       real*8 C
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
       subroutine slx651432(L6,L5,L1,L4,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L1,L4,L3,L2)
       real*8 C
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
       subroutine slx652134(L6,L5,L2,L1,L3,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L2,L1,L3,L4)
       real*8 C
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
       subroutine slx652143(L6,L5,L2,L1,L4,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L2,L1,L4,L3)
       real*8 C
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
       subroutine slx652314(L6,L5,L2,L3,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L2,L3,L1,L4)
       real*8 C
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
       subroutine slx652341(L6,L5,L2,L3,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L2,L3,L4,L1)
       real*8 C
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
       subroutine slx652413(L6,L5,L2,L4,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L2,L4,L1,L3)
       real*8 C
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
       subroutine slx652431(L6,L5,L2,L4,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L2,L4,L3,L1)
       real*8 C
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
       subroutine slx653124(L6,L5,L3,L1,L2,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L3,L1,L2,L4)
       real*8 C
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
       subroutine slx653142(L6,L5,L3,L1,L4,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L3,L1,L4,L2)
       real*8 C
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
       subroutine slx653214(L6,L5,L3,L2,L1,L4,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L3,L2,L1,L4)
       real*8 C
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
       subroutine slx653241(L6,L5,L3,L2,L4,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L3,L2,L4,L1)
       real*8 C
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
       subroutine slx653412(L6,L5,L3,L4,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L3,L4,L1,L2)
       real*8 C
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
       subroutine slx653421(L6,L5,L3,L4,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L3,L4,L2,L1)
       real*8 C
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
       subroutine slx654123(L6,L5,L4,L1,L2,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L4,L1,L2,L3)
       real*8 C
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
       subroutine slx654132(L6,L5,L4,L1,L3,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L4,L1,L3,L2)
       real*8 C
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
       subroutine slx654213(L6,L5,L4,L2,L1,L3,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L4,L2,L1,L3)
       real*8 C
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
       subroutine slx654231(L6,L5,L4,L2,L3,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L4,L2,L3,L1)
       real*8 C
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
       subroutine slx654312(L6,L5,L4,L3,L1,L2,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L4,L3,L1,L2)
       real*8 C
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
       subroutine slx654321(L6,L5,L4,L3,L2,L1,K1,K2,K3,K4,K5,K6,A,B,C)
C
       real*8 A(K1,K2,K3,K4,K5,K6)
       real*8 B(L6,L5,L4,L3,L2,L1)
       real*8 C
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
