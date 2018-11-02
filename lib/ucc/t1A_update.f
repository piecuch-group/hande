       subroutine t1A_update(N0,N1,N2,N3,K1,K2,K3,K4,shift,V1A,
     & FockR,FockB,IntR,IntB,IntM,t1A,t1B,t2A,t2B,t2C,t3A,t3B,t3C,t3D)
C
       integer a,b,c,e,f,g,h,i,j,k,m,n,o,p
       real*8 shift,PP,Coeleft
       real*8 FockR(N3,N3)
       real*8 FockB(N3,N3)
       real*8 IntR(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 IntB(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 IntM(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 t1A(N1+1:N3,N0+1:N1)
       real*8 t1B(N2+1:N3,N0+1:N2)
       real*8 t2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 t2B(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
       real*8 t2C(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 t3A(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)
       real*8 t3B(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2)
       real*8 t3C(N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N1)
       real*8 t3D(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1)
       real*8 V1A(N1+1:N3,N0+1:N1)
C
       real*8,allocatable::B1(:,:)
       real*8,allocatable::B2(:,:)
       real*8,allocatable::C1(:,:,:)
       real*8,allocatable::C2(:,:,:)
       real*8,allocatable::D1(:,:,:,:)
       real*8,allocatable::D2(:,:,:,:)
       real*8,allocatable::F2(:,:,:,:,:,:)
C
       real*8,allocatable::U3(:,:)
       real*8,allocatable::U4(:,:)
       real*8,allocatable::U6(:,:)
       real*8,allocatable::U7(:,:)
       real*8,allocatable::U9(:,:)
       real*8,allocatable::U10(:,:)
       real*8,allocatable::U11(:,:)
       real*8,allocatable::U12(:,:)
       real*8,allocatable::U13(:,:)
       real*8,allocatable::Q14(:,:)
       real*8,allocatable::Q16(:,:)
       real*8,allocatable::Q20(:,:)
       real*8,allocatable::Q26(:,:)
       real*8,allocatable::Q32(:,:)
       real*8,allocatable::Q28(:,:)
       real*8,allocatable::Q40(:,:)
       real*8,allocatable::Q36(:,:)
       real*8,allocatable::Q42(:,:)
       real*8,allocatable::X1(:,:)
       real*8,allocatable::U1(:,:)
       real*8,allocatable::X3(:,:)
       real*8,allocatable::U5(:,:)
       real*8,allocatable::Q18(:,:)
       real*8,allocatable::Q22(:,:)
       real*8,allocatable::Q24(:,:)
       real*8,allocatable::Q30(:,:)
       real*8,allocatable::X2(:,:)
       real*8,allocatable::U2(:,:)
       real*8,allocatable::Q34(:,:)
       real*8,allocatable::Q38(:,:)
       real*8,allocatable::X4(:,:)
       real*8,allocatable::U8(:,:)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(0,N3,0,N3,
     & N1,N3,N0,N1,FockR,B1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(Q14(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,B1,B2,Q14)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X1(N0+1:N1,N0+1:N1))
       X1=0.0d0
       call sum21(N0,N1,N0,N1,X1,Q14, 1.000)
       deallocate(Q14)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder4132(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N0,N1,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q16(N0+1:N1,N0+1:N1))
       I1=K1*K1
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q16)
       deallocate(D1)
       deallocate(B2)
C
       X1=X1-Q16
       deallocate(Q16)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call reorder4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N0,N1,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q20(N0+1:N1,N0+1:N1))
       I1=K1*K1
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q20)
       deallocate(D1)
       deallocate(B2)
C
       X1=X1+Q20
       deallocate(Q20)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(Q26(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,D1,D2,Q26)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N0,N1,N0,N1,X1,Q26,-0.500)
       deallocate(Q26)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2C,D2)
       allocate(Q32(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,D1,D2,Q32)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N0,N1,N0,N1,X1,Q32,-1.000)
       deallocate(Q32)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N1,N3,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q28(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q28)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N0+1:N1,N1+1:N3))
       X3=0.0d0
       X3=X3+Q28
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q28,B1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(Q40(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,B1,B2,Q40)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q28)
C
       call sum21(N0,N1,N0,N1,X1,Q40, 1.000)
       deallocate(Q40)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N1,N3,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q36(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q36)
       deallocate(D1)
       deallocate(B2)
C
       X3=X3+Q36
C
       call sumx21(0,N3,N0,N1,N1,N3,X3,FockR, 1.000)
C
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(U5(N1+1:N3,N0+1:N1))
       I2=K1*K3
       I3=K3*K1
       call jungemm2(I2,I3,X3,D2,U5)
       deallocate(D2)
C
       V1A=V1A-U5
       deallocate(U5)
       deallocate(X3)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q36,B1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(Q42(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,B1,B2,Q42)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q36)
C
       call sum21(N0,N1,N0,N1,X1,Q42, 1.000)
       deallocate(Q42)
C
       call sumx21(0,N3,N0,N1,N0,N1,X1,FockR, 1.000)
C
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(U1(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,X1,B2,U1)
       deallocate(B2)
C
       V1A=V1A-U1
       deallocate(U1)
       deallocate(X1)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call reorder2431(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q18(N1+1:N3,N1+1:N3))
       I1=K3*K3
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q18)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X2(N1+1:N3,N1+1:N3))
       X2=0.0d0
       X2=X2+Q18
       deallocate(Q18)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call reorder2431(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q22(N1+1:N3,N1+1:N3))
       I1=K3*K3
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q22)
       deallocate(D1)
       deallocate(B2)
C
       X2=X2+Q22
       deallocate(Q22)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(Q24(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,D2,Q24)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N1,N3,N1,N3,X2,Q24, 0.500)
       deallocate(Q24)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2C,D2)
       allocate(Q30(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,Q30)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N1,N3,N1,N3,X2,Q30, 1.000)
       deallocate(Q30)
C
       call sumx21(0,N3,N1,N3,N1,N3,X2,FockR, 1.000)
C
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(U2(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,X2,B2,U2)
       deallocate(B2)
C
       call
     & sum21(N1,N3,N0,N1,V1A,U2, 1.000)
       deallocate(U2)
       deallocate(X2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N2,N3,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q34(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q34)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(N0+1:N2,N2+1:N3))
       X4=0.0d0
       X4=X4+Q34
       deallocate(Q34)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q38(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q38)
       deallocate(D1)
       deallocate(B2)
C
       X4=X4+Q38
       deallocate(Q38)
C
       call sumx21(0,N3,N0,N2,N2,N3,X4,FockB, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2C,D2)
       allocate(U8(N1+1:N3,N0+1:N1))
       I2=K1*K3
       I3=K4*K2
       call jungemm2(I2,I3,X4,D2,U8)
       deallocate(D2)
C
       V1A=V1A-U8
       deallocate(U8)
       deallocate(X4)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder2314(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(U3(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,U3)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A-U3
       deallocate(U3)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(U4(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,U4)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A+U4
       deallocate(U4)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call reorder4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(U6(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,D2,U6)
       deallocate(D1)
       deallocate(D2)
C
       V1A=V1A-0.500*U6
       deallocate(U6)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call reorder2431(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(U7(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,D1,D2,U7)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N1,N3,N0,N1,V1A,U7,-0.500)
       deallocate(U7)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call reorder4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N0,N1,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2C,D2)
       allocate(U9(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U9)
       deallocate(D1)
       deallocate(D2)
C
       V1A=V1A+U9
       deallocate(U9)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call reorder2431(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2C,D2)
       allocate(U10(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,D1,D2,U10)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N1,N3,N0,N1,V1A,U10,-1.000)
       deallocate(U10)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,t3A,F2)
       allocate(U11(N1+1:N3,N0+1:N1))
       I2=K1*K3
       I3=K3*K3*K1*K1
       call jungemm2(I2,I3,D1,F2,U11)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A-0.250*U11
       deallocate(U11)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder542136(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,t3C,F2)
       allocate(U12(N1+1:N3,N0+1:N1))
       I2=K1*K3
       I3=K3*K4*K1*K2
       call jungemm2(I2,I3,D1,F2,U12)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A-U12
       deallocate(U12)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,t3D,F2)
       allocate(U13(N1+1:N3,N0+1:N1))
       I2=K1*K3
       I3=K4*K4*K2*K2
       call jungemm2(I2,I3,D1,F2,U13)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A-0.250*U13
       deallocate(U13)
C
       do i=N0+1,N1
       do a=N1+1,N3
         CoeLeft=FockR(a,a)
     &          -FockR(i,i)
     &          +shift
         t1A(a,i)=t1A(a,i)-V1A(a,i)/CoeLeft
       enddo
       enddo
C
       end
