       subroutine t3A_update(N0,N1,N2,N3,K1,K2,K3,K4,shift,V3A,
     & FockR,FockB,IntR,IntB,IntM,t1A,t1B,t2A,t2B,t2C,t3A,t3B,t3C,t3D,
     & p_space)
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
       real*8 V3A(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)
       integer p_space(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)
C
       real*8,allocatable::B1(:,:)
       real*8,allocatable::B2(:,:)
       real*8,allocatable::C1(:,:,:)
       real*8,allocatable::C2(:,:,:)
       real*8,allocatable::D1(:,:,:,:)
       real*8,allocatable::D2(:,:,:,:)
       real*8,allocatable::F2(:,:,:,:,:,:)
C
       real*8,allocatable::S5(:,:,:,:)
       real*8,allocatable::S28(:,:,:,:)
       real*8,allocatable::U15(:,:,:,:,:,:)
       real*8,allocatable::S2(:,:,:,:)
       real*8,allocatable::X9(:,:,:,:)
       real*8,allocatable::U10(:,:,:,:,:,:)
       real*8,allocatable::S1(:,:,:,:)
       real*8,allocatable::S11(:,:,:,:)
       real*8,allocatable::S15(:,:,:,:)
       real*8,allocatable::S7(:,:,:,:)
       real*8,allocatable::S31(:,:,:,:)
       real*8,allocatable::S30(:,:,:,:)
       real*8,allocatable::S19(:,:,:,:)
       real*8,allocatable::S42(:,:,:,:)
       real*8,allocatable::Q16(:,:)
       real*8,allocatable::S43(:,:,:,:)
       real*8,allocatable::Q14(:,:)
       real*8,allocatable::Q15(:,:)
       real*8,allocatable::S46(:,:,:,:)
       real*8,allocatable::S32(:,:,:,:)
       real*8,allocatable::S34(:,:,:,:)
       real*8,allocatable::S48(:,:,:,:)
       real*8,allocatable::Q11(:,:)
       real*8,allocatable::S33(:,:,:,:)
       real*8,allocatable::S47(:,:,:,:)
       real*8,allocatable::X1(:,:,:,:)
       real*8,allocatable::U1(:,:,:,:,:,:)
       real*8,allocatable::S4(:,:,:,:)
       real*8,allocatable::S6(:,:,:,:)
       real*8,allocatable::S29(:,:,:,:)
       real*8,allocatable::X2(:,:,:,:)
       real*8,allocatable::U2(:,:,:,:,:,:)
       real*8,allocatable::S21(:,:,:,:)
       real*8,allocatable::S40(:,:,:,:)
       real*8,allocatable::S41(:,:,:,:)
       real*8,allocatable::S26(:,:,:,:)
       real*8,allocatable::S44(:,:,:,:)
       real*8,allocatable::S45(:,:,:,:)
       real*8,allocatable::X6(:,:,:,:)
       real*8,allocatable::U6(:,:,:,:,:,:)
       real*8,allocatable::S13(:,:,:,:)
       real*8,allocatable::S16(:,:,:,:)
       real*8,allocatable::X12(:,:,:,:)
       real*8,allocatable::U27(:,:,:,:,:,:)
       real*8,allocatable::S14(:,:,:,:)
       real*8,allocatable::S17(:,:,:,:)
       real*8,allocatable::X13(:,:,:,:)
       real*8,allocatable::U28(:,:,:,:,:,:)
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::Q3(:,:)
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::Q9(:,:)
       real*8,allocatable::S37(:,:,:,:)
       real*8,allocatable::S38(:,:,:,:)
       real*8,allocatable::Q13(:,:)
       real*8,allocatable::X3(:,:)
       real*8,allocatable::U3(:,:,:,:,:,:)
       real*8,allocatable::S9(:,:,:,:)
       real*8,allocatable::S10(:,:,:,:)
       real*8,allocatable::S25(:,:,:,:)
       real*8,allocatable::S27(:,:,:,:)
       real*8,allocatable::X8(:,:,:,:)
       real*8,allocatable::U8(:,:,:,:,:,:)
       real*8,allocatable::S3(:,:,:,:)
       real*8,allocatable::X10(:,:,:,:)
       real*8,allocatable::U11(:,:,:,:,:,:)
       real*8,allocatable::S8(:,:,:,:)
       real*8,allocatable::U19(:,:,:,:,:,:)
       real*8,allocatable::S22(:,:,:,:)
       real*8,allocatable::S39(:,:,:,:)
       real*8,allocatable::S35(:,:,:,:)
       real*8,allocatable::Q12(:,:)
       real*8,allocatable::S36(:,:,:,:)
       real*8,allocatable::X7(:,:,:,:)
       real*8,allocatable::U7(:,:,:,:,:,:)
       real*8,allocatable::S12(:,:,:,:)
       real*8,allocatable::X11(:,:,:,:)
       real*8,allocatable::U26(:,:,:,:,:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::Q4(:,:)
       real*8,allocatable::Q6(:,:)
       real*8,allocatable::Q8(:,:)
       real*8,allocatable::Q10(:,:)
       real*8,allocatable::X4(:,:)
       real*8,allocatable::U4(:,:,:,:,:,:)
       real*8,allocatable::X5(:,:,:,:)
       real*8,allocatable::U5(:,:,:,:,:,:)
       real*8,allocatable::S18(:,:,:,:)
       real*8,allocatable::S23(:,:,:,:)
       real*8,allocatable::X14(:,:,:,:)
       real*8,allocatable::U32(:,:,:,:,:,:)
       real*8,allocatable::S20(:,:,:,:)
       real*8,allocatable::S24(:,:,:,:)
       real*8,allocatable::X15(:,:,:,:)
       real*8,allocatable::U34(:,:,:,:,:,:)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       call reorder1432(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N0,N1,N0,N1,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S5(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S5)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call reorder2341(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,S5,D1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,t3A,F2)
       allocate(U15(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K3*K3*K3
       I3=K1*K1
       call jungemm(I1,I2,I3,D1,F2,U15)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum123645(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U15, 0.500)
       call
     & sum123546(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U15,-0.500)
       call
     & sum123654(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U15,-0.500)
       call
     & sum123564(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U15, 0.500)
       V3A=V3A+0.500*U15
       call
     & sum123465(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U15,-0.500)
       deallocate(U15)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call reorder3241(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,S5,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S28(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S28)
       deallocate(D1)
       deallocate(B2)
       deallocate(S5)
C
       allocate(X9(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       X9=0.0d0
       call sum2134(N0,N1,N1,N3,N0,N1,N0,N1,X9,S28,-1.000)
       deallocate(S28)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call reorder3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S2(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S2)
       deallocate(D1)
       deallocate(B2)
C
       call sum4123(N0,N1,N1,N3,N0,N1,N0,N1,X9,S2, 1.000)
       deallocate(S2)
C
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(U10(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K1*K3*K3
       I3=K1
       call jungemm(I1,I2,I3,X9,D2,U10)
       deallocate(D2)
C
       call
     & sum236145(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U10, 1.000)
       call
     & sum136245(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U10,-1.000)
       call
     & sum126345(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U10, 1.000)
       call
     & sum235146(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U10,-1.000)
       call
     & sum135246(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U10, 1.000)
       call
     & sum125346(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U10,-1.000)
       call
     & sum236154(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U10,-1.000)
       call
     & sum136254(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U10, 1.000)
       call
     & sum126354(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U10,-1.000)
       call
     & sum235164(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U10, 1.000)
       call
     & sum135264(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U10,-1.000)
       call
     & sum125364(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U10, 1.000)
       call
     & sum234156(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U10, 1.000)
       call
     & sum134256(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U10,-1.000)
       call
     & sum124356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U10, 1.000)
       call
     & sum234165(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U10,-1.000)
       call
     & sum134265(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U10, 1.000)
       call
     & sum124365(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U10,-1.000)
       deallocate(U10)
       deallocate(X9)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call reorder3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N0,N1,N0,N1,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S1(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       X1=0.0d0
       call sum2134(N0,N1,N1,N3,N0,N1,N0,N1,X1,S1,-1.000)
       deallocate(S1)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(0,N3,0,N3,
     & N1,N3,N0,N1,FockR,B1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(S11(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K1*K3
       I3=K3
       call jungemm(I1,I2,I3,B1,D2,S11)
       deallocate(B1)
       deallocate(D2)
C
       call sum2341(N0,N1,N1,N3,N0,N1,N0,N1,X1,S11,-1.000)
       deallocate(S11)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N0,N1,N1,N3,IntR,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(S15(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K1*K1
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,D2,S15)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N1,N1,N3,N0,N1,N0,N1,X1,S15,-0.500)
       deallocate(S15)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S7(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S7)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X6(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       X6=0.0d0
       call sum4123(N0,N1,N1,N3,N1,N3,N0,N1,X6,S7,-1.000)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder2341(N0,N1,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,S7,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S31(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S31)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X10(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       X10=0.0d0
       call sum3124(N1,N3,N1,N3,N1,N3,N0,N1,X10,S31,-1.000)
       deallocate(S31)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call reorder3241(N0,N1,N0,N1,N1,N3,N1,N3,
     & N1,N3,N0,N1,N1,N3,N0,N1,S7,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S30(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S30)
       deallocate(D1)
       deallocate(B2)
       deallocate(S7)
C
       call sum3124(N0,N1,N1,N3,N0,N1,N0,N1,X1,S30, 1.000)
       deallocate(S30)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder2143(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N0,N1,N0,N1,IntR,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(S19(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K1
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,D2,S19)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X5(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       X5=0.0d0
       call sum3412(N0,N1,N0,N1,N0,N1,N0,N1,X5,S19,-0.500)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call reorder4312(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,S19,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S42(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S42)
       deallocate(D1)
       deallocate(B2)
       deallocate(S19)
C
       call sum2134(N0,N1,N1,N3,N0,N1,N0,N1,X1,S42, 0.500)
       deallocate(S42)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N1,N3,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q16(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q16)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q16,B1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(S43(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K1*K3
       I3=K3
       call jungemm(I1,I2,I3,B1,D2,S43)
       deallocate(B1)
       deallocate(D2)
       deallocate(Q16)
C
       call sum2341(N0,N1,N1,N3,N0,N1,N0,N1,X1,S43,-1.000)
       deallocate(S43)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N1,N3,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q14(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q14)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q15(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,Q14,B2,Q15)
       deallocate(B2)
C
       allocate(X4(N1+1:N3,N1+1:N3))
       X4=0.0d0
       call sum21(N1,N3,N1,N3,X4,Q15,-1.000)
       deallocate(Q15)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q14,B1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(S46(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K1*K3
       I3=K3
       call jungemm(I1,I2,I3,B1,D2,S46)
       deallocate(B1)
       deallocate(D2)
       deallocate(Q14)
C
       call sum2341(N0,N1,N1,N3,N0,N1,N0,N1,X1,S46,-1.000)
       deallocate(S46)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       call reorder1432(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N0,N1,N1,N3,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S32(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S32)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call reorder3241(N0,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,S32,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S34(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S34)
       deallocate(D1)
       deallocate(B2)
C
       call sum3124(N0,N1,N1,N3,N1,N3,N0,N1,X6,S34, 1.000)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder2314(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,S34,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S48(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S48)
       deallocate(D1)
       deallocate(B2)
       deallocate(S34)
C
       allocate(X2(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       X2=0.0d0
       call sum2134(N1,N3,N1,N3,N1,N3,N0,N1,X2,S48,-1.000)
       deallocate(S48)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder3421(N0,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,N0,N1,S32,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q11(N0+1:N1,N0+1:N1))
       I1=K1*K1
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q11)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N0+1:N1,N0+1:N1))
       X3=0.0d0
       X3=X3-Q11
       deallocate(Q11)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       call reorder4231(N0,N1,N0,N1,N0,N1,N1,N3,
     & N1,N3,N0,N1,N0,N1,N0,N1,S32,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S33(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S33)
       deallocate(D1)
       deallocate(B2)
       deallocate(S32)
C
       call sum3124(N0,N1,N0,N1,N0,N1,N0,N1,X5,S33, 1.000)
C
       call sumx4321(N0,N3,N0,N1,N0,N1,N0,N1,N0,N1,X5,IntR, 1.000)
C
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,t3A,F2)
       allocate(U5(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K3*K3*K3
       I3=K1*K1
       call jungemm(I1,I2,I3,X5,F2,U5)
       deallocate(F2)
C
       call
     & sum123645(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U5, 0.500)
       call
     & sum123546(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U5,-0.500)
       V3A=V3A+0.500*U5
       deallocate(U5)
       deallocate(X5)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call reorder3214(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,S33,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S47(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S47)
       deallocate(D1)
       deallocate(B2)
       deallocate(S33)
C
       call sum2134(N0,N1,N1,N3,N0,N1,N0,N1,X1,S47,-1.000)
       deallocate(S47)
C
       call sumx2143(N0,N3,N0,N1,N1,N3,N0,N1,N0,N1,X1,IntR, 1.000)
C
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(U1(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K1*K3*K3
       I3=K1
       call jungemm(I1,I2,I3,X1,D2,U1)
       deallocate(D2)
C
       call
     & sum236145(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U1, 1.000)
       call
     & sum136245(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U1,-1.000)
       call
     & sum126345(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U1, 1.000)
       call
     & sum235146(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U1,-1.000)
       call
     & sum135246(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U1, 1.000)
       call
     & sum125346(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U1,-1.000)
       call
     & sum234156(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U1, 1.000)
       call
     & sum134256(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U1,-1.000)
       call
     & sum124356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U1, 1.000)
       deallocate(U1)
       deallocate(X1)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       call reorder3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N1,N3,N1,N3,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S4(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       I1=K3*K3*K3
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S4)
       deallocate(D1)
       deallocate(B2)
C
       call sum4123(N1,N3,N1,N3,N1,N3,N0,N1,X2,S4,-1.000)
       deallocate(S4)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call reorder3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S6(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S6)
       deallocate(D1)
       deallocate(B2)
C
       call sum3124(N0,N1,N1,N3,N1,N3,N0,N1,X6,S6,-1.000)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder2314(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,S6,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S29(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S29)
       deallocate(D1)
       deallocate(B2)
       deallocate(S6)
C
       call sum2134(N1,N3,N1,N3,N1,N3,N0,N1,X2,S29, 1.000)
       deallocate(S29)
C
       call sumx3214(N0,N3,N1,N3,N1,N3,N1,N3,N0,N1,X2,IntR, 1.000)
C
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(U2(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1*K1*K3
       I3=K3
       call jungemm(I1,I2,I3,X2,D2,U2)
       deallocate(D2)
C
       call
     & sum356124(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U2, 1.000)
       call
     & sum256134(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U2,-1.000)
       call
     & sum156234(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U2, 1.000)
       call
     & sum346125(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U2,-1.000)
       call
     & sum246135(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U2, 1.000)
       call
     & sum146235(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U2,-1.000)
       call
     & sum345126(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U2, 1.000)
       call
     & sum245136(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U2,-1.000)
       call
     & sum145236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U2, 1.000)
       deallocate(U2)
       deallocate(X2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S21(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S21)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N1,N1,N3,N1,N3,N0,N1,X6,S21, 1.000)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call reorder4312(N1,N3,N0,N1,N0,N1,N1,N3,
     & N1,N3,N0,N1,N1,N3,N0,N1,S21,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S40(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S40)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X12(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       X12=0.0d0
       call sum3124(N0,N1,N1,N3,N0,N1,N0,N1,X12,S40, 1.000)
       deallocate(S40)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,S21,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S41(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S41)
       deallocate(D1)
       deallocate(B2)
       deallocate(S21)
C
       allocate(X13(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       X13=0.0d0
       call sum2134(N1,N3,N1,N3,N1,N3,N0,N1,X13,S41, 1.000)
       deallocate(S41)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2C,D2)
       allocate(S26(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S26)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N1,N1,N3,N1,N3,N0,N1,X6,S26, 1.000)
C
       call sumx3124(N0,N3,N0,N1,N1,N3,N1,N3,N0,N1,X6,IntR, 1.000)
C
       allocate(F2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,t3A,F2)
       allocate(U6(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K1*K3*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,X6,F2,U6)
       deallocate(F2)
C
       call
     & sum235614(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U6,-1.000)
       call
     & sum135624(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U6, 1.000)
       call
     & sum125634(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U6,-1.000)
       call
     & sum234615(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U6, 1.000)
       call
     & sum134625(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U6,-1.000)
       call
     & sum124635(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U6, 1.000)
       call
     & sum234516(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U6,-1.000)
       call
     & sum134526(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U6, 1.000)
       call
     & sum124536(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U6,-1.000)
       deallocate(U6)
       deallocate(X6)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call reorder4312(N1,N3,N0,N1,N0,N1,N1,N3,
     & N1,N3,N0,N1,N1,N3,N0,N1,S26,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S44(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S44)
       deallocate(D1)
       deallocate(B2)
C
       call sum3124(N0,N1,N1,N3,N0,N1,N0,N1,X12,S44, 1.000)
       deallocate(S44)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,S26,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S45(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S45)
       deallocate(D1)
       deallocate(B2)
       deallocate(S26)
C
       call sum2134(N1,N3,N1,N3,N1,N3,N0,N1,X13,S45, 1.000)
       deallocate(S45)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N0,N1,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S13(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S13)
       deallocate(D1)
       deallocate(D2)
C
       call sum2413(N0,N1,N1,N3,N0,N1,N0,N1,X12,S13, 1.000)
       deallocate(S13)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call reorder4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N0,N1,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2C,D2)
       allocate(S16(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S16)
       deallocate(D1)
       deallocate(D2)
C
       call sum2413(N0,N1,N1,N3,N0,N1,N0,N1,X12,S16, 1.000)
       deallocate(S16)
C
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(U27(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K1*K3*K3
       I3=K1
       call jungemm(I1,I2,I3,X12,D2,U27)
       deallocate(D2)
C
       call
     & sum125346(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U27, 1.000)
       call
     & sum135246(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U27,-1.000)
       call
     & sum236145(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U27,-1.000)
       call
     & sum235146(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U27, 1.000)
       call
     & sum136245(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U27, 1.000)
       call
     & sum126345(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U27,-1.000)
       call
     & sum124356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U27,-1.000)
       call
     & sum134256(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U27, 1.000)
       call
     & sum236154(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U27, 1.000)
       call
     & sum234156(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U27,-1.000)
       call
     & sum136254(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U27,-1.000)
       call
     & sum126354(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U27, 1.000)
       call
     & sum124365(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U27, 1.000)
       call
     & sum134265(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U27,-1.000)
       call
     & sum235164(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U27,-1.000)
       call
     & sum234165(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U27, 1.000)
       call
     & sum135264(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U27, 1.000)
       call
     & sum125364(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U27,-1.000)
       deallocate(U27)
       deallocate(X12)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call reorder2341(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S14(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S14)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N1,N3,N1,N3,N1,N3,N0,N1,X13,S14, 1.000)
       deallocate(S14)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call reorder2431(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2C,D2)
       allocate(S17(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S17)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N1,N3,N1,N3,N1,N3,N0,N1,X13,S17,-1.000)
       deallocate(S17)
C
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(U28(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1*K1*K3
       I3=K3
       call jungemm(I1,I2,I3,X13,D2,U28)
       deallocate(D2)
C
       call
     & sum245136(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U28, 1.000)
       call
     & sum345126(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U28,-1.000)
       call
     & sum145236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U28,-1.000)
       call
     & sum145326(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U28, 1.000)
       call
     & sum345216(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U28, 1.000)
       call
     & sum245316(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U28,-1.000)
       call
     & sum246135(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U28,-1.000)
       call
     & sum346125(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U28, 1.000)
       call
     & sum146235(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U28, 1.000)
       call
     & sum146325(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U28,-1.000)
       call
     & sum346215(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U28,-1.000)
       call
     & sum246315(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U28, 1.000)
       call
     & sum356124(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U28,-1.000)
       call
     & sum256134(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U28, 1.000)
       call
     & sum356214(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U28, 1.000)
       call
     & sum256314(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U28,-1.000)
       call
     & sum156234(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U28,-1.000)
       call
     & sum156324(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U28, 1.000)
       deallocate(U28)
       deallocate(X13)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(0,N3,0,N3,
     & N1,N3,N0,N1,FockR,B1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(Q1(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,B1,B2,Q1)
       deallocate(B1)
       deallocate(B2)
C
       call sum21(N0,N1,N0,N1,X3,Q1, 1.000)
       deallocate(Q1)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N0,N1,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q3(N0+1:N1,N0+1:N1))
       I1=K1*K1
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q3)
       deallocate(D1)
       deallocate(B2)
C
       X3=X3+Q3
       deallocate(Q3)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call reorder4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N0,N1,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q5(N0+1:N1,N0+1:N1))
       I1=K1*K1
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q5)
       deallocate(D1)
       deallocate(B2)
C
       X3=X3+Q5
       deallocate(Q5)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(Q7(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,D1,D2,Q7)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N0,N1,N0,N1,X3,Q7, 0.500)
       deallocate(Q7)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2C,D2)
       allocate(Q9(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,D1,D2,Q9)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N0,N1,N0,N1,X3,Q9,-1.000)
       deallocate(Q9)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       call reorder1432(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N2,N0,N1,N2,N3,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S37(N0+1:N1,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4*K1*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S37)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,N0+1:N1))
       call reorder3241(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N1,N0,N2,N2,N3,N0,N1,S37,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S38(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S38)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X8(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       X8=0.0d0
       call sum3124(N0,N2,N2,N3,N1,N3,N0,N1,X8,S38,-1.000)
       deallocate(S38)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call reorder2431(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,N0,N1,S37,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q13(N0+1:N1,N0+1:N1))
       I1=K1*K1
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q13)
       deallocate(D1)
       deallocate(B2)
       deallocate(S37)
C
       X3=X3+Q13
       deallocate(Q13)
C
       call sumx21(0,N3,N0,N1,N0,N1,X3,FockR, 1.000)
C
       allocate(F2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,t3A,F2)
       allocate(U3(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K1*K3*K3*K3
       I3=K1
       call jungemm(I1,I2,I3,X3,F2,U3)
       deallocate(F2)
C
       call
     & sum123564(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U3,-1.000)
       call
     & sum123465(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U3, 1.000)
       V3A=V3A-U3
       deallocate(U3)
       deallocate(X3)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,N0+1:N1))
       call reorder3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N2,N2,N3,N0,N1,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S9(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S9)
       deallocate(D1)
       deallocate(B2)
C
       call sum3124(N0,N2,N2,N3,N1,N3,N0,N1,X8,S9,-1.000)
       deallocate(S9)
C
       allocate(D1(N1+1:N3,N0+1:N2,N2+1:N3,N1+1:N3))
       call reorder3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N2,N2,N3,N1,N3,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S10(N0+1:N1,N0+1:N2,N2+1:N3,N1+1:N3))
       I1=K3*K4*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S10)
       deallocate(D1)
       deallocate(B2)
C
       call sum4123(N0,N2,N2,N3,N1,N3,N0,N1,X8,S10, 1.000)
       deallocate(S10)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N2,N3,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S25(N1+1:N3,N0+1:N1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S25)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N2,N2,N3,N1,N3,N0,N1,X8,S25,-1.000)
       deallocate(S25)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2C,D2)
       allocate(S27(N1+1:N3,N0+1:N1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S27)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N2,N2,N3,N1,N3,N0,N1,X8,S27,-1.000)
       deallocate(S27)
C
       call sumx3142(N0,N3,N0,N2,N2,N3,N1,N3,N0,N1,X8,IntM, 1.000)
C
       allocate(F2(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder521346(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N1,N3,N0,N1,N0,N1,t3C,F2)
       allocate(U8(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K1*K3*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,X8,F2,U8)
       deallocate(F2)
C
       call
     & sum235614(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U8, 1.000)
       call
     & sum135624(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U8,-1.000)
       call
     & sum125634(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U8, 1.000)
       call
     & sum234615(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U8,-1.000)
       call
     & sum134625(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U8, 1.000)
       call
     & sum124635(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U8,-1.000)
       call
     & sum234516(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U8, 1.000)
       call
     & sum134526(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U8,-1.000)
       call
     & sum124536(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U8, 1.000)
       deallocate(U8)
       deallocate(X8)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder2314(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S3(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S3)
       deallocate(D1)
       deallocate(B2)
C
       call sum3124(N1,N3,N1,N3,N1,N3,N0,N1,X10,S3, 1.000)
       deallocate(S3)
C
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(U11(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1*K1*K3
       I3=K3
       call jungemm(I1,I2,I3,X10,D2,U11)
       deallocate(D2)
C
       call
     & sum356124(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U11, 1.000)
       call
     & sum256134(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U11,-1.000)
       call
     & sum356214(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U11,-1.000)
       call
     & sum256314(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U11, 1.000)
       call
     & sum156234(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U11, 1.000)
       call
     & sum156324(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U11,-1.000)
       call
     & sum346125(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U11,-1.000)
       call
     & sum246135(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U11, 1.000)
       call
     & sum346215(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U11, 1.000)
       call
     & sum246315(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U11,-1.000)
       call
     & sum146235(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U11,-1.000)
       call
     & sum146325(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U11, 1.000)
       call
     & sum345126(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U11, 1.000)
       call
     & sum245136(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U11,-1.000)
       call
     & sum345216(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U11,-1.000)
       call
     & sum245316(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U11, 1.000)
       call
     & sum145236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U11, 1.000)
       call
     & sum145326(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U11,-1.000)
       deallocate(U11)
       deallocate(X10)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call reorder2431(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S8(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       I1=K3*K3*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S8)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       call reorder2341(N1,N3,N1,N3,N1,N3,N1,N3,
     & N1,N3,N1,N3,N1,N3,N1,N3,S8,D1)
       allocate(F2(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       call reorder123456(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,t3A,F2)
       allocate(U19(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K1*K1*K1*K3
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,F2,U19)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum345612(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U19, 0.500)
       call
     & sum245613(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U19,-0.500)
       call
     & sum345621(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U19,-0.500)
       call
     & sum245631(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U19, 0.500)
       call
     & sum145623(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U19, 0.500)
       call
     & sum145632(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U19,-0.500)
       deallocate(U19)
       deallocate(S8)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(S22(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K3*K3
       I3=K1*K1
       call jungemm(I1,I2,I3,D1,D2,S22)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X7(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       X7=0.0d0
       call sum3412(N1,N3,N1,N3,N1,N3,N1,N3,X7,S22,-0.500)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       call reorder4312(N1,N3,N1,N3,N1,N3,N1,N3,
     & N1,N3,N1,N3,N1,N3,N1,N3,S22,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S39(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       I1=K3*K3*K3
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S39)
       deallocate(D1)
       deallocate(B2)
       deallocate(S22)
C
       allocate(X11(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       X11=0.0d0
       call sum4123(N1,N3,N1,N3,N1,N3,N0,N1,X11,S39,-1.000)
       deallocate(S39)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S35(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S35)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call reorder2431(N1,N3,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,S35,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q12(N1+1:N3,N1+1:N3))
       I1=K3*K3
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q12)
       deallocate(D1)
       deallocate(B2)
C
       X4=X4+Q12
       deallocate(Q12)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call reorder2341(N1,N3,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,S35,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S36(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       I1=K3*K3*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S36)
       deallocate(D1)
       deallocate(B2)
       deallocate(S35)
C
       call sum3124(N1,N3,N1,N3,N1,N3,N1,N3,X7,S36, 1.000)
       deallocate(S36)
C
       call sumx4321(N0,N3,N1,N3,N1,N3,N1,N3,N1,N3,X7,IntR, 1.000)
C
       allocate(F2(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       call reorder123456(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,t3A,F2)
       allocate(U7(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K1*K1*K1*K3
       I3=K3*K3
       call jungemm(I1,I2,I3,X7,F2,U7)
       deallocate(F2)
C
       call
     & sum345612(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U7, 0.500)
       call
     & sum245613(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U7,-0.500)
       call
     & sum145623(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U7, 0.500)
       deallocate(U7)
       deallocate(X7)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call reorder4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(S12(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K3*K3
       I3=K1*K1
       call jungemm(I1,I2,I3,D1,D2,S12)
       deallocate(D1)
       deallocate(D2)
C
       call sum2314(N1,N3,N1,N3,N1,N3,N0,N1,X11,S12, 1.000)
       deallocate(S12)
C
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(U26(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1*K1*K3
       I3=K3
       call jungemm(I1,I2,I3,X11,D2,U26)
       deallocate(D2)
C
       call
     & sum156234(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U26,-0.500)
       call
     & sum256134(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U26, 0.500)
       call
     & sum356124(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U26,-0.500)
       call
     & sum146235(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U26, 0.500)
       call
     & sum246135(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U26,-0.500)
       call
     & sum346125(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U26, 0.500)
       call
     & sum145236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U26,-0.500)
       call
     & sum245136(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U26, 0.500)
       call
     & sum345126(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U26,-0.500)
       deallocate(U26)
       deallocate(X11)
C
       allocate(B1(N0+1:N1,N1+1:N3))
       call reorder12(0,N3,0,N3,
     & N0,N1,N1,N3,FockR,B1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q2(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,B1,B2,Q2)
       deallocate(B1)
       deallocate(B2)
C
       call sum21(N1,N3,N1,N3,X4,Q2,-1.000)
       deallocate(Q2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call reorder2341(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q4(N1+1:N3,N1+1:N3))
       I1=K3*K3
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q4)
       deallocate(D1)
       deallocate(B2)
C
       X4=X4-Q4
       deallocate(Q4)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call reorder2431(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q6(N1+1:N3,N1+1:N3))
       I1=K3*K3
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q6)
       deallocate(D1)
       deallocate(B2)
C
       X4=X4+Q6
       deallocate(Q6)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(Q8(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,D2,Q8)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N1,N3,N1,N3,X4,Q8,-0.500)
       deallocate(Q8)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2C,D2)
       allocate(Q10(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,Q10)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N1,N3,N1,N3,X4,Q10, 1.000)
       deallocate(Q10)
C
       call sumx21(0,N3,N1,N3,N1,N3,X4,FockR, 1.000)
C
       allocate(F2(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       call reorder123456(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,t3A,F2)
       allocate(U4(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1*K1*K1*K3*K3
       I3=K3
       call jungemm(I1,I2,I3,X4,F2,U4)
       deallocate(F2)
C
       call
     & sum234561(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U4, 1.000)
       call
     & sum134562(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U4,-1.000)
       call
     & sum124563(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U4, 1.000)
       deallocate(U4)
       deallocate(X4)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,t3A,F2)
       allocate(S18(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1*K3*K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,F2,S18)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X14(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       X14=0.0d0
       call sum2341(N1,N3,N1,N3,N1,N3,N0,N1,X14,S18, 1.000)
       deallocate(S18)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder542136(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,t3C,F2)
       allocate(S23(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1*K3*K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,F2,S23)
       deallocate(D1)
       deallocate(F2)
C
       call sum2341(N1,N3,N1,N3,N1,N3,N0,N1,X14,S23, 2.000)
       deallocate(S23)
C
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(U32(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1*K1*K3
       I3=K3
       call jungemm(I1,I2,I3,X14,D2,U32)
       deallocate(D2)
C
       call
     & sum145236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U32,-0.500)
       call
     & sum245136(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U32, 0.500)
       call
     & sum345126(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U32,-0.500)
       call
     & sum146235(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U32, 0.500)
       call
     & sum246135(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U32,-0.500)
       call
     & sum346125(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U32, 0.500)
       call
     & sum156234(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U32,-0.500)
       call
     & sum256134(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U32, 0.500)
       call
     & sum356124(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U32,-0.500)
       deallocate(U32)
       deallocate(X14)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(F2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,t3A,F2)
       allocate(S20(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K1*K3
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,D1,F2,S20)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X15(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       X15=0.0d0
       call sum2341(N0,N1,N1,N3,N0,N1,N0,N1,X15,S20, 1.000)
       deallocate(S20)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder521346(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N1,N3,N0,N1,N0,N1,t3C,F2)
       allocate(S24(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K1*K3
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,D1,F2,S24)
       deallocate(D1)
       deallocate(F2)
C
       call sum2341(N0,N1,N1,N3,N0,N1,N0,N1,X15,S24, 2.000)
       deallocate(S24)
C
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(U34(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K1*K3*K3
       I3=K1
       call jungemm(I1,I2,I3,X15,D2,U34)
       deallocate(D2)
C
       call
     & sum124356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U34,-0.500)
       call
     & sum134256(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U34, 0.500)
       call
     & sum234156(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U34,-0.500)
       call
     & sum125346(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U34, 0.500)
       call
     & sum135246(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U34,-0.500)
       call
     & sum235146(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U34, 0.500)
       call
     & sum126345(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U34,-0.500)
       call
     & sum136245(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U34, 0.500)
       call
     & sum236145(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U34,-0.500)
       deallocate(U34)
       deallocate(X15)
C
       do i=N0+1,N1-2
       do j=i+1,N1-1
       do k=j+1,N1
       do a=N1+1,N3-2
       do b=a+1,N3-1
       do c=b+1,N3
         CoeLeft=FockR(c,c)
     &          +FockR(b,b)
     &          +FockR(a,a)
     &          -FockR(k,k)
     &          -FockR(j,j)
     &          -FockR(i,i)
     &          +shift
         if (p_space(c,b,a,k,j,i).eq.0) then
         t3A(c,b,a,k,j,i)=0.0d0
         else
         t3A(c,b,a,k,j,i)=t3A(c,b,a,k,j,i)-V3A(c,b,a,k,j,i)/CoeLeft
         endif
         t3A(c,b,a,k,i,j)=-t3A(c,b,a,k,j,i)
         t3A(c,b,a,i,j,k)=-t3A(c,b,a,k,j,i)
         t3A(c,b,a,i,k,j)= t3A(c,b,a,k,j,i)
         t3A(c,b,a,j,k,i)=-t3A(c,b,a,k,j,i)
         t3A(c,b,a,j,i,k)= t3A(c,b,a,k,j,i)
         t3A(c,a,b,k,j,i)=-t3A(c,b,a,k,j,i)
         t3A(c,a,b,k,i,j)= t3A(c,b,a,k,j,i)
         t3A(c,a,b,i,j,k)= t3A(c,b,a,k,j,i)
         t3A(c,a,b,i,k,j)=-t3A(c,b,a,k,j,i)
         t3A(c,a,b,j,k,i)= t3A(c,b,a,k,j,i)
         t3A(c,a,b,j,i,k)=-t3A(c,b,a,k,j,i)
         t3A(a,b,c,k,j,i)=-t3A(c,b,a,k,j,i)
         t3A(a,b,c,k,i,j)= t3A(c,b,a,k,j,i)
         t3A(a,b,c,i,j,k)= t3A(c,b,a,k,j,i)
         t3A(a,b,c,i,k,j)=-t3A(c,b,a,k,j,i)
         t3A(a,b,c,j,k,i)= t3A(c,b,a,k,j,i)
         t3A(a,b,c,j,i,k)=-t3A(c,b,a,k,j,i)
         t3A(a,c,b,k,j,i)= t3A(c,b,a,k,j,i)
         t3A(a,c,b,k,i,j)=-t3A(c,b,a,k,j,i)
         t3A(a,c,b,i,j,k)=-t3A(c,b,a,k,j,i)
         t3A(a,c,b,i,k,j)= t3A(c,b,a,k,j,i)
         t3A(a,c,b,j,k,i)=-t3A(c,b,a,k,j,i)
         t3A(a,c,b,j,i,k)= t3A(c,b,a,k,j,i)
         t3A(b,c,a,k,j,i)=-t3A(c,b,a,k,j,i)
         t3A(b,c,a,k,i,j)= t3A(c,b,a,k,j,i)
         t3A(b,c,a,i,j,k)= t3A(c,b,a,k,j,i)
         t3A(b,c,a,i,k,j)=-t3A(c,b,a,k,j,i)
         t3A(b,c,a,j,k,i)= t3A(c,b,a,k,j,i)
         t3A(b,c,a,j,i,k)=-t3A(c,b,a,k,j,i)
         t3A(b,a,c,k,j,i)= t3A(c,b,a,k,j,i)
         t3A(b,a,c,k,i,j)=-t3A(c,b,a,k,j,i)
         t3A(b,a,c,i,j,k)=-t3A(c,b,a,k,j,i)
         t3A(b,a,c,i,k,j)= t3A(c,b,a,k,j,i)
         t3A(b,a,c,j,k,i)=-t3A(c,b,a,k,j,i)
         t3A(b,a,c,j,i,k)= t3A(c,b,a,k,j,i)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end