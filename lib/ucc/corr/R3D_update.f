       subroutine R3D_update(N0,N1,N2,N3,V3D,
     & K1,K2,K3,K4,
     & FockR,FockB,IntR,IntB,IntM,
     & H1A,H1B,H2A,H2B,H2C,
     & t1A,t1B,t2A,t2B,t2C,t3A,t3B,t3C,t3D,
     & r1A,r1B,r2A,r2B,r2C,r3A,r3B,r3C,r3D)
C
       integer a,b,c,e,f,g,h,i,j,k,m,n,o,p
       real*8 FockR(N3,N3)
       real*8 FockB(N3,N3)
       real*8 IntR(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 IntB(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 IntM(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 H1A(N0+1:N3,N0+1:N3)
       real*8 H1B(N0+1:N3,N0+1:N3)
       real*8 H2A(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 H2B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 H2C(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 t1A(N1+1:N3,N0+1:N1)
       real*8 t1B(N2+1:N3,N0+1:N2)
       real*8 t2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 t2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 t2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
       real*8 t3A(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)
       real*8 t3B(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1)
       real*8 t3C(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1)
       real*8 t3D(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2)
       real*8 r1A(N1+1:N3,N0+1:N1)
       real*8 r1B(N2+1:N3,N0+1:N2)
       real*8 r2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 r2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 r2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
       real*8 r3A(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)
       real*8 r3B(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1)
       real*8 r3C(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1)
       real*8 r3D(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2)
C
       real*8 V3D(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2)
C
       real*8,allocatable::B1(:,:)
       real*8,allocatable::B2(:,:)
       real*8,allocatable::C1(:,:,:)
       real*8,allocatable::C2(:,:,:)
       real*8,allocatable::D1(:,:,:,:)
       real*8,allocatable::D2(:,:,:,:)
       real*8,allocatable::F2(:,:,:,:,:,:)
C
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::Q3(:,:)
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::S1(:,:,:,:)
       real*8,allocatable::Q4(:,:)
       real*8,allocatable::Q6(:,:)
       real*8,allocatable::S2(:,:,:,:)
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::Q12(:,:)
       real*8,allocatable::Q8(:,:)
       real*8,allocatable::S5(:,:,:,:)
       real*8,allocatable::S31(:,:,:,:)
       real*8,allocatable::S6(:,:,:,:)
       real*8,allocatable::S8(:,:,:,:)
       real*8,allocatable::S9(:,:,:,:)
       real*8,allocatable::S11(:,:,:,:)
       real*8,allocatable::S12(:,:,:,:)
       real*8,allocatable::S10(:,:,:,:)
       real*8,allocatable::S13(:,:,:,:)
       real*8,allocatable::S14(:,:,:,:)
       real*8,allocatable::S24(:,:,:,:)
       real*8,allocatable::S40(:,:,:,:)
       real*8,allocatable::Q16(:,:)
       real*8,allocatable::S57(:,:,:,:)
       real*8,allocatable::S62(:,:,:,:)
       real*8,allocatable::S64(:,:,:,:)
       real*8,allocatable::S15(:,:,:,:)
       real*8,allocatable::S22(:,:,:,:)
       real*8,allocatable::S23(:,:,:,:)
       real*8,allocatable::S26(:,:,:,:)
       real*8,allocatable::S27(:,:,:,:)
       real*8,allocatable::S45(:,:,:,:)
       real*8,allocatable::S46(:,:,:,:)
       real*8,allocatable::S3(:,:,:,:)
       real*8,allocatable::S53(:,:,:,:)
       real*8,allocatable::S4(:,:,:,:)
       real*8,allocatable::S7(:,:,:,:)
       real*8,allocatable::S32(:,:,:,:)
       real*8,allocatable::S33(:,:,:,:)
       real*8,allocatable::S34(:,:,:,:)
       real*8,allocatable::S36(:,:,:,:)
       real*8,allocatable::S37(:,:,:,:)
       real*8,allocatable::S35(:,:,:,:)
       real*8,allocatable::S38(:,:,:,:)
       real*8,allocatable::S39(:,:,:,:)
       real*8,allocatable::Q9(:,:)
       real*8,allocatable::Q13(:,:)
       real*8,allocatable::Q10(:,:)
       real*8,allocatable::S16(:,:,:,:)
       real*8,allocatable::Q17(:,:)
       real*8,allocatable::S17(:,:,:,:)
       real*8,allocatable::S63(:,:,:,:)
       real*8,allocatable::S65(:,:,:,:)
       real*8,allocatable::S68(:,:,:,:)
       real*8,allocatable::S41(:,:,:,:)
       real*8,allocatable::S47(:,:,:,:)
       real*8,allocatable::Q15(:,:)
       real*8,allocatable::S20(:,:,:,:)
       real*8,allocatable::S25(:,:,:,:)
       real*8,allocatable::S21(:,:,:,:)
       real*8,allocatable::Q18(:,:)
       real*8,allocatable::S48(:,:,:,:)
       real*8,allocatable::S49(:,:,:,:)
       real*8,allocatable::S50(:,:,:,:)
       real*8,allocatable::S51(:,:,:,:)
       real*8,allocatable::S52(:,:,:,:)
       real*8,allocatable::S54(:,:,:,:)
       real*8,allocatable::S29(:,:,:,:)
       real*8,allocatable::S30(:,:,:,:)
       real*8,allocatable::Q19(:,:)
       real*8,allocatable::Q20(:,:)
       real*8,allocatable::S55(:,:,:,:)
       real*8,allocatable::S56(:,:,:,:)
       real*8,allocatable::S18(:,:,:,:)
       real*8,allocatable::S44(:,:,:,:)
       real*8,allocatable::S19(:,:,:,:)
       real*8,allocatable::S59(:,:,:,:)
       real*8,allocatable::S60(:,:,:,:)
       real*8,allocatable::S61(:,:,:,:)
       real*8,allocatable::Q11(:,:)
       real*8,allocatable::S58(:,:,:,:)
       real*8,allocatable::S28(:,:,:,:)
       real*8,allocatable::S42(:,:,:,:)
       real*8,allocatable::S43(:,:,:,:)
       real*8,allocatable::S66(:,:,:,:)
       real*8,allocatable::S67(:,:,:,:)
       real*8,allocatable::Q14(:,:)
       real*8,allocatable::Q21(:,:)
       real*8,allocatable::Q22(:,:)
       real*8,allocatable::S69(:,:,:,:)
       real*8,allocatable::S70(:,:,:,:)
       real*8,allocatable::S71(:,:,:,:)
       real*8,allocatable::S72(:,:,:,:)
       real*8,allocatable::X1(:,:)
       real*8,allocatable::U1(:,:,:,:,:,:)
       real*8,allocatable::X2(:,:)
       real*8,allocatable::U2(:,:,:,:,:,:)
       real*8,allocatable::X3(:,:,:,:)
       real*8,allocatable::U5(:,:,:,:,:,:)
       real*8,allocatable::X4(:,:)
       real*8,allocatable::U6(:,:,:,:,:,:)
       real*8,allocatable::X5(:,:,:,:)
       real*8,allocatable::U7(:,:,:,:,:,:)
       real*8,allocatable::X6(:,:)
       real*8,allocatable::U31(:,:,:,:,:,:)
       real*8,allocatable::X7(:,:,:,:)
       real*8,allocatable::U10(:,:,:,:,:,:)
       real*8,allocatable::X8(:,:,:,:)
       real*8,allocatable::U33(:,:,:,:,:,:)
       real*8,allocatable::U13(:,:,:,:,:,:)
       real*8,allocatable::X9(:,:,:,:)
       real*8,allocatable::U15(:,:,:,:,:,:)
       real*8,allocatable::X10(:,:,:,:)
       real*8,allocatable::U14(:,:,:,:,:,:)
       real*8,allocatable::U18(:,:,:,:,:,:)
       real*8,allocatable::X11(:,:,:,:)
       real*8,allocatable::U23(:,:,:,:,:,:)
       real*8,allocatable::U24(:,:,:,:,:,:)
       real*8,allocatable::U25(:,:,:,:,:,:)
       real*8,allocatable::X12(:,:,:,:)
       real*8,allocatable::U27(:,:,:,:,:,:)
       real*8,allocatable::X13(:,:,:,:)
       real*8,allocatable::U28(:,:,:,:,:,:)
       real*8,allocatable::X14(:,:,:,:)
       real*8,allocatable::U42(:,:,:,:,:,:)
       real*8,allocatable::X15(:,:,:,:)
       real*8,allocatable::U47(:,:,:,:,:,:)
       real*8,allocatable::X16(:,:,:,:)
       real*8,allocatable::U48(:,:,:,:,:,:)
       real*8,allocatable::U11(:,:,:,:,:,:)
       real*8,allocatable::X17(:,:,:,:)
       real*8,allocatable::U35(:,:,:,:,:,:)
       real*8,allocatable::X18(:,:,:,:)
       real*8,allocatable::U37(:,:,:,:,:,:)
       real*8,allocatable::X19(:,:,:,:)
       real*8,allocatable::U41(:,:,:,:,:,:)
       real*8,allocatable::U73(:,:,:,:,:,:)
       real*8,allocatable::U75(:,:,:,:,:,:)
       real*8,allocatable::X20(:,:,:,:)
       real*8,allocatable::U78(:,:,:,:,:,:)
       real*8,allocatable::X21(:,:,:,:)
       real*8,allocatable::U55(:,:,:,:,:,:)
       real*8,allocatable::X22(:,:,:,:)
       real*8,allocatable::U58(:,:,:,:,:,:)
       real*8,allocatable::X23(:,:,:,:)
       real*8,allocatable::U32(:,:,:,:,:,:)
       real*8,allocatable::U63(:,:,:,:,:,:)
       real*8,allocatable::U64(:,:,:,:,:,:)
       real*8,allocatable::U66(:,:,:,:,:,:)
       real*8,allocatable::X24(:,:,:,:)
       real*8,allocatable::U45(:,:,:,:,:,:)
       real*8,allocatable::U77(:,:,:,:,:,:)
       real*8,allocatable::U81(:,:,:,:,:,:)
       real*8,allocatable::X25(:,:,:,:)
       real*8,allocatable::U83(:,:,:,:,:,:)
       real*8,allocatable::U84(:,:,:,:,:,:)
       real*8,allocatable::U85(:,:,:,:,:,:)
       real*8,allocatable::U86(:,:,:,:,:,:)
       real*8,allocatable::U87(:,:,:,:,:,:)
       real*8,allocatable::U88(:,:,:,:,:,:)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N0,N2,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(Q1(N0+1:N2,N0+1:N2))
       I1=K2*K2
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(N0+1:N2,N0+1:N2))
       X1=0.0d0
       X1=X1+Q1
       deallocate(Q1)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N2,N3,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(Q2(N2+1:N3,N2+1:N3))
       I1=K4*K4
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q2)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X2(N2+1:N3,N2+1:N3))
       X2=0.0d0
       X2=X2+Q2
       deallocate(Q2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N2,N3,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(Q3(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q3)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q5(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,Q3,B2,Q5)
       deallocate(B2)
C
       call sul21(N2,N3,N2,N3,X2,Q5,-1.0d0)
       deallocate(Q5)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call relef21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q3,B1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2C,D2)
       allocate(S1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K2*K4
       I3=K4
       call jungemm(I1,I2,I3,B1,D2,S1)
       deallocate(B1)
       deallocate(D2)
C
       allocate(X3(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X3=0.0d0
       call sul2341(N0,N2,N2,N3,N0,N2,N0,N2,X3,S1, 1.0d0)
       deallocate(S1)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call relef21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q3,B1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(Q4(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,B1,B2,Q4)
       deallocate(B1)
       deallocate(B2)
C
       call sul21(N0,N2,N0,N2,X1,Q4, 1.0d0)
       deallocate(Q4)
C
       allocate(B1(N0+1:N2,N2+1:N3))
       call relef21(0,N3,0,N3,
     & N0,N2,N2,N3,FockB,B1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(Q6(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,B1,B2,Q6)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X4(N2+1:N3,N2+1:N3))
       X4=0.0d0
       call sul21(N2,N3,N2,N3,X4,Q6, 1.0d0)
       deallocate(Q6)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N0,N2,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S2(N2+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S2)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X5(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       X5=0.0d0
       call sul3124(N0,N1,N1,N3,N2,N3,N0,N2,X5,S2, 1.0d0)
       deallocate(S2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N2,N3,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q7(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q7)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call relef21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q7,B1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(Q12(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,B1,B2,Q12)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X6(N0+1:N2,N0+1:N2))
       X6=0.0d0
       call sul21(N0,N2,N0,N2,X6,Q12, 1.0d0)
       deallocate(Q12)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(Q8(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,Q7,B2,Q8)
       deallocate(B2)
C
       call sul21(N2,N3,N2,N3,X4,Q8, 1.0d0)
       deallocate(Q8)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N2,N3,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,t2B,D2)
       allocate(S5(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S5)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef4312(N2,N3,N0,N2,N0,N2,N2,N3,
     & N2,N3,N0,N2,N2,N3,N0,N2,S5,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S31(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S31)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X8(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X8=0.0d0
       call sul3124(N0,N2,N2,N3,N0,N2,N0,N2,X8,S31, 1.0d0)
       deallocate(S31)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3412(N2,N3,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,S5,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S6(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S6)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X7(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       X7=0.0d0
       call sul3124(N2,N3,N2,N3,N2,N3,N0,N2,X7,S6, 1.0d0)
       deallocate(S6)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S8(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S8)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N2,N3,N2,N3,N2,N3,N0,N2,X7,S8, 1.0d0)
       deallocate(S8)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S9(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S9)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef2314(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,S9,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef412356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,t3D,F2)
       allocate(U13(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K2*K4*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,F2,U13)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul124536(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U13,-1.0d0)
       call
     & sul124635(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U13, 1.0d0)
       call
     & sul125634(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U13,-1.0d0)
       call
     & sul134526(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U13, 1.0d0)
       call
     & sul134625(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U13,-1.0d0)
       call
     & sul135624(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U13, 1.0d0)
       call
     & sul234516(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U13,-1.0d0)
       call
     & sul234615(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U13, 1.0d0)
       call
     & sul235614(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U13,-1.0d0)
       deallocate(U13)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef3214(N2,N3,N0,N2,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,N3,N0,N2,S9,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S11(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S11)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X9(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X9=0.0d0
       call sul3124(N0,N2,N2,N3,N0,N2,N0,N2,X9,S11, 1.0d0)
       deallocate(S11)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef2314(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,S9,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S12(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S12)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N2,N3,N2,N3,N2,N3,N0,N2,X7,S12, 1.0d0)
       deallocate(S12)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S10(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       I1=K4*K4*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S10)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X10(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       X10=0.0d0
       call sul4123(N2,N3,N2,N3,N2,N3,N2,N3,X10,S10, 1.0d0)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef2341(N2,N3,N2,N3,N2,N3,N2,N3,
     & N2,N3,N2,N3,N2,N3,N2,N3,S10,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S13(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       I1=K4*K4*K4
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S13)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N2,N3,N2,N3,N2,N3,N0,N2,X7,S13,-1.0d0)
       deallocate(S13)
C
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2C,D2)
       allocate(U10(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2*K2*K4
       I3=K4
       call jungemm(I1,I2,I3,X7,D2,U10)
       deallocate(D2)
C
       call
     & sul245136(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U10, 1.0d0)
       call
     & sul145236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U10,-1.0d0)
       call
     & sul246135(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U10,-1.0d0)
       call
     & sul146235(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U10, 1.0d0)
       call
     & sul256134(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U10, 1.0d0)
       call
     & sul156234(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U10,-1.0d0)
       call
     & sul345126(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U10,-1.0d0)
       call
     & sul145326(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U10, 1.0d0)
       call
     & sul346125(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U10, 1.0d0)
       call
     & sul146325(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U10,-1.0d0)
       call
     & sul356124(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U10,-1.0d0)
       call
     & sul156324(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U10, 1.0d0)
       call
     & sul345216(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U10, 1.0d0)
       call
     & sul245316(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U10,-1.0d0)
       call
     & sul346215(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U10,-1.0d0)
       call
     & sul246315(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U10, 1.0d0)
       call
     & sul356214(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U10, 1.0d0)
       call
     & sul256314(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U10,-1.0d0)
       deallocate(U10)
       deallocate(X7)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N0,N2,N2,N3,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S14(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S14)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,S14,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S24(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S24)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef2314(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,S24,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef412356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,t3D,F2)
       allocate(U25(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K2*K4*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,F2,U25)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul134526(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U25, 1.0d0)
       call
     & sul134625(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U25,-1.0d0)
       call
     & sul135624(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U25, 1.0d0)
       call
     & sul234516(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U25,-1.0d0)
       call
     & sul234615(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U25, 1.0d0)
       call
     & sul235614(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U25,-1.0d0)
       deallocate(U25)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef4231(N0,N2,N0,N2,N0,N2,N2,N3,
     & N2,N3,N0,N2,N0,N2,N0,N2,S14,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S40(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S40)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X14(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       X14=0.0d0
       call sul3124(N0,N2,N0,N2,N0,N2,N0,N2,X14,S40, 1.0d0)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef2431(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,S14,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(Q16(N0+1:N2,N0+1:N2))
       I1=K2*K2
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q16)
       deallocate(D1)
       deallocate(B2)
C
       X1=X1-Q16
       deallocate(Q16)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,S14,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,r2C,D2)
       allocate(S57(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K4*K4
       I3=K2*K2
       call jungemm(I1,I2,I3,D1,D2,S57)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X13(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       X13=0.0d0
       call sul2314(N2,N3,N2,N3,N2,N3,N0,N2,X13,S57, 0.5d0)
       deallocate(S57)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef3421(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,S14,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,r2C,D2)
       allocate(S62(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S62)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X16(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X16=0.0d0
       call sul2314(N0,N2,N2,N3,N0,N2,N0,N2,X16,S62,-1.0d0)
       deallocate(S62)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef2431(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,S14,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,r2C,D2)
       allocate(S64(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S64)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X15(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X15=0.0d0
       call sul2314(N0,N2,N2,N3,N0,N2,N0,N2,X15,S64,-1.0d0)
       deallocate(S64)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef3241(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,S14,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S15(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S15)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef2314(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,S15,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef412356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,t3D,F2)
       allocate(U18(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K2*K4*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,F2,U18)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul124536(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U18, 1.0d0)
       call
     & sul124635(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U18,-1.0d0)
       call
     & sul125634(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U18, 1.0d0)
       deallocate(U18)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef3214(N2,N3,N0,N2,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,N3,N0,N2,S15,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S22(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S22)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X11(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X11=0.0d0
       call sul3124(N0,N2,N2,N3,N0,N2,N0,N2,X11,S22, 1.0d0)
       deallocate(S22)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef2314(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,S15,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S23(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S23)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef2134(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N2,N3,N2,N3,N0,N2,S23,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2C,D2)
       allocate(U24(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2*K2*K4
       I3=K4
       call jungemm(I1,I2,I3,D1,D2,U24)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul245136(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U24,-1.0d0)
       call
     & sul145236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U24, 1.0d0)
       call
     & sul246135(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U24, 1.0d0)
       call
     & sul146235(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U24,-1.0d0)
       call
     & sul256134(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U24,-1.0d0)
       call
     & sul156234(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U24, 1.0d0)
       call
     & sul345126(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U24, 1.0d0)
       call
     & sul346125(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U24,-1.0d0)
       call
     & sul356124(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U24, 1.0d0)
       deallocate(U24)
       deallocate(S23)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef3214(N2,N3,N0,N2,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,N3,N0,N2,S24,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S26(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S26)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X12(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X12=0.0d0
       call sul3124(N0,N2,N2,N3,N0,N2,N0,N2,X12,S26, 1.0d0)
       deallocate(S26)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef2314(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,S24,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S27(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S27)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N2,N3,N2,N3,N2,N3,N0,N2,X13,S27, 1.0d0)
       deallocate(S27)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef2314(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S40,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S45(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S45)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N2,N2,N3,N0,N2,N0,N2,X15,S45, 1.0d0)
       deallocate(S45)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef3214(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S40,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S46(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S46)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N2,N2,N3,N0,N2,N0,N2,X16,S46, 1.0d0)
       deallocate(S46)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N0,N1,N1,N3,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S3(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S3)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call relef3421(N0,N2,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N2,N0,N2,S3,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,r2B,D2)
       allocate(S53(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S53)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N0,N2,N2,N3,N0,N2,N0,N2,X9,S53, 1.0d0)
       deallocate(S53)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N2,N0,N1,N1,N3,
     & N0,N2,N0,N1,N1,N3,N0,N2,S3,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S4(N2+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S4)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N1,N1,N3,N2,N3,N0,N2,X5,S4, 1.0d0)
       deallocate(S4)
C
       allocate(F2(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef631245(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N2,N3,N0,N2,N0,N2,t3C,F2)
       allocate(U7(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K2*K4*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,X5,F2,U7)
       deallocate(F2)
C
       call
     & sul124536(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U7,-1.0d0)
       call
     & sul124635(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U7, 1.0d0)
       call
     & sul125634(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U7,-1.0d0)
       call
     & sul134526(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U7, 1.0d0)
       call
     & sul134625(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U7,-1.0d0)
       call
     & sul135624(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U7, 1.0d0)
       call
     & sul234516(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U7,-1.0d0)
       call
     & sul234615(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U7, 1.0d0)
       call
     & sul235614(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U7,-1.0d0)
       deallocate(U7)
       deallocate(X5)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N0,N2,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S7(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S7)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef2134(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N0,N2,S7,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(U11(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K2
       call jungemm(I1,I2,I3,D1,D2,U11)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul124356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U11,-1.0d0)
       call
     & sul125346(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U11, 1.0d0)
       call
     & sul126345(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U11,-1.0d0)
       call
     & sul134256(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U11, 1.0d0)
       call
     & sul135246(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U11,-1.0d0)
       call
     & sul136245(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U11, 1.0d0)
       call
     & sul234156(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U11,-1.0d0)
       call
     & sul235146(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U11, 1.0d0)
       call
     & sul236145(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U11,-1.0d0)
       deallocate(U11)
       deallocate(S7)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S32(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S32)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N2,N3,N0,N2,N0,N2,X8,S32, 1.0d0)
       deallocate(S32)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N2,N3,N2,N3,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S33(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       I1=K4*K4*K4
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S33)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X17(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       X17=0.0d0
       call sul4123(N2,N3,N2,N3,N2,N3,N0,N2,X17,S33, 1.0d0)
       deallocate(S33)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N0,N2,N0,N2,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S34(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S34)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N0,N2,N0,N2,N0,N2,X14,S34,-1.0d0)
C
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,t3D,F2)
       allocate(U42(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4*K4*K4
       I3=K2*K2
       call jungemm(I1,I2,I3,X14,F2,U42)
       deallocate(F2)
C
       call
     & sul123546(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U42, 0.5d0)
       call
     & sul123645(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U42,-0.5d0)
       V3D=V3D-0.5d0*U42
       call
     & sul123654(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U42, 0.5d0)
       call
     & sul123465(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U42, 0.5d0)
       call
     & sul123564(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U42,-0.5d0)
       deallocate(U42)
       deallocate(X14)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef2314(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S34,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S36(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S36)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N2,N2,N3,N0,N2,N0,N2,X15,S36,-1.0d0)
       deallocate(S36)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(U47(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K2
       call jungemm(I1,I2,I3,X15,D2,U47)
       deallocate(D2)
C
       call
     & sul235146(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U47, 1.0d0)
       call
     & sul135246(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U47,-1.0d0)
       call
     & sul236145(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U47,-1.0d0)
       call
     & sul136245(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U47, 1.0d0)
       call
     & sul234156(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U47,-1.0d0)
       call
     & sul134256(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U47, 1.0d0)
       call
     & sul236154(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U47, 1.0d0)
       call
     & sul136254(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U47,-1.0d0)
       call
     & sul234165(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U47, 1.0d0)
       call
     & sul134265(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U47,-1.0d0)
       call
     & sul235164(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U47,-1.0d0)
       call
     & sul135264(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U47, 1.0d0)
       deallocate(U47)
       deallocate(X15)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef3214(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S34,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S37(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S37)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N2,N2,N3,N0,N2,N0,N2,X16,S37,-1.0d0)
       deallocate(S37)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(U48(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K2
       call jungemm(I1,I2,I3,X16,D2,U48)
       deallocate(D2)
C
       call
     & sul125346(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U48,-1.0d0)
       call
     & sul126345(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U48, 1.0d0)
       call
     & sul124356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U48, 1.0d0)
       call
     & sul126354(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U48,-1.0d0)
       call
     & sul124365(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U48,-1.0d0)
       call
     & sul125364(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U48, 1.0d0)
       deallocate(U48)
       deallocate(X16)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S35(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S35)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X18(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       X18=0.0d0
       call sul4123(N0,N2,N2,N3,N2,N3,N0,N2,X18,S35, 1.0d0)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef3241(N0,N2,N0,N2,N2,N3,N2,N3,
     & N2,N3,N0,N2,N2,N3,N0,N2,S35,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S38(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S38)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N2,N2,N3,N0,N2,N0,N2,X8,S38, 1.0d0)
       deallocate(S38)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(U33(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K2
       call jungemm(I1,I2,I3,X8,D2,U33)
       deallocate(D2)
C
       call
     & sul235146(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U33,-1.0d0)
       call
     & sul135246(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U33, 1.0d0)
       call
     & sul125346(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U33,-1.0d0)
       call
     & sul236145(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U33, 1.0d0)
       call
     & sul136245(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U33,-1.0d0)
       call
     & sul126345(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U33, 1.0d0)
       call
     & sul234156(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U33, 1.0d0)
       call
     & sul134256(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U33,-1.0d0)
       call
     & sul124356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U33, 1.0d0)
       call
     & sul236154(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U33,-1.0d0)
       call
     & sul136254(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U33, 1.0d0)
       call
     & sul126354(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U33,-1.0d0)
       call
     & sul234165(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U33,-1.0d0)
       call
     & sul134265(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U33, 1.0d0)
       call
     & sul124365(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U33,-1.0d0)
       call
     & sul235164(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U33, 1.0d0)
       call
     & sul135264(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U33,-1.0d0)
       call
     & sul125364(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U33, 1.0d0)
       deallocate(U33)
       deallocate(X8)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,S35,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S39(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S39)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X19(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       X19=0.0d0
       call sul3124(N2,N3,N2,N3,N2,N3,N0,N2,X19,S39, 1.0d0)
       deallocate(S39)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q9(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q9)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call relef21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q9,B1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(Q13(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,B1,B2,Q13)
       deallocate(B1)
       deallocate(B2)
C
       call sul21(N0,N2,N0,N2,X6,Q13, 1.0d0)
       deallocate(Q13)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(Q10(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,Q9,B2,Q10)
       deallocate(B2)
C
       call sul21(N2,N3,N2,N3,X4,Q10, 1.0d0)
       deallocate(Q10)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S16(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S16)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef2341(N2,N3,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,S16,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(Q17(N2+1:N3,N2+1:N3))
       I1=K4*K4
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q17)
       deallocate(D1)
       deallocate(B2)
C
       X2=X2+Q17
       deallocate(Q17)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef2341(N2,N3,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,S16,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S17(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       I1=K4*K4*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S17)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N2,N3,N2,N3,N2,N3,N2,N3,X10,S17, 1.0d0)
       deallocate(S17)
C
       allocate(F2(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef123456(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,t3D,F2)
       allocate(U14(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K2*K2*K2*K4
       I3=K4*K4
       call jungemm(I1,I2,I3,X10,F2,U14)
       deallocate(F2)
C
       call
     & sul245613(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U14,-0.5d0)
       call
     & sul145623(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U14, 0.5d0)
       call
     & sul345612(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U14, 0.5d0)
       call
     & sul145632(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U14,-0.5d0)
       call
     & sul345621(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U14,-0.5d0)
       call
     & sul245631(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U14, 0.5d0)
       deallocate(U14)
       deallocate(X10)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef2431(N2,N3,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,S16,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,r2C,D2)
       allocate(S63(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S63)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3412(N2,N3,N0,N2,N2,N3,N2,N3,
     & N2,N3,N2,N3,N2,N3,N0,N2,S63,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2C,D2)
       allocate(U73(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2*K2*K4
       I3=K4
       call jungemm(I1,I2,I3,D1,D2,U73)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul256134(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U73,-1.0d0)
       call
     & sul156234(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U73, 1.0d0)
       call
     & sul156324(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U73,-1.0d0)
       call
     & sul246135(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U73, 1.0d0)
       call
     & sul146235(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U73,-1.0d0)
       call
     & sul146325(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U73, 1.0d0)
       call
     & sul245136(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U73,-1.0d0)
       call
     & sul145236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U73, 1.0d0)
       call
     & sul145326(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U73,-1.0d0)
       deallocate(U73)
       deallocate(S63)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef2341(N2,N3,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,S16,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,r2C,D2)
       allocate(S65(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S65)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3412(N2,N3,N0,N2,N2,N3,N2,N3,
     & N2,N3,N2,N3,N2,N3,N0,N2,S65,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2C,D2)
       allocate(U75(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2*K2*K4
       I3=K4
       call jungemm(I1,I2,I3,D1,D2,U75)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul356124(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U75,-1.0d0)
       call
     & sul356214(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U75, 1.0d0)
       call
     & sul256314(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U75,-1.0d0)
       call
     & sul346125(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U75, 1.0d0)
       call
     & sul346215(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U75,-1.0d0)
       call
     & sul246315(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U75, 1.0d0)
       call
     & sul345126(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U75,-1.0d0)
       call
     & sul345216(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U75, 1.0d0)
       call
     & sul245316(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U75,-1.0d0)
       deallocate(U75)
       deallocate(S65)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef3421(N2,N3,N0,N2,N2,N3,N2,N3,
     & N2,N3,N2,N3,N0,N2,N2,N3,S16,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,r2C,D2)
       allocate(S68(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K2
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,D2,S68)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X20(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X20=0.0d0
       call sul3412(N0,N2,N2,N3,N0,N2,N0,N2,X20,S68, 1.0d0)
       deallocate(S68)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3241(N2,N3,N0,N2,N2,N3,N2,N3,
     & N2,N3,N0,N2,N2,N3,N2,N3,S16,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S41(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S41)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N2,N2,N3,N2,N3,N0,N2,X18,S41, 1.0d0)
C
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef412356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,t3D,F2)
       allocate(U37(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K2*K4*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,X18,F2,U37)
       deallocate(F2)
C
       call
     & sul235614(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U37,-1.0d0)
       call
     & sul135624(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U37, 1.0d0)
       call
     & sul125634(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U37,-1.0d0)
       call
     & sul234615(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U37, 1.0d0)
       call
     & sul134625(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U37,-1.0d0)
       call
     & sul124635(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U37, 1.0d0)
       call
     & sul234516(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U37,-1.0d0)
       call
     & sul134526(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U37, 1.0d0)
       call
     & sul124536(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U37,-1.0d0)
       deallocate(U37)
       deallocate(X18)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,S41,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S47(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S47)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N2,N3,N2,N3,N2,N3,N0,N2,X17,S47,-1.0d0)
       deallocate(S47)
C
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2C,D2)
       allocate(U35(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2*K2*K4
       I3=K4
       call jungemm(I1,I2,I3,X17,D2,U35)
       deallocate(D2)
C
       call
     & sul356124(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U35, 1.0d0)
       call
     & sul256134(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U35,-1.0d0)
       call
     & sul156234(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U35, 1.0d0)
       call
     & sul346125(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U35,-1.0d0)
       call
     & sul246135(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U35, 1.0d0)
       call
     & sul146235(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U35,-1.0d0)
       call
     & sul345126(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U35, 1.0d0)
       call
     & sul245136(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U35,-1.0d0)
       call
     & sul145236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U35, 1.0d0)
       deallocate(U35)
       deallocate(X17)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(Q15(N2+1:N3,N2+1:N3))
       I1=K4*K4
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q15)
       deallocate(D1)
       deallocate(B2)
C
       X2=X2+Q15
       deallocate(Q15)
C
       allocate(F2(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef123456(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,t3D,F2)
       allocate(U2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2*K2*K2*K4*K4
       I3=K4
       call jungemm(I1,I2,I3,X2,F2,U2)
       deallocate(F2)
C
       call
     & sul234561(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U2, 1.0d0)
       call
     & sul134562(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U2,-1.0d0)
       call
     & sul124563(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U2, 1.0d0)
       deallocate(U2)
       deallocate(X2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2C,D2)
       allocate(S20(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K2
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,D2,S20)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S20,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S25(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S25)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N2,N2,N3,N0,N2,N0,N2,X12,S25,-0.5d0)
       deallocate(S25)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(U27(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K2
       call jungemm(I1,I2,I3,X12,D2,U27)
       deallocate(D2)
C
       call
     & sul134256(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U27, 1.0d0)
       call
     & sul135246(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U27,-1.0d0)
       call
     & sul136245(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U27, 1.0d0)
       call
     & sul234156(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U27,-1.0d0)
       call
     & sul235146(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U27, 1.0d0)
       call
     & sul236145(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U27,-1.0d0)
       deallocate(U27)
       deallocate(X12)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef4312(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S20,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S21(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S21)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N2,N2,N3,N0,N2,N0,N2,X11,S21,-0.5d0)
       deallocate(S21)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(U23(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K2
       call jungemm(I1,I2,I3,X11,D2,U23)
       deallocate(D2)
C
       call
     & sul124356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U23, 1.0d0)
       call
     & sul125346(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U23,-1.0d0)
       call
     & sul126345(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U23, 1.0d0)
       deallocate(U23)
       deallocate(X11)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(Q18(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q18)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call relef21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q18,B1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2C,D2)
       allocate(S48(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K2*K4
       I3=K4
       call jungemm(I1,I2,I3,B1,D2,S48)
       deallocate(B1)
       deallocate(D2)
C
       call sul2341(N0,N2,N2,N3,N0,N2,N0,N2,X3,S48, 1.0d0)
       deallocate(S48)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(U5(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K2
       call jungemm(I1,I2,I3,X3,D2,U5)
       deallocate(D2)
C
       call
     & sul234156(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5, 1.0d0)
       call
     & sul134256(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5,-1.0d0)
       call
     & sul124356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5, 1.0d0)
       call
     & sul235146(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5,-1.0d0)
       call
     & sul135246(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5, 1.0d0)
       call
     & sul125346(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5,-1.0d0)
       call
     & sul126345(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5, 1.0d0)
       call
     & sul136245(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5,-1.0d0)
       call
     & sul236145(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5, 1.0d0)
       deallocate(U5)
       deallocate(X3)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,r2B,D2)
       allocate(S49(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S49)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X21(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       X21=0.0d0
       call sul3412(N0,N1,N1,N3,N2,N3,N0,N2,X21,S49, 1.0d0)
       deallocate(S49)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N0,N2,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,r2B,D2)
       allocate(S50(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S50)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N0,N2,N2,N3,N0,N2,N0,N2,X9,S50, 1.0d0)
       deallocate(S50)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,r2B,D2)
       allocate(S51(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S51)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N2,N3,N2,N3,N2,N3,N0,N2,X19,S51,-1.0d0)
       deallocate(S51)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N2,N3,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,r2B,D2)
       allocate(S52(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S52)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X22(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       X22=0.0d0
       call sul3412(N0,N2,N2,N3,N2,N3,N0,N2,X22,S52, 1.0d0)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3412(N2,N3,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,S52,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S54(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S54)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N2,N3,N2,N3,N2,N3,N0,N2,X19,S54, 1.0d0)
       deallocate(S54)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N0,N1,N1,N3,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S29(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S29)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N2,N0,N1,N1,N3,
     & N0,N2,N0,N1,N1,N3,N0,N2,S29,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S30(N2+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S30)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X23(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       X23=0.0d0
       call sul3124(N0,N1,N1,N3,N2,N3,N0,N2,X23,S30, 1.0d0)
       deallocate(S30)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,r2B,D2)
       allocate(Q19(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,D2,Q19)
       deallocate(D1)
       deallocate(D2)
C
       call sul21(N2,N3,N2,N3,X4,Q19, 1.0d0)
       deallocate(Q19)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N0,N2,IntM,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,r2B,D2)
       allocate(Q20(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,D1,D2,Q20)
       deallocate(D1)
       deallocate(D2)
C
       call sul21(N0,N2,N0,N2,X6,Q20, 1.0d0)
       deallocate(Q20)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,r2C,D2)
       allocate(S55(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K4*K4
       I3=K2*K2
       call jungemm(I1,I2,I3,D1,D2,S55)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N2,N3,N2,N3,N2,N3,N0,N2,X13,S55,-0.5d0)
       deallocate(S55)
C
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2C,D2)
       allocate(U28(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2*K2*K4
       I3=K4
       call jungemm(I1,I2,I3,X13,D2,U28)
       deallocate(D2)
C
       call
     & sul145236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U28, 1.0d0)
       call
     & sul146235(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U28,-1.0d0)
       call
     & sul156234(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U28, 1.0d0)
       call
     & sul345126(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U28, 1.0d0)
       call
     & sul245136(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U28,-1.0d0)
       call
     & sul346125(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U28,-1.0d0)
       call
     & sul246135(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U28, 1.0d0)
       call
     & sul356124(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U28, 1.0d0)
       call
     & sul256134(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U28,-1.0d0)
       deallocate(U28)
       deallocate(X13)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,r2C,D2)
       allocate(S56(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K4*K4
       I3=K2*K2
       call jungemm(I1,I2,I3,D1,D2,S56)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N2,N3,N2,N3,
     & N2,N3,N2,N3,N2,N3,N2,N3,S56,D1)
       allocate(F2(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef123456(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,t3D,F2)
       allocate(U66(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K2*K2*K2*K4
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,F2,U66)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul145623(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U66, 0.250)
       call
     & sul245613(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U66,-0.250)
       call
     & sul345612(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U66, 0.250)
       deallocate(U66)
       deallocate(S56)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S18(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S18)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef4312(N2,N3,N0,N2,N0,N2,N2,N3,
     & N2,N3,N0,N2,N2,N3,N0,N2,S18,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S44(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S44)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N2,N3,N0,N2,N0,N2,X9,S44, 1.0d0)
       deallocate(S44)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3412(N2,N3,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,S18,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S19(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S19)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N2,N3,N2,N3,N2,N3,N0,N2,X19,S19, 1.0d0)
       deallocate(S19)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef4132(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,r2C,D2)
       allocate(S59(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S59)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N0,N2,N2,N3,N0,N2,N0,N2,X9,S59,-1.0d0)
       deallocate(S59)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(U15(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K2
       call jungemm(I1,I2,I3,X9,D2,U15)
       deallocate(D2)
C
       call
     & sul124356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U15,-1.0d0)
       call
     & sul125346(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U15, 1.0d0)
       call
     & sul124365(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U15, 1.0d0)
       call
     & sul125364(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U15,-1.0d0)
       call
     & sul126345(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U15,-1.0d0)
       call
     & sul126354(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U15, 1.0d0)
       call
     & sul134256(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U15, 1.0d0)
       call
     & sul135246(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U15,-1.0d0)
       call
     & sul134265(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U15,-1.0d0)
       call
     & sul135264(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U15, 1.0d0)
       call
     & sul136245(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U15, 1.0d0)
       call
     & sul136254(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U15,-1.0d0)
       call
     & sul234156(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U15,-1.0d0)
       call
     & sul235146(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U15, 1.0d0)
       call
     & sul234165(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U15, 1.0d0)
       call
     & sul235164(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U15,-1.0d0)
       call
     & sul236145(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U15,-1.0d0)
       call
     & sul236154(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U15, 1.0d0)
       deallocate(U15)
       deallocate(X9)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,r2C,D2)
       allocate(S60(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S60)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N2,N3,N2,N3,N2,N3,N0,N2,X19,S60, 1.0d0)
       deallocate(S60)
C
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2C,D2)
       allocate(U41(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2*K2*K4
       I3=K4
       call jungemm(I1,I2,I3,X19,D2,U41)
       deallocate(D2)
C
       call
     & sul356124(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U41, 1.0d0)
       call
     & sul256134(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U41,-1.0d0)
       call
     & sul356214(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U41,-1.0d0)
       call
     & sul256314(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U41, 1.0d0)
       call
     & sul156234(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U41, 1.0d0)
       call
     & sul156324(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U41,-1.0d0)
       call
     & sul346125(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U41,-1.0d0)
       call
     & sul246135(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U41, 1.0d0)
       call
     & sul346215(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U41, 1.0d0)
       call
     & sul246315(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U41,-1.0d0)
       call
     & sul146235(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U41,-1.0d0)
       call
     & sul146325(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U41, 1.0d0)
       call
     & sul345126(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U41, 1.0d0)
       call
     & sul245136(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U41,-1.0d0)
       call
     & sul345216(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U41,-1.0d0)
       call
     & sul245316(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U41, 1.0d0)
       call
     & sul145236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U41, 1.0d0)
       call
     & sul145326(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U41,-1.0d0)
       deallocate(U41)
       deallocate(X19)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,r2C,D2)
       allocate(S61(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S61)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N2,N2,N3,N2,N3,N0,N2,X22,S61, 1.0d0)
       deallocate(S61)
C
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef412356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,t3D,F2)
       allocate(U58(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K2*K4*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,X22,F2,U58)
       deallocate(F2)
C
       call
     & sul125634(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U58, 1.0d0)
       call
     & sul135624(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U58,-1.0d0)
       call
     & sul235614(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U58, 1.0d0)
       call
     & sul124635(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U58,-1.0d0)
       call
     & sul134625(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U58, 1.0d0)
       call
     & sul234615(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U58,-1.0d0)
       call
     & sul124536(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U58, 1.0d0)
       call
     & sul134526(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U58,-1.0d0)
       call
     & sul234516(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U58, 1.0d0)
       deallocate(U58)
       deallocate(X22)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call relef12(0,N3,0,N3,
     & N2,N3,N0,N2,FockB,B1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(Q11(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,B1,B2,Q11)
       deallocate(B1)
       deallocate(B2)
C
       call sul21(N0,N2,N0,N2,X6,Q11, 1.0d0)
       deallocate(Q11)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,r2C,D2)
       allocate(S58(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S58)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N1,N1,N3,N2,N3,N0,N2,X21,S58, 1.0d0)
       deallocate(S58)
C
       allocate(F2(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef631245(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N2,N3,N0,N2,N0,N2,t3C,F2)
       allocate(U55(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K2*K4*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,X21,F2,U55)
       deallocate(F2)
C
       call
     & sul125634(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U55, 1.0d0)
       call
     & sul135624(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U55,-1.0d0)
       call
     & sul235614(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U55, 1.0d0)
       call
     & sul124635(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U55,-1.0d0)
       call
     & sul134625(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U55, 1.0d0)
       call
     & sul234615(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U55,-1.0d0)
       call
     & sul124536(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U55, 1.0d0)
       call
     & sul134526(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U55,-1.0d0)
       call
     & sul234516(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U55, 1.0d0)
       deallocate(U55)
       deallocate(X21)
C
       allocate(D1(N2+1:N3,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef1423(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S28(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       I1=K4*K3*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S28)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N1,N1,N3,N2,N3,N0,N2,X23,S28,-1.0d0)
       deallocate(S28)
C
       allocate(F2(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef631245(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N2,N3,N0,N2,N0,N2,t3C,F2)
       allocate(U32(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K2*K4*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,X23,F2,U32)
       deallocate(F2)
C
       call
     & sul235614(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U32,-1.0d0)
       call
     & sul135624(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U32, 1.0d0)
       call
     & sul125634(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U32,-1.0d0)
       call
     & sul234615(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U32, 1.0d0)
       call
     & sul134625(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U32,-1.0d0)
       call
     & sul124635(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U32, 1.0d0)
       call
     & sul234516(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U32,-1.0d0)
       call
     & sul134526(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U32, 1.0d0)
       call
     & sul124536(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U32,-1.0d0)
       deallocate(U32)
       deallocate(X23)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N0,N2,N2,N3,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S42(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S42)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,S42,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(S43(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K4*K4
       I3=K2*K2
       call jungemm(I1,I2,I3,D1,D2,S43)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X24(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       X24=0.0d0
       call sul2314(N2,N3,N2,N3,N2,N3,N0,N2,X24,S43, 1.0d0)
       deallocate(S43)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,r2C,D2)
       allocate(S66(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K2
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,D2,S66)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N2,N2,N3,N0,N2,N0,N2,X20,S66, 1.0d0)
       deallocate(S66)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(U78(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K2
       call jungemm(I1,I2,I3,X20,D2,U78)
       deallocate(D2)
C
       call
     & sul236145(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U78, 0.5d0)
       call
     & sul136245(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U78,-0.5d0)
       call
     & sul126345(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U78, 0.5d0)
       call
     & sul235146(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U78,-0.5d0)
       call
     & sul135246(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U78, 0.5d0)
       call
     & sul125346(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U78,-0.5d0)
       call
     & sul234156(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U78, 0.5d0)
       call
     & sul134256(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U78,-0.5d0)
       call
     & sul124356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U78, 0.5d0)
       deallocate(U78)
       deallocate(X20)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,r2C,D2)
       allocate(S67(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K2
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,D2,S67)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S67,D1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,t3D,F2)
       allocate(U77(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4*K4*K4
       I3=K2*K2
       call jungemm(I1,I2,I3,D1,F2,U77)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul123645(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U77, 0.250)
       call
     & sul123546(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U77,-0.250)
       V3D=V3D+0.250*U77
       deallocate(U77)
       deallocate(S67)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(Q14(N0+1:N2,N0+1:N2))
       I1=K2*K2
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q14)
       deallocate(D1)
       deallocate(B2)
C
       X1=X1+Q14
       deallocate(Q14)
C
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef412356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,t3D,F2)
       allocate(U1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K2*K4*K4*K4
       I3=K2
       call jungemm(I1,I2,I3,X1,F2,U1)
       deallocate(F2)
C
       V3D=V3D-U1
       call
     & sul123465(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U1, 1.0d0)
       call
     & sul123564(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U1,-1.0d0)
       deallocate(U1)
       deallocate(X1)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,r2C,D2)
       allocate(Q21(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,D2,Q21)
       deallocate(D1)
       deallocate(D2)
C
       call sul21(N2,N3,N2,N3,X4,Q21,-0.5d0)
       deallocate(Q21)
C
       allocate(F2(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef123456(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,t3D,F2)
       allocate(U6(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2*K2*K2*K4*K4
       I3=K4
       call jungemm(I1,I2,I3,X4,F2,U6)
       deallocate(F2)
C
       call
     & sul124563(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U6,-1.0d0)
       call
     & sul134562(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U6, 1.0d0)
       call
     & sul234561(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U6,-1.0d0)
       deallocate(U6)
       deallocate(X4)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,r2C,D2)
       allocate(Q22(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,D1,D2,Q22)
       deallocate(D1)
       deallocate(D2)
C
       call sul21(N0,N2,N0,N2,X6,Q22, 0.5d0)
       deallocate(Q22)
C
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef412356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,t3D,F2)
       allocate(U31(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K2*K4*K4*K4
       I3=K2
       call jungemm(I1,I2,I3,X6,F2,U31)
       deallocate(F2)
C
       call
     & sul123564(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U31,-1.0d0)
       call
     & sul123465(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U31, 1.0d0)
       V3D=V3D-U31
       deallocate(U31)
       deallocate(X6)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(F2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef463125(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,N2,N3,N0,N2,r3C,F2)
       allocate(S69(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2*K4*K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,F2,S69)
       deallocate(D1)
       deallocate(F2)
C
       call sul2341(N2,N3,N2,N3,N2,N3,N0,N2,X24,S69, 2.0d0)
       deallocate(S69)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N0,N2,IntM,D1)
       allocate(F2(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef613245(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N2,N3,N0,N2,N0,N2,r3C,F2)
       allocate(S70(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K2*K4
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,D1,F2,S70)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X25(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X25=0.0d0
       call sul2341(N0,N2,N2,N3,N0,N2,N0,N2,X25,S70, 1.0d0)
       deallocate(S70)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,r3D,F2)
       allocate(S71(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2*K4*K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,F2,S71)
       deallocate(D1)
       deallocate(F2)
C
       call sul2341(N2,N3,N2,N3,N2,N3,N0,N2,X24,S71,-1.0d0)
       deallocate(S71)
C
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2C,D2)
       allocate(U45(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2*K2*K4
       I3=K4
       call jungemm(I1,I2,I3,X24,D2,U45)
       deallocate(D2)
C
       call
     & sul156234(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U45, 0.5d0)
       call
     & sul256134(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U45,-0.5d0)
       call
     & sul356124(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U45, 0.5d0)
       call
     & sul146235(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U45,-0.5d0)
       call
     & sul246135(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U45, 0.5d0)
       call
     & sul346125(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U45,-0.5d0)
       call
     & sul145236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U45, 0.5d0)
       call
     & sul245136(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U45,-0.5d0)
       call
     & sul345126(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U45, 0.5d0)
       deallocate(U45)
       deallocate(X24)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef4123(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef412356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,r3D,F2)
       allocate(S72(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K2*K4
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,D1,F2,S72)
       deallocate(D1)
       deallocate(F2)
C
       call sul2341(N0,N2,N2,N3,N0,N2,N0,N2,X25,S72,-0.5d0)
       deallocate(S72)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(U83(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K2
       call jungemm(I1,I2,I3,X25,D2,U83)
       deallocate(D2)
C
       call
     & sul126345(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U83, 1.0d0)
       call
     & sul136245(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U83,-1.0d0)
       call
     & sul236145(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U83, 1.0d0)
       call
     & sul125346(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U83,-1.0d0)
       call
     & sul135246(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U83, 1.0d0)
       call
     & sul235146(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U83,-1.0d0)
       call
     & sul124356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U83, 1.0d0)
       call
     & sul134256(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U83,-1.0d0)
       call
     & sul234156(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U83, 1.0d0)
       deallocate(U83)
       deallocate(X25)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,H2C,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,r2C,D2)
       allocate(U63(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K2
       call jungemm(I1,I2,I3,D1,D2,U63)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul234156(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U63, 1.0d0)
       call
     & sul134256(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U63,-1.0d0)
       call
     & sul124356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U63, 1.0d0)
       call
     & sul235146(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U63,-1.0d0)
       call
     & sul135246(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U63, 1.0d0)
       call
     & sul125346(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U63,-1.0d0)
       call
     & sul236145(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U63, 1.0d0)
       call
     & sul136245(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U63,-1.0d0)
       call
     & sul126345(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U63, 1.0d0)
       deallocate(U63)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N2,N3,N0,N2,H2C,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,r2C,D2)
       allocate(U64(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2*K2*K4
       I3=K4
       call jungemm(I1,I2,I3,D1,D2,U64)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul345126(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U64,-1.0d0)
       call
     & sul245136(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U64, 1.0d0)
       call
     & sul145236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U64,-1.0d0)
       call
     & sul346125(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U64, 1.0d0)
       call
     & sul246135(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U64,-1.0d0)
       call
     & sul146235(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U64, 1.0d0)
       call
     & sul356124(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U64,-1.0d0)
       call
     & sul256134(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U64, 1.0d0)
       call
     & sul156234(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U64,-1.0d0)
       deallocate(U64)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,H2B,D1)
       allocate(F2(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef631245(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N2,N3,N0,N2,N0,N2,r3C,F2)
       allocate(U81(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K2*K4*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,F2,U81)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul234516(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U81, 1.0d0)
       call
     & sul134526(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U81,-1.0d0)
       call
     & sul124536(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U81, 1.0d0)
       call
     & sul234615(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U81,-1.0d0)
       call
     & sul134625(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U81, 1.0d0)
       call
     & sul124635(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U81,-1.0d0)
       call
     & sul235614(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U81, 1.0d0)
       call
     & sul135624(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U81,-1.0d0)
       call
     & sul125634(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U81, 1.0d0)
       deallocate(U81)
C
       allocate(B1(N0+1:N2,N0+1:N2))
       call relef21(N0,N3,N0,N3,
     & N0,N2,N0,N2,H1B,B1)
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef412356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,r3D,F2)
       allocate(U84(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K2*K4*K4*K4
       I3=K2
       call jungemm(I1,I2,I3,B1,F2,U84)
       deallocate(B1)
       deallocate(F2)
C
       V3D=V3D-U84
       call
     & sul123465(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U84, 1.0d0)
       call
     & sul123564(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U84,-1.0d0)
       deallocate(U84)
C
       allocate(B1(N2+1:N3,N2+1:N3))
       call relef12(N0,N3,N0,N3,
     & N2,N3,N2,N3,H1B,B1)
       allocate(F2(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef123456(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,r3D,F2)
       allocate(U85(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2*K2*K2*K4*K4
       I3=K4
       call jungemm(I1,I2,I3,B1,F2,U85)
       deallocate(B1)
       deallocate(F2)
C
       call
     & sul234561(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U85, 1.0d0)
       call
     & sul134562(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U85,-1.0d0)
       call
     & sul124563(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U85, 1.0d0)
       deallocate(U85)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N0,N2,N0,N2,H2C,D1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,r3D,F2)
       allocate(U86(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4*K4*K4
       I3=K2*K2
       call jungemm(I1,I2,I3,D1,F2,U86)
       deallocate(D1)
       deallocate(F2)
C
       V3D=V3D+0.5d0*U86
       call
     & sul123546(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U86,-0.5d0)
       call
     & sul123645(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U86, 0.5d0)
       deallocate(U86)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,H2C,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef412356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,r3D,F2)
       allocate(U87(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K2*K4*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,F2,U87)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul234516(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U87, 1.0d0)
       call
     & sul134526(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U87,-1.0d0)
       call
     & sul124536(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U87, 1.0d0)
       call
     & sul234615(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U87,-1.0d0)
       call
     & sul134625(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U87, 1.0d0)
       call
     & sul124635(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U87,-1.0d0)
       call
     & sul235614(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U87, 1.0d0)
       call
     & sul135624(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U87,-1.0d0)
       call
     & sul125634(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U87, 1.0d0)
       deallocate(U87)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N2,N3,N2,N3,H2C,D1)
       allocate(F2(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef123456(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,r3D,F2)
       allocate(U88(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K2*K2*K2*K4
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,F2,U88)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul345612(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U88, 0.5d0)
       call
     & sul245613(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U88,-0.5d0)
       call
     & sul145623(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U88, 0.5d0)
       deallocate(U88)
C
       end
