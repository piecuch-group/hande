       subroutine R3A_update(N0,N1,N2,N3,V3A,
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
       real*8 V3A(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)
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
       real*8,allocatable::S1(:,:,:,:)
       real*8,allocatable::S2(:,:,:,:)
       real*8,allocatable::S3(:,:,:,:)
       real*8,allocatable::S5(:,:,:,:)
       real*8,allocatable::S6(:,:,:,:)
       real*8,allocatable::S4(:,:,:,:)
       real*8,allocatable::S7(:,:,:,:)
       real*8,allocatable::S8(:,:,:,:)
       real*8,allocatable::S23(:,:,:,:)
       real*8,allocatable::S35(:,:,:,:)
       real*8,allocatable::Q11(:,:)
       real*8,allocatable::S51(:,:,:,:)
       real*8,allocatable::S55(:,:,:,:)
       real*8,allocatable::S58(:,:,:,:)
       real*8,allocatable::S9(:,:,:,:)
       real*8,allocatable::S16(:,:,:,:)
       real*8,allocatable::S17(:,:,:,:)
       real*8,allocatable::S25(:,:,:,:)
       real*8,allocatable::S26(:,:,:,:)
       real*8,allocatable::S40(:,:,:,:)
       real*8,allocatable::S41(:,:,:,:)
       real*8,allocatable::S14(:,:,:,:)
       real*8,allocatable::S24(:,:,:,:)
       real*8,allocatable::S15(:,:,:,:)
       real*8,allocatable::Q6(:,:)
       real*8,allocatable::S27(:,:,:,:)
       real*8,allocatable::S28(:,:,:,:)
       real*8,allocatable::S29(:,:,:,:)
       real*8,allocatable::S31(:,:,:,:)
       real*8,allocatable::S32(:,:,:,:)
       real*8,allocatable::S30(:,:,:,:)
       real*8,allocatable::S33(:,:,:,:)
       real*8,allocatable::S34(:,:,:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::Q3(:,:)
       real*8,allocatable::S10(:,:,:,:)
       real*8,allocatable::Q12(:,:)
       real*8,allocatable::S56(:,:,:,:)
       real*8,allocatable::S11(:,:,:,:)
       real*8,allocatable::S59(:,:,:,:)
       real*8,allocatable::S62(:,:,:,:)
       real*8,allocatable::S36(:,:,:,:)
       real*8,allocatable::S42(:,:,:,:)
       real*8,allocatable::S44(:,:,:,:)
       real*8,allocatable::S45(:,:,:,:)
       real*8,allocatable::Q4(:,:)
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::Q8(:,:)
       real*8,allocatable::Q9(:,:)
       real*8,allocatable::Q10(:,:)
       real*8,allocatable::S21(:,:,:,:)
       real*8,allocatable::S22(:,:,:,:)
       real*8,allocatable::S46(:,:,:,:)
       real*8,allocatable::Q13(:,:)
       real*8,allocatable::S47(:,:,:,:)
       real*8,allocatable::Q14(:,:)
       real*8,allocatable::Q15(:,:)
       real*8,allocatable::S19(:,:,:,:)
       real*8,allocatable::S20(:,:,:,:)
       real*8,allocatable::S66(:,:,:,:)
       real*8,allocatable::Q16(:,:)
       real*8,allocatable::S49(:,:,:,:)
       real*8,allocatable::S50(:,:,:,:)
       real*8,allocatable::S12(:,:,:,:)
       real*8,allocatable::S13(:,:,:,:)
       real*8,allocatable::S39(:,:,:,:)
       real*8,allocatable::S53(:,:,:,:)
       real*8,allocatable::S54(:,:,:,:)
       real*8,allocatable::S52(:,:,:,:)
       real*8,allocatable::S18(:,:,:,:)
       real*8,allocatable::S57(:,:,:,:)
       real*8,allocatable::Q17(:,:)
       real*8,allocatable::S48(:,:,:,:)
       real*8,allocatable::Q18(:,:)
       real*8,allocatable::S60(:,:,:,:)
       real*8,allocatable::S61(:,:,:,:)
       real*8,allocatable::S43(:,:,:,:)
       real*8,allocatable::Q19(:,:)
       real*8,allocatable::Q20(:,:)
       real*8,allocatable::S63(:,:,:,:)
       real*8,allocatable::S64(:,:,:,:)
       real*8,allocatable::S65(:,:,:,:)
       real*8,allocatable::S67(:,:,:,:)
       real*8,allocatable::S37(:,:,:,:)
       real*8,allocatable::S38(:,:,:,:)
       real*8,allocatable::S68(:,:,:,:)
       real*8,allocatable::Q21(:,:)
       real*8,allocatable::Q22(:,:)
       real*8,allocatable::S69(:,:,:,:)
       real*8,allocatable::S70(:,:,:,:)
       real*8,allocatable::S71(:,:,:,:)
       real*8,allocatable::S72(:,:,:,:)
       real*8,allocatable::X1(:,:)
       real*8,allocatable::U1(:,:,:,:,:,:)
       real*8,allocatable::U2(:,:,:,:,:,:)
       real*8,allocatable::X2(:,:,:,:)
       real*8,allocatable::U3(:,:,:,:,:,:)
       real*8,allocatable::U4(:,:,:,:,:,:)
       real*8,allocatable::X3(:,:,:,:)
       real*8,allocatable::U6(:,:,:,:,:,:)
       real*8,allocatable::X4(:,:,:,:)
       real*8,allocatable::U5(:,:,:,:,:,:)
       real*8,allocatable::U9(:,:,:,:,:,:)
       real*8,allocatable::X5(:,:,:,:)
       real*8,allocatable::U14(:,:,:,:,:,:)
       real*8,allocatable::U15(:,:,:,:,:,:)
       real*8,allocatable::U20(:,:,:,:,:,:)
       real*8,allocatable::X6(:,:,:,:)
       real*8,allocatable::U22(:,:,:,:,:,:)
       real*8,allocatable::X7(:,:,:,:)
       real*8,allocatable::U23(:,:,:,:,:,:)
       real*8,allocatable::X8(:,:,:,:)
       real*8,allocatable::U33(:,:,:,:,:,:)
       real*8,allocatable::X9(:,:,:,:)
       real*8,allocatable::U38(:,:,:,:,:,:)
       real*8,allocatable::X10(:,:,:,:)
       real*8,allocatable::U39(:,:,:,:,:,:)
       real*8,allocatable::X11(:,:)
       real*8,allocatable::U47(:,:,:,:,:,:)
       real*8,allocatable::X12(:,:)
       real*8,allocatable::U24(:,:,:,:,:,:)
       real*8,allocatable::X13(:,:,:,:)
       real*8,allocatable::U25(:,:,:,:,:,:)
       real*8,allocatable::X14(:,:,:,:)
       real*8,allocatable::U26(:,:,:,:,:,:)
       real*8,allocatable::X15(:,:,:,:)
       real*8,allocatable::U28(:,:,:,:,:,:)
       real*8,allocatable::X16(:,:,:,:)
       real*8,allocatable::U32(:,:,:,:,:,:)
       real*8,allocatable::X17(:,:)
       real*8,allocatable::U48(:,:,:,:,:,:)
       real*8,allocatable::U64(:,:,:,:,:,:)
       real*8,allocatable::U67(:,:,:,:,:,:)
       real*8,allocatable::X18(:,:,:,:)
       real*8,allocatable::U70(:,:,:,:,:,:)
       real*8,allocatable::X19(:,:,:,:)
       real*8,allocatable::U42(:,:,:,:,:,:)
       real*8,allocatable::X20(:,:,:,:)
       real*8,allocatable::U49(:,:,:,:,:,:)
       real*8,allocatable::X21(:,:,:,:)
       real*8,allocatable::U17(:,:,:,:,:,:)
       real*8,allocatable::U55(:,:,:,:,:,:)
       real*8,allocatable::U56(:,:,:,:,:,:)
       real*8,allocatable::U58(:,:,:,:,:,:)
       real*8,allocatable::X22(:,:,:,:)
       real*8,allocatable::U62(:,:,:,:,:,:)
       real*8,allocatable::X23(:,:,:,:)
       real*8,allocatable::U65(:,:,:,:,:,:)
       real*8,allocatable::U69(:,:,:,:,:,:)
       real*8,allocatable::X24(:,:,:,:)
       real*8,allocatable::U36(:,:,:,:,:,:)
       real*8,allocatable::U81(:,:,:,:,:,:)
       real*8,allocatable::U82(:,:,:,:,:,:)
       real*8,allocatable::U83(:,:,:,:,:,:)
       real*8,allocatable::U84(:,:,:,:,:,:)
       real*8,allocatable::U85(:,:,:,:,:,:)
       real*8,allocatable::X25(:,:,:,:)
       real*8,allocatable::U87(:,:,:,:,:,:)
       real*8,allocatable::U88(:,:,:,:,:,:)
C
       allocate(B1(N0+1:N1,N1+1:N3))
       call relef21(0,N3,0,N3,
     & N0,N1,N1,N3,FockR,B1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(Q1(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,B1,B2,Q1)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X1(N1+1:N3,N1+1:N3))
       X1=0.0d0
       call sul21(N1,N3,N1,N3,X1,Q1, 1.0d0)
       deallocate(Q1)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N0,N1,N0,N1,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S1(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef2134(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N0,N1,S1,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(U2(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K1*K3*K3
       I3=K1
       call jungemm(I1,I2,I3,D1,D2,U2)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul124356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U2,-1.0d0)
       call
     & sul125346(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U2, 1.0d0)
       call
     & sul126345(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U2,-1.0d0)
       call
     & sul134256(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U2, 1.0d0)
       call
     & sul135246(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U2,-1.0d0)
       call
     & sul136245(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U2, 1.0d0)
       call
     & sul234156(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U2,-1.0d0)
       call
     & sul235146(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U2, 1.0d0)
       call
     & sul236145(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U2,-1.0d0)
       deallocate(U2)
       deallocate(S1)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S2(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S2)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X2(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       X2=0.0d0
       call sul3124(N1,N3,N1,N3,N1,N3,N0,N1,X2,S2, 1.0d0)
       deallocate(S2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S3(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S3)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef2314(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,S3,D1)
       allocate(F2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,t3A,F2)
       allocate(U4(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K1*K3*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,F2,U4)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul124536(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U4,-1.0d0)
       call
     & sul124635(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U4, 1.0d0)
       call
     & sul125634(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U4,-1.0d0)
       call
     & sul134526(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U4, 1.0d0)
       call
     & sul134625(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U4,-1.0d0)
       call
     & sul135624(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U4, 1.0d0)
       call
     & sul234516(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U4,-1.0d0)
       call
     & sul234615(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U4, 1.0d0)
       call
     & sul235614(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U4,-1.0d0)
       deallocate(U4)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef3214(N1,N3,N0,N1,N1,N3,N0,N1,
     & N1,N3,N0,N1,N1,N3,N0,N1,S3,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S5(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S5)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       X3=0.0d0
       call sul3124(N0,N1,N1,N3,N0,N1,N0,N1,X3,S5, 1.0d0)
       deallocate(S5)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef2314(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,S3,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S6(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S6)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N1,N3,N1,N3,N1,N3,N0,N1,X2,S6, 1.0d0)
       deallocate(S6)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S4(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       I1=K3*K3*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S4)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       X4=0.0d0
       call sul4123(N1,N3,N1,N3,N1,N3,N1,N3,X4,S4, 1.0d0)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef2341(N1,N3,N1,N3,N1,N3,N1,N3,
     & N1,N3,N1,N3,N1,N3,N1,N3,S4,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S7(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       I1=K3*K3*K3
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S7)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N1,N3,N1,N3,N1,N3,N0,N1,X2,S7,-1.0d0)
       deallocate(S7)
C
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(U3(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1*K1*K3
       I3=K3
       call jungemm(I1,I2,I3,X2,D2,U3)
       deallocate(D2)
C
       call
     & sul245136(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U3, 1.0d0)
       call
     & sul145236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U3,-1.0d0)
       call
     & sul246135(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U3,-1.0d0)
       call
     & sul146235(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U3, 1.0d0)
       call
     & sul256134(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U3, 1.0d0)
       call
     & sul156234(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U3,-1.0d0)
       call
     & sul345126(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U3,-1.0d0)
       call
     & sul145326(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U3, 1.0d0)
       call
     & sul346125(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U3, 1.0d0)
       call
     & sul146325(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U3,-1.0d0)
       call
     & sul356124(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U3,-1.0d0)
       call
     & sul156324(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U3, 1.0d0)
       call
     & sul345216(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U3, 1.0d0)
       call
     & sul245316(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U3,-1.0d0)
       call
     & sul346215(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U3,-1.0d0)
       call
     & sul246315(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U3, 1.0d0)
       call
     & sul356214(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U3, 1.0d0)
       call
     & sul256314(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U3,-1.0d0)
       deallocate(U3)
       deallocate(X2)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N0,N1,N1,N3,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S8(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S8)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef2341(N0,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,S8,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S23(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S23)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef2314(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,S23,D1)
       allocate(F2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,t3A,F2)
       allocate(U20(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K1*K3*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,F2,U20)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul134526(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U20, 1.0d0)
       call
     & sul134625(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U20,-1.0d0)
       call
     & sul135624(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U20, 1.0d0)
       call
     & sul234516(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U20,-1.0d0)
       call
     & sul234615(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U20, 1.0d0)
       call
     & sul235614(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U20,-1.0d0)
       deallocate(U20)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef4231(N0,N1,N0,N1,N0,N1,N1,N3,
     & N1,N3,N0,N1,N0,N1,N0,N1,S8,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S35(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S35)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X8(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       X8=0.0d0
       call sul3124(N0,N1,N0,N1,N0,N1,N0,N1,X8,S35, 1.0d0)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef2431(N0,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,N0,N1,S8,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(Q11(N0+1:N1,N0+1:N1))
       I1=K1*K1
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q11)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X11(N0+1:N1,N0+1:N1))
       X11=0.0d0
       X11=X11+Q11
       deallocate(Q11)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef2341(N0,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,S8,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,r2A,D2)
       allocate(S51(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K3*K3
       I3=K1*K1
       call jungemm(I1,I2,I3,D1,D2,S51)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X7(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       X7=0.0d0
       call sul2314(N1,N3,N1,N3,N1,N3,N0,N1,X7,S51, 0.5d0)
       deallocate(S51)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef3421(N0,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,N0,N1,S8,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,r2A,D2)
       allocate(S55(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S55)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X10(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       X10=0.0d0
       call sul2314(N0,N1,N1,N3,N0,N1,N0,N1,X10,S55,-1.0d0)
       deallocate(S55)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef2431(N0,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,N0,N1,S8,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,r2A,D2)
       allocate(S58(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S58)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X9(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       X9=0.0d0
       call sul2314(N0,N1,N1,N3,N0,N1,N0,N1,X9,S58,-1.0d0)
       deallocate(S58)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef3241(N0,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,S8,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S9(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S9)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef2314(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,S9,D1)
       allocate(F2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,t3A,F2)
       allocate(U9(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K1*K3*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,F2,U9)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul124536(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U9, 1.0d0)
       call
     & sul124635(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U9,-1.0d0)
       call
     & sul125634(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U9, 1.0d0)
       deallocate(U9)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef3214(N1,N3,N0,N1,N1,N3,N0,N1,
     & N1,N3,N0,N1,N1,N3,N0,N1,S9,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S16(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S16)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X5(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       X5=0.0d0
       call sul3124(N0,N1,N1,N3,N0,N1,N0,N1,X5,S16, 1.0d0)
       deallocate(S16)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef2314(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,S9,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S17(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S17)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef2134(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N1,N3,N1,N3,N0,N1,S17,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(U15(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1*K1*K3
       I3=K3
       call jungemm(I1,I2,I3,D1,D2,U15)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul245136(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U15,-1.0d0)
       call
     & sul145236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U15, 1.0d0)
       call
     & sul246135(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U15, 1.0d0)
       call
     & sul146235(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U15,-1.0d0)
       call
     & sul256134(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U15,-1.0d0)
       call
     & sul156234(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U15, 1.0d0)
       call
     & sul345126(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U15, 1.0d0)
       call
     & sul346125(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U15,-1.0d0)
       call
     & sul356124(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U15, 1.0d0)
       deallocate(U15)
       deallocate(S17)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef3214(N1,N3,N0,N1,N1,N3,N0,N1,
     & N1,N3,N0,N1,N1,N3,N0,N1,S23,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S25(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S25)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X6(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       X6=0.0d0
       call sul3124(N0,N1,N1,N3,N0,N1,N0,N1,X6,S25, 1.0d0)
       deallocate(S25)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef2314(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,S23,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S26(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S26)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N1,N3,N1,N3,N1,N3,N0,N1,X7,S26, 1.0d0)
       deallocate(S26)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef2314(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,S35,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S40(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S40)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N1,N1,N3,N0,N1,N0,N1,X9,S40, 1.0d0)
       deallocate(S40)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef3214(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,S35,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S41(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S41)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N1,N1,N3,N0,N1,N0,N1,X10,S41, 1.0d0)
       deallocate(S41)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N0,N1,N0,N1,IntR,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(S14(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K1
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,D2,S14)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef3412(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,S14,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S24(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S24)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N1,N1,N3,N0,N1,N0,N1,X6,S24,-0.5d0)
       deallocate(S24)
C
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(U22(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K1*K3*K3
       I3=K1
       call jungemm(I1,I2,I3,X6,D2,U22)
       deallocate(D2)
C
       call
     & sul134256(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U22, 1.0d0)
       call
     & sul135246(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U22,-1.0d0)
       call
     & sul136245(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U22, 1.0d0)
       call
     & sul234156(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U22,-1.0d0)
       call
     & sul235146(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U22, 1.0d0)
       call
     & sul236145(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U22,-1.0d0)
       deallocate(U22)
       deallocate(X6)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef4312(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,S14,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S15(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S15)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N1,N1,N3,N0,N1,N0,N1,X5,S15,-0.5d0)
       deallocate(S15)
C
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(U14(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K1*K3*K3
       I3=K1
       call jungemm(I1,I2,I3,X5,D2,U14)
       deallocate(D2)
C
       call
     & sul124356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U14, 1.0d0)
       call
     & sul125346(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U14,-1.0d0)
       call
     & sul126345(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U14, 1.0d0)
       deallocate(U14)
       deallocate(X5)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call relef12(0,N3,0,N3,
     & N1,N3,N0,N1,FockR,B1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(Q6(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,B1,B2,Q6)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X12(N0+1:N1,N0+1:N1))
       X12=0.0d0
       call sul21(N0,N1,N0,N1,X12,Q6, 1.0d0)
       deallocate(Q6)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S27(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S27)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X13(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       X13=0.0d0
       call sul3124(N0,N1,N1,N3,N0,N1,N0,N1,X13,S27, 1.0d0)
       deallocate(S27)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N1,N3,N1,N3,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S28(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       I1=K3*K3*K3
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S28)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X14(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       X14=0.0d0
       call sul4123(N1,N3,N1,N3,N1,N3,N0,N1,X14,S28, 1.0d0)
       deallocate(S28)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N0,N1,N0,N1,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S29(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S29)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N1,N0,N1,N0,N1,N0,N1,X8,S29,-1.0d0)
C
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,t3A,F2)
       allocate(U33(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K3*K3*K3
       I3=K1*K1
       call jungemm(I1,I2,I3,X8,F2,U33)
       deallocate(F2)
C
       call
     & sul123546(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U33, 0.5d0)
       call
     & sul123645(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U33,-0.5d0)
       V3A=V3A-0.5d0*U33
       call
     & sul123654(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U33, 0.5d0)
       call
     & sul123465(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U33, 0.5d0)
       call
     & sul123564(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U33,-0.5d0)
       deallocate(U33)
       deallocate(X8)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef2314(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,S29,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S31(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S31)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N1,N1,N3,N0,N1,N0,N1,X9,S31,-1.0d0)
       deallocate(S31)
C
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(U38(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K1*K3*K3
       I3=K1
       call jungemm(I1,I2,I3,X9,D2,U38)
       deallocate(D2)
C
       call
     & sul235146(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U38, 1.0d0)
       call
     & sul135246(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U38,-1.0d0)
       call
     & sul236145(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U38,-1.0d0)
       call
     & sul136245(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U38, 1.0d0)
       call
     & sul234156(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U38,-1.0d0)
       call
     & sul134256(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U38, 1.0d0)
       call
     & sul236154(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U38, 1.0d0)
       call
     & sul136254(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U38,-1.0d0)
       call
     & sul234165(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U38, 1.0d0)
       call
     & sul134265(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U38,-1.0d0)
       call
     & sul235164(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U38,-1.0d0)
       call
     & sul135264(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U38, 1.0d0)
       deallocate(U38)
       deallocate(X9)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef3214(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,S29,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S32(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S32)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N1,N1,N3,N0,N1,N0,N1,X10,S32,-1.0d0)
       deallocate(S32)
C
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(U39(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K1*K3*K3
       I3=K1
       call jungemm(I1,I2,I3,X10,D2,U39)
       deallocate(D2)
C
       call
     & sul125346(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U39,-1.0d0)
       call
     & sul126345(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U39, 1.0d0)
       call
     & sul124356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U39, 1.0d0)
       call
     & sul126354(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U39,-1.0d0)
       call
     & sul124365(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U39,-1.0d0)
       call
     & sul125364(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U39, 1.0d0)
       deallocate(U39)
       deallocate(X10)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S30(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S30)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X15(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       X15=0.0d0
       call sul4123(N0,N1,N1,N3,N1,N3,N0,N1,X15,S30, 1.0d0)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef3241(N0,N1,N0,N1,N1,N3,N1,N3,
     & N1,N3,N0,N1,N1,N3,N0,N1,S30,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S33(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S33)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N1,N1,N3,N0,N1,N0,N1,X13,S33, 1.0d0)
       deallocate(S33)
C
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(U25(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K1*K3*K3
       I3=K1
       call jungemm(I1,I2,I3,X13,D2,U25)
       deallocate(D2)
C
       call
     & sul235146(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U25,-1.0d0)
       call
     & sul135246(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U25, 1.0d0)
       call
     & sul125346(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U25,-1.0d0)
       call
     & sul236145(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U25, 1.0d0)
       call
     & sul136245(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U25,-1.0d0)
       call
     & sul126345(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U25, 1.0d0)
       call
     & sul234156(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U25, 1.0d0)
       call
     & sul134256(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U25,-1.0d0)
       call
     & sul124356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U25, 1.0d0)
       call
     & sul236154(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U25,-1.0d0)
       call
     & sul136254(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U25, 1.0d0)
       call
     & sul126354(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U25,-1.0d0)
       call
     & sul234165(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U25,-1.0d0)
       call
     & sul134265(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U25, 1.0d0)
       call
     & sul124365(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U25,-1.0d0)
       call
     & sul235164(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U25, 1.0d0)
       call
     & sul135264(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U25,-1.0d0)
       call
     & sul125364(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U25, 1.0d0)
       deallocate(U25)
       deallocate(X13)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef2341(N0,N1,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,S30,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S34(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S34)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X16(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       X16=0.0d0
       call sul3124(N1,N3,N1,N3,N1,N3,N0,N1,X16,S34, 1.0d0)
       deallocate(S34)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N1,N3,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q2(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q2)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call relef21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q2,B1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(Q7(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,B1,B2,Q7)
       deallocate(B1)
       deallocate(B2)
C
       call sul21(N0,N1,N0,N1,X12,Q7, 1.0d0)
       deallocate(Q7)
C
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(Q3(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,Q2,B2,Q3)
       deallocate(B2)
C
       call sul21(N1,N3,N1,N3,X1,Q3, 1.0d0)
       deallocate(Q3)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S10(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S10)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef2341(N1,N3,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,S10,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(Q12(N1+1:N3,N1+1:N3))
       I1=K3*K3
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q12)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X17(N1+1:N3,N1+1:N3))
       X17=0.0d0
       X17=X17+Q12
       deallocate(Q12)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef2431(N1,N3,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,S10,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,r2A,D2)
       allocate(S56(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S56)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3412(N1,N3,N0,N1,N1,N3,N1,N3,
     & N1,N3,N1,N3,N1,N3,N0,N1,S56,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(U64(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1*K1*K3
       I3=K3
       call jungemm(I1,I2,I3,D1,D2,U64)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul256134(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U64,-1.0d0)
       call
     & sul156234(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U64, 1.0d0)
       call
     & sul156324(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U64,-1.0d0)
       call
     & sul246135(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U64, 1.0d0)
       call
     & sul146235(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U64,-1.0d0)
       call
     & sul146325(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U64, 1.0d0)
       call
     & sul245136(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U64,-1.0d0)
       call
     & sul145236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U64, 1.0d0)
       call
     & sul145326(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U64,-1.0d0)
       deallocate(U64)
       deallocate(S56)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef2341(N1,N3,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,S10,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S11(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       I1=K3*K3*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S11)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N1,N3,N1,N3,N1,N3,N1,N3,X4,S11, 1.0d0)
       deallocate(S11)
C
       allocate(F2(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef123456(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,t3A,F2)
       allocate(U5(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K1*K1*K1*K3
       I3=K3*K3
       call jungemm(I1,I2,I3,X4,F2,U5)
       deallocate(F2)
C
       call
     & sul245613(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U5,-0.5d0)
       call
     & sul145623(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U5, 0.5d0)
       call
     & sul345612(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U5, 0.5d0)
       call
     & sul145632(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U5,-0.5d0)
       call
     & sul345621(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U5,-0.5d0)
       call
     & sul245631(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U5, 0.5d0)
       deallocate(U5)
       deallocate(X4)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef2341(N1,N3,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,S10,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,r2A,D2)
       allocate(S59(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S59)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3412(N1,N3,N0,N1,N1,N3,N1,N3,
     & N1,N3,N1,N3,N1,N3,N0,N1,S59,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(U67(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1*K1*K3
       I3=K3
       call jungemm(I1,I2,I3,D1,D2,U67)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul356124(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U67,-1.0d0)
       call
     & sul356214(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U67, 1.0d0)
       call
     & sul256314(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U67,-1.0d0)
       call
     & sul346125(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U67, 1.0d0)
       call
     & sul346215(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U67,-1.0d0)
       call
     & sul246315(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U67, 1.0d0)
       call
     & sul345126(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U67,-1.0d0)
       call
     & sul345216(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U67, 1.0d0)
       call
     & sul245316(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U67,-1.0d0)
       deallocate(U67)
       deallocate(S59)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef3421(N1,N3,N0,N1,N1,N3,N1,N3,
     & N1,N3,N1,N3,N0,N1,N1,N3,S10,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,r2A,D2)
       allocate(S62(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K1*K1
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,D2,S62)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X18(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       X18=0.0d0
       call sul3412(N0,N1,N1,N3,N0,N1,N0,N1,X18,S62, 1.0d0)
       deallocate(S62)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3241(N1,N3,N0,N1,N1,N3,N1,N3,
     & N1,N3,N0,N1,N1,N3,N1,N3,S10,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S36(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S36)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N1,N1,N3,N1,N3,N0,N1,X15,S36, 1.0d0)
C
       allocate(F2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,t3A,F2)
       allocate(U28(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K1*K3*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,X15,F2,U28)
       deallocate(F2)
C
       call
     & sul235614(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U28,-1.0d0)
       call
     & sul135624(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U28, 1.0d0)
       call
     & sul125634(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U28,-1.0d0)
       call
     & sul234615(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U28, 1.0d0)
       call
     & sul134625(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U28,-1.0d0)
       call
     & sul124635(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U28, 1.0d0)
       call
     & sul234516(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U28,-1.0d0)
       call
     & sul134526(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U28, 1.0d0)
       call
     & sul124536(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U28,-1.0d0)
       deallocate(U28)
       deallocate(X15)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef2341(N0,N1,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,S36,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S42(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S42)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N1,N3,N1,N3,N1,N3,N0,N1,X14,S42,-1.0d0)
       deallocate(S42)
C
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(U26(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1*K1*K3
       I3=K3
       call jungemm(I1,I2,I3,X14,D2,U26)
       deallocate(D2)
C
       call
     & sul356124(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U26, 1.0d0)
       call
     & sul256134(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U26,-1.0d0)
       call
     & sul156234(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U26, 1.0d0)
       call
     & sul346125(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U26,-1.0d0)
       call
     & sul246135(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U26, 1.0d0)
       call
     & sul146235(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U26,-1.0d0)
       call
     & sul345126(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U26, 1.0d0)
       call
     & sul245136(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U26,-1.0d0)
       call
     & sul145236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U26, 1.0d0)
       deallocate(U26)
       deallocate(X14)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       call relef2341(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N2,N0,N1,N2,N3,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S44(N0+1:N1,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4*K1*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S44)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,N0+1:N1))
       call relef3241(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N1,N0,N2,N2,N3,N0,N1,S44,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S45(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S45)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X19(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       X19=0.0d0
       call sul3124(N0,N2,N2,N3,N1,N3,N0,N1,X19,S45, 1.0d0)
       deallocate(S45)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N1,N3,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q4(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q4)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(Q5(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,Q4,B2,Q5)
       deallocate(B2)
C
       call sul21(N1,N3,N1,N3,X1,Q5, 1.0d0)
       deallocate(Q5)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call relef21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q4,B1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(Q8(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,B1,B2,Q8)
       deallocate(B1)
       deallocate(B2)
C
       call sul21(N0,N1,N0,N1,X12,Q8, 1.0d0)
       deallocate(Q8)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N0,N1,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(Q9(N0+1:N1,N0+1:N1))
       I1=K1*K1
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q9)
       deallocate(D1)
       deallocate(B2)
C
       X11=X11-Q9
       deallocate(Q9)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(Q10(N1+1:N3,N1+1:N3))
       I1=K3*K3
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q10)
       deallocate(D1)
       deallocate(B2)
C
       X17=X17+Q10
       deallocate(Q10)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(S21(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S21)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3412(N1,N3,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,S21,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S22(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S22)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N1,N3,N1,N3,N1,N3,N0,N1,X16,S22,-1.0d0)
       deallocate(S22)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef4312(N1,N3,N0,N1,N0,N1,N1,N3,
     & N1,N3,N0,N1,N1,N3,N0,N1,S21,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S46(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S46)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N1,N1,N3,N0,N1,N0,N1,X3,S46,-1.0d0)
       deallocate(S46)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N1,N3,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(Q13(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q13)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call relef21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q13,B1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(S47(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K1*K3
       I3=K3
       call jungemm(I1,I2,I3,B1,D2,S47)
       deallocate(B1)
       deallocate(D2)
C
       allocate(X20(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       X20=0.0d0
       call sul2341(N0,N1,N1,N3,N0,N1,N0,N1,X20,S47, 1.0d0)
       deallocate(S47)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N0,N1,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(Q14(N0+1:N1,N0+1:N1))
       I1=K1*K1
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q14)
       deallocate(D1)
       deallocate(B2)
C
       X11=X11-Q14
       deallocate(Q14)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(Q15(N1+1:N3,N1+1:N3))
       I1=K3*K3
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q15)
       deallocate(D1)
       deallocate(B2)
C
       X17=X17+Q15
       deallocate(Q15)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       call relef2341(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N2,N0,N1,N2,N3,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S19(N0+1:N1,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4*K1*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S19)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,N0+1:N1))
       call relef3241(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N1,N0,N2,N2,N3,N0,N1,S19,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S20(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S20)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X21(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       X21=0.0d0
       call sul3124(N0,N2,N2,N3,N1,N3,N0,N1,X21,S20, 1.0d0)
       deallocate(S20)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call relef2431(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,N0,N1,S19,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,r2B,D2)
       allocate(S66(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S66)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N0,N1,N1,N3,N0,N1,N0,N1,X3,S66, 1.0d0)
       deallocate(S66)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call relef2431(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,N0,N1,S19,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(Q16(N0+1:N1,N0+1:N1))
       I1=K1*K1
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q16)
       deallocate(D1)
       deallocate(B2)
C
       X11=X11-Q16
       deallocate(Q16)
C
       allocate(F2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,t3A,F2)
       allocate(U47(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K1*K3*K3*K3
       I3=K1
       call jungemm(I1,I2,I3,X11,F2,U47)
       deallocate(F2)
C
       V3A=V3A+U47
       call
     & sul123465(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U47,-1.0d0)
       call
     & sul123564(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U47, 1.0d0)
       deallocate(U47)
       deallocate(X11)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,r2A,D2)
       allocate(S49(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K3*K3
       I3=K1*K1
       call jungemm(I1,I2,I3,D1,D2,S49)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N1,N3,N1,N3,N1,N3,N0,N1,X7,S49,-0.5d0)
       deallocate(S49)
C
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(U23(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1*K1*K3
       I3=K3
       call jungemm(I1,I2,I3,X7,D2,U23)
       deallocate(D2)
C
       call
     & sul145236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U23, 1.0d0)
       call
     & sul146235(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U23,-1.0d0)
       call
     & sul156234(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U23, 1.0d0)
       call
     & sul345126(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U23, 1.0d0)
       call
     & sul245136(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U23,-1.0d0)
       call
     & sul346125(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U23,-1.0d0)
       call
     & sul246135(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U23, 1.0d0)
       call
     & sul356124(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U23, 1.0d0)
       call
     & sul256134(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U23,-1.0d0)
       deallocate(U23)
       deallocate(X7)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,r2A,D2)
       allocate(S50(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K3*K3
       I3=K1*K1
       call jungemm(I1,I2,I3,D1,D2,S50)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N1,N3,N1,N3,
     & N1,N3,N1,N3,N1,N3,N1,N3,S50,D1)
       allocate(F2(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef123456(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,t3A,F2)
       allocate(U58(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K1*K1*K1*K3
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,F2,U58)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul145623(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U58, 0.250)
       call
     & sul245613(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U58,-0.250)
       call
     & sul345612(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U58, 0.250)
       deallocate(U58)
       deallocate(S50)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S12(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S12)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3412(N1,N3,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,S12,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S13(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S13)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N1,N3,N1,N3,N1,N3,N0,N1,X16,S13, 1.0d0)
       deallocate(S13)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef4312(N1,N3,N0,N1,N0,N1,N1,N3,
     & N1,N3,N0,N1,N1,N3,N0,N1,S12,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S39(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S39)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N1,N1,N3,N0,N1,N0,N1,X3,S39, 1.0d0)
       deallocate(S39)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,r2A,D2)
       allocate(S53(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S53)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N1,N3,N1,N3,N1,N3,N0,N1,X16,S53, 1.0d0)
       deallocate(S53)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,r2A,D2)
       allocate(S54(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S54)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X22(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       X22=0.0d0
       call sul3412(N0,N1,N1,N3,N1,N3,N0,N1,X22,S54, 1.0d0)
       deallocate(S54)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef4132(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N0,N1,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,r2A,D2)
       allocate(S52(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S52)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N0,N1,N1,N3,N0,N1,N0,N1,X3,S52,-1.0d0)
       deallocate(S52)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,N0+1:N1))
       call relef4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N2,N2,N3,N0,N1,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S18(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S18)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N2,N3,N1,N3,N0,N1,X21,S18, 1.0d0)
       deallocate(S18)
C
       allocate(F2(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N2,N3,N1,N3,N1,N3,N0,N1,N0,N1,t3B,F2)
       allocate(U17(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K1*K3*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,X21,F2,U17)
       deallocate(F2)
C
       call
     & sul124536(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U17,-1.0d0)
       call
     & sul124635(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U17, 1.0d0)
       call
     & sul125634(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U17,-1.0d0)
       call
     & sul134526(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U17, 1.0d0)
       call
     & sul134625(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U17,-1.0d0)
       call
     & sul135624(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U17, 1.0d0)
       call
     & sul234516(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U17,-1.0d0)
       call
     & sul234615(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U17, 1.0d0)
       call
     & sul235614(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U17,-1.0d0)
       deallocate(U17)
       deallocate(X21)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N2,N3,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,r2A,D2)
       allocate(S57(N1+1:N3,N0+1:N1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S57)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X23(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       X23=0.0d0
       call sul3412(N0,N2,N2,N3,N1,N3,N0,N1,X23,S57, 1.0d0)
       deallocate(S57)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N1,N3,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(Q17(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q17)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call relef21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q17,B1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(S48(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K1*K3
       I3=K3
       call jungemm(I1,I2,I3,B1,D2,S48)
       deallocate(B1)
       deallocate(D2)
C
       call sul2341(N0,N1,N1,N3,N0,N1,N0,N1,X20,S48, 1.0d0)
       deallocate(S48)
C
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(U49(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K1*K3*K3
       I3=K1
       call jungemm(I1,I2,I3,X20,D2,U49)
       deallocate(D2)
C
       call
     & sul234156(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U49, 1.0d0)
       call
     & sul134256(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U49,-1.0d0)
       call
     & sul124356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U49, 1.0d0)
       call
     & sul235146(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U49,-1.0d0)
       call
     & sul135246(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U49, 1.0d0)
       call
     & sul125346(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U49,-1.0d0)
       call
     & sul126345(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U49, 1.0d0)
       call
     & sul136245(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U49,-1.0d0)
       call
     & sul236145(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U49, 1.0d0)
       deallocate(U49)
       deallocate(X20)
C
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q18(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,Q17,B2,Q18)
       deallocate(B2)
C
       call sul21(N1,N3,N1,N3,X17,Q18,-1.0d0)
       deallocate(Q18)
C
       allocate(F2(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef123456(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,t3A,F2)
       allocate(U48(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1*K1*K1*K3*K3
       I3=K3
       call jungemm(I1,I2,I3,X17,F2,U48)
       deallocate(F2)
C
       call
     & sul234561(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U48, 1.0d0)
       call
     & sul134562(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U48,-1.0d0)
       call
     & sul124563(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U48, 1.0d0)
       deallocate(U48)
       deallocate(X17)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N0,N1,N1,N3,IntR,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,r2A,D2)
       allocate(S60(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K1*K1
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,D2,S60)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N1,N1,N3,N0,N1,N0,N1,X18,S60, 1.0d0)
       deallocate(S60)
C
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(U70(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K1*K3*K3
       I3=K1
       call jungemm(I1,I2,I3,X18,D2,U70)
       deallocate(D2)
C
       call
     & sul236145(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U70, 0.5d0)
       call
     & sul136245(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U70,-0.5d0)
       call
     & sul126345(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U70, 0.5d0)
       call
     & sul235146(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U70,-0.5d0)
       call
     & sul135246(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U70, 0.5d0)
       call
     & sul125346(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U70,-0.5d0)
       call
     & sul234156(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U70, 0.5d0)
       call
     & sul134256(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U70,-0.5d0)
       call
     & sul124356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U70, 0.5d0)
       deallocate(U70)
       deallocate(X18)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N0,N1,N0,N1,IntR,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,r2A,D2)
       allocate(S61(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K1
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,D2,S61)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef3412(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,S61,D1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,t3A,F2)
       allocate(U69(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K3*K3*K3
       I3=K1*K1
       call jungemm(I1,I2,I3,D1,F2,U69)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul123645(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U69, 0.250)
       call
     & sul123546(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U69,-0.250)
       V3A=V3A+0.250*U69
       deallocate(U69)
       deallocate(S61)
C
       allocate(D1(N1+1:N3,N0+1:N2,N2+1:N3,N1+1:N3))
       call relef2314(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N2,N2,N3,N1,N3,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S43(N0+1:N1,N0+1:N2,N2+1:N3,N1+1:N3))
       I1=K3*K4*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S43)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N2,N2,N3,N1,N3,N0,N1,X19,S43,-1.0d0)
       deallocate(S43)
C
       allocate(F2(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N2,N3,N1,N3,N1,N3,N0,N1,N0,N1,t3B,F2)
       allocate(U42(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K1*K3*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,X19,F2,U42)
       deallocate(F2)
C
       call
     & sul235614(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U42,-1.0d0)
       call
     & sul135624(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U42, 1.0d0)
       call
     & sul125634(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U42,-1.0d0)
       call
     & sul234615(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U42, 1.0d0)
       call
     & sul134625(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U42,-1.0d0)
       call
     & sul124635(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U42, 1.0d0)
       call
     & sul234516(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U42,-1.0d0)
       call
     & sul134526(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U42, 1.0d0)
       call
     & sul124536(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U42,-1.0d0)
       deallocate(U42)
       deallocate(X19)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,r2A,D2)
       allocate(Q19(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,D2,Q19)
       deallocate(D1)
       deallocate(D2)
C
       call sul21(N1,N3,N1,N3,X1,Q19,-0.5d0)
       deallocate(Q19)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,r2A,D2)
       allocate(Q20(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,D1,D2,Q20)
       deallocate(D1)
       deallocate(D2)
C
       call sul21(N0,N1,N0,N1,X12,Q20, 0.5d0)
       deallocate(Q20)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N0,N1,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,r2B,D2)
       allocate(S63(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S63)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N0,N1,N1,N3,N0,N1,N0,N1,X3,S63, 1.0d0)
       deallocate(S63)
C
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(U6(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K1*K3*K3
       I3=K1
       call jungemm(I1,I2,I3,X3,D2,U6)
       deallocate(D2)
C
       call
     & sul124356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U6,-1.0d0)
       call
     & sul125346(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U6, 1.0d0)
       call
     & sul124365(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U6, 1.0d0)
       call
     & sul125364(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U6,-1.0d0)
       call
     & sul126345(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U6,-1.0d0)
       call
     & sul126354(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U6, 1.0d0)
       call
     & sul134256(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U6, 1.0d0)
       call
     & sul135246(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U6,-1.0d0)
       call
     & sul134265(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U6,-1.0d0)
       call
     & sul135264(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U6, 1.0d0)
       call
     & sul136245(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U6, 1.0d0)
       call
     & sul136254(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U6,-1.0d0)
       call
     & sul234156(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U6,-1.0d0)
       call
     & sul235146(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U6, 1.0d0)
       call
     & sul234165(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U6, 1.0d0)
       call
     & sul235164(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U6,-1.0d0)
       call
     & sul236145(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U6,-1.0d0)
       call
     & sul236154(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U6, 1.0d0)
       deallocate(U6)
       deallocate(X3)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,r2B,D2)
       allocate(S64(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S64)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N1,N3,N1,N3,N1,N3,N0,N1,X16,S64,-1.0d0)
       deallocate(S64)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,r2B,D2)
       allocate(S65(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S65)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N1,N1,N3,N1,N3,N0,N1,X22,S65, 1.0d0)
C
       allocate(F2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,t3A,F2)
       allocate(U62(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K1*K3*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,X22,F2,U62)
       deallocate(F2)
C
       call
     & sul125634(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U62, 1.0d0)
       call
     & sul135624(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U62,-1.0d0)
       call
     & sul235614(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U62, 1.0d0)
       call
     & sul124635(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U62,-1.0d0)
       call
     & sul134625(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U62, 1.0d0)
       call
     & sul234615(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U62,-1.0d0)
       call
     & sul124536(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U62, 1.0d0)
       call
     & sul134526(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U62,-1.0d0)
       call
     & sul234516(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U62, 1.0d0)
       deallocate(U62)
       deallocate(X22)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3412(N1,N3,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,S65,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S67(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S67)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N1,N3,N1,N3,N1,N3,N0,N1,X16,S67, 1.0d0)
       deallocate(S67)
C
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(U32(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1*K1*K3
       I3=K3
       call jungemm(I1,I2,I3,X16,D2,U32)
       deallocate(D2)
C
       call
     & sul356124(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U32, 1.0d0)
       call
     & sul256134(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U32,-1.0d0)
       call
     & sul356214(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U32,-1.0d0)
       call
     & sul256314(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U32, 1.0d0)
       call
     & sul156234(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U32, 1.0d0)
       call
     & sul156324(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U32,-1.0d0)
       call
     & sul346125(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U32,-1.0d0)
       call
     & sul246135(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U32, 1.0d0)
       call
     & sul346215(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U32, 1.0d0)
       call
     & sul246315(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U32,-1.0d0)
       call
     & sul146235(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U32,-1.0d0)
       call
     & sul146325(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U32, 1.0d0)
       call
     & sul345126(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U32, 1.0d0)
       call
     & sul245136(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U32,-1.0d0)
       call
     & sul345216(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U32,-1.0d0)
       call
     & sul245316(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U32, 1.0d0)
       call
     & sul145236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U32, 1.0d0)
       call
     & sul145326(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U32,-1.0d0)
       deallocate(U32)
       deallocate(X16)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N0,N1,N1,N3,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S37(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S37)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef2341(N0,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,S37,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(S38(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K3*K3
       I3=K1*K1
       call jungemm(I1,I2,I3,D1,D2,S38)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X24(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       X24=0.0d0
       call sul2314(N1,N3,N1,N3,N1,N3,N0,N1,X24,S38, 1.0d0)
       deallocate(S38)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,r2B,D2)
       allocate(S68(N1+1:N3,N0+1:N1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S68)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N2,N2,N3,N1,N3,N0,N1,X23,S68, 1.0d0)
       deallocate(S68)
C
       allocate(F2(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N2,N3,N1,N3,N1,N3,N0,N1,N0,N1,t3B,F2)
       allocate(U65(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K1*K3*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,X23,F2,U65)
       deallocate(F2)
C
       call
     & sul125634(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U65, 1.0d0)
       call
     & sul135624(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U65,-1.0d0)
       call
     & sul235614(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U65, 1.0d0)
       call
     & sul124635(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U65,-1.0d0)
       call
     & sul134625(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U65, 1.0d0)
       call
     & sul234615(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U65,-1.0d0)
       call
     & sul124536(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U65, 1.0d0)
       call
     & sul134526(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U65,-1.0d0)
       call
     & sul234516(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U65, 1.0d0)
       deallocate(U65)
       deallocate(X23)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,r2B,D2)
       allocate(Q21(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,Q21)
       deallocate(D1)
       deallocate(D2)
C
       call sul21(N1,N3,N1,N3,X1,Q21, 1.0d0)
       deallocate(Q21)
C
       allocate(F2(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef123456(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,t3A,F2)
       allocate(U1(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1*K1*K1*K3*K3
       I3=K3
       call jungemm(I1,I2,I3,X1,F2,U1)
       deallocate(F2)
C
       call
     & sul124563(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U1,-1.0d0)
       call
     & sul134562(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U1, 1.0d0)
       call
     & sul234561(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U1,-1.0d0)
       deallocate(U1)
       deallocate(X1)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,r2B,D2)
       allocate(Q22(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,D1,D2,Q22)
       deallocate(D1)
       deallocate(D2)
C
       call sul21(N0,N1,N0,N1,X12,Q22, 1.0d0)
       deallocate(Q22)
C
       allocate(F2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,t3A,F2)
       allocate(U24(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K1*K3*K3*K3
       I3=K1
       call jungemm(I1,I2,I3,X12,F2,U24)
       deallocate(F2)
C
       call
     & sul123564(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U24,-1.0d0)
       call
     & sul123465(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U24, 1.0d0)
       V3A=V3A-U24
       deallocate(U24)
       deallocate(X12)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,r3A,F2)
       allocate(S69(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1*K3*K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,F2,S69)
       deallocate(D1)
       deallocate(F2)
C
       call sul2341(N1,N3,N1,N3,N1,N3,N0,N1,X24,S69,-1.0d0)
       deallocate(S69)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef4123(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(F2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,r3A,F2)
       allocate(S70(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K1*K3
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,D1,F2,S70)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X25(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       X25=0.0d0
       call sul2341(N0,N1,N1,N3,N0,N1,N0,N1,X25,S70, 1.0d0)
       deallocate(S70)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,r3B,F2)
       allocate(S71(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1*K3*K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,F2,S71)
       deallocate(D1)
       deallocate(F2)
C
       call sul2341(N1,N3,N1,N3,N1,N3,N0,N1,X24,S71, 2.0d0)
       deallocate(S71)
C
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(U36(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1*K1*K3
       I3=K3
       call jungemm(I1,I2,I3,X24,D2,U36)
       deallocate(D2)
C
       call
     & sul156234(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U36, 0.5d0)
       call
     & sul256134(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U36,-0.5d0)
       call
     & sul356124(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U36, 0.5d0)
       call
     & sul146235(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U36,-0.5d0)
       call
     & sul246135(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U36, 0.5d0)
       call
     & sul346125(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U36,-0.5d0)
       call
     & sul145236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U36, 0.5d0)
       call
     & sul245136(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U36,-0.5d0)
       call
     & sul345126(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U36, 0.5d0)
       deallocate(U36)
       deallocate(X24)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N2,N3,N1,N3,N1,N3,N0,N1,N0,N1,r3B,F2)
       allocate(S72(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K1*K3
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,D1,F2,S72)
       deallocate(D1)
       deallocate(F2)
C
       call sul2341(N0,N1,N1,N3,N0,N1,N0,N1,X25,S72,-2.0d0)
       deallocate(S72)
C
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(U87(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K1*K3*K3
       I3=K1
       call jungemm(I1,I2,I3,X25,D2,U87)
       deallocate(D2)
C
       call
     & sul126345(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U87,-0.5d0)
       call
     & sul136245(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U87, 0.5d0)
       call
     & sul236145(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U87,-0.5d0)
       call
     & sul125346(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U87, 0.5d0)
       call
     & sul135246(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U87,-0.5d0)
       call
     & sul235146(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U87, 0.5d0)
       call
     & sul124356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U87,-0.5d0)
       call
     & sul134256(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U87, 0.5d0)
       call
     & sul234156(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U87,-0.5d0)
       deallocate(U87)
       deallocate(X25)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N0,N1,H2A,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,r2A,D2)
       allocate(U55(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K1*K3*K3
       I3=K1
       call jungemm(I1,I2,I3,D1,D2,U55)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul234156(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U55, 1.0d0)
       call
     & sul134256(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U55,-1.0d0)
       call
     & sul124356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U55, 1.0d0)
       call
     & sul235146(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U55,-1.0d0)
       call
     & sul135246(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U55, 1.0d0)
       call
     & sul125346(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U55,-1.0d0)
       call
     & sul236145(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U55, 1.0d0)
       call
     & sul136245(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U55,-1.0d0)
       call
     & sul126345(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U55, 1.0d0)
       deallocate(U55)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N1,N3,N0,N1,H2A,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,r2A,D2)
       allocate(U56(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1*K1*K3
       I3=K3
       call jungemm(I1,I2,I3,D1,D2,U56)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul345126(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U56,-1.0d0)
       call
     & sul245136(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U56, 1.0d0)
       call
     & sul145236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U56,-1.0d0)
       call
     & sul346125(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U56, 1.0d0)
       call
     & sul246135(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U56,-1.0d0)
       call
     & sul146235(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U56, 1.0d0)
       call
     & sul356124(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U56,-1.0d0)
       call
     & sul256134(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U56, 1.0d0)
       call
     & sul156234(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U56,-1.0d0)
       deallocate(U56)
C
       allocate(B1(N0+1:N1,N0+1:N1))
       call relef21(N0,N3,N0,N3,
     & N0,N1,N0,N1,H1A,B1)
       allocate(F2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,r3A,F2)
       allocate(U81(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K1*K3*K3*K3
       I3=K1
       call jungemm(I1,I2,I3,B1,F2,U81)
       deallocate(B1)
       deallocate(F2)
C
       V3A=V3A-U81
       call
     & sul123465(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U81, 1.0d0)
       call
     & sul123564(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U81,-1.0d0)
       deallocate(U81)
C
       allocate(B1(N1+1:N3,N1+1:N3))
       call relef12(N0,N3,N0,N3,
     & N1,N3,N1,N3,H1A,B1)
       allocate(F2(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef123456(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,r3A,F2)
       allocate(U82(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1*K1*K1*K3*K3
       I3=K3
       call jungemm(I1,I2,I3,B1,F2,U82)
       deallocate(B1)
       deallocate(F2)
C
       call
     & sul234561(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U82, 1.0d0)
       call
     & sul134562(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U82,-1.0d0)
       call
     & sul124563(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U82, 1.0d0)
       deallocate(U82)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N0,N1,N0,N1,H2A,D1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,r3A,F2)
       allocate(U83(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K3*K3*K3
       I3=K1*K1
       call jungemm(I1,I2,I3,D1,F2,U83)
       deallocate(D1)
       deallocate(F2)
C
       V3A=V3A+0.5d0*U83
       call
     & sul123546(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U83,-0.5d0)
       call
     & sul123645(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U83, 0.5d0)
       deallocate(U83)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,H2A,D1)
       allocate(F2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,r3A,F2)
       allocate(U84(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K1*K3*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,F2,U84)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul234516(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U84, 1.0d0)
       call
     & sul134526(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U84,-1.0d0)
       call
     & sul124536(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U84, 1.0d0)
       call
     & sul234615(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U84,-1.0d0)
       call
     & sul134625(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U84, 1.0d0)
       call
     & sul124635(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U84,-1.0d0)
       call
     & sul235614(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U84, 1.0d0)
       call
     & sul135624(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U84,-1.0d0)
       call
     & sul125634(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U84, 1.0d0)
       deallocate(U84)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N1,N3,N1,N3,H2A,D1)
       allocate(F2(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef123456(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,r3A,F2)
       allocate(U85(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K1*K1*K1*K3
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,F2,U85)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul345612(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U85, 0.5d0)
       call
     & sul245613(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U85,-0.5d0)
       call
     & sul145623(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U85, 0.5d0)
       deallocate(U85)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,H2B,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N2,N3,N1,N3,N1,N3,N0,N1,N0,N1,r3B,F2)
       allocate(U88(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K1*K3*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,F2,U88)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul234516(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U88, 1.0d0)
       call
     & sul134526(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U88,-1.0d0)
       call
     & sul124536(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U88, 1.0d0)
       call
     & sul234615(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U88,-1.0d0)
       call
     & sul134625(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U88, 1.0d0)
       call
     & sul124635(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U88,-1.0d0)
       call
     & sul235614(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U88, 1.0d0)
       call
     & sul135624(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U88,-1.0d0)
       call
     & sul125634(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,V3A,U88, 1.0d0)
       deallocate(U88)
C
       end
