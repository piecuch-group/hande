       subroutine L2A_update(N0,N1,N2,N3,V2A,
     & K1,K2,K3,K4,
     & FockR,FockB,IntR,IntB,IntM,
     & H1A,H1B,H2A,H2B,H2C,
     & t1A,t1B,t2A,t2B,t2C,t3A,t3B,t3C,t3D,
     & l1A,l1B,l2A,l2B,l2C,l3A,l3B,l3C,l3D)
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
       real*8 l1A(N1+1:N3,N0+1:N1)
       real*8 l1B(N2+1:N3,N0+1:N2)
       real*8 l2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 l2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 l2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
       real*8 l3A(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)
       real*8 l3B(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1)
       real*8 l3C(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1)
       real*8 l3D(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2)
C
       real*8 V2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
C
       real*8,allocatable::B1(:,:)
       real*8,allocatable::B2(:,:)
       real*8,allocatable::C1(:,:,:)
       real*8,allocatable::C2(:,:,:)
       real*8,allocatable::D1(:,:,:,:)
       real*8,allocatable::D2(:,:,:,:)
       real*8,allocatable::F1(:,:,:,:,:,:)
       real*8,allocatable::F2(:,:,:,:,:,:)
C
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::Q3(:,:)
       real*8,allocatable::Q4(:,:)
       real*8,allocatable::S15(:,:,:,:)
       real*8,allocatable::S17(:,:,:,:)
       real*8,allocatable::S19(:,:,:,:)
       real*8,allocatable::S20(:,:,:,:)
       real*8,allocatable::S47(:,:,:,:)
       real*8,allocatable::S22(:,:,:,:)
       real*8,allocatable::S24(:,:,:,:)
       real*8,allocatable::S26(:,:,:,:)
       real*8,allocatable::S29(:,:,:,:)
       real*8,allocatable::S28(:,:,:,:)
       real*8,allocatable::S52(:,:,:,:)
       real*8,allocatable::S31(:,:,:,:)
       real*8,allocatable::S32(:,:,:,:)
       real*8,allocatable::S34(:,:,:,:)
       real*8,allocatable::S36(:,:,:,:)
       real*8,allocatable::S38(:,:,:,:)
       real*8,allocatable::S40(:,:,:,:)
       real*8,allocatable::S41(:,:,:,:)
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::Q6(:,:)
       real*8,allocatable::S49(:,:,:,:)
       real*8,allocatable::S54(:,:,:,:)
       real*8,allocatable::S59(:,:,:,:)
       real*8,allocatable::S56(:,:,:,:)
       real*8,allocatable::S61(:,:,:,:)
       real*8,allocatable::S67(:,:,:,:)
       real*8,allocatable::S87(:,:,:,:)
       real*8,allocatable::S63(:,:,:,:)
       real*8,allocatable::S69(:,:,:,:)
       real*8,allocatable::S65(:,:,:,:)
       real*8,allocatable::S71(:,:,:,:)
       real*8,allocatable::S75(:,:,:,:)
       real*8,allocatable::S73(:,:,:,:)
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::Q8(:,:)
       real*8,allocatable::S79(:,:,:,:)
       real*8,allocatable::S81(:,:,:,:)
       real*8,allocatable::S83(:,:,:,:)
       real*8,allocatable::S85(:,:,:,:)
       real*8,allocatable::S89(:,:,:,:)
       real*8,allocatable::S90(:,:,:,:)
       real*8,allocatable::Q9(:,:)
       real*8,allocatable::Q10(:,:)
       real*8,allocatable::U1(:,:,:,:)
       real*8,allocatable::U2(:,:,:,:)
       real*8,allocatable::U3(:,:,:,:)
       real*8,allocatable::U4(:,:,:,:)
       real*8,allocatable::U5(:,:,:,:)
       real*8,allocatable::U6(:,:,:,:)
       real*8,allocatable::U7(:,:,:,:)
       real*8,allocatable::U8(:,:,:,:)
       real*8,allocatable::U9(:,:,:,:)
       real*8,allocatable::U10(:,:,:,:)
       real*8,allocatable::U11(:,:,:,:)
       real*8,allocatable::U12(:,:,:,:)
       real*8,allocatable::U13(:,:,:,:)
       real*8,allocatable::U14(:,:,:,:)
       real*8,allocatable::U16(:,:,:,:)
       real*8,allocatable::U18(:,:,:,:)
       real*8,allocatable::U21(:,:,:,:)
       real*8,allocatable::X1(:,:,:,:)
       real*8,allocatable::U51(:,:,:,:)
       real*8,allocatable::U23(:,:,:,:)
       real*8,allocatable::U25(:,:,:,:)
       real*8,allocatable::U27(:,:,:,:)
       real*8,allocatable::U30(:,:,:,:)
       real*8,allocatable::X2(:,:,:,:)
       real*8,allocatable::U58(:,:,:,:)
       real*8,allocatable::U33(:,:,:,:)
       real*8,allocatable::U35(:,:,:,:)
       real*8,allocatable::U37(:,:,:,:)
       real*8,allocatable::U39(:,:,:,:)
       real*8,allocatable::U42(:,:,:,:)
       real*8,allocatable::U43(:,:,:,:)
       real*8,allocatable::U44(:,:,:,:)
       real*8,allocatable::U45(:,:,:,:)
       real*8,allocatable::U46(:,:,:,:)
       real*8,allocatable::U50(:,:,:,:)
       real*8,allocatable::U55(:,:,:,:)
       real*8,allocatable::U60(:,:,:,:)
       real*8,allocatable::U57(:,:,:,:)
       real*8,allocatable::X3(:,:,:,:)
       real*8,allocatable::U62(:,:,:,:)
       real*8,allocatable::U88(:,:,:,:)
       real*8,allocatable::U64(:,:,:,:)
       real*8,allocatable::U70(:,:,:,:)
       real*8,allocatable::U66(:,:,:,:)
       real*8,allocatable::U72(:,:,:,:)
       real*8,allocatable::U76(:,:,:,:)
       real*8,allocatable::U74(:,:,:,:)
       real*8,allocatable::U77(:,:,:,:)
       real*8,allocatable::U78(:,:,:,:)
       real*8,allocatable::U80(:,:,:,:)
       real*8,allocatable::U82(:,:,:,:)
       real*8,allocatable::U84(:,:,:,:)
       real*8,allocatable::U86(:,:,:,:)
       real*8,allocatable::U91(:,:,:,:)
       real*8,allocatable::U92(:,:,:,:)
       real*8,allocatable::U93(:,:,:,:)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,l2A,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(Q1(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,D2,Q1)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N0,N1,N0,N1,IntR,D1)
       allocate(U8(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K3
       I3=K3
       call jungemm(I1,I2,I3,D1,Q1,U8)
       deallocate(D1)
C
       call
     & sul2134(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U8,1.0d0/2)
       V2A=V2A-1.0d0/2*U8
       deallocate(U8)
       deallocate(Q1)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,l2A,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(Q2(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,D1,D2,Q2)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(U9(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1
       I3=K1
       call jungemm(I1,I2,I3,D1,Q2,U9)
       deallocate(D1)
C
       call
     & sul3124(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U9,-1.0d0/2)
       call
     & sul4123(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U9,1.0d0/2)
       deallocate(U9)
       deallocate(Q2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,l2B,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2B,D2)
       allocate(Q3(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,Q3)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N0,N1,N0,N1,IntR,D1)
       allocate(U11(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K3
       I3=K3
       call jungemm(I1,I2,I3,D1,Q3,U11)
       deallocate(D1)
C
       call
     & sul2134(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U11,1.0d0)
       V2A=V2A-U11
       deallocate(U11)
       deallocate(Q3)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,l2B,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(Q4(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,D1,D2,Q4)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(U12(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1
       I3=K1
       call jungemm(I1,I2,I3,D1,Q4,U12)
       deallocate(D1)
C
       call
     & sul3124(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U12,-1.0d0)
       call
     & sul4123(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U12,1.0d0)
       deallocate(U12)
       deallocate(Q4)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef451623(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,N1,N1,N3,N1,N3,l3A,F1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(S15(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3*K1
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,F1,D2,S15)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N0,N1,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef2134(N1,N3,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,S15,D2)
       allocate(U16(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K3*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,U16)
       deallocate(D1)
       deallocate(D2)
C
       V2A=V2A-1.0d0/2*U16
       deallocate(U16)
       deallocate(S15)
C
       allocate(F1(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,l3A,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,t3A,F2)
       allocate(S17(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K3*K3
       I3=K3*K1*K1*K1
       call jungemm(I1,I2,I3,F1,F2,S17)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N0,N1,N0,N1,IntR,D1)
       allocate(U18(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K3*K3
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,S17,U18)
       deallocate(D1)
C
       V2A=V2A+1.0d0/12*U18
       deallocate(U18)
       deallocate(S17)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef2134(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N0,N1,N0,N1,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S19(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S19)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       X1=0.0d0
       X1=X1+S19
C
       call slx1234(N0,N3,N0,N1,N1,N3,N0,N1,N0,N1,X1,IntR,-1.000d0)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef451623(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,N1,N1,N3,N1,N3,l3A,F1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(S20(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3*K1
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,F1,D2,S20)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef2134(N1,N3,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,S20,D2)
       allocate(U21(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K3*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,S19,D2,U21)
       deallocate(D2)
C
       V2A=V2A+1.0d0/2*U21
       deallocate(U21)
       deallocate(S20)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef451623(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,N1,N1,N3,N1,N3,l3B,F1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2B,D2)
       allocate(S47(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3*K1
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,F1,D2,S47)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef2134(N1,N3,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,S47,D2)
       allocate(U51(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K3*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,X1,D2,U51)
       deallocate(D2)
C
       V2A=V2A+U51
       deallocate(U51)
       deallocate(X1)
       deallocate(S47)
C
       allocate(F1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef412536(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,N1,N3,N0,N1,l3A,F1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S22(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,F1,D2,S22)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef2134(N0,N1,N0,N1,N1,N3,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,N1,S22,D2)
       allocate(U23(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K3
       I3=K1*K1
       call jungemm(I1,I2,I3,D1,D2,U23)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul2314(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U23,1.0d0/2)
       call
     & sul1324(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U23,-1.0d0/2)
       call
     & sul2413(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U23,-1.0d0/2)
       call
     & sul1423(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U23,1.0d0/2)
       deallocate(U23)
       deallocate(S22)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,l3A,F1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(S24(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,F1,D2,S24)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(U25(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K3
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,S24,U25)
       deallocate(D1)
C
       call
     & sul2314(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U25,-1.0d0/2)
       call
     & sul1324(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U25,1.0d0/2)
       call
     & sul2413(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U25,1.0d0/2)
       call
     & sul1423(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U25,-1.0d0/2)
       deallocate(U25)
       deallocate(S24)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,l3A,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N0,N1,N1,N3,t3A,F2)
       allocate(S26(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K3*K1
       I3=K3*K3*K1*K1
       call jungemm(I1,I2,I3,F1,F2,S26)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(U27(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,S26,U27)
       deallocate(D1)
C
       call
     & sul2314(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U27,-1.0d0/4)
       call
     & sul1324(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U27,1.0d0/4)
       call
     & sul2413(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U27,1.0d0/4)
       call
     & sul1423(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U27,-1.0d0/4)
       deallocate(U27)
       deallocate(S26)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S29(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S29)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X2(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       X2=0.0d0
       X2=X2+S29
C
       call slx1324(N0,N3,N0,N1,N0,N1,N1,N3,N0,N1,X2,IntR,1.000d0)
C
       allocate(F1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef412536(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,N1,N3,N0,N1,l3A,F1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S28(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,F1,D2,S28)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef2134(N0,N1,N0,N1,N1,N3,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,N1,S28,D2)
       allocate(U30(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K3
       I3=K1*K1
       call jungemm(I1,I2,I3,S29,D2,U30)
       deallocate(D2)
C
       call
     & sul2314(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U30,1.0d0/2)
       call
     & sul1324(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U30,-1.0d0/2)
       call
     & sul2413(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U30,-1.0d0/2)
       call
     & sul1423(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U30,1.0d0/2)
       deallocate(U30)
       deallocate(S28)
C
       allocate(F1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef412536(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,N1,N3,N0,N1,l3B,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(S52(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S52)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef2134(N0,N1,N0,N1,N1,N3,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,N1,S52,D2)
       allocate(U58(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K3
       I3=K1*K1
       call jungemm(I1,I2,I3,X2,D2,U58)
       deallocate(D2)
C
       call
     & sul2314(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U58,1.0d0)
       call
     & sul1324(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U58,-1.0d0)
       call
     & sul2413(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U58,-1.0d0)
       call
     & sul1423(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U58,1.0d0)
       deallocate(U58)
       deallocate(X2)
       deallocate(S52)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,l3A,F1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(S31(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,F1,D2,S31)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef2134(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N1,N3,N1,N3,N0,N1,S31,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S32(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S32)
       deallocate(D1)
       deallocate(B2)
       deallocate(S31)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(U33(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,S32,U33)
       deallocate(D1)
C
       call
     & sul2314(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U33,1.0d0/2)
       call
     & sul1324(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U33,-1.0d0/2)
       call
     & sul2413(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U33,-1.0d0/2)
       call
     & sul1423(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U33,1.0d0/2)
       deallocate(U33)
       deallocate(S32)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,l3A,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef562341(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N0,N2,N2,N3,t3B,F2)
       allocate(S34(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K4*K2
       I3=K3*K3*K1*K1
       call jungemm(I1,I2,I3,F1,F2,S34)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(U35(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,S34,U35)
       deallocate(D1)
C
       call
     & sul2314(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U35,-1.0d0/4)
       call
     & sul1324(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U35,1.0d0/4)
       call
     & sul2413(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U35,1.0d0/4)
       call
     & sul1423(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U35,-1.0d0/4)
       deallocate(U35)
       deallocate(S34)
C
       allocate(F1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,l3A,F1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S36(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,F1,D2,S36)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,IntR,D1)
       allocate(U37(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K1*K1
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,S36,U37)
       deallocate(D1)
C
       call
     & sul3412(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U37,-1.0d0/2)
       deallocate(U37)
       deallocate(S36)
C
       allocate(F1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,l3A,F1)
       allocate(F2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,t3A,F2)
       allocate(S38(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K1
       I3=K3*K3*K3*K1
       call jungemm(I1,I2,I3,F1,F2,S38)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(U39(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K1*K1
       I3=K1*K1
       call jungemm(I1,I2,I3,D1,S38,U39)
       deallocate(D1)
C
       call
     & sul3412(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U39,1.0d0/12)
       deallocate(U39)
       deallocate(S38)
C
       allocate(F1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,l3A,F1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S40(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,F1,D2,S40)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef2134(N0,N1,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,N1,N0,N1,N0,N1,S40,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S41(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S41)
       deallocate(D1)
       deallocate(B2)
       deallocate(S40)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef2134(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,S41,D2)
       allocate(U42(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K1*K1
       I3=K1*K1
       call jungemm(I1,I2,I3,D1,D2,U42)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul3412(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U42,1.0d0/2)
       deallocate(U42)
       deallocate(S41)
C
       allocate(F1(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,l3A,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,t3A,F2)
       allocate(Q5(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K3*K3*K1*K1*K1
       call jungemm(I1,I2,I3,F1,F2,Q5)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N0,N1,N0,N1,IntR,D1)
       allocate(U43(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K3
       I3=K3
       call jungemm(I1,I2,I3,D1,Q5,U43)
       deallocate(D1)
C
       call
     & sul2134(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U43,1.0d0/12)
       V2A=V2A-1.0d0/12*U43
       deallocate(U43)
       deallocate(Q5)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,l3A,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,t3A,F2)
       allocate(Q6(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K3*K3*K1*K1
       call jungemm(I1,I2,I3,F1,F2,Q6)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(U44(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1
       I3=K1
       call jungemm(I1,I2,I3,D1,Q6,U44)
       deallocate(D1)
C
       call
     & sul3124(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U44,-1.0d0/12)
       call
     & sul4123(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U44,1.0d0/12)
       deallocate(U44)
       deallocate(Q6)
C
       allocate(F1(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,t3B,F2)
       allocate(S49(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K3*K3
       I3=K4*K1*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S49)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N0,N1,N0,N1,IntR,D1)
       allocate(U50(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K3*K3
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,S49,U50)
       deallocate(D1)
C
       V2A=V2A+1.0d0/4*U50
       deallocate(U50)
       deallocate(S49)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,l3B,F1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2B,D2)
       allocate(S54(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,F1,D2,S54)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(U55(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K3
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,S54,U55)
       deallocate(D1)
C
       call
     & sul2314(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U55,-1.0d0)
       call
     & sul1324(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U55,1.0d0)
       call
     & sul2413(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U55,1.0d0)
       call
     & sul1423(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U55,-1.0d0)
       deallocate(U55)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef2134(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N1,N3,N1,N3,N0,N1,S54,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S59(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S59)
       deallocate(D1)
       deallocate(B2)
       deallocate(S54)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(U60(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,S59,U60)
       deallocate(D1)
C
       call
     & sul2314(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U60,1.0d0)
       call
     & sul1324(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U60,-1.0d0)
       call
     & sul2413(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U60,-1.0d0)
       call
     & sul1423(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U60,1.0d0)
       deallocate(U60)
       deallocate(S59)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N0,N1,N1,N3,t3B,F2)
       allocate(S56(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K3*K1
       I3=K3*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S56)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(U57(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,S56,U57)
       deallocate(D1)
C
       call
     & sul2314(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U57,-1.0d0)
       call
     & sul1324(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U57,1.0d0)
       call
     & sul2413(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U57,1.0d0)
       call
     & sul1423(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U57,-1.0d0)
       deallocate(U57)
       deallocate(S56)
C
       allocate(F1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef512436(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,N1,N3,N0,N1,l3B,F1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(S61(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,F1,D2,S61)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N1,N3,N0,N1,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S67(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S67)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       X3=0.0d0
       call sul2134(N0,N2,N0,N2,N1,N3,N0,N1,X3,S67,1.000d0)
C
       call slx2314(N0,N3,N0,N2,N0,N2,N1,N3,N0,N1,X3,IntM,1.000d0)
C
       allocate(D1(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef2134(N0,N2,N0,N2,N1,N3,N0,N1,
     & N0,N2,N0,N2,N1,N3,N0,N1,X3,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef2134(N0,N2,N0,N2,N1,N3,N0,N1,
     & N0,N2,N0,N2,N1,N3,N0,N1,S61,D2)
       allocate(U62(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K3
       I3=K2*K2
       call jungemm(I1,I2,I3,D1,D2,U62)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul2314(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U62,1.0d0)
       call
     & sul1324(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U62,-1.0d0)
       call
     & sul2413(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U62,-1.0d0)
       call
     & sul1423(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U62,1.0d0)
       deallocate(U62)
       deallocate(S61)
       deallocate(X3)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef412536(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N0,N2,N1,N3,N0,N1,l3C,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S87(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S87)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D2(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef2134(N0,N2,N0,N2,N1,N3,N0,N1,
     & N0,N2,N0,N2,N1,N3,N0,N1,S87,D2)
       allocate(U88(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K3
       I3=K2*K2
       call jungemm(I1,I2,I3,S67,D2,U88)
       deallocate(D2)
C
       call
     & sul2314(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U88,1.0d0/2)
       call
     & sul1324(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U88,-1.0d0/2)
       call
     & sul2413(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U88,-1.0d0/2)
       call
     & sul1423(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U88,1.0d0/2)
       deallocate(U88)
       deallocate(S87)
C
       allocate(F1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef452136(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,N1,N3,N0,N1,l3B,F1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,t2B,D2)
       allocate(S63(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,F1,D2,S63)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef2134(N2,N3,N2,N3,N1,N3,N0,N1,
     & N2,N3,N2,N3,N1,N3,N0,N1,S63,D2)
       allocate(U64(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K3
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,D2,U64)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul2314(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U64,-1.0d0)
       call
     & sul1324(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U64,1.0d0)
       call
     & sul2413(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U64,1.0d0)
       call
     & sul1423(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U64,-1.0d0)
       deallocate(U64)
C
       allocate(D1(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef2134(N2,N3,N2,N3,N1,N3,N0,N1,
     & N2,N3,N2,N3,N1,N3,N0,N1,S63,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S69(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S69)
       deallocate(D1)
       deallocate(B2)
       deallocate(S63)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(U70(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,S69,U70)
       deallocate(D1)
C
       call
     & sul2314(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U70,1.0d0)
       call
     & sul1324(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U70,-1.0d0)
       call
     & sul2413(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U70,-1.0d0)
       call
     & sul1423(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U70,1.0d0)
       deallocate(U70)
       deallocate(S69)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef461352(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N0,N2,N2,N3,t3C,F2)
       allocate(S65(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K4*K2
       I3=K3*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S65)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(U66(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,S65,U66)
       deallocate(D1)
C
       call
     & sul2314(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U66,-1.0d0)
       call
     & sul1324(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U66,1.0d0)
       call
     & sul2413(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U66,1.0d0)
       call
     & sul1423(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U66,-1.0d0)
       deallocate(U66)
       deallocate(S65)
C
       allocate(F1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N2,N3,N1,N3,N1,N3,N0,N1,N0,N1,l3B,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(S71(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K1
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S71)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,IntR,D1)
       allocate(U72(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K1*K1
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,S71,U72)
       deallocate(D1)
C
       call
     & sul3412(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U72,-1.0d0)
       deallocate(U72)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef2134(N0,N1,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,N1,N0,N1,N0,N1,S71,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S75(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S75)
       deallocate(D1)
       deallocate(B2)
       deallocate(S71)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef2134(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,S75,D2)
       allocate(U76(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K1*K1
       I3=K1*K1
       call jungemm(I1,I2,I3,D1,D2,U76)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul3412(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U76,1.0d0)
       deallocate(U76)
       deallocate(S75)
C
       allocate(F1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N2,N3,N1,N3,N1,N3,N0,N1,N0,N1,l3B,F1)
       allocate(F2(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N2,N3,N1,N3,N1,N3,N0,N1,N0,N1,t3B,F2)
       allocate(S73(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K1
       I3=K3*K3*K4*K2
       call jungemm(I1,I2,I3,F1,F2,S73)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(U74(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K1*K1
       I3=K1*K1
       call jungemm(I1,I2,I3,D1,S73,U74)
       deallocate(D1)
C
       call
     & sul3412(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U74,1.0d0/4)
       deallocate(U74)
       deallocate(S73)
C
       allocate(F1(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,t3B,F2)
       allocate(Q7(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K3*K4*K1*K1*K2
       call jungemm(I1,I2,I3,F1,F2,Q7)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N0,N1,N0,N1,IntR,D1)
       allocate(U77(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K3
       I3=K3
       call jungemm(I1,I2,I3,D1,Q7,U77)
       deallocate(D1)
C
       call
     & sul2134(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U77,1.0d0/2)
       V2A=V2A-1.0d0/2*U77
       deallocate(U77)
       deallocate(Q7)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,t3B,F2)
       allocate(Q8(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K3*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,Q8)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(U78(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1
       I3=K1
       call jungemm(I1,I2,I3,D1,Q8,U78)
       deallocate(D1)
C
       call
     & sul3124(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U78,-1.0d0/2)
       call
     & sul4123(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U78,1.0d0/2)
       deallocate(U78)
       deallocate(Q8)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N0,N1,N1,N3,t3C,F2)
       allocate(S79(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K3*K1
       I3=K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S79)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(U80(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,S79,U80)
       deallocate(D1)
C
       call
     & sul2314(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U80,-1.0d0/4)
       call
     & sul1324(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U80,1.0d0/4)
       call
     & sul2413(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U80,1.0d0/4)
       call
     & sul1423(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U80,-1.0d0/4)
       deallocate(U80)
       deallocate(S79)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef412536(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N0,N2,N1,N3,N0,N1,l3C,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S81(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S81)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N1,N3,N0,N1,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef2134(N0,N2,N0,N2,N1,N3,N0,N1,
     & N0,N2,N0,N2,N1,N3,N0,N1,S81,D2)
       allocate(U82(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K3
       I3=K2*K2
       call jungemm(I1,I2,I3,D1,D2,U82)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul2314(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U82,1.0d0/2)
       call
     & sul1324(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U82,-1.0d0/2)
       call
     & sul2413(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U82,-1.0d0/2)
       call
     & sul1423(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U82,1.0d0/2)
       deallocate(U82)
       deallocate(S81)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(S83(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,F1,D2,S83)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(U84(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K3
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,S83,U84)
       deallocate(D1)
C
       call
     & sul2314(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U84,-1.0d0/2)
       call
     & sul1324(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U84,1.0d0/2)
       call
     & sul2413(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U84,1.0d0/2)
       call
     & sul1423(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U84,-1.0d0/2)
       deallocate(U84)
       deallocate(S83)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef451263(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N0,N2,N2,N3,t3D,F2)
       allocate(S85(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K4*K2
       I3=K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S85)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(U86(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,S85,U86)
       deallocate(D1)
C
       call
     & sul2314(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U86,-1.0d0/4)
       call
     & sul1324(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U86,1.0d0/4)
       call
     & sul2413(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U86,1.0d0/4)
       call
     & sul1423(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U86,-1.0d0/4)
       deallocate(U86)
       deallocate(S85)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(S89(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,F1,D2,S89)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef2134(N2,N3,N2,N3,N1,N3,N0,N1,
     & N2,N3,N2,N3,N1,N3,N0,N1,S89,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S90(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S90)
       deallocate(D1)
       deallocate(B2)
       deallocate(S89)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(U91(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,S90,U91)
       deallocate(D1)
C
       call
     & sul2314(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U91,1.0d0/2)
       call
     & sul1324(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U91,-1.0d0/2)
       call
     & sul2413(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U91,-1.0d0/2)
       call
     & sul1423(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U91,1.0d0/2)
       deallocate(U91)
       deallocate(S90)
C
       allocate(F1(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef456123(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef456123(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,t3C,F2)
       allocate(Q9(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K4*K4*K1*K2*K2
       call jungemm(I1,I2,I3,F1,F2,Q9)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N0,N1,N0,N1,IntR,D1)
       allocate(U92(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K3
       I3=K3
       call jungemm(I1,I2,I3,D1,Q9,U92)
       deallocate(D1)
C
       call
     & sul2134(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U92,1.0d0/4)
       V2A=V2A-1.0d0/4*U92
       deallocate(U92)
       deallocate(Q9)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,t3C,F2)
       allocate(Q10(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,Q10)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(U93(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1
       I3=K1
       call jungemm(I1,I2,I3,D1,Q10,U93)
       deallocate(D1)
C
       call
     & sul3124(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U93,-1.0d0/4)
       call
     & sul4123(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U93,1.0d0/4)
       deallocate(U93)
       deallocate(Q10)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N0,N1,H2A,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,l1A,B2)
       allocate(U1(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,U1)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sul2134(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U1,1.0d0)
       V2A=V2A-U1
       deallocate(U1)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N1,N3,N0,N1,H2A,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,l1A,B2)
       allocate(U2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,U2)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sul3124(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U2,1.0d0)
       call
     & sul4123(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U2,-1.0d0)
       deallocate(U2)
C
       allocate(B1(N0+1:N1,N0+1:N1))
       call relef12(N0,N3,N0,N3,
     & N0,N1,N0,N1,H1A,B1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,l2A,D2)
       allocate(U3(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K3*K3
       I3=K1
       call jungemm(I1,I2,I3,B1,D2,U3)
       deallocate(B1)
       deallocate(D2)
C
       V2A=V2A+U3
       call
     & sul1243(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U3,-1.0d0)
       deallocate(U3)
C
       allocate(B1(N1+1:N3,N1+1:N3))
       call relef21(N0,N3,N0,N3,
     & N1,N3,N1,N3,H1A,B1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,l2A,D2)
       allocate(U4(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1*K1*K3
       I3=K3
       call jungemm(I1,I2,I3,B1,D2,U4)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sul2341(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U4,1.0d0)
       call
     & sul1342(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U4,-1.0d0)
       deallocate(U4)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N0,N1,N0,N1,H2A,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,l2A,D2)
       allocate(U5(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K3*K3
       I3=K1*K1
       call jungemm(I1,I2,I3,D1,D2,U5)
       deallocate(D1)
       deallocate(D2)
C
       V2A=V2A+1.0d0/2*U5
       deallocate(U5)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,H2A,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,l2A,D2)
       allocate(U6(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,U6)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul2314(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U6,-1.0d0)
       call
     & sul1324(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U6,1.0d0)
       call
     & sul2413(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U6,1.0d0)
       call
     & sul1423(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U6,-1.0d0)
       deallocate(U6)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N1,N3,N1,N3,H2A,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,l2A,D2)
       allocate(U7(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K1*K1
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,D2,U7)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul3412(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U7,1.0d0/2)
       deallocate(U7)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,H2B,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,l2B,D2)
       allocate(U10(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,U10)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul2314(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U10,-1.0d0)
       call
     & sul1324(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U10,1.0d0)
       call
     & sul2413(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U10,1.0d0)
       call
     & sul1423(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U10,-1.0d0)
       deallocate(U10)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,H2A,D1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,l3A,F2)
       allocate(U13(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K3*K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,F2,U13)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A+1.0d0/2*U13
       call
     & sul1243(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U13,-1.0d0/2)
       deallocate(U13)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,H2A,D1)
       allocate(F2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,l3A,F2)
       allocate(U14(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1*K1*K3
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,D1,F2,U14)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul2341(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U14,1.0d0/2)
       call
     & sul1342(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U14,-1.0d0/2)
       deallocate(U14)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N0,N1,H2B,D1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,l3B,F2)
       allocate(U45(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K3*K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,F2,U45)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A+U45
       call
     & sul1243(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U45,-1.0d0)
       deallocate(U45)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,H2B,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N2,N3,N1,N3,N1,N3,N0,N1,N0,N1,l3B,F2)
       allocate(U46(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1*K1*K3
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,D1,F2,U46)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul2341(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U46,1.0d0)
       call
     & sul1342(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U46,-1.0d0)
       deallocate(U46)
C
       end
