       subroutine L2C_update(N0,N1,N2,N3,V2C,
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
       real*8 V2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
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
       real*8,allocatable::S13(:,:,:,:)
       real*8,allocatable::S15(:,:,:,:)
       real*8,allocatable::S17(:,:,:,:)
       real*8,allocatable::S20(:,:,:,:)
       real*8,allocatable::S19(:,:,:,:)
       real*8,allocatable::S37(:,:,:,:)
       real*8,allocatable::S22(:,:,:,:)
       real*8,allocatable::S23(:,:,:,:)
       real*8,allocatable::S25(:,:,:,:)
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::Q6(:,:)
       real*8,allocatable::S31(:,:,:,:)
       real*8,allocatable::S35(:,:,:,:)
       real*8,allocatable::S70(:,:,:,:)
       real*8,allocatable::S33(:,:,:,:)
       real*8,allocatable::S39(:,:,:,:)
       real*8,allocatable::S44(:,:,:,:)
       real*8,allocatable::S41(:,:,:,:)
       real*8,allocatable::S46(:,:,:,:)
       real*8,allocatable::S52(:,:,:,:)
       real*8,allocatable::S80(:,:,:,:)
       real*8,allocatable::S48(:,:,:,:)
       real*8,allocatable::S54(:,:,:,:)
       real*8,allocatable::S50(:,:,:,:)
       real*8,allocatable::S56(:,:,:,:)
       real*8,allocatable::S60(:,:,:,:)
       real*8,allocatable::S58(:,:,:,:)
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::Q8(:,:)
       real*8,allocatable::S66(:,:,:,:)
       real*8,allocatable::S68(:,:,:,:)
       real*8,allocatable::S72(:,:,:,:)
       real*8,allocatable::S74(:,:,:,:)
       real*8,allocatable::S76(:,:,:,:)
       real*8,allocatable::S78(:,:,:,:)
       real*8,allocatable::S82(:,:,:,:)
       real*8,allocatable::S83(:,:,:,:)
       real*8,allocatable::S85(:,:,:,:)
       real*8,allocatable::S87(:,:,:,:)
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
       real*8,allocatable::U14(:,:,:,:)
       real*8,allocatable::U16(:,:,:,:)
       real*8,allocatable::U18(:,:,:,:)
       real*8,allocatable::U21(:,:,:,:)
       real*8,allocatable::X1(:,:,:,:)
       real*8,allocatable::U43(:,:,:,:)
       real*8,allocatable::U24(:,:,:,:)
       real*8,allocatable::U26(:,:,:,:)
       real*8,allocatable::U27(:,:,:,:)
       real*8,allocatable::U28(:,:,:,:)
       real*8,allocatable::U29(:,:,:,:)
       real*8,allocatable::U30(:,:,:,:)
       real*8,allocatable::X2(:,:,:,:)
       real*8,allocatable::U32(:,:,:,:)
       real*8,allocatable::U71(:,:,:,:)
       real*8,allocatable::U34(:,:,:,:)
       real*8,allocatable::U40(:,:,:,:)
       real*8,allocatable::U45(:,:,:,:)
       real*8,allocatable::U42(:,:,:,:)
       real*8,allocatable::X3(:,:,:,:)
       real*8,allocatable::U47(:,:,:,:)
       real*8,allocatable::U81(:,:,:,:)
       real*8,allocatable::U49(:,:,:,:)
       real*8,allocatable::U55(:,:,:,:)
       real*8,allocatable::U51(:,:,:,:)
       real*8,allocatable::U57(:,:,:,:)
       real*8,allocatable::U61(:,:,:,:)
       real*8,allocatable::U59(:,:,:,:)
       real*8,allocatable::U62(:,:,:,:)
       real*8,allocatable::U63(:,:,:,:)
       real*8,allocatable::U64(:,:,:,:)
       real*8,allocatable::U65(:,:,:,:)
       real*8,allocatable::U67(:,:,:,:)
       real*8,allocatable::U69(:,:,:,:)
       real*8,allocatable::U73(:,:,:,:)
       real*8,allocatable::U75(:,:,:,:)
       real*8,allocatable::U77(:,:,:,:)
       real*8,allocatable::U79(:,:,:,:)
       real*8,allocatable::U84(:,:,:,:)
       real*8,allocatable::U86(:,:,:,:)
       real*8,allocatable::U88(:,:,:,:)
       real*8,allocatable::U91(:,:,:,:)
       real*8,allocatable::U92(:,:,:,:)
       real*8,allocatable::U93(:,:,:,:)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,l2B,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,t2B,D2)
       allocate(Q1(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,D2,Q1)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(U4(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K4
       I3=K4
       call jungemm(I1,I2,I3,D1,Q1,U4)
       deallocate(D1)
C
       call
     & sul2134(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U4,1.0d0)
       V2C=V2C-U4
       deallocate(U4)
       deallocate(Q1)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,l2B,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(Q2(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,D1,D2,Q2)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U5(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2
       I3=K2
       call jungemm(I1,I2,I3,D1,Q2,U5)
       deallocate(D1)
C
       call
     & sul3124(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U5,-1.0d0)
       call
     & sul4123(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U5,1.0d0)
       deallocate(U5)
       deallocate(Q2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,l2C,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(Q3(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,D2,Q3)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(U11(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K4
       I3=K4
       call jungemm(I1,I2,I3,D1,Q3,U11)
       deallocate(D1)
C
       call
     & sul2134(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U11,1.0d0/2)
       V2C=V2C-1.0d0/2*U11
       deallocate(U11)
       deallocate(Q3)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,l2C,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(Q4(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,D1,D2,Q4)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U12(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2
       I3=K2
       call jungemm(I1,I2,I3,D1,Q4,U12)
       deallocate(D1)
C
       call
     & sul3124(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U12,-1.0d0/2)
       call
     & sul4123(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U12,1.0d0/2)
       deallocate(U12)
       deallocate(Q4)
C
       allocate(F1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef523614(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,N2,N3,N0,N2,l3B,F1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S13(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4*K1
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,F1,D2,S13)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N2,N3,N0,N2,IntM,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef2134(N0,N1,N0,N1,N2,N3,N0,N2,
     & N0,N1,N0,N1,N2,N3,N0,N2,S13,D2)
       allocate(U14(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K4
       I3=K1*K1
       call jungemm(I1,I2,I3,D1,D2,U14)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul2314(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U14,1.0d0/2)
       call
     & sul1324(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U14,-1.0d0/2)
       call
     & sul2413(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U14,-1.0d0/2)
       call
     & sul1423(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U14,1.0d0/2)
       deallocate(U14)
       deallocate(S13)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef562314(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N2,N3,N0,N2,l3B,F1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(S15(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K3
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,F1,D2,S15)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(U16(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K4
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,S15,U16)
       deallocate(D1)
C
       call
     & sul2314(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U16,-1.0d0/2)
       call
     & sul1324(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U16,1.0d0/2)
       call
     & sul2413(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U16,1.0d0/2)
       call
     & sul1423(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U16,-1.0d0/2)
       deallocate(U16)
       deallocate(S15)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef562314(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N2,N3,N0,N2,l3B,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N0,N1,N1,N3,t3A,F2)
       allocate(S17(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K3*K1
       I3=K3*K3*K1*K1
       call jungemm(I1,I2,I3,F1,F2,S17)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(U18(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,S17,U18)
       deallocate(D1)
C
       call
     & sul2314(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U18,-1.0d0/4)
       call
     & sul1324(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U18,1.0d0/4)
       call
     & sul2413(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U18,1.0d0/4)
       call
     & sul1423(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U18,-1.0d0/4)
       deallocate(U18)
       deallocate(S17)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N2,N3,N0,N2,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S20(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S20)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       X1=0.0d0
       X1=X1+S20
C
       call slx3142(N0,N3,N0,N1,N0,N1,N2,N3,N0,N2,X1,IntM,1.000d0)
C
       allocate(F1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef523614(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,N2,N3,N0,N2,l3B,F1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S19(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4*K1
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,F1,D2,S19)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D2(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef2134(N0,N1,N0,N1,N2,N3,N0,N2,
     & N0,N1,N0,N1,N2,N3,N0,N2,S19,D2)
       allocate(U21(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K4
       I3=K1*K1
       call jungemm(I1,I2,I3,S20,D2,U21)
       deallocate(D2)
C
       call
     & sul2314(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U21,1.0d0/2)
       call
     & sul1324(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U21,-1.0d0/2)
       call
     & sul2413(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U21,-1.0d0/2)
       call
     & sul1423(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U21,1.0d0/2)
       deallocate(U21)
       deallocate(S19)
C
       allocate(F1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef413625(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,N2,N3,N0,N2,l3C,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(S37(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4*K1
       I2=K1
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S37)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D2(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef2134(N0,N1,N0,N1,N2,N3,N0,N2,
     & N0,N1,N0,N1,N2,N3,N0,N2,S37,D2)
       allocate(U43(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K4
       I3=K1*K1
       call jungemm(I1,I2,I3,X1,D2,U43)
       deallocate(D2)
C
       call
     & sul2314(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U43,1.0d0)
       call
     & sul1324(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U43,-1.0d0)
       call
     & sul2413(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U43,-1.0d0)
       call
     & sul1423(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U43,1.0d0)
       deallocate(U43)
       deallocate(X1)
       deallocate(S37)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef562314(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N2,N3,N0,N2,l3B,F1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(S22(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K3
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,F1,D2,S22)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef2134(N1,N3,N1,N3,N2,N3,N0,N2,
     & N1,N3,N1,N3,N2,N3,N0,N2,S22,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S23(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K3
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S23)
       deallocate(D1)
       deallocate(B2)
       deallocate(S22)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(U24(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,S23,U24)
       deallocate(D1)
C
       call
     & sul2314(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U24,1.0d0/2)
       call
     & sul1324(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U24,-1.0d0/2)
       call
     & sul2413(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U24,-1.0d0/2)
       call
     & sul1423(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U24,1.0d0/2)
       deallocate(U24)
       deallocate(S23)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef562314(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N2,N3,N0,N2,l3B,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef562341(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N0,N2,N2,N3,t3B,F2)
       allocate(S25(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K4*K2
       I3=K3*K3*K1*K1
       call jungemm(I1,I2,I3,F1,F2,S25)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U26(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,S25,U26)
       deallocate(D1)
C
       call
     & sul2314(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U26,-1.0d0/4)
       call
     & sul1324(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U26,1.0d0/4)
       call
     & sul2413(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U26,1.0d0/4)
       call
     & sul1423(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U26,-1.0d0/4)
       deallocate(U26)
       deallocate(S25)
C
       allocate(F1(N0+1:N2,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3))
       call relef456231(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N1,N3,N1,N3,N2,N3,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3))
       call relef456231(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N1,N3,N1,N3,N2,N3,t3B,F2)
       allocate(Q5(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K3*K3*K1*K1*K2
       call jungemm(I1,I2,I3,F1,F2,Q5)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(U27(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K4
       I3=K4
       call jungemm(I1,I2,I3,D1,Q5,U27)
       deallocate(D1)
C
       call
     & sul2134(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U27,1.0d0/4)
       V2C=V2C-1.0d0/4*U27
       deallocate(U27)
       deallocate(Q5)
C
       allocate(F1(N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef561234(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,l3B,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef561234(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,t3B,F2)
       allocate(Q6(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K3*K3*K4*K1*K1
       call jungemm(I1,I2,I3,F1,F2,Q6)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U28(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2
       I3=K2
       call jungemm(I1,I2,I3,D1,Q6,U28)
       deallocate(D1)
C
       call
     & sul3124(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U28,-1.0d0/4)
       call
     & sul4123(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U28,1.0d0/4)
       deallocate(U28)
       deallocate(Q6)
C
       allocate(F1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef463512(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N0,N2,N2,N3,N2,N3,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,t2B,D2)
       allocate(S31(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4*K2
       I2=K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,F1,D2,S31)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef2134(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S35(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S35)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X2(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X2=0.0d0
       X2=X2-S35
C
       call slx1234(N0,N3,N0,N2,N2,N3,N0,N2,N0,N2,X2,IntB,1.000d0)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef2134(N2,N3,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,S31,D2)
       allocate(U32(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K4*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,X2,D2,U32)
       deallocate(D2)
C
       V2C=V2C-U32
       deallocate(U32)
       deallocate(S31)
       deallocate(X2)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef451623(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N0,N2,N2,N3,N2,N3,l3D,F1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(S70(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4*K2
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,F1,D2,S70)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef2134(N2,N3,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,S70,D2)
       allocate(U71(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K4*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,S35,D2,U71)
       deallocate(D2)
C
       V2C=V2C+1.0d0/2*U71
       deallocate(U71)
       deallocate(S70)
C
       allocate(F1(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3))
       call relef456312(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N1,N3,N2,N3,N2,N3,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3))
       call relef456312(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N1,N3,N2,N3,N2,N3,t3C,F2)
       allocate(S33(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K4*K4
       I3=K3*K1*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S33)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(U34(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K4*K4
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,S33,U34)
       deallocate(D1)
C
       V2C=V2C+1.0d0/4*U34
       deallocate(U34)
       deallocate(S33)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef461325(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N2,N3,N0,N2,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2B,D2)
       allocate(S39(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K3
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,F1,D2,S39)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(U40(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K4
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,S39,U40)
       deallocate(D1)
C
       call
     & sul2314(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U40,-1.0d0)
       call
     & sul1324(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U40,1.0d0)
       call
     & sul2413(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U40,1.0d0)
       call
     & sul1423(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U40,-1.0d0)
       deallocate(U40)
C
       allocate(D1(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef2134(N1,N3,N1,N3,N2,N3,N0,N2,
     & N1,N3,N1,N3,N2,N3,N0,N2,S39,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S44(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K3
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S44)
       deallocate(D1)
       deallocate(B2)
       deallocate(S39)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(U45(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,S44,U45)
       deallocate(D1)
C
       call
     & sul2314(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U45,1.0d0)
       call
     & sul1324(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U45,-1.0d0)
       call
     & sul2413(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U45,-1.0d0)
       call
     & sul1423(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U45,1.0d0)
       deallocate(U45)
       deallocate(S44)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef461325(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N2,N3,N0,N2,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N0,N1,N1,N3,t3B,F2)
       allocate(S41(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K3*K1
       I3=K3*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S41)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(U42(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,S41,U42)
       deallocate(D1)
C
       call
     & sul2314(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U42,-1.0d0)
       call
     & sul1324(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U42,1.0d0)
       call
     & sul2413(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U42,1.0d0)
       call
     & sul1423(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U42,-1.0d0)
       deallocate(U42)
       deallocate(S41)
C
       allocate(F1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef613425(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,N2,N3,N0,N2,l3C,F1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(S46(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,F1,D2,S46)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S52(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S52)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       X3=0.0d0
       call sul2134(N0,N2,N0,N2,N2,N3,N0,N2,X3,S52,1.000d0)
C
       call slx2314(N0,N3,N0,N2,N0,N2,N2,N3,N0,N2,X3,IntB,1.000d0)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef2134(N0,N2,N0,N2,N2,N3,N0,N2,
     & N0,N2,N0,N2,N2,N3,N0,N2,X3,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef2134(N0,N2,N0,N2,N2,N3,N0,N2,
     & N0,N2,N0,N2,N2,N3,N0,N2,S46,D2)
       allocate(U47(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K4
       I3=K2*K2
       call jungemm(I1,I2,I3,D1,D2,U47)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul2314(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U47,1.0d0)
       call
     & sul1324(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U47,-1.0d0)
       call
     & sul2413(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U47,-1.0d0)
       call
     & sul1423(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U47,1.0d0)
       deallocate(U47)
       deallocate(S46)
       deallocate(X3)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef412536(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,N2,N3,N0,N2,l3D,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S80(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S80)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef2134(N0,N2,N0,N2,N2,N3,N0,N2,
     & N0,N2,N0,N2,N2,N3,N0,N2,S80,D2)
       allocate(U81(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K4
       I3=K2*K2
       call jungemm(I1,I2,I3,S52,D2,U81)
       deallocate(D2)
C
       call
     & sul2314(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U81,1.0d0/2)
       call
     & sul1324(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U81,-1.0d0/2)
       call
     & sul2413(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U81,-1.0d0/2)
       call
     & sul1423(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U81,1.0d0/2)
       deallocate(U81)
       deallocate(S80)
C
       allocate(F1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef463125(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,N2,N3,N0,N2,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,t2B,D2)
       allocate(S48(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,F1,D2,S48)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef2134(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N2,N3,N2,N3,N0,N2,S48,D2)
       allocate(U49(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K4
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,D2,U49)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul2314(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U49,-1.0d0)
       call
     & sul1324(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U49,1.0d0)
       call
     & sul2413(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U49,1.0d0)
       call
     & sul1423(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U49,-1.0d0)
       deallocate(U49)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef2134(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N2,N3,N2,N3,N0,N2,S48,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S54(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S54)
       deallocate(D1)
       deallocate(B2)
       deallocate(S48)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U55(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,S54,U55)
       deallocate(D1)
C
       call
     & sul2314(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U55,1.0d0)
       call
     & sul1324(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U55,-1.0d0)
       call
     & sul2413(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U55,-1.0d0)
       call
     & sul1423(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U55,1.0d0)
       deallocate(U55)
       deallocate(S54)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef461325(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N2,N3,N0,N2,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef461352(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N0,N2,N2,N3,t3C,F2)
       allocate(S50(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K4*K2
       I3=K3*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S50)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U51(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,S50,U51)
       deallocate(D1)
C
       call
     & sul2314(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U51,-1.0d0)
       call
     & sul1324(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U51,1.0d0)
       call
     & sul2413(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U51,1.0d0)
       call
     & sul1423(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U51,-1.0d0)
       deallocate(U51)
       deallocate(S50)
C
       allocate(F1(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef613245(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N2,N3,N0,N2,N0,N2,l3C,F1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(S56(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,F1,D2,S56)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,IntB,D1)
       allocate(U57(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K2*K2
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,S56,U57)
       deallocate(D1)
C
       call
     & sul3412(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U57,-1.0d0)
       deallocate(U57)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef2134(N0,N2,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,N2,N0,N2,N0,N2,S56,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S60(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S60)
       deallocate(D1)
       deallocate(B2)
       deallocate(S56)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef2134(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S60,D2)
       allocate(U61(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K2*K2
       I3=K2*K2
       call jungemm(I1,I2,I3,D1,D2,U61)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul3412(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U61,1.0d0)
       deallocate(U61)
       deallocate(S60)
C
       allocate(F1(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2))
       call relef612345(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,l3C,F1)
       allocate(F2(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2))
       call relef612345(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,t3C,F2)
       allocate(S58(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K2
       I3=K3*K4*K4*K1
       call jungemm(I1,I2,I3,F1,F2,S58)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(U59(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K2*K2
       I3=K2*K2
       call jungemm(I1,I2,I3,D1,S58,U59)
       deallocate(D1)
C
       call
     & sul3412(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U59,1.0d0/4)
       deallocate(U59)
       deallocate(S58)
C
       allocate(F1(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef456132(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N1,N3,N2,N3,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef456132(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N1,N3,N2,N3,t3C,F2)
       allocate(Q7(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K3*K4*K1*K2*K2
       call jungemm(I1,I2,I3,F1,F2,Q7)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(U62(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K4
       I3=K4
       call jungemm(I1,I2,I3,D1,Q7,U62)
       deallocate(D1)
C
       call
     & sul2134(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U62,1.0d0/2)
       V2C=V2C-1.0d0/2*U62
       deallocate(U62)
       deallocate(Q7)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef461235(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef461235(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,t3C,F2)
       allocate(Q8(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K3*K4*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,Q8)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U63(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2
       I3=K2
       call jungemm(I1,I2,I3,D1,Q8,U63)
       deallocate(D1)
C
       call
     & sul3124(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U63,-1.0d0/2)
       call
     & sul4123(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U63,1.0d0/2)
       deallocate(U63)
       deallocate(Q8)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef451623(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N0,N2,N2,N3,N2,N3,l3D,F1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(S66(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4*K2
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,F1,D2,S66)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef2134(N2,N3,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,S66,D2)
       allocate(U67(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K4*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,U67)
       deallocate(D1)
       deallocate(D2)
C
       V2C=V2C-1.0d0/2*U67
       deallocate(U67)
       deallocate(S66)
C
       allocate(F1(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef456123(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,l3D,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef456123(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,t3D,F2)
       allocate(S68(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K4*K4
       I3=K4*K2*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S68)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(U69(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K4*K4
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,S68,U69)
       deallocate(D1)
C
       V2C=V2C+1.0d0/12*U69
       deallocate(U69)
       deallocate(S68)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,l3D,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N0,N1,N1,N3,t3C,F2)
       allocate(S72(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K3*K1
       I3=K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S72)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(U73(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,S72,U73)
       deallocate(D1)
C
       call
     & sul2314(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U73,-1.0d0/4)
       call
     & sul1324(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U73,1.0d0/4)
       call
     & sul2413(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U73,1.0d0/4)
       call
     & sul1423(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U73,-1.0d0/4)
       deallocate(U73)
       deallocate(S72)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef412536(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,N2,N3,N0,N2,l3D,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S74(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S74)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef2134(N0,N2,N0,N2,N2,N3,N0,N2,
     & N0,N2,N0,N2,N2,N3,N0,N2,S74,D2)
       allocate(U75(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K4
       I3=K2*K2
       call jungemm(I1,I2,I3,D1,D2,U75)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul2314(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U75,1.0d0/2)
       call
     & sul1324(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U75,-1.0d0/2)
       call
     & sul2413(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U75,-1.0d0/2)
       call
     & sul1423(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U75,1.0d0/2)
       deallocate(U75)
       deallocate(S74)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,l3D,F1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(S76(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,F1,D2,S76)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U77(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K4
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,S76,U77)
       deallocate(D1)
C
       call
     & sul2314(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U77,-1.0d0/2)
       call
     & sul1324(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U77,1.0d0/2)
       call
     & sul2413(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U77,1.0d0/2)
       call
     & sul1423(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U77,-1.0d0/2)
       deallocate(U77)
       deallocate(S76)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,l3D,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef451263(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N0,N2,N2,N3,t3D,F2)
       allocate(S78(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K4*K2
       I3=K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S78)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U79(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,S78,U79)
       deallocate(D1)
C
       call
     & sul2314(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U79,-1.0d0/4)
       call
     & sul1324(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U79,1.0d0/4)
       call
     & sul2413(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U79,1.0d0/4)
       call
     & sul1423(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U79,-1.0d0/4)
       deallocate(U79)
       deallocate(S78)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,l3D,F1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(S82(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,F1,D2,S82)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef2134(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N2,N3,N2,N3,N0,N2,S82,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S83(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S83)
       deallocate(D1)
       deallocate(B2)
       deallocate(S82)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U84(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,S83,U84)
       deallocate(D1)
C
       call
     & sul2314(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U84,1.0d0/2)
       call
     & sul1324(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U84,-1.0d0/2)
       call
     & sul2413(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U84,-1.0d0/2)
       call
     & sul1423(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U84,1.0d0/2)
       deallocate(U84)
       deallocate(S83)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef412356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,l3D,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S85(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S85)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,IntB,D1)
       allocate(U86(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K2*K2
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,S85,U86)
       deallocate(D1)
C
       call
     & sul3412(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U86,-1.0d0/2)
       deallocate(U86)
       deallocate(S85)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef412356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,l3D,F1)
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef412356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,t3D,F2)
       allocate(S87(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K2
       I3=K4*K4*K4*K2
       call jungemm(I1,I2,I3,F1,F2,S87)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(U88(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K2*K2
       I3=K2*K2
       call jungemm(I1,I2,I3,D1,S87,U88)
       deallocate(D1)
C
       call
     & sul3412(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U88,1.0d0/12)
       deallocate(U88)
       deallocate(S87)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef412356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,l3D,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S89(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S89)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef2134(N0,N2,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,N2,N0,N2,N0,N2,S89,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S90(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S90)
       deallocate(D1)
       deallocate(B2)
       deallocate(S89)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef2134(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S90,D2)
       allocate(U91(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K2*K2
       I3=K2*K2
       call jungemm(I1,I2,I3,D1,D2,U91)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul3412(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U91,1.0d0/2)
       deallocate(U91)
       deallocate(S90)
C
       allocate(F1(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef456123(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,l3D,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef456123(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,t3D,F2)
       allocate(Q9(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K4*K4*K2*K2*K2
       call jungemm(I1,I2,I3,F1,F2,Q9)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(U92(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K4
       I3=K4
       call jungemm(I1,I2,I3,D1,Q9,U92)
       deallocate(D1)
C
       call
     & sul2134(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U92,1.0d0/12)
       V2C=V2C-1.0d0/12*U92
       deallocate(U92)
       deallocate(Q9)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,l3D,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,t3D,F2)
       allocate(Q10(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4*K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,Q10)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U93(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2
       I3=K2
       call jungemm(I1,I2,I3,D1,Q10,U93)
       deallocate(D1)
C
       call
     & sul3124(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U93,-1.0d0/12)
       call
     & sul4123(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U93,1.0d0/12)
       deallocate(U93)
       deallocate(Q10)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,H2C,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,l1B,B2)
       allocate(U1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,U1)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sul2134(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U1,1.0d0)
       V2C=V2C-U1
       deallocate(U1)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N2,N3,N0,N2,H2C,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,l1B,B2)
       allocate(U2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,U2)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sul3124(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U2,1.0d0)
       call
     & sul4123(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U2,-1.0d0)
       deallocate(U2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,H2B,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,l2B,D2)
       allocate(U3(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,U3)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul2314(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U3,-1.0d0)
       call
     & sul1324(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U3,1.0d0)
       call
     & sul2413(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U3,1.0d0)
       call
     & sul1423(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U3,-1.0d0)
       deallocate(U3)
C
       allocate(B1(N0+1:N2,N0+1:N2))
       call relef12(N0,N3,N0,N3,
     & N0,N2,N0,N2,H1B,B1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,l2C,D2)
       allocate(U6(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K4*K4
       I3=K2
       call jungemm(I1,I2,I3,B1,D2,U6)
       deallocate(B1)
       deallocate(D2)
C
       V2C=V2C+U6
       call
     & sul1243(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U6,-1.0d0)
       deallocate(U6)
C
       allocate(B1(N2+1:N3,N2+1:N3))
       call relef21(N0,N3,N0,N3,
     & N2,N3,N2,N3,H1B,B1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,l2C,D2)
       allocate(U7(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2*K2*K4
       I3=K4
       call jungemm(I1,I2,I3,B1,D2,U7)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sul2341(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U7,1.0d0)
       call
     & sul1342(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U7,-1.0d0)
       deallocate(U7)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N0,N2,N0,N2,H2C,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,l2C,D2)
       allocate(U8(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K4*K4
       I3=K2*K2
       call jungemm(I1,I2,I3,D1,D2,U8)
       deallocate(D1)
       deallocate(D2)
C
       V2C=V2C+1.0d0/2*U8
       deallocate(U8)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,H2C,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,l2C,D2)
       allocate(U9(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,U9)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul2314(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U9,-1.0d0)
       call
     & sul1324(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U9,1.0d0)
       call
     & sul2413(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U9,1.0d0)
       call
     & sul1423(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U9,-1.0d0)
       deallocate(U9)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N2,N3,N2,N3,H2C,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,l2C,D2)
       allocate(U10(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K2*K2
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,D2,U10)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul3412(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U10,1.0d0/2)
       deallocate(U10)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef1243(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N0,N2,H2B,D1)
       allocate(F2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef463125(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,N2,N3,N0,N2,l3C,F2)
       allocate(U29(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K4*K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,F2,U29)
       deallocate(D1)
       deallocate(F2)
C
       V2C=V2C+U29
       call
     & sul1243(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U29,-1.0d0)
       deallocate(U29)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef2341(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N2,N3,H2B,D1)
       allocate(F2(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef613245(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N2,N3,N0,N2,N0,N2,l3C,F2)
       allocate(U30(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2*K2*K4
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,D1,F2,U30)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul2341(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U30,1.0d0)
       call
     & sul1342(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U30,-1.0d0)
       deallocate(U30)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,H2C,D1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,l3D,F2)
       allocate(U64(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K4*K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,F2,U64)
       deallocate(D1)
       deallocate(F2)
C
       V2C=V2C+1.0d0/2*U64
       call
     & sul1243(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U64,-1.0d0/2)
       deallocate(U64)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,H2C,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef412356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,l3D,F2)
       allocate(U65(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2*K2*K4
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,D1,F2,U65)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul2341(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U65,1.0d0/2)
       call
     & sul1342(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U65,-1.0d0/2)
       deallocate(U65)
C
       end
