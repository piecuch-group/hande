       subroutine t3D_update_cor(N0,N1,N2,N3,V3D,
     & K1,K2,K3,K4,
     & FockR,FockB,IntR,IntB,IntM,
     & t1A,t1B,t2A,t2B,t2C,t3A,t3B,t3C,t3D)
C
       integer a,b,c,e,f,g,h,i,j,k,m,n,o,p
       real*8 FockR(N3,N3)
       real*8 FockB(N3,N3)
       real*8 IntR(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 IntB(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 IntM(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 t1A(N1+1:N3,N0+1:N1)
       real*8 t1B(N2+1:N3,N0+1:N2)
       real*8 t2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 t2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 t2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
       real*8 t3A(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)
       real*8 t3B(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1)
       real*8 t3C(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1)
       real*8 t3D(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2)
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
       real*8,allocatable::S1(:,:,:,:)
       real*8,allocatable::S2(:,:,:,:)
       real*8,allocatable::S3(:,:,:,:)
       real*8,allocatable::S4(:,:,:,:)
       real*8,allocatable::S5(:,:,:,:)
       real*8,allocatable::S6(:,:,:,:)
       real*8,allocatable::Q3(:,:)
       real*8,allocatable::Q4(:,:)
       real*8,allocatable::S7(:,:,:,:)
       real*8,allocatable::S29(:,:,:,:)
       real*8,allocatable::S30(:,:,:,:)
       real*8,allocatable::S9(:,:,:,:)
       real*8,allocatable::S32(:,:,:,:)
       real*8,allocatable::S33(:,:,:,:)
       real*8,allocatable::S11(:,:,:,:)
       real*8,allocatable::S12(:,:,:,:)
       real*8,allocatable::S13(:,:,:,:)
       real*8,allocatable::S14(:,:,:,:)
       real*8,allocatable::S42(:,:,:,:)
       real*8,allocatable::S43(:,:,:,:)
       real*8,allocatable::S15(:,:,:,:)
       real*8,allocatable::S16(:,:,:,:)
       real*8,allocatable::S17(:,:,:,:)
       real*8,allocatable::S18(:,:,:,:)
       real*8,allocatable::S19(:,:,:,:)
       real*8,allocatable::S20(:,:,:,:)
       real*8,allocatable::S21(:,:,:,:)
       real*8,allocatable::S22(:,:,:,:)
       real*8,allocatable::S23(:,:,:,:)
       real*8,allocatable::S24(:,:,:,:)
       real*8,allocatable::S47(:,:,:,:)
       real*8,allocatable::S49(:,:,:,:)
       real*8,allocatable::Q9(:,:)
       real*8,allocatable::S27(:,:,:,:)
       real*8,allocatable::S44(:,:,:,:)
       real*8,allocatable::Q11(:,:)
       real*8,allocatable::Q13(:,:)
       real*8,allocatable::S28(:,:,:,:)
       real*8,allocatable::Q12(:,:)
       real*8,allocatable::S8(:,:,:,:)
       real*8,allocatable::S31(:,:,:,:)
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::S10(:,:,:,:)
       real*8,allocatable::Q6(:,:)
       real*8,allocatable::S34(:,:,:,:)
       real*8,allocatable::S35(:,:,:,:)
       real*8,allocatable::S36(:,:,:,:)
       real*8,allocatable::S38(:,:,:,:)
       real*8,allocatable::Q14(:,:)
       real*8,allocatable::S39(:,:,:,:)
       real*8,allocatable::S37(:,:,:,:)
       real*8,allocatable::S50(:,:,:,:)
       real*8,allocatable::S51(:,:,:,:)
       real*8,allocatable::S52(:,:,:,:)
       real*8,allocatable::Q8(:,:)
       real*8,allocatable::Q10(:,:)
       real*8,allocatable::S26(:,:,:,:)
       real*8,allocatable::S46(:,:,:,:)
       real*8,allocatable::S45(:,:,:,:)
       real*8,allocatable::Q16(:,:)
       real*8,allocatable::S48(:,:,:,:)
       real*8,allocatable::S25(:,:,:,:)
       real*8,allocatable::S40(:,:,:,:)
       real*8,allocatable::Q15(:,:)
       real*8,allocatable::S41(:,:,:,:)
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::X1(:,:,:,:)
       real*8,allocatable::U1(:,:,:,:,:,:)
       real*8,allocatable::X2(:,:,:,:)
       real*8,allocatable::U2(:,:,:,:,:,:)
       real*8,allocatable::X3(:,:,:,:)
       real*8,allocatable::U3(:,:,:,:,:,:)
       real*8,allocatable::X4(:,:)
       real*8,allocatable::U4(:,:,:,:,:,:)
       real*8,allocatable::X5(:,:)
       real*8,allocatable::U5(:,:,:,:,:,:)
       real*8,allocatable::X6(:,:,:,:)
       real*8,allocatable::U6(:,:,:,:,:,:)
       real*8,allocatable::X7(:,:,:,:)
       real*8,allocatable::U7(:,:,:,:,:,:)
       real*8,allocatable::X8(:,:,:,:)
       real*8,allocatable::U8(:,:,:,:,:,:)
       real*8,allocatable::X9(:,:,:,:)
       real*8,allocatable::U12(:,:,:,:,:,:)
       real*8,allocatable::X10(:,:,:,:)
       real*8,allocatable::U13(:,:,:,:,:,:)
       real*8,allocatable::U19(:,:,:,:,:,:)
       real*8,allocatable::U49(:,:,:,:,:,:)
       real*8,allocatable::U50(:,:,:,:,:,:)
       real*8,allocatable::X11(:,:,:,:)
       real*8,allocatable::U32(:,:,:,:,:,:)
       real*8,allocatable::X12(:,:,:,:)
       real*8,allocatable::U33(:,:,:,:,:,:)
       real*8,allocatable::X13(:,:,:,:)
       real*8,allocatable::U34(:,:,:,:,:,:)
       real*8,allocatable::X14(:,:,:,:)
       real*8,allocatable::U36(:,:,:,:,:,:)
       real*8,allocatable::X15(:,:,:,:)
       real*8,allocatable::U37(:,:,:,:,:,:)
       real*8,allocatable::X16(:,:,:,:)
       real*8,allocatable::U66(:,:,:,:,:,:)
       real*8,allocatable::X17(:,:,:,:)
       real*8,allocatable::U68(:,:,:,:,:,:)
       real*8,allocatable::U23(:,:,:,:,:,:)
       real*8,allocatable::U56(:,:,:,:,:,:)
       real*8,allocatable::U57(:,:,:,:,:,:)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N0,N2,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q1(N0+1:N2,N0+1:N2))
       I1=K2*K2
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(N0+1:N2,N0+1:N2))
       X4=0.0d0
       X4=X4+Q1
       deallocate(Q1)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N2,N3,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q2(N2+1:N3,N2+1:N3))
       I1=K4*K4
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q2)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X5(N2+1:N3,N2+1:N3))
       X5=0.0d0
       X5=X5+Q2
       deallocate(Q2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N0,N2,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X1=0.0d0
       call sul2134(N0,N2,N2,N3,N0,N2,N0,N2,X1,S1, 1.0d0)
       deallocate(S1)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S2(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S2)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X9(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X9=0.0d0
       call sul3124(N0,N2,N2,N3,N0,N2,N0,N2,X9,S2, 1.0d0)
       deallocate(S2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S3(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S3)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X10(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       X10=0.0d0
       call sul3124(N2,N3,N2,N3,N2,N3,N0,N2,X10,S3, 1.0d0)
       deallocate(S3)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N2,N3,N2,N3,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S4(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       I1=K4*K4*K4
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S4)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X2(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       X2=0.0d0
       call sul4123(N2,N3,N2,N3,N2,N3,N0,N2,X2,S4,-1.0d0)
       deallocate(S4)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N0,N2,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S5(N2+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S5)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       X3=0.0d0
       call sul3124(N0,N1,N1,N3,N2,N3,N0,N2,X3,S5,-1.0d0)
       deallocate(S5)
C
       allocate(D1(N2+1:N3,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef1423(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S6(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       I1=K4*K3*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S6)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N1,N1,N3,N2,N3,N0,N2,X3,S6, 1.0d0)
       deallocate(S6)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call relef12(0,N3,0,N3,
     & N2,N3,N0,N2,FockB,B1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(Q3(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,B1,B2,Q3)
       deallocate(B1)
       deallocate(B2)
C
       call sul21(N0,N2,N0,N2,X4,Q3, 1.0d0)
       deallocate(Q3)
C
       allocate(B1(N0+1:N2,N2+1:N3))
       call relef21(0,N3,0,N3,
     & N0,N2,N2,N3,FockB,B1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q4(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,B1,B2,Q4)
       deallocate(B1)
       deallocate(B2)
C
       call sul21(N2,N3,N2,N3,X5,Q4,-1.0d0)
       deallocate(Q4)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N0,N2,N0,N2,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S7(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S7)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef2314(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S7,D1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,t3D,F2)
       allocate(U19(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4*K4*K4
       I3=K2*K2
       call jungemm(I1,I2,I3,D1,F2,U19)
       deallocate(D1)
       deallocate(F2)
C
       V3D=V3D+0.5d0*U19
       call
     & sul123546(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U19,-0.5d0)
       call
     & sul123465(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U19,-0.5d0)
       call
     & sul123564(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U19, 0.5d0)
       call
     & sul123645(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U19, 0.5d0)
       call
     & sul123654(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U19,-0.5d0)
       deallocate(U19)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef2314(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S7,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S29(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S29)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef2134(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N0,N2,S29,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(U49(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K2
       call jungemm(I1,I2,I3,D1,D2,U49)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul234156(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U49, 1.0d0)
       call
     & sul134256(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U49,-1.0d0)
       call
     & sul235146(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U49,-1.0d0)
       call
     & sul135246(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U49, 1.0d0)
       call
     & sul234165(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U49,-1.0d0)
       call
     & sul134265(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U49, 1.0d0)
       call
     & sul235164(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U49, 1.0d0)
       call
     & sul135264(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U49,-1.0d0)
       call
     & sul236145(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U49, 1.0d0)
       call
     & sul136245(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U49,-1.0d0)
       call
     & sul236154(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U49,-1.0d0)
       call
     & sul136254(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U49, 1.0d0)
       deallocate(U49)
       deallocate(S29)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef3214(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S7,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S30(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S30)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef2134(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N0,N2,S30,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(U50(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K2
       call jungemm(I1,I2,I3,D1,D2,U50)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul124356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U50,-1.0d0)
       call
     & sul125346(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U50, 1.0d0)
       call
     & sul124365(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U50, 1.0d0)
       call
     & sul125364(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U50,-1.0d0)
       call
     & sul126345(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U50,-1.0d0)
       call
     & sul126354(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U50, 1.0d0)
       deallocate(U50)
       deallocate(S30)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S9(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S9)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X7(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       X7=0.0d0
       call sul4123(N0,N2,N2,N3,N2,N3,N0,N2,X7,S9,-1.0d0)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef3241(N0,N2,N0,N2,N2,N3,N2,N3,
     & N2,N3,N0,N2,N2,N3,N0,N2,S9,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S32(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S32)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N2,N3,N0,N2,N0,N2,X1,S32,-1.0d0)
       deallocate(S32)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,S9,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S33(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S33)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N2,N3,N2,N3,N2,N3,N0,N2,X10,S33,-1.0d0)
       deallocate(S33)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N0,N2,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,t2B,D2)
       allocate(S11(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S11)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N0,N2,N2,N3,N0,N2,N0,N2,X9,S11,-1.0d0)
       deallocate(S11)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,t2B,D2)
       allocate(S12(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S12)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N2,N3,N2,N3,N2,N3,N0,N2,X10,S12, 1.0d0)
       deallocate(S12)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,t2B,D2)
       allocate(S13(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S13)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N1,N1,N3,N2,N3,N0,N2,X3,S13, 1.0d0)
       deallocate(S13)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N2,N3,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,t2B,D2)
       allocate(S14(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S14)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N2,N2,N3,N2,N3,N0,N2,X7,S14, 1.0d0)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef4312(N2,N3,N0,N2,N0,N2,N2,N3,
     & N2,N3,N0,N2,N2,N3,N0,N2,S14,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S42(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S42)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N2,N2,N3,N0,N2,N0,N2,X9,S42,-1.0d0)
       deallocate(S42)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(U12(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K2
       call jungemm(I1,I2,I3,X9,D2,U12)
       deallocate(D2)
C
       call
     & sul234156(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12, 1.0d0)
       call
     & sul134256(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12,-1.0d0)
       call
     & sul124356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12, 1.0d0)
       call
     & sul235146(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12,-1.0d0)
       call
     & sul135246(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12, 1.0d0)
       call
     & sul125346(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12,-1.0d0)
       call
     & sul234165(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12,-1.0d0)
       call
     & sul134265(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12, 1.0d0)
       call
     & sul124365(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12,-1.0d0)
       call
     & sul235164(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12, 1.0d0)
       call
     & sul135264(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12,-1.0d0)
       call
     & sul125364(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12, 1.0d0)
       call
     & sul236145(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12, 1.0d0)
       call
     & sul136245(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12,-1.0d0)
       call
     & sul126345(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12, 1.0d0)
       call
     & sul236154(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12,-1.0d0)
       call
     & sul136254(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12, 1.0d0)
       call
     & sul126354(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12,-1.0d0)
       deallocate(U12)
       deallocate(X9)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3412(N2,N3,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,S14,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S43(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S43)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N2,N3,N2,N3,N2,N3,N0,N2,X10,S43,-1.0d0)
       deallocate(S43)
C
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2C,D2)
       allocate(U13(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2*K2*K4
       I3=K4
       call jungemm(I1,I2,I3,X10,D2,U13)
       deallocate(D2)
C
       call
     & sul345126(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U13,-1.0d0)
       call
     & sul245136(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U13, 1.0d0)
       call
     & sul345216(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U13, 1.0d0)
       call
     & sul245316(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U13,-1.0d0)
       call
     & sul145236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U13,-1.0d0)
       call
     & sul145326(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U13, 1.0d0)
       call
     & sul346125(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U13, 1.0d0)
       call
     & sul246135(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U13,-1.0d0)
       call
     & sul346215(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U13,-1.0d0)
       call
     & sul246315(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U13, 1.0d0)
       call
     & sul146235(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U13, 1.0d0)
       call
     & sul146325(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U13,-1.0d0)
       call
     & sul356124(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U13,-1.0d0)
       call
     & sul256134(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U13, 1.0d0)
       call
     & sul356214(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U13, 1.0d0)
       call
     & sul256314(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U13,-1.0d0)
       call
     & sul156234(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U13,-1.0d0)
       call
     & sul156324(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U13, 1.0d0)
       deallocate(U13)
       deallocate(X10)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call relef12(0,N3,0,N3,
     & N2,N3,N0,N2,FockB,B1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2C,D2)
       allocate(S15(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K2*K4
       I3=K4
       call jungemm(I1,I2,I3,B1,D2,S15)
       deallocate(B1)
       deallocate(D2)
C
       call sul2341(N0,N2,N2,N3,N0,N2,N0,N2,X1,S15, 1.0d0)
       deallocate(S15)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(S16(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K4*K4
       I3=K2*K2
       call jungemm(I1,I2,I3,D1,D2,S16)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X11(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       X11=0.0d0
       call sul2314(N2,N3,N2,N3,N2,N3,N0,N2,X11,S16, 1.0d0)
       deallocate(S16)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef4132(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S17(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S17)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X12(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X12=0.0d0
       call sul2314(N0,N2,N2,N3,N0,N2,N0,N2,X12,S17, 1.0d0)
       deallocate(S17)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S18(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S18)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X13(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       X13=0.0d0
       call sul3412(N2,N3,N2,N3,N2,N3,N0,N2,X13,S18, 1.0d0)
       deallocate(S18)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2C,D2)
       allocate(S19(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K2
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,D2,S19)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N2,N2,N3,N0,N2,N0,N2,X1,S19, 0.5d0)
       deallocate(S19)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(F2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef463125(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,N2,N3,N0,N2,t3C,F2)
       allocate(S20(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2*K4*K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,F2,S20)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X14(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       X14=0.0d0
       call sul2341(N2,N3,N2,N3,N2,N3,N0,N2,X14,S20, 1.0d0)
       deallocate(S20)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N0,N2,IntM,D1)
       allocate(F2(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef613245(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N2,N3,N0,N2,N0,N2,t3C,F2)
       allocate(S21(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K2*K4
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,D1,F2,S21)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X15(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X15=0.0d0
       call sul2341(N0,N2,N2,N3,N0,N2,N0,N2,X15,S21, 1.0d0)
       deallocate(S21)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S22(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S22)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N1,N1,N3,N2,N3,N0,N2,X3,S22, 1.0d0)
       deallocate(S22)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,t3D,F2)
       allocate(S23(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2*K4*K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,F2,S23)
       deallocate(D1)
       deallocate(F2)
C
       call sul2341(N2,N3,N2,N3,N2,N3,N0,N2,X14,S23,-0.5d0)
       deallocate(S23)
C
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2C,D2)
       allocate(U36(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2*K2*K4
       I3=K4
       call jungemm(I1,I2,I3,X14,D2,U36)
       deallocate(D2)
C
       call
     & sul156234(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U36, 1.0d0)
       call
     & sul256134(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U36,-1.0d0)
       call
     & sul356124(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U36, 1.0d0)
       call
     & sul146235(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U36,-1.0d0)
       call
     & sul246135(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U36, 1.0d0)
       call
     & sul346125(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U36,-1.0d0)
       call
     & sul145236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U36, 1.0d0)
       call
     & sul245136(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U36,-1.0d0)
       call
     & sul345126(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U36, 1.0d0)
       deallocate(U36)
       deallocate(X14)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2C,D2)
       allocate(S24(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K2
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,D2,S24)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X6(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       X6=0.0d0
       call sul3412(N0,N2,N0,N2,N0,N2,N0,N2,X6,S24, 0.5d0)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S24,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S47(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S47)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X16(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X16=0.0d0
       call sul2134(N0,N2,N2,N3,N0,N2,N0,N2,X16,S47, 1.0d0)
       deallocate(S47)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef4312(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S24,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S49(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S49)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X17(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X17=0.0d0
       call sul2134(N0,N2,N2,N3,N0,N2,N0,N2,X17,S49, 1.0d0)
       deallocate(S49)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(Q9(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,D1,D2,Q9)
       deallocate(D1)
       deallocate(D2)
C
       call sul21(N0,N2,N0,N2,X4,Q9, 0.5d0)
       deallocate(Q9)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(S27(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K4*K4
       I3=K2*K2
       call jungemm(I1,I2,I3,D1,D2,S27)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X8(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       X8=0.0d0
       call sul3412(N2,N3,N2,N3,N2,N3,N2,N3,X8,S27, 0.5d0)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N2,N3,N2,N3,
     & N2,N3,N2,N3,N2,N3,N2,N3,S27,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S44(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       I1=K4*K4*K4
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S44)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N2,N3,N2,N3,N2,N3,N0,N2,X11,S44,-1.0d0)
       deallocate(S44)
C
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2C,D2)
       allocate(U32(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2*K2*K4
       I3=K4
       call jungemm(I1,I2,I3,X11,D2,U32)
       deallocate(D2)
C
       call
     & sul145236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U32,-0.5d0)
       call
     & sul245136(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U32, 0.5d0)
       call
     & sul345126(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U32,-0.5d0)
       call
     & sul146235(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U32, 0.5d0)
       call
     & sul246135(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U32,-0.5d0)
       call
     & sul346125(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U32, 0.5d0)
       call
     & sul156234(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U32,-0.5d0)
       call
     & sul256134(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U32, 0.5d0)
       call
     & sul356124(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U32,-0.5d0)
       deallocate(U32)
       deallocate(X11)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N2,N3,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q11(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q11)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q13(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,Q11,B2,Q13)
       deallocate(B2)
C
       call sul21(N2,N3,N2,N3,X5,Q13,-1.0d0)
       deallocate(Q13)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call relef21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q11,B1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2C,D2)
       allocate(S28(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K2*K4
       I3=K4
       call jungemm(I1,I2,I3,B1,D2,S28)
       deallocate(B1)
       deallocate(D2)
C
       call sul2341(N0,N2,N2,N3,N0,N2,N0,N2,X1,S28, 1.0d0)
       deallocate(S28)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call relef21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q11,B1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(Q12(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,B1,B2,Q12)
       deallocate(B1)
       deallocate(B2)
C
       call sul21(N0,N2,N0,N2,X4,Q12, 1.0d0)
       deallocate(Q12)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S8(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S8)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N2,N3,N2,N3,N0,N2,X7,S8, 1.0d0)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef2314(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,S8,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S31(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S31)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N2,N3,N2,N3,N2,N3,N0,N2,X2,S31,-1.0d0)
       deallocate(S31)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q5(N0+1:N2,N0+1:N2))
       I1=K2*K2
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q5)
       deallocate(D1)
       deallocate(B2)
C
       X4=X4+Q5
       deallocate(Q5)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S10(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       I1=K4*K4*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S10)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef2341(N2,N3,N2,N3,N2,N3,N2,N3,
     & N2,N3,N2,N3,N2,N3,N2,N3,S10,D1)
       allocate(F2(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef123456(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,t3D,F2)
       allocate(U23(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K2*K2*K2*K4
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,F2,U23)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul345612(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U23, 0.5d0)
       call
     & sul245613(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U23,-0.5d0)
       call
     & sul345621(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U23,-0.5d0)
       call
     & sul245631(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U23, 0.5d0)
       call
     & sul145623(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U23, 0.5d0)
       call
     & sul145632(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U23,-0.5d0)
       deallocate(U23)
       deallocate(S10)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q6(N2+1:N3,N2+1:N3))
       I1=K4*K4
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q6)
       deallocate(D1)
       deallocate(B2)
C
       X5=X5+Q6
       deallocate(Q6)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N0,N1,N1,N3,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S34(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S34)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N2,N0,N1,N1,N3,
     & N0,N2,N0,N1,N1,N3,N0,N2,S34,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S35(N2+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S35)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N1,N1,N3,N2,N3,N0,N2,X3,S35,-1.0d0)
       deallocate(S35)
C
       call slx4231(N0,N3,N0,N1,N1,N3,N2,N3,N0,N2,X3,IntM, 1.0d0)
C
       allocate(F2(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef631245(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N2,N3,N0,N2,N0,N2,t3C,F2)
       allocate(U3(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K2*K4*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,X3,F2,U3)
       deallocate(F2)
C
       call
     & sul234516(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U3, 1.0d0)
       call
     & sul134526(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U3,-1.0d0)
       call
     & sul124536(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U3, 1.0d0)
       call
     & sul234615(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U3,-1.0d0)
       call
     & sul134625(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U3, 1.0d0)
       call
     & sul124635(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U3,-1.0d0)
       call
     & sul235614(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U3, 1.0d0)
       call
     & sul135624(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U3,-1.0d0)
       call
     & sul125634(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U3, 1.0d0)
       deallocate(U3)
       deallocate(X3)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       call relef2341(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N0,N2,N2,N3,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S36(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S36)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,S36,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S38(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S38)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef2314(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,S38,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef412356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,t3D,F2)
       allocate(U56(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K2*K4*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,F2,U56)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul234516(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U56, 1.0d0)
       call
     & sul134526(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U56,-1.0d0)
       call
     & sul234615(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U56,-1.0d0)
       call
     & sul134625(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U56, 1.0d0)
       call
     & sul235614(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U56, 1.0d0)
       call
     & sul135624(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U56,-1.0d0)
       deallocate(U56)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef2431(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,S36,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q14(N0+1:N2,N0+1:N2))
       I1=K2*K2
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q14)
       deallocate(D1)
       deallocate(B2)
C
       X4=X4+Q14
       deallocate(Q14)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef3241(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,S36,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S39(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S39)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef2314(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,S39,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef412356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,t3D,F2)
       allocate(U57(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K2*K4*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,F2,U57)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul124536(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U57,-1.0d0)
       call
     & sul124635(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U57, 1.0d0)
       call
     & sul125634(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U57,-1.0d0)
       deallocate(U57)
       deallocate(S39)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef4231(N0,N2,N0,N2,N0,N2,N2,N3,
     & N2,N3,N0,N2,N0,N2,N0,N2,S36,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S37(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S37)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N0,N2,N0,N2,N0,N2,X6,S37, 1.0d0)
C
       call slx3412(N0,N3,N0,N2,N0,N2,N0,N2,N0,N2,X6,IntB, 1.0d0)
C
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,t3D,F2)
       allocate(U6(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4*K4*K4
       I3=K2*K2
       call jungemm(I1,I2,I3,X6,F2,U6)
       deallocate(F2)
C
       V3D=V3D+0.5d0*U6
       call
     & sul123546(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U6,-0.5d0)
       call
     & sul123645(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U6, 0.5d0)
       deallocate(U6)
       deallocate(X6)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef2314(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S37,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S50(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S50)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N2,N2,N3,N0,N2,N0,N2,X16,S50, 2.0d0)
       deallocate(S50)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(U66(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K2
       call jungemm(I1,I2,I3,X16,D2,U66)
       deallocate(D2)
C
       call
     & sul234156(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U66, 0.5d0)
       call
     & sul134256(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U66,-0.5d0)
       call
     & sul235146(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U66,-0.5d0)
       call
     & sul135246(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U66, 0.5d0)
       call
     & sul236145(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U66, 0.5d0)
       call
     & sul136245(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U66,-0.5d0)
       deallocate(U66)
       deallocate(X16)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef3214(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S37,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S51(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S51)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N2,N2,N3,N0,N2,N0,N2,X17,S51, 2.0d0)
       deallocate(S51)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(U68(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K2
       call jungemm(I1,I2,I3,X17,D2,U68)
       deallocate(D2)
C
       call
     & sul124356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U68,-0.5d0)
       call
     & sul125346(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U68, 0.5d0)
       call
     & sul126345(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U68,-0.5d0)
       deallocate(U68)
       deallocate(X17)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef2314(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,S38,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S52(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S52)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N2,N3,N2,N3,N2,N3,N0,N2,X2,S52, 1.0d0)
       deallocate(S52)
C
       call slx1423(N0,N3,N2,N3,N2,N3,N2,N3,N0,N2,X2,IntB, 1.0d0)
C
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2C,D2)
       allocate(U2(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2*K2*K4
       I3=K4
       call jungemm(I1,I2,I3,X2,D2,U2)
       deallocate(D2)
C
       call
     & sul345126(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U2,-1.0d0)
       call
     & sul245136(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U2, 1.0d0)
       call
     & sul145236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U2,-1.0d0)
       call
     & sul346125(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U2, 1.0d0)
       call
     & sul246135(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U2,-1.0d0)
       call
     & sul146235(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U2, 1.0d0)
       call
     & sul356124(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U2,-1.0d0)
       call
     & sul256134(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U2, 1.0d0)
       call
     & sul156234(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U2,-1.0d0)
       deallocate(U2)
       deallocate(X2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,t2B,D2)
       allocate(Q8(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,D2,Q8)
       deallocate(D1)
       deallocate(D2)
C
       call sul21(N2,N3,N2,N3,X5,Q8,-1.0d0)
       deallocate(Q8)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(Q10(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,D2,Q10)
       deallocate(D1)
       deallocate(D2)
C
       call sul21(N2,N3,N2,N3,X5,Q10,-0.5d0)
       deallocate(Q10)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S26(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S26)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N2,N2,N3,N2,N3,N0,N2,X7,S26, 1.0d0)
C
       call slx2413(N0,N3,N0,N2,N2,N3,N2,N3,N0,N2,X7,IntB, 1.0d0)
C
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef412356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,t3D,F2)
       allocate(U7(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K2*K4*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,X7,F2,U7)
       deallocate(F2)
C
       call
     & sul234516(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U7, 1.0d0)
       call
     & sul134526(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U7,-1.0d0)
       call
     & sul124536(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U7, 1.0d0)
       call
     & sul234615(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U7,-1.0d0)
       call
     & sul134625(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U7, 1.0d0)
       call
     & sul124635(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U7,-1.0d0)
       call
     & sul235614(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U7, 1.0d0)
       call
     & sul135624(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U7,-1.0d0)
       call
     & sul125634(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U7, 1.0d0)
       deallocate(U7)
       deallocate(X7)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3412(N2,N3,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,S26,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S46(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S46)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N2,N3,N2,N3,N2,N3,N0,N2,X13,S46, 1.0d0)
       deallocate(S46)
C
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2C,D2)
       allocate(U34(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2*K2*K4
       I3=K4
       call jungemm(I1,I2,I3,X13,D2,U34)
       deallocate(D2)
C
       call
     & sul256134(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U34,-1.0d0)
       call
     & sul356124(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U34, 1.0d0)
       call
     & sul156234(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U34, 1.0d0)
       call
     & sul156324(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U34,-1.0d0)
       call
     & sul356214(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U34,-1.0d0)
       call
     & sul256314(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U34, 1.0d0)
       call
     & sul246135(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U34, 1.0d0)
       call
     & sul346125(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U34,-1.0d0)
       call
     & sul146235(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U34,-1.0d0)
       call
     & sul146325(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U34, 1.0d0)
       call
     & sul346215(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U34, 1.0d0)
       call
     & sul246315(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U34,-1.0d0)
       call
     & sul345126(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U34, 1.0d0)
       call
     & sul245136(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U34,-1.0d0)
       call
     & sul345216(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U34,-1.0d0)
       call
     & sul245316(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U34, 1.0d0)
       call
     & sul145236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U34, 1.0d0)
       call
     & sul145326(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U34,-1.0d0)
       deallocate(U34)
       deallocate(X13)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef4312(N2,N3,N0,N2,N0,N2,N2,N3,
     & N2,N3,N0,N2,N2,N3,N0,N2,S26,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S45(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S45)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N2,N2,N3,N0,N2,N0,N2,X12,S45,-1.0d0)
       deallocate(S45)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(U33(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K2
       call jungemm(I1,I2,I3,X12,D2,U33)
       deallocate(D2)
C
       call
     & sul125346(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U33,-1.0d0)
       call
     & sul135246(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U33, 1.0d0)
       call
     & sul234156(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U33, 1.0d0)
       call
     & sul235146(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U33,-1.0d0)
       call
     & sul134256(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U33,-1.0d0)
       call
     & sul124356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U33, 1.0d0)
       call
     & sul126345(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U33, 1.0d0)
       call
     & sul136245(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U33,-1.0d0)
       call
     & sul234165(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U33,-1.0d0)
       call
     & sul236145(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U33, 1.0d0)
       call
     & sul134265(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U33, 1.0d0)
       call
     & sul124365(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U33,-1.0d0)
       call
     & sul126354(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U33,-1.0d0)
       call
     & sul136254(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U33, 1.0d0)
       call
     & sul235164(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U33, 1.0d0)
       call
     & sul236154(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U33,-1.0d0)
       call
     & sul135264(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U33,-1.0d0)
       call
     & sul125364(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U33, 1.0d0)
       deallocate(U33)
       deallocate(X12)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q16(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q16)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call relef21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q16,B1)
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
       call sul2341(N0,N2,N2,N3,N0,N2,N0,N2,X1,S48, 1.0d0)
       deallocate(S48)
C
       call slx3412(N0,N3,N0,N2,N2,N3,N0,N2,N0,N2,X1,IntB, 1.0d0)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(U1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K2
       call jungemm(I1,I2,I3,X1,D2,U1)
       deallocate(D2)
C
       call
     & sul234156(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U1, 1.0d0)
       call
     & sul134256(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U1,-1.0d0)
       call
     & sul124356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U1, 1.0d0)
       call
     & sul235146(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U1,-1.0d0)
       call
     & sul135246(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U1, 1.0d0)
       call
     & sul125346(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U1,-1.0d0)
       call
     & sul236145(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U1, 1.0d0)
       call
     & sul136245(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U1,-1.0d0)
       call
     & sul126345(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U1, 1.0d0)
       deallocate(U1)
       deallocate(X1)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef4123(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef412356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,t3D,F2)
       allocate(S25(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K2*K4
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,D1,F2,S25)
       deallocate(D1)
       deallocate(F2)
C
       call sul2341(N0,N2,N2,N3,N0,N2,N0,N2,X15,S25,-0.5d0)
       deallocate(S25)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(U37(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K2
       call jungemm(I1,I2,I3,X15,D2,U37)
       deallocate(D2)
C
       call
     & sul126345(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U37, 1.0d0)
       call
     & sul136245(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U37,-1.0d0)
       call
     & sul236145(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U37, 1.0d0)
       call
     & sul125346(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U37,-1.0d0)
       call
     & sul135246(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U37, 1.0d0)
       call
     & sul235146(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U37,-1.0d0)
       call
     & sul124356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U37, 1.0d0)
       call
     & sul134256(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U37,-1.0d0)
       call
     & sul234156(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U37, 1.0d0)
       deallocate(U37)
       deallocate(X15)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S40(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S40)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef2341(N2,N3,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,S40,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q15(N2+1:N3,N2+1:N3))
       I1=K4*K4
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q15)
       deallocate(D1)
       deallocate(B2)
C
       X5=X5-Q15
       deallocate(Q15)
C
       call slx12(0,N3,N2,N3,N2,N3,X5,FockB, 1.0d0)
C
       allocate(F2(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef123456(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,t3D,F2)
       allocate(U5(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2*K2*K2*K4*K4
       I3=K4
       call jungemm(I1,I2,I3,X5,F2,U5)
       deallocate(F2)
C
       call
     & sul234561(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5, 1.0d0)
       call
     & sul134562(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5,-1.0d0)
       call
     & sul124563(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5, 1.0d0)
       deallocate(U5)
       deallocate(X5)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef2341(N2,N3,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,S40,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S41(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       I1=K4*K4*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S41)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N2,N3,N2,N3,N2,N3,N2,N3,X8,S41, 1.0d0)
       deallocate(S41)
C
       call slx1234(N0,N3,N2,N3,N2,N3,N2,N3,N2,N3,X8,IntB, 1.0d0)
C
       allocate(F2(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef123456(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,t3D,F2)
       allocate(U8(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K2*K2*K2*K4
       I3=K4*K4
       call jungemm(I1,I2,I3,X8,F2,U8)
       deallocate(F2)
C
       call
     & sul345612(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U8, 0.5d0)
       call
     & sul245613(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U8,-0.5d0)
       call
     & sul145623(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U8, 0.5d0)
       deallocate(U8)
       deallocate(X8)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N0,N2,IntM,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(Q7(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,D1,D2,Q7)
       deallocate(D1)
       deallocate(D2)
C
       call sul21(N0,N2,N0,N2,X4,Q7, 1.0d0)
       deallocate(Q7)
C
       call slx21(0,N3,N0,N2,N0,N2,X4,FockB, 1.0d0)
C
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef412356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,t3D,F2)
       allocate(U4(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K2*K4*K4*K4
       I3=K2
       call jungemm(I1,I2,I3,X4,F2,U4)
       deallocate(F2)
C
       V3D=V3D-U4
       call
     & sul123465(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U4, 1.0d0)
       call
     & sul123564(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U4,-1.0d0)
       deallocate(U4)
       deallocate(X4)
C
       end
