       subroutine t2A_update(N0,N1,N2,N3,K1,K2,K3,K4,shift,V2A,
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
       real*8 V2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
C
       real*8,allocatable::B1(:,:)
       real*8,allocatable::B2(:,:)
       real*8,allocatable::C1(:,:,:)
       real*8,allocatable::C2(:,:,:)
       real*8,allocatable::D1(:,:,:,:)
       real*8,allocatable::D2(:,:,:,:)
       real*8,allocatable::F2(:,:,:,:,:,:)
C
       real*8,allocatable::U2(:,:,:,:)
       real*8,allocatable::U7(:,:,:,:)
       real*8,allocatable::U11(:,:,:,:)
       real*8,allocatable::U14(:,:,:,:)
       real*8,allocatable::S19(:,:,:,:)
       real*8,allocatable::U20(:,:,:,:)
       real*8,allocatable::S23(:,:,:,:)
       real*8,allocatable::S63(:,:,:,:)
       real*8,allocatable::U24(:,:,:,:)
       real*8,allocatable::U64(:,:,:,:)
       real*8,allocatable::S61(:,:,:,:)
       real*8,allocatable::U62(:,:,:,:)
       real*8,allocatable::S55(:,:,:,:)
       real*8,allocatable::U56(:,:,:,:)
       real*8,allocatable::S30(:,:,:,:)
       real*8,allocatable::S39(:,:,:,:)
       real*8,allocatable::S44(:,:,:,:)
       real*8,allocatable::S28(:,:,:,:)
       real*8,allocatable::S65(:,:,:,:)
       real*8,allocatable::X1(:,:,:,:)
       real*8,allocatable::U1(:,:,:,:)
       real*8,allocatable::X5(:,:,:,:)
       real*8,allocatable::U6(:,:,:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::Q4(:,:)
       real*8,allocatable::Q8(:,:)
       real*8,allocatable::Q11(:,:)
       real*8,allocatable::Q13(:,:)
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::Q16(:,:)
       real*8,allocatable::Q9(:,:)
       real*8,allocatable::Q18(:,:)
       real*8,allocatable::X3(:,:)
       real*8,allocatable::U4(:,:,:,:)
       real*8,allocatable::X7(:,:)
       real*8,allocatable::U9(:,:,:,:)
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::Q3(:,:)
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::Q12(:,:)
       real*8,allocatable::Q14(:,:)
       real*8,allocatable::S37(:,:,:,:)
       real*8,allocatable::S67(:,:,:,:)
       real*8,allocatable::S69(:,:,:,:)
       real*8,allocatable::Q15(:,:)
       real*8,allocatable::S42(:,:,:,:)
       real*8,allocatable::S75(:,:,:,:)
       real*8,allocatable::Q17(:,:)
       real*8,allocatable::X2(:,:)
       real*8,allocatable::U3(:,:,:,:)
       real*8,allocatable::S52(:,:,:,:)
       real*8,allocatable::S72(:,:,:,:)
       real*8,allocatable::X4(:,:,:,:)
       real*8,allocatable::U5(:,:,:,:)
       real*8,allocatable::S15(:,:,:,:)
       real*8,allocatable::S79(:,:,:,:)
       real*8,allocatable::X11(:,:,:,:)
       real*8,allocatable::U16(:,:,:,:)
       real*8,allocatable::X10(:,:,:,:)
       real*8,allocatable::U13(:,:,:,:)
       real*8,allocatable::X8(:,:,:,:)
       real*8,allocatable::U10(:,:,:,:)
       real*8,allocatable::S17(:,:,:,:)
       real*8,allocatable::S25(:,:,:,:)
       real*8,allocatable::S33(:,:,:,:)
       real*8,allocatable::X12(:,:,:,:)
       real*8,allocatable::U18(:,:,:,:)
       real*8,allocatable::Q6(:,:)
       real*8,allocatable::Q10(:,:)
       real*8,allocatable::X9(:,:)
       real*8,allocatable::U12(:,:,:,:)
       real*8,allocatable::S35(:,:,:,:)
       real*8,allocatable::S59(:,:,:,:)
       real*8,allocatable::X6(:,:,:,:)
       real*8,allocatable::U8(:,:,:,:)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       call reorder3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N1,N3,N1,N3,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S19(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       I1=K3*K3*K3
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S19)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder2341(N0,N1,N1,N3,N1,N3,N1,N3,
     & N1,N3,N1,N3,N1,N3,N0,N1,S19,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(U20(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,U20)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U20,-1.000)
       deallocate(U20)
       deallocate(S19)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       call reorder1432(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N0,N1,N0,N1,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S23(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S23)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call reorder2341(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,S23,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(U24(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K3*K3
       I3=K1*K1
       call jungemm(I1,I2,I3,D1,D2,U24)
       deallocate(D1)
       deallocate(D2)
C
       V2A=V2A+0.500*U24
       call
     & sum1243(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U24,-0.500)
       deallocate(U24)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call reorder3241(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,S23,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S63(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S63)
       deallocate(D1)
       deallocate(B2)
       deallocate(S23)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder2134(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N0,N1,S63,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(U64(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,U64)
       deallocate(D1)
       deallocate(B2)
C
       V2A=V2A-U64
       call
     & sum1243(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U64, 1.000)
       deallocate(U64)
       deallocate(S63)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2C,D2)
       allocate(S61(N1+1:N3,N0+1:N1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S61)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3412(N1,N3,N0,N1,N0,N2,N2,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,S61,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2C,D2)
       allocate(U62(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,U62)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum1324(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U62,-1.000)
       call
     & sum1423(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U62, 1.000)
       deallocate(U62)
       deallocate(S61)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S55(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S55)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,S55,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(U56(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,U56)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum1324(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U56,-1.000)
       call
     & sum1423(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U56, 1.000)
       deallocate(U56)
       deallocate(S55)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N0,N1,N1,N3,IntR,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(S30(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K1*K1
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,D2,S30)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       X1=0.0d0
       call sum3412(N0,N1,N1,N3,N0,N1,N0,N1,X1,S30,-0.500)
       deallocate(S30)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(F2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,t3A,F2)
       allocate(S39(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K1*K3
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,D1,F2,S39)
       deallocate(D1)
       deallocate(F2)
C
       call sum2341(N0,N1,N1,N3,N0,N1,N0,N1,X1,S39,-0.500)
       deallocate(S39)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder521346(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N1,N3,N0,N1,N0,N1,t3C,F2)
       allocate(S44(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K1*K3
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,D1,F2,S44)
       deallocate(D1)
       deallocate(F2)
C
       call sum2341(N0,N1,N1,N3,N0,N1,N0,N1,X1,S44,-1.000)
       deallocate(S44)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S28(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S28)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X5(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       X5=0.0d0
       call sum4123(N0,N1,N1,N3,N1,N3,N0,N1,X5,S28,-1.000)
C
       call sumx3124(N0,N3,N0,N1,N1,N3,N1,N3,N0,N1,X5,IntR, 1.000)
C
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(U6(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,X5,D2,U6)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U6,-1.000)
       call
     & sum1423(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U6, 1.000)
       call
     & sum2314(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U6, 1.000)
       call
     & sum1324(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U6,-1.000)
       deallocate(U6)
       deallocate(X5)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call reorder3241(N0,N1,N0,N1,N1,N3,N1,N3,
     & N1,N3,N0,N1,N1,N3,N0,N1,S28,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S65(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S65)
       deallocate(D1)
       deallocate(B2)
       deallocate(S28)
C
       call sum3124(N0,N1,N1,N3,N0,N1,N0,N1,X1,S65, 1.000)
       deallocate(S65)
C
       call sumx2143(N0,N3,N0,N1,N1,N3,N0,N1,N0,N1,X1,IntR, 1.000)
C
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(U1(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,X1,B2,U1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U1,-1.000)
       V2A=V2A+U1
       deallocate(U1)
       deallocate(X1)
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
       allocate(X3(N1+1:N3,N1+1:N3))
       X3=0.0d0
       call sum21(N1,N3,N1,N3,X3,Q2,-1.000)
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
       X3=X3-Q4
       deallocate(Q4)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call reorder2431(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q8(N1+1:N3,N1+1:N3))
       I1=K3*K3
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q8)
       deallocate(D1)
       deallocate(B2)
C
       X3=X3+Q8
       deallocate(Q8)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(Q11(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,D2,Q11)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N1,N3,N1,N3,X3,Q11,-0.500)
       deallocate(Q11)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2C,D2)
       allocate(Q13(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,Q13)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N1,N3,N1,N3,X3,Q13, 1.000)
       deallocate(Q13)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N1,N3,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q5(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q5)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X7(N0+1:N1,N1+1:N3))
       X7=0.0d0
       X7=X7+Q5
C
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q16(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,Q5,B2,Q16)
       deallocate(B2)
       deallocate(Q5)
C
       call sum21(N1,N3,N1,N3,X3,Q16,-1.000)
       deallocate(Q16)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N1,N3,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q9(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q9)
       deallocate(D1)
       deallocate(B2)
C
       X7=X7+Q9
C
       call sumx21(0,N3,N0,N1,N1,N3,X7,FockR, 1.000)
C
       allocate(F2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,t3A,F2)
       allocate(U9(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I2=K1*K1*K3*K3
       I3=K3*K1
       call jungemm2(I2,I3,X7,F2,U9)
       deallocate(F2)
C
       V2A=V2A+U9
       deallocate(U9)
       deallocate(X7)
C
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q18(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,Q9,B2,Q18)
       deallocate(B2)
       deallocate(Q9)
C
       call sum21(N1,N3,N1,N3,X3,Q18,-1.000)
       deallocate(Q18)
C
       call sumx21(0,N3,N1,N3,N1,N3,X3,FockR, 1.000)
C
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(U4(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1*K1*K3
       I3=K3
       call jungemm(I1,I2,I3,X3,D2,U4)
       deallocate(D2)
C
       call
     & sum2341(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U4, 1.000)
       call
     & sum1342(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U4,-1.000)
       deallocate(U4)
       deallocate(X3)
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
       allocate(X2(N0+1:N1,N0+1:N1))
       X2=0.0d0
       call sum21(N0,N1,N0,N1,X2,Q1, 1.000)
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
       X2=X2+Q3
       deallocate(Q3)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call reorder4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N0,N1,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q7(N0+1:N1,N0+1:N1))
       I1=K1*K1
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q7)
       deallocate(D1)
       deallocate(B2)
C
       X2=X2+Q7
       deallocate(Q7)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(Q12(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,D1,D2,Q12)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N0,N1,N0,N1,X2,Q12,-0.500)
       deallocate(Q12)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2C,D2)
       allocate(Q14(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,D1,D2,Q14)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N0,N1,N0,N1,X2,Q14,-1.000)
       deallocate(Q14)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       call reorder1432(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N0,N1,N1,N3,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S37(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S37)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X8(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       X8=0.0d0
       call sum4123(N0,N1,N0,N1,N1,N3,N0,N1,X8,S37,-1.000)
C
       call sumx3421(N0,N3,N0,N1,N0,N1,N1,N3,N0,N1,X8,IntR, 1.000)
C
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,t3A,F2)
       allocate(U10(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K3*K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,X8,F2,U10)
       deallocate(F2)
C
       call
     & sum1243(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U10, 0.500)
       V2A=V2A-0.500*U10
       deallocate(U10)
       deallocate(X8)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       call reorder4231(N0,N1,N0,N1,N0,N1,N1,N3,
     & N1,N3,N0,N1,N0,N1,N0,N1,S37,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S67(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S67)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       X4=0.0d0
       call sum3124(N0,N1,N0,N1,N0,N1,N0,N1,X4,S67, 1.000)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder2431(N0,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,N0,N1,S37,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S69(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S69)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X12(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       X12=0.0d0
       call sum2413(N0,N1,N1,N3,N0,N1,N0,N1,X12,S69,-1.000)
       deallocate(S69)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder3421(N0,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,N0,N1,S37,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q15(N0+1:N1,N0+1:N1))
       I1=K1*K1
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q15)
       deallocate(D1)
       deallocate(B2)
       deallocate(S37)
C
       X2=X2-Q15
       deallocate(Q15)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       call reorder1432(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N2,N0,N1,N2,N3,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S42(N0+1:N1,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4*K1*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S42)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X10(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       X10=0.0d0
       call sum4123(N0,N2,N0,N1,N2,N3,N0,N1,X10,S42, 1.000)
C
       call sumx4321(N0,N3,N0,N2,N0,N1,N2,N3,N0,N1,X10,IntM, 1.000)
C
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder542136(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,t3C,F2)
       allocate(U13(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K3*K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,X10,F2,U13)
       deallocate(F2)
C
       call
     & sum1243(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U13,-1.000)
       V2A=V2A+U13
       deallocate(U13)
       deallocate(X10)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call reorder2431(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,N0,N1,S42,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2C,D2)
       allocate(S75(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S75)
       deallocate(D1)
       deallocate(D2)
C
       call sum2413(N0,N1,N1,N3,N0,N1,N0,N1,X12,S75,-1.000)
       deallocate(S75)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call reorder2431(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,N0,N1,S42,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q17(N0+1:N1,N0+1:N1))
       I1=K1*K1
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q17)
       deallocate(D1)
       deallocate(B2)
       deallocate(S42)
C
       X2=X2+Q17
       deallocate(Q17)
C
       call sumx21(0,N3,N0,N1,N0,N1,X2,FockR, 1.000)
C
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(U3(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K3*K3
       I3=K1
       call jungemm(I1,I2,I3,X2,D2,U3)
       deallocate(D2)
C
       call
     & sum1243(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U3,-1.000)
       V2A=V2A+U3
       deallocate(U3)
       deallocate(X2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder2143(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N0,N1,N0,N1,IntR,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(S52(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K1
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,D2,S52)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N1,N0,N1,N0,N1,N0,N1,X4,S52,-0.500)
C
       call sumx4321(N0,N3,N0,N1,N0,N1,N0,N1,N0,N1,X4,IntR, 1.000)
C
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(U5(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K3*K3
       I3=K1*K1
       call jungemm(I1,I2,I3,X4,D2,U5)
       deallocate(D2)
C
       V2A=V2A+0.500*U5
       deallocate(U5)
       deallocate(X4)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call reorder4312(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,S52,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S72(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S72)
       deallocate(D1)
       deallocate(B2)
       deallocate(S52)
C
       allocate(X11(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       X11=0.0d0
       call sum2134(N0,N1,N1,N3,N0,N1,N0,N1,X11,S72,-0.500)
       deallocate(S72)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call reorder3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N0,N1,N0,N1,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S15(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S15)
       deallocate(D1)
       deallocate(B2)
C
       call sum2134(N0,N1,N1,N3,N0,N1,N0,N1,X11,S15, 1.000)
       deallocate(S15)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call reorder3214(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,S67,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S79(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S79)
       deallocate(D1)
       deallocate(B2)
       deallocate(S67)
C
       call sum2134(N0,N1,N1,N3,N0,N1,N0,N1,X11,S79, 1.000)
       deallocate(S79)
C
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(U16(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,X11,B2,U16)
       deallocate(B2)
C
       V2A=V2A-U16
       deallocate(U16)
       deallocate(X11)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call reorder3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S17(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S17)
       deallocate(D1)
       deallocate(B2)
C
       call sum4123(N0,N1,N1,N3,N0,N1,N0,N1,X12,S17, 1.000)
       deallocate(S17)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder4132(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N0,N1,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S25(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S25)
       deallocate(D1)
       deallocate(D2)
C
       call sum2413(N0,N1,N1,N3,N0,N1,N0,N1,X12,S25, 1.000)
       deallocate(S25)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call reorder4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N0,N1,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2C,D2)
       allocate(S33(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S33)
       deallocate(D1)
       deallocate(D2)
C
       call sum2413(N0,N1,N1,N3,N0,N1,N0,N1,X12,S33,-1.000)
       deallocate(S33)
C
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(U18(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,X12,B2,U18)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U18,-1.000)
       V2A=V2A+U18
       call
     & sum2143(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U18, 1.000)
       call
     & sum1243(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U18,-1.000)
       deallocate(U18)
       deallocate(X12)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N2,N3,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q6(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q6)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X9(N0+1:N2,N2+1:N3))
       X9=0.0d0
       X9=X9+Q6
       deallocate(Q6)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q10(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q10)
       deallocate(D1)
       deallocate(B2)
C
       X9=X9+Q10
       deallocate(Q10)
C
       call sumx21(0,N3,N0,N2,N2,N3,X9,FockB, 1.000)
C
       allocate(F2(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder521346(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N1,N3,N0,N1,N0,N1,t3C,F2)
       allocate(U12(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I2=K1*K1*K3*K3
       I3=K4*K2
       call jungemm2(I2,I3,X9,F2,U12)
       deallocate(F2)
C
       V2A=V2A+U12
       deallocate(U12)
       deallocate(X9)
C
       allocate(D1(N1+1:N3,N0+1:N2,N2+1:N3,N1+1:N3))
       call reorder3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N2,N2,N3,N1,N3,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S35(N0+1:N1,N0+1:N2,N2+1:N3,N1+1:N3))
       I1=K3*K4*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S35)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X6(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       X6=0.0d0
       call sum4123(N0,N2,N2,N3,N1,N3,N0,N1,X6,S35, 1.000)
       deallocate(S35)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N2,N3,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S59(N1+1:N3,N0+1:N1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S59)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N2,N2,N3,N1,N3,N0,N1,X6,S59,-1.000)
       deallocate(S59)
C
       call sumx3142(N0,N3,N0,N2,N2,N3,N1,N3,N0,N1,X6,IntM, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2C,D2)
       allocate(U8(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,X6,D2,U8)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U8, 1.000)
       call
     & sum1423(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U8,-1.000)
       call
     & sum2314(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U8,-1.000)
       call
     & sum1324(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U8, 1.000)
       deallocate(U8)
       deallocate(X6)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(U2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,U2)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U2,-1.000)
       call
     & sum3124(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U2, 1.000)
       deallocate(U2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       call reorder4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(U7(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K1*K1
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,D2,U7)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U7, 0.500)
       deallocate(U7)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call reorder2431(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,IntR,D1)
       allocate(F2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,t3A,F2)
       allocate(U11(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1*K1*K3
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,D1,F2,U11)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U11, 0.500)
       call
     & sum1342(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U11,-0.500)
       deallocate(U11)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call reorder2431(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,IntM,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder521346(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N1,N3,N0,N1,N0,N1,t3C,F2)
       allocate(U14(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1*K1*K3
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,D1,F2,U14)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U14, 1.000)
       call
     & sum1342(N1,N3,N1,N3,N0,N1,N0,N1,V2A,U14,-1.000)
       deallocate(U14)
C
       do i=N0+1,N1-1
       do j=i+1,N1
       do a=N1+1,N3-1
       do b=a+1,N3
         CoeLeft=FockR(b,b)
     &          +FockR(a,a)
     &          -FockR(j,j)
     &          -FockR(i,i)
     &          +shift
         t2A(b,a,j,i)=t2A(b,a,j,i)-V2A(b,a,j,i)/CoeLeft
         t2A(b,a,i,j)=-t2A(b,a,j,i)
         t2A(a,b,j,i)=-t2A(b,a,j,i)
         t2A(a,b,i,j)= t2A(b,a,j,i)
       enddo
       enddo
       enddo
       enddo
C
       end
