       subroutine t2B_update(N0,N1,N2,N3,K1,K2,K3,K4,shift,V2B,
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
       real*8 V2B(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
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
       real*8,allocatable::S23(:,:,:,:)
       real*8,allocatable::U24(:,:,:,:)
       real*8,allocatable::S27(:,:,:,:)
       real*8,allocatable::S65(:,:,:,:)
       real*8,allocatable::U28(:,:,:,:)
       real*8,allocatable::U66(:,:,:,:)
       real*8,allocatable::S61(:,:,:,:)
       real*8,allocatable::U62(:,:,:,:)
       real*8,allocatable::S55(:,:,:,:)
       real*8,allocatable::U56(:,:,:,:)
       real*8,allocatable::S34(:,:,:,:)
       real*8,allocatable::S43(:,:,:,:)
       real*8,allocatable::S48(:,:,:,:)
       real*8,allocatable::S32(:,:,:,:)
       real*8,allocatable::S67(:,:,:,:)
       real*8,allocatable::X1(:,:,:,:)
       real*8,allocatable::U1(:,:,:,:)
       real*8,allocatable::X5(:,:,:,:)
       real*8,allocatable::U6(:,:,:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::Q6(:,:)
       real*8,allocatable::Q8(:,:)
       real*8,allocatable::Q11(:,:)
       real*8,allocatable::Q13(:,:)
       real*8,allocatable::Q3(:,:)
       real*8,allocatable::Q15(:,:)
       real*8,allocatable::Q16(:,:)
       real*8,allocatable::Q9(:,:)
       real*8,allocatable::Q18(:,:)
       real*8,allocatable::X3(:,:)
       real*8,allocatable::U4(:,:,:,:)
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::Q12(:,:)
       real*8,allocatable::Q14(:,:)
       real*8,allocatable::S41(:,:,:,:)
       real*8,allocatable::S69(:,:,:,:)
       real*8,allocatable::S71(:,:,:,:)
       real*8,allocatable::Q17(:,:)
       real*8,allocatable::X2(:,:)
       real*8,allocatable::U3(:,:,:,:)
       real*8,allocatable::S52(:,:,:,:)
       real*8,allocatable::S74(:,:,:,:)
       real*8,allocatable::X4(:,:,:,:)
       real*8,allocatable::U5(:,:,:,:)
       real*8,allocatable::S19(:,:,:,:)
       real*8,allocatable::S79(:,:,:,:)
       real*8,allocatable::X11(:,:,:,:)
       real*8,allocatable::U20(:,:,:,:)
       real*8,allocatable::X8(:,:,:,:)
       real*8,allocatable::U10(:,:,:,:)
       real*8,allocatable::S21(:,:,:,:)
       real*8,allocatable::S29(:,:,:,:)
       real*8,allocatable::S37(:,:,:,:)
       real*8,allocatable::S46(:,:,:,:)
       real*8,allocatable::S77(:,:,:,:)
       real*8,allocatable::X12(:,:,:,:)
       real*8,allocatable::U22(:,:,:,:)
       real*8,allocatable::X10(:,:,:,:)
       real*8,allocatable::U13(:,:,:,:)
       real*8,allocatable::Q4(:,:)
       real*8,allocatable::Q10(:,:)
       real*8,allocatable::X9(:,:)
       real*8,allocatable::U12(:,:,:,:)
       real*8,allocatable::X7(:,:)
       real*8,allocatable::U9(:,:,:,:)
       real*8,allocatable::S39(:,:,:,:)
       real*8,allocatable::S59(:,:,:,:)
       real*8,allocatable::X6(:,:,:,:)
       real*8,allocatable::U8(:,:,:,:)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       call reorder3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N2,N3,N2,N3,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S23(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       I1=K4*K4*K4
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S23)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder2341(N0,N2,N2,N3,N2,N3,N2,N3,
     & N2,N3,N2,N3,N2,N3,N0,N2,S23,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(U24(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,U24)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,N3,N2,N3,N0,N2,N0,N2,V2B,U24,-1.000)
       deallocate(U24)
       deallocate(S23)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder1432(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N0,N2,N0,N2,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S27(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S27)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder2341(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S27,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2B,D2)
       allocate(U28(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K4*K4
       I3=K2*K2
       call jungemm(I1,I2,I3,D1,D2,U28)
       deallocate(D1)
       deallocate(D2)
C
       V2B=V2B+0.500*U28
       call
     & sum1243(N2,N3,N2,N3,N0,N2,N0,N2,V2B,U28,-0.500)
       deallocate(U28)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder3241(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S27,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S65(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S65)
       deallocate(D1)
       deallocate(B2)
       deallocate(S27)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder2134(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N0,N2,S65,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(U66(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,U66)
       deallocate(D1)
       deallocate(B2)
C
       V2B=V2B-U66
       call
     & sum1243(N2,N3,N2,N3,N0,N2,N0,N2,V2B,U66, 1.000)
       deallocate(U66)
       deallocate(S65)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S61(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S61)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,S61,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,t2C,D2)
       allocate(U62(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,U62)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum1324(N2,N3,N2,N3,N0,N2,N0,N2,V2B,U62,-1.000)
       call
     & sum1423(N2,N3,N2,N3,N0,N2,N0,N2,V2B,U62, 1.000)
       deallocate(U62)
       deallocate(S61)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2B,D2)
       allocate(S55(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S55)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,S55,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2B,D2)
       allocate(U56(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,U56)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum1324(N2,N3,N2,N3,N0,N2,N0,N2,V2B,U56,-1.000)
       call
     & sum1423(N2,N3,N2,N3,N0,N2,N0,N2,V2B,U56, 1.000)
       deallocate(U56)
       deallocate(S55)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2B,D2)
       allocate(S34(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K2
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,D2,S34)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X1=0.0d0
       call sum3412(N0,N2,N2,N3,N0,N2,N0,N2,X1,S34,-0.500)
       deallocate(S34)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,t3B,F2)
       allocate(S43(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K2*K4
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,D1,F2,S43)
       deallocate(D1)
       deallocate(F2)
C
       call sum2341(N0,N2,N2,N3,N0,N2,N0,N2,X1,S43,-0.500)
       deallocate(S43)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N0,N2,IntM,D1)
       allocate(F2(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder613245(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N2,N3,N0,N2,N0,N2,t3D,F2)
       allocate(S48(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K2*K4
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,D1,F2,S48)
       deallocate(D1)
       deallocate(F2)
C
       call sum2341(N0,N2,N2,N3,N0,N2,N0,N2,X1,S48,-1.000)
       deallocate(S48)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S32(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S32)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X5(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       X5=0.0d0
       call sum4123(N0,N2,N2,N3,N2,N3,N0,N2,X5,S32,-1.000)
C
       call sumx3124(N0,N3,N0,N2,N2,N3,N2,N3,N0,N2,X5,IntB, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2B,D2)
       allocate(U6(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,X5,D2,U6)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,N2,N3,N0,N2,N0,N2,V2B,U6,-1.000)
       call
     & sum1423(N2,N3,N2,N3,N0,N2,N0,N2,V2B,U6, 1.000)
       call
     & sum2314(N2,N3,N2,N3,N0,N2,N0,N2,V2B,U6, 1.000)
       call
     & sum1324(N2,N3,N2,N3,N0,N2,N0,N2,V2B,U6,-1.000)
       deallocate(U6)
       deallocate(X5)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call reorder3241(N0,N2,N0,N2,N2,N3,N2,N3,
     & N2,N3,N0,N2,N2,N3,N0,N2,S32,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S67(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S67)
       deallocate(D1)
       deallocate(B2)
       deallocate(S32)
C
       call sum3124(N0,N2,N2,N3,N0,N2,N0,N2,X1,S67, 1.000)
       deallocate(S67)
C
       call sumx2143(N0,N3,N0,N2,N2,N3,N0,N2,N0,N2,X1,IntB, 1.000)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(U1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,X1,B2,U1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,N3,N0,N2,N0,N2,V2B,U1,-1.000)
       V2B=V2B+U1
       deallocate(U1)
       deallocate(X1)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3))
       call reorder3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N2,N3,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q2(N2+1:N3,N2+1:N3))
       I1=K4*K4
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q2)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N2+1:N3,N2+1:N3))
       X3=0.0d0
       X3=X3+Q2
       deallocate(Q2)
C
       allocate(B1(N0+1:N2,N2+1:N3))
       call reorder12(0,N3,0,N3,
     & N0,N2,N2,N3,FockB,B1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q6(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,B1,B2,Q6)
       deallocate(B1)
       deallocate(B2)
C
       call sum21(N2,N3,N2,N3,X3,Q6,-1.000)
       deallocate(Q6)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call reorder2341(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q8(N2+1:N3,N2+1:N3))
       I1=K4*K4
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q8)
       deallocate(D1)
       deallocate(B2)
C
       X3=X3-Q8
       deallocate(Q8)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2B,D2)
       allocate(Q11(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,D2,Q11)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N2,N3,N2,N3,X3,Q11,-0.500)
       deallocate(Q11)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call reorder4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,t2C,D2)
       allocate(Q13(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,D2,Q13)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N2,N3,N2,N3,X3,Q13, 1.000)
       deallocate(Q13)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N2,N3,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q3(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q3)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X7(N0+1:N2,N2+1:N3))
       X7=0.0d0
       X7=X7+Q3
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q3,B1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(Q15(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,B1,B2,Q15)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X2(N0+1:N2,N0+1:N2))
       X2=0.0d0
       call sum21(N0,N2,N0,N2,X2,Q15, 1.000)
       deallocate(Q15)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q16(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,Q3,B2,Q16)
       deallocate(B2)
       deallocate(Q3)
C
       call sum21(N2,N3,N2,N3,X3,Q16,-1.000)
       deallocate(Q16)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q9(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q9)
       deallocate(D1)
       deallocate(B2)
C
       X7=X7+Q9
C
       call sumx21(0,N3,N0,N2,N2,N3,X7,FockB, 1.000)
C
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,t3B,F2)
       allocate(U9(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I2=K2*K2*K4*K4
       I3=K4*K2
       call jungemm2(I2,I3,X7,F2,U9)
       deallocate(F2)
C
       V2B=V2B+U9
       deallocate(U9)
       deallocate(X7)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q18(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,Q9,B2,Q18)
       deallocate(B2)
       deallocate(Q9)
C
       call sum21(N2,N3,N2,N3,X3,Q18,-1.000)
       deallocate(Q18)
C
       call sumx21(0,N3,N2,N3,N2,N3,X3,FockB, 1.000)
C
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2B,D2)
       allocate(U4(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2*K2*K4
       I3=K4
       call jungemm(I1,I2,I3,X3,D2,U4)
       deallocate(D2)
C
       call
     & sum2341(N2,N3,N2,N3,N0,N2,N0,N2,V2B,U4, 1.000)
       call
     & sum1342(N2,N3,N2,N3,N0,N2,N0,N2,V2B,U4,-1.000)
       deallocate(U4)
       deallocate(X3)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N0,N2,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q1(N0+1:N2,N0+1:N2))
       I1=K2*K2
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q1)
       deallocate(D1)
       deallocate(B2)
C
       X2=X2+Q1
       deallocate(Q1)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(0,N3,0,N3,
     & N2,N3,N0,N2,FockB,B1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(Q5(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,B1,B2,Q5)
       deallocate(B1)
       deallocate(B2)
C
       call sum21(N0,N2,N0,N2,X2,Q5, 1.000)
       deallocate(Q5)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q7(N0+1:N2,N0+1:N2))
       I1=K2*K2
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q7)
       deallocate(D1)
       deallocate(B2)
C
       X2=X2+Q7
       deallocate(Q7)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2B,D2)
       allocate(Q12(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,D1,D2,Q12)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N0,N2,N0,N2,X2,Q12,-0.500)
       deallocate(Q12)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N0,N2,IntM,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2C,D2)
       allocate(Q14(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,D1,D2,Q14)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N0,N2,N0,N2,X2,Q14,-1.000)
       deallocate(Q14)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       call reorder1432(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N0,N2,N2,N3,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S41(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S41)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X8(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       X8=0.0d0
       call sum4123(N0,N2,N0,N2,N2,N3,N0,N2,X8,S41,-1.000)
C
       call sumx3421(N0,N3,N0,N2,N0,N2,N2,N3,N0,N2,X8,IntB, 1.000)
C
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,t3B,F2)
       allocate(U10(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K4*K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,X8,F2,U10)
       deallocate(F2)
C
       call
     & sum1243(N2,N3,N2,N3,N0,N2,N0,N2,V2B,U10, 0.500)
       V2B=V2B-0.500*U10
       deallocate(U10)
       deallocate(X8)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder4231(N0,N2,N0,N2,N0,N2,N2,N3,
     & N2,N3,N0,N2,N0,N2,N0,N2,S41,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S69(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S69)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       X4=0.0d0
       call sum3124(N0,N2,N0,N2,N0,N2,N0,N2,X4,S69, 1.000)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder2431(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,S41,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2B,D2)
       allocate(S71(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S71)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X12(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X12=0.0d0
       call sum2413(N0,N2,N2,N3,N0,N2,N0,N2,X12,S71,-1.000)
       deallocate(S71)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder3421(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,S41,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q17(N0+1:N2,N0+1:N2))
       I1=K2*K2
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q17)
       deallocate(D1)
       deallocate(B2)
       deallocate(S41)
C
       X2=X2-Q17
       deallocate(Q17)
C
       call sumx21(0,N3,N0,N2,N0,N2,X2,FockB, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2B,D2)
       allocate(U3(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K4*K4
       I3=K2
       call jungemm(I1,I2,I3,X2,D2,U3)
       deallocate(D2)
C
       call
     & sum1243(N2,N3,N2,N3,N0,N2,N0,N2,V2B,U3,-1.000)
       V2B=V2B+U3
       deallocate(U3)
       deallocate(X2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder2143(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2B,D2)
       allocate(S52(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K2
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,D2,S52)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N2,N0,N2,N0,N2,N0,N2,X4,S52,-0.500)
C
       call sumx4321(N0,N3,N0,N2,N0,N2,N0,N2,N0,N2,X4,IntB, 1.000)
C
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2B,D2)
       allocate(U5(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K4*K4
       I3=K2*K2
       call jungemm(I1,I2,I3,X4,D2,U5)
       deallocate(D2)
C
       V2B=V2B+0.500*U5
       deallocate(U5)
       deallocate(X4)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder4312(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S52,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S74(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S74)
       deallocate(D1)
       deallocate(B2)
       deallocate(S52)
C
       allocate(X11(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X11=0.0d0
       call sum2134(N0,N2,N2,N3,N0,N2,N0,N2,X11,S74,-0.500)
       deallocate(S74)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N0,N2,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S19(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S19)
       deallocate(D1)
       deallocate(B2)
C
       call sum2134(N0,N2,N2,N3,N0,N2,N0,N2,X11,S19, 1.000)
       deallocate(S19)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder3214(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S69,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S79(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S79)
       deallocate(D1)
       deallocate(B2)
       deallocate(S69)
C
       call sum2134(N0,N2,N2,N3,N0,N2,N0,N2,X11,S79, 1.000)
       deallocate(S79)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(U20(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,X11,B2,U20)
       deallocate(B2)
C
       V2B=V2B-U20
       deallocate(U20)
       deallocate(X11)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call reorder3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S21(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S21)
       deallocate(D1)
       deallocate(B2)
C
       call sum4123(N0,N2,N2,N3,N0,N2,N0,N2,X12,S21, 1.000)
       deallocate(S21)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder4132(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2B,D2)
       allocate(S29(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S29)
       deallocate(D1)
       deallocate(D2)
C
       call sum2413(N0,N2,N2,N3,N0,N2,N0,N2,X12,S29, 1.000)
       deallocate(S29)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N0,N2,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S37(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S37)
       deallocate(D1)
       deallocate(D2)
C
       call sum2413(N0,N2,N2,N3,N0,N2,N0,N2,X12,S37,-1.000)
       deallocate(S37)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       call reorder2431(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N0,N1,N1,N3,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S46(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S46)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X10(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       X10=0.0d0
       call sum4123(N0,N2,N0,N1,N1,N3,N0,N2,X10,S46, 1.000)
C
       call sumx3421(N0,N3,N0,N2,N0,N1,N1,N3,N0,N2,X10,IntM, 1.000)
C
       allocate(F2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder463125(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,N2,N3,N0,N2,t3D,F2)
       allocate(U13(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K4*K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,X10,F2,U13)
       deallocate(F2)
C
       call
     & sum1243(N2,N3,N2,N3,N0,N2,N0,N2,V2B,U13,-1.000)
       V2B=V2B+U13
       deallocate(U13)
       deallocate(X10)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call reorder3421(N0,N2,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N2,N0,N2,S46,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S77(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S77)
       deallocate(D1)
       deallocate(D2)
       deallocate(S46)
C
       call sum2413(N0,N2,N2,N3,N0,N2,N0,N2,X12,S77,-1.000)
       deallocate(S77)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(U22(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,X12,B2,U22)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,N3,N0,N2,N0,N2,V2B,U22,-1.000)
       V2B=V2B+U22
       call
     & sum2143(N2,N3,N2,N3,N0,N2,N0,N2,V2B,U22, 1.000)
       call
     & sum1243(N2,N3,N2,N3,N0,N2,N0,N2,V2B,U22,-1.000)
       deallocate(U22)
       deallocate(X12)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N1,N3,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q4(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q4)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X9(N0+1:N1,N1+1:N3))
       X9=0.0d0
       X9=X9+Q4
       deallocate(Q4)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N1,N3,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q10(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q10)
       deallocate(D1)
       deallocate(B2)
C
       X9=X9+Q10
       deallocate(Q10)
C
       call sumx21(0,N3,N0,N1,N1,N3,X9,FockR, 1.000)
C
       allocate(F2(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder631245(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N2,N3,N0,N2,N0,N2,t3D,F2)
       allocate(U12(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I2=K2*K2*K4*K4
       I3=K3*K1
       call jungemm2(I2,I3,X9,F2,U12)
       deallocate(F2)
C
       V2B=V2B+U12
       deallocate(U12)
       deallocate(X9)
C
       allocate(D1(N2+1:N3,N0+1:N1,N1+1:N3,N2+1:N3))
       call reorder2314(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S39(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       I1=K4*K3*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S39)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X6(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       X6=0.0d0
       call sum4123(N0,N1,N1,N3,N2,N3,N0,N2,X6,S39, 1.000)
       deallocate(S39)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2B,D2)
       allocate(S59(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S59)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N1,N1,N3,N2,N3,N0,N2,X6,S59,-1.000)
       deallocate(S59)
C
       call sumx2413(N0,N3,N0,N1,N1,N3,N2,N3,N0,N2,X6,IntM, 1.000)
C
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,t2C,D2)
       allocate(U8(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,X6,D2,U8)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,N2,N3,N0,N2,N0,N2,V2B,U8, 1.000)
       call
     & sum1423(N2,N3,N2,N3,N0,N2,N0,N2,V2B,U8,-1.000)
       call
     & sum2314(N2,N3,N2,N3,N0,N2,N0,N2,V2B,U8,-1.000)
       call
     & sum1324(N2,N3,N2,N3,N0,N2,N0,N2,V2B,U8, 1.000)
       deallocate(U8)
       deallocate(X6)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(U2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,U2)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N2,N3,N2,N3,N0,N2,N0,N2,V2B,U2,-1.000)
       call
     & sum3124(N2,N3,N2,N3,N0,N2,N0,N2,V2B,U2, 1.000)
       deallocate(U2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       call reorder4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2B,D2)
       allocate(U7(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K2*K2
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,D2,U7)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,N3,N2,N3,N0,N2,N0,N2,V2B,U7, 0.500)
       deallocate(U7)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call reorder2431(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,IntB,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,t3B,F2)
       allocate(U11(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2*K2*K4
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,D1,F2,U11)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,N3,N0,N2,N0,N2,V2B,U11, 0.500)
       call
     & sum1342(N2,N3,N2,N3,N0,N2,N0,N2,V2B,U11,-0.500)
       deallocate(U11)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call reorder3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N2,N3,IntM,D1)
       allocate(F2(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder613245(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N2,N3,N0,N2,N0,N2,t3D,F2)
       allocate(U14(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2*K2*K4
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,D1,F2,U14)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,N3,N0,N2,N0,N2,V2B,U14, 1.000)
       call
     & sum1342(N2,N3,N2,N3,N0,N2,N0,N2,V2B,U14,-1.000)
       deallocate(U14)
C
       do i=N0+1,N2-1
       do j=i+1,N2
       do a=N2+1,N3-1
       do b=a+1,N3
         CoeLeft=FockB(b,b)
     &          +FockB(a,a)
     &          -FockB(j,j)
     &          -FockB(i,i)
     &          +shift
         t2B(b,a,j,i)=t2B(b,a,j,i)-V2B(b,a,j,i)/CoeLeft
         t2B(b,a,i,j)=-t2B(b,a,j,i)
         t2B(a,b,j,i)=-t2B(b,a,j,i)
         t2B(a,b,i,j)= t2B(b,a,j,i)
       enddo
       enddo
       enddo
       enddo
C
       end
