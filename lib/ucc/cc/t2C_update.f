       subroutine t2C_update(N0,N1,N2,N3,K1,K2,K3,K4,shift,V2C,
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
       real*8 V2C(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
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
       real*8,allocatable::U16(:,:,:,:)
       real*8,allocatable::U19(:,:,:,:)
       real*8,allocatable::U21(:,:,:,:)
       real*8,allocatable::U24(:,:,:,:)
       real*8,allocatable::U26(:,:,:,:)
       real*8,allocatable::S27(:,:,:,:)
       real*8,allocatable::S31(:,:,:,:)
       real*8,allocatable::S37(:,:,:,:)
       real*8,allocatable::S43(:,:,:,:)
       real*8,allocatable::S51(:,:,:,:)
       real*8,allocatable::S56(:,:,:,:)
       real*8,allocatable::S61(:,:,:,:)
       real*8,allocatable::S66(:,:,:,:)
       real*8,allocatable::S54(:,:,:,:)
       real*8,allocatable::S125(:,:,:,:)
       real*8,allocatable::S64(:,:,:,:)
       real*8,allocatable::S142(:,:,:,:)
       real*8,allocatable::S137(:,:,:,:)
       real*8,allocatable::S155(:,:,:,:)
       real*8,allocatable::Q28(:,:)
       real*8,allocatable::S127(:,:,:,:)
       real*8,allocatable::S59(:,:,:,:)
       real*8,allocatable::Q25(:,:)
       real*8,allocatable::S129(:,:,:,:)
       real*8,allocatable::S93(:,:,:,:)
       real*8,allocatable::S149(:,:,:,:)
       real*8,allocatable::S139(:,:,:,:)
       real*8,allocatable::X1(:,:,:,:)
       real*8,allocatable::U1(:,:,:,:)
       real*8,allocatable::S29(:,:,:,:)
       real*8,allocatable::S33(:,:,:,:)
       real*8,allocatable::S69(:,:,:,:)
       real*8,allocatable::S71(:,:,:,:)
       real*8,allocatable::S77(:,:,:,:)
       real*8,allocatable::S85(:,:,:,:)
       real*8,allocatable::S90(:,:,:,:)
       real*8,allocatable::S95(:,:,:,:)
       real*8,allocatable::S100(:,:,:,:)
       real*8,allocatable::S49(:,:,:,:)
       real*8,allocatable::S123(:,:,:,:)
       real*8,allocatable::S83(:,:,:,:)
       real*8,allocatable::S133(:,:,:,:)
       real*8,allocatable::S39(:,:,:,:)
       real*8,allocatable::S135(:,:,:,:)
       real*8,allocatable::S115(:,:,:,:)
       real*8,allocatable::S145(:,:,:,:)
       real*8,allocatable::S98(:,:,:,:)
       real*8,allocatable::Q31(:,:)
       real*8,allocatable::S151(:,:,:,:)
       real*8,allocatable::X2(:,:,:,:)
       real*8,allocatable::U3(:,:,:,:)
       real*8,allocatable::X12(:,:,:,:)
       real*8,allocatable::U13(:,:,:,:)
       real*8,allocatable::S103(:,:,:,:)
       real*8,allocatable::X5(:,:,:,:)
       real*8,allocatable::U6(:,:,:,:)
       real*8,allocatable::X19(:,:,:,:)
       real*8,allocatable::U23(:,:,:,:)
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::Q9(:,:)
       real*8,allocatable::Q11(:,:)
       real*8,allocatable::Q19(:,:)
       real*8,allocatable::Q24(:,:)
       real*8,allocatable::Q8(:,:)
       real*8,allocatable::Q29(:,:)
       real*8,allocatable::Q27(:,:)
       real*8,allocatable::X8(:,:)
       real*8,allocatable::U9(:,:,:,:)
       real*8,allocatable::Q6(:,:)
       real*8,allocatable::Q10(:,:)
       real*8,allocatable::Q12(:,:)
       real*8,allocatable::Q20(:,:)
       real*8,allocatable::Q22(:,:)
       real*8,allocatable::Q16(:,:)
       real*8,allocatable::Q32(:,:)
       real*8,allocatable::X9(:,:)
       real*8,allocatable::U10(:,:,:,:)
       real*8,allocatable::X18(:,:)
       real*8,allocatable::U22(:,:,:,:)
       real*8,allocatable::X17(:,:,:,:)
       real*8,allocatable::U20(:,:,:,:)
       real*8,allocatable::S46(:,:,:,:)
       real*8,allocatable::S105(:,:,:,:)
       real*8,allocatable::S118(:,:,:,:)
       real*8,allocatable::X10(:,:,:,:)
       real*8,allocatable::U11(:,:,:,:)
       real*8,allocatable::S80(:,:,:,:)
       real*8,allocatable::S109(:,:,:,:)
       real*8,allocatable::X11(:,:,:,:)
       real*8,allocatable::U12(:,:,:,:)
       real*8,allocatable::S73(:,:,:,:)
       real*8,allocatable::X4(:,:,:,:)
       real*8,allocatable::U5(:,:,:,:)
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::Q3(:,:)
       real*8,allocatable::Q13(:,:)
       real*8,allocatable::Q17(:,:)
       real*8,allocatable::Q23(:,:)
       real*8,allocatable::X6(:,:)
       real*8,allocatable::U7(:,:,:,:)
       real*8,allocatable::S88(:,:,:,:)
       real*8,allocatable::S120(:,:,:,:)
       real*8,allocatable::X14(:,:,:,:)
       real*8,allocatable::U15(:,:,:,:)
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::Q26(:,:)
       real*8,allocatable::Q15(:,:)
       real*8,allocatable::Q30(:,:)
       real*8,allocatable::X15(:,:)
       real*8,allocatable::U17(:,:,:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::Q4(:,:)
       real*8,allocatable::Q14(:,:)
       real*8,allocatable::Q18(:,:)
       real*8,allocatable::Q21(:,:)
       real*8,allocatable::X7(:,:)
       real*8,allocatable::U8(:,:,:,:)
       real*8,allocatable::S35(:,:,:,:)
       real*8,allocatable::X3(:,:,:,:)
       real*8,allocatable::U4(:,:,:,:)
       real*8,allocatable::X20(:,:,:,:)
       real*8,allocatable::U25(:,:,:,:)
       real*8,allocatable::X13(:,:,:,:)
       real*8,allocatable::U14(:,:,:,:)
       real*8,allocatable::X16(:,:,:,:)
       real*8,allocatable::U18(:,:,:,:)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       call reorder1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N2,N3,N0,N2,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S27(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S27)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       X1=0.0d0
       call sum4123(N0,N1,N2,N3,N0,N2,N0,N1,X1,S27, 1.000)
       deallocate(S27)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:N3,N0+1:N1))
       call reorder2341(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N1,N2,N3,N0,N1,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S31(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       I1=K1*K4*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S31)
       deallocate(D1)
       deallocate(B2)
C
       call sum3124(N0,N1,N2,N3,N0,N2,N0,N1,X1,S31, 1.000)
       deallocate(S31)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call reorder4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N0,N1,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2B,D2)
       allocate(S37(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S37)
       deallocate(D1)
       deallocate(D2)
C
       call sum2314(N0,N1,N2,N3,N0,N2,N0,N1,X1,S37,-1.000)
       deallocate(S37)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder4132(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N0,N1,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S43(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S43)
       deallocate(D1)
       deallocate(D2)
C
       call sum2314(N0,N1,N2,N3,N0,N2,N0,N1,X1,S43, 1.000)
       deallocate(S43)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,N0+1:N2))
       call reorder4132(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N0,N1,N0,N2,IntM,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,N3,N0,N1,t2C,D2)
       allocate(S51(N2+1:N3,N0+1:N1,N0+1:N1,N0+1:N2))
       I1=K2*K1
       I2=K1*K4
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,D2,S51)
       deallocate(D1)
       deallocate(D2)
C
       call sum2413(N0,N1,N2,N3,N0,N2,N0,N1,X1,S51, 1.000)
       deallocate(S51)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N1,N2+1:N3))
       call reorder2134(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N0,N1,N2,N3,IntM,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,t2C,D2)
       allocate(S56(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3))
       I1=K4*K1
       I2=K1*K2
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,D2,S56)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N1,N2,N3,N0,N2,N0,N1,X1,S56,-1.000)
       deallocate(S56)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(F2(N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call reorder413256(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N1,N3,N1,N3,N2,N3,N0,N2,N0,N1,t3C,F2)
       allocate(S61(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K2*K4
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,D1,F2,S61)
       deallocate(D1)
       deallocate(F2)
C
       call sum2341(N0,N1,N2,N3,N0,N2,N0,N1,X1,S61,-0.500)
       deallocate(S61)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call reorder413256(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N2,N3,N0,N2,N0,N1,t3D,F2)
       allocate(S66(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K2*K4
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,D1,F2,S66)
       deallocate(D1)
       deallocate(F2)
C
       call sum2341(N0,N1,N2,N3,N0,N2,N0,N1,X1,S66,-1.000)
       deallocate(S66)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:N3,N2+1:N3))
       call reorder1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N2,N3,N2,N3,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S54(N0+1:N1,N0+1:N1,N2+1:N3,N2+1:N3))
       I1=K4*K4*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S54)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X13(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       X13=0.0d0
       call sum4123(N0,N1,N2,N3,N2,N3,N0,N1,X13,S54, 1.000)
C
       call sumx4213(N0,N3,N0,N1,N2,N3,N2,N3,N0,N1,X13,IntM, 1.000)
C
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2C,D2)
       allocate(U14(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K2*K3
       I3=K4*K1
       call jungemm(I1,I2,I3,X13,D2,U14)
       deallocate(D2)
C
       call
     & sum2314(N2,N3,N1,N3,N0,N2,N0,N1,V2C,U14,-1.000)
       deallocate(U14)
       deallocate(X13)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:N3,N0+1:N1))
       call reorder3241(N0,N1,N0,N1,N2,N3,N2,N3,
     & N2,N3,N0,N1,N2,N3,N0,N1,S54,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S125(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       I1=K1*K4*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S125)
       deallocate(D1)
       deallocate(B2)
       deallocate(S54)
C
       call sum3124(N0,N1,N2,N3,N0,N2,N0,N1,X1,S125, 1.000)
       deallocate(S125)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       call reorder1432(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N2,N0,N1,N2,N3,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S64(N0+1:N1,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4*K1*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S64)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X20(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       X20=0.0d0
       call sum4123(N0,N2,N0,N1,N2,N3,N0,N1,X20,S64, 1.000)
C
       call sumx4321(N0,N3,N0,N2,N0,N1,N2,N3,N0,N1,X20,IntM, 1.000)
C
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder461235(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,t3D,F2)
       allocate(U25(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1
       I2=K2*K3*K4
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,X20,F2,U25)
       deallocate(F2)
C
       V2C=V2C-U25
       deallocate(U25)
       deallocate(X20)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       call reorder3421(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N1,N2,N3,N0,N2,N0,N1,S64,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2C,D2)
       allocate(S142(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K2*K3
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,D2,S142)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X2(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1))
       X2=0.0d0
       call sum2314(N0,N2,N1,N3,N0,N2,N0,N1,X2,S142, 1.000)
       deallocate(S142)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       call reorder4231(N0,N1,N0,N2,N0,N1,N2,N3,
     & N2,N3,N0,N2,N0,N1,N0,N1,S64,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S137(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S137)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X12(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       X12=0.0d0
       call sum3124(N0,N2,N0,N1,N0,N2,N0,N1,X12,S137, 1.000)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:N2,N0+1:N1))
       call reorder3214(N0,N2,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N2,N0,N2,N0,N1,S137,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S155(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S155)
       deallocate(D1)
       deallocate(B2)
       deallocate(S137)
C
       call sum2134(N0,N2,N1,N3,N0,N2,N0,N1,X2,S155,-1.000)
       deallocate(S155)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call reorder2431(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,N0,N1,S64,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q28(N0+1:N1,N0+1:N1))
       I1=K1*K1
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q28)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X6(N0+1:N1,N0+1:N1))
       X6=0.0d0
       X6=X6+Q28
       deallocate(Q28)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call reorder2431(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,N0,N1,S64,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2B,D2)
       allocate(S127(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S127)
       deallocate(D1)
       deallocate(D2)
       deallocate(S64)
C
       call sum2314(N0,N1,N2,N3,N0,N2,N0,N1,X1,S127,-1.000)
       deallocate(S127)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       call reorder1432(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N0,N1,N1,N3,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S59(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S59)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X16(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       X16=0.0d0
       call sum4123(N0,N1,N0,N1,N1,N3,N0,N1,X16,S59,-1.000)
C
       call sumx3421(N0,N3,N0,N1,N0,N1,N1,N3,N0,N1,X16,IntR, 1.000)
C
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder461235(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,N1,N1,N3,N2,N3,N1,N3,N0,N2,t3C,F2)
       allocate(U18(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1
       I2=K2*K3*K4
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,X16,F2,U18)
       deallocate(F2)
C
       V2C=V2C+0.500*U18
       deallocate(U18)
       deallocate(X16)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder3421(N0,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,N0,N1,S59,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q25(N0+1:N1,N0+1:N1))
       I1=K1*K1
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q25)
       deallocate(D1)
       deallocate(B2)
C
       X6=X6-Q25
       deallocate(Q25)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder2431(N0,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,N0,N1,S59,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S129(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S129)
       deallocate(D1)
       deallocate(D2)
       deallocate(S59)
C
       call sum2314(N0,N1,N2,N3,N0,N2,N0,N1,X1,S129,-1.000)
       deallocate(S129)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       call reorder2431(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N0,N1,N1,N3,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S93(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S93)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X17(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       X17=0.0d0
       call sum4123(N0,N2,N0,N1,N1,N3,N0,N2,X17,S93, 1.000)
C
       call sumx3421(N0,N3,N0,N2,N0,N1,N1,N3,N0,N2,X17,IntM, 1.000)
C
       allocate(F2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder541236(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,N1,N3,N0,N1,t3C,F2)
       allocate(U20(N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N2))
       I1=K2
       I2=K1*K3*K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,X17,F2,U20)
       deallocate(F2)
C
       call
     & sum1243(N2,N3,N1,N3,N0,N2,N0,N1,V2C,U20,-1.000)
       deallocate(U20)
       deallocate(X17)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call reorder3421(N0,N2,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N2,N0,N2,S93,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S149(N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S149)
       deallocate(D1)
       deallocate(D2)
C
       call sum2413(N0,N2,N1,N3,N0,N2,N0,N1,X2,S149,-1.000)
       deallocate(S149)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,N0+1:N2))
       call reorder2431(N0,N2,N0,N2,N0,N1,N1,N3,
     & N0,N2,N1,N3,N0,N1,N0,N2,S93,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,N3,N0,N1,t2C,D2)
       allocate(S139(N2+1:N3,N0+1:N1,N0+1:N1,N0+1:N2))
       I1=K2*K1
       I2=K1*K4
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,D2,S139)
       deallocate(D1)
       deallocate(D2)
       deallocate(S93)
C
       call sum2413(N0,N1,N2,N3,N0,N2,N0,N1,X1,S139, 1.000)
       deallocate(S139)
C
       call sumx4312(N0,N3,N0,N1,N2,N3,N0,N2,N0,N1,X1,IntM, 1.000)
C
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(U1(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K4
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,X1,B2,U1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N1,N3,N0,N2,N0,N1,V2C,U1, 1.000)
       deallocate(U1)
       deallocate(X1)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:N2,N0+1:N1))
       call reorder3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N2,N0,N2,N0,N1,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S29(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S29)
       deallocate(D1)
       deallocate(B2)
C
       call sum2134(N0,N2,N1,N3,N0,N2,N0,N1,X2,S29,-1.000)
       deallocate(S29)
C
       allocate(D1(N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N2))
       call reorder3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N2,N1,N3,N0,N2,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S33(N0+1:N1,N0+1:N2,N1+1:N3,N0+1:N2))
       I1=K2*K3*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S33)
       deallocate(D1)
       deallocate(B2)
C
       call sum4123(N0,N2,N1,N3,N0,N2,N0,N1,X2,S33, 1.000)
       deallocate(S33)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call reorder4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N1,N3,N0,N1,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S69(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S69)
       deallocate(D1)
       deallocate(B2)
C
       call sum3124(N0,N2,N1,N3,N0,N2,N0,N1,X2,S69, 1.000)
       deallocate(S69)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N0,N2,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S71(N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S71)
       deallocate(D1)
       deallocate(D2)
C
       call sum2413(N0,N2,N1,N3,N0,N2,N0,N1,X2,S71,-1.000)
       deallocate(S71)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder4132(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2C,D2)
       allocate(S77(N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S77)
       deallocate(D1)
       deallocate(D2)
C
       call sum2413(N0,N2,N1,N3,N0,N2,N0,N1,X2,S77, 1.000)
       deallocate(S77)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       call reorder3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N0,N2,N0,N1,IntM,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2C,D2)
       allocate(S85(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K2*K3
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,D2,S85)
       deallocate(D1)
       deallocate(D2)
C
       call sum2314(N0,N2,N1,N3,N0,N2,N0,N1,X2,S85, 1.000)
       deallocate(S85)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3))
       call reorder4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N0,N2,N1,N3,IntM,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,t2C,D2)
       allocate(S90(N0+1:N2,N0+1:N1,N0+1:N2,N1+1:N3))
       I1=K3*K2
       I2=K1*K2
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,D2,S90)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N2,N1,N3,N0,N2,N0,N1,X2,S90,-1.000)
       deallocate(S90)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N0,N2,IntM,D1)
       allocate(F2(N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder421356(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,t3C,F2)
       allocate(S95(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
       I1=K2
       I2=K1*K2*K3
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,D1,F2,S95)
       deallocate(D1)
       deallocate(F2)
C
       call sum2341(N0,N2,N1,N3,N0,N2,N0,N1,X2,S95,-1.000)
       deallocate(S95)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder412356(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N1,N3,N0,N2,N0,N1,t3D,F2)
       allocate(S100(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
       I1=K2
       I2=K1*K2*K3
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,D1,F2,S100)
       deallocate(D1)
       deallocate(F2)
C
       call sum2341(N0,N2,N1,N3,N0,N2,N0,N1,X2,S100,-0.500)
       deallocate(S100)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
       call reorder1432(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N2,N0,N1,N0,N2,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S49(N0+1:N1,N0+1:N2,N0+1:N1,N0+1:N2))
       I1=K2*K1*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S49)
       deallocate(D1)
       deallocate(B2)
C
       call sum4123(N0,N2,N0,N1,N0,N2,N0,N1,X12,S49, 1.000)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:N2,N0+1:N1))
       call reorder3241(N0,N1,N0,N2,N0,N1,N0,N2,
     & N0,N1,N0,N2,N0,N2,N0,N1,S49,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S123(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S123)
       deallocate(D1)
       deallocate(B2)
       deallocate(S49)
C
       call sum2134(N0,N2,N1,N3,N0,N2,N0,N1,X2,S123,-1.000)
       deallocate(S123)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       call reorder2431(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N0,N1,N0,N1,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S83(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S83)
       deallocate(D1)
       deallocate(B2)
C
       call sum3124(N0,N2,N0,N1,N0,N2,N0,N1,X12,S83, 1.000)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:N2,N0+1:N1))
       call reorder3214(N0,N2,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N2,N0,N2,N0,N1,S83,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S133(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S133)
       deallocate(D1)
       deallocate(B2)
       deallocate(S83)
C
       call sum2134(N0,N2,N1,N3,N0,N2,N0,N1,X2,S133,-1.000)
       deallocate(S133)
C
       allocate(D1(N1+1:N3,N0+1:N2,N2+1:N3,N1+1:N3))
       call reorder3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N2,N2,N3,N1,N3,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S39(N0+1:N1,N0+1:N2,N2+1:N3,N1+1:N3))
       I1=K3*K4*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S39)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X5(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       X5=0.0d0
       call sum4123(N0,N2,N2,N3,N1,N3,N0,N1,X5,S39, 1.000)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call reorder3241(N0,N1,N0,N2,N2,N3,N1,N3,
     & N2,N3,N0,N2,N1,N3,N0,N1,S39,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S135(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S135)
       deallocate(D1)
       deallocate(B2)
       deallocate(S39)
C
       call sum3124(N0,N2,N1,N3,N0,N2,N0,N1,X2,S135, 1.000)
       deallocate(S135)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder2143(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N0,N2,N0,N1,IntM,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,t2C,D2)
       allocate(S115(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K1*K2
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,D2,S115)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N2,N0,N1,N0,N2,N0,N1,X12,S115,-1.000)
C
       call sumx4321(N0,N3,N0,N2,N0,N1,N0,N2,N0,N1,X12,IntM, 1.000)
C
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2C,D2)
       allocate(U13(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K3*K4
       I3=K1*K2
       call jungemm(I1,I2,I3,X12,D2,U13)
       deallocate(D2)
C
       V2C=V2C+U13
       deallocate(U13)
       deallocate(X12)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:N2,N0+1:N1))
       call reorder4312(N0,N2,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,N2,N0,N2,N0,N1,S115,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S145(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S145)
       deallocate(D1)
       deallocate(B2)
       deallocate(S115)
C
       call sum2134(N0,N2,N1,N3,N0,N2,N0,N1,X2,S145, 1.000)
       deallocate(S145)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       call reorder1432(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N0,N2,N2,N3,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S98(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S98)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X19(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       X19=0.0d0
       call sum4123(N0,N2,N0,N2,N2,N3,N0,N2,X19,S98,-1.000)
C
       call sumx3421(N0,N3,N0,N2,N0,N2,N2,N3,N0,N2,X19,IntB, 1.000)
C
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,t3D,F2)
       allocate(U23(N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N2))
       I1=K2
       I2=K1*K3*K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,X19,F2,U23)
       deallocate(F2)
C
       call
     & sum1243(N2,N3,N1,N3,N0,N2,N0,N1,V2C,U23, 0.500)
       deallocate(U23)
       deallocate(X19)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder3421(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,S98,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q31(N0+1:N2,N0+1:N2))
       I1=K2*K2
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q31)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X8(N0+1:N2,N0+1:N2))
       X8=0.0d0
       X8=X8-Q31
       deallocate(Q31)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder2431(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,S98,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2C,D2)
       allocate(S151(N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S151)
       deallocate(D1)
       deallocate(D2)
       deallocate(S98)
C
       call sum2413(N0,N2,N1,N3,N0,N2,N0,N1,X2,S151,-1.000)
       deallocate(S151)
C
       call sumx2143(N0,N3,N0,N2,N1,N3,N0,N2,N0,N1,X2,IntM, 1.000)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(U3(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K3
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,X2,B2,U3)
       deallocate(B2)
C
       V2C=V2C+U3
       deallocate(U3)
       deallocate(X2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N2,N3,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S103(N1+1:N3,N0+1:N1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S103)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N2,N2,N3,N1,N3,N0,N1,X5,S103,-1.000)
       deallocate(S103)
C
       call sumx3142(N0,N3,N0,N2,N2,N3,N1,N3,N0,N1,X5,IntM, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2B,D2)
       allocate(U6(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,X5,D2,U6)
       deallocate(D2)
C
       call
     & sum1324(N2,N3,N1,N3,N0,N2,N0,N1,V2C,U6, 1.000)
       deallocate(U6)
       deallocate(X5)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N0,N2,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q5(N0+1:N2,N0+1:N2))
       I1=K2*K2
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q5)
       deallocate(D1)
       deallocate(B2)
C
       X8=X8+Q5
       deallocate(Q5)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(0,N3,0,N3,
     & N2,N3,N0,N2,FockB,B1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(Q9(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,B1,B2,Q9)
       deallocate(B1)
       deallocate(B2)
C
       call sum21(N0,N2,N0,N2,X8,Q9, 1.000)
       deallocate(Q9)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q11(N0+1:N2,N0+1:N2))
       I1=K2*K2
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q11)
       deallocate(D1)
       deallocate(B2)
C
       X8=X8+Q11
       deallocate(Q11)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2B,D2)
       allocate(Q19(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,D1,D2,Q19)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N0,N2,N0,N2,X8,Q19, 0.500)
       deallocate(Q19)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N0,N2,IntM,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2C,D2)
       allocate(Q24(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,D1,D2,Q24)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N0,N2,N0,N2,X8,Q24,-1.000)
       deallocate(Q24)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N2,N3,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q8(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q8)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X18(N0+1:N2,N2+1:N3))
       X18=0.0d0
       X18=X18+Q8
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q29(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,Q8,B2,Q29)
       deallocate(B2)
C
       allocate(X9(N2+1:N3,N2+1:N3))
       X9=0.0d0
       call sum21(N2,N3,N2,N3,X9,Q29,-1.000)
       deallocate(Q29)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q8,B1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(Q27(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,B1,B2,Q27)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q8)
C
       call sum21(N0,N2,N0,N2,X8,Q27, 1.000)
       deallocate(Q27)
C
       call sumx21(0,N3,N0,N2,N0,N2,X8,FockB, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2C,D2)
       allocate(U9(N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N2))
       I1=K2
       I2=K1*K3*K4
       I3=K2
       call jungemm(I1,I2,I3,X8,D2,U9)
       deallocate(D2)
C
       call
     & sum1243(N2,N3,N1,N3,N0,N2,N0,N1,V2C,U9,-1.000)
       deallocate(U9)
       deallocate(X8)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3))
       call reorder3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N2,N3,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q6(N2+1:N3,N2+1:N3))
       I1=K4*K4
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q6)
       deallocate(D1)
       deallocate(B2)
C
       X9=X9+Q6
       deallocate(Q6)
C
       allocate(B1(N0+1:N2,N2+1:N3))
       call reorder12(0,N3,0,N3,
     & N0,N2,N2,N3,FockB,B1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q10(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,B1,B2,Q10)
       deallocate(B1)
       deallocate(B2)
C
       call sum21(N2,N3,N2,N3,X9,Q10,-1.000)
       deallocate(Q10)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call reorder2341(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q12(N2+1:N3,N2+1:N3))
       I1=K4*K4
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q12)
       deallocate(D1)
       deallocate(B2)
C
       X9=X9-Q12
       deallocate(Q12)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2B,D2)
       allocate(Q20(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,D2,Q20)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N2,N3,N2,N3,X9,Q20,-0.500)
       deallocate(Q20)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call reorder4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,t2C,D2)
       allocate(Q22(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,D2,Q22)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N2,N3,N2,N3,X9,Q22, 1.000)
       deallocate(Q22)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q16(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q16)
       deallocate(D1)
       deallocate(B2)
C
       X18=X18+Q16
C
       call sumx21(0,N3,N0,N2,N2,N3,X18,FockB, 1.000)
C
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder412356(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N1,N3,N0,N2,N0,N1,t3D,F2)
       allocate(U22(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I2=K1*K2*K3*K4
       I3=K4*K2
       call jungemm2(I2,I3,X18,F2,U22)
       deallocate(F2)
C
       V2C=V2C+U22
       deallocate(U22)
       deallocate(X18)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q32(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,Q16,B2,Q32)
       deallocate(B2)
       deallocate(Q16)
C
       call sum21(N2,N3,N2,N3,X9,Q32,-1.000)
       deallocate(Q32)
C
       call sumx21(0,N3,N2,N3,N2,N3,X9,FockB, 1.000)
C
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,t2C,D2)
       allocate(U10(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4
       I2=K1*K2*K3
       I3=K4
       call jungemm(I1,I2,I3,X9,D2,U10)
       deallocate(D2)
C
       call
     & sum2341(N2,N3,N1,N3,N0,N2,N0,N1,V2C,U10, 1.000)
       deallocate(U10)
       deallocate(X9)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S46(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S46)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X10(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       X10=0.0d0
       call sum4123(N0,N1,N1,N3,N1,N3,N0,N1,X10,S46,-1.000)
       deallocate(S46)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S105(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S105)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N1,N1,N3,N1,N3,N0,N1,X10,S105, 1.000)
       deallocate(S105)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2C,D2)
       allocate(S118(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S118)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N1,N1,N3,N1,N3,N0,N1,X10,S118, 1.000)
       deallocate(S118)
C
       call sumx3124(N0,N3,N0,N1,N1,N3,N1,N3,N0,N1,X10,IntR, 1.000)
C
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,t2C,D2)
       allocate(U11(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,X10,D2,U11)
       deallocate(D2)
C
       call
     & sum1324(N2,N3,N1,N3,N0,N2,N0,N1,V2C,U11,-1.000)
       deallocate(U11)
       deallocate(X10)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S80(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S80)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X11(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       X11=0.0d0
       call sum4123(N0,N2,N2,N3,N2,N3,N0,N2,X11,S80,-1.000)
       deallocate(S80)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2B,D2)
       allocate(S109(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S109)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N2,N2,N3,N2,N3,N0,N2,X11,S109, 1.000)
       deallocate(S109)
C
       call sumx3124(N0,N3,N0,N2,N2,N3,N2,N3,N0,N2,X11,IntB, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2C,D2)
       allocate(U12(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,X11,D2,U12)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,N1,N3,N0,N2,N0,N1,V2C,U12,-1.000)
       deallocate(U12)
       deallocate(X11)
C
       allocate(D1(N2+1:N3,N0+1:N1,N1+1:N3,N2+1:N3))
       call reorder2314(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S73(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       I1=K4*K3*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S73)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       X4=0.0d0
       call sum4123(N0,N1,N1,N3,N2,N3,N0,N2,X4,S73, 1.000)
       deallocate(S73)
C
       call sumx2413(N0,N3,N0,N1,N1,N3,N2,N3,N0,N2,X4,IntM, 1.000)
C
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(U5(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,X4,D2,U5)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,N1,N3,N0,N2,N0,N1,V2C,U5, 1.000)
       deallocate(U5)
       deallocate(X4)
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
       call sum21(N0,N1,N0,N1,X6,Q1, 1.000)
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
       X6=X6+Q3
       deallocate(Q3)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call reorder4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N0,N1,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q13(N0+1:N1,N0+1:N1))
       I1=K1*K1
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q13)
       deallocate(D1)
       deallocate(B2)
C
       X6=X6+Q13
       deallocate(Q13)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(Q17(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,D1,D2,Q17)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N0,N1,N0,N1,X6,Q17, 0.500)
       deallocate(Q17)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2C,D2)
       allocate(Q23(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,D1,D2,Q23)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N0,N1,N0,N1,X6,Q23,-1.000)
       deallocate(Q23)
C
       call sumx21(0,N3,N0,N1,N0,N1,X6,FockR, 1.000)
C
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2C,D2)
       allocate(U7(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1
       I2=K2*K3*K4
       I3=K1
       call jungemm(I1,I2,I3,X6,D2,U7)
       deallocate(D2)
C
       V2C=V2C-U7
       deallocate(U7)
       deallocate(X6)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N1+1:N3))
       call reorder4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N1,N3,N1,N3,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S88(N0+1:N2,N0+1:N2,N1+1:N3,N1+1:N3))
       I1=K3*K3*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S88)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X14(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       X14=0.0d0
       call sum4123(N0,N2,N1,N3,N1,N3,N0,N2,X14,S88, 1.000)
       deallocate(S88)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N1+1:N3))
       call reorder3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N0,N2,N1,N3,IntM,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2C,D2)
       allocate(S120(N1+1:N3,N0+1:N2,N0+1:N2,N1+1:N3))
       I1=K3*K2
       I2=K2*K3
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,D2,S120)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N2,N1,N3,N1,N3,N0,N2,X14,S120, 1.000)
       deallocate(S120)
C
       call sumx3124(N0,N3,N0,N2,N1,N3,N1,N3,N0,N2,X14,IntM, 1.000)
C
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,N3,N0,N1,t2C,D2)
       allocate(U15(N2+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3
       I2=K1*K4
       I3=K3*K2
       call jungemm(I1,I2,I3,X14,D2,U15)
       deallocate(D2)
C
       call
     & sum1423(N2,N3,N1,N3,N0,N2,N0,N1,V2C,U15,-1.000)
       deallocate(U15)
       deallocate(X14)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N1,N3,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q7(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q7)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X15(N0+1:N1,N1+1:N3))
       X15=0.0d0
       X15=X15+Q7
C
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q26(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,Q7,B2,Q26)
       deallocate(B2)
       deallocate(Q7)
C
       allocate(X7(N1+1:N3,N1+1:N3))
       X7=0.0d0
       call sum21(N1,N3,N1,N3,X7,Q26,-1.000)
       deallocate(Q26)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N1,N3,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q15(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q15)
       deallocate(D1)
       deallocate(B2)
C
       X15=X15+Q15
C
       call sumx21(0,N3,N0,N1,N1,N3,X15,FockR, 1.000)
C
       allocate(F2(N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder412356(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N1,N3,N0,N2,N0,N1,t3C,F2)
       allocate(U17(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I2=K1*K2*K3*K4
       I3=K3*K1
       call jungemm2(I2,I3,X15,F2,U17)
       deallocate(F2)
C
       V2C=V2C+U17
       deallocate(U17)
       deallocate(X15)
C
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q30(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,Q15,B2,Q30)
       deallocate(B2)
       deallocate(Q15)
C
       call sum21(N1,N3,N1,N3,X7,Q30,-1.000)
       deallocate(Q30)
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
       call sum21(N1,N3,N1,N3,X7,Q2,-1.000)
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
       X7=X7-Q4
       deallocate(Q4)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call reorder2431(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q14(N1+1:N3,N1+1:N3))
       I1=K3*K3
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q14)
       deallocate(D1)
       deallocate(B2)
C
       X7=X7+Q14
       deallocate(Q14)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(Q18(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,D2,Q18)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N1,N3,N1,N3,X7,Q18,-0.500)
       deallocate(Q18)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2C,D2)
       allocate(Q21(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,Q21)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N1,N3,N1,N3,X7,Q21, 1.000)
       deallocate(Q21)
C
       call sumx21(0,N3,N1,N3,N1,N3,X7,FockR, 1.000)
C
       allocate(D2(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,N3,N0,N2,N0,N1,t2C,D2)
       allocate(U8(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1*K2*K4
       I3=K3
       call jungemm(I1,I2,I3,X7,D2,U8)
       deallocate(D2)
C
       call
     & sum1342(N2,N3,N1,N3,N0,N2,N0,N1,V2C,U8, 1.000)
       deallocate(U8)
       deallocate(X7)
C
       allocate(D1(N1+1:N3,N2+1:N3,N2+1:N3,N1+1:N3))
       call reorder3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N2,N3,N2,N3,N1,N3,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S35(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       I1=K3*K4*K4
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S35)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       X3=0.0d0
       call sum4123(N2,N3,N2,N3,N1,N3,N0,N1,X3,S35, 1.000)
       deallocate(S35)
C
       call sumx3241(N0,N3,N2,N3,N2,N3,N1,N3,N0,N1,X3,IntM, 1.000)
C
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(U4(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,X3,B2,U4)
       deallocate(B2)
C
       call
     & sum3124(N2,N3,N1,N3,N0,N2,N0,N1,V2C,U4,-1.000)
       deallocate(U4)
       deallocate(X3)
C
       allocate(D1(N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N2,N3,N1,N3,N0,N2,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(U2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K4
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,U2)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N2,N3,N1,N3,N0,N2,N0,N1,V2C,U2,-1.000)
       deallocate(U2)
C
       allocate(D1(N2+1:N3,N1+1:N3,N2+1:N3,N1+1:N3))
       call reorder4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,t2C,D2)
       allocate(U16(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K1*K2
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,D2,U16)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,N3,N1,N3,N0,N2,N0,N1,V2C,U16, 1.000)
       deallocate(U16)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call reorder2431(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,IntR,D1)
       allocate(F2(N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call reorder413256(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N1,N3,N1,N3,N2,N3,N0,N2,N0,N1,t3C,F2)
       allocate(U19(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1*K2*K4
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,D1,F2,U19)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N2,N3,N1,N3,N0,N2,N0,N1,V2C,U19, 0.500)
       deallocate(U19)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call reorder3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N2,N3,IntM,D1)
       allocate(F2(N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder421356(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,t3C,F2)
       allocate(U21(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4
       I2=K1*K2*K3
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,D1,F2,U21)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N1,N3,N0,N2,N0,N1,V2C,U21, 1.000)
       deallocate(U21)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call reorder2431(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,IntB,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder412356(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N1,N3,N0,N2,N0,N1,t3D,F2)
       allocate(U24(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4
       I2=K1*K2*K3
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,D1,F2,U24)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N1,N3,N0,N2,N0,N1,V2C,U24, 0.500)
       deallocate(U24)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call reorder2431(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,IntM,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call reorder413256(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N2,N3,N0,N2,N0,N1,t3D,F2)
       allocate(U26(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1*K2*K4
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,D1,F2,U26)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N2,N3,N1,N3,N0,N2,N0,N1,V2C,U26, 1.000)
       deallocate(U26)
C
       do i=N0+1,N1
       do j=N0+1,N2
       do a=N1+1,N3
       do b=N2+1,N3
         CoeLeft=FockB(b,b)
     &          +FockR(a,a)
     &          -FockB(j,j)
     &          -FockR(i,i)
     &          +shift
         t2C(b,a,j,i)=t2C(b,a,j,i)-V2C(b,a,j,i)/CoeLeft
         !if (dabs(t2C(b,a,j,i)) > 1.0d-5) print *, t2c(b,a,j,i)
       enddo
       enddo
       enddo
       enddo
C
       end
