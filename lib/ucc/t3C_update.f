       subroutine t3C_update(N0,N1,N2,N3,K1,K2,K3,K4,shift,V3C,
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
       real*8 V3C(N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N1)
       integer p_space(N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N1)
C
       real*8,allocatable::B1(:,:)
       real*8,allocatable::B2(:,:)
       real*8,allocatable::C1(:,:,:)
       real*8,allocatable::C2(:,:,:)
       real*8,allocatable::D1(:,:,:,:)
       real*8,allocatable::D2(:,:,:,:)
       real*8,allocatable::F2(:,:,:,:,:,:)
C
       real*8,allocatable::S11(:,:,:,:)
       real*8,allocatable::S86(:,:,:,:)
       real*8,allocatable::U33(:,:,:,:,:,:)
       real*8,allocatable::S4(:,:,:,:)
       real*8,allocatable::S40(:,:,:,:)
       real*8,allocatable::S65(:,:,:,:)
       real*8,allocatable::S54(:,:,:,:)
       real*8,allocatable::S121(:,:,:,:)
       real*8,allocatable::S116(:,:,:,:)
       real*8,allocatable::S77(:,:,:,:)
       real*8,allocatable::S126(:,:,:,:)
       real*8,allocatable::S124(:,:,:,:)
       real*8,allocatable::X21(:,:,:,:)
       real*8,allocatable::U24(:,:,:,:,:,:)
       real*8,allocatable::S12(:,:,:,:)
       real*8,allocatable::S87(:,:,:,:)
       real*8,allocatable::S13(:,:,:,:)
       real*8,allocatable::S89(:,:,:,:)
       real*8,allocatable::S88(:,:,:,:)
       real*8,allocatable::S92(:,:,:,:)
       real*8,allocatable::S93(:,:,:,:)
       real*8,allocatable::S146(:,:,:,:)
       real*8,allocatable::Q21(:,:)
       real*8,allocatable::S94(:,:,:,:)
       real*8,allocatable::S147(:,:,:,:)
       real*8,allocatable::X13(:,:,:,:)
       real*8,allocatable::U13(:,:,:,:,:,:)
       real*8,allocatable::S5(:,:,:,:)
       real*8,allocatable::S44(:,:,:,:)
       real*8,allocatable::S67(:,:,:,:)
       real*8,allocatable::X22(:,:,:,:)
       real*8,allocatable::U25(:,:,:,:,:,:)
       real*8,allocatable::S3(:,:,:,:)
       real*8,allocatable::S37(:,:,:,:)
       real*8,allocatable::S43(:,:,:,:)
       real*8,allocatable::S71(:,:,:,:)
       real*8,allocatable::S79(:,:,:,:)
       real*8,allocatable::S52(:,:,:,:)
       real*8,allocatable::S119(:,:,:,:)
       real*8,allocatable::Q28(:,:)
       real*8,allocatable::S122(:,:,:,:)
       real*8,allocatable::S120(:,:,:,:)
       real*8,allocatable::Q32(:,:)
       real*8,allocatable::S142(:,:,:,:)
       real*8,allocatable::S139(:,:,:,:)
       real*8,allocatable::X3(:,:,:,:)
       real*8,allocatable::U3(:,:,:,:,:,:)
       real*8,allocatable::X12(:,:,:,:)
       real*8,allocatable::U12(:,:,:,:,:,:)
       real*8,allocatable::S1(:,:,:,:)
       real*8,allocatable::S21(:,:,:,:)
       real*8,allocatable::S22(:,:,:,:)
       real*8,allocatable::S35(:,:,:,:)
       real*8,allocatable::S38(:,:,:,:)
       real*8,allocatable::S39(:,:,:,:)
       real*8,allocatable::S46(:,:,:,:)
       real*8,allocatable::S49(:,:,:,:)
       real*8,allocatable::S53(:,:,:,:)
       real*8,allocatable::S57(:,:,:,:)
       real*8,allocatable::S15(:,:,:,:)
       real*8,allocatable::S90(:,:,:,:)
       real*8,allocatable::S99(:,:,:,:)
       real*8,allocatable::S17(:,:,:,:)
       real*8,allocatable::S91(:,:,:,:)
       real*8,allocatable::S101(:,:,:,:)
       real*8,allocatable::S59(:,:,:,:)
       real*8,allocatable::S114(:,:,:,:)
       real*8,allocatable::S113(:,:,:,:)
       real*8,allocatable::S72(:,:,:,:)
       real*8,allocatable::S118(:,:,:,:)
       real*8,allocatable::S115(:,:,:,:)
       real*8,allocatable::S31(:,:,:,:)
       real*8,allocatable::S130(:,:,:,:)
       real*8,allocatable::S78(:,:,:,:)
       real*8,allocatable::S127(:,:,:,:)
       real*8,allocatable::S136(:,:,:,:)
       real*8,allocatable::S75(:,:,:,:)
       real*8,allocatable::S128(:,:,:,:)
       real*8,allocatable::S140(:,:,:,:)
       real*8,allocatable::S97(:,:,:,:)
       real*8,allocatable::S98(:,:,:,:)
       real*8,allocatable::S148(:,:,:,:)
       real*8,allocatable::S108(:,:,:,:)
       real*8,allocatable::Q23(:,:)
       real*8,allocatable::S107(:,:,:,:)
       real*8,allocatable::S150(:,:,:,:)
       real*8,allocatable::X1(:,:,:,:)
       real*8,allocatable::U1(:,:,:,:,:,:)
       real*8,allocatable::S27(:,:,:,:)
       real*8,allocatable::S28(:,:,:,:)
       real*8,allocatable::S109(:,:,:,:)
       real*8,allocatable::S110(:,:,:,:)
       real*8,allocatable::S151(:,:,:,:)
       real*8,allocatable::Q24(:,:)
       real*8,allocatable::S132(:,:,:,:)
       real*8,allocatable::X7(:,:,:,:)
       real*8,allocatable::U7(:,:,:,:,:,:)
       real*8,allocatable::S2(:,:,:,:)
       real*8,allocatable::S23(:,:,:,:)
       real*8,allocatable::S24(:,:,:,:)
       real*8,allocatable::S36(:,:,:,:)
       real*8,allocatable::S42(:,:,:,:)
       real*8,allocatable::S45(:,:,:,:)
       real*8,allocatable::S48(:,:,:,:)
       real*8,allocatable::S51(:,:,:,:)
       real*8,allocatable::S56(:,:,:,:)
       real*8,allocatable::S16(:,:,:,:)
       real*8,allocatable::S100(:,:,:,:)
       real*8,allocatable::S18(:,:,:,:)
       real*8,allocatable::S102(:,:,:,:)
       real*8,allocatable::S33(:,:,:,:)
       real*8,allocatable::S131(:,:,:,:)
       real*8,allocatable::S82(:,:,:,:)
       real*8,allocatable::S123(:,:,:,:)
       real*8,allocatable::S135(:,:,:,:)
       real*8,allocatable::S80(:,:,:,:)
       real*8,allocatable::S125(:,:,:,:)
       real*8,allocatable::S138(:,:,:,:)
       real*8,allocatable::X2(:,:,:,:)
       real*8,allocatable::U2(:,:,:,:,:,:)
       real*8,allocatable::X18(:,:,:,:)
       real*8,allocatable::U18(:,:,:,:,:,:)
       real*8,allocatable::S7(:,:,:,:)
       real*8,allocatable::S9(:,:,:,:)
       real*8,allocatable::S25(:,:,:,:)
       real*8,allocatable::S47(:,:,:,:)
       real*8,allocatable::S61(:,:,:,:)
       real*8,allocatable::S62(:,:,:,:)
       real*8,allocatable::S66(:,:,:,:)
       real*8,allocatable::S69(:,:,:,:)
       real*8,allocatable::S76(:,:,:,:)
       real*8,allocatable::S84(:,:,:,:)
       real*8,allocatable::S19(:,:,:,:)
       real*8,allocatable::S104(:,:,:,:)
       real*8,allocatable::S103(:,:,:,:)
       real*8,allocatable::S20(:,:,:,:)
       real*8,allocatable::S106(:,:,:,:)
       real*8,allocatable::S105(:,:,:,:)
       real*8,allocatable::Q26(:,:)
       real*8,allocatable::Q27(:,:)
       real*8,allocatable::S129(:,:,:,:)
       real*8,allocatable::S58(:,:,:,:)
       real*8,allocatable::S141(:,:,:,:)
       real*8,allocatable::S137(:,:,:,:)
       real*8,allocatable::S85(:,:,:,:)
       real*8,allocatable::S144(:,:,:,:)
       real*8,allocatable::S143(:,:,:,:)
       real*8,allocatable::Q30(:,:)
       real*8,allocatable::Q31(:,:)
       real*8,allocatable::S145(:,:,:,:)
       real*8,allocatable::X5(:,:,:,:)
       real*8,allocatable::U5(:,:,:,:,:,:)
       real*8,allocatable::S8(:,:,:,:)
       real*8,allocatable::S10(:,:,:,:)
       real*8,allocatable::S26(:,:,:,:)
       real*8,allocatable::S50(:,:,:,:)
       real*8,allocatable::S63(:,:,:,:)
       real*8,allocatable::S64(:,:,:,:)
       real*8,allocatable::S68(:,:,:,:)
       real*8,allocatable::S73(:,:,:,:)
       real*8,allocatable::S83(:,:,:,:)
       real*8,allocatable::S149(:,:,:,:)
       real*8,allocatable::X6(:,:,:,:)
       real*8,allocatable::U6(:,:,:,:,:,:)
       real*8,allocatable::X20(:,:,:,:)
       real*8,allocatable::U20(:,:,:,:,:,:)
       real*8,allocatable::Q6(:,:)
       real*8,allocatable::Q8(:,:)
       real*8,allocatable::Q10(:,:)
       real*8,allocatable::Q16(:,:)
       real*8,allocatable::Q20(:,:)
       real*8,allocatable::X11(:,:)
       real*8,allocatable::U11(:,:,:,:,:,:)
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::Q9(:,:)
       real*8,allocatable::Q15(:,:)
       real*8,allocatable::Q18(:,:)
       real*8,allocatable::S133(:,:,:,:)
       real*8,allocatable::S134(:,:,:,:)
       real*8,allocatable::Q29(:,:)
       real*8,allocatable::X10(:,:)
       real*8,allocatable::U10(:,:,:,:,:,:)
       real*8,allocatable::S29(:,:,:,:)
       real*8,allocatable::S30(:,:,:,:)
       real*8,allocatable::S60(:,:,:,:)
       real*8,allocatable::S81(:,:,:,:)
       real*8,allocatable::X15(:,:,:,:)
       real*8,allocatable::U15(:,:,:,:,:,:)
       real*8,allocatable::S55(:,:,:,:)
       real*8,allocatable::S117(:,:,:,:)
       real*8,allocatable::S95(:,:,:,:)
       real*8,allocatable::Q22(:,:)
       real*8,allocatable::S96(:,:,:,:)
       real*8,allocatable::X14(:,:,:,:)
       real*8,allocatable::U14(:,:,:,:,:,:)
       real*8,allocatable::S6(:,:,:,:)
       real*8,allocatable::S41(:,:,:,:)
       real*8,allocatable::S70(:,:,:,:)
       real*8,allocatable::S74(:,:,:,:)
       real*8,allocatable::X4(:,:,:,:)
       real*8,allocatable::U4(:,:,:,:,:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::Q4(:,:)
       real*8,allocatable::Q12(:,:)
       real*8,allocatable::Q14(:,:)
       real*8,allocatable::Q19(:,:)
       real*8,allocatable::S111(:,:,:,:)
       real*8,allocatable::S112(:,:,:,:)
       real*8,allocatable::Q25(:,:)
       real*8,allocatable::X9(:,:)
       real*8,allocatable::U9(:,:,:,:,:,:)
       real*8,allocatable::S34(:,:,:,:)
       real*8,allocatable::X19(:,:,:,:)
       real*8,allocatable::U19(:,:,:,:,:,:)
       real*8,allocatable::S32(:,:,:,:)
       real*8,allocatable::X17(:,:,:,:)
       real*8,allocatable::U17(:,:,:,:,:,:)
       real*8,allocatable::X16(:,:,:,:)
       real*8,allocatable::U16(:,:,:,:,:,:)
       real*8,allocatable::S14(:,:,:,:)
       real*8,allocatable::U37(:,:,:,:,:,:)
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::Q3(:,:)
       real*8,allocatable::Q11(:,:)
       real*8,allocatable::Q13(:,:)
       real*8,allocatable::Q17(:,:)
       real*8,allocatable::X8(:,:)
       real*8,allocatable::U8(:,:,:,:,:,:)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       call reorder1432(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N0,N1,N0,N1,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S11(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S11)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call reorder2341(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,S11,D1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder461235(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,N1,N1,N3,N2,N3,N1,N3,N0,N2,t3C,F2)
       allocate(U33(N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K2*K3*K4*K3
       I3=K1*K1
       call jungemm(I1,I2,I3,D1,F2,U33)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum123546(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U33, 0.500)
       call
     & sum123564(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U33,-0.500)
       deallocate(U33)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call reorder3241(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,S11,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S86(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S86)
       deallocate(D1)
       deallocate(B2)
       deallocate(S11)
C
       allocate(X21(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       X21=0.0d0
       call sum2134(N0,N1,N1,N3,N0,N1,N0,N1,X21,S86,-1.000)
       deallocate(S86)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call reorder3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S4(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S4)
       deallocate(D1)
       deallocate(B2)
C
       call sum4123(N0,N1,N1,N3,N0,N1,N0,N1,X21,S4, 1.000)
       deallocate(S4)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N0,N1,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S40(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S40)
       deallocate(D1)
       deallocate(D2)
C
       call sum2413(N0,N1,N1,N3,N0,N1,N0,N1,X21,S40,-1.000)
       deallocate(S40)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call reorder4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N0,N1,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2C,D2)
       allocate(S65(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S65)
       deallocate(D1)
       deallocate(D2)
C
       call sum2413(N0,N1,N1,N3,N0,N1,N0,N1,X21,S65,-1.000)
       deallocate(S65)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S54(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S54)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X13(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       X13=0.0d0
       call sum3412(N0,N1,N1,N3,N1,N3,N0,N1,X13,S54, 1.000)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,S54,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S121(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S121)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X22(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       X22=0.0d0
       call sum2134(N1,N3,N1,N3,N1,N3,N0,N1,X22,S121,-1.000)
       deallocate(S121)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call reorder4312(N1,N3,N0,N1,N0,N1,N1,N3,
     & N1,N3,N0,N1,N1,N3,N0,N1,S54,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S116(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S116)
       deallocate(D1)
       deallocate(B2)
       deallocate(S54)
C
       call sum3124(N0,N1,N1,N3,N0,N1,N0,N1,X21,S116,-1.000)
       deallocate(S116)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2C,D2)
       allocate(S77(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S77)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N1,N1,N3,N1,N3,N0,N1,X13,S77, 1.000)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,S77,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S126(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S126)
       deallocate(D1)
       deallocate(B2)
C
       call sum2134(N1,N3,N1,N3,N1,N3,N0,N1,X22,S126,-1.000)
       deallocate(S126)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call reorder4312(N1,N3,N0,N1,N0,N1,N1,N3,
     & N1,N3,N0,N1,N1,N3,N0,N1,S77,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S124(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S124)
       deallocate(D1)
       deallocate(B2)
       deallocate(S77)
C
       call sum3124(N0,N1,N1,N3,N0,N1,N0,N1,X21,S124,-1.000)
       deallocate(S124)
C
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2C,D2)
       allocate(U24(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K2*K3*K4
       I3=K1
       call jungemm(I1,I2,I3,X21,D2,U24)
       deallocate(D2)
C
       call
     & sum235146(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U24, 1.000)
       call
     & sum215346(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U24,-1.000)
       call
     & sum235164(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U24,-1.000)
       call
     & sum215364(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U24, 1.000)
       deallocate(U24)
       deallocate(X21)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call reorder3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S12(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S12)
       deallocate(D1)
       deallocate(B2)
C
       call sum3124(N0,N1,N1,N3,N1,N3,N0,N1,X13,S12,-1.000)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder2314(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,S12,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S87(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S87)
       deallocate(D1)
       deallocate(B2)
       deallocate(S12)
C
       allocate(X4(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       X4=0.0d0
       call sum2134(N1,N3,N1,N3,N1,N3,N0,N1,X4,S87, 1.000)
       deallocate(S87)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S13(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S13)
       deallocate(D1)
       deallocate(B2)
C
       call sum4123(N0,N1,N1,N3,N1,N3,N0,N1,X13,S13,-1.000)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder2341(N0,N1,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,S13,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S89(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S89)
       deallocate(D1)
       deallocate(B2)
C
       call sum3124(N1,N3,N1,N3,N1,N3,N0,N1,X22,S89,-1.000)
       deallocate(S89)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call reorder3241(N0,N1,N0,N1,N1,N3,N1,N3,
     & N1,N3,N0,N1,N1,N3,N0,N1,S13,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S88(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S88)
       deallocate(D1)
       deallocate(B2)
       deallocate(S13)
C
       allocate(X3(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       X3=0.0d0
       call sum3124(N0,N1,N1,N3,N0,N1,N0,N1,X3,S88, 1.000)
       deallocate(S88)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       call reorder1432(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N0,N1,N1,N3,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S92(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S92)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       call reorder4231(N0,N1,N0,N1,N0,N1,N1,N3,
     & N1,N3,N0,N1,N0,N1,N0,N1,S92,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S93(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S93)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X12(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       X12=0.0d0
       call sum3124(N0,N1,N0,N1,N0,N1,N0,N1,X12,S93, 1.000)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call reorder3214(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,S93,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S146(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S146)
       deallocate(D1)
       deallocate(B2)
       deallocate(S93)
C
       call sum2134(N0,N1,N1,N3,N0,N1,N0,N1,X3,S146,-1.000)
       deallocate(S146)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder3421(N0,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,N0,N1,S92,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q21(N0+1:N1,N0+1:N1))
       I1=K1*K1
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q21)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X8(N0+1:N1,N0+1:N1))
       X8=0.0d0
       X8=X8-Q21
       deallocate(Q21)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call reorder3241(N0,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,S92,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S94(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S94)
       deallocate(D1)
       deallocate(B2)
       deallocate(S92)
C
       call sum3124(N0,N1,N1,N3,N1,N3,N0,N1,X13,S94, 1.000)
C
       call sumx3124(N0,N3,N0,N1,N1,N3,N1,N3,N0,N1,X13,IntR, 1.000)
C
       allocate(F2(N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder412356(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N1,N3,N0,N2,N0,N1,t3C,F2)
       allocate(U13(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K2*K3*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,X13,F2,U13)
       deallocate(F2)
C
       call
     & sum235614(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U13,-1.000)
       call
     & sum215634(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U13, 1.000)
       call
     & sum235416(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U13, 1.000)
       call
     & sum215436(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U13,-1.000)
       deallocate(U13)
       deallocate(X13)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder2314(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,S94,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S147(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S147)
       deallocate(D1)
       deallocate(B2)
       deallocate(S94)
C
       call sum2134(N1,N3,N1,N3,N1,N3,N0,N1,X4,S147,-1.000)
       deallocate(S147)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder2314(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S5(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S5)
       deallocate(D1)
       deallocate(B2)
C
       call sum3124(N1,N3,N1,N3,N1,N3,N0,N1,X22,S5, 1.000)
       deallocate(S5)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call reorder2341(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S44(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S44)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N1,N3,N1,N3,N1,N3,N0,N1,X22,S44,-1.000)
       deallocate(S44)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call reorder2431(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2C,D2)
       allocate(S67(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S67)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N1,N3,N1,N3,N1,N3,N0,N1,X22,S67, 1.000)
       deallocate(S67)
C
       allocate(D2(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,N3,N0,N2,N0,N1,t2C,D2)
       allocate(U25(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1*K2*K4
       I3=K3
       call jungemm(I1,I2,I3,X22,D2,U25)
       deallocate(D2)
C
       call
     & sum256134(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U25, 1.000)
       call
     & sum256314(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U25,-1.000)
       call
     & sum254136(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U25,-1.000)
       call
     & sum254316(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U25, 1.000)
       deallocate(U25)
       deallocate(X22)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call reorder3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N0,N1,N0,N1,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S3(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S3)
       deallocate(D1)
       deallocate(B2)
C
       call sum2134(N0,N1,N1,N3,N0,N1,N0,N1,X3,S3,-1.000)
       deallocate(S3)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(0,N3,0,N3,
     & N1,N3,N0,N1,FockR,B1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(S37(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K1*K3
       I3=K3
       call jungemm(I1,I2,I3,B1,D2,S37)
       deallocate(B1)
       deallocate(D2)
C
       call sum2341(N0,N1,N1,N3,N0,N1,N0,N1,X3,S37,-1.000)
       deallocate(S37)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N0,N1,N1,N3,IntR,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(S43(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K1*K1
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,D2,S43)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N1,N1,N3,N0,N1,N0,N1,X3,S43,-0.500)
       deallocate(S43)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(F2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,t3A,F2)
       allocate(S71(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K1*K3
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,D1,F2,S71)
       deallocate(D1)
       deallocate(F2)
C
       call sum2341(N0,N1,N1,N3,N0,N1,N0,N1,X3,S71,-0.500)
       deallocate(S71)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder521346(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N1,N3,N0,N1,N0,N1,t3C,F2)
       allocate(S79(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K1*K3
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,D1,F2,S79)
       deallocate(D1)
       deallocate(F2)
C
       call sum2341(N0,N1,N1,N3,N0,N1,N0,N1,X3,S79,-1.000)
       deallocate(S79)
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
       call sum3412(N0,N1,N0,N1,N0,N1,N0,N1,X12,S52,-0.500)
C
       call sumx4321(N0,N3,N0,N1,N0,N1,N0,N1,N0,N1,X12,IntR, 1.000)
C
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder461235(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,N1,N1,N3,N2,N3,N1,N3,N0,N2,t3C,F2)
       allocate(U12(N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K2*K3*K4*K3
       I3=K1*K1
       call jungemm(I1,I2,I3,X12,F2,U12)
       deallocate(F2)
C
       call
     & sum123546(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U12, 0.500)
       deallocate(U12)
       deallocate(X12)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call reorder4312(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,S52,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S119(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S119)
       deallocate(D1)
       deallocate(B2)
       deallocate(S52)
C
       call sum2134(N0,N1,N1,N3,N0,N1,N0,N1,X3,S119, 0.500)
       deallocate(S119)
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
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q28,B1)
       allocate(D2(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,N3,N0,N2,N0,N1,t2C,D2)
       allocate(S122(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K2*K4
       I3=K3
       call jungemm(I1,I2,I3,B1,D2,S122)
       deallocate(B1)
       deallocate(D2)
C
       allocate(X1(N0+1:N1,N2+1:N3,N0+1:N1,N0+1:N2))
       X1=0.0d0
       call sum2431(N0,N1,N2,N3,N0,N1,N0,N2,X1,S122,-1.000)
       deallocate(S122)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q28,B1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(S120(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K1*K3
       I3=K3
       call jungemm(I1,I2,I3,B1,D2,S120)
       deallocate(B1)
       deallocate(D2)
       deallocate(Q28)
C
       call sum2341(N0,N1,N1,N3,N0,N1,N0,N1,X3,S120,-1.000)
       deallocate(S120)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N1,N3,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q32(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q32)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q32,B1)
       allocate(D2(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,N3,N0,N2,N0,N1,t2C,D2)
       allocate(S142(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K2*K4
       I3=K3
       call jungemm(I1,I2,I3,B1,D2,S142)
       deallocate(B1)
       deallocate(D2)
C
       call sum2431(N0,N1,N2,N3,N0,N1,N0,N2,X1,S142,-1.000)
       deallocate(S142)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q32,B1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(S139(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K1*K3
       I3=K3
       call jungemm(I1,I2,I3,B1,D2,S139)
       deallocate(B1)
       deallocate(D2)
       deallocate(Q32)
C
       call sum2341(N0,N1,N1,N3,N0,N1,N0,N1,X3,S139,-1.000)
       deallocate(S139)
C
       call sumx2143(N0,N3,N0,N1,N1,N3,N0,N1,N0,N1,X3,IntR, 1.000)
C
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2C,D2)
       allocate(U3(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K2*K3*K4
       I3=K1
       call jungemm(I1,I2,I3,X3,D2,U3)
       deallocate(D2)
C
       call
     & sum235146(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U3, 1.000)
       call
     & sum215346(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U3,-1.000)
       deallocate(U3)
       deallocate(X3)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       call reorder1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N2,N3,N0,N2,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S1(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S1)
       deallocate(D1)
       deallocate(B2)
C
       call sum3124(N0,N1,N2,N3,N0,N1,N0,N2,X1,S1, 1.000)
       deallocate(S1)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:N1,N0+1:N2))
       call reorder4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N0,N1,N0,N2,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S21(N2+1:N3,N0+1:N1,N0+1:N1,N0+1:N2))
       I1=K2*K1*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S21)
       deallocate(D1)
       deallocate(B2)
C
       call sum2134(N0,N1,N2,N3,N0,N1,N0,N2,X1,S21,-1.000)
       deallocate(S21)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:N3,N0+1:N1))
       call reorder2341(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N1,N2,N3,N0,N1,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S22(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       I1=K1*K4*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S22)
       deallocate(D1)
       deallocate(B2)
C
       call sum4123(N0,N1,N2,N3,N0,N1,N0,N2,X1,S22, 1.000)
       deallocate(S22)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call reorder4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N0,N1,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2B,D2)
       allocate(S35(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S35)
       deallocate(D1)
       deallocate(D2)
C
       call sum2413(N0,N1,N2,N3,N0,N1,N0,N2,X1,S35,-1.000)
       deallocate(S35)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(0,N3,0,N3,
     & N1,N3,N0,N1,FockR,B1)
       allocate(D2(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,N3,N0,N2,N0,N1,t2C,D2)
       allocate(S38(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K2*K4
       I3=K3
       call jungemm(I1,I2,I3,B1,D2,S38)
       deallocate(B1)
       deallocate(D2)
C
       call sum2431(N0,N1,N2,N3,N0,N1,N0,N2,X1,S38,-1.000)
       deallocate(S38)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder4132(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N0,N1,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S39(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S39)
       deallocate(D1)
       deallocate(D2)
C
       call sum2413(N0,N1,N2,N3,N0,N1,N0,N2,X1,S39, 1.000)
       deallocate(S39)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,N0+1:N2))
       call reorder4132(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N0,N1,N0,N2,IntM,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,N3,N0,N1,t2C,D2)
       allocate(S46(N2+1:N3,N0+1:N1,N0+1:N1,N0+1:N2))
       I1=K2*K1
       I2=K1*K4
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,D2,S46)
       deallocate(D1)
       deallocate(D2)
C
       call sum2314(N0,N1,N2,N3,N0,N1,N0,N2,X1,S46, 1.000)
       deallocate(S46)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N1,N2+1:N3))
       call reorder2134(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N0,N1,N2,N3,IntM,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,t2C,D2)
       allocate(S49(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3))
       I1=K4*K1
       I2=K1*K2
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,D2,S49)
       deallocate(D1)
       deallocate(D2)
C
       call sum4312(N0,N1,N2,N3,N0,N1,N0,N2,X1,S49,-1.000)
       deallocate(S49)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(F2(N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call reorder413256(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N1,N3,N1,N3,N2,N3,N0,N2,N0,N1,t3C,F2)
       allocate(S53(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K2*K4
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,D1,F2,S53)
       deallocate(D1)
       deallocate(F2)
C
       call sum2431(N0,N1,N2,N3,N0,N1,N0,N2,X1,S53,-0.500)
       deallocate(S53)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call reorder413256(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N2,N3,N0,N2,N0,N1,t3D,F2)
       allocate(S57(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K2*K4
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,D1,F2,S57)
       deallocate(D1)
       deallocate(F2)
C
       call sum2431(N0,N1,N2,N3,N0,N1,N0,N2,X1,S57,-1.000)
       deallocate(S57)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
       call reorder1432(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N2,N0,N1,N0,N2,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S15(N0+1:N1,N0+1:N2,N0+1:N1,N0+1:N2))
       I1=K2*K1*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S15)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X16(N0+1:N2,N0+1:N1,N0+1:N1,N0+1:N2))
       X16=0.0d0
       call sum3124(N0,N2,N0,N1,N0,N1,N0,N2,X16,S15, 1.000)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:N1,N0+1:N2))
       call reorder3214(N0,N1,N0,N2,N0,N1,N0,N2,
     & N0,N1,N0,N2,N0,N1,N0,N2,S15,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S90(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
       I1=K2*K1*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S90)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X5(N0+1:N2,N1+1:N3,N0+1:N1,N0+1:N2))
       X5=0.0d0
       call sum2134(N0,N2,N1,N3,N0,N1,N0,N2,X5,S90,-1.000)
       deallocate(S90)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:N1,N0+1:N2))
       call reorder2314(N0,N1,N0,N2,N0,N1,N0,N2,
     & N0,N2,N0,N1,N0,N1,N0,N2,S15,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S99(N2+1:N3,N0+1:N1,N0+1:N1,N0+1:N2))
       I1=K2*K1*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S99)
       deallocate(D1)
       deallocate(B2)
       deallocate(S15)
C
       call sum2134(N0,N1,N2,N3,N0,N1,N0,N2,X1,S99,-1.000)
       deallocate(S99)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:N3,N2+1:N3))
       call reorder1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N2,N3,N2,N3,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S17(N0+1:N1,N0+1:N1,N2+1:N3,N2+1:N3))
       I1=K4*K4*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S17)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X17(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       X17=0.0d0
       call sum4123(N0,N1,N2,N3,N2,N3,N0,N1,X17,S17, 1.000)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       call reorder2341(N0,N1,N0,N1,N2,N3,N2,N3,
     & N0,N1,N2,N3,N2,N3,N0,N1,S17,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S91(N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4*K4
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S91)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X6(N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N1))
       X6=0.0d0
       call sum2134(N2,N3,N1,N3,N2,N3,N0,N1,X6,S91,-1.000)
       deallocate(S91)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:N3,N0+1:N1))
       call reorder3241(N0,N1,N0,N1,N2,N3,N2,N3,
     & N2,N3,N0,N1,N2,N3,N0,N1,S17,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S101(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       I1=K1*K4*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S101)
       deallocate(D1)
       deallocate(B2)
       deallocate(S17)
C
       call sum4123(N0,N1,N2,N3,N0,N1,N0,N2,X1,S101, 1.000)
       deallocate(S101)
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
       allocate(X7(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       X7=0.0d0
       call sum3412(N0,N1,N1,N3,N2,N3,N0,N2,X7,S59,-1.000)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,S59,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S114(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S114)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X2(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       X2=0.0d0
       call sum2134(N1,N3,N1,N3,N2,N3,N0,N2,X2,S114, 1.000)
       deallocate(S114)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       call reorder4312(N2,N3,N0,N2,N0,N1,N1,N3,
     & N1,N3,N0,N1,N2,N3,N0,N2,S59,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S113(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S113)
       deallocate(D1)
       deallocate(B2)
       deallocate(S59)
C
       call sum3124(N0,N1,N2,N3,N0,N1,N0,N2,X1,S113,-1.000)
       deallocate(S113)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S72(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S72)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N1,N1,N3,N2,N3,N0,N2,X7,S72,-1.000)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,S72,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S118(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S118)
       deallocate(D1)
       deallocate(B2)
C
       call sum2134(N1,N3,N1,N3,N2,N3,N0,N2,X2,S118, 1.000)
       deallocate(S118)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       call reorder4312(N2,N3,N0,N2,N0,N1,N1,N3,
     & N1,N3,N0,N1,N2,N3,N0,N2,S72,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S115(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S115)
       deallocate(D1)
       deallocate(B2)
       deallocate(S72)
C
       call sum3124(N0,N1,N2,N3,N0,N1,N0,N2,X1,S115,-1.000)
       deallocate(S115)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       call reorder2431(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N0,N1,N0,N1,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S31(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S31)
       deallocate(D1)
       deallocate(B2)
C
       call sum4123(N0,N2,N0,N1,N0,N1,N0,N2,X16,S31, 1.000)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:N1,N0+1:N2))
       call reorder2341(N0,N2,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N0,N2,S31,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S130(N2+1:N3,N0+1:N1,N0+1:N1,N0+1:N2))
       I1=K2*K1*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S130)
       deallocate(D1)
       deallocate(B2)
       deallocate(S31)
C
       call sum2134(N0,N1,N2,N3,N0,N1,N0,N2,X1,S130,-1.000)
       deallocate(S130)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,N2+1:N3))
       call reorder4132(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N0,N1,N2,N3,IntM,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,N3,N0,N1,t2C,D2)
       allocate(S78(N2+1:N3,N0+1:N1,N0+1:N1,N2+1:N3))
       I1=K4*K1
       I2=K1*K4
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,D2,S78)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N1,N2,N3,N2,N3,N0,N1,X17,S78, 1.000)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N1,N2,N3,
     & N0,N1,N2,N3,N2,N3,N0,N1,S78,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S127(N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4*K4
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S127)
       deallocate(D1)
       deallocate(B2)
C
       call sum2134(N2,N3,N1,N3,N2,N3,N0,N1,X6,S127,-1.000)
       deallocate(S127)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:N3,N0+1:N1))
       call reorder4312(N2,N3,N0,N1,N0,N1,N2,N3,
     & N2,N3,N0,N1,N2,N3,N0,N1,S78,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S136(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       I1=K1*K4*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S136)
       deallocate(D1)
       deallocate(B2)
       deallocate(S78)
C
       call sum4123(N0,N1,N2,N3,N0,N1,N0,N2,X1,S136, 1.000)
       deallocate(S136)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder2143(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N0,N2,N0,N1,IntM,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N2))
       call reorder1243(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N1,N0,N2,t2C,D2)
       allocate(S75(N0+1:N1,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K2*K1
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,D2,S75)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N2,N0,N1,N0,N1,N0,N2,X16,S75,-1.000)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:N1,N0+1:N2))
       call reorder4312(N0,N1,N0,N2,N0,N2,N0,N1,
     & N0,N1,N0,N2,N0,N1,N0,N2,S75,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S128(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
       I1=K2*K1*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S128)
       deallocate(D1)
       deallocate(B2)
C
       call sum2134(N0,N2,N1,N3,N0,N1,N0,N2,X5,S128, 1.000)
       deallocate(S128)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       call reorder3421(N0,N1,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N0,N2,N0,N1,S75,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S140(N2+1:N3,N0+1:N1,N0+1:N2,N0+1:N1))
       I1=K1*K2*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S140)
       deallocate(D1)
       deallocate(B2)
       deallocate(S75)
C
       call sum2143(N0,N1,N2,N3,N0,N1,N0,N2,X1,S140, 1.000)
       deallocate(S140)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       call reorder1432(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N2,N0,N1,N2,N3,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S97(N0+1:N1,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4*K1*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S97)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,N0+1:N1))
       call reorder3241(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N1,N0,N2,N2,N3,N0,N1,S97,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S98(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S98)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X20(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       X20=0.0d0
       call sum3124(N0,N2,N2,N3,N1,N3,N0,N1,X20,S98,-1.000)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call reorder3214(N1,N3,N0,N2,N2,N3,N0,N1,
     & N2,N3,N0,N2,N1,N3,N0,N1,S98,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S148(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S148)
       deallocate(D1)
       deallocate(B2)
C
       call sum4123(N0,N2,N1,N3,N0,N1,N0,N2,X5,S148,-1.000)
       deallocate(S148)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call reorder2341(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N0,N1,N2,N3,N0,N1,S97,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S108(N2+1:N3,N0+1:N1,N2+1:N3,N0+1:N1))
       I1=K1*K4*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S108)
       deallocate(D1)
       deallocate(B2)
C
       call sum3124(N0,N1,N2,N3,N2,N3,N0,N1,X17,S108,-1.000)
       deallocate(S108)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call reorder2431(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,N0,N1,S97,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q23(N0+1:N1,N0+1:N1))
       I1=K1*K1
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q23)
       deallocate(D1)
       deallocate(B2)
C
       X8=X8+Q23
       deallocate(Q23)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       call reorder4231(N0,N1,N0,N2,N0,N1,N2,N3,
     & N2,N3,N0,N2,N0,N1,N0,N1,S97,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S107(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S107)
       deallocate(D1)
       deallocate(B2)
       deallocate(S97)
C
       call sum4123(N0,N2,N0,N1,N0,N1,N0,N2,X16,S107, 1.000)
C
       call sumx3421(N0,N3,N0,N2,N0,N1,N0,N1,N0,N2,X16,IntM, 1.000)
C
       allocate(F2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder541236(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,N1,N3,N0,N1,t3C,F2)
       allocate(U16(N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N2))
       I1=K2*K1
       I2=K1*K3*K4*K3
       I3=K1*K2
       call jungemm(I1,I2,I3,X16,F2,U16)
       deallocate(F2)
C
       call
     & sum123645(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U16, 1.000)
       call
     & sum123465(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U16,-1.000)
       deallocate(U16)
       deallocate(X16)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:N1,N0+1:N2))
       call reorder2341(N0,N2,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N0,N2,S107,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S150(N2+1:N3,N0+1:N1,N0+1:N1,N0+1:N2))
       I1=K2*K1*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S150)
       deallocate(D1)
       deallocate(B2)
       deallocate(S107)
C
       call sum2134(N0,N1,N2,N3,N0,N1,N0,N2,X1,S150,-1.000)
       deallocate(S150)
C
       call sumx3412(N0,N3,N0,N1,N2,N3,N0,N1,N0,N2,X1,IntM, 1.000)
C
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(U1(N1+1:N3,N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N1,N0+1:N2))
       I1=K2*K1*K4
       I2=K1*K3*K3
       I3=K1
       call jungemm(I1,I2,I3,X1,D2,U1)
       deallocate(D2)
C
       call
     & sum136245(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U1,-1.000)
       call
     & sum134265(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U1, 1.000)
       deallocate(U1)
       deallocate(X1)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call reorder4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N0,N2,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S27(N2+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S27)
       deallocate(D1)
       deallocate(B2)
C
       call sum3124(N0,N1,N1,N3,N2,N3,N0,N2,X7,S27,-1.000)
       deallocate(S27)
C
       allocate(D1(N2+1:N3,N0+1:N1,N1+1:N3,N2+1:N3))
       call reorder2314(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S28(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       I1=K4*K3*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S28)
       deallocate(D1)
       deallocate(B2)
C
       call sum4123(N0,N1,N1,N3,N2,N3,N0,N2,X7,S28, 1.000)
       deallocate(S28)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       call reorder2431(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N0,N1,N1,N3,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S109(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S109)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N2,N1+1:N3,N0+1:N2))
       call reorder3241(N0,N2,N0,N2,N0,N1,N1,N3,
     & N0,N1,N0,N2,N1,N3,N0,N2,S109,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S110(N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N2))
       I1=K2*K3*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S110)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X18(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       X18=0.0d0
       call sum3124(N0,N2,N1,N3,N1,N3,N0,N2,X18,S110,-1.000)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       call reorder2314(N1,N3,N0,N2,N1,N3,N0,N2,
     & N0,N2,N1,N3,N1,N3,N0,N2,S110,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S151(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K3
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S151)
       deallocate(D1)
       deallocate(B2)
       deallocate(S110)
C
       call sum3124(N1,N3,N1,N3,N2,N3,N0,N2,X2,S151, 1.000)
       deallocate(S151)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call reorder3421(N0,N2,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N2,N0,N2,S109,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q24(N0+1:N2,N0+1:N2))
       I1=K2*K2
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q24)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X10(N0+1:N2,N0+1:N2))
       X10=0.0d0
       X10=X10+Q24
       deallocate(Q24)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call reorder2341(N0,N2,N0,N2,N0,N1,N1,N3,
     & N0,N2,N0,N1,N1,N3,N0,N2,S109,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S132(N2+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S132)
       deallocate(D1)
       deallocate(B2)
       deallocate(S109)
C
       call sum3124(N0,N1,N1,N3,N2,N3,N0,N2,X7,S132,-1.000)
       deallocate(S132)
C
       call sumx2413(N0,N3,N0,N1,N1,N3,N2,N3,N0,N2,X7,IntM, 1.000)
C
       allocate(F2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,t3A,F2)
       allocate(U7(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K1*K3*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,X7,F2,U7)
       deallocate(F2)
C
       call
     & sum134625(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U7, 1.000)
       deallocate(U7)
       deallocate(X7)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S2(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S2)
       deallocate(D1)
       deallocate(B2)
C
       call sum2134(N1,N3,N1,N3,N2,N3,N0,N2,X2,S2,-1.000)
       deallocate(S2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       call reorder2314(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N1,N3,N0,N2,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S23(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K3
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S23)
       deallocate(D1)
       deallocate(B2)
C
       call sum3124(N1,N3,N1,N3,N2,N3,N0,N2,X2,S23,-1.000)
       deallocate(S23)
C
       allocate(D1(N2+1:N3,N1+1:N3,N1+1:N3,N2+1:N3))
       call reorder4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N1,N3,N2,N3,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S24(N0+1:N2,N1+1:N3,N1+1:N3,N2+1:N3))
       I1=K4*K3*K3
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S24)
       deallocate(D1)
       deallocate(B2)
C
       call sum4123(N1,N3,N1,N3,N2,N3,N0,N2,X2,S24, 1.000)
       deallocate(S24)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call reorder2431(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2B,D2)
       allocate(S36(N2+1:N3,N0+1:N2,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S36)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N1,N3,N1,N3,N2,N3,N0,N2,X2,S36,-1.000)
       deallocate(S36)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call reorder2431(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S42(N2+1:N3,N0+1:N2,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S42)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N1,N3,N1,N3,N2,N3,N0,N2,X2,S42,-1.000)
       deallocate(S42)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call reorder4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N0,N2,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2C,D2)
       allocate(S45(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3
       I2=K3*K4
       I3=K1*K2
       call jungemm(I1,I2,I3,D1,D2,S45)
       deallocate(D1)
       deallocate(D2)
C
       call sum3214(N1,N3,N1,N3,N2,N3,N0,N2,X2,S45,-1.000)
       deallocate(S45)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call reorder3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2C,D2)
       allocate(S48(N1+1:N3,N0+1:N2,N1+1:N3,N2+1:N3))
       I1=K4*K3
       I2=K2*K3
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,D2,S48)
       deallocate(D1)
       deallocate(D2)
C
       call sum2413(N1,N3,N1,N3,N2,N3,N0,N2,X2,S48, 1.000)
       deallocate(S48)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder461235(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,N1,N1,N3,N2,N3,N1,N3,N0,N2,t3C,F2)
       allocate(S51(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3))
       I1=K3
       I2=K2*K3*K4
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,F2,S51)
       deallocate(D1)
       deallocate(F2)
C
       call sum3241(N1,N3,N1,N3,N2,N3,N0,N2,X2,S51, 0.500)
       deallocate(S51)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder461235(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,t3D,F2)
       allocate(S56(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3))
       I1=K3
       I2=K2*K3*K4
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,F2,S56)
       deallocate(D1)
       deallocate(F2)
C
       call sum3241(N1,N3,N1,N3,N2,N3,N0,N2,X2,S56, 1.000)
       deallocate(S56)
C
       allocate(D1(N0+1:N1,N0+1:N2,N1+1:N3,N0+1:N2))
       call reorder3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N2,N1,N3,N0,N2,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S16(N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N2))
       I1=K2*K3*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S16)
       deallocate(D1)
       deallocate(B2)
C
       call sum3124(N0,N2,N1,N3,N1,N3,N0,N2,X18,S16,-1.000)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       call reorder2314(N1,N3,N0,N2,N1,N3,N0,N2,
     & N0,N2,N1,N3,N1,N3,N0,N2,S16,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S100(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K3
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S100)
       deallocate(D1)
       deallocate(B2)
       deallocate(S16)
C
       call sum3124(N1,N3,N1,N3,N2,N3,N0,N2,X2,S100, 1.000)
       deallocate(S100)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call reorder3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N2,N3,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S18(N1+1:N3,N2+1:N3,N1+1:N3,N2+1:N3))
       I1=K4*K3*K4
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S18)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X19(N2+1:N3,N1+1:N3,N1+1:N3,N2+1:N3))
       X19=0.0d0
       call sum3124(N2,N3,N1,N3,N1,N3,N2,N3,X19,S18,-1.000)
C
       allocate(D1(N2+1:N3,N1+1:N3,N1+1:N3,N2+1:N3))
       call reorder2314(N1,N3,N2,N3,N1,N3,N2,N3,
     & N2,N3,N1,N3,N1,N3,N2,N3,S18,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S102(N0+1:N2,N1+1:N3,N1+1:N3,N2+1:N3))
       I1=K4*K3*K3
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S102)
       deallocate(D1)
       deallocate(B2)
       deallocate(S18)
C
       call sum4123(N1,N3,N1,N3,N2,N3,N0,N2,X2,S102,-1.000)
       deallocate(S102)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N1+1:N3))
       call reorder4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N1,N3,N1,N3,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S33(N0+1:N2,N0+1:N2,N1+1:N3,N1+1:N3))
       I1=K3*K3*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S33)
       deallocate(D1)
       deallocate(B2)
C
       call sum4123(N0,N2,N1,N3,N1,N3,N0,N2,X18,S33, 1.000)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       call reorder2341(N0,N2,N0,N2,N1,N3,N1,N3,
     & N0,N2,N1,N3,N1,N3,N0,N2,S33,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S131(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K3
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S131)
       deallocate(D1)
       deallocate(B2)
       deallocate(S33)
C
       call sum3124(N1,N3,N1,N3,N2,N3,N0,N2,X2,S131,-1.000)
       deallocate(S131)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,t2C,D2)
       allocate(S82(N1+1:N3,N2+1:N3,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K4*K3
       I3=K1*K2
       call jungemm(I1,I2,I3,D1,D2,S82)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N2,N3,N1,N3,N1,N3,N2,N3,X19,S82,-1.000)
C
       allocate(D1(N1+1:N3,N2+1:N3,N2+1:N3,N1+1:N3))
       call reorder4321(N1,N3,N2,N3,N2,N3,N1,N3,
     & N1,N3,N2,N3,N2,N3,N1,N3,S82,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S123(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       I1=K3*K4*K4
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S123)
       deallocate(D1)
       deallocate(B2)
C
       call sum4132(N2,N3,N1,N3,N2,N3,N0,N1,X6,S123,-1.000)
       deallocate(S123)
C
       allocate(D1(N2+1:N3,N1+1:N3,N2+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N2,N3,N2,N3,N1,N3,
     & N2,N3,N1,N3,N2,N3,N1,N3,S82,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S135(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:N3))
       I1=K3*K4*K3
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S135)
       deallocate(D1)
       deallocate(B2)
       deallocate(S82)
C
       call sum4132(N1,N3,N1,N3,N2,N3,N0,N2,X2,S135,-1.000)
       deallocate(S135)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N1+1:N3))
       call reorder3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N0,N2,N1,N3,IntM,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2C,D2)
       allocate(S80(N1+1:N3,N0+1:N2,N0+1:N2,N1+1:N3))
       I1=K3*K2
       I2=K2*K3
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,D2,S80)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N2,N1,N3,N1,N3,N0,N2,X18,S80, 1.000)
C
       call sumx3124(N0,N3,N0,N2,N1,N3,N1,N3,N0,N2,X18,IntM, 1.000)
C
       allocate(F2(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder512346(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,N3,N1,N3,N0,N1,N0,N1,t3C,F2)
       allocate(U18(N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3
       I2=K1*K1*K3*K4
       I3=K3*K2
       call jungemm(I1,I2,I3,X18,F2,U18)
       deallocate(F2)
C
       call
     & sum234615(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U18,-1.000)
       call
     & sum214635(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U18, 1.000)
       deallocate(U18)
       deallocate(X18)
C
       allocate(D1(N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N2))
       call reorder4312(N1,N3,N0,N2,N0,N2,N1,N3,
     & N1,N3,N0,N2,N1,N3,N0,N2,S80,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S125(N0+1:N1,N0+1:N2,N1+1:N3,N0+1:N2))
       I1=K2*K3*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S125)
       deallocate(D1)
       deallocate(B2)
C
       call sum3124(N0,N2,N1,N3,N0,N1,N0,N2,X5,S125, 1.000)
       deallocate(S125)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       call reorder3412(N1,N3,N0,N2,N0,N2,N1,N3,
     & N0,N2,N1,N3,N1,N3,N0,N2,S80,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S138(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K3
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S138)
       deallocate(D1)
       deallocate(B2)
       deallocate(S80)
C
       call sum3124(N1,N3,N1,N3,N2,N3,N0,N2,X2,S138,-1.000)
       deallocate(S138)
C
       call sumx2314(N0,N3,N1,N3,N1,N3,N2,N3,N0,N2,X2,IntM, 1.000)
C
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(U2(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K3
       I2=K1*K1*K3
       I3=K3
       call jungemm(I1,I2,I3,X2,D2,U2)
       deallocate(D2)
C
       call
     & sum346125(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U2, 1.000)
       call
     & sum146325(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U2,-1.000)
       deallocate(U2)
       deallocate(X2)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:N1,N0+1:N2))
       call reorder3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N2,N0,N1,N0,N2,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S7(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
       I1=K2*K1*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S7)
       deallocate(D1)
       deallocate(B2)
C
       call sum2134(N0,N2,N1,N3,N0,N1,N0,N2,X5,S7,-1.000)
       deallocate(S7)
C
       allocate(D1(N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N2))
       call reorder3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N2,N1,N3,N0,N2,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S9(N0+1:N1,N0+1:N2,N1+1:N3,N0+1:N2))
       I1=K2*K3*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S9)
       deallocate(D1)
       deallocate(B2)
C
       call sum3124(N0,N2,N1,N3,N0,N1,N0,N2,X5,S9, 1.000)
       deallocate(S9)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call reorder4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N1,N3,N0,N1,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S25(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S25)
       deallocate(D1)
       deallocate(B2)
C
       call sum4123(N0,N2,N1,N3,N0,N1,N0,N2,X5,S25, 1.000)
       deallocate(S25)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N0,N2,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S47(N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S47)
       deallocate(D1)
       deallocate(D2)
C
       call sum2314(N0,N2,N1,N3,N0,N1,N0,N2,X5,S47,-1.000)
       deallocate(S47)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(0,N3,0,N3,
     & N2,N3,N0,N2,FockB,B1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N2))
       call reorder1243(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N1,N0,N2,t2C,D2)
       allocate(S61(N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K1*K3
       I3=K4
       call jungemm(I1,I2,I3,B1,D2,S61)
       deallocate(B1)
       deallocate(D2)
C
       call sum2341(N0,N2,N1,N3,N0,N1,N0,N2,X5,S61,-1.000)
       deallocate(S61)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2C,D2)
       allocate(S62(N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S62)
       deallocate(D1)
       deallocate(D2)
C
       call sum2314(N0,N2,N1,N3,N0,N1,N0,N2,X5,S62,-1.000)
       deallocate(S62)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       call reorder3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N0,N2,N0,N1,IntM,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2C,D2)
       allocate(S66(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K2*K3
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,D2,S66)
       deallocate(D1)
       deallocate(D2)
C
       call sum2413(N0,N2,N1,N3,N0,N1,N0,N2,X5,S66, 1.000)
       deallocate(S66)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3))
       call reorder4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N0,N2,N1,N3,IntM,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N2))
       call reorder1243(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N1,N0,N2,t2C,D2)
       allocate(S69(N0+1:N1,N0+1:N2,N0+1:N2,N1+1:N3))
       I1=K3*K2
       I2=K2*K1
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,D2,S69)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N2,N1,N3,N0,N1,N0,N2,X5,S69,-1.000)
       deallocate(S69)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N0,N2,IntM,D1)
       allocate(F2(N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder421356(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,t3C,F2)
       allocate(S76(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
       I1=K2
       I2=K1*K2*K3
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,D1,F2,S76)
       deallocate(D1)
       deallocate(F2)
C
       call sum2431(N0,N2,N1,N3,N0,N1,N0,N2,X5,S76,-1.000)
       deallocate(S76)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder412356(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N1,N3,N0,N2,N0,N1,t3D,F2)
       allocate(S84(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
       I1=K2
       I2=K1*K2*K3
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,D1,F2,S84)
       deallocate(D1)
       deallocate(F2)
C
       call sum2431(N0,N2,N1,N3,N0,N1,N0,N2,X5,S84,-0.500)
       deallocate(S84)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,N0+1:N1))
       call reorder3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N2,N2,N3,N0,N1,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S19(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S19)
       deallocate(D1)
       deallocate(B2)
C
       call sum3124(N0,N2,N2,N3,N1,N3,N0,N1,X20,S19,-1.000)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder2314(N1,N3,N0,N2,N2,N3,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,S19,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S104(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S104)
       deallocate(D1)
       deallocate(B2)
C
       call sum3124(N2,N3,N1,N3,N2,N3,N0,N1,X6,S104, 1.000)
       deallocate(S104)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call reorder3214(N1,N3,N0,N2,N2,N3,N0,N1,
     & N2,N3,N0,N2,N1,N3,N0,N1,S19,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S103(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S103)
       deallocate(D1)
       deallocate(B2)
       deallocate(S19)
C
       call sum4123(N0,N2,N1,N3,N0,N1,N0,N2,X5,S103,-1.000)
       deallocate(S103)
C
       allocate(D1(N1+1:N3,N0+1:N2,N2+1:N3,N1+1:N3))
       call reorder3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N2,N2,N3,N1,N3,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S20(N0+1:N1,N0+1:N2,N2+1:N3,N1+1:N3))
       I1=K3*K4*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S20)
       deallocate(D1)
       deallocate(B2)
C
       call sum4123(N0,N2,N2,N3,N1,N3,N0,N1,X20,S20, 1.000)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder2341(N0,N1,N0,N2,N2,N3,N1,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,S20,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S106(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S106)
       deallocate(D1)
       deallocate(B2)
C
       call sum3124(N2,N3,N1,N3,N2,N3,N0,N1,X6,S106,-1.000)
       deallocate(S106)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call reorder3241(N0,N1,N0,N2,N2,N3,N1,N3,
     & N2,N3,N0,N2,N1,N3,N0,N1,S20,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S105(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S105)
       deallocate(D1)
       deallocate(B2)
       deallocate(S20)
C
       call sum4123(N0,N2,N1,N3,N0,N1,N0,N2,X5,S105, 1.000)
       deallocate(S105)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N2,N3,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q26(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q26)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q27(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,Q26,B2,Q27)
       deallocate(B2)
C
       allocate(X11(N2+1:N3,N2+1:N3))
       X11=0.0d0
       call sum21(N2,N3,N2,N3,X11,Q27,-1.000)
       deallocate(Q27)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q26,B1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N2))
       call reorder1243(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N1,N0,N2,t2C,D2)
       allocate(S129(N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K1*K3
       I3=K4
       call jungemm(I1,I2,I3,B1,D2,S129)
       deallocate(B1)
       deallocate(D2)
       deallocate(Q26)
C
       call sum2341(N0,N2,N1,N3,N0,N1,N0,N2,X5,S129,-1.000)
       deallocate(S129)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N2,N3,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S58(N1+1:N3,N0+1:N1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S58)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N2,N2,N3,N1,N3,N0,N1,X20,S58,-1.000)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3412(N1,N3,N0,N1,N0,N2,N2,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,S58,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S141(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S141)
       deallocate(D1)
       deallocate(B2)
C
       call sum3124(N2,N3,N1,N3,N2,N3,N0,N1,X6,S141, 1.000)
       deallocate(S141)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call reorder4312(N1,N3,N0,N1,N0,N2,N2,N3,
     & N2,N3,N0,N2,N1,N3,N0,N1,S58,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S137(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S137)
       deallocate(D1)
       deallocate(B2)
       deallocate(S58)
C
       call sum4123(N0,N2,N1,N3,N0,N1,N0,N2,X5,S137,-1.000)
       deallocate(S137)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2C,D2)
       allocate(S85(N1+1:N3,N0+1:N1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S85)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N2,N2,N3,N1,N3,N0,N1,X20,S85,-1.000)
C
       call sumx3142(N0,N3,N0,N2,N2,N3,N1,N3,N0,N1,X20,IntM, 1.000)
C
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder412356(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N1,N3,N0,N2,N0,N1,t3D,F2)
       allocate(U20(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K2*K3*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,X20,F2,U20)
       deallocate(F2)
C
       call
     & sum235614(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U20, 1.000)
       call
     & sum215634(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U20,-1.000)
       call
     & sum235416(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U20,-1.000)
       call
     & sum215436(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U20, 1.000)
       deallocate(U20)
       deallocate(X20)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3412(N1,N3,N0,N1,N0,N2,N2,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,S85,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S144(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S144)
       deallocate(D1)
       deallocate(B2)
C
       call sum3124(N2,N3,N1,N3,N2,N3,N0,N1,X6,S144, 1.000)
       deallocate(S144)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call reorder4312(N1,N3,N0,N1,N0,N2,N2,N3,
     & N2,N3,N0,N2,N1,N3,N0,N1,S85,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S143(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S143)
       deallocate(D1)
       deallocate(B2)
       deallocate(S85)
C
       call sum4123(N0,N2,N1,N3,N0,N1,N0,N2,X5,S143,-1.000)
       deallocate(S143)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder4132(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q30(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q30)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q31(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,Q30,B2,Q31)
       deallocate(B2)
C
       call sum21(N2,N3,N2,N3,X11,Q31, 1.000)
       deallocate(Q31)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q30,B1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N2))
       call reorder1243(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N1,N0,N2,t2C,D2)
       allocate(S145(N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K1*K3
       I3=K4
       call jungemm(I1,I2,I3,B1,D2,S145)
       deallocate(B1)
       deallocate(D2)
       deallocate(Q30)
C
       call sum2341(N0,N2,N1,N3,N0,N1,N0,N2,X5,S145, 1.000)
       deallocate(S145)
C
       call sumx2134(N0,N3,N0,N2,N1,N3,N0,N1,N0,N2,X5,IntM, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2C,D2)
       allocate(U5(N2+1:N3,N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N2))
       I1=K2*K1*K3
       I2=K1*K3*K4
       I3=K2
       call jungemm(I1,I2,I3,X5,D2,U5)
       deallocate(D2)
C
       call
     & sum236145(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U5,-1.000)
       call
     & sum216345(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U5, 1.000)
       call
     & sum234165(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U5, 1.000)
       call
     & sum214365(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U5,-1.000)
       deallocate(U5)
       deallocate(X5)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       call reorder3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N2,N3,N0,N1,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S8(N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4*K4
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S8)
       deallocate(D1)
       deallocate(B2)
C
       call sum2134(N2,N3,N1,N3,N2,N3,N0,N1,X6,S8,-1.000)
       deallocate(S8)
C
       allocate(D1(N1+1:N3,N2+1:N3,N1+1:N3,N2+1:N3))
       call reorder3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N2,N3,N1,N3,N2,N3,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S10(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       I1=K4*K3*K4
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S10)
       deallocate(D1)
       deallocate(B2)
C
       call sum4123(N2,N3,N1,N3,N2,N3,N0,N1,X6,S10, 1.000)
       deallocate(S10)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S26(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S26)
       deallocate(D1)
       deallocate(B2)
C
       call sum3124(N2,N3,N1,N3,N2,N3,N0,N1,X6,S26,-1.000)
       deallocate(S26)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3))
       call reorder3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S50(N1+1:N3,N0+1:N1,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S50)
       deallocate(D1)
       deallocate(D2)
C
       call sum2413(N2,N3,N1,N3,N2,N3,N0,N1,X6,S50,-1.000)
       deallocate(S50)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call reorder2341(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2C,D2)
       allocate(S63(N1+1:N3,N0+1:N1,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S63)
       deallocate(D1)
       deallocate(D2)
C
       call sum2413(N2,N3,N1,N3,N2,N3,N0,N1,X6,S63, 1.000)
       deallocate(S63)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call reorder4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N0,N1,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2C,D2)
       allocate(S64(N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K3*K4
       I3=K1*K2
       call jungemm(I1,I2,I3,D1,D2,S64)
       deallocate(D1)
       deallocate(D2)
C
       call sum3214(N2,N3,N1,N3,N2,N3,N0,N1,X6,S64,-1.000)
       deallocate(S64)
C
       allocate(D1(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:N3))
       call reorder2341(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,N3,N0,N1,t2C,D2)
       allocate(S68(N2+1:N3,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K1*K4
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,D2,S68)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N2,N3,N1,N3,N2,N3,N0,N1,X6,S68, 1.000)
       deallocate(S68)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call reorder4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(F2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder541236(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,N1,N3,N0,N1,t3C,F2)
       allocate(S73(N2+1:N3,N1+1:N3,N0+1:N1,N2+1:N3))
       I1=K4
       I2=K1*K3*K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,F2,S73)
       deallocate(D1)
       deallocate(F2)
C
       call sum3241(N2,N3,N1,N3,N2,N3,N0,N1,X6,S73, 1.000)
       deallocate(S73)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,t3D,F2)
       allocate(S83(N2+1:N3,N1+1:N3,N0+1:N1,N2+1:N3))
       I1=K4
       I2=K1*K3*K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,F2,S83)
       deallocate(D1)
       deallocate(F2)
C
       call sum3241(N2,N3,N1,N3,N2,N3,N0,N1,X6,S83, 0.500)
       deallocate(S83)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder2314(N1,N3,N0,N2,N2,N3,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,S98,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S149(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S149)
       deallocate(D1)
       deallocate(B2)
       deallocate(S98)
C
       call sum3124(N2,N3,N1,N3,N2,N3,N0,N1,X6,S149, 1.000)
       deallocate(S149)
C
       call sumx2341(N0,N3,N2,N3,N1,N3,N2,N3,N0,N1,X6,IntM, 1.000)
C
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,t2C,D2)
       allocate(U6(N1+1:N3,N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4*K3
       I2=K1*K2*K3
       I3=K4
       call jungemm(I1,I2,I3,X6,D2,U6)
       deallocate(D2)
C
       call
     & sum356124(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U6, 1.000)
       call
     & sum156324(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U6,-1.000)
       call
     & sum354126(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U6,-1.000)
       call
     & sum154326(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U6, 1.000)
       deallocate(U6)
       deallocate(X6)
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
       X11=X11+Q6
       deallocate(Q6)
C
       allocate(B1(N0+1:N2,N2+1:N3))
       call reorder12(0,N3,0,N3,
     & N0,N2,N2,N3,FockB,B1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q8(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,B1,B2,Q8)
       deallocate(B1)
       deallocate(B2)
C
       call sum21(N2,N3,N2,N3,X11,Q8,-1.000)
       deallocate(Q8)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call reorder2341(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q10(N2+1:N3,N2+1:N3))
       I1=K4*K4
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q10)
       deallocate(D1)
       deallocate(B2)
C
       X11=X11-Q10
       deallocate(Q10)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2B,D2)
       allocate(Q16(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,D2,Q16)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N2,N3,N2,N3,X11,Q16,-0.500)
       deallocate(Q16)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call reorder4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,t2C,D2)
       allocate(Q20(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,D2,Q20)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N2,N3,N2,N3,X11,Q20, 1.000)
       deallocate(Q20)
C
       call sumx21(0,N3,N2,N3,N2,N3,X11,FockB, 1.000)
C
       allocate(F2(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N1))
       call reorder213456(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,
     & N2,N3,N1,N3,N1,N3,N0,N1,N0,N2,N0,N1,t3C,F2)
       allocate(U11(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4
       I2=K1*K2*K1*K3*K3
       I3=K4
       call jungemm(I1,I2,I3,X11,F2,U11)
       deallocate(F2)
C
       call
     & sum134562(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U11, 1.000)
       deallocate(U11)
       deallocate(X11)
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
       X10=X10+Q5
       deallocate(Q5)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(0,N3,0,N3,
     & N2,N3,N0,N2,FockB,B1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(Q7(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,B1,B2,Q7)
       deallocate(B1)
       deallocate(B2)
C
       call sum21(N0,N2,N0,N2,X10,Q7, 1.000)
       deallocate(Q7)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q9(N0+1:N2,N0+1:N2))
       I1=K2*K2
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q9)
       deallocate(D1)
       deallocate(B2)
C
       X10=X10+Q9
       deallocate(Q9)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2B,D2)
       allocate(Q15(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,D1,D2,Q15)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N0,N2,N0,N2,X10,Q15, 0.500)
       deallocate(Q15)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N0,N2,IntM,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2C,D2)
       allocate(Q18(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,D1,D2,Q18)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N0,N2,N0,N2,X10,Q18,-1.000)
       deallocate(Q18)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       call reorder1432(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N0,N2,N2,N3,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S133(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S133)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call reorder3241(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,S133,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S134(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S134)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X15(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       X15=0.0d0
       call sum3124(N0,N2,N2,N3,N2,N3,N0,N2,X15,S134, 1.000)
       deallocate(S134)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder3421(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,S133,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q29(N0+1:N2,N0+1:N2))
       I1=K2*K2
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q29)
       deallocate(D1)
       deallocate(B2)
       deallocate(S133)
C
       X10=X10-Q29
       deallocate(Q29)
C
       call sumx21(0,N3,N0,N2,N0,N2,X10,FockB, 1.000)
C
       allocate(F2(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder512346(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,N3,N1,N3,N0,N1,N0,N1,t3C,F2)
       allocate(U10(N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N2))
       I1=K2
       I2=K1*K1*K3*K4*K3
       I3=K2
       call jungemm(I1,I2,I3,X10,F2,U10)
       deallocate(F2)
C
       call
     & sum123465(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U10,-1.000)
       deallocate(U10)
       deallocate(X10)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call reorder3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S29(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S29)
       deallocate(D1)
       deallocate(B2)
C
       call sum3124(N0,N2,N2,N3,N2,N3,N0,N2,X15,S29,-1.000)
       deallocate(S29)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S30(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S30)
       deallocate(D1)
       deallocate(B2)
C
       call sum4123(N0,N2,N2,N3,N2,N3,N0,N2,X15,S30,-1.000)
       deallocate(S30)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2B,D2)
       allocate(S60(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S60)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N2,N2,N3,N2,N3,N0,N2,X15,S60, 1.000)
       deallocate(S60)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N2,N3,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S81(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S81)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N2,N2,N3,N2,N3,N0,N2,X15,S81, 1.000)
       deallocate(S81)
C
       call sumx3124(N0,N3,N0,N2,N2,N3,N2,N3,N0,N2,X15,IntB, 1.000)
C
       allocate(F2(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder521346(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N1,N3,N0,N1,N0,N1,t3C,F2)
       allocate(U15(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K1*K3*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,X15,F2,U15)
       deallocate(F2)
C
       call
     & sum134625(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U15,-1.000)
       deallocate(U15)
       deallocate(X15)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(S55(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K3*K3
       I3=K1*K1
       call jungemm(I1,I2,I3,D1,D2,S55)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X14(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       X14=0.0d0
       call sum3412(N1,N3,N1,N3,N1,N3,N1,N3,X14,S55,-0.500)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       call reorder4312(N1,N3,N1,N3,N1,N3,N1,N3,
     & N1,N3,N1,N3,N1,N3,N1,N3,S55,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S117(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       I1=K3*K3*K3
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S117)
       deallocate(D1)
       deallocate(B2)
       deallocate(S55)
C
       call sum4123(N1,N3,N1,N3,N1,N3,N0,N1,X4,S117, 0.500)
       deallocate(S117)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S95(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S95)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call reorder2431(N1,N3,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,S95,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q22(N1+1:N3,N1+1:N3))
       I1=K3*K3
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q22)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X9(N1+1:N3,N1+1:N3))
       X9=0.0d0
       X9=X9+Q22
       deallocate(Q22)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call reorder2341(N1,N3,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,S95,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S96(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       I1=K3*K3*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S96)
       deallocate(D1)
       deallocate(B2)
       deallocate(S95)
C
       call sum3124(N1,N3,N1,N3,N1,N3,N1,N3,X14,S96, 1.000)
       deallocate(S96)
C
       call sumx4321(N0,N3,N1,N3,N1,N3,N1,N3,N1,N3,X14,IntR, 1.000)
C
       allocate(F2(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N1,N0+1:N2,N0+1:N1))
       call reorder132456(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,
     & N1,N3,N1,N3,N2,N3,N0,N1,N0,N2,N0,N1,t3C,F2)
       allocate(U14(N2+1:N3,N0+1:N1,N0+1:N2,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K1*K2*K1*K4
       I3=K3*K3
       call jungemm(I1,I2,I3,X14,F2,U14)
       deallocate(F2)
C
       call
     & sum245613(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U14, 0.500)
       deallocate(U14)
       deallocate(X14)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       call reorder3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N1,N3,N1,N3,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S6(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       I1=K3*K3*K3
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S6)
       deallocate(D1)
       deallocate(B2)
C
       call sum4123(N1,N3,N1,N3,N1,N3,N0,N1,X4,S6,-1.000)
       deallocate(S6)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call reorder4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(S41(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K3*K3
       I3=K1*K1
       call jungemm(I1,I2,I3,D1,D2,S41)
       deallocate(D1)
       deallocate(D2)
C
       call sum2314(N1,N3,N1,N3,N1,N3,N0,N1,X4,S41,-0.500)
       deallocate(S41)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,t3A,F2)
       allocate(S70(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1*K3*K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,F2,S70)
       deallocate(D1)
       deallocate(F2)
C
       call sum2341(N1,N3,N1,N3,N1,N3,N0,N1,X4,S70,-0.500)
       deallocate(S70)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder542136(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,t3C,F2)
       allocate(S74(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1*K3*K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,F2,S74)
       deallocate(D1)
       deallocate(F2)
C
       call sum2341(N1,N3,N1,N3,N1,N3,N0,N1,X4,S74,-1.000)
       deallocate(S74)
C
       call sumx3214(N0,N3,N1,N3,N1,N3,N1,N3,N0,N1,X4,IntR, 1.000)
C
       allocate(D2(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,N3,N0,N2,N0,N1,t2C,D2)
       allocate(U4(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1*K2*K4
       I3=K3
       call jungemm(I1,I2,I3,X4,D2,U4)
       deallocate(D2)
C
       call
     & sum256134(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U4, 1.000)
       call
     & sum254136(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U4,-1.000)
       deallocate(U4)
       deallocate(X4)
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
       call sum21(N1,N3,N1,N3,X9,Q2,-1.000)
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
       X9=X9-Q4
       deallocate(Q4)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call reorder2431(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q12(N1+1:N3,N1+1:N3))
       I1=K3*K3
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q12)
       deallocate(D1)
       deallocate(B2)
C
       X9=X9+Q12
       deallocate(Q12)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(Q14(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,D2,Q14)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N1,N3,N1,N3,X9,Q14,-0.500)
       deallocate(Q14)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2C,D2)
       allocate(Q19(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,Q19)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N1,N3,N1,N3,X9,Q19, 1.000)
       deallocate(Q19)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,N1+1:N3))
       call reorder3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N2,N2,N3,N1,N3,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S111(N1+1:N3,N0+1:N2,N2+1:N3,N1+1:N3))
       I1=K3*K4*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S111)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call reorder2341(N1,N3,N0,N2,N2,N3,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,S111,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S112(N2+1:N3,N2+1:N3,N1+1:N3,N1+1:N3))
       I1=K3*K3*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S112)
       deallocate(D1)
       deallocate(B2)
C
       call sum4123(N2,N3,N1,N3,N1,N3,N2,N3,X19,S112, 1.000)
       deallocate(S112)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call reorder2341(N1,N3,N0,N2,N2,N3,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,S111,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q25(N1+1:N3,N1+1:N3))
       I1=K3*K3
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q25)
       deallocate(D1)
       deallocate(B2)
       deallocate(S111)
C
       X9=X9-Q25
       deallocate(Q25)
C
       call sumx21(0,N3,N1,N3,N1,N3,X9,FockR, 1.000)
C
       allocate(F2(N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N1))
       call reorder123456(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,
     & N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,t3C,F2)
       allocate(U9(N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1*K2*K1*K3*K4
       I3=K3
       call jungemm(I1,I2,I3,X9,F2,U9)
       deallocate(F2)
C
       call
     & sum234561(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U9, 1.000)
       call
     & sum214563(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U9,-1.000)
       deallocate(U9)
       deallocate(X9)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call reorder2431(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S34(N2+1:N3,N2+1:N3,N1+1:N3,N1+1:N3))
       I1=K3*K3*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S34)
       deallocate(D1)
       deallocate(B2)
C
       call sum4123(N2,N3,N1,N3,N1,N3,N2,N3,X19,S34,-1.000)
       deallocate(S34)
C
       call sumx3421(N0,N3,N2,N3,N1,N3,N1,N3,N2,N3,X19,IntM, 1.000)
C
       allocate(F2(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N1))
       call reorder213456(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,
     & N2,N3,N1,N3,N1,N3,N0,N1,N0,N2,N0,N1,t3C,F2)
       allocate(U19(N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       I1=K4*K3
       I2=K1*K2*K1*K3
       I3=K3*K4
       call jungemm(I1,I2,I3,X19,F2,U19)
       deallocate(F2)
C
       call
     & sum345612(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U19, 1.000)
       call
     & sum145632(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U19,-1.000)
       deallocate(U19)
       deallocate(X19)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call reorder4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N0,N1,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S32(N2+1:N3,N0+1:N1,N2+1:N3,N0+1:N1))
       I1=K1*K4*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S32)
       deallocate(D1)
       deallocate(B2)
C
       call sum3124(N0,N1,N2,N3,N2,N3,N0,N1,X17,S32,-1.000)
       deallocate(S32)
C
       call sumx4213(N0,N3,N0,N1,N2,N3,N2,N3,N0,N1,X17,IntM, 1.000)
C
       allocate(F2(N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder421356(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,t3C,F2)
       allocate(U17(N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K1*K2*K3*K3
       I3=K4*K1
       call jungemm(I1,I2,I3,X17,F2,U17)
       deallocate(F2)
C
       call
     & sum135624(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U17,-1.000)
       call
     & sum135426(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U17, 1.000)
       deallocate(U17)
       deallocate(X17)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call reorder2431(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S14(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       I1=K3*K3*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S14)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       call reorder2341(N1,N3,N1,N3,N1,N3,N1,N3,
     & N1,N3,N1,N3,N1,N3,N1,N3,S14,D1)
       allocate(F2(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N1,N0+1:N2,N0+1:N1))
       call reorder132456(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,
     & N1,N3,N1,N3,N2,N3,N0,N1,N0,N2,N0,N1,t3C,F2)
       allocate(U37(N2+1:N3,N0+1:N1,N0+1:N2,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K1*K2*K1*K4
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,F2,U37)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum245613(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U37, 0.500)
       call
     & sum245631(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U37,-0.500)
       deallocate(U37)
       deallocate(S14)
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
       call sum21(N0,N1,N0,N1,X8,Q1, 1.000)
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
       X8=X8+Q3
       deallocate(Q3)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call reorder4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N0,N1,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q11(N0+1:N1,N0+1:N1))
       I1=K1*K1
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q11)
       deallocate(D1)
       deallocate(B2)
C
       X8=X8+Q11
       deallocate(Q11)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(Q13(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,D1,D2,Q13)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N0,N1,N0,N1,X8,Q13, 0.500)
       deallocate(Q13)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2C,D2)
       allocate(Q17(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,D1,D2,Q17)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N0,N1,N0,N1,X8,Q17,-1.000)
       deallocate(Q17)
C
       call sumx21(0,N3,N0,N1,N0,N1,X8,FockR, 1.000)
C
       allocate(F2(N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder412356(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N1,N3,N0,N2,N0,N1,t3C,F2)
       allocate(U8(N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K2*K3*K4*K3
       I3=K1
       call jungemm(I1,I2,I3,X8,F2,U8)
       deallocate(F2)
C
       call
     & sum123564(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U8,-1.000)
       call
     & sum123546(N1,N3,N2,N3,N1,N3,N0,N1,N0,N2,N0,N1,V3C,U8, 1.000)
       deallocate(U8)
       deallocate(X8)
C
       do i=N0+1,N1-1
       do j=N0+1,N2
       do k=i+1,N1
       do a=N1+1,N3-1
       do b=N2+1,N3
       do c=a+1,N3
         CoeLeft=FockR(c,c)
     &          +FockB(b,b)
     &          +FockR(a,a)
     &          -FockR(k,k)
     &          -FockB(j,j)
     &          -FockR(i,i)
     &          +shift
         if (p_space(c,b,a,k,j,i).eq.0) then
         t3C(c,b,a,k,j,i)=0.0d0
         else
         t3C(c,b,a,k,j,i)=t3C(c,b,a,k,j,i)-V3C(c,b,a,k,j,i)/CoeLeft
         endif
         t3C(c,b,a,i,j,k)=-t3C(c,b,a,k,j,i)
         t3C(a,b,c,k,j,i)=-t3C(c,b,a,k,j,i)
         t3C(a,b,c,i,j,k)= t3C(c,b,a,k,j,i)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
