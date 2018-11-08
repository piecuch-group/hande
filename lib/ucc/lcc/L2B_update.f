       subroutine L2B_update(N0,N1,N2,N3,V2B,
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
       real*8 V2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
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
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::Q6(:,:)
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::Q8(:,:)
       real*8,allocatable::S25(:,:,:,:)
       real*8,allocatable::S27(:,:,:,:)
       real*8,allocatable::S29(:,:,:,:)
       real*8,allocatable::S32(:,:,:,:)
       real*8,allocatable::S31(:,:,:,:)
       real*8,allocatable::S100(:,:,:,:)
       real*8,allocatable::S34(:,:,:,:)
       real*8,allocatable::S35(:,:,:,:)
       real*8,allocatable::S37(:,:,:,:)
       real*8,allocatable::Q9(:,:)
       real*8,allocatable::Q10(:,:)
       real*8,allocatable::S45(:,:,:,:)
       real*8,allocatable::S47(:,:,:,:)
       real*8,allocatable::S49(:,:,:,:)
       real*8,allocatable::S52(:,:,:,:)
       real*8,allocatable::S51(:,:,:,:)
       real*8,allocatable::S127(:,:,:,:)
       real*8,allocatable::S54(:,:,:,:)
       real*8,allocatable::S55(:,:,:,:)
       real*8,allocatable::S57(:,:,:,:)
       real*8,allocatable::S59(:,:,:,:)
       real*8,allocatable::S67(:,:,:,:)
       real*8,allocatable::S146(:,:,:,:)
       real*8,allocatable::S61(:,:,:,:)
       real*8,allocatable::S65(:,:,:,:)
       real*8,allocatable::S152(:,:,:,:)
       real*8,allocatable::S63(:,:,:,:)
       real*8,allocatable::S69(:,:,:,:)
       real*8,allocatable::S71(:,:,:,:)
       real*8,allocatable::S75(:,:,:,:)
       real*8,allocatable::S161(:,:,:,:)
       real*8,allocatable::S73(:,:,:,:)
       real*8,allocatable::S77(:,:,:,:)
       real*8,allocatable::S78(:,:,:,:)
       real*8,allocatable::S80(:,:,:,:)
       real*8,allocatable::S88(:,:,:,:)
       real*8,allocatable::S165(:,:,:,:)
       real*8,allocatable::S82(:,:,:,:)
       real*8,allocatable::S86(:,:,:,:)
       real*8,allocatable::S84(:,:,:,:)
       real*8,allocatable::S90(:,:,:,:)
       real*8,allocatable::S98(:,:,:,:)
       real*8,allocatable::S92(:,:,:,:)
       real*8,allocatable::S96(:,:,:,:)
       real*8,allocatable::S94(:,:,:,:)
       real*8,allocatable::S102(:,:,:,:)
       real*8,allocatable::S107(:,:,:,:)
       real*8,allocatable::S104(:,:,:,:)
       real*8,allocatable::S109(:,:,:,:)
       real*8,allocatable::S115(:,:,:,:)
       real*8,allocatable::S194(:,:,:,:)
       real*8,allocatable::S111(:,:,:,:)
       real*8,allocatable::S117(:,:,:,:)
       real*8,allocatable::S113(:,:,:,:)
       real*8,allocatable::Q11(:,:)
       real*8,allocatable::Q12(:,:)
       real*8,allocatable::Q13(:,:)
       real*8,allocatable::Q14(:,:)
       real*8,allocatable::S129(:,:,:,:)
       real*8,allocatable::S134(:,:,:,:)
       real*8,allocatable::S131(:,:,:,:)
       real*8,allocatable::S136(:,:,:,:)
       real*8,allocatable::S142(:,:,:,:)
       real*8,allocatable::S211(:,:,:,:)
       real*8,allocatable::S138(:,:,:,:)
       real*8,allocatable::S144(:,:,:,:)
       real*8,allocatable::S140(:,:,:,:)
       real*8,allocatable::S148(:,:,:,:)
       real*8,allocatable::S150(:,:,:,:)
       real*8,allocatable::S155(:,:,:,:)
       real*8,allocatable::S163(:,:,:,:)
       real*8,allocatable::S157(:,:,:,:)
       real*8,allocatable::S159(:,:,:,:)
       real*8,allocatable::S167(:,:,:,:)
       real*8,allocatable::S169(:,:,:,:)
       real*8,allocatable::S171(:,:,:,:)
       real*8,allocatable::S172(:,:,:,:)
       real*8,allocatable::S175(:,:,:,:)
       real*8,allocatable::S184(:,:,:,:)
       real*8,allocatable::S177(:,:,:,:)
       real*8,allocatable::S179(:,:,:,:)
       real*8,allocatable::S181(:,:,:,:)
       real*8,allocatable::S182(:,:,:,:)
       real*8,allocatable::S186(:,:,:,:)
       real*8,allocatable::S188(:,:,:,:)
       real*8,allocatable::S190(:,:,:,:)
       real*8,allocatable::S192(:,:,:,:)
       real*8,allocatable::S196(:,:,:,:)
       real*8,allocatable::S197(:,:,:,:)
       real*8,allocatable::Q15(:,:)
       real*8,allocatable::Q16(:,:)
       real*8,allocatable::Q17(:,:)
       real*8,allocatable::Q18(:,:)
       real*8,allocatable::S203(:,:,:,:)
       real*8,allocatable::S205(:,:,:,:)
       real*8,allocatable::S207(:,:,:,:)
       real*8,allocatable::S209(:,:,:,:)
       real*8,allocatable::S213(:,:,:,:)
       real*8,allocatable::S214(:,:,:,:)
       real*8,allocatable::Q19(:,:)
       real*8,allocatable::Q20(:,:)
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
       real*8,allocatable::U15(:,:,:,:)
       real*8,allocatable::U16(:,:,:,:)
       real*8,allocatable::U17(:,:,:,:)
       real*8,allocatable::U18(:,:,:,:)
       real*8,allocatable::U19(:,:,:,:)
       real*8,allocatable::U20(:,:,:,:)
       real*8,allocatable::U21(:,:,:,:)
       real*8,allocatable::U22(:,:,:,:)
       real*8,allocatable::U23(:,:,:,:)
       real*8,allocatable::U24(:,:,:,:)
       real*8,allocatable::U26(:,:,:,:)
       real*8,allocatable::U28(:,:,:,:)
       real*8,allocatable::U30(:,:,:,:)
       real*8,allocatable::U33(:,:,:,:)
       real*8,allocatable::X1(:,:,:,:)
       real*8,allocatable::U106(:,:,:,:)
       real*8,allocatable::U36(:,:,:,:)
       real*8,allocatable::U38(:,:,:,:)
       real*8,allocatable::U39(:,:,:,:)
       real*8,allocatable::U40(:,:,:,:)
       real*8,allocatable::U41(:,:,:,:)
       real*8,allocatable::U42(:,:,:,:)
       real*8,allocatable::U43(:,:,:,:)
       real*8,allocatable::U44(:,:,:,:)
       real*8,allocatable::U46(:,:,:,:)
       real*8,allocatable::U48(:,:,:,:)
       real*8,allocatable::U50(:,:,:,:)
       real*8,allocatable::U53(:,:,:,:)
       real*8,allocatable::X2(:,:,:,:)
       real*8,allocatable::U133(:,:,:,:)
       real*8,allocatable::U56(:,:,:,:)
       real*8,allocatable::U58(:,:,:,:)
       real*8,allocatable::X3(:,:,:,:)
       real*8,allocatable::U60(:,:,:,:)
       real*8,allocatable::X4(:,:,:,:)
       real*8,allocatable::U154(:,:,:,:)
       real*8,allocatable::X5(:,:,:,:)
       real*8,allocatable::U62(:,:,:,:)
       real*8,allocatable::U153(:,:,:,:)
       real*8,allocatable::U64(:,:,:,:)
       real*8,allocatable::U70(:,:,:,:)
       real*8,allocatable::X6(:,:,:,:)
       real*8,allocatable::U72(:,:,:,:)
       real*8,allocatable::U162(:,:,:,:)
       real*8,allocatable::U74(:,:,:,:)
       real*8,allocatable::U79(:,:,:,:)
       real*8,allocatable::X7(:,:,:,:)
       real*8,allocatable::U81(:,:,:,:)
       real*8,allocatable::X8(:,:,:,:)
       real*8,allocatable::U174(:,:,:,:)
       real*8,allocatable::U83(:,:,:,:)
       real*8,allocatable::U87(:,:,:,:)
       real*8,allocatable::U85(:,:,:,:)
       real*8,allocatable::U91(:,:,:,:)
       real*8,allocatable::U99(:,:,:,:)
       real*8,allocatable::U93(:,:,:,:)
       real*8,allocatable::U97(:,:,:,:)
       real*8,allocatable::U95(:,:,:,:)
       real*8,allocatable::U103(:,:,:,:)
       real*8,allocatable::U108(:,:,:,:)
       real*8,allocatable::U105(:,:,:,:)
       real*8,allocatable::X9(:,:,:,:)
       real*8,allocatable::U110(:,:,:,:)
       real*8,allocatable::U195(:,:,:,:)
       real*8,allocatable::U112(:,:,:,:)
       real*8,allocatable::U118(:,:,:,:)
       real*8,allocatable::U114(:,:,:,:)
       real*8,allocatable::U119(:,:,:,:)
       real*8,allocatable::U120(:,:,:,:)
       real*8,allocatable::U121(:,:,:,:)
       real*8,allocatable::U122(:,:,:,:)
       real*8,allocatable::U123(:,:,:,:)
       real*8,allocatable::U124(:,:,:,:)
       real*8,allocatable::U125(:,:,:,:)
       real*8,allocatable::U126(:,:,:,:)
       real*8,allocatable::U130(:,:,:,:)
       real*8,allocatable::U135(:,:,:,:)
       real*8,allocatable::U132(:,:,:,:)
       real*8,allocatable::X10(:,:,:,:)
       real*8,allocatable::U137(:,:,:,:)
       real*8,allocatable::U212(:,:,:,:)
       real*8,allocatable::U139(:,:,:,:)
       real*8,allocatable::U145(:,:,:,:)
       real*8,allocatable::U141(:,:,:,:)
       real*8,allocatable::U149(:,:,:,:)
       real*8,allocatable::U151(:,:,:,:)
       real*8,allocatable::U156(:,:,:,:)
       real*8,allocatable::U164(:,:,:,:)
       real*8,allocatable::U158(:,:,:,:)
       real*8,allocatable::U160(:,:,:,:)
       real*8,allocatable::U168(:,:,:,:)
       real*8,allocatable::U170(:,:,:,:)
       real*8,allocatable::U173(:,:,:,:)
       real*8,allocatable::U176(:,:,:,:)
       real*8,allocatable::U185(:,:,:,:)
       real*8,allocatable::U178(:,:,:,:)
       real*8,allocatable::U180(:,:,:,:)
       real*8,allocatable::U183(:,:,:,:)
       real*8,allocatable::U187(:,:,:,:)
       real*8,allocatable::U189(:,:,:,:)
       real*8,allocatable::U191(:,:,:,:)
       real*8,allocatable::U193(:,:,:,:)
       real*8,allocatable::U198(:,:,:,:)
       real*8,allocatable::U199(:,:,:,:)
       real*8,allocatable::U200(:,:,:,:)
       real*8,allocatable::U201(:,:,:,:)
       real*8,allocatable::U202(:,:,:,:)
       real*8,allocatable::U204(:,:,:,:)
       real*8,allocatable::U206(:,:,:,:)
       real*8,allocatable::U208(:,:,:,:)
       real*8,allocatable::U210(:,:,:,:)
       real*8,allocatable::U215(:,:,:,:)
       real*8,allocatable::U216(:,:,:,:)
       real*8,allocatable::U217(:,:,:,:)
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
       allocate(D1(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef2134(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N2,N3,N0,N2,N0,N1,IntM,D1)
       allocate(U6(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K4
       I2=K3
       I3=K3
       call jungemm(I1,I2,I3,D1,Q1,U6)
       deallocate(D1)
C
       call
     & sul2134(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U6,-1.0d0/2)
       deallocate(U6)
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
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N0,N2,IntM,D1)
       allocate(U7(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K4
       I2=K1
       I3=K1
       call jungemm(I1,I2,I3,D1,Q2,U7)
       deallocate(D1)
C
       call
     & sul4123(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U7,-1.0d0/2)
       deallocate(U7)
       deallocate(Q2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,l2B,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,t2B,D2)
       allocate(Q3(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,D2,Q3)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N0,N2,N0,N1,IntM,D1)
       allocate(U18(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K3
       I2=K4
       I3=K4
       call jungemm(I1,I2,I3,D1,Q3,U18)
       deallocate(D1)
C
       V2B=V2B-U18
       deallocate(U18)
       deallocate(Q3)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,l2B,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(Q4(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,D1,D2,Q4)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(U19(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K2
       I3=K2
       call jungemm(I1,I2,I3,D1,Q4,U19)
       deallocate(D1)
C
       call
     & sul3124(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U19,-1.0d0)
       deallocate(U19)
       deallocate(Q4)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,l2B,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2B,D2)
       allocate(Q5(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,Q5)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef2134(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N2,N3,N0,N2,N0,N1,IntM,D1)
       allocate(U20(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K4
       I2=K3
       I3=K3
       call jungemm(I1,I2,I3,D1,Q5,U20)
       deallocate(D1)
C
       call
     & sul2134(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U20,-1.0d0)
       deallocate(U20)
       deallocate(Q5)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,l2B,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(Q6(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,D1,D2,Q6)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N0,N2,IntM,D1)
       allocate(U21(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K4
       I2=K1
       I3=K1
       call jungemm(I1,I2,I3,D1,Q6,U21)
       deallocate(D1)
C
       call
     & sul4123(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U21,-1.0d0)
       deallocate(U21)
       deallocate(Q6)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,l2C,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(Q7(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,D2,Q7)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N0,N2,N0,N1,IntM,D1)
       allocate(U23(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K3
       I2=K4
       I3=K4
       call jungemm(I1,I2,I3,D1,Q7,U23)
       deallocate(D1)
C
       V2B=V2B-1.0d0/2*U23
       deallocate(U23)
       deallocate(Q7)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,l2C,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(Q8(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,D1,D2,Q8)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(U24(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K2
       I3=K2
       call jungemm(I1,I2,I3,D1,Q8,U24)
       deallocate(D1)
C
       call
     & sul3124(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U24,-1.0d0/2)
       deallocate(U24)
       deallocate(Q8)
C
       allocate(F1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef412536(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,N1,N3,N0,N1,l3A,F1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S25(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,F1,D2,S25)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N2,N3,N0,N2,IntM,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef2134(N0,N1,N0,N1,N1,N3,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,N1,S25,D2)
       allocate(U26(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K3
       I3=K1*K1
       call jungemm(I1,I2,I3,D1,D2,U26)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul2413(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U26,-1.0d0/2)
       deallocate(U26)
       deallocate(S25)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,l3A,F1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(S27(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,F1,D2,S27)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(U28(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K3
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,S27,U28)
       deallocate(D1)
C
       call
     & sul2413(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U28,1.0d0/2)
       deallocate(U28)
       deallocate(S27)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,l3A,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N0,N1,N1,N3,t3A,F2)
       allocate(S29(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K3*K1
       I3=K3*K3*K1*K1
       call jungemm(I1,I2,I3,F1,F2,S29)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(U30(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,S29,U30)
       deallocate(D1)
C
       call
     & sul2413(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U30,1.0d0/4)
       deallocate(U30)
       deallocate(S29)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N2,N3,N0,N2,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S32(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S32)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       X1=0.0d0
       X1=X1+S32
C
       call slx3142(N0,N3,N0,N1,N0,N1,N2,N3,N0,N2,X1,IntM,1.000d0)
C
       allocate(F1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef412536(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,N1,N3,N0,N1,l3A,F1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S31(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,F1,D2,S31)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef2134(N0,N1,N0,N1,N1,N3,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,N1,S31,D2)
       allocate(U33(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K3
       I3=K1*K1
       call jungemm(I1,I2,I3,S32,D2,U33)
       deallocate(D2)
C
       call
     & sul2413(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U33,-1.0d0/2)
       deallocate(U33)
       deallocate(S31)
C
       allocate(F1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef412536(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,N1,N3,N0,N1,l3B,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(S100(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S100)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef2134(N0,N1,N0,N1,N1,N3,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,N1,S100,D2)
       allocate(U106(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K3
       I3=K1*K1
       call jungemm(I1,I2,I3,X1,D2,U106)
       deallocate(D2)
C
       call
     & sul2413(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U106,-1.0d0)
       deallocate(U106)
       deallocate(X1)
       deallocate(S100)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,l3A,F1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(S34(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,F1,D2,S34)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef2134(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N1,N3,N1,N3,N0,N1,S34,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S35(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S35)
       deallocate(D1)
       deallocate(B2)
       deallocate(S34)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(U36(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,S35,U36)
       deallocate(D1)
C
       call
     & sul2413(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U36,-1.0d0/2)
       deallocate(U36)
       deallocate(S35)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,l3A,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef562341(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N0,N2,N2,N3,t3B,F2)
       allocate(S37(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K4*K2
       I3=K3*K3*K1*K1
       call jungemm(I1,I2,I3,F1,F2,S37)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U38(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,S37,U38)
       deallocate(D1)
C
       call
     & sul2413(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U38,1.0d0/4)
       deallocate(U38)
       deallocate(S37)
C
       allocate(F1(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,l3A,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,t3A,F2)
       allocate(Q9(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K3*K3*K1*K1*K1
       call jungemm(I1,I2,I3,F1,F2,Q9)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef2134(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N2,N3,N0,N2,N0,N1,IntM,D1)
       allocate(U39(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K4
       I2=K3
       I3=K3
       call jungemm(I1,I2,I3,D1,Q9,U39)
       deallocate(D1)
C
       call
     & sul2134(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U39,-1.0d0/12)
       deallocate(U39)
       deallocate(Q9)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,l3A,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,t3A,F2)
       allocate(Q10(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K3*K3*K1*K1
       call jungemm(I1,I2,I3,F1,F2,Q10)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N0,N2,IntM,D1)
       allocate(U40(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K4
       I2=K1
       I3=K1
       call jungemm(I1,I2,I3,D1,Q10,U40)
       deallocate(D1)
C
       call
     & sul4123(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U40,-1.0d0/12)
       deallocate(U40)
       deallocate(Q10)
C
       allocate(F1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef523614(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,N2,N3,N0,N2,l3B,F1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S45(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4*K1
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,F1,D2,S45)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef2134(N0,N1,N0,N1,N2,N3,N0,N2,
     & N0,N1,N0,N1,N2,N3,N0,N2,S45,D2)
       allocate(U46(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K2*K4
       I3=K1*K1
       call jungemm(I1,I2,I3,D1,D2,U46)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul1324(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U46,-1.0d0/2)
       deallocate(U46)
       deallocate(S45)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef562314(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N2,N3,N0,N2,l3B,F1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(S47(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K3
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,F1,D2,S47)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(U48(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K2*K4
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,S47,U48)
       deallocate(D1)
C
       call
     & sul1324(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U48,1.0d0/2)
       deallocate(U48)
       deallocate(S47)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef562314(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N2,N3,N0,N2,l3B,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N0,N1,N1,N3,t3A,F2)
       allocate(S49(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K3*K1
       I3=K3*K3*K1*K1
       call jungemm(I1,I2,I3,F1,F2,S49)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(U50(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,S49,U50)
       deallocate(D1)
C
       call
     & sul1324(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U50,1.0d0/4)
       deallocate(U50)
       deallocate(S49)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S52(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S52)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X2(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       X2=0.0d0
       X2=X2+S52
C
       call slx1324(N0,N3,N0,N1,N0,N1,N1,N3,N0,N1,X2,IntR,1.000d0)
C
       allocate(F1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef523614(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,N2,N3,N0,N2,l3B,F1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S51(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4*K1
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,F1,D2,S51)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D2(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef2134(N0,N1,N0,N1,N2,N3,N0,N2,
     & N0,N1,N0,N1,N2,N3,N0,N2,S51,D2)
       allocate(U53(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K2*K4
       I3=K1*K1
       call jungemm(I1,I2,I3,S52,D2,U53)
       deallocate(D2)
C
       call
     & sul1324(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U53,-1.0d0/2)
       deallocate(U53)
       deallocate(S51)
C
       allocate(F1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef413625(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,N2,N3,N0,N2,l3C,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(S127(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4*K1
       I2=K1
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S127)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D2(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef2134(N0,N1,N0,N1,N2,N3,N0,N2,
     & N0,N1,N0,N1,N2,N3,N0,N2,S127,D2)
       allocate(U133(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K2*K4
       I3=K1*K1
       call jungemm(I1,I2,I3,X2,D2,U133)
       deallocate(D2)
C
       call
     & sul1324(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U133,-1.0d0)
       deallocate(U133)
       deallocate(X2)
       deallocate(S127)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef562314(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N2,N3,N0,N2,l3B,F1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(S54(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K3
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,F1,D2,S54)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef2134(N1,N3,N1,N3,N2,N3,N0,N2,
     & N1,N3,N1,N3,N2,N3,N0,N2,S54,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S55(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K3
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S55)
       deallocate(D1)
       deallocate(B2)
       deallocate(S54)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(U56(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,S55,U56)
       deallocate(D1)
C
       call
     & sul1324(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U56,-1.0d0/2)
       deallocate(U56)
       deallocate(S55)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef562314(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N2,N3,N0,N2,l3B,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef562341(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N0,N2,N2,N3,t3B,F2)
       allocate(S57(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K4*K2
       I3=K3*K3*K1*K1
       call jungemm(I1,I2,I3,F1,F2,S57)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(U58(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,S57,U58)
       deallocate(D1)
C
       call
     & sul1324(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U58,1.0d0/4)
       deallocate(U58)
       deallocate(S57)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3,N1+1:N3))
       call relef562413(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,N2,N2,N3,N1,N3,l3B,F1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(S59(N1+1:N3,N0+1:N2,N2+1:N3,N1+1:N3))
       I1=K3*K4*K2
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,F1,D2,S59)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N0,N2,N0,N1,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S67(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K3
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S67)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1))
       X3=0.0d0
       X3=X3+S67
C
       allocate(X4(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1))
       X4=0.0d0
       X4=X4+S67
       deallocate(S67)
C
       call slx1234(N0,N3,N0,N2,N1,N3,N0,N2,N0,N1,X3,IntM,1.000d0)
C
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:N3))
       call relef2134(N1,N3,N0,N2,N2,N3,N1,N3,
     & N0,N2,N1,N3,N2,N3,N1,N3,S59,D2)
       allocate(U60(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K3*K4
       I3=K3*K2
       call jungemm(I1,I2,I3,X3,D2,U60)
       deallocate(D2)
C
       V2B=V2B+1.0d0/2*U60
       deallocate(U60)
       deallocate(S59)
       deallocate(X3)
C
       call slx1234(N0,N3,N0,N2,N1,N3,N0,N2,N0,N1,X4,IntM,1.000d0)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N2,N2+1:N3,N1+1:N3))
       call relef461523(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,N2,N2,N3,N1,N3,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2B,D2)
       allocate(S146(N1+1:N3,N0+1:N2,N2+1:N3,N1+1:N3))
       I1=K3*K4*K2
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,F1,D2,S146)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:N3))
       call relef2134(N1,N3,N0,N2,N2,N3,N1,N3,
     & N0,N2,N1,N3,N2,N3,N1,N3,S146,D2)
       allocate(U154(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K3*K4
       I3=K3*K2
       call jungemm(I1,I2,I3,X4,D2,U154)
       deallocate(D2)
C
       V2B=V2B+U154
       deallocate(U154)
       deallocate(X4)
       deallocate(S146)
C
       allocate(F1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef452613(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N1,N3,N0,N1,N2,N3,N1,N3,l3B,F1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,t2B,D2)
       allocate(S61(N2+1:N3,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4*K1
       I2=K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,F1,D2,S61)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef2134(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N2,N3,N0,N2,N0,N1,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S65(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K4
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S65)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X5(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       X5=0.0d0
       X5=X5+S65
C
       call slx2134(N0,N3,N0,N1,N2,N3,N0,N2,N0,N1,X5,IntM,1.000d0)
C
       allocate(D2(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef2134(N2,N3,N0,N1,N2,N3,N1,N3,
     & N0,N1,N2,N3,N2,N3,N1,N3,S61,D2)
       allocate(U62(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K3*K4
       I3=K4*K1
       call jungemm(I1,I2,I3,X5,D2,U62)
       deallocate(D2)
C
       V2B=V2B+U62
       deallocate(U62)
       deallocate(S61)
       deallocate(X5)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef451623(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N0,N1,N2,N3,N1,N3,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(S152(N2+1:N3,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4*K1
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,F1,D2,S152)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D2(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef2134(N2,N3,N0,N1,N2,N3,N1,N3,
     & N0,N1,N2,N3,N2,N3,N1,N3,S152,D2)
       allocate(U153(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K3*K4
       I3=K4*K1
       call jungemm(I1,I2,I3,S65,D2,U153)
       deallocate(D2)
C
       V2B=V2B+1.0d0/2*U153
       deallocate(U153)
       deallocate(S152)
C
       allocate(F1(N0+1:N2,N0+1:N1,N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3))
       call relef456213(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N1,N3,N2,N3,N1,N3,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3))
       call relef456213(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N1,N3,N2,N3,N1,N3,t3B,F2)
       allocate(S63(N2+1:N3,N1+1:N3,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K3*K4
       I3=K3*K1*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S63)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N0,N2,N0,N1,IntM,D1)
       allocate(U64(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K3*K4
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,S63,U64)
       deallocate(D1)
C
       V2B=V2B+1.0d0/2*U64
       deallocate(U64)
       deallocate(S63)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef562134(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N2,N3,N1,N3,N0,N2,l3B,F1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(S69(N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K4
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,F1,D2,S69)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N1))
       call relef3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N2,N3,N0,N1,IntM,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef2134(N1,N3,N2,N3,N1,N3,N0,N2,
     & N2,N3,N1,N3,N1,N3,N0,N2,S69,D2)
       allocate(U70(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K2*K3
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,D2,U70)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul2314(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U70,1.0d0/2)
       deallocate(U70)
       deallocate(S69)
C
       allocate(F1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef512634(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N1,N1,N3,N0,N2,l3B,F1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(S71(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3*K1
       I2=K2
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,F1,D2,S71)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       call relef2314(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N2,N2,N3,N0,N1,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S75(N0+1:N1,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S75)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X6(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       X6=0.0d0
       call sul2134(N0,N2,N0,N1,N2,N3,N0,N1,X6,S75,1.000d0)
C
       call slx3214(N0,N3,N0,N2,N0,N1,N2,N3,N0,N1,X6,IntM,1.000d0)
C
       allocate(U72(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K2*K3
       I3=K1*K2
       call jungemm(I1,I2,I3,X6,S71,U72)
C
       call
     & sul2314(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U72,1.0d0)
       deallocate(U72)
       deallocate(S71)
       deallocate(X6)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef412635(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N0,N1,N1,N3,N0,N2,l3C,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S161(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3*K1
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S161)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef2134(N0,N1,N0,N2,N2,N3,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,N1,S75,D1)
       allocate(U162(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K2*K3
       I3=K1*K2
       call jungemm(I1,I2,I3,D1,S161,U162)
       deallocate(D1)
C
       call
     & sul2314(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U162,1.0d0/2)
       deallocate(U162)
       deallocate(S161)
C
       allocate(F1(N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef561234(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,l3B,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3))
       call relef561243(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N2,N3,N1,N3,N0,N2,N1,N3,t3B,F2)
       allocate(S73(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3
       I2=K3*K2
       I3=K3*K4*K1*K1
       call jungemm(I1,I2,I3,F1,F2,S73)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
       call relef3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N2,N3,N0,N1,IntM,D1)
       allocate(U74(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K2*K3
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,S73,U74)
       deallocate(D1)
C
       call
     & sul2314(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U74,1.0d0/2)
       deallocate(U74)
       deallocate(S73)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef562134(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N2,N3,N1,N3,N0,N2,l3B,F1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(S77(N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K4
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,F1,D2,S77)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef2134(N1,N3,N2,N3,N1,N3,N0,N2,
     & N2,N3,N1,N3,N1,N3,N0,N2,S77,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S78(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K3
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S78)
       deallocate(D1)
       deallocate(B2)
       deallocate(S77)
C
       allocate(D1(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
       call relef3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N2,N3,N0,N1,IntM,D1)
       allocate(U79(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K2*K3
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,S78,U79)
       deallocate(D1)
C
       call
     & sul2314(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U79,1.0d0/2)
       deallocate(U79)
       deallocate(S78)
C
       allocate(F1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       call relef523416(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N2,N2,N3,N0,N1,l3B,F1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S80(N0+1:N1,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4*K2
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,F1,D2,S80)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef1423(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N1,N1,N3,N0,N2,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S88(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S88)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X7(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       X7=0.0d0
       X7=X7+S88
C
       allocate(X8(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       X8=0.0d0
       X8=X8+S88
       deallocate(S88)
C
       call slx1342(N0,N3,N0,N2,N0,N1,N1,N3,N0,N2,X7,IntM,1.000d0)
C
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef2134(N0,N1,N0,N2,N2,N3,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,N1,S80,D2)
       allocate(U81(N2+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3
       I2=K1*K4
       I3=K1*K2
       call jungemm(I1,I2,I3,X7,D2,U81)
       deallocate(D2)
C
       call
     & sul1423(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U81,1.0d0/2)
       deallocate(U81)
       deallocate(S80)
       deallocate(X7)
C
       call slx1342(N0,N3,N0,N2,N0,N1,N1,N3,N0,N2,X8,IntM,1.000d0)
C
       allocate(F1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       call relef413526(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N2,N2,N3,N0,N1,l3C,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(S165(N0+1:N1,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4*K2
       I2=K1
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S165)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef2134(N0,N1,N0,N2,N2,N3,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,N1,S165,D2)
       allocate(U174(N2+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3
       I2=K1*K4
       I3=K1*K2
       call jungemm(I1,I2,I3,X8,D2,U174)
       deallocate(D2)
C
       call
     & sul1423(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U174,1.0d0)
       deallocate(U174)
       deallocate(X8)
       deallocate(S165)
C
       allocate(F1(N0+1:N2,N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N1))
       call relef452316(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N1,N3,N1,N3,N2,N3,N0,N1,l3B,F1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,t2B,D2)
       allocate(S82(N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4*K3
       I2=K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,F1,D2,S82)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef1423(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N1,N3,N0,N2,IntM,D1)
       allocate(U83(N2+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3
       I2=K1*K4
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,S82,U83)
       deallocate(D1)
C
       call
     & sul1423(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U83,-1.0d0)
       deallocate(U83)
C
       allocate(D1(N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N1))
       call relef2134(N2,N3,N1,N3,N2,N3,N0,N1,
     & N1,N3,N2,N3,N2,N3,N0,N1,S82,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S86(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4*K4
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S86)
       deallocate(D1)
       deallocate(B2)
       deallocate(S82)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N0,N2,IntM,D1)
       allocate(U87(N2+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3
       I2=K1*K4
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,S86,U87)
       deallocate(D1)
C
       call
     & sul1423(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U87,1.0d0)
       deallocate(U87)
       deallocate(S86)
C
       allocate(F1(N0+1:N2,N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N1))
       call relef452316(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N1,N3,N1,N3,N2,N3,N0,N1,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N2+1:N3))
       call relef452361(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N1,N3,N1,N3,N0,N1,N2,N3,t3B,F2)
       allocate(S84(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K4*K1
       I3=K3*K3*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S84)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N0,N2,IntM,D1)
       allocate(U85(N2+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3
       I2=K1*K4
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,S84,U85)
       deallocate(D1)
C
       call
     & sul1423(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U85,1.0d0/2)
       deallocate(U85)
       deallocate(S84)
C
       allocate(F1(N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef523146(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N2,N3,N0,N2,N0,N1,l3B,F1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S90(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K4
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,F1,D2,S90)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N2,N3,N1,N3,IntM,D1)
       allocate(U91(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K1*K2
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,S90,U91)
       deallocate(D1)
C
       call
     & sul3412(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U91,-1.0d0/2)
       deallocate(U91)
C
       allocate(D1(N2+1:N3,N0+1:N1,N0+1:N2,N0+1:N1))
       call relef2134(N0,N1,N2,N3,N0,N2,N0,N1,
     & N2,N3,N0,N1,N0,N2,N0,N1,S90,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S98(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       I1=K1*K2*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S98)
       deallocate(D1)
       deallocate(B2)
       deallocate(S90)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(U99(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K1*K2
       I3=K1*K2
       call jungemm(I1,I2,I3,D1,S98,U99)
       deallocate(D1)
C
       call
     & sul3412(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U99,1.0d0/2)
       deallocate(U99)
       deallocate(S98)
C
       allocate(F1(N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef512346(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,l3B,F1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(S92(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K3
       I2=K2
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,F1,D2,S92)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N2,N3,N1,N3,IntM,D1)
       allocate(U93(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K1*K2
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,S92,U93)
       deallocate(D1)
C
       call
     & sul3412(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U93,-1.0d0)
       deallocate(U93)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       call relef2134(N0,N2,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,N2,N0,N2,N0,N1,S92,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S96(N0+1:N1,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S96)
       deallocate(D1)
       deallocate(B2)
       deallocate(S92)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       call relef2134(N0,N1,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N0,N2,N0,N1,S96,D2)
       allocate(U97(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K1*K2
       I3=K1*K2
       call jungemm(I1,I2,I3,D1,D2,U97)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul3412(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U97,1.0d0)
       deallocate(U97)
       deallocate(S96)
C
       allocate(F1(N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef512346(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,l3B,F1)
       allocate(F2(N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef512346(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,t3B,F2)
       allocate(S94(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K1*K2
       I3=K3*K3*K4*K1
       call jungemm(I1,I2,I3,F1,F2,S94)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(U95(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K1*K2
       I3=K1*K2
       call jungemm(I1,I2,I3,D1,S94,U95)
       deallocate(D1)
C
       call
     & sul3412(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U95,1.0d0/2)
       deallocate(U95)
       deallocate(S94)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,l3B,F1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2B,D2)
       allocate(S102(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,F1,D2,S102)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(U103(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K3
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,S102,U103)
       deallocate(D1)
C
       call
     & sul2413(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U103,1.0d0)
       deallocate(U103)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef2134(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N1,N3,N1,N3,N0,N1,S102,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S107(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S107)
       deallocate(D1)
       deallocate(B2)
       deallocate(S102)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(U108(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,S107,U108)
       deallocate(D1)
C
       call
     & sul2413(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U108,-1.0d0)
       deallocate(U108)
       deallocate(S107)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N0,N1,N1,N3,t3B,F2)
       allocate(S104(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K3*K1
       I3=K3*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S104)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(U105(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,S104,U105)
       deallocate(D1)
C
       call
     & sul2413(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U105,1.0d0)
       deallocate(U105)
       deallocate(S104)
C
       allocate(F1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef512436(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,N1,N3,N0,N1,l3B,F1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(S109(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,F1,D2,S109)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S115(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S115)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X9(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       X9=0.0d0
       call sul2134(N0,N2,N0,N2,N2,N3,N0,N2,X9,S115,1.000d0)
C
       call slx2314(N0,N3,N0,N2,N0,N2,N2,N3,N0,N2,X9,IntB,1.000d0)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef2134(N0,N2,N0,N2,N2,N3,N0,N2,
     & N0,N2,N0,N2,N2,N3,N0,N2,X9,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef2134(N0,N2,N0,N2,N1,N3,N0,N1,
     & N0,N2,N0,N2,N1,N3,N0,N1,S109,D2)
       allocate(U110(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K3
       I3=K2*K2
       call jungemm(I1,I2,I3,D1,D2,U110)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul2413(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U110,-1.0d0)
       deallocate(U110)
       deallocate(S109)
       deallocate(X9)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef412536(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N0,N2,N1,N3,N0,N1,l3C,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S194(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S194)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D2(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef2134(N0,N2,N0,N2,N1,N3,N0,N1,
     & N0,N2,N0,N2,N1,N3,N0,N1,S194,D2)
       allocate(U195(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K3
       I3=K2*K2
       call jungemm(I1,I2,I3,S115,D2,U195)
       deallocate(D2)
C
       call
     & sul2413(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U195,-1.0d0/2)
       deallocate(U195)
       deallocate(S194)
C
       allocate(F1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef452136(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,N1,N3,N0,N1,l3B,F1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,t2B,D2)
       allocate(S111(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,F1,D2,S111)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef2134(N2,N3,N2,N3,N1,N3,N0,N1,
     & N2,N3,N2,N3,N1,N3,N0,N1,S111,D2)
       allocate(U112(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K3
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,D2,U112)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul2413(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U112,1.0d0)
       deallocate(U112)
C
       allocate(D1(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef2134(N2,N3,N2,N3,N1,N3,N0,N1,
     & N2,N3,N2,N3,N1,N3,N0,N1,S111,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S117(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S117)
       deallocate(D1)
       deallocate(B2)
       deallocate(S111)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U118(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,S117,U118)
       deallocate(D1)
C
       call
     & sul2413(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U118,-1.0d0)
       deallocate(U118)
       deallocate(S117)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef461352(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N0,N2,N2,N3,t3C,F2)
       allocate(S113(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K4*K2
       I3=K3*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S113)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U114(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,S113,U114)
       deallocate(D1)
C
       call
     & sul2413(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U114,1.0d0)
       deallocate(U114)
       deallocate(S113)
C
       allocate(F1(N0+1:N2,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3))
       call relef456231(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N1,N3,N1,N3,N2,N3,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3))
       call relef456231(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N1,N3,N1,N3,N2,N3,t3B,F2)
       allocate(Q11(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K3*K3*K1*K1*K2
       call jungemm(I1,I2,I3,F1,F2,Q11)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N0,N2,N0,N1,IntM,D1)
       allocate(U119(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K3
       I2=K4
       I3=K4
       call jungemm(I1,I2,I3,D1,Q11,U119)
       deallocate(D1)
C
       V2B=V2B-1.0d0/4*U119
       deallocate(U119)
       deallocate(Q11)
C
       allocate(F1(N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef561234(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,l3B,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef561234(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,t3B,F2)
       allocate(Q12(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K3*K3*K4*K1*K1
       call jungemm(I1,I2,I3,F1,F2,Q12)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(U120(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K2
       I3=K2
       call jungemm(I1,I2,I3,D1,Q12,U120)
       deallocate(D1)
C
       call
     & sul3124(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U120,-1.0d0/4)
       deallocate(U120)
       deallocate(Q12)
C
       allocate(F1(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,t3B,F2)
       allocate(Q13(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K3*K4*K1*K1*K2
       call jungemm(I1,I2,I3,F1,F2,Q13)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef2134(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N2,N3,N0,N2,N0,N1,IntM,D1)
       allocate(U121(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K4
       I2=K3
       I3=K3
       call jungemm(I1,I2,I3,D1,Q13,U121)
       deallocate(D1)
C
       call
     & sul2134(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U121,-1.0d0/2)
       deallocate(U121)
       deallocate(Q13)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,t3B,F2)
       allocate(Q14(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K3*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,Q14)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N0,N2,IntM,D1)
       allocate(U122(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K4
       I2=K1
       I3=K1
       call jungemm(I1,I2,I3,D1,Q14,U122)
       deallocate(D1)
C
       call
     & sul4123(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U122,-1.0d0/2)
       deallocate(U122)
       deallocate(Q14)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef461325(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N2,N3,N0,N2,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2B,D2)
       allocate(S129(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K3
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,F1,D2,S129)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(U130(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K2*K4
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,S129,U130)
       deallocate(D1)
C
       call
     & sul1324(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U130,1.0d0)
       deallocate(U130)
C
       allocate(D1(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef2134(N1,N3,N1,N3,N2,N3,N0,N2,
     & N1,N3,N1,N3,N2,N3,N0,N2,S129,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S134(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K3
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S134)
       deallocate(D1)
       deallocate(B2)
       deallocate(S129)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(U135(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,S134,U135)
       deallocate(D1)
C
       call
     & sul1324(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U135,-1.0d0)
       deallocate(U135)
       deallocate(S134)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef461325(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N2,N3,N0,N2,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N0,N1,N1,N3,t3B,F2)
       allocate(S131(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K3*K1
       I3=K3*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S131)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(U132(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,S131,U132)
       deallocate(D1)
C
       call
     & sul1324(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U132,1.0d0)
       deallocate(U132)
       deallocate(S131)
C
       allocate(F1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef613425(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,N2,N3,N0,N2,l3C,F1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(S136(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,F1,D2,S136)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N1,N3,N0,N1,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S142(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S142)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X10(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       X10=0.0d0
       call sul2134(N0,N2,N0,N2,N1,N3,N0,N1,X10,S142,1.000d0)
C
       call slx2314(N0,N3,N0,N2,N0,N2,N1,N3,N0,N1,X10,IntM,1.000d0)
C
       allocate(D1(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef2134(N0,N2,N0,N2,N1,N3,N0,N1,
     & N0,N2,N0,N2,N1,N3,N0,N1,X10,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef2134(N0,N2,N0,N2,N2,N3,N0,N2,
     & N0,N2,N0,N2,N2,N3,N0,N2,S136,D2)
       allocate(U137(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K2*K4
       I3=K2*K2
       call jungemm(I1,I2,I3,D1,D2,U137)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul1324(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U137,-1.0d0)
       deallocate(U137)
       deallocate(S136)
       deallocate(X10)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef412536(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,N2,N3,N0,N2,l3D,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S211(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S211)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef2134(N0,N2,N0,N2,N2,N3,N0,N2,
     & N0,N2,N0,N2,N2,N3,N0,N2,S211,D2)
       allocate(U212(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K2*K4
       I3=K2*K2
       call jungemm(I1,I2,I3,S142,D2,U212)
       deallocate(D2)
C
       call
     & sul1324(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U212,-1.0d0/2)
       deallocate(U212)
       deallocate(S211)
C
       allocate(F1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef463125(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,N2,N3,N0,N2,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,t2B,D2)
       allocate(S138(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,F1,D2,S138)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef2134(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N2,N3,N2,N3,N0,N2,S138,D2)
       allocate(U139(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K2*K4
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,D2,U139)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul1324(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U139,1.0d0)
       deallocate(U139)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef2134(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N2,N3,N2,N3,N0,N2,S138,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S144(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S144)
       deallocate(D1)
       deallocate(B2)
       deallocate(S138)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(U145(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,S144,U145)
       deallocate(D1)
C
       call
     & sul1324(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U145,-1.0d0)
       deallocate(U145)
       deallocate(S144)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef461325(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N2,N3,N0,N2,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef461352(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N0,N2,N2,N3,t3C,F2)
       allocate(S140(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K4*K2
       I3=K3*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S140)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(U141(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,S140,U141)
       deallocate(D1)
C
       call
     & sul1324(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U141,1.0d0)
       deallocate(U141)
       deallocate(S140)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef451623(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N0,N1,N2,N3,N1,N3,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(S148(N2+1:N3,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4*K1
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,F1,D2,S148)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef2134(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N0,N2,N0,N1,IntM,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef2134(N2,N3,N0,N1,N2,N3,N1,N3,
     & N0,N1,N2,N3,N2,N3,N1,N3,S148,D2)
       allocate(U149(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K3*K4
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,D2,U149)
       deallocate(D1)
       deallocate(D2)
C
       V2B=V2B+1.0d0/2*U149
       deallocate(U149)
       deallocate(S148)
C
       allocate(F1(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef456123(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef456123(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,t3C,F2)
       allocate(S150(N2+1:N3,N1+1:N3,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K3*K4
       I3=K4*K1*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S150)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N0,N2,N0,N1,IntM,D1)
       allocate(U151(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K3*K4
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,S150,U151)
       deallocate(D1)
C
       V2B=V2B+1.0d0/2*U151
       deallocate(U151)
       deallocate(S150)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef461235(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2B,D2)
       allocate(S155(N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K4
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,F1,D2,S155)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N1))
       call relef3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N2,N3,N0,N1,IntM,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef2134(N1,N3,N2,N3,N1,N3,N0,N2,
     & N2,N3,N1,N3,N1,N3,N0,N2,S155,D2)
       allocate(U156(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K2*K3
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,D2,U156)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul2314(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U156,-1.0d0)
       deallocate(U156)
C
       allocate(D1(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef2134(N1,N3,N2,N3,N1,N3,N0,N2,
     & N2,N3,N1,N3,N1,N3,N0,N2,S155,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S163(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K3
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S163)
       deallocate(D1)
       deallocate(B2)
       deallocate(S155)
C
       allocate(D1(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
       call relef3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N2,N3,N0,N1,IntM,D1)
       allocate(U164(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K2*K3
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,S163,U164)
       deallocate(D1)
C
       call
     & sul2314(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U164,1.0d0)
       deallocate(U164)
       deallocate(S163)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef412635(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N0,N1,N1,N3,N0,N2,l3C,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S157(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3*K1
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S157)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N0,N1,IntM,D1)
       allocate(U158(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K2*K3
       I3=K1*K2
       call jungemm(I1,I2,I3,D1,S157,U158)
       deallocate(D1)
C
       call
     & sul2314(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U158,1.0d0/2)
       deallocate(U158)
       deallocate(S157)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef461235(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N2,N1+1:N3))
       call relef461253(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N0,N2,N1,N3,t3C,F2)
       allocate(S159(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3
       I2=K3*K2
       I3=K4*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S159)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
       call relef3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N2,N3,N0,N1,IntM,D1)
       allocate(U160(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K2*K3
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,S159,U160)
       deallocate(D1)
C
       call
     & sul2314(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U160,1.0d0/2)
       deallocate(U160)
       deallocate(S159)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N1))
       call relef451326(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N1,N3,N2,N3,N0,N1,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(S167(N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4*K3
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,F1,D2,S167)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef1423(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N1,N3,N0,N2,IntM,D1)
       allocate(U168(N2+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3
       I2=K1*K4
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,S167,U168)
       deallocate(D1)
C
       call
     & sul1423(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U168,-1.0d0/2)
       deallocate(U168)
       deallocate(S167)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N1))
       call relef451326(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N1,N3,N2,N3,N0,N1,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1,N2+1:N3))
       call relef451362(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N1,N3,N0,N1,N2,N3,t3C,F2)
       allocate(S169(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K4*K1
       I3=K3*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S169)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N0,N2,IntM,D1)
       allocate(U170(N2+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3
       I2=K1*K4
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,S169,U170)
       deallocate(D1)
C
       call
     & sul1423(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U170,1.0d0/2)
       deallocate(U170)
       deallocate(S169)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N1))
       call relef451326(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N1,N3,N2,N3,N0,N1,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(S171(N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4*K3
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,F1,D2,S171)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N1))
       call relef2134(N2,N3,N1,N3,N2,N3,N0,N1,
     & N1,N3,N2,N3,N2,N3,N0,N1,S171,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S172(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4*K4
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S172)
       deallocate(D1)
       deallocate(B2)
       deallocate(S171)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N0,N2,IntM,D1)
       allocate(U173(N2+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3
       I2=K1*K4
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,S172,U173)
       deallocate(D1)
C
       call
     & sul1423(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U173,1.0d0/2)
       deallocate(U173)
       deallocate(S172)
C
       allocate(F1(N0+1:N2,N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef413256(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N2,N3,N0,N2,N0,N1,l3C,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(S175(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K4
       I2=K1
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S175)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N2,N3,N1,N3,IntM,D1)
       allocate(U176(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K1*K2
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,S175,U176)
       deallocate(D1)
C
       call
     & sul3412(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U176,-1.0d0)
       deallocate(U176)
C
       allocate(D1(N2+1:N3,N0+1:N1,N0+1:N2,N0+1:N1))
       call relef2134(N0,N1,N2,N3,N0,N2,N0,N1,
     & N2,N3,N0,N1,N0,N2,N0,N1,S175,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S184(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       I1=K1*K2*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S184)
       deallocate(D1)
       deallocate(B2)
       deallocate(S175)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(U185(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K1*K2
       I3=K1*K2
       call jungemm(I1,I2,I3,D1,S184,U185)
       deallocate(D1)
C
       call
     & sul3412(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U185,1.0d0)
       deallocate(U185)
       deallocate(S184)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef412356(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N1,N3,N0,N2,N0,N1,l3C,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S177(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K3
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S177)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N2,N3,N1,N3,IntM,D1)
       allocate(U178(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K1*K2
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,S177,U178)
       deallocate(D1)
C
       call
     & sul3412(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U178,-1.0d0/2)
       deallocate(U178)
       deallocate(S177)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef412356(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N1,N3,N0,N2,N0,N1,l3C,F1)
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef412356(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N1,N3,N0,N2,N0,N1,t3C,F2)
       allocate(S179(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K1*K2
       I3=K3*K4*K4*K2
       call jungemm(I1,I2,I3,F1,F2,S179)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(U180(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K1*K2
       I3=K1*K2
       call jungemm(I1,I2,I3,D1,S179,U180)
       deallocate(D1)
C
       call
     & sul3412(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U180,1.0d0/2)
       deallocate(U180)
       deallocate(S179)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef412356(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N1,N3,N0,N2,N0,N1,l3C,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S181(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K3
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S181)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       call relef2134(N0,N2,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,N2,N0,N2,N0,N1,S181,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S182(N0+1:N1,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S182)
       deallocate(D1)
       deallocate(B2)
       deallocate(S181)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       call relef2134(N0,N1,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N0,N2,N0,N1,S182,D2)
       allocate(U183(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K1*K2
       I3=K1*K2
       call jungemm(I1,I2,I3,D1,D2,U183)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul3412(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U183,1.0d0/2)
       deallocate(U183)
       deallocate(S182)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N0,N1,N1,N3,t3C,F2)
       allocate(S186(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K3*K1
       I3=K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S186)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(U187(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,S186,U187)
       deallocate(D1)
C
       call
     & sul2413(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U187,1.0d0/4)
       deallocate(U187)
       deallocate(S186)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef412536(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N0,N2,N1,N3,N0,N1,l3C,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S188(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S188)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef2134(N0,N2,N0,N2,N1,N3,N0,N1,
     & N0,N2,N0,N2,N1,N3,N0,N1,S188,D2)
       allocate(U189(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K3
       I3=K2*K2
       call jungemm(I1,I2,I3,D1,D2,U189)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul2413(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U189,-1.0d0/2)
       deallocate(U189)
       deallocate(S188)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(S190(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,F1,D2,S190)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U191(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K3
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,S190,U191)
       deallocate(D1)
C
       call
     & sul2413(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U191,1.0d0/2)
       deallocate(U191)
       deallocate(S190)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef451263(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N0,N2,N2,N3,t3D,F2)
       allocate(S192(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K4*K2
       I3=K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S192)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U193(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,S192,U193)
       deallocate(D1)
C
       call
     & sul2413(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U193,1.0d0/4)
       deallocate(U193)
       deallocate(S192)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(S196(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,F1,D2,S196)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef2134(N2,N3,N2,N3,N1,N3,N0,N1,
     & N2,N3,N2,N3,N1,N3,N0,N1,S196,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S197(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S197)
       deallocate(D1)
       deallocate(B2)
       deallocate(S196)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U198(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,S197,U198)
       deallocate(D1)
C
       call
     & sul2413(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U198,-1.0d0/2)
       deallocate(U198)
       deallocate(S197)
C
       allocate(F1(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef456132(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N1,N3,N2,N3,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef456132(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N1,N3,N2,N3,t3C,F2)
       allocate(Q15(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K3*K4*K1*K2*K2
       call jungemm(I1,I2,I3,F1,F2,Q15)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N0,N2,N0,N1,IntM,D1)
       allocate(U199(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K3
       I2=K4
       I3=K4
       call jungemm(I1,I2,I3,D1,Q15,U199)
       deallocate(D1)
C
       V2B=V2B-1.0d0/2*U199
       deallocate(U199)
       deallocate(Q15)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef461235(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef461235(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,t3C,F2)
       allocate(Q16(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K3*K4*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,Q16)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(U200(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K2
       I3=K2
       call jungemm(I1,I2,I3,D1,Q16,U200)
       deallocate(D1)
C
       call
     & sul3124(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U200,-1.0d0/2)
       deallocate(U200)
       deallocate(Q16)
C
       allocate(F1(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef456123(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef456123(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,t3C,F2)
       allocate(Q17(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K4*K4*K1*K2*K2
       call jungemm(I1,I2,I3,F1,F2,Q17)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef2134(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N2,N3,N0,N2,N0,N1,IntM,D1)
       allocate(U201(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K4
       I2=K3
       I3=K3
       call jungemm(I1,I2,I3,D1,Q17,U201)
       deallocate(D1)
C
       call
     & sul2134(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U201,-1.0d0/4)
       deallocate(U201)
       deallocate(Q17)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,t3C,F2)
       allocate(Q18(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,Q18)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N0,N2,IntM,D1)
       allocate(U202(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K4
       I2=K1
       I3=K1
       call jungemm(I1,I2,I3,D1,Q18,U202)
       deallocate(D1)
C
       call
     & sul4123(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U202,-1.0d0/4)
       deallocate(U202)
       deallocate(Q18)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,l3D,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N0,N1,N1,N3,t3C,F2)
       allocate(S203(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K3*K1
       I3=K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S203)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(U204(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,S203,U204)
       deallocate(D1)
C
       call
     & sul1324(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U204,1.0d0/4)
       deallocate(U204)
       deallocate(S203)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef412536(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,N2,N3,N0,N2,l3D,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S205(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S205)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N1,N3,N0,N1,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef2134(N0,N2,N0,N2,N2,N3,N0,N2,
     & N0,N2,N0,N2,N2,N3,N0,N2,S205,D2)
       allocate(U206(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K2*K4
       I3=K2*K2
       call jungemm(I1,I2,I3,D1,D2,U206)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul1324(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U206,-1.0d0/2)
       deallocate(U206)
       deallocate(S205)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,l3D,F1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(S207(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,F1,D2,S207)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(U208(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K2*K4
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,S207,U208)
       deallocate(D1)
C
       call
     & sul1324(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U208,1.0d0/2)
       deallocate(U208)
       deallocate(S207)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,l3D,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef451263(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N0,N2,N2,N3,t3D,F2)
       allocate(S209(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K4*K2
       I3=K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S209)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(U210(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,S209,U210)
       deallocate(D1)
C
       call
     & sul1324(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U210,1.0d0/4)
       deallocate(U210)
       deallocate(S209)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,l3D,F1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(S213(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,F1,D2,S213)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef2134(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N2,N3,N2,N3,N0,N2,S213,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S214(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S214)
       deallocate(D1)
       deallocate(B2)
       deallocate(S213)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(U215(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,S214,U215)
       deallocate(D1)
C
       call
     & sul1324(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U215,-1.0d0/2)
       deallocate(U215)
       deallocate(S214)
C
       allocate(F1(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef456123(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,l3D,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef456123(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,t3D,F2)
       allocate(Q19(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K4*K4*K2*K2*K2
       call jungemm(I1,I2,I3,F1,F2,Q19)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N0,N2,N0,N1,IntM,D1)
       allocate(U216(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K3
       I2=K4
       I3=K4
       call jungemm(I1,I2,I3,D1,Q19,U216)
       deallocate(D1)
C
       V2B=V2B-1.0d0/12*U216
       deallocate(U216)
       deallocate(Q19)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,l3D,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,t3D,F2)
       allocate(Q20(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4*K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,Q20)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(U217(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K2
       I3=K2
       call jungemm(I1,I2,I3,D1,Q20,U217)
       deallocate(D1)
C
       call
     & sul3124(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U217,-1.0d0/12)
       deallocate(U217)
       deallocate(Q20)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef2134(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N0,N2,N0,N1,H2B,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,l1A,B2)
       allocate(U1(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K4
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,U1)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sul2134(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U1,-1.0d0)
       deallocate(U1)
C
       allocate(D1(N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N2,N3,N1,N3,N0,N2,H2B,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,l1A,B2)
       allocate(U2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K4
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,U2)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sul4123(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U2,1.0d0)
       deallocate(U2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N0,N2,N0,N1,H2B,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,l1B,B2)
       allocate(U3(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K3
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,U3)
       deallocate(D1)
       deallocate(B2)
C
       V2B=V2B-U3
       deallocate(U3)
C
       allocate(D1(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N1,N3,N0,N1,H2B,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,l1B,B2)
       allocate(U4(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,U4)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sul3124(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U4,1.0d0)
       deallocate(U4)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,H2B,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,l2A,D2)
       allocate(U5(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,U5)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul2413(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U5,1.0d0)
       deallocate(U5)
C
       allocate(B1(N0+1:N1,N0+1:N1))
       call relef12(N0,N3,N0,N3,
     & N0,N1,N0,N1,H1A,B1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,l2B,D2)
       allocate(U8(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1
       I2=K2*K3*K4
       I3=K1
       call jungemm(I1,I2,I3,B1,D2,U8)
       deallocate(B1)
       deallocate(D2)
C
       V2B=V2B-U8
       deallocate(U8)
C
       allocate(B1(N1+1:N3,N1+1:N3))
       call relef21(N0,N3,N0,N3,
     & N1,N3,N1,N3,H1A,B1)
       allocate(D2(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,N3,N0,N2,N0,N1,l2B,D2)
       allocate(U9(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1*K2*K4
       I3=K3
       call jungemm(I1,I2,I3,B1,D2,U9)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sul1342(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U9,1.0d0)
       deallocate(U9)
C
       allocate(B1(N0+1:N2,N0+1:N2))
       call relef12(N0,N3,N0,N3,
     & N0,N2,N0,N2,H1B,B1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,l2B,D2)
       allocate(U10(N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N2))
       I1=K2
       I2=K1*K3*K4
       I3=K2
       call jungemm(I1,I2,I3,B1,D2,U10)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sul1243(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U10,-1.0d0)
       deallocate(U10)
C
       allocate(B1(N2+1:N3,N2+1:N3))
       call relef21(N0,N3,N0,N3,
     & N2,N3,N2,N3,H1B,B1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,l2B,D2)
       allocate(U11(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4
       I2=K1*K2*K3
       I3=K4
       call jungemm(I1,I2,I3,B1,D2,U11)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sul2341(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U11,1.0d0)
       deallocate(U11)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,H2A,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,l2B,D2)
       allocate(U12(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,U12)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul1324(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U12,1.0d0)
       deallocate(U12)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N0,N2,N0,N1,H2B,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,l2B,D2)
       allocate(U13(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K3*K4
       I3=K1*K2
       call jungemm(I1,I2,I3,D1,D2,U13)
       deallocate(D1)
       deallocate(D2)
C
       V2B=V2B+U13
       deallocate(U13)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       call relef2314(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N2,N3,N0,N1,H2B,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,l2B,D2)
       allocate(U14(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K2*K3
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,D2,U14)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul2314(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U14,-1.0d0)
       deallocate(U14)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef1423(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N1,N3,N0,N2,H2B,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
       call relef3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,N3,N0,N1,l2B,D2)
       allocate(U15(N2+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3
       I2=K1*K4
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,D2,U15)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul1423(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U15,-1.0d0)
       deallocate(U15)
C
       allocate(D1(N2+1:N3,N1+1:N3,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N2,N3,N1,N3,H2B,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,l2B,D2)
       allocate(U16(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K1*K2
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,D2,U16)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul3412(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U16,1.0d0)
       deallocate(U16)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,H2C,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,l2B,D2)
       allocate(U17(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,U17)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul2413(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U17,1.0d0)
       deallocate(U17)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,H2B,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,l2C,D2)
       allocate(U22(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,U22)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul1324(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U22,1.0d0)
       deallocate(U22)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,H2A,D1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef562134(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N2,N3,N1,N3,N0,N2,l3B,F2)
       allocate(U41(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1
       I2=K2*K3*K4
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,F2,U41)
       deallocate(D1)
       deallocate(F2)
C
       V2B=V2B-1.0d0/2*U41
       deallocate(U41)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,H2A,D1)
       allocate(F2(N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef523146(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N2,N3,N0,N2,N0,N1,l3B,F2)
       allocate(U42(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1*K2*K4
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,D1,F2,U42)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul1342(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U42,1.0d0/2)
       deallocate(U42)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef1243(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N0,N2,H2B,D1)
       allocate(F2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef452136(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,N1,N3,N0,N1,l3B,F2)
       allocate(U43(N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N2))
       I1=K2
       I2=K1*K3*K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,F2,U43)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul1243(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U43,-1.0d0)
       deallocate(U43)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef2341(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N2,N3,H2B,D1)
       allocate(F2(N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef512346(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,l3B,F2)
       allocate(U44(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4
       I2=K1*K2*K3
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,D1,F2,U44)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul2341(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U44,1.0d0)
       deallocate(U44)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N0,N1,H2B,D1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef461235(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,l3C,F2)
       allocate(U123(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1
       I2=K2*K3*K4
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,F2,U123)
       deallocate(D1)
       deallocate(F2)
C
       V2B=V2B-U123
       deallocate(U123)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,H2B,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef413256(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N2,N3,N0,N2,N0,N1,l3C,F2)
       allocate(U124(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1*K2*K4
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,D1,F2,U124)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul1342(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U124,1.0d0)
       deallocate(U124)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,H2C,D1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,l3C,F2)
       allocate(U125(N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N2))
       I1=K2
       I2=K1*K3*K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,F2,U125)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul1243(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U125,-1.0d0/2)
       deallocate(U125)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,H2C,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef412356(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N1,N3,N0,N2,N0,N1,l3C,F2)
       allocate(U126(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4
       I2=K1*K2*K3
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,D1,F2,U126)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul2341(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U126,1.0d0/2)
       deallocate(U126)
C
       end
