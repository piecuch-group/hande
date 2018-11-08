       subroutine R3C_update(N0,N1,N2,N3,V3C,
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
       real*8 V3C(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1)
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
       real*8,allocatable::Q11(:,:)
       real*8,allocatable::S159(:,:,:,:)
       real*8,allocatable::S3(:,:,:,:)
       real*8,allocatable::S6(:,:,:,:)
       real*8,allocatable::S7(:,:,:,:)
       real*8,allocatable::S8(:,:,:,:)
       real*8,allocatable::S9(:,:,:,:)
       real*8,allocatable::S12(:,:,:,:)
       real*8,allocatable::S16(:,:,:,:)
       real*8,allocatable::S10(:,:,:,:)
       real*8,allocatable::S13(:,:,:,:)
       real*8,allocatable::S17(:,:,:,:)
       real*8,allocatable::S11(:,:,:,:)
       real*8,allocatable::S18(:,:,:,:)
       real*8,allocatable::S19(:,:,:,:)
       real*8,allocatable::S14(:,:,:,:)
       real*8,allocatable::S72(:,:,:,:)
       real*8,allocatable::S119(:,:,:,:)
       real*8,allocatable::Q29(:,:)
       real*8,allocatable::S169(:,:,:,:)
       real*8,allocatable::S174(:,:,:,:)
       real*8,allocatable::S198(:,:,:,:)
       real*8,allocatable::S15(:,:,:,:)
       real*8,allocatable::S30(:,:,:,:)
       real*8,allocatable::S31(:,:,:,:)
       real*8,allocatable::S85(:,:,:,:)
       real*8,allocatable::S86(:,:,:,:)
       real*8,allocatable::S129(:,:,:,:)
       real*8,allocatable::S130(:,:,:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::Q3(:,:)
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::S37(:,:,:,:)
       real*8,allocatable::S38(:,:,:,:)
       real*8,allocatable::S39(:,:,:,:)
       real*8,allocatable::S40(:,:,:,:)
       real*8,allocatable::S43(:,:,:,:)
       real*8,allocatable::S47(:,:,:,:)
       real*8,allocatable::S41(:,:,:,:)
       real*8,allocatable::S44(:,:,:,:)
       real*8,allocatable::S48(:,:,:,:)
       real*8,allocatable::S42(:,:,:,:)
       real*8,allocatable::S49(:,:,:,:)
       real*8,allocatable::S50(:,:,:,:)
       real*8,allocatable::S45(:,:,:,:)
       real*8,allocatable::S54(:,:,:,:)
       real*8,allocatable::S55(:,:,:,:)
       real*8,allocatable::S46(:,:,:,:)
       real*8,allocatable::Q4(:,:)
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::Q8(:,:)
       real*8,allocatable::S20(:,:,:,:)
       real*8,allocatable::Q16(:,:)
       real*8,allocatable::S77(:,:,:,:)
       real*8,allocatable::S21(:,:,:,:)
       real*8,allocatable::S154(:,:,:,:)
       real*8,allocatable::S164(:,:,:,:)
       real*8,allocatable::S170(:,:,:,:)
       real*8,allocatable::S180(:,:,:,:)
       real*8,allocatable::S51(:,:,:,:)
       real*8,allocatable::S56(:,:,:,:)
       real*8,allocatable::S58(:,:,:,:)
       real*8,allocatable::S87(:,:,:,:)
       real*8,allocatable::S32(:,:,:,:)
       real*8,allocatable::S22(:,:,:,:)
       real*8,allocatable::Q17(:,:)
       real*8,allocatable::S123(:,:,:,:)
       real*8,allocatable::S23(:,:,:,:)
       real*8,allocatable::S155(:,:,:,:)
       real*8,allocatable::S165(:,:,:,:)
       real*8,allocatable::S175(:,:,:,:)
       real*8,allocatable::S185(:,:,:,:)
       real*8,allocatable::S52(:,:,:,:)
       real*8,allocatable::S57(:,:,:,:)
       real*8,allocatable::S131(:,:,:,:)
       real*8,allocatable::S67(:,:,:,:)
       real*8,allocatable::S71(:,:,:,:)
       real*8,allocatable::S76(:,:,:,:)
       real*8,allocatable::S66(:,:,:,:)
       real*8,allocatable::S75(:,:,:,:)
       real*8,allocatable::S70(:,:,:,:)
       real*8,allocatable::S73(:,:,:,:)
       real*8,allocatable::S120(:,:,:,:)
       real*8,allocatable::Q30(:,:)
       real*8,allocatable::S179(:,:,:,:)
       real*8,allocatable::S184(:,:,:,:)
       real*8,allocatable::S199(:,:,:,:)
       real*8,allocatable::S74(:,:,:,:)
       real*8,allocatable::S80(:,:,:,:)
       real*8,allocatable::S81(:,:,:,:)
       real*8,allocatable::S82(:,:,:,:)
       real*8,allocatable::S127(:,:,:,:)
       real*8,allocatable::S83(:,:,:,:)
       real*8,allocatable::S24(:,:,:,:)
       real*8,allocatable::S53(:,:,:,:)
       real*8,allocatable::S25(:,:,:,:)
       real*8,allocatable::Q9(:,:)
       real*8,allocatable::S88(:,:,:,:)
       real*8,allocatable::S89(:,:,:,:)
       real*8,allocatable::S90(:,:,:,:)
       real*8,allocatable::S92(:,:,:,:)
       real*8,allocatable::S93(:,:,:,:)
       real*8,allocatable::S91(:,:,:,:)
       real*8,allocatable::S94(:,:,:,:)
       real*8,allocatable::S95(:,:,:,:)
       real*8,allocatable::S106(:,:,:,:)
       real*8,allocatable::S107(:,:,:,:)
       real*8,allocatable::S139(:,:,:,:)
       real*8,allocatable::Q34(:,:)
       real*8,allocatable::S190(:,:,:,:)
       real*8,allocatable::S194(:,:,:,:)
       real*8,allocatable::S203(:,:,:,:)
       real*8,allocatable::S96(:,:,:,:)
       real*8,allocatable::S105(:,:,:,:)
       real*8,allocatable::S108(:,:,:,:)
       real*8,allocatable::S145(:,:,:,:)
       real*8,allocatable::S109(:,:,:,:)
       real*8,allocatable::S110(:,:,:,:)
       real*8,allocatable::S111(:,:,:,:)
       real*8,allocatable::S112(:,:,:,:)
       real*8,allocatable::S115(:,:,:,:)
       real*8,allocatable::S116(:,:,:,:)
       real*8,allocatable::S113(:,:,:,:)
       real*8,allocatable::S117(:,:,:,:)
       real*8,allocatable::S121(:,:,:,:)
       real*8,allocatable::S114(:,:,:,:)
       real*8,allocatable::S122(:,:,:,:)
       real*8,allocatable::S118(:,:,:,:)
       real*8,allocatable::Q20(:,:)
       real*8,allocatable::Q21(:,:)
       real*8,allocatable::Q25(:,:)
       real*8,allocatable::S28(:,:,:,:)
       real*8,allocatable::S128(:,:,:,:)
       real*8,allocatable::S29(:,:,:,:)
       real*8,allocatable::S78(:,:,:,:)
       real*8,allocatable::S79(:,:,:,:)
       real*8,allocatable::S124(:,:,:,:)
       real*8,allocatable::S26(:,:,:,:)
       real*8,allocatable::S27(:,:,:,:)
       real*8,allocatable::S84(:,:,:,:)
       real*8,allocatable::Q10(:,:)
       real*8,allocatable::Q19(:,:)
       real*8,allocatable::S62(:,:,:,:)
       real*8,allocatable::S132(:,:,:,:)
       real*8,allocatable::S133(:,:,:,:)
       real*8,allocatable::S134(:,:,:,:)
       real*8,allocatable::S136(:,:,:,:)
       real*8,allocatable::S135(:,:,:,:)
       real*8,allocatable::S137(:,:,:,:)
       real*8,allocatable::S138(:,:,:,:)
       real*8,allocatable::S101(:,:,:,:)
       real*8,allocatable::S102(:,:,:,:)
       real*8,allocatable::S97(:,:,:,:)
       real*8,allocatable::S98(:,:,:,:)
       real*8,allocatable::Q35(:,:)
       real*8,allocatable::S191(:,:,:,:)
       real*8,allocatable::S204(:,:,:,:)
       real*8,allocatable::S207(:,:,:,:)
       real*8,allocatable::S140(:,:,:,:)
       real*8,allocatable::S146(:,:,:,:)
       real*8,allocatable::Q27(:,:)
       real*8,allocatable::Q28(:,:)
       real*8,allocatable::S4(:,:,:,:)
       real*8,allocatable::S5(:,:,:,:)
       real*8,allocatable::S36(:,:,:,:)
       real*8,allocatable::Q31(:,:)
       real*8,allocatable::S147(:,:,:,:)
       real*8,allocatable::Q32(:,:)
       real*8,allocatable::Q33(:,:)
       real*8,allocatable::S141(:,:,:,:)
       real*8,allocatable::S142(:,:,:,:)
       real*8,allocatable::S99(:,:,:,:)
       real*8,allocatable::S1d0(:,:,:,:)
       real*8,allocatable::S143(:,:,:,:)
       real*8,allocatable::Q36(:,:)
       real*8,allocatable::S148(:,:,:,:)
       real*8,allocatable::S149(:,:,:,:)
       real*8,allocatable::S151(:,:,:,:)
       real*8,allocatable::S152(:,:,:,:)
       real*8,allocatable::S153(:,:,:,:)
       real*8,allocatable::Q12(:,:)
       real*8,allocatable::S59(:,:,:,:)
       real*8,allocatable::Q13(:,:)
       real*8,allocatable::Q37(:,:)
       real*8,allocatable::Q38(:,:)
       real*8,allocatable::S156(:,:,:,:)
       real*8,allocatable::S157(:,:,:,:)
       real*8,allocatable::S158(:,:,:,:)
       real*8,allocatable::S160(:,:,:,:)
       real*8,allocatable::S65(:,:,:,:)
       real*8,allocatable::S69(:,:,:,:)
       real*8,allocatable::S68(:,:,:,:)
       real*8,allocatable::S163(:,:,:,:)
       real*8,allocatable::S63(:,:,:,:)
       real*8,allocatable::S64(:,:,:,:)
       real*8,allocatable::S166(:,:,:,:)
       real*8,allocatable::S167(:,:,:,:)
       real*8,allocatable::S168(:,:,:,:)
       real*8,allocatable::Q6(:,:)
       real*8,allocatable::Q14(:,:)
       real*8,allocatable::S171(:,:,:,:)
       real*8,allocatable::S172(:,:,:,:)
       real*8,allocatable::S173(:,:,:,:)
       real*8,allocatable::S33(:,:,:,:)
       real*8,allocatable::S161(:,:,:,:)
       real*8,allocatable::S176(:,:,:,:)
       real*8,allocatable::S177(:,:,:,:)
       real*8,allocatable::S178(:,:,:,:)
       real*8,allocatable::S162(:,:,:,:)
       real*8,allocatable::Q15(:,:)
       real*8,allocatable::S181(:,:,:,:)
       real*8,allocatable::S182(:,:,:,:)
       real*8,allocatable::S183(:,:,:,:)
       real*8,allocatable::Q18(:,:)
       real*8,allocatable::S61(:,:,:,:)
       real*8,allocatable::S60(:,:,:,:)
       real*8,allocatable::S186(:,:,:,:)
       real*8,allocatable::S187(:,:,:,:)
       real*8,allocatable::S188(:,:,:,:)
       real*8,allocatable::S189(:,:,:,:)
       real*8,allocatable::Q22(:,:)
       real*8,allocatable::Q26(:,:)
       real*8,allocatable::Q23(:,:)
       real*8,allocatable::Q39(:,:)
       real*8,allocatable::Q40(:,:)
       real*8,allocatable::Q41(:,:)
       real*8,allocatable::Q42(:,:)
       real*8,allocatable::S192(:,:,:,:)
       real*8,allocatable::S193(:,:,:,:)
       real*8,allocatable::S150(:,:,:,:)
       real*8,allocatable::S195(:,:,:,:)
       real*8,allocatable::S196(:,:,:,:)
       real*8,allocatable::S197(:,:,:,:)
       real*8,allocatable::S34(:,:,:,:)
       real*8,allocatable::S35(:,:,:,:)
       real*8,allocatable::S125(:,:,:,:)
       real*8,allocatable::S126(:,:,:,:)
       real*8,allocatable::S2d0(:,:,:,:)
       real*8,allocatable::S201(:,:,:,:)
       real*8,allocatable::S202(:,:,:,:)
       real*8,allocatable::Q24(:,:)
       real*8,allocatable::S103(:,:,:,:)
       real*8,allocatable::S144(:,:,:,:)
       real*8,allocatable::S104(:,:,:,:)
       real*8,allocatable::S206(:,:,:,:)
       real*8,allocatable::S205(:,:,:,:)
       real*8,allocatable::Q43(:,:)
       real*8,allocatable::Q44(:,:)
       real*8,allocatable::S208(:,:,:,:)
       real*8,allocatable::S209(:,:,:,:)
       real*8,allocatable::S210(:,:,:,:)
       real*8,allocatable::S211(:,:,:,:)
       real*8,allocatable::S212(:,:,:,:)
       real*8,allocatable::S213(:,:,:,:)
       real*8,allocatable::S214(:,:,:,:)
       real*8,allocatable::S215(:,:,:,:)
       real*8,allocatable::S216(:,:,:,:)
       real*8,allocatable::S217(:,:,:,:)
       real*8,allocatable::S218(:,:,:,:)
       real*8,allocatable::S219(:,:,:,:)
       real*8,allocatable::X1(:,:)
       real*8,allocatable::U1(:,:,:,:,:,:)
       real*8,allocatable::X2(:,:,:,:)
       real*8,allocatable::U2(:,:,:,:,:,:)
       real*8,allocatable::X3(:,:)
       real*8,allocatable::U57(:,:,:,:,:,:)
       real*8,allocatable::X4(:,:,:,:)
       real*8,allocatable::U171(:,:,:,:,:,:)
       real*8,allocatable::X5(:,:,:,:)
       real*8,allocatable::U6(:,:,:,:,:,:)
       real*8,allocatable::X6(:,:,:,:)
       real*8,allocatable::U7(:,:,:,:,:,:)
       real*8,allocatable::X7(:,:,:,:)
       real*8,allocatable::U8(:,:,:,:,:,:)
       real*8,allocatable::X8(:,:,:,:)
       real*8,allocatable::U9(:,:,:,:,:,:)
       real*8,allocatable::X9(:,:,:,:)
       real*8,allocatable::U10(:,:,:,:,:,:)
       real*8,allocatable::X10(:,:,:,:)
       real*8,allocatable::U11(:,:,:,:,:,:)
       real*8,allocatable::X11(:,:,:,:)
       real*8,allocatable::U77(:,:,:,:,:,:)
       real*8,allocatable::X12(:,:,:,:)
       real*8,allocatable::U117(:,:,:,:,:,:)
       real*8,allocatable::X13(:,:)
       real*8,allocatable::U62(:,:,:,:,:,:)
       real*8,allocatable::X14(:,:,:,:)
       real*8,allocatable::U82(:,:,:,:,:,:)
       real*8,allocatable::X15(:,:,:,:)
       real*8,allocatable::U176(:,:,:,:,:,:)
       real*8,allocatable::X16(:,:)
       real*8,allocatable::U63(:,:,:,:,:,:)
       real*8,allocatable::X17(:,:,:,:)
       real*8,allocatable::U177(:,:,:,:,:,:)
       real*8,allocatable::X18(:,:,:,:)
       real*8,allocatable::U90(:,:,:,:,:,:)
       real*8,allocatable::X19(:,:,:,:)
       real*8,allocatable::U92(:,:,:,:,:,:)
       real*8,allocatable::X20(:,:,:,:)
       real*8,allocatable::U93(:,:,:,:,:,:)
       real*8,allocatable::X21(:,:,:,:)
       real*8,allocatable::U104(:,:,:,:,:,:)
       real*8,allocatable::X22(:,:,:,:)
       real*8,allocatable::U137(:,:,:,:,:,:)
       real*8,allocatable::U164(:,:,:,:,:,:)
       real*8,allocatable::U165(:,:,:,:,:,:)
       real*8,allocatable::U166(:,:,:,:,:,:)
       real*8,allocatable::U167(:,:,:,:,:,:)
       real*8,allocatable::U208(:,:,:,:,:,:)
       real*8,allocatable::U209(:,:,:,:,:,:)
       real*8,allocatable::U211(:,:,:,:,:,:)
       real*8,allocatable::U224(:,:,:,:,:,:)
       real*8,allocatable::U228(:,:,:,:,:,:)
       real*8,allocatable::U233(:,:,:,:,:,:)
       real*8,allocatable::U234(:,:,:,:,:,:)
       real*8,allocatable::U235(:,:,:,:,:,:)
       real*8,allocatable::U236(:,:,:,:,:,:)
       real*8,allocatable::U237(:,:,:,:,:,:)
       real*8,allocatable::U238(:,:,:,:,:,:)
       real*8,allocatable::U239(:,:,:,:,:,:)
       real*8,allocatable::U240(:,:,:,:,:,:)
       real*8,allocatable::U241(:,:,:,:,:,:)
       real*8,allocatable::U242(:,:,:,:,:,:)
       real*8,allocatable::U243(:,:,:,:,:,:)
       real*8,allocatable::U244(:,:,:,:,:,:)
       real*8,allocatable::U251(:,:,:,:,:,:)
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
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       X2=0.0d0
       call sul3124(N0,N1,N1,N3,N1,N3,N0,N1,X2,S1, 1.0d0)
       deallocate(S1)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       call relef2341(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N0,N1,N1,N3,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S2(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S2)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef2431(N0,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,N0,N1,S2,D1)
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
       allocate(X3(N0+1:N1,N0+1:N1))
       X3=0.0d0
       X3=X3+Q11
       deallocate(Q11)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef2431(N0,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,N0,N1,S2,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,r2B,D2)
       allocate(S159(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S159)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X4(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       X4=0.0d0
       call sul2314(N0,N1,N2,N3,N0,N2,N0,N1,X4,S159, 1.0d0)
       deallocate(S159)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef3241(N0,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,S2,D1)
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
       call sul3124(N0,N1,N1,N3,N1,N3,N0,N1,X2,S3, 1.0d0)
       deallocate(S3)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S6(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S6)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X5(N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       X5=0.0d0
       call sul3124(N1,N3,N2,N3,N1,N3,N0,N2,X5,S6, 1.0d0)
       deallocate(S6)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:N2,N0+1:N1))
       call relef4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N2,N0,N2,N0,N1,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S7(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S7)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X6(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1))
       X6=0.0d0
       call sul2134(N0,N2,N1,N3,N0,N2,N0,N1,X6,S7, 1.0d0)
       deallocate(S7)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       call relef4132(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N2,N3,N0,N1,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S8(N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4*K4
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S8)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X7(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       X7=0.0d0
       call sul3124(N2,N3,N2,N3,N1,N3,N0,N1,X7,S8, 1.0d0)
       deallocate(S8)
C
       allocate(D1(N0+1:N1,N0+1:N2,N1+1:N3,N0+1:N2))
       call relef4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N2,N1,N3,N0,N2,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S9(N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N2))
       I1=K2*K3*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S9)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X8(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       X8=0.0d0
       call sul3124(N0,N2,N1,N3,N1,N3,N0,N2,X8,S9, 1.0d0)
C
       allocate(D1(N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N2))
       call relef3214(N1,N3,N0,N2,N1,N3,N0,N2,
     & N1,N3,N0,N2,N1,N3,N0,N2,S9,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S12(N0+1:N1,N0+1:N2,N1+1:N3,N0+1:N2))
       I1=K2*K3*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S12)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N2,N1,N3,N0,N2,N0,N1,X6,S12, 1.0d0)
       deallocate(S12)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef2314(N1,N3,N0,N2,N1,N3,N0,N2,
     & N0,N2,N1,N3,N1,N3,N0,N2,S9,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S16(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K3
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S16)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N1,N3,N2,N3,N1,N3,N0,N2,X5,S16,-1.0d0)
       deallocate(S16)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef4123(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N2,N3,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S10(N1+1:N3,N2+1:N3,N1+1:N3,N2+1:N3))
       I1=K4*K3*K4
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S10)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X9(N2+1:N3,N1+1:N3,N2+1:N3,N1+1:N3))
       X9=0.0d0
       call sul4123(N2,N3,N1,N3,N2,N3,N1,N3,X9,S10, 1.0d0)
C
       allocate(D1(N1+1:N3,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef3241(N1,N3,N2,N3,N1,N3,N2,N3,
     & N1,N3,N2,N3,N2,N3,N1,N3,S10,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S13(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       I1=K3*K4*K4
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S13)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N2,N3,N2,N3,N1,N3,N0,N1,X7,S13, 1.0d0)
       deallocate(S13)
C
       allocate(D1(N2+1:N3,N1+1:N3,N2+1:N3,N1+1:N3))
       call relef2341(N1,N3,N2,N3,N1,N3,N2,N3,
     & N2,N3,N1,N3,N2,N3,N1,N3,S10,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S17(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:N3))
       I1=K3*K4*K3
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S17)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N1,N3,N2,N3,N1,N3,N0,N2,X5,S17, 1.0d0)
       deallocate(S17)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,N0+1:N1))
       call relef4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N2,N2,N3,N0,N1,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S11(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S11)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X10(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       X10=0.0d0
       call sul3124(N0,N2,N2,N3,N1,N3,N0,N1,X10,S11, 1.0d0)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef3214(N1,N3,N0,N2,N2,N3,N0,N1,
     & N2,N3,N0,N2,N1,N3,N0,N1,S11,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S18(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S18)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N1,N3,N0,N2,N0,N1,X6,S18, 1.0d0)
       deallocate(S18)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef2314(N1,N3,N0,N2,N2,N3,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,S11,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S19(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S19)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N2,N3,N2,N3,N1,N3,N0,N1,X7,S19,-1.0d0)
       deallocate(S19)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       call relef2341(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N2,N0,N1,N2,N3,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S14(N0+1:N1,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4*K1*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S14)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef2341(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N0,N1,N2,N3,N0,N1,S14,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S72(N2+1:N3,N0+1:N1,N2+1:N3,N0+1:N1))
       I1=K1*K4*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S72)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X11(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       X11=0.0d0
       call sul3124(N0,N1,N2,N3,N2,N3,N0,N1,X11,S72, 1.0d0)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       call relef4231(N0,N1,N0,N2,N0,N1,N2,N3,
     & N2,N3,N0,N2,N0,N1,N0,N1,S14,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S119(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S119)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X12(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       X12=0.0d0
       call sul3124(N0,N2,N0,N1,N0,N2,N0,N1,X12,S119, 1.0d0)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call relef2431(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,N0,N1,S14,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(Q29(N0+1:N1,N0+1:N1))
       I1=K1*K1
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q29)
       deallocate(D1)
       deallocate(B2)
C
       X3=X3+Q29
       deallocate(Q29)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef2341(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N0,N1,N2,N3,N0,N1,S14,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,r2B,D2)
       allocate(S169(N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K3*K4
       I3=K1*K2
       call jungemm(I1,I2,I3,D1,D2,S169)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N2,N3,N2,N3,N1,N3,N0,N1,X7,S169,-1.0d0)
       deallocate(S169)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef3421(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N1,N2,N3,N0,N2,N0,N1,S14,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,r2B,D2)
       allocate(S174(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K2*K3
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,D2,S174)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N0,N2,N1,N3,N0,N2,N0,N1,X6,S174, 1.0d0)
       deallocate(S174)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call relef2431(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,N0,N1,S14,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,r2C,D2)
       allocate(S198(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S198)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N0,N1,N2,N3,N0,N2,N0,N1,X4,S198, 1.0d0)
       deallocate(S198)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,N0+1:N1))
       call relef3241(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N1,N0,N2,N2,N3,N0,N1,S14,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S15(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S15)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N2,N3,N1,N3,N0,N1,X10,S15, 1.0d0)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef3214(N1,N3,N0,N2,N2,N3,N0,N1,
     & N2,N3,N0,N2,N1,N3,N0,N1,S15,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S30(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S30)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N1,N3,N0,N2,N0,N1,X6,S30, 1.0d0)
       deallocate(S30)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef2314(N1,N3,N0,N2,N2,N3,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,S15,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S31(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S31)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N2,N3,N2,N3,N1,N3,N0,N1,X7,S31,-1.0d0)
       deallocate(S31)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       call relef2314(N2,N3,N0,N1,N2,N3,N0,N1,
     & N0,N1,N2,N3,N2,N3,N0,N1,S72,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S85(N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4*K4
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S85)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N2,N3,N2,N3,N1,N3,N0,N1,X7,S85,-1.0d0)
       deallocate(S85)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef3214(N2,N3,N0,N1,N2,N3,N0,N1,
     & N2,N3,N0,N1,N2,N3,N0,N1,S72,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S86(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       I1=K1*K4*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S86)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N1,N2,N3,N0,N2,N0,N1,X4,S86,-1.0d0)
       deallocate(S86)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:N2,N0+1:N1))
       call relef3214(N0,N2,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N2,N0,N2,N0,N1,S119,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S129(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S129)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N2,N1,N3,N0,N2,N0,N1,X6,S129, 1.0d0)
       deallocate(S129)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       call relef2314(N0,N2,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N2,N0,N1,S119,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S130(N2+1:N3,N0+1:N1,N0+1:N2,N0+1:N1))
       I1=K1*K2*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S130)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N1,N2,N3,N0,N2,N0,N1,X4,S130,-1.0d0)
       deallocate(S130)
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
       call sul21(N0,N1,N0,N1,X3,Q7, 1.0d0)
       deallocate(Q7)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef2431(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N2,N3,N0,N2,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S37(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S37)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N1,N2,N3,N0,N2,N0,N1,X4,S37, 1.0d0)
       deallocate(S37)
C
       allocate(D1(N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N2))
       call relef2341(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N2,N1,N3,N0,N2,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S38(N0+1:N1,N0+1:N2,N1+1:N3,N0+1:N2))
       I1=K2*K3*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S38)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N2,N1,N3,N0,N2,N0,N1,X6,S38,-1.0d0)
       deallocate(S38)
C
       allocate(D1(N1+1:N3,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef2134(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N2,N3,N2,N3,N1,N3,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S39(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       I1=K3*K4*K4
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S39)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N2,N3,N2,N3,N1,N3,N0,N1,X7,S39,-1.0d0)
       deallocate(S39)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
       call relef2341(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N2,N0,N1,N0,N2,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S40(N0+1:N1,N0+1:N2,N0+1:N1,N0+1:N2))
       I1=K2*K1*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S40)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N2,N0,N1,N0,N2,N0,N1,X12,S40, 1.0d0)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:N2,N0+1:N1))
       call relef3241(N0,N1,N0,N2,N0,N1,N0,N2,
     & N0,N1,N0,N2,N0,N2,N0,N1,S40,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S43(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S43)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N2,N1,N3,N0,N2,N0,N1,X6,S43, 1.0d0)
       deallocate(S43)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       call relef2341(N0,N1,N0,N2,N0,N1,N0,N2,
     & N0,N2,N0,N1,N0,N2,N0,N1,S40,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S47(N2+1:N3,N0+1:N1,N0+1:N2,N0+1:N1))
       I1=K1*K2*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S47)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N1,N2,N3,N0,N2,N0,N1,X4,S47,-1.0d0)
       deallocate(S47)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:N3,N2+1:N3))
       call relef2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N2,N3,N2,N3,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S41(N0+1:N1,N0+1:N1,N2+1:N3,N2+1:N3))
       I1=K4*K4*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S41)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N1,N2,N3,N2,N3,N0,N1,X11,S41,-1.0d0)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       call relef2341(N0,N1,N0,N1,N2,N3,N2,N3,
     & N0,N1,N2,N3,N2,N3,N0,N1,S41,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S44(N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4*K4
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S44)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N2,N3,N2,N3,N1,N3,N0,N1,X7,S44, 1.0d0)
       deallocate(S44)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef3241(N0,N1,N0,N1,N2,N3,N2,N3,
     & N2,N3,N0,N1,N2,N3,N0,N1,S41,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S48(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       I1=K1*K4*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S48)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N1,N2,N3,N0,N2,N0,N1,X4,S48, 1.0d0)
       deallocate(S48)
C
       allocate(D1(N1+1:N3,N0+1:N2,N2+1:N3,N1+1:N3))
       call relef2314(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N2,N2,N3,N1,N3,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S42(N0+1:N1,N0+1:N2,N2+1:N3,N1+1:N3))
       I1=K3*K4*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S42)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N2,N2,N3,N1,N3,N0,N1,X10,S42,-1.0d0)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef3241(N0,N1,N0,N2,N2,N3,N1,N3,
     & N2,N3,N0,N2,N1,N3,N0,N1,S42,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S49(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S49)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N1,N3,N0,N2,N0,N1,X6,S49,-1.0d0)
       deallocate(S49)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef2341(N0,N1,N0,N2,N2,N3,N1,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,S42,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S50(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S50)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N2,N3,N2,N3,N1,N3,N0,N1,X7,S50, 1.0d0)
       deallocate(S50)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       call relef2341(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N2,N0,N1,N2,N3,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S45(N0+1:N1,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4*K1*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S45)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef3421(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N1,N2,N3,N0,N2,N0,N1,S45,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(S54(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K2*K3
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,D2,S54)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N0,N2,N1,N3,N0,N2,N0,N1,X6,S54, 1.0d0)
       deallocate(S54)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef2341(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N0,N1,N2,N3,N0,N1,S45,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2B,D2)
       allocate(S55(N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K3*K4
       I3=K1*K2
       call jungemm(I1,I2,I3,D1,D2,S55)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N2,N3,N2,N3,N1,N3,N0,N1,X7,S55,-1.0d0)
       deallocate(S55)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,N0+1:N1))
       call relef3241(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N1,N0,N2,N2,N3,N0,N1,S45,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S46(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S46)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N2,N3,N1,N3,N0,N1,X10,S46, 1.0d0)
       deallocate(S46)
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
       call sul21(N0,N1,N0,N1,X3,Q8, 1.0d0)
       deallocate(Q8)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N0,N1,N1,N3,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S20(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S20)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call relef3421(N0,N2,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N2,N0,N2,S20,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(Q16(N0+1:N2,N0+1:N2))
       I1=K2*K2
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q16)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X13(N0+1:N2,N0+1:N2))
       X13=0.0d0
       X13=X13+Q16
       deallocate(Q16)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N2,N0,N1,N1,N3,
     & N0,N2,N0,N1,N1,N3,N0,N2,S20,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S77(N2+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S77)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X14(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       X14=0.0d0
       call sul3124(N0,N1,N1,N3,N2,N3,N0,N2,X14,S77, 1.0d0)
C
       allocate(D1(N0+1:N1,N0+1:N2,N1+1:N3,N0+1:N2))
       call relef3241(N0,N2,N0,N2,N0,N1,N1,N3,
     & N0,N1,N0,N2,N1,N3,N0,N2,S20,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S21(N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N2))
       I1=K2*K3*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S21)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N1,N3,N1,N3,N0,N2,X8,S21, 1.0d0)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call relef3421(N0,N2,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N2,N0,N2,S20,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,r2A,D2)
       allocate(S154(N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S154)
       deallocate(D1)
       deallocate(D2)
C
       call sul2413(N0,N2,N1,N3,N0,N2,N0,N1,X6,S154,-1.0d0)
       deallocate(S154)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call relef3421(N0,N2,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N2,N0,N2,S20,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,r2B,D2)
       allocate(S164(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S164)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X15(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X15=0.0d0
       call sul2314(N0,N2,N2,N3,N0,N2,N0,N2,X15,S164, 1.0d0)
       deallocate(S164)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N2,N0,N1,N1,N3,
     & N0,N2,N0,N1,N1,N3,N0,N2,S20,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,r2B,D2)
       allocate(S170(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3
       I2=K3*K4
       I3=K1*K2
       call jungemm(I1,I2,I3,D1,D2,S170)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N1,N3,N2,N3,N1,N3,N0,N2,X5,S170,-1.0d0)
       deallocate(S170)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,N0+1:N2))
       call relef2431(N0,N2,N0,N2,N0,N1,N1,N3,
     & N0,N2,N1,N3,N0,N1,N0,N2,S20,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
       call relef3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,N3,N0,N1,r2B,D2)
       allocate(S180(N2+1:N3,N0+1:N1,N0+1:N1,N0+1:N2))
       I1=K2*K1
       I2=K1*K4
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,D2,S180)
       deallocate(D1)
       deallocate(D2)
C
       call sul2413(N0,N1,N2,N3,N0,N2,N0,N1,X4,S180,-1.0d0)
       deallocate(S180)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
       call relef4231(N0,N2,N0,N2,N0,N1,N1,N3,
     & N1,N3,N0,N2,N0,N1,N0,N2,S20,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S51(N0+1:N1,N0+1:N2,N0+1:N1,N0+1:N2))
       I1=K2*K1*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S51)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N2,N0,N1,N0,N2,N0,N1,X12,S51, 1.0d0)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:N2,N0+1:N1))
       call relef3241(N0,N1,N0,N2,N0,N1,N0,N2,
     & N0,N1,N0,N2,N0,N2,N0,N1,S51,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S56(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S56)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N2,N1,N3,N0,N2,N0,N1,X6,S56, 1.0d0)
       deallocate(S56)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       call relef2341(N0,N1,N0,N2,N0,N1,N0,N2,
     & N0,N2,N0,N1,N0,N2,N0,N1,S51,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S58(N2+1:N3,N0+1:N1,N0+1:N2,N0+1:N1))
       I1=K1*K2*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S58)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N1,N2,N3,N0,N2,N0,N1,X4,S58,-1.0d0)
       deallocate(S58)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef2314(N2,N3,N0,N1,N1,N3,N0,N2,
     & N0,N1,N1,N3,N2,N3,N0,N2,S77,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S87(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S87)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N1,N3,N2,N3,N1,N3,N0,N2,X5,S87,-1.0d0)
       deallocate(S87)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef2314(N1,N3,N0,N2,N1,N3,N0,N2,
     & N0,N2,N1,N3,N1,N3,N0,N2,S21,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S32(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K3
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S32)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N1,N3,N2,N3,N1,N3,N0,N2,X5,S32,-1.0d0)
       deallocate(S32)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S22(N2+1:N3,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S22)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3))
       call relef2431(N2,N3,N0,N1,N2,N3,N1,N3,
     & N0,N1,N1,N3,N2,N3,N2,N3,S22,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(Q17(N2+1:N3,N2+1:N3))
       I1=K4*K4
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q17)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X16(N2+1:N3,N2+1:N3))
       X16=0.0d0
       X16=X16+Q17
       deallocate(Q17)
C
       allocate(D1(N2+1:N3,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3241(N2,N3,N0,N1,N2,N3,N1,N3,
     & N2,N3,N0,N1,N1,N3,N2,N3,S22,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S123(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       I1=K4*K3*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S123)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N1,N1,N3,N2,N3,N0,N2,X14,S123, 1.0d0)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef2341(N2,N3,N0,N1,N2,N3,N1,N3,
     & N0,N1,N2,N3,N1,N3,N2,N3,S22,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S23(N1+1:N3,N2+1:N3,N1+1:N3,N2+1:N3))
       I1=K4*K3*K4
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S23)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N2,N3,N1,N3,N2,N3,N1,N3,X9,S23,-1.0d0)
       deallocate(S23)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3))
       call relef2431(N2,N3,N0,N1,N2,N3,N1,N3,
     & N0,N1,N1,N3,N2,N3,N2,N3,S22,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,r2A,D2)
       allocate(S155(N1+1:N3,N0+1:N1,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S155)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N2,N3,N2,N3,N1,N3,N0,N1,X7,S155, 1.0d0)
       deallocate(S155)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3))
       call relef2431(N2,N3,N0,N1,N2,N3,N1,N3,
     & N0,N1,N1,N3,N2,N3,N2,N3,S22,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,r2B,D2)
       allocate(S165(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S165)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X17(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       X17=0.0d0
       call sul3412(N2,N3,N2,N3,N2,N3,N0,N2,X17,S165, 1.0d0)
       deallocate(S165)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef2341(N2,N3,N0,N1,N2,N3,N1,N3,
     & N0,N1,N2,N3,N1,N3,N2,N3,S22,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,r2B,D2)
       allocate(S175(N1+1:N3,N0+1:N2,N1+1:N3,N2+1:N3))
       I1=K4*K3
       I2=K2*K3
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,D2,S175)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N1,N3,N2,N3,N1,N3,N0,N2,X5,S175,-1.0d0)
       deallocate(S175)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N1,N2+1:N3))
       call relef3421(N2,N3,N0,N1,N2,N3,N1,N3,
     & N2,N3,N1,N3,N0,N1,N2,N3,S22,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,r2B,D2)
       allocate(S185(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3))
       I1=K4*K1
       I2=K1*K2
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,D2,S185)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N1,N2,N3,N0,N2,N0,N1,X4,S185,-1.0d0)
       deallocate(S185)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:N3,N2+1:N3))
       call relef4231(N2,N3,N0,N1,N2,N3,N1,N3,
     & N1,N3,N0,N1,N2,N3,N2,N3,S22,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S52(N0+1:N1,N0+1:N1,N2+1:N3,N2+1:N3))
       I1=K4*K4*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S52)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N1,N2,N3,N2,N3,N0,N1,X11,S52, 1.0d0)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       call relef2341(N0,N1,N0,N1,N2,N3,N2,N3,
     & N0,N1,N2,N3,N2,N3,N0,N1,S52,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S57(N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4*K4
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S57)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N2,N3,N2,N3,N1,N3,N0,N1,X7,S57,-1.0d0)
       deallocate(S57)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N1,N1,N3,N2,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,S123,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S131(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S131)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N1,N3,N2,N3,N1,N3,N0,N2,X5,S131,-1.0d0)
       deallocate(S131)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S67(N2+1:N3,N2+1:N3,N1+1:N3,N1+1:N3))
       I1=K3*K3*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S67)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N2,N3,N1,N3,N2,N3,N1,N3,X9,S67, 1.0d0)
C
       allocate(D1(N1+1:N3,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef3214(N2,N3,N2,N3,N1,N3,N1,N3,
     & N1,N3,N2,N3,N2,N3,N1,N3,S67,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S71(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       I1=K3*K4*K4
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S71)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N2,N3,N2,N3,N1,N3,N0,N1,X7,S71, 1.0d0)
       deallocate(S71)
C
       allocate(D1(N2+1:N3,N1+1:N3,N2+1:N3,N1+1:N3))
       call relef2314(N2,N3,N2,N3,N1,N3,N1,N3,
     & N2,N3,N1,N3,N2,N3,N1,N3,S67,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S76(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:N3))
       I1=K3*K4*K3
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S76)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N1,N3,N2,N3,N1,N3,N0,N2,X5,S76, 1.0d0)
       deallocate(S76)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N0,N1,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S66(N2+1:N3,N0+1:N1,N2+1:N3,N0+1:N1))
       I1=K1*K4*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S66)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N1,N2,N3,N2,N3,N0,N1,X11,S66, 1.0d0)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef3214(N2,N3,N0,N1,N2,N3,N0,N1,
     & N2,N3,N0,N1,N2,N3,N0,N1,S66,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S75(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       I1=K1*K4*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S75)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N1,N2,N3,N0,N2,N0,N1,X4,S75,-1.0d0)
       deallocate(S75)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       call relef2314(N2,N3,N0,N1,N2,N3,N0,N1,
     & N0,N1,N2,N3,N2,N3,N0,N1,S66,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S70(N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4*K4
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S70)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N2,N3,N2,N3,N1,N3,N0,N1,X7,S70,-1.0d0)
       deallocate(S70)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,N1+1:N3))
       call relef4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N2,N2,N3,N1,N3,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S73(N1+1:N3,N0+1:N2,N2+1:N3,N1+1:N3))
       I1=K3*K4*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S73)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N1+1:N3))
       call relef3241(N1,N3,N0,N2,N2,N3,N1,N3,
     & N2,N3,N0,N2,N1,N3,N1,N3,S73,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S120(N0+1:N2,N0+1:N2,N1+1:N3,N1+1:N3))
       I1=K3*K3*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S120)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N2,N1,N3,N1,N3,N0,N2,X8,S120, 1.0d0)
       deallocate(S120)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef2341(N1,N3,N0,N2,N2,N3,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,S73,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(Q30(N1+1:N3,N1+1:N3))
       I1=K3*K3
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q30)
       deallocate(D1)
       deallocate(B2)
C
       X1=X1+Q30
       deallocate(Q30)
C
       allocate(D1(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:N3))
       call relef2431(N1,N3,N0,N2,N2,N3,N1,N3,
     & N0,N2,N1,N3,N2,N3,N1,N3,S73,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
       call relef3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,N3,N0,N1,r2B,D2)
       allocate(S179(N2+1:N3,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K1*K4
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,D2,S179)
       deallocate(D1)
       deallocate(D2)
C
       call sul2413(N2,N3,N2,N3,N1,N3,N0,N1,X7,S179,-1.0d0)
       deallocate(S179)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3))
       call relef3421(N1,N3,N0,N2,N2,N3,N1,N3,
     & N2,N3,N1,N3,N0,N2,N1,N3,S73,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,r2B,D2)
       allocate(S184(N0+1:N2,N0+1:N1,N0+1:N2,N1+1:N3))
       I1=K3*K2
       I2=K1*K2
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,D2,S184)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N2,N1,N3,N0,N2,N0,N1,X6,S184, 1.0d0)
       deallocate(S184)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef2341(N1,N3,N0,N2,N2,N3,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,S73,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,r2C,D2)
       allocate(S199(N2+1:N3,N0+1:N2,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S199)
       deallocate(D1)
       deallocate(D2)
C
       call sul2413(N1,N3,N2,N3,N1,N3,N0,N2,X5,S199, 1.0d0)
       deallocate(S199)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef2341(N1,N3,N0,N2,N2,N3,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,S73,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S74(N2+1:N3,N2+1:N3,N1+1:N3,N1+1:N3))
       I1=K3*K3*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S74)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N2,N3,N1,N3,N2,N3,N1,N3,X9,S74,-1.0d0)
       deallocate(S74)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N1+1:N3))
       call relef4132(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N0,N2,N1,N3,IntM,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(S80(N1+1:N3,N0+1:N2,N0+1:N2,N1+1:N3))
       I1=K3*K2
       I2=K2*K3
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,D2,S80)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef3412(N1,N3,N0,N2,N0,N2,N1,N3,
     & N0,N2,N1,N3,N1,N3,N0,N2,S80,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S81(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K3
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S81)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N1,N3,N2,N3,N1,N3,N0,N2,X5,S81,-1.0d0)
       deallocate(S81)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N2,N3,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,t2B,D2)
       allocate(S82(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S82)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef4312(N2,N3,N0,N2,N0,N2,N2,N3,
     & N2,N3,N0,N2,N2,N3,N0,N2,S82,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S127(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S127)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N2,N3,N0,N2,N0,N2,X15,S127,-1.0d0)
       deallocate(S127)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3412(N2,N3,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,S82,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S83(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S83)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N2,N3,N2,N3,N2,N3,N0,N2,X17,S83,-1.0d0)
       deallocate(S83)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S24(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S24)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef4312(N2,N3,N0,N2,N0,N1,N1,N3,
     & N1,N3,N0,N1,N2,N3,N0,N2,S24,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S53(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S53)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N1,N2,N3,N0,N2,N0,N1,X4,S53, 1.0d0)
       deallocate(S53)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef3412(N2,N3,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,S24,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S25(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S25)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N1,N3,N2,N3,N1,N3,N0,N2,X5,S25, 1.0d0)
       deallocate(S25)
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
       X3=X3+Q9
       deallocate(Q9)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N0,N2,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S88(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S88)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X18(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X18=0.0d0
       call sul2134(N0,N2,N2,N3,N0,N2,N0,N2,X18,S88, 1.0d0)
       deallocate(S88)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S89(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S89)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N2,N3,N2,N3,N2,N3,N0,N2,X17,S89,-1.0d0)
       deallocate(S89)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S90(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S90)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X19(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       X19=0.0d0
       call sul3124(N0,N2,N2,N3,N2,N3,N0,N2,X19,S90, 1.0d0)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef3214(N2,N3,N0,N2,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,N3,N0,N2,S90,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S92(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S92)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N2,N3,N0,N2,N0,N2,X15,S92,-1.0d0)
       deallocate(S92)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef2314(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,S90,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S93(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S93)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N2,N3,N2,N3,N2,N3,N0,N2,X17,S93, 1.0d0)
       deallocate(S93)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S91(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       I1=K4*K4*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S91)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X20(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       X20=0.0d0
       call sul4123(N2,N3,N2,N3,N2,N3,N2,N3,X20,S91, 1.0d0)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef2341(N2,N3,N2,N3,N2,N3,N2,N3,
     & N2,N3,N2,N3,N2,N3,N2,N3,S91,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S94(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       I1=K4*K4*K4
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S94)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N2,N3,N2,N3,N2,N3,N0,N2,X17,S94, 1.0d0)
       deallocate(S94)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N0,N2,N2,N3,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S95(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S95)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,S95,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S106(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S106)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef2314(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,S106,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S107(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S107)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X21(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       X21=0.0d0
       call sul3124(N2,N3,N2,N3,N2,N3,N0,N2,X21,S107, 1.0d0)
       deallocate(S107)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef4231(N0,N2,N0,N2,N0,N2,N2,N3,
     & N2,N3,N0,N2,N0,N2,N0,N2,S95,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S139(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S139)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X22(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       X22=0.0d0
       call sul3124(N0,N2,N0,N2,N0,N2,N0,N2,X22,S139, 1.0d0)
       deallocate(S139)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef2431(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,S95,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(Q34(N0+1:N2,N0+1:N2))
       I1=K2*K2
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q34)
       deallocate(D1)
       deallocate(B2)
C
       X13=X13-Q34
       deallocate(Q34)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef3421(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,S95,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,r2B,D2)
       allocate(S190(N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S190)
       deallocate(D1)
       deallocate(D2)
C
       call sul2413(N0,N2,N1,N3,N0,N2,N0,N1,X6,S190,-1.0d0)
       deallocate(S190)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,S95,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,r2C,D2)
       allocate(S194(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K4*K4
       I3=K2*K2
       call jungemm(I1,I2,I3,D1,D2,S194)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N2,N3,N2,N3,N2,N3,N0,N2,X21,S194, 0.5d0)
       deallocate(S194)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef2431(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,S95,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,r2C,D2)
       allocate(S203(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S203)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N0,N2,N2,N3,N0,N2,N0,N2,X15,S203,-1.0d0)
       deallocate(S203)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,S95,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S96(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S96)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N2,N3,N2,N3,N0,N2,X19,S96,-1.0d0)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef3214(N2,N3,N0,N2,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,N3,N0,N2,S96,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S105(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S105)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N2,N3,N0,N2,N0,N2,X18,S105,-1.0d0)
       deallocate(S105)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef2314(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,S96,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S108(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S108)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N2,N3,N2,N3,N2,N3,N0,N2,X21,S108, 1.0d0)
       deallocate(S108)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef3214(N2,N3,N0,N2,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,N3,N0,N2,S106,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S145(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S145)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N2,N3,N0,N2,N0,N2,X15,S145, 1.0d0)
       deallocate(S145)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef1432(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N1,N2,N3,N0,N1,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S109(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       I1=K1*K4*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S109)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N1,N2,N3,N0,N2,N0,N1,X4,S109, 1.0d0)
       deallocate(S109)
C
       allocate(D1(N2+1:N3,N1+1:N3,N2+1:N3,N1+1:N3))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N2,N3,N1,N3,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S110(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:N3))
       I1=K3*K4*K3
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S110)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N1,N3,N2,N3,N1,N3,N0,N2,X5,S110,-1.0d0)
       deallocate(S110)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N1,N3,N0,N1,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S111(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S111)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N1,N3,N0,N2,N0,N1,X6,S111,-1.0d0)
       deallocate(S111)
C
       allocate(D1(N2+1:N3,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef1423(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S112(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       I1=K4*K3*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S112)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N1,N1,N3,N2,N3,N0,N2,X14,S112,-1.0d0)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef3241(N0,N2,N0,N1,N1,N3,N2,N3,
     & N1,N3,N0,N1,N2,N3,N0,N2,S112,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S115(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S115)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N1,N2,N3,N0,N2,N0,N1,X4,S115, 1.0d0)
       deallocate(S115)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N1,N1,N3,N2,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,S112,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S116(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S116)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N1,N3,N2,N3,N1,N3,N0,N2,X5,S116, 1.0d0)
       deallocate(S116)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N0,N1,N0,N1,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S113(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S113)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N0,N1,N0,N2,N0,N1,X12,S113, 1.0d0)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:N2,N0+1:N1))
       call relef3214(N0,N2,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N2,N0,N2,N0,N1,S113,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S117(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S117)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N2,N1,N3,N0,N2,N0,N1,X6,S117, 1.0d0)
       deallocate(S117)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       call relef2314(N0,N2,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N2,N0,N1,S113,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S121(N2+1:N3,N0+1:N1,N0+1:N2,N0+1:N1))
       I1=K1*K2*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S121)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N1,N2,N3,N0,N2,N0,N1,X4,S121,-1.0d0)
       deallocate(S121)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N1+1:N3))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N1,N3,N1,N3,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S114(N0+1:N2,N0+1:N2,N1+1:N3,N1+1:N3))
       I1=K3*K3*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S114)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N2,N1,N3,N1,N3,N0,N2,X8,S114,-1.0d0)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N2,N1,N3,N1,N3,
     & N0,N2,N1,N3,N1,N3,N0,N2,S114,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S122(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K3
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S122)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N1,N3,N2,N3,N1,N3,N0,N2,X5,S122, 1.0d0)
       deallocate(S122)
C
       allocate(D1(N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N2))
       call relef3241(N0,N2,N0,N2,N1,N3,N1,N3,
     & N1,N3,N0,N2,N1,N3,N0,N2,S114,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S118(N0+1:N1,N0+1:N2,N1+1:N3,N0+1:N2))
       I1=K2*K3*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S118)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N2,N1,N3,N0,N2,N0,N1,X6,S118,-1.0d0)
       deallocate(S118)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N2,N3,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q20(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q20)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(Q21(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,Q20,B2,Q21)
       deallocate(B2)
C
       call sul21(N2,N3,N2,N3,X16,Q21, 1.0d0)
       deallocate(Q21)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call relef21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q20,B1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(Q25(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,B1,B2,Q25)
       deallocate(B1)
       deallocate(B2)
C
       call sul21(N0,N2,N0,N2,X13,Q25, 1.0d0)
       deallocate(Q25)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,N2+1:N3))
       call relef3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N0,N1,N2,N3,IntM,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
       call relef3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,N3,N0,N1,t2B,D2)
       allocate(S28(N2+1:N3,N0+1:N1,N0+1:N1,N2+1:N3))
       I1=K4*K1
       I2=K1*K4
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,D2,S28)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef4312(N2,N3,N0,N1,N0,N1,N2,N3,
     & N2,N3,N0,N1,N2,N3,N0,N1,S28,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S128(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       I1=K1*K4*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S128)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N1,N2,N3,N0,N2,N0,N1,X4,S128,-1.0d0)
       deallocate(S128)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       call relef3412(N2,N3,N0,N1,N0,N1,N2,N3,
     & N0,N1,N2,N3,N2,N3,N0,N1,S28,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S29(N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4*K4
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S29)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N2,N3,N2,N3,N1,N3,N0,N1,X7,S29,-1.0d0)
       deallocate(S29)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N2,N3,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S78(N1+1:N3,N0+1:N1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S78)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3412(N1,N3,N0,N1,N0,N2,N2,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,S78,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S79(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S79)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N2,N3,N2,N3,N1,N3,N0,N1,X7,S79, 1.0d0)
       deallocate(S79)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef4312(N1,N3,N0,N1,N0,N2,N2,N3,
     & N2,N3,N0,N2,N1,N3,N0,N1,S78,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S124(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S124)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N1,N3,N0,N2,N0,N1,X6,S124,-1.0d0)
       deallocate(S124)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N0,N2,N0,N1,IntM,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,t2B,D2)
       allocate(S26(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K1*K2
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,D2,S26)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:N2,N0+1:N1))
       call relef4312(N0,N2,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,N2,N0,N2,N0,N1,S26,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S27(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S27)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N2,N1,N3,N0,N2,N0,N1,X6,S27, 1.0d0)
       deallocate(S27)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       call relef3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,N1,N0,N2,N0,N1,S26,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S84(N2+1:N3,N0+1:N1,N0+1:N2,N0+1:N1))
       I1=K1*K2*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S84)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N1,N2,N3,N0,N2,N0,N1,X4,S84,-1.0d0)
       deallocate(S84)
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
       X1=X1-Q10
       deallocate(Q10)
C
       allocate(B1(N0+1:N2,N2+1:N3))
       call relef21(0,N3,0,N3,
     & N0,N2,N2,N3,FockB,B1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(Q19(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,B1,B2,Q19)
       deallocate(B1)
       deallocate(B2)
C
       call sul21(N2,N3,N2,N3,X16,Q19, 1.0d0)
       deallocate(Q19)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N0,N2,N0,N1,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S62(N2+1:N3,N0+1:N1,N0+1:N2,N0+1:N1))
       I1=K1*K2*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S62)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N1,N2,N3,N0,N2,N0,N1,X4,S62,-1.0d0)
       deallocate(S62)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S132(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S132)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N2,N3,N0,N2,N0,N2,X15,S132,-1.0d0)
       deallocate(S132)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N2,N3,N2,N3,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S133(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       I1=K4*K4*K4
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S133)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N2,N3,N2,N3,N2,N3,N0,N2,X21,S133, 1.0d0)
       deallocate(S133)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N0,N2,N0,N2,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S134(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S134)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N0,N2,N0,N2,N0,N2,X22,S134,-1.0d0)
C
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,t3C,F2)
       allocate(U137(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K1*K3*K4*K4
       I3=K2*K2
       call jungemm(I1,I2,I3,X22,F2,U137)
       deallocate(F2)
C
       call
     & sul123645(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U137,-0.5d0)
       call
     & sul123654(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U137, 0.5d0)
       deallocate(U137)
       deallocate(X22)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef2314(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S134,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S136(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S136)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N2,N2,N3,N0,N2,N0,N2,X15,S136,-1.0d0)
       deallocate(S136)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S135(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S135)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N2,N2,N3,N2,N3,N0,N2,X19,S135,-1.0d0)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef3241(N0,N2,N0,N2,N2,N3,N2,N3,
     & N2,N3,N0,N2,N2,N3,N0,N2,S135,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S137(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S137)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N2,N2,N3,N0,N2,N0,N2,X15,S137,-1.0d0)
       deallocate(S137)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,S135,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S138(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S138)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N2,N3,N2,N3,N2,N3,N0,N2,X17,S138, 1.0d0)
       deallocate(S138)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2C,D2)
       allocate(S101(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K2
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,D2,S101)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S101,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S102(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S102)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N2,N2,N3,N0,N2,N0,N2,X18,S102, 0.5d0)
       deallocate(S102)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S97(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S97)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef2341(N2,N3,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,S97,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S98(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       I1=K4*K4*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S98)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N2,N3,N2,N3,N2,N3,N2,N3,X20,S98, 1.0d0)
       deallocate(S98)
C
       allocate(F2(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       call relef123456(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,t3C,F2)
       allocate(U93(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K1*K2*K2*K3
       I3=K4*K4
       call jungemm(I1,I2,I3,X20,F2,U93)
       deallocate(F2)
C
       call
     & sul345612(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U93, 0.5d0)
       call
     & sul345621(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U93,-0.5d0)
       deallocate(U93)
       deallocate(X20)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef2341(N2,N3,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,S97,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(Q35(N2+1:N3,N2+1:N3))
       I1=K4*K4
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q35)
       deallocate(D1)
       deallocate(B2)
C
       X16=X16-Q35
       deallocate(Q35)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef2431(N2,N3,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,S97,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,r2B,D2)
       allocate(S191(N1+1:N3,N0+1:N1,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S191)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N2,N3,N2,N3,N1,N3,N0,N1,X7,S191, 1.0d0)
       deallocate(S191)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef2341(N2,N3,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,S97,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,r2C,D2)
       allocate(S204(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S204)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N2,N3,N2,N3,N2,N3,N0,N2,X17,S204,-1.0d0)
       deallocate(S204)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef3421(N2,N3,N0,N2,N2,N3,N2,N3,
     & N2,N3,N2,N3,N0,N2,N2,N3,S97,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,r2C,D2)
       allocate(S207(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K2
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,D2,S207)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N2,N2,N3,N0,N2,N0,N2,X18,S207, 0.5d0)
       deallocate(S207)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3241(N2,N3,N0,N2,N2,N3,N2,N3,
     & N2,N3,N0,N2,N2,N3,N2,N3,S97,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S140(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S140)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N2,N2,N3,N2,N3,N0,N2,X19,S140,-1.0d0)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,S140,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S146(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S146)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N2,N3,N2,N3,N2,N3,N0,N2,X21,S146,-1.0d0)
       deallocate(S146)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N0,N1,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(Q27(N0+1:N1,N0+1:N1))
       I1=K1*K1
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q27)
       deallocate(D1)
       deallocate(B2)
C
       X3=X3+Q27
       deallocate(Q27)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(Q28(N1+1:N3,N1+1:N3))
       I1=K3*K3
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q28)
       deallocate(D1)
       deallocate(B2)
C
       X1=X1-Q28
       deallocate(Q28)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,t2B,D2)
       allocate(S4(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S4)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef3412(N2,N3,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,S4,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S5(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S5)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N1,N3,N2,N3,N1,N3,N0,N2,X5,S5, 1.0d0)
       deallocate(S5)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef4312(N2,N3,N0,N2,N0,N1,N1,N3,
     & N1,N3,N0,N1,N2,N3,N0,N2,S4,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S36(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S36)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N1,N2,N3,N0,N2,N0,N1,X4,S36, 1.0d0)
       deallocate(S36)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N1,N3,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(Q31(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q31)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call relef21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q31,B1)
       allocate(D2(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,N3,N0,N2,N0,N1,t2B,D2)
       allocate(S147(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K2*K4
       I3=K3
       call jungemm(I1,I2,I3,B1,D2,S147)
       deallocate(B1)
       deallocate(D2)
C
       call sul2341(N0,N1,N2,N3,N0,N2,N0,N1,X4,S147, 1.0d0)
       deallocate(S147)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(Q32(N0+1:N2,N0+1:N2))
       I1=K2*K2
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q32)
       deallocate(D1)
       deallocate(B2)
C
       X13=X13+Q32
       deallocate(Q32)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(Q33(N2+1:N3,N2+1:N3))
       I1=K4*K4
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q33)
       deallocate(D1)
       deallocate(B2)
C
       X16=X16-Q33
       deallocate(Q33)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N0,N2,N2,N3,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S141(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S141)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,S141,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(S142(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K4*K4
       I3=K2*K2
       call jungemm(I1,I2,I3,D1,D2,S142)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N2,N3,N2,N3,N2,N3,N0,N2,X21,S142, 0.5d0)
       deallocate(S142)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S99(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S99)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3412(N2,N3,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,S99,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S1d0(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S1d0)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N2,N3,N2,N3,N2,N3,N0,N2,X17,S1d0,-1.0d0)
       deallocate(S1d0)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef4312(N2,N3,N0,N2,N0,N2,N2,N3,
     & N2,N3,N0,N2,N2,N3,N0,N2,S99,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S143(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S143)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N2,N3,N0,N2,N0,N2,X15,S143,-1.0d0)
       deallocate(S143)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(Q36(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q36)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call relef21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q36,B1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,t2B,D2)
       allocate(S148(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
       I1=K2
       I2=K1*K2*K3
       I3=K4
       call jungemm(I1,I2,I3,B1,D2,S148)
       deallocate(B1)
       deallocate(D2)
C
       call sul2341(N0,N2,N1,N3,N0,N2,N0,N1,X6,S148,-1.0d0)
       deallocate(S148)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call relef21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q36,B1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2C,D2)
       allocate(S149(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K2*K4
       I3=K4
       call jungemm(I1,I2,I3,B1,D2,S149)
       deallocate(B1)
       deallocate(D2)
C
       call sul2341(N0,N2,N2,N3,N0,N2,N0,N2,X18,S149, 1.0d0)
       deallocate(S149)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N0,N2,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,r2A,D2)
       allocate(S151(N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S151)
       deallocate(D1)
       deallocate(D2)
C
       call sul2413(N0,N2,N1,N3,N0,N2,N0,N1,X6,S151,-1.0d0)
       deallocate(S151)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,r2A,D2)
       allocate(S152(N1+1:N3,N0+1:N1,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S152)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N2,N3,N2,N3,N1,N3,N0,N1,X7,S152,-1.0d0)
       deallocate(S152)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N2,N3,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,r2A,D2)
       allocate(S153(N1+1:N3,N0+1:N1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S153)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N2,N2,N3,N1,N3,N0,N1,X10,S153,-1.0d0)
       deallocate(S153)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N1,N3,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(Q12(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q12)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call relef21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q12,B1)
       allocate(D2(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,N3,N0,N2,N0,N1,t2B,D2)
       allocate(S59(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K2*K4
       I3=K3
       call jungemm(I1,I2,I3,B1,D2,S59)
       deallocate(B1)
       deallocate(D2)
C
       call sul2341(N0,N1,N2,N3,N0,N2,N0,N1,X4,S59, 1.0d0)
       deallocate(S59)
C
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q13(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,Q12,B2,Q13)
       deallocate(B2)
C
       call sul21(N1,N3,N1,N3,X1,Q13, 1.0d0)
       deallocate(Q13)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,r2A,D2)
       allocate(Q37(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,D2,Q37)
       deallocate(D1)
       deallocate(D2)
C
       call sul21(N1,N3,N1,N3,X1,Q37,-0.5d0)
       deallocate(Q37)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,r2A,D2)
       allocate(Q38(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,D1,D2,Q38)
       deallocate(D1)
       deallocate(D2)
C
       call sul21(N0,N1,N0,N1,X3,Q38, 0.5d0)
       deallocate(Q38)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N0,N1,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,r2B,D2)
       allocate(S156(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S156)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N0,N1,N2,N3,N0,N2,N0,N1,X4,S156, 1.0d0)
       deallocate(S156)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,r2B,D2)
       allocate(S157(N2+1:N3,N0+1:N2,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S157)
       deallocate(D1)
       deallocate(D2)
C
       call sul2413(N1,N3,N2,N3,N1,N3,N0,N2,X5,S157,-1.0d0)
       deallocate(S157)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,r2B,D2)
       allocate(S158(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S158)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N1,N1,N3,N2,N3,N0,N2,X14,S158,-1.0d0)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef3412(N2,N3,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,S158,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S160(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S160)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N1,N3,N2,N3,N1,N3,N0,N2,X5,S160, 1.0d0)
       deallocate(S160)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N0,N2,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S65(N2+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S65)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N1,N1,N3,N2,N3,N0,N2,X14,S65, 1.0d0)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef2314(N2,N3,N0,N1,N1,N3,N0,N2,
     & N0,N1,N1,N3,N2,N3,N0,N2,S65,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S69(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S69)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N1,N3,N2,N3,N1,N3,N0,N2,X5,S69,-1.0d0)
       deallocate(S69)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef3214(N2,N3,N0,N1,N1,N3,N0,N2,
     & N1,N3,N0,N1,N2,N3,N0,N2,S65,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S68(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S68)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N1,N2,N3,N0,N2,N0,N1,X4,S68,-1.0d0)
       deallocate(S68)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N2,N3,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,r2B,D2)
       allocate(S163(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S163)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N2,N2,N3,N2,N3,N0,N2,X19,S163, 1.0d0)
       deallocate(S163)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N1,N3,N0,N2,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S63(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K3
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S63)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N1,N3,N2,N3,N1,N3,N0,N2,X5,S63, 1.0d0)
       deallocate(S63)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S64(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S64)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N2,N3,N2,N3,N1,N3,N0,N1,X7,S64, 1.0d0)
       deallocate(S64)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N0,N2,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,r2B,D2)
       allocate(S166(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3
       I2=K3*K4
       I3=K1*K2
       call jungemm(I1,I2,I3,D1,D2,S166)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N1,N3,N2,N3,N1,N3,N0,N2,X5,S166,-1.0d0)
       deallocate(S166)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N0,N1,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,r2B,D2)
       allocate(S167(N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K3*K4
       I3=K1*K2
       call jungemm(I1,I2,I3,D1,D2,S167)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N2,N3,N2,N3,N1,N3,N0,N1,X7,S167,-1.0d0)
       deallocate(S167)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,r2B,D2)
       allocate(S168(N2+1:N3,N1+1:N3,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K3*K4
       I3=K1*K2
       call jungemm(I1,I2,I3,D1,D2,S168)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N2,N3,N1,N3,N2,N3,N1,N3,X9,S168,-1.0d0)
       deallocate(S168)
C
       allocate(F2(N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       call relef132456(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N2,N3,N1,N3,N2,N3,N0,N2,N0,N2,N0,N1,t3C,F2)
       allocate(U10(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K1*K2*K2*K4
       I3=K3*K4
       call jungemm(I1,I2,I3,X9,F2,U10)
       deallocate(F2)
C
       call
     & sul245613(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U10,-1.0d0)
       call
     & sul145623(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U10, 1.0d0)
       deallocate(U10)
       deallocate(X9)
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
       call sul21(N0,N1,N0,N1,X3,Q6, 1.0d0)
       deallocate(Q6)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N0,N2,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(Q14(N0+1:N2,N0+1:N2))
       I1=K2*K2
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q14)
       deallocate(D1)
       deallocate(B2)
C
       X13=X13+Q14
       deallocate(Q14)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef4123(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,r2B,D2)
       allocate(S171(N1+1:N3,N0+1:N2,N1+1:N3,N2+1:N3))
       I1=K4*K3
       I2=K2*K3
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,D2,S171)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N1,N3,N2,N3,N1,N3,N0,N2,X5,S171, 1.0d0)
       deallocate(S171)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef4132(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N0,N2,N0,N1,IntM,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,r2B,D2)
       allocate(S172(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K2*K3
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,D2,S172)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N0,N2,N1,N3,N0,N2,N0,N1,X6,S172, 1.0d0)
       deallocate(S172)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N1+1:N3))
       call relef4132(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N0,N2,N1,N3,IntM,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,r2B,D2)
       allocate(S173(N1+1:N3,N0+1:N2,N0+1:N2,N1+1:N3))
       I1=K3*K2
       I2=K2*K3
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,D2,S173)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N2,N1,N3,N1,N3,N0,N2,X8,S173, 1.0d0)
       deallocate(S173)
C
       allocate(F2(N0+1:N2,N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef431256(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,N3,N2,N3,N0,N2,N0,N1,t3C,F2)
       allocate(U9(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3
       I2=K1*K2*K4*K4
       I3=K3*K2
       call jungemm(I1,I2,I3,X8,F2,U9)
       deallocate(F2)
C
       call
     & sul124635(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U9,-1.0d0)
       call
     & sul125634(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U9, 1.0d0)
       deallocate(U9)
       deallocate(X8)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef2314(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S33(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S33)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N1,N1,N3,N1,N3,N0,N1,X2,S33,-1.0d0)
       deallocate(S33)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N0,N2,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,r2B,D2)
       allocate(S161(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S161)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N0,N2,N2,N3,N0,N2,N0,N2,X15,S161, 1.0d0)
       deallocate(S161)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,N0+1:N2))
       call relef3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N0,N1,N0,N2,IntM,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
       call relef3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,N3,N0,N1,r2B,D2)
       allocate(S176(N2+1:N3,N0+1:N1,N0+1:N1,N0+1:N2))
       I1=K2*K1
       I2=K1*K4
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,D2,S176)
       deallocate(D1)
       deallocate(D2)
C
       call sul2413(N0,N1,N2,N3,N0,N2,N0,N1,X4,S176,-1.0d0)
       deallocate(S176)
C
       allocate(D1(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:N3))
       call relef3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
       call relef3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,N3,N0,N1,r2B,D2)
       allocate(S177(N2+1:N3,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K1*K4
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,D2,S177)
       deallocate(D1)
       deallocate(D2)
C
       call sul2413(N2,N3,N2,N3,N1,N3,N0,N1,X7,S177, 1.0d0)
       deallocate(S177)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,N2+1:N3))
       call relef3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N0,N1,N2,N3,IntM,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
       call relef3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,N3,N0,N1,r2B,D2)
       allocate(S178(N2+1:N3,N0+1:N1,N0+1:N1,N2+1:N3))
       I1=K4*K1
       I2=K1*K4
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,D2,S178)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N1,N2,N3,N2,N3,N0,N1,X11,S178, 1.0d0)
       deallocate(S178)
C
       allocate(F2(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2))
       call relef612345(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,t3C,F2)
       allocate(U77(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K2*K2*K3*K4
       I3=K4*K1
       call jungemm(I1,I2,I3,X11,F2,U77)
       deallocate(F2)
C
       call
     & sul134526(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U77,-1.0d0)
       call
     & sul234516(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U77, 1.0d0)
       deallocate(U77)
       deallocate(X11)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,r2B,D2)
       allocate(S162(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S162)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N2,N3,N2,N3,N2,N3,N0,N2,X17,S162,-1.0d0)
       deallocate(S162)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N2,N3,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(Q15(N2+1:N3,N2+1:N3))
       I1=K4*K4
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q15)
       deallocate(D1)
       deallocate(B2)
C
       X16=X16-Q15
       deallocate(Q15)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N1,N2+1:N3))
       call relef1243(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N0,N1,N2,N3,IntM,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,r2B,D2)
       allocate(S181(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3))
       I1=K4*K1
       I2=K1*K2
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,D2,S181)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N1,N2,N3,N0,N2,N0,N1,X4,S181, 1.0d0)
       deallocate(S181)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N0,N2,N1,N3,IntM,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,r2B,D2)
       allocate(S182(N0+1:N2,N0+1:N1,N0+1:N2,N1+1:N3))
       I1=K3*K2
       I2=K1*K2
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,D2,S182)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N2,N1,N3,N0,N2,N0,N1,X6,S182,-1.0d0)
       deallocate(S182)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N0,N2,N0,N1,IntM,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,r2B,D2)
       allocate(S183(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K1*K2
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,D2,S183)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N2,N0,N1,N0,N2,N0,N1,X12,S183, 1.0d0)
       deallocate(S183)
C
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef461235(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,t3C,F2)
       allocate(U117(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K2*K3*K4*K4
       I3=K1*K2
       call jungemm(I1,I2,I3,X12,F2,U117)
       deallocate(F2)
C
       call
     & sul123546(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U117, 1.0d0)
       V3C=V3C-U117
       deallocate(U117)
       deallocate(X12)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N2,N3,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(Q18(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K3*K1
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
       allocate(S61(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K2*K4
       I3=K4
       call jungemm(I1,I2,I3,B1,D2,S61)
       deallocate(B1)
       deallocate(D2)
C
       call sul2341(N0,N2,N2,N3,N0,N2,N0,N2,X18,S61, 1.0d0)
       deallocate(S61)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call relef21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q18,B1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,t2B,D2)
       allocate(S60(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
       I1=K2
       I2=K1*K2*K3
       I3=K4
       call jungemm(I1,I2,I3,B1,D2,S60)
       deallocate(B1)
       deallocate(D2)
C
       call sul2341(N0,N2,N1,N3,N0,N2,N0,N1,X6,S60,-1.0d0)
       deallocate(S60)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,r2B,D2)
       allocate(S186(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S186)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N1,N1,N3,N1,N3,N0,N1,X2,S186,-1.0d0)
       deallocate(S186)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef4132(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,r2B,D2)
       allocate(S187(N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S187)
       deallocate(D1)
       deallocate(D2)
C
       call sul2413(N0,N2,N1,N3,N0,N2,N0,N1,X6,S187, 1.0d0)
       deallocate(S187)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,r2B,D2)
       allocate(S188(N1+1:N3,N0+1:N1,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S188)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N2,N3,N2,N3,N1,N3,N0,N1,X7,S188, 1.0d0)
       deallocate(S188)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,r2B,D2)
       allocate(S189(N1+1:N3,N0+1:N1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S189)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N2,N2,N3,N1,N3,N0,N1,X10,S189,-1.0d0)
       deallocate(S189)
C
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef412356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,t3D,F2)
       allocate(U11(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K2*K2*K4*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,X10,F2,U11)
       deallocate(F2)
C
       call
     & sul124536(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U11,-1.0d0)
       deallocate(U11)
       deallocate(X10)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q22(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q22)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call relef21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q22,B1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(Q26(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,B1,B2,Q26)
       deallocate(B1)
       deallocate(B2)
C
       call sul21(N0,N2,N0,N2,X13,Q26, 1.0d0)
       deallocate(Q26)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(Q23(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,Q22,B2,Q23)
       deallocate(B2)
C
       call sul21(N2,N3,N2,N3,X16,Q23, 1.0d0)
       deallocate(Q23)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,r2B,D2)
       allocate(Q39(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,D2,Q39)
       deallocate(D1)
       deallocate(D2)
C
       call sul21(N2,N3,N2,N3,X16,Q39, 1.0d0)
       deallocate(Q39)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N0,N2,IntM,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,r2B,D2)
       allocate(Q40(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,D1,D2,Q40)
       deallocate(D1)
       deallocate(D2)
C
       call sul21(N0,N2,N0,N2,X13,Q40, 1.0d0)
       deallocate(Q40)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,r2B,D2)
       allocate(Q41(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,Q41)
       deallocate(D1)
       deallocate(D2)
C
       call sul21(N1,N3,N1,N3,X1,Q41, 1.0d0)
       deallocate(Q41)
C
       allocate(F2(N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       call relef312456(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N1,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N1,t3C,F2)
       allocate(U1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1*K2*K2*K4*K4
       I3=K3
       call jungemm(I1,I2,I3,X1,F2,U1)
       deallocate(F2)
C
       call
     & sul124563(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U1,-1.0d0)
       deallocate(U1)
       deallocate(X1)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,r2B,D2)
       allocate(Q42(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,D1,D2,Q42)
       deallocate(D1)
       deallocate(D2)
C
       call sul21(N0,N1,N0,N1,X3,Q42, 1.0d0)
       deallocate(Q42)
C
       allocate(F2(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2))
       call relef612345(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,t3C,F2)
       allocate(U57(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1
       I2=K2*K2*K3*K4*K4
       I3=K1
       call jungemm(I1,I2,I3,X3,F2,U57)
       deallocate(F2)
C
       V3C=V3C-U57
       deallocate(U57)
       deallocate(X3)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,r2C,D2)
       allocate(S192(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K4*K4
       I3=K2*K2
       call jungemm(I1,I2,I3,D1,D2,S192)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N2,N3,N2,N3,N2,N3,N0,N2,X21,S192,-0.5d0)
       deallocate(S192)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,r2C,D2)
       allocate(S193(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K4*K4
       I3=K2*K2
       call jungemm(I1,I2,I3,D1,D2,S193)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N2,N3,N2,N3,
     & N2,N3,N2,N3,N2,N3,N2,N3,S193,D1)
       allocate(F2(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       call relef123456(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,t3C,F2)
       allocate(U211(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K1*K2*K2*K3
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,F2,U211)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul345612(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U211, 0.250)
       deallocate(U211)
       deallocate(S193)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,r2A,D2)
       allocate(S150(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S150)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N1,N1,N3,N1,N3,N0,N1,X2,S150,-1.0d0)
       deallocate(S150)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N0,N1,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,r2C,D2)
       allocate(S195(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S195)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N0,N1,N2,N3,N0,N2,N0,N1,X4,S195, 1.0d0)
       deallocate(S195)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,r2C,D2)
       allocate(S196(N2+1:N3,N0+1:N2,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S196)
       deallocate(D1)
       deallocate(D2)
C
       call sul2413(N1,N3,N2,N3,N1,N3,N0,N2,X5,S196,-1.0d0)
       deallocate(S196)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,r2C,D2)
       allocate(S197(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S197)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N1,N1,N3,N2,N3,N0,N2,X14,S197,-1.0d0)
       deallocate(S197)
C
       allocate(F2(N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef521346(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N1,N3,N2,N3,N1,N3,N0,N2,N0,N1,t3B,F2)
       allocate(U82(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K2*K3*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,X14,F2,U82)
       deallocate(F2)
C
       call
     & sul134625(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U82,-1.0d0)
       call
     & sul135624(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U82, 1.0d0)
       call
     & sul234615(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U82, 1.0d0)
       call
     & sul235614(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U82,-1.0d0)
       deallocate(U82)
       deallocate(X14)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       call relef2341(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N0,N1,N1,N3,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S34(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S34)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef3241(N0,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,S34,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S35(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S35)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N1,N1,N3,N1,N3,N0,N1,X2,S35, 1.0d0)
       deallocate(S35)
C
       allocate(F2(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef631245(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N2,N3,N0,N2,N0,N2,t3C,F2)
       allocate(U2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K2*K2*K4*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,X2,F2,U2)
       deallocate(F2)
C
       call
     & sul124536(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U2,-1.0d0)
       deallocate(U2)
       deallocate(X2)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N0,N1,N1,N3,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S125(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S125)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N2,N0,N1,N1,N3,
     & N0,N2,N0,N1,N1,N3,N0,N2,S125,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2B,D2)
       allocate(S126(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3
       I2=K3*K4
       I3=K1*K2
       call jungemm(I1,I2,I3,D1,D2,S126)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N1,N3,N2,N3,N1,N3,N0,N2,X5,S126,-1.0d0)
       deallocate(S126)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,r2C,D2)
       allocate(S2d0(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S2d0)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N0,N2,N2,N3,N0,N2,N0,N2,X15,S2d0, 1.0d0)
       deallocate(S2d0)
C
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(U176(N2+1:N3,N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K1*K3*K4
       I3=K2
       call jungemm(I1,I2,I3,X15,D2,U176)
       deallocate(D2)
C
       call
     & sul136245(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U176, 1.0d0)
       call
     & sul236145(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U176,-1.0d0)
       call
     & sul136254(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U176,-1.0d0)
       call
     & sul236154(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U176, 1.0d0)
       deallocate(U176)
       deallocate(X15)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,r2C,D2)
       allocate(S201(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S201)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N2,N3,N2,N3,N2,N3,N0,N2,X17,S201,-1.0d0)
       deallocate(S201)
C
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,t2B,D2)
       allocate(U177(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K1*K2*K3
       I3=K4
       call jungemm(I1,I2,I3,X17,D2,U177)
       deallocate(D2)
C
       call
     & sul356124(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U177, 1.0d0)
       call
     & sul356214(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U177,-1.0d0)
       call
     & sul346125(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U177,-1.0d0)
       call
     & sul346215(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U177, 1.0d0)
       deallocate(U177)
       deallocate(X17)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,r2C,D2)
       allocate(S202(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S202)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N2,N2,N3,N2,N3,N0,N2,X19,S202, 1.0d0)
       deallocate(S202)
C
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef412356(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N1,N3,N0,N2,N0,N1,t3C,F2)
       allocate(U92(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K2*K3*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,X19,F2,U92)
       deallocate(F2)
C
       call
     & sul134625(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U92, 1.0d0)
       call
     & sul135624(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U92,-1.0d0)
       call
     & sul234615(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U92,-1.0d0)
       call
     & sul235614(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U92, 1.0d0)
       deallocate(U92)
       deallocate(X19)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call relef12(0,N3,0,N3,
     & N2,N3,N0,N2,FockB,B1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(Q24(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,B1,B2,Q24)
       deallocate(B1)
       deallocate(B2)
C
       call sul21(N0,N2,N0,N2,X13,Q24, 1.0d0)
       deallocate(Q24)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(S103(N1+1:N3,N0+1:N1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S103)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef4312(N1,N3,N0,N1,N0,N2,N2,N3,
     & N2,N3,N0,N2,N1,N3,N0,N1,S103,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S144(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S144)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N1,N3,N0,N2,N0,N1,X6,S144,-1.0d0)
       deallocate(S144)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3412(N1,N3,N0,N1,N0,N2,N2,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,S103,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S104(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S104)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N2,N3,N2,N3,N1,N3,N0,N1,X7,S104, 1.0d0)
       deallocate(S104)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,r2C,D2)
       allocate(S206(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K2
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,D2,S206)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S206,D1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,t3C,F2)
       allocate(U224(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K1*K3*K4*K4
       I3=K2*K2
       call jungemm(I1,I2,I3,D1,F2,U224)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul123645(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U224, 0.250)
       deallocate(U224)
       deallocate(S206)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,r2C,D2)
       allocate(S205(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K2
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,D2,S205)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N2,N2,N3,N0,N2,N0,N2,X18,S205, 0.5d0)
       deallocate(S205)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,r2C,D2)
       allocate(Q43(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,D2,Q43)
       deallocate(D1)
       deallocate(D2)
C
       call sul21(N2,N3,N2,N3,X16,Q43, 0.5d0)
       deallocate(Q43)
C
       allocate(F2(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       call relef123456(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,t3C,F2)
       allocate(U63(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4
       I2=K1*K2*K2*K3*K4
       I3=K4
       call jungemm(I1,I2,I3,X16,F2,U63)
       deallocate(F2)
C
       call
     & sul234561(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U63,-1.0d0)
       call
     & sul134562(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U63, 1.0d0)
       deallocate(U63)
       deallocate(X16)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,r2C,D2)
       allocate(Q44(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,D1,D2,Q44)
       deallocate(D1)
       deallocate(D2)
C
       call sul21(N0,N2,N0,N2,X13,Q44, 0.5d0)
       deallocate(Q44)
C
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef412356(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N1,N3,N0,N2,N0,N1,t3C,F2)
       allocate(U62(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
       I1=K2
       I2=K1*K2*K3*K4*K4
       I3=K2
       call jungemm(I1,I2,I3,X13,F2,U62)
       deallocate(F2)
C
       call
     & sul123465(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U62, 1.0d0)
       call
     & sul123564(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U62,-1.0d0)
       deallocate(U62)
       deallocate(X13)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef562134(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N2,N3,N1,N3,N0,N2,r3B,F2)
       allocate(S208(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3))
       I1=K3
       I2=K2*K3*K4
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,F2,S208)
       deallocate(D1)
       deallocate(F2)
C
       call sul2341(N1,N3,N2,N3,N1,N3,N0,N2,X5,S208,-0.5d0)
       deallocate(S208)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(F2(N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef523146(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N2,N3,N0,N2,N0,N1,r3B,F2)
       allocate(S209(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K2*K4
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,D1,F2,S209)
       deallocate(D1)
       deallocate(F2)
C
       call sul2341(N0,N1,N2,N3,N0,N2,N0,N1,X4,S209, 0.5d0)
       deallocate(S209)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(F2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef452136(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,N1,N3,N0,N1,r3B,F2)
       allocate(S210(N2+1:N3,N1+1:N3,N0+1:N1,N2+1:N3))
       I1=K4
       I2=K1*K3*K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,F2,S210)
       deallocate(D1)
       deallocate(F2)
C
       call sul2341(N2,N3,N2,N3,N1,N3,N0,N1,X7,S210, 1.0d0)
       deallocate(S210)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N0,N2,IntM,D1)
       allocate(F2(N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef512346(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,r3B,F2)
       allocate(S211(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
       I1=K2
       I2=K1*K2*K3
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,D1,F2,S211)
       deallocate(D1)
       deallocate(F2)
C
       call sul2341(N0,N2,N1,N3,N0,N2,N0,N1,X6,S211,-1.0d0)
       deallocate(S211)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(F2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef463125(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,N2,N3,N0,N2,r3C,F2)
       allocate(S212(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2*K4*K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,F2,S212)
       deallocate(D1)
       deallocate(F2)
C
       call sul2341(N2,N3,N2,N3,N2,N3,N0,N2,X21,S212, 1.0d0)
       deallocate(S212)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N0,N2,IntM,D1)
       allocate(F2(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef613245(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N2,N3,N0,N2,N0,N2,r3C,F2)
       allocate(S213(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K2*K4
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,D1,F2,S213)
       deallocate(D1)
       deallocate(F2)
C
       call sul2341(N0,N2,N2,N3,N0,N2,N0,N2,X18,S213, 1.0d0)
       deallocate(S213)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef461235(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,r3C,F2)
       allocate(S214(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3))
       I1=K3
       I2=K2*K3*K4
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,F2,S214)
       deallocate(D1)
       deallocate(F2)
C
       call sul2341(N1,N3,N2,N3,N1,N3,N0,N2,X5,S214, 1.0d0)
       deallocate(S214)
C
       allocate(D2(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,N3,N0,N2,N0,N1,t2B,D2)
       allocate(U6(N2+1:N3,N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K4
       I2=K1*K2*K4
       I3=K3
       call jungemm(I1,I2,I3,X5,D2,U6)
       deallocate(D2)
C
       call
     & sul246135(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U6, 1.0d0)
       call
     & sul146235(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U6,-1.0d0)
       call
     & sul256134(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U6,-1.0d0)
       call
     & sul156234(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U6, 1.0d0)
       deallocate(U6)
       deallocate(X5)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef413256(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N2,N3,N0,N2,N0,N1,r3C,F2)
       allocate(S215(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K2*K4
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,D1,F2,S215)
       deallocate(D1)
       deallocate(F2)
C
       call sul2341(N0,N1,N2,N3,N0,N2,N0,N1,X4,S215, 1.0d0)
       deallocate(S215)
C
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(U171(N2+1:N3,N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K4
       I2=K2*K3*K4
       I3=K1
       call jungemm(I1,I2,I3,X4,D2,U171)
       deallocate(D2)
C
       call
     & sul135246(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U171, 1.0d0)
       call
     & sul235146(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U171,-1.0d0)
       call
     & sul134256(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U171,-1.0d0)
       call
     & sul234156(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U171, 1.0d0)
       deallocate(U171)
       deallocate(X4)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,r3C,F2)
       allocate(S216(N2+1:N3,N1+1:N3,N0+1:N1,N2+1:N3))
       I1=K4
       I2=K1*K3*K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,F2,S216)
       deallocate(D1)
       deallocate(F2)
C
       call sul2341(N2,N3,N2,N3,N1,N3,N0,N1,X7,S216,-0.5d0)
       deallocate(S216)
C
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2C,D2)
       allocate(U8(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K2*K2*K4
       I3=K4
       call jungemm(I1,I2,I3,X7,D2,U8)
       deallocate(D2)
C
       call
     & sul245136(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U8,-1.0d0)
       call
     & sul145236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U8, 1.0d0)
       deallocate(U8)
       deallocate(X7)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef4123(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef412356(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N1,N3,N0,N2,N0,N1,r3C,F2)
       allocate(S217(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
       I1=K2
       I2=K1*K2*K3
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,D1,F2,S217)
       deallocate(D1)
       deallocate(F2)
C
       call sul2341(N0,N2,N1,N3,N0,N2,N0,N1,X6,S217, 0.5d0)
       deallocate(S217)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(U7(N2+1:N3,N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K3
       I2=K2*K4*K4
       I3=K2
       call jungemm(I1,I2,I3,X6,D2,U7)
       deallocate(D2)
C
       call
     & sul124356(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U7,-1.0d0)
       call
     & sul125346(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U7, 1.0d0)
       deallocate(U7)
       deallocate(X6)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,r3D,F2)
       allocate(S218(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2*K4*K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,F2,S218)
       deallocate(D1)
       deallocate(F2)
C
       call sul2341(N2,N3,N2,N3,N2,N3,N0,N2,X21,S218, 0.5d0)
       deallocate(S218)
C
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,t2B,D2)
       allocate(U104(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K1*K2*K3
       I3=K4
       call jungemm(I1,I2,I3,X21,D2,U104)
       deallocate(D2)
C
       call
     & sul346125(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U104,-1.0d0)
       call
     & sul356124(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U104, 1.0d0)
       deallocate(U104)
       deallocate(X21)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef412356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,r3D,F2)
       allocate(S219(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K2*K4
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,D1,F2,S219)
       deallocate(D1)
       deallocate(F2)
C
       call sul2341(N0,N2,N2,N3,N0,N2,N0,N2,X18,S219, 0.5d0)
       deallocate(S219)
C
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(U90(N2+1:N3,N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K1*K3*K4
       I3=K2
       call jungemm(I1,I2,I3,X18,D2,U90)
       deallocate(D2)
C
       call
     & sul136245(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U90,-1.0d0)
       call
     & sul236145(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U90, 1.0d0)
       deallocate(U90)
       deallocate(X18)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N0,N2,N0,N1,H2B,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,r2B,D2)
       allocate(U164(N2+1:N3,N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K4
       I2=K2*K3*K4
       I3=K1
       call jungemm(I1,I2,I3,D1,D2,U164)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul234156(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U164, 1.0d0)
       call
     & sul134256(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U164,-1.0d0)
       call
     & sul235146(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U164,-1.0d0)
       call
     & sul135246(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U164, 1.0d0)
       deallocate(U164)
C
       allocate(D1(N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef2341(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N2,N3,N1,N3,N0,N2,H2B,D1)
       allocate(D2(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,N3,N0,N2,N0,N1,r2B,D2)
       allocate(U165(N2+1:N3,N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K4
       I2=K1*K2*K4
       I3=K3
       call jungemm(I1,I2,I3,D1,D2,U165)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul246135(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U165,-1.0d0)
       call
     & sul146235(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U165, 1.0d0)
       call
     & sul256134(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U165, 1.0d0)
       call
     & sul156234(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U165,-1.0d0)
       deallocate(U165)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,H2C,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,r2B,D2)
       allocate(U166(N2+1:N3,N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K1*K3*K4
       I3=K2
       call jungemm(I1,I2,I3,D1,D2,U166)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul236145(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U166, 1.0d0)
       call
     & sul136245(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U166,-1.0d0)
       deallocate(U166)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N2,N3,N0,N2,H2C,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,r2B,D2)
       allocate(U167(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K1*K2*K3
       I3=K4
       call jungemm(I1,I2,I3,D1,D2,U167)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul346125(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U167, 1.0d0)
       call
     & sul356124(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U167,-1.0d0)
       deallocate(U167)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N0,N2,N0,N1,H2B,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,r2C,D2)
       allocate(U208(N2+1:N3,N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K3
       I2=K2*K4*K4
       I3=K2
       call jungemm(I1,I2,I3,D1,D2,U208)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul124356(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U208, 1.0d0)
       call
     & sul125346(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U208,-1.0d0)
       deallocate(U208)
C
       allocate(D1(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N1,N3,N0,N1,H2B,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,r2C,D2)
       allocate(U209(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K2*K2*K4
       I3=K4
       call jungemm(I1,I2,I3,D1,D2,U209)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul245136(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U209, 1.0d0)
       call
     & sul145236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U209,-1.0d0)
       deallocate(U209)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,H2B,D1)
       allocate(F2(N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef521346(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N1,N3,N2,N3,N1,N3,N0,N2,N0,N1,r3B,F2)
       allocate(U228(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K2*K3*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,F2,U228)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul234615(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U228,-1.0d0)
       call
     & sul134625(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U228, 1.0d0)
       call
     & sul235614(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U228, 1.0d0)
       call
     & sul135624(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U228,-1.0d0)
       deallocate(U228)
C
       allocate(B1(N0+1:N1,N0+1:N1))
       call relef21(N0,N3,N0,N3,
     & N0,N1,N0,N1,H1A,B1)
       allocate(F2(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2))
       call relef612345(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,r3C,F2)
       allocate(U233(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1
       I2=K2*K2*K3*K4*K4
       I3=K1
       call jungemm(I1,I2,I3,B1,F2,U233)
       deallocate(B1)
       deallocate(F2)
C
       V3C=V3C-U233
       deallocate(U233)
C
       allocate(B1(N1+1:N3,N1+1:N3))
       call relef12(N0,N3,N0,N3,
     & N1,N3,N1,N3,H1A,B1)
       allocate(F2(N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       call relef312456(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N1,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N1,r3C,F2)
       allocate(U234(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1*K2*K2*K4*K4
       I3=K3
       call jungemm(I1,I2,I3,B1,F2,U234)
       deallocate(B1)
       deallocate(F2)
C
       call
     & sul124563(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U234, 1.0d0)
       deallocate(U234)
C
       allocate(B1(N0+1:N2,N0+1:N2))
       call relef21(N0,N3,N0,N3,
     & N0,N2,N0,N2,H1B,B1)
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef412356(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N1,N3,N0,N2,N0,N1,r3C,F2)
       allocate(U235(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
       I1=K2
       I2=K1*K2*K3*K4*K4
       I3=K2
       call jungemm(I1,I2,I3,B1,F2,U235)
       deallocate(B1)
       deallocate(F2)
C
       call
     & sul123465(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U235, 1.0d0)
       call
     & sul123564(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U235,-1.0d0)
       deallocate(U235)
C
       allocate(B1(N2+1:N3,N2+1:N3))
       call relef12(N0,N3,N0,N3,
     & N2,N3,N2,N3,H1B,B1)
       allocate(F2(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       call relef123456(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,r3C,F2)
       allocate(U236(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4
       I2=K1*K2*K2*K3*K4
       I3=K4
       call jungemm(I1,I2,I3,B1,F2,U236)
       deallocate(B1)
       deallocate(F2)
C
       call
     & sul234561(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U236, 1.0d0)
       call
     & sul134562(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U236,-1.0d0)
       deallocate(U236)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,H2A,D1)
       allocate(F2(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef631245(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N2,N3,N0,N2,N0,N2,r3C,F2)
       allocate(U237(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K2*K2*K4*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,F2,U237)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul124536(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U237, 1.0d0)
       deallocate(U237)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N0,N2,N0,N1,H2B,D1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef461235(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,r3C,F2)
       allocate(U238(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K2*K3*K4*K4
       I3=K1*K2
       call jungemm(I1,I2,I3,D1,F2,U238)
       deallocate(D1)
       deallocate(F2)
C
       V3C=V3C-U238
       call
     & sul123546(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U238, 1.0d0)
       deallocate(U238)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       call relef4132(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N2,N3,N0,N1,H2B,D1)
       allocate(F2(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2))
       call relef612345(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,r3C,F2)
       allocate(U239(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K2*K2*K3*K4
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,F2,U239)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul234516(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U239,-1.0d0)
       call
     & sul134526(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U239, 1.0d0)
       deallocate(U239)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N1,N3,N0,N2,H2B,D1)
       allocate(F2(N0+1:N2,N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef431256(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,N3,N2,N3,N0,N2,N0,N1,r3C,F2)
       allocate(U240(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3
       I2=K1*K2*K4*K4
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,F2,U240)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul124635(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U240, 1.0d0)
       call
     & sul125634(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U240,-1.0d0)
       deallocate(U240)
C
       allocate(D1(N2+1:N3,N1+1:N3,N2+1:N3,N1+1:N3))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N2,N3,N1,N3,H2B,D1)
       allocate(F2(N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       call relef132456(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N2,N3,N1,N3,N2,N3,N0,N2,N0,N2,N0,N1,r3C,F2)
       allocate(U241(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K1*K2*K2*K4
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,F2,U241)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul245613(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U241, 1.0d0)
       call
     & sul145623(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U241,-1.0d0)
       deallocate(U241)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N0,N2,N0,N2,H2C,D1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,r3C,F2)
       allocate(U242(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K1*K3*K4*K4
       I3=K2*K2
       call jungemm(I1,I2,I3,D1,F2,U242)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul123645(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U242, 0.5d0)
       deallocate(U242)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,H2C,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef412356(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N1,N3,N0,N2,N0,N1,r3C,F2)
       allocate(U243(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K2*K3*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,F2,U243)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul234615(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U243,-1.0d0)
       call
     & sul134625(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U243, 1.0d0)
       call
     & sul235614(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U243, 1.0d0)
       call
     & sul135624(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U243,-1.0d0)
       deallocate(U243)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N2,N3,N2,N3,H2C,D1)
       allocate(F2(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       call relef123456(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,r3C,F2)
       allocate(U244(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K1*K2*K2*K3
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,F2,U244)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul345612(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U244, 0.5d0)
       deallocate(U244)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,H2B,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef412356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,r3D,F2)
       allocate(U251(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K2*K2*K4*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,F2,U251)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul124536(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U251, 1.0d0)
       deallocate(U251)
C
       end
