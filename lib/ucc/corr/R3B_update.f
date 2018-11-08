       subroutine R3B_update(N0,N1,N2,N3,V3B,
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
       real*8 V3B(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1)
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
       real*8,allocatable::S9(:,:,:,:)
       real*8,allocatable::S50(:,:,:,:)
       real*8,allocatable::S62(:,:,:,:)
       real*8,allocatable::Q11(:,:)
       real*8,allocatable::S159(:,:,:,:)
       real*8,allocatable::S163(:,:,:,:)
       real*8,allocatable::S170(:,:,:,:)
       real*8,allocatable::S177(:,:,:,:)
       real*8,allocatable::S10(:,:,:,:)
       real*8,allocatable::S19(:,:,:,:)
       real*8,allocatable::S20(:,:,:,:)
       real*8,allocatable::S52(:,:,:,:)
       real*8,allocatable::S53(:,:,:,:)
       real*8,allocatable::S68(:,:,:,:)
       real*8,allocatable::S69(:,:,:,:)
       real*8,allocatable::S24(:,:,:,:)
       real*8,allocatable::S31(:,:,:,:)
       real*8,allocatable::S27(:,:,:,:)
       real*8,allocatable::S25(:,:,:,:)
       real*8,allocatable::S32(:,:,:,:)
       real*8,allocatable::S28(:,:,:,:)
       real*8,allocatable::S26(:,:,:,:)
       real*8,allocatable::S33(:,:,:,:)
       real*8,allocatable::S34(:,:,:,:)
       real*8,allocatable::Q4(:,:)
       real*8,allocatable::Q8(:,:)
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::S41(:,:,:,:)
       real*8,allocatable::S90(:,:,:,:)
       real*8,allocatable::S42(:,:,:,:)
       real*8,allocatable::S45(:,:,:,:)
       real*8,allocatable::S117(:,:,:,:)
       real*8,allocatable::S46(:,:,:,:)
       real*8,allocatable::S29(:,:,:,:)
       real*8,allocatable::S109(:,:,:,:)
       real*8,allocatable::S138(:,:,:,:)
       real*8,allocatable::Q29(:,:)
       real*8,allocatable::S183(:,:,:,:)
       real*8,allocatable::S188(:,:,:,:)
       real*8,allocatable::S203(:,:,:,:)
       real*8,allocatable::S214(:,:,:,:)
       real*8,allocatable::S30(:,:,:,:)
       real*8,allocatable::S47(:,:,:,:)
       real*8,allocatable::S48(:,:,:,:)
       real*8,allocatable::S120(:,:,:,:)
       real*8,allocatable::S121(:,:,:,:)
       real*8,allocatable::S147(:,:,:,:)
       real*8,allocatable::S148(:,:,:,:)
       real*8,allocatable::S57(:,:,:,:)
       real*8,allocatable::S60(:,:,:,:)
       real*8,allocatable::S61(:,:,:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::Q3(:,:)
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::S17(:,:,:,:)
       real*8,allocatable::S18(:,:,:,:)
       real*8,allocatable::S65(:,:,:,:)
       real*8,allocatable::S13(:,:,:,:)
       real*8,allocatable::S14(:,:,:,:)
       real*8,allocatable::S64(:,:,:,:)
       real*8,allocatable::S11(:,:,:,:)
       real*8,allocatable::S94(:,:,:,:)
       real*8,allocatable::S95(:,:,:,:)
       real*8,allocatable::Q12(:,:)
       real*8,allocatable::S164(:,:,:,:)
       real*8,allocatable::S173(:,:,:,:)
       real*8,allocatable::S178(:,:,:,:)
       real*8,allocatable::S12(:,:,:,:)
       real*8,allocatable::S63(:,:,:,:)
       real*8,allocatable::S70(:,:,:,:)
       real*8,allocatable::S75(:,:,:,:)
       real*8,allocatable::S82(:,:,:,:)
       real*8,allocatable::S78(:,:,:,:)
       real*8,allocatable::S79(:,:,:,:)
       real*8,allocatable::S88(:,:,:,:)
       real*8,allocatable::S89(:,:,:,:)
       real*8,allocatable::S80(:,:,:,:)
       real*8,allocatable::S35(:,:,:,:)
       real*8,allocatable::Q16(:,:)
       real*8,allocatable::S36(:,:,:,:)
       real*8,allocatable::S49(:,:,:,:)
       real*8,allocatable::S114(:,:,:,:)
       real*8,allocatable::S122(:,:,:,:)
       real*8,allocatable::S168(:,:,:,:)
       real*8,allocatable::S184(:,:,:,:)
       real*8,allocatable::S194(:,:,:,:)
       real*8,allocatable::S85(:,:,:,:)
       real*8,allocatable::S91(:,:,:,:)
       real*8,allocatable::S93(:,:,:,:)
       real*8,allocatable::Q9(:,:)
       real*8,allocatable::Q10(:,:)
       real*8,allocatable::S22(:,:,:,:)
       real*8,allocatable::S72(:,:,:,:)
       real*8,allocatable::Q13(:,:)
       real*8,allocatable::S97(:,:,:,:)
       real*8,allocatable::S96(:,:,:,:)
       real*8,allocatable::Q14(:,:)
       real*8,allocatable::Q15(:,:)
       real*8,allocatable::S39(:,:,:,:)
       real*8,allocatable::S40(:,:,:,:)
       real*8,allocatable::S87(:,:,:,:)
       real*8,allocatable::Q18(:,:)
       real*8,allocatable::S98(:,:,:,:)
       real*8,allocatable::Q19(:,:)
       real*8,allocatable::S99(:,:,:,:)
       real*8,allocatable::S1d0(:,:,:,:)
       real*8,allocatable::S101(:,:,:,:)
       real*8,allocatable::S102(:,:,:,:)
       real*8,allocatable::S105(:,:,:,:)
       real*8,allocatable::S106(:,:,:,:)
       real*8,allocatable::S103(:,:,:,:)
       real*8,allocatable::S107(:,:,:,:)
       real*8,allocatable::S112(:,:,:,:)
       real*8,allocatable::S104(:,:,:,:)
       real*8,allocatable::S113(:,:,:,:)
       real*8,allocatable::S108(:,:,:,:)
       real*8,allocatable::Q20(:,:)
       real*8,allocatable::Q25(:,:)
       real*8,allocatable::Q21(:,:)
       real*8,allocatable::S110(:,:,:,:)
       real*8,allocatable::S139(:,:,:,:)
       real*8,allocatable::Q30(:,:)
       real*8,allocatable::S193(:,:,:,:)
       real*8,allocatable::S198(:,:,:,:)
       real*8,allocatable::S204(:,:,:,:)
       real*8,allocatable::S215(:,:,:,:)
       real*8,allocatable::S111(:,:,:,:)
       real*8,allocatable::S43(:,:,:,:)
       real*8,allocatable::S145(:,:,:,:)
       real*8,allocatable::S44(:,:,:,:)
       real*8,allocatable::S124(:,:,:,:)
       real*8,allocatable::Q34(:,:)
       real*8,allocatable::S208(:,:,:,:)
       real*8,allocatable::S210(:,:,:,:)
       real*8,allocatable::S125(:,:,:,:)
       real*8,allocatable::S128(:,:,:,:)
       real*8,allocatable::S129(:,:,:,:)
       real*8,allocatable::S130(:,:,:,:)
       real*8,allocatable::S131(:,:,:,:)
       real*8,allocatable::S134(:,:,:,:)
       real*8,allocatable::S135(:,:,:,:)
       real*8,allocatable::S132(:,:,:,:)
       real*8,allocatable::S136(:,:,:,:)
       real*8,allocatable::S140(:,:,:,:)
       real*8,allocatable::S133(:,:,:,:)
       real*8,allocatable::S141(:,:,:,:)
       real*8,allocatable::S137(:,:,:,:)
       real*8,allocatable::S76(:,:,:,:)
       real*8,allocatable::S84(:,:,:,:)
       real*8,allocatable::S83(:,:,:,:)
       real*8,allocatable::S37(:,:,:,:)
       real*8,allocatable::S86(:,:,:,:)
       real*8,allocatable::S169(:,:,:,:)
       real*8,allocatable::S38(:,:,:,:)
       real*8,allocatable::Q17(:,:)
       real*8,allocatable::S189(:,:,:,:)
       real*8,allocatable::S199(:,:,:,:)
       real*8,allocatable::S142(:,:,:,:)
       real*8,allocatable::S149(:,:,:,:)
       real*8,allocatable::S92(:,:,:,:)
       real*8,allocatable::Q22(:,:)
       real*8,allocatable::Q23(:,:)
       real*8,allocatable::Q26(:,:)
       real*8,allocatable::Q27(:,:)
       real*8,allocatable::Q28(:,:)
       real*8,allocatable::S55(:,:,:,:)
       real*8,allocatable::S115(:,:,:,:)
       real*8,allocatable::S116(:,:,:,:)
       real*8,allocatable::Q31(:,:)
       real*8,allocatable::S155(:,:,:,:)
       real*8,allocatable::S154(:,:,:,:)
       real*8,allocatable::Q32(:,:)
       real*8,allocatable::Q33(:,:)
       real*8,allocatable::S126(:,:,:,:)
       real*8,allocatable::S127(:,:,:,:)
       real*8,allocatable::S153(:,:,:,:)
       real*8,allocatable::Q35(:,:)
       real*8,allocatable::Q36(:,:)
       real*8,allocatable::S156(:,:,:,:)
       real*8,allocatable::S157(:,:,:,:)
       real*8,allocatable::S158(:,:,:,:)
       real*8,allocatable::S23(:,:,:,:)
       real*8,allocatable::S160(:,:,:,:)
       real*8,allocatable::S161(:,:,:,:)
       real*8,allocatable::S162(:,:,:,:)
       real*8,allocatable::S56(:,:,:,:)
       real*8,allocatable::S58(:,:,:,:)
       real*8,allocatable::S59(:,:,:,:)
       real*8,allocatable::S166(:,:,:,:)
       real*8,allocatable::S167(:,:,:,:)
       real*8,allocatable::S123(:,:,:,:)
       real*8,allocatable::S118(:,:,:,:)
       real*8,allocatable::S119(:,:,:,:)
       real*8,allocatable::S146(:,:,:,:)
       real*8,allocatable::S171(:,:,:,:)
       real*8,allocatable::S172(:,:,:,:)
       real*8,allocatable::S74(:,:,:,:)
       real*8,allocatable::S81(:,:,:,:)
       real*8,allocatable::S77(:,:,:,:)
       real*8,allocatable::S174(:,:,:,:)
       real*8,allocatable::S175(:,:,:,:)
       real*8,allocatable::S176(:,:,:,:)
       real*8,allocatable::S54(:,:,:,:)
       real*8,allocatable::Q37(:,:)
       real*8,allocatable::S179(:,:,:,:)
       real*8,allocatable::S180(:,:,:,:)
       real*8,allocatable::S181(:,:,:,:)
       real*8,allocatable::S182(:,:,:,:)
       real*8,allocatable::S66(:,:,:,:)
       real*8,allocatable::S67(:,:,:,:)
       real*8,allocatable::S143(:,:,:,:)
       real*8,allocatable::S144(:,:,:,:)
       real*8,allocatable::S185(:,:,:,:)
       real*8,allocatable::S186(:,:,:,:)
       real*8,allocatable::S187(:,:,:,:)
       real*8,allocatable::S73(:,:,:,:)
       real*8,allocatable::S150(:,:,:,:)
       real*8,allocatable::S190(:,:,:,:)
       real*8,allocatable::S191(:,:,:,:)
       real*8,allocatable::S192(:,:,:,:)
       real*8,allocatable::S21(:,:,:,:)
       real*8,allocatable::S71(:,:,:,:)
       real*8,allocatable::S195(:,:,:,:)
       real*8,allocatable::S196(:,:,:,:)
       real*8,allocatable::S197(:,:,:,:)
       real*8,allocatable::Q6(:,:)
       real*8,allocatable::S151(:,:,:,:)
       real*8,allocatable::S152(:,:,:,:)
       real*8,allocatable::S2d0(:,:,:,:)
       real*8,allocatable::S201(:,:,:,:)
       real*8,allocatable::S202(:,:,:,:)
       real*8,allocatable::S165(:,:,:,:)
       real*8,allocatable::S15(:,:,:,:)
       real*8,allocatable::S16(:,:,:,:)
       real*8,allocatable::S51(:,:,:,:)
       real*8,allocatable::S206(:,:,:,:)
       real*8,allocatable::S207(:,:,:,:)
       real*8,allocatable::S209(:,:,:,:)
       real*8,allocatable::Q24(:,:)
       real*8,allocatable::Q39(:,:)
       real*8,allocatable::Q40(:,:)
       real*8,allocatable::Q41(:,:)
       real*8,allocatable::Q42(:,:)
       real*8,allocatable::S211(:,:,:,:)
       real*8,allocatable::S212(:,:,:,:)
       real*8,allocatable::S213(:,:,:,:)
       real*8,allocatable::Q38(:,:)
       real*8,allocatable::S205(:,:,:,:)
       real*8,allocatable::S216(:,:,:,:)
       real*8,allocatable::Q43(:,:)
       real*8,allocatable::Q44(:,:)
       real*8,allocatable::S217(:,:,:,:)
       real*8,allocatable::S218(:,:,:,:)
       real*8,allocatable::S219(:,:,:,:)
       real*8,allocatable::S220(:,:,:,:)
       real*8,allocatable::S221(:,:,:,:)
       real*8,allocatable::S222(:,:,:,:)
       real*8,allocatable::S223(:,:,:,:)
       real*8,allocatable::S224(:,:,:,:)
       real*8,allocatable::S225(:,:,:,:)
       real*8,allocatable::S226(:,:,:,:)
       real*8,allocatable::S227(:,:,:,:)
       real*8,allocatable::S228(:,:,:,:)
       real*8,allocatable::X1(:,:)
       real*8,allocatable::U1(:,:,:,:,:,:)
       real*8,allocatable::X2(:,:,:,:)
       real*8,allocatable::U2(:,:,:,:,:,:)
       real*8,allocatable::X3(:,:,:,:)
       real*8,allocatable::U3(:,:,:,:,:,:)
       real*8,allocatable::X4(:,:,:,:)
       real*8,allocatable::U4(:,:,:,:,:,:)
       real*8,allocatable::X5(:,:,:,:)
       real*8,allocatable::U6(:,:,:,:,:,:)
       real*8,allocatable::X6(:,:,:,:)
       real*8,allocatable::U5(:,:,:,:,:,:)
       real*8,allocatable::U8(:,:,:,:,:,:)
       real*8,allocatable::U9(:,:,:,:,:,:)
       real*8,allocatable::U10(:,:,:,:,:,:)
       real*8,allocatable::X7(:,:,:,:)
       real*8,allocatable::U16(:,:,:,:,:,:)
       real*8,allocatable::X8(:,:,:,:)
       real*8,allocatable::U17(:,:,:,:,:,:)
       real*8,allocatable::U41(:,:,:,:,:,:)
       real*8,allocatable::X9(:,:,:,:)
       real*8,allocatable::U43(:,:,:,:,:,:)
       real*8,allocatable::X10(:,:,:,:)
       real*8,allocatable::U54(:,:,:,:,:,:)
       real*8,allocatable::X11(:,:,:,:)
       real*8,allocatable::U60(:,:,:,:,:,:)
       real*8,allocatable::X12(:,:,:,:)
       real*8,allocatable::U61(:,:,:,:,:,:)
       real*8,allocatable::X13(:,:)
       real*8,allocatable::U89(:,:,:,:,:,:)
       real*8,allocatable::X14(:,:,:,:)
       real*8,allocatable::U191(:,:,:,:,:,:)
       real*8,allocatable::X15(:,:,:,:)
       real*8,allocatable::U24(:,:,:,:,:,:)
       real*8,allocatable::X16(:,:,:,:)
       real*8,allocatable::U27(:,:,:,:,:,:)
       real*8,allocatable::X17(:,:,:,:)
       real*8,allocatable::U21(:,:,:,:,:,:)
       real*8,allocatable::X18(:,:,:,:)
       real*8,allocatable::U25(:,:,:,:,:,:)
       real*8,allocatable::X19(:,:,:,:)
       real*8,allocatable::U22(:,:,:,:,:,:)
       real*8,allocatable::X20(:,:,:,:)
       real*8,allocatable::U23(:,:,:,:,:,:)
       real*8,allocatable::X21(:,:,:,:)
       real*8,allocatable::U109(:,:,:,:,:,:)
       real*8,allocatable::X22(:,:,:,:)
       real*8,allocatable::U136(:,:,:,:,:,:)
       real*8,allocatable::U62(:,:,:,:,:,:)
       real*8,allocatable::U86(:,:,:,:,:,:)
       real*8,allocatable::X23(:,:)
       real*8,allocatable::U95(:,:,:,:,:,:)
       real*8,allocatable::X24(:,:,:,:)
       real*8,allocatable::U114(:,:,:,:,:,:)
       real*8,allocatable::X25(:,:)
       real*8,allocatable::U94(:,:,:,:,:,:)
       real*8,allocatable::X26(:,:,:,:)
       real*8,allocatable::U122(:,:,:,:,:,:)
       real*8,allocatable::U222(:,:,:,:,:,:)
       real*8,allocatable::U224(:,:,:,:,:,:)
       real*8,allocatable::U164(:,:,:,:,:,:)
       real*8,allocatable::U166(:,:,:,:,:,:)
       real*8,allocatable::U180(:,:,:,:,:,:)
       real*8,allocatable::U184(:,:,:,:,:,:)
       real*8,allocatable::U185(:,:,:,:,:,:)
       real*8,allocatable::U186(:,:,:,:,:,:)
       real*8,allocatable::U187(:,:,:,:,:,:)
       real*8,allocatable::U163(:,:,:,:,:,:)
       real*8,allocatable::U237(:,:,:,:,:,:)
       real*8,allocatable::U240(:,:,:,:,:,:)
       real*8,allocatable::U241(:,:,:,:,:,:)
       real*8,allocatable::U242(:,:,:,:,:,:)
       real*8,allocatable::U243(:,:,:,:,:,:)
       real*8,allocatable::U244(:,:,:,:,:,:)
       real*8,allocatable::U245(:,:,:,:,:,:)
       real*8,allocatable::U246(:,:,:,:,:,:)
       real*8,allocatable::U247(:,:,:,:,:,:)
       real*8,allocatable::U248(:,:,:,:,:,:)
       real*8,allocatable::U249(:,:,:,:,:,:)
       real*8,allocatable::U250(:,:,:,:,:,:)
       real*8,allocatable::U251(:,:,:,:,:,:)
       real*8,allocatable::U258(:,:,:,:,:,:)
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
       allocate(X2(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       X2=0.0d0
       call sul2134(N0,N1,N1,N3,N0,N1,N0,N1,X2,S1, 1.0d0)
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
       allocate(X3(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       X3=0.0d0
       call sul3124(N1,N3,N1,N3,N1,N3,N0,N1,X3,S2, 1.0d0)
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
       allocate(X4(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       X4=0.0d0
       call sul3124(N0,N1,N1,N3,N1,N3,N0,N1,X4,S3, 1.0d0)
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
       allocate(X5(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       X5=0.0d0
       call sul3124(N0,N1,N1,N3,N0,N1,N0,N1,X5,S5, 1.0d0)
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
       call sul2134(N1,N3,N1,N3,N1,N3,N0,N1,X3,S6, 1.0d0)
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
       allocate(X6(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       X6=0.0d0
       call sul4123(N1,N3,N1,N3,N1,N3,N1,N3,X6,S4, 1.0d0)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef3241(N1,N3,N1,N3,N1,N3,N1,N3,
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
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef2341(N0,N1,N1,N3,N1,N3,N1,N3,
     & N1,N3,N1,N3,N1,N3,N0,N1,S7,D1)
       allocate(D2(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,N3,N0,N2,N0,N1,t2B,D2)
       allocate(U8(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1*K2*K4
       I3=K3
       call jungemm(I1,I2,I3,D1,D2,U8)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul145236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U8, 1.0d0)
       call
     & sul145326(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U8,-1.0d0)
       deallocate(U8)
       deallocate(S7)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef2341(N1,N3,N1,N3,N1,N3,N1,N3,
     & N1,N3,N1,N3,N1,N3,N1,N3,S4,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S8(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       I1=K3*K3*K3
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S8)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef2341(N0,N1,N1,N3,N1,N3,N1,N3,
     & N1,N3,N1,N3,N1,N3,N0,N1,S8,D1)
       allocate(D2(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,N3,N0,N2,N0,N1,t2B,D2)
       allocate(U9(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1*K2*K4
       I3=K3
       call jungemm(I1,I2,I3,D1,D2,U9)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul146235(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U9, 1.0d0)
       call
     & sul146325(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U9,-1.0d0)
       deallocate(U9)
       deallocate(S8)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N0,N1,N1,N3,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S9(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S9)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef2341(N0,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,S9,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S50(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S50)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef2314(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,S50,D1)
       allocate(F2(N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef521346(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N1,N3,N2,N3,N1,N3,N0,N2,N0,N1,t3B,F2)
       allocate(U41(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K2*K3*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,F2,U41)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul134526(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U41, 1.0d0)
       call
     & sul134625(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U41,-1.0d0)
       deallocate(U41)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef4231(N0,N1,N0,N1,N0,N1,N1,N3,
     & N1,N3,N0,N1,N0,N1,N0,N1,S9,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S62(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S62)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X10(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       X10=0.0d0
       call sul3124(N0,N1,N0,N1,N0,N1,N0,N1,X10,S62, 1.0d0)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef2431(N0,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,N0,N1,S9,D1)
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
       allocate(X13(N0+1:N1,N0+1:N1))
       X13=0.0d0
       X13=X13+Q11
       deallocate(Q11)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef2341(N0,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,S9,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,r2A,D2)
       allocate(S159(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K3*K3
       I3=K1*K1
       call jungemm(I1,I2,I3,D1,D2,S159)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X8(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       X8=0.0d0
       call sul2314(N1,N3,N1,N3,N1,N3,N0,N1,X8,S159, 0.5d0)
       deallocate(S159)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef3421(N0,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,N0,N1,S9,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,r2A,D2)
       allocate(S163(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S163)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X12(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       X12=0.0d0
       call sul2314(N0,N1,N1,N3,N0,N1,N0,N1,X12,S163,-1.0d0)
       deallocate(S163)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef2431(N0,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,N0,N1,S9,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,r2A,D2)
       allocate(S170(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S170)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X11(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       X11=0.0d0
       call sul2314(N0,N1,N1,N3,N0,N1,N0,N1,X11,S170,-1.0d0)
       deallocate(S170)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef2431(N0,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,N0,N1,S9,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,r2B,D2)
       allocate(S177(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S177)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X14(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       X14=0.0d0
       call sul2314(N0,N1,N2,N3,N0,N2,N0,N1,X14,S177, 1.0d0)
       deallocate(S177)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef3241(N0,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,S9,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S10(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S10)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef2314(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,S10,D1)
       allocate(F2(N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef521346(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N1,N3,N2,N3,N1,N3,N0,N2,N0,N1,t3B,F2)
       allocate(U10(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K2*K3*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,F2,U10)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul124536(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U10, 1.0d0)
       call
     & sul124635(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U10,-1.0d0)
       deallocate(U10)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef3214(N1,N3,N0,N1,N1,N3,N0,N1,
     & N1,N3,N0,N1,N1,N3,N0,N1,S10,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S19(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S19)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X7(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       X7=0.0d0
       call sul3124(N0,N1,N1,N3,N0,N1,N0,N1,X7,S19, 1.0d0)
       deallocate(S19)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef2314(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,S10,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S20(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S20)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N1,N3,N1,N3,N1,N3,N0,N1,X8,S20, 1.0d0)
       deallocate(S20)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef3214(N1,N3,N0,N1,N1,N3,N0,N1,
     & N1,N3,N0,N1,N1,N3,N0,N1,S50,D1)
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
       allocate(X9(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       X9=0.0d0
       call sul3124(N0,N1,N1,N3,N0,N1,N0,N1,X9,S52, 1.0d0)
       deallocate(S52)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef2314(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,S50,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S53(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S53)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N1,N3,N1,N3,N1,N3,N0,N1,X8,S53, 1.0d0)
       deallocate(S53)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef2314(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,S62,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S68(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S68)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N1,N1,N3,N0,N1,N0,N1,X11,S68, 1.0d0)
       deallocate(S68)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef3214(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,S62,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S69(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S69)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N1,N1,N3,N0,N1,N0,N1,X12,S69, 1.0d0)
       deallocate(S69)
C
       allocate(D1(N0+1:N1,N0+1:N2,N1+1:N3,N0+1:N2))
       call relef4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N2,N1,N3,N0,N2,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S24(N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N2))
       I1=K2*K3*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S24)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X17(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       X17=0.0d0
       call sul3124(N0,N2,N1,N3,N1,N3,N0,N2,X17,S24, 1.0d0)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef2314(N1,N3,N0,N2,N1,N3,N0,N2,
     & N0,N2,N1,N3,N1,N3,N0,N2,S24,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S31(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K3
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S31)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X16(N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       X16=0.0d0
       call sul2134(N1,N3,N2,N3,N1,N3,N0,N2,X16,S31, 1.0d0)
       deallocate(S31)
C
       allocate(D1(N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N2))
       call relef3214(N1,N3,N0,N2,N1,N3,N0,N2,
     & N1,N3,N0,N2,N1,N3,N0,N2,S24,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S27(N0+1:N1,N0+1:N2,N1+1:N3,N0+1:N2))
       I1=K2*K3*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S27)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X15(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1))
       X15=0.0d0
       call sul4123(N0,N2,N1,N3,N0,N2,N0,N1,X15,S27, 1.0d0)
       deallocate(S27)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef4123(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N2,N3,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S25(N1+1:N3,N2+1:N3,N1+1:N3,N2+1:N3))
       I1=K4*K3*K4
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S25)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X19(N2+1:N3,N1+1:N3,N2+1:N3,N1+1:N3))
       X19=0.0d0
       call sul4123(N2,N3,N1,N3,N2,N3,N1,N3,X19,S25, 1.0d0)
C
       allocate(D1(N2+1:N3,N1+1:N3,N2+1:N3,N1+1:N3))
       call relef2341(N1,N3,N2,N3,N1,N3,N2,N3,
     & N2,N3,N1,N3,N2,N3,N1,N3,S25,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S32(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:N3))
       I1=K3*K4*K3
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S32)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N1,N3,N2,N3,N1,N3,N0,N2,X16,S32,-1.0d0)
       deallocate(S32)
C
       allocate(D1(N1+1:N3,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef3241(N1,N3,N2,N3,N1,N3,N2,N3,
     & N1,N3,N2,N3,N2,N3,N1,N3,S25,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S28(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       I1=K3*K4*K4
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S28)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X18(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       X18=0.0d0
       call sul4123(N2,N3,N2,N3,N1,N3,N0,N1,X18,S28, 1.0d0)
       deallocate(S28)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,N0+1:N1))
       call relef4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N2,N2,N3,N0,N1,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S26(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S26)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X20(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       X20=0.0d0
       call sul3124(N0,N2,N2,N3,N1,N3,N0,N1,X20,S26, 1.0d0)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef3214(N1,N3,N0,N2,N2,N3,N0,N1,
     & N2,N3,N0,N2,N1,N3,N0,N1,S26,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S33(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S33)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N1,N3,N0,N2,N0,N1,X15,S33, 1.0d0)
       deallocate(S33)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef2314(N1,N3,N0,N2,N2,N3,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,S26,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S34(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S34)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N2,N3,N2,N3,N1,N3,N0,N1,X18,S34,-1.0d0)
       deallocate(S34)
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
       call sul21(N0,N1,N0,N1,X13,Q8,-1.0d0)
       deallocate(Q8)
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
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(S41(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S41)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef4312(N1,N3,N0,N1,N0,N1,N1,N3,
     & N1,N3,N0,N1,N1,N3,N0,N1,S41,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S90(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S90)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N1,N1,N3,N0,N1,N0,N1,X5,S90,-1.0d0)
       deallocate(S90)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3412(N1,N3,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,S41,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S42(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S42)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N1,N3,N1,N3,N1,N3,N0,N1,X3,S42, 1.0d0)
       deallocate(S42)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N0,N2,N0,N1,IntM,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,t2B,D2)
       allocate(S45(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K1*K2
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,D2,S45)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       call relef3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,N1,N0,N2,N0,N1,S45,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S117(N2+1:N3,N0+1:N1,N0+1:N2,N0+1:N1))
       I1=K1*K2*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S117)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N1,N2,N3,N0,N2,N0,N1,X14,S117, 1.0d0)
       deallocate(S117)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:N2,N0+1:N1))
       call relef4312(N0,N2,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,N2,N0,N2,N0,N1,S45,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S46(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S46)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N2,N1,N3,N0,N2,N0,N1,X15,S46, 1.0d0)
       deallocate(S46)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       call relef2341(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N2,N0,N1,N2,N3,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S29(N0+1:N1,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4*K1*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S29)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef2341(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N0,N1,N2,N3,N0,N1,S29,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S109(N2+1:N3,N0+1:N1,N2+1:N3,N0+1:N1))
       I1=K1*K4*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S109)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X21(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       X21=0.0d0
       call sul3124(N0,N1,N2,N3,N2,N3,N0,N1,X21,S109, 1.0d0)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       call relef4231(N0,N1,N0,N2,N0,N1,N2,N3,
     & N2,N3,N0,N2,N0,N1,N0,N1,S29,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S138(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S138)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X22(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       X22=0.0d0
       call sul3124(N0,N2,N0,N1,N0,N2,N0,N1,X22,S138, 1.0d0)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call relef2431(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,N0,N1,S29,D1)
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
       X13=X13-Q29
       deallocate(Q29)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef2341(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N0,N1,N2,N3,N0,N1,S29,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,r2B,D2)
       allocate(S183(N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K3*K4
       I3=K1*K2
       call jungemm(I1,I2,I3,D1,D2,S183)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N2,N3,N2,N3,N1,N3,N0,N1,X18,S183,-1.0d0)
       deallocate(S183)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef3421(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N1,N2,N3,N0,N2,N0,N1,S29,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,r2B,D2)
       allocate(S188(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K2*K3
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,D2,S188)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N0,N2,N1,N3,N0,N2,N0,N1,X15,S188, 1.0d0)
       deallocate(S188)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call relef2431(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,N0,N1,S29,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,r2B,D2)
       allocate(S203(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S203)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N0,N1,N1,N3,N0,N1,N0,N1,X5,S203, 1.0d0)
       deallocate(S203)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call relef2431(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,N0,N1,S29,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,r2C,D2)
       allocate(S214(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S214)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N0,N1,N2,N3,N0,N2,N0,N1,X14,S214,-1.0d0)
       deallocate(S214)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,N0+1:N1))
       call relef3241(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N1,N0,N2,N2,N3,N0,N1,S29,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S30(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S30)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N2,N3,N1,N3,N0,N1,X20,S30, 1.0d0)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef3214(N1,N3,N0,N2,N2,N3,N0,N1,
     & N2,N3,N0,N2,N1,N3,N0,N1,S30,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S47(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S47)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N1,N3,N0,N2,N0,N1,X15,S47, 1.0d0)
       deallocate(S47)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef2314(N1,N3,N0,N2,N2,N3,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,S30,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S48(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S48)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N2,N3,N2,N3,N1,N3,N0,N1,X18,S48,-1.0d0)
       deallocate(S48)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       call relef2314(N2,N3,N0,N1,N2,N3,N0,N1,
     & N0,N1,N2,N3,N2,N3,N0,N1,S109,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S120(N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4*K4
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S120)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N2,N3,N2,N3,N1,N3,N0,N1,X18,S120,-1.0d0)
       deallocate(S120)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef3214(N2,N3,N0,N1,N2,N3,N0,N1,
     & N2,N3,N0,N1,N2,N3,N0,N1,S109,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S121(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       I1=K1*K4*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S121)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N1,N2,N3,N0,N2,N0,N1,X14,S121, 1.0d0)
       deallocate(S121)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:N2,N0+1:N1))
       call relef3214(N0,N2,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N2,N0,N2,N0,N1,S138,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S147(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S147)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N2,N1,N3,N0,N2,N0,N1,X15,S147, 1.0d0)
       deallocate(S147)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       call relef2314(N0,N2,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N2,N0,N1,S138,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S148(N2+1:N3,N0+1:N1,N0+1:N2,N0+1:N1))
       I1=K1*K2*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S148)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N1,N2,N3,N0,N2,N0,N1,X14,S148, 1.0d0)
       deallocate(S148)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S57(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S57)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N1,N1,N3,N1,N3,N0,N1,X4,S57, 1.0d0)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef3241(N0,N1,N0,N1,N1,N3,N1,N3,
     & N1,N3,N0,N1,N1,N3,N0,N1,S57,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S60(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S60)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N1,N1,N3,N0,N1,N0,N1,X5,S60,-1.0d0)
       deallocate(S60)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef2341(N0,N1,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,S57,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S61(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S61)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N1,N3,N1,N3,N1,N3,N0,N1,X3,S61,-1.0d0)
       deallocate(S61)
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
       call sul21(N0,N1,N0,N1,X13,Q7,-1.0d0)
       deallocate(Q7)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S17(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S17)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3412(N1,N3,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,S17,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S18(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S18)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N1,N3,N1,N3,N1,N3,N0,N1,X3,S18,-1.0d0)
       deallocate(S18)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef4312(N1,N3,N0,N1,N0,N1,N1,N3,
     & N1,N3,N0,N1,N1,N3,N0,N1,S17,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S65(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S65)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N1,N1,N3,N0,N1,N0,N1,X5,S65, 1.0d0)
       deallocate(S65)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
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
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef3412(N2,N3,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,S13,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S14(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S14)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N1,N3,N2,N3,N1,N3,N0,N2,X16,S14,-1.0d0)
       deallocate(S14)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef4312(N2,N3,N0,N2,N0,N1,N1,N3,
     & N1,N3,N0,N1,N2,N3,N0,N2,S13,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S64(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S64)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N1,N2,N3,N0,N2,N0,N1,X14,S64,-1.0d0)
       deallocate(S64)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S11(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S11)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef4231(N1,N3,N0,N1,N1,N3,N1,N3,
     & N1,N3,N0,N1,N1,N3,N1,N3,S11,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S94(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S94)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef2341(N0,N1,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,S94,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S95(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S95)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef2134(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N1,N3,N1,N3,N0,N1,S95,D1)
       allocate(D2(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,N3,N0,N2,N0,N1,t2B,D2)
       allocate(U86(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1*K2*K4
       I3=K3
       call jungemm(I1,I2,I3,D1,D2,U86)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul145236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U86,-1.0d0)
       deallocate(U86)
       deallocate(S95)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef2341(N1,N3,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,S11,D1)
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
       X1=X1-Q12
       deallocate(Q12)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef2431(N1,N3,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,S11,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,r2A,D2)
       allocate(S164(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S164)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N1,N3,N1,N3,N1,N3,N0,N1,X3,S164,-1.0d0)
       deallocate(S164)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef3421(N1,N3,N0,N1,N1,N3,N1,N3,
     & N1,N3,N1,N3,N0,N1,N1,N3,S11,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,r2A,D2)
       allocate(S173(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K1*K1
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,D2,S173)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N1,N1,N3,N0,N1,N0,N1,X2,S173,-0.5d0)
       deallocate(S173)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef2341(N1,N3,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,S11,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,r2B,D2)
       allocate(S178(N2+1:N3,N0+1:N2,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S178)
       deallocate(D1)
       deallocate(D2)
C
       call sul2413(N1,N3,N2,N3,N1,N3,N0,N2,X16,S178, 1.0d0)
       deallocate(S178)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef2341(N1,N3,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,S11,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S12(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       I1=K3*K3*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S12)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N1,N3,N1,N3,N1,N3,N1,N3,X6,S12, 1.0d0)
       deallocate(S12)
C
       allocate(F2(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       call relef231456(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N1,N3,N1,N3,N2,N3,N0,N2,N0,N1,N0,N1,t3B,F2)
       allocate(U5(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K1*K1*K2*K4
       I3=K3*K3
       call jungemm(I1,I2,I3,X6,F2,U5)
       deallocate(F2)
C
       call
     & sul145623(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U5, 0.5d0)
       call
     & sul145632(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U5,-0.5d0)
       deallocate(U5)
       deallocate(X6)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3241(N1,N3,N0,N1,N1,N3,N1,N3,
     & N1,N3,N0,N1,N1,N3,N1,N3,S11,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S63(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S63)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N1,N1,N3,N1,N3,N0,N1,X4,S63, 1.0d0)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef2341(N0,N1,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,S63,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S70(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S70)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef2134(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N1,N3,N1,N3,N0,N1,S70,D1)
       allocate(D2(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,N3,N0,N2,N0,N1,t2B,D2)
       allocate(U62(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1*K2*K4
       I3=K3
       call jungemm(I1,I2,I3,D1,D2,U62)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul146235(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U62,-1.0d0)
       deallocate(U62)
       deallocate(S70)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:N3,N2+1:N3))
       call relef2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N2,N3,N2,N3,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S75(N0+1:N1,N0+1:N1,N2+1:N3,N2+1:N3))
       I1=K4*K4*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S75)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N1,N2,N3,N2,N3,N0,N1,X21,S75,-1.0d0)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef3241(N0,N1,N0,N1,N2,N3,N2,N3,
     & N2,N3,N0,N1,N2,N3,N0,N1,S75,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S82(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       I1=K1*K4*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S82)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N1,N2,N3,N0,N2,N0,N1,X14,S82,-1.0d0)
       deallocate(S82)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       call relef2341(N0,N1,N0,N1,N2,N3,N2,N3,
     & N0,N1,N2,N3,N2,N3,N0,N1,S75,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S78(N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4*K4
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S78)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N2,N3,N2,N3,N1,N3,N0,N1,X18,S78, 1.0d0)
       deallocate(S78)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       call relef2341(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N2,N0,N1,N2,N3,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S79(N0+1:N1,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4*K1*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S79)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef2341(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N0,N1,N2,N3,N0,N1,S79,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2B,D2)
       allocate(S88(N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K3*K4
       I3=K1*K2
       call jungemm(I1,I2,I3,D1,D2,S88)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N2,N3,N2,N3,N1,N3,N0,N1,X18,S88,-1.0d0)
       deallocate(S88)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef3421(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N1,N2,N3,N0,N2,N0,N1,S79,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(S89(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K2*K3
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,D2,S89)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N0,N2,N1,N3,N0,N2,N0,N1,X15,S89, 1.0d0)
       deallocate(S89)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,N0+1:N1))
       call relef3241(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N1,N0,N2,N2,N3,N0,N1,S79,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S80(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S80)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N2,N3,N1,N3,N0,N1,X20,S80, 1.0d0)
       deallocate(S80)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N0,N1,N1,N3,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S35(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S35)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call relef3421(N0,N2,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N2,N0,N2,S35,D1)
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
       allocate(X23(N0+1:N2,N0+1:N2))
       X23=0.0d0
       X23=X23+Q16
       deallocate(Q16)
C
       allocate(D1(N0+1:N1,N0+1:N2,N1+1:N3,N0+1:N2))
       call relef3241(N0,N2,N0,N2,N0,N1,N1,N3,
     & N0,N1,N0,N2,N1,N3,N0,N2,S35,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S36(N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N2))
       I1=K2*K3*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S36)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N1,N3,N1,N3,N0,N2,X17,S36, 1.0d0)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef2314(N1,N3,N0,N2,N1,N3,N0,N2,
     & N0,N2,N1,N3,N1,N3,N0,N2,S36,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S49(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K3
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S49)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N1,N3,N2,N3,N1,N3,N0,N2,X16,S49, 1.0d0)
       deallocate(S49)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N2,N0,N1,N1,N3,
     & N0,N2,N0,N1,N1,N3,N0,N2,S35,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S114(N2+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S114)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X24(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       X24=0.0d0
       call sul3124(N0,N1,N1,N3,N2,N3,N0,N2,X24,S114, 1.0d0)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef2314(N2,N3,N0,N1,N1,N3,N0,N2,
     & N0,N1,N1,N3,N2,N3,N0,N2,S114,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S122(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S122)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N1,N3,N2,N3,N1,N3,N0,N2,X16,S122, 1.0d0)
       deallocate(S122)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call relef3421(N0,N2,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N2,N0,N2,S35,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,r2A,D2)
       allocate(S168(N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S168)
       deallocate(D1)
       deallocate(D2)
C
       call sul2413(N0,N2,N1,N3,N0,N2,N0,N1,X15,S168,-1.0d0)
       deallocate(S168)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N2,N0,N1,N1,N3,
     & N0,N2,N0,N1,N1,N3,N0,N2,S35,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,r2B,D2)
       allocate(S184(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3
       I2=K3*K4
       I3=K1*K2
       call jungemm(I1,I2,I3,D1,D2,S184)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N1,N3,N2,N3,N1,N3,N0,N2,X16,S184, 1.0d0)
       deallocate(S184)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,N0+1:N2))
       call relef2431(N0,N2,N0,N2,N0,N1,N1,N3,
     & N0,N2,N1,N3,N0,N1,N0,N2,S35,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
       call relef3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,N3,N0,N1,r2B,D2)
       allocate(S194(N2+1:N3,N0+1:N1,N0+1:N1,N0+1:N2))
       I1=K2*K1
       I2=K1*K4
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,D2,S194)
       deallocate(D1)
       deallocate(D2)
C
       call sul2413(N0,N1,N2,N3,N0,N2,N0,N1,X14,S194, 1.0d0)
       deallocate(S194)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
       call relef4231(N0,N2,N0,N2,N0,N1,N1,N3,
     & N1,N3,N0,N2,N0,N1,N0,N2,S35,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S85(N0+1:N1,N0+1:N2,N0+1:N1,N0+1:N2))
       I1=K2*K1*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S85)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N2,N0,N1,N0,N2,N0,N1,X22,S85, 1.0d0)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:N2,N0+1:N1))
       call relef3241(N0,N1,N0,N2,N0,N1,N0,N2,
     & N0,N1,N0,N2,N0,N2,N0,N1,S85,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S91(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S91)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N2,N1,N3,N0,N2,N0,N1,X15,S91, 1.0d0)
       deallocate(S91)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       call relef2341(N0,N1,N0,N2,N0,N1,N0,N2,
     & N0,N2,N0,N1,N0,N2,N0,N1,S85,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S93(N2+1:N3,N0+1:N1,N0+1:N2,N0+1:N1))
       I1=K1*K2*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S93)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N1,N2,N3,N0,N2,N0,N1,X14,S93, 1.0d0)
       deallocate(S93)
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
       X13=X13-Q9
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
       X1=X1-Q10
       deallocate(Q10)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:N2,N0+1:N1))
       call relef4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N2,N0,N2,N0,N1,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S22(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S22)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N2,N1,N3,N0,N2,N0,N1,X15,S22, 1.0d0)
       deallocate(S22)
C
       allocate(D1(N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N2))
       call relef2341(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N2,N1,N3,N0,N2,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S72(N0+1:N1,N0+1:N2,N1+1:N3,N0+1:N2))
       I1=K2*K3*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S72)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N2,N1,N3,N0,N2,N0,N1,X15,S72,-1.0d0)
       deallocate(S72)
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
       allocate(D2(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,N3,N0,N2,N0,N1,t2B,D2)
       allocate(S97(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K2*K4
       I3=K3
       call jungemm(I1,I2,I3,B1,D2,S97)
       deallocate(B1)
       deallocate(D2)
C
       call sul2341(N0,N1,N2,N3,N0,N2,N0,N1,X14,S97,-1.0d0)
       deallocate(S97)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call relef21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q13,B1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(S96(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K1*K3
       I3=K3
       call jungemm(I1,I2,I3,B1,D2,S96)
       deallocate(B1)
       deallocate(D2)
C
       call sul2341(N0,N1,N1,N3,N0,N1,N0,N1,X2,S96,-1.0d0)
       deallocate(S96)
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
       X23=X23+Q14
       deallocate(Q14)
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
       allocate(X25(N2+1:N3,N2+1:N3))
       X25=0.0d0
       X25=X25+Q15
       deallocate(Q15)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S39(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S39)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef3412(N2,N3,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,S39,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S40(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S40)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N1,N3,N2,N3,N1,N3,N0,N2,X16,S40,-1.0d0)
       deallocate(S40)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef4312(N2,N3,N0,N2,N0,N1,N1,N3,
     & N1,N3,N0,N1,N2,N3,N0,N2,S39,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S87(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S87)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N1,N2,N3,N0,N2,N0,N1,X14,S87,-1.0d0)
       deallocate(S87)
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
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,t2B,D2)
       allocate(S98(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
       I1=K2
       I2=K1*K2*K3
       I3=K4
       call jungemm(I1,I2,I3,B1,D2,S98)
       deallocate(B1)
       deallocate(D2)
C
       call sul2341(N0,N2,N1,N3,N0,N2,N0,N1,X15,S98,-1.0d0)
       deallocate(S98)
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
       call sul21(N2,N3,N2,N3,X25,Q19,-1.0d0)
       deallocate(Q19)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N0,N2,N0,N1,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S99(N2+1:N3,N0+1:N1,N0+1:N2,N0+1:N1))
       I1=K1*K2*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S99)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N1,N2,N3,N0,N2,N0,N1,X14,S99, 1.0d0)
       deallocate(S99)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N1,N3,N0,N2,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S1d0(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K3
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S1d0)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N1,N3,N2,N3,N1,N3,N0,N2,X16,S1d0,-1.0d0)
       deallocate(S1d0)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S101(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S101)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N2,N3,N2,N3,N1,N3,N0,N1,X18,S101, 1.0d0)
       deallocate(S101)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N0,N2,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S102(N2+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S102)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N1,N1,N3,N2,N3,N0,N2,X24,S102, 1.0d0)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef3214(N2,N3,N0,N1,N1,N3,N0,N2,
     & N1,N3,N0,N1,N2,N3,N0,N2,S102,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S105(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S105)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N1,N2,N3,N0,N2,N0,N1,X14,S105, 1.0d0)
       deallocate(S105)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef2314(N2,N3,N0,N1,N1,N3,N0,N2,
     & N0,N1,N1,N3,N2,N3,N0,N2,S102,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S106(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S106)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N1,N3,N2,N3,N1,N3,N0,N2,X16,S106, 1.0d0)
       deallocate(S106)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N0,N1,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S103(N2+1:N3,N0+1:N1,N2+1:N3,N0+1:N1))
       I1=K1*K4*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S103)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N1,N2,N3,N2,N3,N0,N1,X21,S103, 1.0d0)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       call relef2314(N2,N3,N0,N1,N2,N3,N0,N1,
     & N0,N1,N2,N3,N2,N3,N0,N1,S103,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S107(N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4*K4
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S107)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N2,N3,N2,N3,N1,N3,N0,N1,X18,S107,-1.0d0)
       deallocate(S107)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef3214(N2,N3,N0,N1,N2,N3,N0,N1,
     & N2,N3,N0,N1,N2,N3,N0,N1,S103,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S112(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       I1=K1*K4*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S112)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N1,N2,N3,N0,N2,N0,N1,X14,S112, 1.0d0)
       deallocate(S112)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S104(N2+1:N3,N2+1:N3,N1+1:N3,N1+1:N3))
       I1=K3*K3*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S104)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N2,N3,N1,N3,N2,N3,N1,N3,X19,S104, 1.0d0)
C
       allocate(D1(N2+1:N3,N1+1:N3,N2+1:N3,N1+1:N3))
       call relef2314(N2,N3,N2,N3,N1,N3,N1,N3,
     & N2,N3,N1,N3,N2,N3,N1,N3,S104,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S113(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:N3))
       I1=K3*K4*K3
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S113)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N1,N3,N2,N3,N1,N3,N0,N2,X16,S113,-1.0d0)
       deallocate(S113)
C
       allocate(D1(N1+1:N3,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef3214(N2,N3,N2,N3,N1,N3,N1,N3,
     & N1,N3,N2,N3,N2,N3,N1,N3,S104,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S108(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       I1=K3*K4*K4
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S108)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N2,N3,N2,N3,N1,N3,N0,N1,X18,S108, 1.0d0)
       deallocate(S108)
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
       call sul21(N0,N2,N0,N2,X23,Q25, 1.0d0)
       deallocate(Q25)
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
       call sul21(N2,N3,N2,N3,X25,Q21,-1.0d0)
       deallocate(Q21)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,N1+1:N3))
       call relef4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N2,N2,N3,N1,N3,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S110(N1+1:N3,N0+1:N2,N2+1:N3,N1+1:N3))
       I1=K3*K4*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S110)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N1+1:N3))
       call relef3241(N1,N3,N0,N2,N2,N3,N1,N3,
     & N2,N3,N0,N2,N1,N3,N1,N3,S110,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S139(N0+1:N2,N0+1:N2,N1+1:N3,N1+1:N3))
       I1=K3*K3*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S139)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N2,N1,N3,N1,N3,N0,N2,X17,S139, 1.0d0)
       deallocate(S139)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef2341(N1,N3,N0,N2,N2,N3,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,S110,D1)
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
     & N0,N2,N1,N3,N2,N3,N1,N3,S110,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
       call relef3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,N3,N0,N1,r2B,D2)
       allocate(S193(N2+1:N3,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K1*K4
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,D2,S193)
       deallocate(D1)
       deallocate(D2)
C
       call sul2413(N2,N3,N2,N3,N1,N3,N0,N1,X18,S193,-1.0d0)
       deallocate(S193)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3))
       call relef3421(N1,N3,N0,N2,N2,N3,N1,N3,
     & N2,N3,N1,N3,N0,N2,N1,N3,S110,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,r2B,D2)
       allocate(S198(N0+1:N2,N0+1:N1,N0+1:N2,N1+1:N3))
       I1=K3*K2
       I2=K1*K2
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,D2,S198)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N2,N1,N3,N0,N2,N0,N1,X15,S198, 1.0d0)
       deallocate(S198)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef2341(N1,N3,N0,N2,N2,N3,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,S110,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,r2B,D2)
       allocate(S204(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S204)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N1,N3,N1,N3,N1,N3,N0,N1,X3,S204,-1.0d0)
       deallocate(S204)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef2341(N1,N3,N0,N2,N2,N3,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,S110,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,r2C,D2)
       allocate(S215(N2+1:N3,N0+1:N2,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S215)
       deallocate(D1)
       deallocate(D2)
C
       call sul2413(N1,N3,N2,N3,N1,N3,N0,N2,X16,S215,-1.0d0)
       deallocate(S215)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef2341(N1,N3,N0,N2,N2,N3,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,S110,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S111(N2+1:N3,N2+1:N3,N1+1:N3,N1+1:N3))
       I1=K3*K3*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S111)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N2,N3,N1,N3,N2,N3,N1,N3,X19,S111,-1.0d0)
       deallocate(S111)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,N2+1:N3))
       call relef3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N0,N1,N2,N3,IntM,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
       call relef3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,N3,N0,N1,t2B,D2)
       allocate(S43(N2+1:N3,N0+1:N1,N0+1:N1,N2+1:N3))
       I1=K4*K1
       I2=K1*K4
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,D2,S43)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef4312(N2,N3,N0,N1,N0,N1,N2,N3,
     & N2,N3,N0,N1,N2,N3,N0,N1,S43,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S145(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       I1=K1*K4*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S145)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N1,N2,N3,N0,N2,N0,N1,X14,S145, 1.0d0)
       deallocate(S145)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       call relef3412(N2,N3,N0,N1,N0,N1,N2,N3,
     & N0,N1,N2,N3,N2,N3,N0,N1,S43,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S44(N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4*K4
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S44)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N2,N3,N2,N3,N1,N3,N0,N1,X18,S44,-1.0d0)
       deallocate(S44)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N0,N2,N2,N3,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S124(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S124)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef2431(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,S124,D1)
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
       X23=X23-Q34
       deallocate(Q34)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef3421(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,S124,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,r2B,D2)
       allocate(S208(N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S208)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef3142(N1,N3,N0,N1,N0,N2,N0,N2,
     & N0,N2,N1,N3,N0,N2,N0,N1,S208,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(U222(N2+1:N3,N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K3
       I2=K1*K3*K4
       I3=K2
       call jungemm(I1,I2,I3,D1,D2,U222)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul126345(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U222, 1.0d0)
       call
     & sul125346(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U222,-1.0d0)
       deallocate(U222)
       deallocate(S208)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef2431(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,S124,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,r2B,D2)
       allocate(S210(N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S210)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef3142(N1,N3,N0,N1,N0,N2,N0,N2,
     & N0,N2,N1,N3,N0,N2,N0,N1,S210,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(U224(N2+1:N3,N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K3
       I2=K1*K3*K4
       I3=K2
       call jungemm(I1,I2,I3,D1,D2,U224)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul136245(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U224, 1.0d0)
       call
     & sul135246(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U224,-1.0d0)
       deallocate(U224)
       deallocate(S210)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,S124,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S125(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S125)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X26(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       X26=0.0d0
       call sul3124(N0,N2,N2,N3,N2,N3,N0,N2,X26,S125, 1.0d0)
       deallocate(S125)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef1432(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N1,N2,N3,N0,N1,IntM,D1)
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
       call sul3124(N0,N1,N2,N3,N0,N2,N0,N1,X14,S128,-1.0d0)
       deallocate(S128)
C
       allocate(D1(N2+1:N3,N1+1:N3,N2+1:N3,N1+1:N3))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N2,N3,N1,N3,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S129(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:N3))
       I1=K3*K4*K3
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S129)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N1,N3,N2,N3,N1,N3,N0,N2,X16,S129, 1.0d0)
       deallocate(S129)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N1,N3,N0,N1,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S130(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S130)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N1,N3,N0,N2,N0,N1,X15,S130,-1.0d0)
       deallocate(S130)
C
       allocate(D1(N2+1:N3,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef1423(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S131(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       I1=K4*K3*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S131)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N1,N1,N3,N2,N3,N0,N2,X24,S131,-1.0d0)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef3241(N0,N2,N0,N1,N1,N3,N2,N3,
     & N1,N3,N0,N1,N2,N3,N0,N2,S131,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S134(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S134)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N1,N2,N3,N0,N2,N0,N1,X14,S134,-1.0d0)
       deallocate(S134)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N1,N1,N3,N2,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,S131,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S135(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S135)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N1,N3,N2,N3,N1,N3,N0,N2,X16,S135,-1.0d0)
       deallocate(S135)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N0,N1,N0,N1,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S132(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S132)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N0,N1,N0,N2,N0,N1,X22,S132, 1.0d0)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:N2,N0+1:N1))
       call relef3214(N0,N2,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N2,N0,N2,N0,N1,S132,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S136(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S136)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N2,N1,N3,N0,N2,N0,N1,X15,S136, 1.0d0)
       deallocate(S136)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       call relef2314(N0,N2,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N2,N0,N1,S132,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S140(N2+1:N3,N0+1:N1,N0+1:N2,N0+1:N1))
       I1=K1*K2*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S140)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N1,N2,N3,N0,N2,N0,N1,X14,S140, 1.0d0)
       deallocate(S140)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N1+1:N3))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N1,N3,N1,N3,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S133(N0+1:N2,N0+1:N2,N1+1:N3,N1+1:N3))
       I1=K3*K3*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S133)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N2,N1,N3,N1,N3,N0,N2,X17,S133,-1.0d0)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N2,N1,N3,N1,N3,
     & N0,N2,N1,N3,N1,N3,N0,N2,S133,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S141(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K3
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S141)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N1,N3,N2,N3,N1,N3,N0,N2,X16,S141,-1.0d0)
       deallocate(S141)
C
       allocate(D1(N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N2))
       call relef3241(N0,N2,N0,N2,N1,N3,N1,N3,
     & N1,N3,N0,N2,N1,N3,N0,N2,S133,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S137(N0+1:N1,N0+1:N2,N1+1:N3,N0+1:N2))
       I1=K2*K3*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S137)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N2,N1,N3,N0,N2,N0,N1,X15,S137,-1.0d0)
       deallocate(S137)
C
       allocate(D1(N1+1:N3,N0+1:N2,N2+1:N3,N1+1:N3))
       call relef2314(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N2,N2,N3,N1,N3,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S76(N0+1:N1,N0+1:N2,N2+1:N3,N1+1:N3))
       I1=K3*K4*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S76)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N2,N2,N3,N1,N3,N0,N1,X20,S76,-1.0d0)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef2341(N0,N1,N0,N2,N2,N3,N1,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,S76,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S84(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S84)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N2,N3,N2,N3,N1,N3,N0,N1,X18,S84, 1.0d0)
       deallocate(S84)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef3241(N0,N1,N0,N2,N2,N3,N1,N3,
     & N2,N3,N0,N2,N1,N3,N0,N1,S76,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S83(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S83)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N1,N3,N0,N2,N0,N1,X15,S83,-1.0d0)
       deallocate(S83)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S37(N2+1:N3,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S37)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:N3,N2+1:N3))
       call relef4231(N2,N3,N0,N1,N2,N3,N1,N3,
     & N1,N3,N0,N1,N2,N3,N2,N3,S37,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S86(N0+1:N1,N0+1:N1,N2+1:N3,N2+1:N3))
       I1=K4*K4*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S86)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N1,N2,N3,N2,N3,N0,N1,X21,S86, 1.0d0)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3))
       call relef2431(N2,N3,N0,N1,N2,N3,N1,N3,
     & N0,N1,N1,N3,N2,N3,N2,N3,S37,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,r2A,D2)
       allocate(S169(N1+1:N3,N0+1:N1,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S169)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N2,N3,N2,N3,N1,N3,N0,N1,X18,S169, 1.0d0)
       deallocate(S169)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef2341(N2,N3,N0,N1,N2,N3,N1,N3,
     & N0,N1,N2,N3,N1,N3,N2,N3,S37,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S38(N1+1:N3,N2+1:N3,N1+1:N3,N2+1:N3))
       I1=K4*K3*K4
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S38)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N2,N3,N1,N3,N2,N3,N1,N3,X19,S38,-1.0d0)
       deallocate(S38)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3))
       call relef2431(N2,N3,N0,N1,N2,N3,N1,N3,
     & N0,N1,N1,N3,N2,N3,N2,N3,S37,D1)
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
       X25=X25-Q17
       deallocate(Q17)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef2341(N2,N3,N0,N1,N2,N3,N1,N3,
     & N0,N1,N2,N3,N1,N3,N2,N3,S37,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,r2B,D2)
       allocate(S189(N1+1:N3,N0+1:N2,N1+1:N3,N2+1:N3))
       I1=K4*K3
       I2=K2*K3
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,D2,S189)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N1,N3,N2,N3,N1,N3,N0,N2,X16,S189, 1.0d0)
       deallocate(S189)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N1,N2+1:N3))
       call relef3421(N2,N3,N0,N1,N2,N3,N1,N3,
     & N2,N3,N1,N3,N0,N1,N2,N3,S37,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,r2B,D2)
       allocate(S199(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3))
       I1=K4*K1
       I2=K1*K2
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,D2,S199)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N1,N2,N3,N0,N2,N0,N1,X14,S199, 1.0d0)
       deallocate(S199)
C
       allocate(D1(N2+1:N3,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3241(N2,N3,N0,N1,N2,N3,N1,N3,
     & N2,N3,N0,N1,N1,N3,N2,N3,S37,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S142(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       I1=K4*K3*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S142)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N1,N1,N3,N2,N3,N0,N2,X24,S142, 1.0d0)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N1,N1,N3,N2,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,S142,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S149(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S149)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N1,N3,N2,N3,N1,N3,N0,N2,X16,S149, 1.0d0)
       deallocate(S149)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       call relef2341(N0,N1,N0,N1,N2,N3,N2,N3,
     & N0,N1,N2,N3,N2,N3,N0,N1,S86,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S92(N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4*K4
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S92)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N2,N3,N2,N3,N1,N3,N0,N1,X18,S92,-1.0d0)
       deallocate(S92)
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
       call sul21(N2,N3,N2,N3,X25,Q23,-1.0d0)
       deallocate(Q23)
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
       call sul21(N0,N2,N0,N2,X23,Q26, 1.0d0)
       deallocate(Q26)
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
       X13=X13-Q27
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
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N1,N3,N1,N3,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S55(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       I1=K3*K3*K3
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S55)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N1,N3,N1,N3,N1,N3,N0,N1,X8,S55, 1.0d0)
       deallocate(S55)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N1+1:N3))
       call relef4132(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N0,N2,N1,N3,IntM,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(S115(N1+1:N3,N0+1:N2,N0+1:N2,N1+1:N3))
       I1=K3*K2
       I2=K2*K3
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,D2,S115)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef3412(N1,N3,N0,N2,N0,N2,N1,N3,
     & N0,N2,N1,N3,N1,N3,N0,N2,S115,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S116(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K3
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S116)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N1,N3,N2,N3,N1,N3,N0,N2,X16,S116, 1.0d0)
       deallocate(S116)
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
       allocate(S155(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K2*K4
       I3=K3
       call jungemm(I1,I2,I3,B1,D2,S155)
       deallocate(B1)
       deallocate(D2)
C
       call sul2341(N0,N1,N2,N3,N0,N2,N0,N1,X14,S155,-1.0d0)
       deallocate(S155)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call relef21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q31,B1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(S154(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K1*K3
       I3=K3
       call jungemm(I1,I2,I3,B1,D2,S154)
       deallocate(B1)
       deallocate(D2)
C
       call sul2341(N0,N1,N1,N3,N0,N1,N0,N1,X2,S154,-1.0d0)
       deallocate(S154)
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
       X23=X23+Q32
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
       X25=X25+Q33
       deallocate(Q33)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(S126(N1+1:N3,N0+1:N1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S126)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3412(N1,N3,N0,N1,N0,N2,N2,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,S126,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S127(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S127)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N2,N3,N2,N3,N1,N3,N0,N1,X18,S127, 1.0d0)
       deallocate(S127)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef4312(N1,N3,N0,N1,N0,N2,N2,N3,
     & N2,N3,N0,N2,N1,N3,N0,N1,S126,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S153(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S153)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N1,N3,N0,N2,N0,N1,X15,S153,-1.0d0)
       deallocate(S153)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(Q35(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q35)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q36(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,Q35,B2,Q36)
       deallocate(B2)
C
       call sul21(N2,N3,N2,N3,X25,Q36,-1.0d0)
       deallocate(Q36)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call relef21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q35,B1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,t2B,D2)
       allocate(S156(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
       I1=K2
       I2=K1*K2*K3
       I3=K4
       call jungemm(I1,I2,I3,B1,D2,S156)
       deallocate(B1)
       deallocate(D2)
C
       call sul2341(N0,N2,N1,N3,N0,N2,N0,N1,X15,S156,-1.0d0)
       deallocate(S156)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,r2A,D2)
       allocate(S157(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K3*K3
       I3=K1*K1
       call jungemm(I1,I2,I3,D1,D2,S157)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N1,N3,N1,N3,N1,N3,N0,N1,X8,S157,-0.5d0)
       deallocate(S157)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,r2A,D2)
       allocate(S158(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K3*K3
       I3=K1*K1
       call jungemm(I1,I2,I3,D1,D2,S158)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N1,N3,N1,N3,
     & N1,N3,N1,N3,N1,N3,N1,N3,S158,D1)
       allocate(F2(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       call relef231456(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N1,N3,N1,N3,N2,N3,N0,N2,N0,N1,N0,N1,t3B,F2)
       allocate(U166(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K1*K1*K2*K4
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,F2,U166)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul145623(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U166, 0.250)
       deallocate(U166)
       deallocate(S158)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       call relef4132(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N2,N3,N0,N1,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S23(N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4*K4
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S23)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N2,N3,N2,N3,N1,N3,N0,N1,X18,S23, 1.0d0)
       deallocate(S23)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef4132(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N0,N1,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,r2A,D2)
       allocate(S160(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S160)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N0,N1,N1,N3,N0,N1,N0,N1,X5,S160,-1.0d0)
       deallocate(S160)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,r2A,D2)
       allocate(S161(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S161)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N1,N3,N1,N3,N1,N3,N0,N1,X3,S161,-1.0d0)
       deallocate(S161)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,r2A,D2)
       allocate(S162(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S162)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N1,N1,N3,N1,N3,N0,N1,X4,S162,-1.0d0)
       deallocate(S162)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N0,N1,N0,N1,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S56(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S56)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N1,N0,N1,N0,N1,N0,N1,X10,S56,-1.0d0)
C
       allocate(F2(N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef561234(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,t3B,F2)
       allocate(U54(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K2*K3*K3*K4
       I3=K1*K1
       call jungemm(I1,I2,I3,X10,F2,U54)
       deallocate(F2)
C
       V3B=V3B-0.5d0*U54
       call
     & sul123465(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U54, 0.5d0)
       deallocate(U54)
       deallocate(X10)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef2314(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,S56,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S58(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S58)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N1,N1,N3,N0,N1,N0,N1,X11,S58,-1.0d0)
       deallocate(S58)
C
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(U60(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K2*K3*K4
       I3=K1
       call jungemm(I1,I2,I3,X11,D2,U60)
       deallocate(D2)
C
       call
     & sul134256(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U60,-1.0d0)
       call
     & sul134265(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U60, 1.0d0)
       deallocate(U60)
       deallocate(X11)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef3214(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,S56,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S59(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S59)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N1,N1,N3,N0,N1,N0,N1,X12,S59,-1.0d0)
       deallocate(S59)
C
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(U61(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K2*K3*K4
       I3=K1
       call jungemm(I1,I2,I3,X12,D2,U61)
       deallocate(D2)
C
       call
     & sul124356(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U61,-1.0d0)
       call
     & sul124365(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U61, 1.0d0)
       deallocate(U61)
       deallocate(X12)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,r2A,D2)
       allocate(S166(N1+1:N3,N0+1:N1,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S166)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N2,N3,N2,N3,N1,N3,N0,N1,X18,S166,-1.0d0)
       deallocate(S166)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N2,N3,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,r2A,D2)
       allocate(S167(N1+1:N3,N0+1:N1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S167)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N2,N2,N3,N1,N3,N0,N1,X20,S167,-1.0d0)
       deallocate(S167)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S123(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S123)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N2,N3,N2,N3,N0,N2,X26,S123,-1.0d0)
       deallocate(S123)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N2,N3,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S118(N1+1:N3,N0+1:N1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S118)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3412(N1,N3,N0,N1,N0,N2,N2,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,S118,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,r1B,B2)
       allocate(S119(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S119)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N2,N3,N2,N3,N1,N3,N0,N1,X18,S119, 1.0d0)
       deallocate(S119)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef4312(N1,N3,N0,N1,N0,N2,N2,N3,
     & N2,N3,N0,N2,N1,N3,N0,N1,S118,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S146(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S146)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N1,N3,N0,N2,N0,N1,X15,S146,-1.0d0)
       deallocate(S146)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N0,N1,N1,N3,IntR,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,r2A,D2)
       allocate(S171(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K1*K1
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,D2,S171)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N1,N1,N3,N0,N1,N0,N1,X2,S171,-0.5d0)
       deallocate(S171)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N0,N1,N0,N1,IntR,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,r2A,D2)
       allocate(S172(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K1
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,D2,S172)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef3412(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,S172,D1)
       allocate(F2(N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef561234(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,t3B,F2)
       allocate(U180(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K2*K3*K3*K4
       I3=K1*K1
       call jungemm(I1,I2,I3,D1,F2,U180)
       deallocate(D1)
       deallocate(F2)
C
       V3B=V3B+0.250*U180
       deallocate(U180)
       deallocate(S172)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
       call relef2341(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N2,N0,N1,N0,N2,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S74(N0+1:N1,N0+1:N2,N0+1:N1,N0+1:N2))
       I1=K2*K1*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S74)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N2,N0,N1,N0,N2,N0,N1,X22,S74, 1.0d0)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       call relef2341(N0,N1,N0,N2,N0,N1,N0,N2,
     & N0,N2,N0,N1,N0,N2,N0,N1,S74,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S81(N2+1:N3,N0+1:N1,N0+1:N2,N0+1:N1))
       I1=K1*K2*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S81)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N1,N2,N3,N0,N2,N0,N1,X14,S81, 1.0d0)
       deallocate(S81)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:N2,N0+1:N1))
       call relef3241(N0,N1,N0,N2,N0,N1,N0,N2,
     & N0,N1,N0,N2,N0,N2,N0,N1,S74,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S77(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S77)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N2,N1,N3,N0,N2,N0,N1,X15,S77, 1.0d0)
       deallocate(S77)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N0,N1,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,r2B,D2)
       allocate(S174(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S174)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N0,N1,N2,N3,N0,N2,N0,N1,X14,S174,-1.0d0)
       deallocate(S174)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,r2B,D2)
       allocate(S175(N2+1:N3,N0+1:N2,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S175)
       deallocate(D1)
       deallocate(D2)
C
       call sul2413(N1,N3,N2,N3,N1,N3,N0,N2,X16,S175, 1.0d0)
       deallocate(S175)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,r2B,D2)
       allocate(S176(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S176)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N1,N1,N3,N2,N3,N0,N2,X24,S176,-1.0d0)
       deallocate(S176)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S54(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S54)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N1,N1,N3,N0,N1,N0,N1,X5,S54,-1.0d0)
       deallocate(S54)
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
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N2,N3,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,r2B,D2)
       allocate(S179(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S179)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N2,N2,N3,N2,N3,N0,N2,X26,S179,-1.0d0)
       deallocate(S179)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N0,N2,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,r2B,D2)
       allocate(S180(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3
       I2=K3*K4
       I3=K1*K2
       call jungemm(I1,I2,I3,D1,D2,S180)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N1,N3,N2,N3,N1,N3,N0,N2,X16,S180, 1.0d0)
       deallocate(S180)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N0,N1,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,r2B,D2)
       allocate(S181(N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K3*K4
       I3=K1*K2
       call jungemm(I1,I2,I3,D1,D2,S181)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N2,N3,N2,N3,N1,N3,N0,N1,X18,S181,-1.0d0)
       deallocate(S181)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,r2B,D2)
       allocate(S182(N2+1:N3,N1+1:N3,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K3*K4
       I3=K1*K2
       call jungemm(I1,I2,I3,D1,D2,S182)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N2,N3,N1,N3,N2,N3,N1,N3,X19,S182,-1.0d0)
       deallocate(S182)
C
       allocate(F2(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       call relef123456(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,t3B,F2)
       allocate(U22(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K1*K1*K2*K3
       I3=K3*K4
       call jungemm(I1,I2,I3,X19,F2,U22)
       deallocate(F2)
C
       call
     & sul245613(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U22, 1.0d0)
       call
     & sul345612(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U22,-1.0d0)
       deallocate(U22)
       deallocate(X19)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N0,N1,N1,N3,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S66(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S66)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef2341(N0,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,S66,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(S67(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K3*K3
       I3=K1*K1
       call jungemm(I1,I2,I3,D1,D2,S67)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N1,N3,N1,N3,N1,N3,N0,N1,X8,S67, 0.5d0)
       deallocate(S67)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N0,N1,N1,N3,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S143(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S143)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N2,N0,N1,N1,N3,
     & N0,N2,N0,N1,N1,N3,N0,N2,S143,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2B,D2)
       allocate(S144(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3
       I2=K3*K4
       I3=K1*K2
       call jungemm(I1,I2,I3,D1,D2,S144)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N1,N3,N2,N3,N1,N3,N0,N2,X16,S144, 1.0d0)
       deallocate(S144)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef4123(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,r2B,D2)
       allocate(S185(N1+1:N3,N0+1:N2,N1+1:N3,N2+1:N3))
       I1=K4*K3
       I2=K2*K3
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,D2,S185)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N1,N3,N2,N3,N1,N3,N0,N2,X16,S185,-1.0d0)
       deallocate(S185)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef4132(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N0,N2,N0,N1,IntM,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,r2B,D2)
       allocate(S186(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K2*K3
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,D2,S186)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N0,N2,N1,N3,N0,N2,N0,N1,X15,S186, 1.0d0)
       deallocate(S186)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N1+1:N3))
       call relef4132(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N0,N2,N1,N3,IntM,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,r2B,D2)
       allocate(S187(N1+1:N3,N0+1:N2,N0+1:N2,N1+1:N3))
       I1=K3*K2
       I2=K2*K3
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,D2,S187)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N2,N1,N3,N1,N3,N0,N2,X17,S187, 1.0d0)
       deallocate(S187)
C
       allocate(F2(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef421356(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N1,N3,N2,N3,N1,N3,N0,N1,N0,N1,t3B,F2)
       allocate(U21(N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3
       I2=K1*K1*K3*K4
       I3=K3*K2
       call jungemm(I1,I2,I3,X17,F2,U21)
       deallocate(F2)
C
       call
     & sul125634(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U21,-1.0d0)
       call
     & sul135624(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U21, 1.0d0)
       deallocate(U21)
       deallocate(X17)
C
       allocate(D1(N1+1:N3,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef2134(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N2,N3,N2,N3,N1,N3,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S73(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       I1=K3*K4*K4
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S73)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N2,N3,N2,N3,N1,N3,N0,N1,X18,S73,-1.0d0)
       deallocate(S73)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S150(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S150)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N2,N2,N3,N2,N3,N0,N2,X26,S150, 1.0d0)
       deallocate(S150)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,N0+1:N2))
       call relef3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N0,N1,N0,N2,IntM,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
       call relef3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,N3,N0,N1,r2B,D2)
       allocate(S190(N2+1:N3,N0+1:N1,N0+1:N1,N0+1:N2))
       I1=K2*K1
       I2=K1*K4
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,D2,S190)
       deallocate(D1)
       deallocate(D2)
C
       call sul2413(N0,N1,N2,N3,N0,N2,N0,N1,X14,S190, 1.0d0)
       deallocate(S190)
C
       allocate(D1(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:N3))
       call relef3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
       call relef3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,N3,N0,N1,r2B,D2)
       allocate(S191(N2+1:N3,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K1*K4
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,D2,S191)
       deallocate(D1)
       deallocate(D2)
C
       call sul2413(N2,N3,N2,N3,N1,N3,N0,N1,X18,S191, 1.0d0)
       deallocate(S191)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,N2+1:N3))
       call relef3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N0,N1,N2,N3,IntM,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
       call relef3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,N3,N0,N1,r2B,D2)
       allocate(S192(N2+1:N3,N0+1:N1,N0+1:N1,N2+1:N3))
       I1=K4*K1
       I2=K1*K4
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,D2,S192)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N1,N2,N3,N2,N3,N0,N1,X21,S192, 1.0d0)
       deallocate(S192)
C
       allocate(F2(N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef512346(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,t3B,F2)
       allocate(U109(N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K1*K2*K3*K3
       I3=K4*K1
       call jungemm(I1,I2,I3,X21,F2,U109)
       deallocate(F2)
C
       call
     & sul234516(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U109,-1.0d0)
       call
     & sul234615(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U109, 1.0d0)
       deallocate(U109)
       deallocate(X21)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S21(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S21)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N1,N3,N2,N3,N1,N3,N0,N2,X16,S21,-1.0d0)
       deallocate(S21)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef2431(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N2,N3,N0,N2,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,r1A,B2)
       allocate(S71(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S71)
       deallocate(D1)
       deallocate(B2)
C
       call sul4123(N0,N1,N2,N3,N0,N2,N0,N1,X14,S71,-1.0d0)
       deallocate(S71)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N1,N2+1:N3))
       call relef1243(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N0,N1,N2,N3,IntM,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,r2B,D2)
       allocate(S195(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3))
       I1=K4*K1
       I2=K1*K2
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,D2,S195)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N1,N2,N3,N0,N2,N0,N1,X14,S195,-1.0d0)
       deallocate(S195)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N0,N2,N1,N3,IntM,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,r2B,D2)
       allocate(S196(N0+1:N2,N0+1:N1,N0+1:N2,N1+1:N3))
       I1=K3*K2
       I2=K1*K2
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,D2,S196)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N2,N1,N3,N0,N2,N0,N1,X15,S196,-1.0d0)
       deallocate(S196)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N0,N2,N0,N1,IntM,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,r2B,D2)
       allocate(S197(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K1*K2
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,D2,S197)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N2,N0,N1,N0,N2,N0,N1,X22,S197, 1.0d0)
       deallocate(S197)
C
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,t3B,F2)
       allocate(U136(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K1*K3*K3*K4
       I3=K1*K2
       call jungemm(I1,I2,I3,X22,F2,U136)
       deallocate(F2)
C
       call
     & sul123546(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U136,-1.0d0)
       call
     & sul123645(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U136, 1.0d0)
       deallocate(U136)
       deallocate(X22)
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
       call sul21(N0,N1,N0,N1,X13,Q6,-1.0d0)
       deallocate(Q6)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N0,N2,N2,N3,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,r1B,B2)
       allocate(S151(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S151)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,S151,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S152(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S152)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N2,N3,N2,N3,N0,N2,X26,S152, 1.0d0)
       deallocate(S152)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N0,N1,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,r2B,D2)
       allocate(S2d0(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S2d0)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N0,N1,N1,N3,N0,N1,N0,N1,X5,S2d0, 1.0d0)
       deallocate(S2d0)
C
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(U6(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K2*K3*K4
       I3=K1
       call jungemm(I1,I2,I3,X5,D2,U6)
       deallocate(D2)
C
       call
     & sul124356(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U6, 1.0d0)
       call
     & sul124365(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U6,-1.0d0)
       call
     & sul134256(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U6,-1.0d0)
       call
     & sul134265(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U6, 1.0d0)
       deallocate(U6)
       deallocate(X5)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,r2B,D2)
       allocate(S201(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S201)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N1,N3,N1,N3,N1,N3,N0,N1,X3,S201, 1.0d0)
       deallocate(S201)
C
       allocate(D2(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,N3,N0,N2,N0,N1,t2B,D2)
       allocate(U3(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1*K2*K4
       I3=K3
       call jungemm(I1,I2,I3,X3,D2,U3)
       deallocate(D2)
C
       call
     & sul145236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U3, 1.0d0)
       call
     & sul146235(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U3,-1.0d0)
       call
     & sul145326(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U3,-1.0d0)
       call
     & sul146325(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U3, 1.0d0)
       deallocate(U3)
       deallocate(X3)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,r2B,D2)
       allocate(S202(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S202)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N1,N1,N3,N1,N3,N0,N1,X4,S202,-1.0d0)
       deallocate(S202)
C
       allocate(F2(N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef521346(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N1,N3,N2,N3,N1,N3,N0,N2,N0,N1,t3B,F2)
       allocate(U4(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K2*K3*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,X4,F2,U4)
       deallocate(F2)
C
       call
     & sul124536(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U4,-1.0d0)
       call
     & sul124635(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U4, 1.0d0)
       call
     & sul134526(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U4, 1.0d0)
       call
     & sul134625(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U4,-1.0d0)
       deallocate(U4)
       deallocate(X4)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N0,N2,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,r2A,D2)
       allocate(S165(N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K1*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S165)
       deallocate(D1)
       deallocate(D2)
C
       call sul2413(N0,N2,N1,N3,N0,N2,N0,N1,X15,S165,-1.0d0)
       deallocate(S165)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N0,N1,N0,N1,IntR,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(S15(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K1
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,D2,S15)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef4312(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,S15,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S16(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S16)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N1,N1,N3,N0,N1,N0,N1,X7,S16,-0.5d0)
       deallocate(S16)
C
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(U16(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K2*K3*K4
       I3=K1
       call jungemm(I1,I2,I3,X7,D2,U16)
       deallocate(D2)
C
       call
     & sul124356(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U16,-1.0d0)
       deallocate(U16)
       deallocate(X7)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef3412(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,S15,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,r1A,B2)
       allocate(S51(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S51)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N1,N1,N3,N0,N1,N0,N1,X9,S51,-0.5d0)
       deallocate(S51)
C
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(U43(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K2*K3*K4
       I3=K1
       call jungemm(I1,I2,I3,X9,D2,U43)
       deallocate(D2)
C
       call
     & sul134256(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U43,-1.0d0)
       deallocate(U43)
       deallocate(X9)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,r2B,D2)
       allocate(S206(N1+1:N3,N0+1:N1,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S206)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N2,N3,N2,N3,N1,N3,N0,N1,X18,S206, 1.0d0)
       deallocate(S206)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,r2B,D2)
       allocate(S207(N1+1:N3,N0+1:N1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S207)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N2,N2,N3,N1,N3,N0,N1,X20,S207,-1.0d0)
C
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef412356(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N1,N3,N0,N2,N0,N1,t3C,F2)
       allocate(U23(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K2*K3*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,X20,F2,U23)
       deallocate(F2)
C
       call
     & sul124536(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U23,-1.0d0)
       call
     & sul124635(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U23, 1.0d0)
       call
     & sul134526(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U23, 1.0d0)
       call
     & sul134625(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U23,-1.0d0)
       deallocate(U23)
       deallocate(X20)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3412(N1,N3,N0,N1,N0,N2,N2,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,S207,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S209(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S209)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N2,N3,N2,N3,N1,N3,N0,N1,X18,S209, 1.0d0)
       deallocate(S209)
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
       call sul21(N0,N2,N0,N2,X23,Q24, 1.0d0)
       deallocate(Q24)
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
       call sul21(N2,N3,N2,N3,X25,Q39,-1.0d0)
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
       call sul21(N0,N2,N0,N2,X23,Q40, 1.0d0)
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
       allocate(F2(N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       call relef213456(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N1,N3,N2,N3,N1,N3,N0,N2,N0,N1,N0,N1,t3B,F2)
       allocate(U1(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1*K1*K2*K3*K4
       I3=K3
       call jungemm(I1,I2,I3,X1,F2,U1)
       deallocate(F2)
C
       call
     & sul124563(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U1, 1.0d0)
       call
     & sul134562(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U1,-1.0d0)
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
       call sul21(N0,N1,N0,N1,X13,Q42,-1.0d0)
       deallocate(Q42)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N0,N1,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,r2C,D2)
       allocate(S211(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S211)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N0,N1,N2,N3,N0,N2,N0,N1,X14,S211,-1.0d0)
       deallocate(S211)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,r2C,D2)
       allocate(S212(N2+1:N3,N0+1:N2,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S212)
       deallocate(D1)
       deallocate(D2)
C
       call sul2413(N1,N3,N2,N3,N1,N3,N0,N2,X16,S212, 1.0d0)
       deallocate(S212)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,r2C,D2)
       allocate(S213(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S213)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N1,N1,N3,N2,N3,N0,N2,X24,S213,-1.0d0)
       deallocate(S213)
C
       allocate(F2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,t3A,F2)
       allocate(U114(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K1*K3*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,X24,F2,U114)
       deallocate(F2)
C
       call
     & sul235614(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U114,-1.0d0)
       deallocate(U114)
       deallocate(X24)
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
       call sul21(N0,N1,N0,N1,X13,Q38,-0.5d0)
       deallocate(Q38)
C
       allocate(F2(N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef512346(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,t3B,F2)
       allocate(U89(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K2*K3*K3*K4
       I3=K1
       call jungemm(I1,I2,I3,X13,F2,U89)
       deallocate(F2)
C
       V3B=V3B-U89
       call
     & sul123465(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U89, 1.0d0)
       deallocate(U89)
       deallocate(X13)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef4132(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,r2B,D2)
       allocate(S205(N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K1*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S205)
       deallocate(D1)
       deallocate(D2)
C
       call sul2413(N0,N2,N1,N3,N0,N2,N0,N1,X15,S205, 1.0d0)
       deallocate(S205)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,r2C,D2)
       allocate(S216(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S216)
       deallocate(D1)
       deallocate(D2)
C
       call sul3412(N0,N2,N2,N3,N2,N3,N0,N2,X26,S216,-1.0d0)
       deallocate(S216)
C
       allocate(F2(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N2,N3,N1,N3,N1,N3,N0,N1,N0,N1,t3B,F2)
       allocate(U122(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K1*K3*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,X26,F2,U122)
       deallocate(F2)
C
       call
     & sul235614(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U122,-1.0d0)
       deallocate(U122)
       deallocate(X26)
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
       call sul21(N2,N3,N2,N3,X25,Q43,-0.5d0)
       deallocate(Q43)
C
       allocate(F2(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       call relef123456(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,t3B,F2)
       allocate(U94(N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3))
       I1=K4
       I2=K1*K1*K2*K3*K3
       I3=K4
       call jungemm(I1,I2,I3,X25,F2,U94)
       deallocate(F2)
C
       call
     & sul234561(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U94, 1.0d0)
       deallocate(U94)
       deallocate(X25)
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
       call sul21(N0,N2,N0,N2,X23,Q44, 0.5d0)
       deallocate(Q44)
C
       allocate(F2(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N2,N3,N1,N3,N1,N3,N0,N1,N0,N1,t3B,F2)
       allocate(U95(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N2))
       I1=K2
       I2=K1*K1*K3*K3*K4
       I3=K2
       call jungemm(I1,I2,I3,X23,F2,U95)
       deallocate(F2)
C
       call
     & sul123564(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U95,-1.0d0)
       deallocate(U95)
       deallocate(X23)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,r3A,F2)
       allocate(S217(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1*K3*K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,F2,S217)
       deallocate(D1)
       deallocate(F2)
C
       call sul2341(N1,N3,N1,N3,N1,N3,N0,N1,X8,S217,-0.5d0)
       deallocate(S217)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef4123(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(F2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,r3A,F2)
       allocate(S218(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K1*K3
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,D1,F2,S218)
       deallocate(D1)
       deallocate(F2)
C
       call sul2341(N0,N1,N1,N3,N0,N1,N0,N1,X2,S218, 0.5d0)
       deallocate(S218)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef562134(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N2,N3,N1,N3,N0,N2,r3B,F2)
       allocate(S219(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3))
       I1=K3
       I2=K2*K3*K4
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,F2,S219)
       deallocate(D1)
       deallocate(F2)
C
       call sul2341(N1,N3,N2,N3,N1,N3,N0,N2,X16,S219, 0.5d0)
       deallocate(S219)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(F2(N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef523146(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N2,N3,N0,N2,N0,N1,r3B,F2)
       allocate(S220(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K2*K4
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,D1,F2,S220)
       deallocate(D1)
       deallocate(F2)
C
       call sul2341(N0,N1,N2,N3,N0,N2,N0,N1,X14,S220,-0.5d0)
       deallocate(S220)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(F2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef452136(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,N1,N3,N0,N1,r3B,F2)
       allocate(S221(N2+1:N3,N1+1:N3,N0+1:N1,N2+1:N3))
       I1=K4
       I2=K1*K3*K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,F2,S221)
       deallocate(D1)
       deallocate(F2)
C
       call sul2341(N2,N3,N2,N3,N1,N3,N0,N1,X18,S221, 1.0d0)
       deallocate(S221)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N0,N2,IntM,D1)
       allocate(F2(N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef512346(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,r3B,F2)
       allocate(S222(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
       I1=K2
       I2=K1*K2*K3
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,D1,F2,S222)
       deallocate(D1)
       deallocate(F2)
C
       call sul2341(N0,N2,N1,N3,N0,N2,N0,N1,X15,S222,-1.0d0)
       deallocate(S222)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,r3B,F2)
       allocate(S223(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1*K3*K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,F2,S223)
       deallocate(D1)
       deallocate(F2)
C
       call sul2341(N1,N3,N1,N3,N1,N3,N0,N1,X8,S223, 1.0d0)
       deallocate(S223)
C
       allocate(D2(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,N3,N0,N2,N0,N1,t2B,D2)
       allocate(U17(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1*K2*K4
       I3=K3
       call jungemm(I1,I2,I3,X8,D2,U17)
       deallocate(D2)
C
       call
     & sul145236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U17,-1.0d0)
       call
     & sul146235(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U17, 1.0d0)
       deallocate(U17)
       deallocate(X8)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N2,N3,N1,N3,N1,N3,N0,N1,N0,N1,r3B,F2)
       allocate(S224(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K1*K3
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,D1,F2,S224)
       deallocate(D1)
       deallocate(F2)
C
       call sul2341(N0,N1,N1,N3,N0,N1,N0,N1,X2,S224,-1.0d0)
       deallocate(S224)
C
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(U2(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K2*K3*K4
       I3=K1
       call jungemm(I1,I2,I3,X2,D2,U2)
       deallocate(D2)
C
       call
     & sul124356(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U2, 1.0d0)
       call
     & sul134256(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U2,-1.0d0)
       deallocate(U2)
       deallocate(X2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef461235(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,r3C,F2)
       allocate(S225(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3))
       I1=K3
       I2=K2*K3*K4
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,F2,S225)
       deallocate(D1)
       deallocate(F2)
C
       call sul2341(N1,N3,N2,N3,N1,N3,N0,N2,X16,S225,-1.0d0)
       deallocate(S225)
C
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(U27(N1+1:N3,N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K4
       I2=K1*K1*K3
       I3=K3
       call jungemm(I1,I2,I3,X16,D2,U27)
       deallocate(D2)
C
       call
     & sul256134(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U27,-1.0d0)
       call
     & sul356124(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U27, 1.0d0)
       deallocate(U27)
       deallocate(X16)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef413256(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N2,N3,N0,N2,N0,N1,r3C,F2)
       allocate(S226(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K2*K4
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,D1,F2,S226)
       deallocate(D1)
       deallocate(F2)
C
       call sul2341(N0,N1,N2,N3,N0,N2,N0,N1,X14,S226,-1.0d0)
       deallocate(S226)
C
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(U191(N1+1:N3,N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K4
       I2=K1*K3*K3
       I3=K1
       call jungemm(I1,I2,I3,X14,D2,U191)
       deallocate(D2)
C
       call
     & sul235146(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U191,-1.0d0)
       call
     & sul236145(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U191, 1.0d0)
       deallocate(U191)
       deallocate(X14)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,r3C,F2)
       allocate(S227(N2+1:N3,N1+1:N3,N0+1:N1,N2+1:N3))
       I1=K4
       I2=K1*K3*K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,F2,S227)
       deallocate(D1)
       deallocate(F2)
C
       call sul2341(N2,N3,N2,N3,N1,N3,N0,N1,X18,S227,-0.5d0)
       deallocate(S227)
C
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,t2B,D2)
       allocate(U25(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K1*K2*K3
       I3=K4
       call jungemm(I1,I2,I3,X18,D2,U25)
       deallocate(D2)
C
       call
     & sul245136(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U25,-1.0d0)
       call
     & sul246135(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U25, 1.0d0)
       call
     & sul345126(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U25, 1.0d0)
       call
     & sul346125(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U25,-1.0d0)
       deallocate(U25)
       deallocate(X18)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef4123(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef412356(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N1,N3,N0,N2,N0,N1,r3C,F2)
       allocate(S228(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
       I1=K2
       I2=K1*K2*K3
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,D1,F2,S228)
       deallocate(D1)
       deallocate(F2)
C
       call sul2341(N0,N2,N1,N3,N0,N2,N0,N1,X15,S228, 0.5d0)
       deallocate(S228)
C
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(U24(N2+1:N3,N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K3
       I2=K1*K3*K4
       I3=K2
       call jungemm(I1,I2,I3,X15,D2,U24)
       deallocate(D2)
C
       call
     & sul125346(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U24, 1.0d0)
       call
     & sul126345(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U24,-1.0d0)
       call
     & sul135246(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U24,-1.0d0)
       call
     & sul136245(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U24, 1.0d0)
       deallocate(U24)
       deallocate(X15)
C
       allocate(D1(N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef2341(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N2,N3,N1,N3,N0,N2,H2B,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,r2A,D2)
       allocate(U164(N1+1:N3,N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K4
       I2=K1*K1*K3
       I3=K3
       call jungemm(I1,I2,I3,D1,D2,U164)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul356124(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U164, 1.0d0)
       call
     & sul256134(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U164,-1.0d0)
       deallocate(U164)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N0,N1,H2A,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,r2B,D2)
       allocate(U184(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K2*K3*K4
       I3=K1
       call jungemm(I1,I2,I3,D1,D2,U184)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul134256(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U184, 1.0d0)
       call
     & sul124356(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U184,-1.0d0)
       deallocate(U184)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N1,N3,N0,N1,H2A,D1)
       allocate(D2(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,N3,N0,N2,N0,N1,r2B,D2)
       allocate(U185(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1*K2*K4
       I3=K3
       call jungemm(I1,I2,I3,D1,D2,U185)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul145236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U185, 1.0d0)
       call
     & sul146235(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U185,-1.0d0)
       deallocate(U185)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N0,N2,N0,N1,H2B,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,r2B,D2)
       allocate(U186(N2+1:N3,N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K3
       I2=K1*K3*K4
       I3=K2
       call jungemm(I1,I2,I3,D1,D2,U186)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul135246(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U186, 1.0d0)
       call
     & sul125346(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U186,-1.0d0)
       call
     & sul136245(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U186,-1.0d0)
       call
     & sul126345(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U186, 1.0d0)
       deallocate(U186)
C
       allocate(D1(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N1,N3,N0,N1,H2B,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,r2B,D2)
       allocate(U187(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K1*K2*K3
       I3=K4
       call jungemm(I1,I2,I3,D1,D2,U187)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul345126(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U187,-1.0d0)
       call
     & sul245136(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U187, 1.0d0)
       call
     & sul346125(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U187, 1.0d0)
       call
     & sul246135(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U187,-1.0d0)
       deallocate(U187)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N0,N2,N0,N1,H2B,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,r2A,D2)
       allocate(U163(N1+1:N3,N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K4
       I2=K1*K3*K3
       I3=K1
       call jungemm(I1,I2,I3,D1,D2,U163)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul235146(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U163, 1.0d0)
       call
     & sul236145(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U163,-1.0d0)
       deallocate(U163)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,H2B,D1)
       allocate(F2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,r3A,F2)
       allocate(U237(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K1*K3*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,F2,U237)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul235614(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U237, 1.0d0)
       deallocate(U237)
C
       allocate(B1(N0+1:N1,N0+1:N1))
       call relef21(N0,N3,N0,N3,
     & N0,N1,N0,N1,H1A,B1)
       allocate(F2(N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef512346(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,r3B,F2)
       allocate(U240(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K2*K3*K3*K4
       I3=K1
       call jungemm(I1,I2,I3,B1,F2,U240)
       deallocate(B1)
       deallocate(F2)
C
       V3B=V3B+U240
       call
     & sul123465(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U240,-1.0d0)
       deallocate(U240)
C
       allocate(B1(N1+1:N3,N1+1:N3))
       call relef12(N0,N3,N0,N3,
     & N1,N3,N1,N3,H1A,B1)
       allocate(F2(N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       call relef213456(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N1,N3,N2,N3,N1,N3,N0,N2,N0,N1,N0,N1,r3B,F2)
       allocate(U241(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1*K1*K2*K3*K4
       I3=K3
       call jungemm(I1,I2,I3,B1,F2,U241)
       deallocate(B1)
       deallocate(F2)
C
       call
     & sul134562(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U241, 1.0d0)
       call
     & sul124563(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U241,-1.0d0)
       deallocate(U241)
C
       allocate(B1(N0+1:N2,N0+1:N2))
       call relef21(N0,N3,N0,N3,
     & N0,N2,N0,N2,H1B,B1)
       allocate(F2(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N2,N3,N1,N3,N1,N3,N0,N1,N0,N1,r3B,F2)
       allocate(U242(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N2))
       I1=K2
       I2=K1*K1*K3*K3*K4
       I3=K2
       call jungemm(I1,I2,I3,B1,F2,U242)
       deallocate(B1)
       deallocate(F2)
C
       call
     & sul123564(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U242,-1.0d0)
       deallocate(U242)
C
       allocate(B1(N2+1:N3,N2+1:N3))
       call relef12(N0,N3,N0,N3,
     & N2,N3,N2,N3,H1B,B1)
       allocate(F2(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       call relef123456(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,r3B,F2)
       allocate(U243(N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3))
       I1=K4
       I2=K1*K1*K2*K3*K3
       I3=K4
       call jungemm(I1,I2,I3,B1,F2,U243)
       deallocate(B1)
       deallocate(F2)
C
       call
     & sul234561(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U243, 1.0d0)
       deallocate(U243)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N0,N1,N0,N1,H2A,D1)
       allocate(F2(N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef561234(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,r3B,F2)
       allocate(U244(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K2*K3*K3*K4
       I3=K1*K1
       call jungemm(I1,I2,I3,D1,F2,U244)
       deallocate(D1)
       deallocate(F2)
C
       V3B=V3B+0.5d0*U244
       deallocate(U244)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,H2A,D1)
       allocate(F2(N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef521346(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N1,N3,N2,N3,N1,N3,N0,N2,N0,N1,r3B,F2)
       allocate(U245(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K2*K3*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,F2,U245)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul134526(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U245,-1.0d0)
       call
     & sul124536(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U245, 1.0d0)
       call
     & sul134625(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U245, 1.0d0)
       call
     & sul124635(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U245,-1.0d0)
       deallocate(U245)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N1,N3,N1,N3,H2A,D1)
       allocate(F2(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       call relef231456(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N1,N3,N1,N3,N2,N3,N0,N2,N0,N1,N0,N1,r3B,F2)
       allocate(U246(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K1*K1*K2*K4
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,F2,U246)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul145623(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U246, 0.5d0)
       deallocate(U246)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N0,N2,N0,N1,H2B,D1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,r3B,F2)
       allocate(U247(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K1*K3*K3*K4
       I3=K1*K2
       call jungemm(I1,I2,I3,D1,F2,U247)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul123546(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U247,-1.0d0)
       call
     & sul123645(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U247, 1.0d0)
       deallocate(U247)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       call relef4132(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N2,N3,N0,N1,H2B,D1)
       allocate(F2(N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef512346(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,r3B,F2)
       allocate(U248(N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K1*K2*K3*K3
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,F2,U248)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul234516(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U248, 1.0d0)
       call
     & sul234615(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U248,-1.0d0)
       deallocate(U248)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N1,N3,N0,N2,H2B,D1)
       allocate(F2(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef421356(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N1,N3,N2,N3,N1,N3,N0,N1,N0,N1,r3B,F2)
       allocate(U249(N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3
       I2=K1*K1*K3*K4
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,F2,U249)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul135624(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U249,-1.0d0)
       call
     & sul125634(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U249, 1.0d0)
       deallocate(U249)
C
       allocate(D1(N2+1:N3,N1+1:N3,N2+1:N3,N1+1:N3))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N2,N3,N1,N3,H2B,D1)
       allocate(F2(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       call relef123456(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,r3B,F2)
       allocate(U250(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K1*K1*K2*K3
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,F2,U250)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul345612(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U250, 1.0d0)
       call
     & sul245613(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U250,-1.0d0)
       deallocate(U250)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,H2C,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N2,N3,N1,N3,N1,N3,N0,N1,N0,N1,r3B,F2)
       allocate(U251(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K1*K3*K3
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,F2,U251)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul235614(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U251, 1.0d0)
       deallocate(U251)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,H2B,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef412356(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N1,N3,N0,N2,N0,N1,r3C,F2)
       allocate(U258(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K2*K3*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,F2,U258)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul134526(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U258,-1.0d0)
       call
     & sul124536(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U258, 1.0d0)
       call
     & sul134625(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U258, 1.0d0)
       call
     & sul124635(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,V3B,U258,-1.0d0)
       deallocate(U258)
C
       end
