       subroutine L1A_update(N0,N1,N2,N3,V1A,
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
       real*8 V1A(N1+1:N3,N0+1:N1)
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
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::Q9(:,:)
       real*8,allocatable::Q11(:,:)
       real*8,allocatable::Q13(:,:)
       real*8,allocatable::Q14(:,:)
       real*8,allocatable::Q16(:,:)
       real*8,allocatable::Q17(:,:)
       real*8,allocatable::Q19(:,:)
       real*8,allocatable::Q23(:,:)
       real*8,allocatable::Q29(:,:)
       real*8,allocatable::Q25(:,:)
       real*8,allocatable::Q31(:,:)
       real*8,allocatable::Q27(:,:)
       real*8,allocatable::Q33(:,:)
       real*8,allocatable::Q39(:,:)
       real*8,allocatable::Q35(:,:)
       real*8,allocatable::Q41(:,:)
       real*8,allocatable::Q37(:,:)
       real*8,allocatable::Q43(:,:)
       real*8,allocatable::Q45(:,:)
       real*8,allocatable::Q47(:,:)
       real*8,allocatable::Q49(:,:)
       real*8,allocatable::Q51(:,:)
       real*8,allocatable::Q52(:,:)
       real*8,allocatable::Q54(:,:)
       real*8,allocatable::Q55(:,:)
       real*8,allocatable::Q57(:,:)
       real*8,allocatable::Q77(:,:)
       real*8,allocatable::Q225(:,:)
       real*8,allocatable::Q68(:,:)
       real*8,allocatable::Q126(:,:)
       real*8,allocatable::Q69(:,:)
       real*8,allocatable::Q115(:,:)
       real*8,allocatable::S1(:,:,:,:)
       real*8,allocatable::S11(:,:,:,:)
       real*8,allocatable::S12(:,:,:,:)
       real*8,allocatable::S128(:,:,:,:)
       real*8,allocatable::S8(:,:,:,:)
       real*8,allocatable::S18(:,:,:,:)
       real*8,allocatable::S15(:,:,:,:)
       real*8,allocatable::S51(:,:,:,:)
       real*8,allocatable::S9(:,:,:,:)
       real*8,allocatable::S10(:,:,:,:)
       real*8,allocatable::S7(:,:,:,:)
       real*8,allocatable::S58(:,:,:,:)
       real*8,allocatable::S54(:,:,:,:)
       real*8,allocatable::S17(:,:,:,:)
       real*8,allocatable::S16(:,:,:,:)
       real*8,allocatable::S50(:,:,:,:)
       real*8,allocatable::S5(:,:,:,:)
       real*8,allocatable::S6(:,:,:,:)
       real*8,allocatable::S2(:,:,:,:)
       real*8,allocatable::S3(:,:,:,:)
       real*8,allocatable::S4(:,:,:,:)
       real*8,allocatable::S13(:,:,:,:)
       real*8,allocatable::S14(:,:,:,:)
       real*8,allocatable::S55(:,:,:,:)
       real*8,allocatable::S19(:,:,:,:)
       real*8,allocatable::S20(:,:,:,:)
       real*8,allocatable::S21(:,:,:,:)
       real*8,allocatable::S142(:,:,:,:)
       real*8,allocatable::S146(:,:,:,:)
       real*8,allocatable::S77(:,:,:,:)
       real*8,allocatable::S61(:,:,:,:)
       real*8,allocatable::S71(:,:,:,:)
       real*8,allocatable::S80(:,:,:,:)
       real*8,allocatable::S138(:,:,:,:)
       real*8,allocatable::S67(:,:,:,:)
       real*8,allocatable::S79(:,:,:,:)
       real*8,allocatable::S130(:,:,:,:)
       real*8,allocatable::S78(:,:,:,:)
       real*8,allocatable::S60(:,:,:,:)
       real*8,allocatable::S70(:,:,:,:)
       real*8,allocatable::S135(:,:,:,:)
       real*8,allocatable::S65(:,:,:,:)
       real*8,allocatable::S66(:,:,:,:)
       real*8,allocatable::S141(:,:,:,:)
       real*8,allocatable::S22(:,:,:,:)
       real*8,allocatable::S23(:,:,:,:)
       real*8,allocatable::S76(:,:,:,:)
       real*8,allocatable::Q80(:,:)
       real*8,allocatable::Q100(:,:)
       real*8,allocatable::Q254(:,:)
       real*8,allocatable::Q92(:,:)
       real*8,allocatable::Q167(:,:)
       real*8,allocatable::Q91(:,:)
       real*8,allocatable::Q156(:,:)
       real*8,allocatable::S24(:,:,:,:)
       real*8,allocatable::S28(:,:,:,:)
       real*8,allocatable::S25(:,:,:,:)
       real*8,allocatable::S42(:,:,:,:)
       real*8,allocatable::S43(:,:,:,:)
       real*8,allocatable::S26(:,:,:,:)
       real*8,allocatable::S27(:,:,:,:)
       real*8,allocatable::S29(:,:,:,:)
       real*8,allocatable::S30(:,:,:,:)
       real*8,allocatable::S39(:,:,:,:)
       real*8,allocatable::S40(:,:,:,:)
       real*8,allocatable::S41(:,:,:,:)
       real*8,allocatable::S31(:,:,:,:)
       real*8,allocatable::S32(:,:,:,:)
       real*8,allocatable::S33(:,:,:,:)
       real*8,allocatable::S34(:,:,:,:)
       real*8,allocatable::S35(:,:,:,:)
       real*8,allocatable::S36(:,:,:,:)
       real*8,allocatable::S37(:,:,:,:)
       real*8,allocatable::S38(:,:,:,:)
       real*8,allocatable::S44(:,:,:,:)
       real*8,allocatable::S45(:,:,:,:)
       real*8,allocatable::S46(:,:,:,:)
       real*8,allocatable::S47(:,:,:,:)
       real*8,allocatable::S48(:,:,:,:)
       real*8,allocatable::Q103(:,:)
       real*8,allocatable::Q105(:,:)
       real*8,allocatable::Q107(:,:)
       real*8,allocatable::Q108(:,:)
       real*8,allocatable::Q110(:,:)
       real*8,allocatable::Q111(:,:)
       real*8,allocatable::S49(:,:,:,:)
       real*8,allocatable::Q113(:,:)
       real*8,allocatable::S52(:,:,:,:)
       real*8,allocatable::S53(:,:,:,:)
       real*8,allocatable::S56(:,:,:,:)
       real*8,allocatable::S57(:,:,:,:)
       real*8,allocatable::S129(:,:,:,:)
       real*8,allocatable::S72(:,:,:,:)
       real*8,allocatable::S147(:,:,:,:)
       real*8,allocatable::S143(:,:,:,:)
       real*8,allocatable::S81(:,:,:,:)
       real*8,allocatable::S68(:,:,:,:)
       real*8,allocatable::S75(:,:,:,:)
       real*8,allocatable::S145(:,:,:,:)
       real*8,allocatable::S62(:,:,:,:)
       real*8,allocatable::S140(:,:,:,:)
       real*8,allocatable::S139(:,:,:,:)
       real*8,allocatable::S59(:,:,:,:)
       real*8,allocatable::S69(:,:,:,:)
       real*8,allocatable::S63(:,:,:,:)
       real*8,allocatable::S64(:,:,:,:)
       real*8,allocatable::S73(:,:,:,:)
       real*8,allocatable::S74(:,:,:,:)
       real*8,allocatable::S144(:,:,:,:)
       real*8,allocatable::S82(:,:,:,:)
       real*8,allocatable::S87(:,:,:,:)
       real*8,allocatable::S86(:,:,:,:)
       real*8,allocatable::S95(:,:,:,:)
       real*8,allocatable::S97(:,:,:,:)
       real*8,allocatable::S83(:,:,:,:)
       real*8,allocatable::S88(:,:,:,:)
       real*8,allocatable::S96(:,:,:,:)
       real*8,allocatable::S98(:,:,:,:)
       real*8,allocatable::S99(:,:,:,:)
       real*8,allocatable::S84(:,:,:,:)
       real*8,allocatable::S85(:,:,:,:)
       real*8,allocatable::S89(:,:,:,:)
       real*8,allocatable::S90(:,:,:,:)
       real*8,allocatable::S91(:,:,:,:)
       real*8,allocatable::S92(:,:,:,:)
       real*8,allocatable::S93(:,:,:,:)
       real*8,allocatable::S110(:,:,:,:)
       real*8,allocatable::S94(:,:,:,:)
       real*8,allocatable::S115(:,:,:,:)
       real*8,allocatable::S109(:,:,:,:)
       real*8,allocatable::S123(:,:,:,:)
       real*8,allocatable::S100(:,:,:,:)
       real*8,allocatable::S111(:,:,:,:)
       real*8,allocatable::S105(:,:,:,:)
       real*8,allocatable::S119(:,:,:,:)
       real*8,allocatable::S120(:,:,:,:)
       real*8,allocatable::S101(:,:,:,:)
       real*8,allocatable::S112(:,:,:,:)
       real*8,allocatable::S106(:,:,:,:)
       real*8,allocatable::S116(:,:,:,:)
       real*8,allocatable::S121(:,:,:,:)
       real*8,allocatable::S122(:,:,:,:)
       real*8,allocatable::S102(:,:,:,:)
       real*8,allocatable::S113(:,:,:,:)
       real*8,allocatable::S103(:,:,:,:)
       real*8,allocatable::S114(:,:,:,:)
       real*8,allocatable::S104(:,:,:,:)
       real*8,allocatable::S107(:,:,:,:)
       real*8,allocatable::S108(:,:,:,:)
       real*8,allocatable::S117(:,:,:,:)
       real*8,allocatable::S118(:,:,:,:)
       real*8,allocatable::Q197(:,:)
       real*8,allocatable::Q199(:,:)
       real*8,allocatable::Q201(:,:)
       real*8,allocatable::Q202(:,:)
       real*8,allocatable::Q204(:,:)
       real*8,allocatable::Q205(:,:)
       real*8,allocatable::S124(:,:,:,:)
       real*8,allocatable::Q207(:,:)
       real*8,allocatable::S125(:,:,:,:)
       real*8,allocatable::Q209(:,:)
       real*8,allocatable::Q211(:,:)
       real*8,allocatable::Q215(:,:)
       real*8,allocatable::Q213(:,:)
       real*8,allocatable::Q217(:,:)
       real*8,allocatable::S126(:,:,:,:)
       real*8,allocatable::Q219(:,:)
       real*8,allocatable::Q221(:,:)
       real*8,allocatable::S127(:,:,:,:)
       real*8,allocatable::S131(:,:,:,:)
       real*8,allocatable::S132(:,:,:,:)
       real*8,allocatable::S133(:,:,:,:)
       real*8,allocatable::S134(:,:,:,:)
       real*8,allocatable::S136(:,:,:,:)
       real*8,allocatable::S137(:,:,:,:)
       real*8,allocatable::Q250(:,:)
       real*8,allocatable::S148(:,:,:,:)
       real*8,allocatable::S149(:,:,:,:)
       real*8,allocatable::S150(:,:,:,:)
       real*8,allocatable::S151(:,:,:,:)
       real*8,allocatable::S164(:,:,:,:)
       real*8,allocatable::S152(:,:,:,:)
       real*8,allocatable::S163(:,:,:,:)
       real*8,allocatable::S180(:,:,:,:)
       real*8,allocatable::S153(:,:,:,:)
       real*8,allocatable::S158(:,:,:,:)
       real*8,allocatable::S154(:,:,:,:)
       real*8,allocatable::S175(:,:,:,:)
       real*8,allocatable::S178(:,:,:,:)
       real*8,allocatable::S179(:,:,:,:)
       real*8,allocatable::S155(:,:,:,:)
       real*8,allocatable::S156(:,:,:,:)
       real*8,allocatable::S157(:,:,:,:)
       real*8,allocatable::S159(:,:,:,:)
       real*8,allocatable::S160(:,:,:,:)
       real*8,allocatable::S167(:,:,:,:)
       real*8,allocatable::S161(:,:,:,:)
       real*8,allocatable::S162(:,:,:,:)
       real*8,allocatable::S165(:,:,:,:)
       real*8,allocatable::S166(:,:,:,:)
       real*8,allocatable::S174(:,:,:,:)
       real*8,allocatable::S176(:,:,:,:)
       real*8,allocatable::S177(:,:,:,:)
       real*8,allocatable::S168(:,:,:,:)
       real*8,allocatable::S169(:,:,:,:)
       real*8,allocatable::S170(:,:,:,:)
       real*8,allocatable::S171(:,:,:,:)
       real*8,allocatable::S172(:,:,:,:)
       real*8,allocatable::S173(:,:,:,:)
       real*8,allocatable::Q279(:,:)
       real*8,allocatable::Q281(:,:)
       real*8,allocatable::Q283(:,:)
       real*8,allocatable::Q284(:,:)
       real*8,allocatable::Q286(:,:)
       real*8,allocatable::Q287(:,:)
       real*8,allocatable::S181(:,:,:,:)
       real*8,allocatable::Q289(:,:)
       real*8,allocatable::Q291(:,:)
       real*8,allocatable::Q293(:,:)
       real*8,allocatable::Q295(:,:)
       real*8,allocatable::Q296(:,:)
       real*8,allocatable::Q298(:,:)
       real*8,allocatable::Q299(:,:)
       real*8,allocatable::S182(:,:,:,:)
       real*8,allocatable::Q301(:,:)
       real*8,allocatable::S183(:,:,:,:)
       real*8,allocatable::Q303(:,:)
       real*8,allocatable::Q305(:,:)
       real*8,allocatable::Q307(:,:)
       real*8,allocatable::Q309(:,:)
       real*8,allocatable::Q310(:,:)
       real*8,allocatable::Q312(:,:)
       real*8,allocatable::Q313(:,:)
       real*8,allocatable::S184(:,:,:,:)
       real*8,allocatable::Q315(:,:)
       real*8,allocatable::U1(:,:)
       real*8,allocatable::U2(:,:)
       real*8,allocatable::U3(:,:)
       real*8,allocatable::U4(:,:)
       real*8,allocatable::U5(:,:)
       real*8,allocatable::U6(:,:)
       real*8,allocatable::U8(:,:)
       real*8,allocatable::U10(:,:)
       real*8,allocatable::U12(:,:)
       real*8,allocatable::U15(:,:)
       real*8,allocatable::U18(:,:)
       real*8,allocatable::U20(:,:)
       real*8,allocatable::U21(:,:)
       real*8,allocatable::U22(:,:)
       real*8,allocatable::U24(:,:)
       real*8,allocatable::U30(:,:)
       real*8,allocatable::U26(:,:)
       real*8,allocatable::U32(:,:)
       real*8,allocatable::U28(:,:)
       real*8,allocatable::U34(:,:)
       real*8,allocatable::U40(:,:)
       real*8,allocatable::U36(:,:)
       real*8,allocatable::U42(:,:)
       real*8,allocatable::U38(:,:)
       real*8,allocatable::U44(:,:)
       real*8,allocatable::U46(:,:)
       real*8,allocatable::U48(:,:)
       real*8,allocatable::U50(:,:)
       real*8,allocatable::U53(:,:)
       real*8,allocatable::U56(:,:)
       real*8,allocatable::X1(:,:)
       real*8,allocatable::U58(:,:)
       real*8,allocatable::X2(:,:)
       real*8,allocatable::U243(:,:)
       real*8,allocatable::U127(:,:)
       real*8,allocatable::U70(:,:)
       real*8,allocatable::X3(:,:)
       real*8,allocatable::U148(:,:)
       real*8,allocatable::X4(:,:,:,:)
       real*8,allocatable::U59(:,:)
       real*8,allocatable::U66(:,:)
       real*8,allocatable::U224(:,:)
       real*8,allocatable::X5(:,:,:,:)
       real*8,allocatable::U71(:,:)
       real*8,allocatable::X6(:,:,:,:)
       real*8,allocatable::U130(:,:)
       real*8,allocatable::U65(:,:)
       real*8,allocatable::U124(:,:)
       real*8,allocatable::X7(:,:,:,:)
       real*8,allocatable::U74(:,:)
       real*8,allocatable::X8(:,:,:,:)
       real*8,allocatable::U129(:,:)
       real*8,allocatable::U63(:,:)
       real*8,allocatable::U60(:,:)
       real*8,allocatable::U61(:,:)
       real*8,allocatable::U62(:,:)
       real*8,allocatable::U67(:,:)
       real*8,allocatable::U125(:,:)
       real*8,allocatable::U75(:,:)
       real*8,allocatable::U76(:,:)
       real*8,allocatable::X9(:,:,:,:)
       real*8,allocatable::U246(:,:)
       real*8,allocatable::X10(:,:,:,:)
       real*8,allocatable::U151(:,:)
       real*8,allocatable::U236(:,:)
       real*8,allocatable::U141(:,:)
       real*8,allocatable::X11(:,:,:,:)
       real*8,allocatable::U247(:,:)
       real*8,allocatable::X12(:,:,:,:)
       real*8,allocatable::U152(:,:)
       real*8,allocatable::X13(:,:,:,:)
       real*8,allocatable::U239(:,:)
       real*8,allocatable::X14(:,:,:,:)
       real*8,allocatable::U154(:,:)
       real*8,allocatable::U79(:,:)
       real*8,allocatable::U150(:,:)
       real*8,allocatable::X15(:,:)
       real*8,allocatable::U81(:,:)
       real*8,allocatable::X16(:,:)
       real*8,allocatable::U272(:,:)
       real*8,allocatable::U168(:,:)
       real*8,allocatable::U93(:,:)
       real*8,allocatable::X17(:,:)
       real*8,allocatable::U189(:,:)
       real*8,allocatable::U82(:,:)
       real*8,allocatable::U86(:,:)
       real*8,allocatable::U83(:,:)
       real*8,allocatable::U97(:,:)
       real*8,allocatable::U84(:,:)
       real*8,allocatable::U85(:,:)
       real*8,allocatable::U95(:,:)
       real*8,allocatable::U96(:,:)
       real*8,allocatable::U89(:,:)
       real*8,allocatable::U90(:,:)
       real*8,allocatable::U94(:,:)
       real*8,allocatable::U98(:,:)
       real*8,allocatable::U99(:,:)
       real*8,allocatable::U102(:,:)
       real*8,allocatable::U104(:,:)
       real*8,allocatable::U106(:,:)
       real*8,allocatable::U109(:,:)
       real*8,allocatable::U112(:,:)
       real*8,allocatable::U114(:,:)
       real*8,allocatable::U119(:,:)
       real*8,allocatable::U120(:,:)
       real*8,allocatable::X18(:,:,:,:)
       real*8,allocatable::U128(:,:)
       real*8,allocatable::X19(:,:,:,:)
       real*8,allocatable::U227(:,:)
       real*8,allocatable::U241(:,:)
       real*8,allocatable::U244(:,:)
       real*8,allocatable::X20(:,:,:,:)
       real*8,allocatable::U146(:,:)
       real*8,allocatable::U133(:,:)
       real*8,allocatable::U137(:,:)
       real*8,allocatable::U138(:,:)
       real*8,allocatable::U147(:,:)
       real*8,allocatable::U242(:,:)
       real*8,allocatable::U158(:,:)
       real*8,allocatable::U162(:,:)
       real*8,allocatable::U170(:,:)
       real*8,allocatable::U172(:,:)
       real*8,allocatable::U159(:,:)
       real*8,allocatable::U171(:,:)
       real*8,allocatable::U173(:,:)
       real*8,allocatable::U160(:,:)
       real*8,allocatable::U161(:,:)
       real*8,allocatable::U165(:,:)
       real*8,allocatable::U166(:,:)
       real*8,allocatable::U174(:,:)
       real*8,allocatable::U169(:,:)
       real*8,allocatable::U190(:,:)
       real*8,allocatable::U183(:,:)
       real*8,allocatable::U196(:,:)
       real*8,allocatable::U175(:,:)
       real*8,allocatable::U180(:,:)
       real*8,allocatable::U193(:,:)
       real*8,allocatable::U194(:,:)
       real*8,allocatable::U176(:,:)
       real*8,allocatable::U191(:,:)
       real*8,allocatable::U195(:,:)
       real*8,allocatable::U177(:,:)
       real*8,allocatable::U187(:,:)
       real*8,allocatable::U178(:,:)
       real*8,allocatable::U188(:,:)
       real*8,allocatable::U179(:,:)
       real*8,allocatable::U182(:,:)
       real*8,allocatable::U192(:,:)
       real*8,allocatable::U198(:,:)
       real*8,allocatable::U200(:,:)
       real*8,allocatable::U203(:,:)
       real*8,allocatable::U206(:,:)
       real*8,allocatable::U208(:,:)
       real*8,allocatable::U210(:,:)
       real*8,allocatable::U212(:,:)
       real*8,allocatable::U216(:,:)
       real*8,allocatable::U214(:,:)
       real*8,allocatable::U218(:,:)
       real*8,allocatable::U220(:,:)
       real*8,allocatable::U222(:,:)
       real*8,allocatable::U223(:,:)
       real*8,allocatable::U230(:,:)
       real*8,allocatable::U231(:,:)
       real*8,allocatable::U232(:,:)
       real*8,allocatable::U233(:,:)
       real*8,allocatable::U235(:,:)
       real*8,allocatable::U251(:,:)
       real*8,allocatable::U252(:,:)
       real*8,allocatable::U253(:,:)
       real*8,allocatable::U257(:,:)
       real*8,allocatable::U256(:,:)
       real*8,allocatable::U266(:,:)
       real*8,allocatable::U278(:,:)
       real*8,allocatable::U258(:,:)
       real*8,allocatable::U263(:,:)
       real*8,allocatable::U259(:,:)
       real*8,allocatable::U275(:,:)
       real*8,allocatable::U277(:,:)
       real*8,allocatable::U260(:,:)
       real*8,allocatable::U261(:,:)
       real*8,allocatable::U262(:,:)
       real*8,allocatable::U265(:,:)
       real*8,allocatable::U274(:,:)
       real*8,allocatable::U276(:,:)
       real*8,allocatable::U270(:,:)
       real*8,allocatable::U271(:,:)
       real*8,allocatable::U273(:,:)
       real*8,allocatable::U280(:,:)
       real*8,allocatable::U282(:,:)
       real*8,allocatable::U285(:,:)
       real*8,allocatable::U288(:,:)
       real*8,allocatable::U290(:,:)
       real*8,allocatable::U292(:,:)
       real*8,allocatable::U294(:,:)
       real*8,allocatable::U297(:,:)
       real*8,allocatable::U300(:,:)
       real*8,allocatable::U302(:,:)
       real*8,allocatable::U304(:,:)
       real*8,allocatable::U306(:,:)
       real*8,allocatable::U308(:,:)
       real*8,allocatable::U311(:,:)
       real*8,allocatable::U314(:,:)
       real*8,allocatable::U316(:,:)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,l2A,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(Q7(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,D1,D2,Q7)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N0+1:N1,N0+1:N1))
       call relef21(N0,N1,N0,N1,
     & N0,N1,N0,N1,Q7,B2)
       allocate(U8(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K1*K1
       call jungemm1(I1,I3,D1,B2,U8)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A-1.0d0/2*U8
       deallocate(U8)
       deallocate(Q7)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,l2A,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(Q9(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,D2,Q9)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(U10(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K3*K3
       call jungemm1(I1,I3,D1,Q9,U10)
       deallocate(D1)
C
       V1A=V1A+1.0d0/2*U10
       deallocate(U10)
       deallocate(Q9)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,l2A,D1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N0,N1,N1,N3,t3A,F2)
       allocate(Q11(N0+1:N1,N1+1:N3))
       I2=K3*K1
       I3=K3*K3*K1*K1
       call jungemm2(I2,I3,D1,F2,Q11)
       deallocate(D1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(U12(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K3*K1
       call jungemm1(I1,I3,D1,Q11,U12)
       deallocate(D1)
C
       V1A=V1A+1.0d0/4*U12
       deallocate(U12)
       deallocate(Q11)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,l2A,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(Q13(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,D1,D2,Q13)
       deallocate(D1)
       deallocate(D2)
C
       allocate(B1(N0+1:N1,N0+1:N1))
       call relef21(N0,N1,N0,N1,
     & N0,N1,N0,N1,Q13,B1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q14(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,B1,B2,Q14)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q13)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,Q14,B2)
       allocate(U15(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,U15)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A-1.0d0/2*U15
       deallocate(U15)
       deallocate(Q14)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,l2A,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(Q16(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,D2,Q16)
       deallocate(D1)
       deallocate(D2)
C
       allocate(B1(N1+1:N3,N1+1:N3))
       call relef21(N1,N3,N1,N3,
     & N1,N3,N1,N3,Q16,B1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(Q17(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,B1,B2,Q17)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q16)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(U18(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K3*K1
       call jungemm1(I1,I3,D1,Q17,U18)
       deallocate(D1)
C
       V1A=V1A-1.0d0/2*U18
       deallocate(U18)
       deallocate(Q17)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,l2A,D1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef562341(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N0,N2,N2,N3,t3B,F2)
       allocate(Q19(N0+1:N2,N2+1:N3))
       I2=K4*K2
       I3=K3*K3*K1*K1
       call jungemm2(I2,I3,D1,F2,Q19)
       deallocate(D1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(U20(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K4*K2
       call jungemm1(I1,I3,D1,Q19,U20)
       deallocate(D1)
C
       V1A=V1A+1.0d0/4*U20
       deallocate(U20)
       deallocate(Q19)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,l2B,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(Q23(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,D1,D2,Q23)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N0+1:N1,N0+1:N1))
       call relef21(N0,N1,N0,N1,
     & N0,N1,N0,N1,Q23,B2)
       allocate(U24(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K1*K1
       call jungemm1(I1,I3,D1,B2,U24)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A-U24
       deallocate(U24)
C
       allocate(B1(N0+1:N1,N0+1:N1))
       call relef21(N0,N1,N0,N1,
     & N0,N1,N0,N1,Q23,B1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q29(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,B1,B2,Q29)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q23)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,Q29,B2)
       allocate(U30(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,U30)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A-U30
       deallocate(U30)
       deallocate(Q29)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,l2B,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2B,D2)
       allocate(Q25(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,Q25)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(U26(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K3*K3
       call jungemm1(I1,I3,D1,Q25,U26)
       deallocate(D1)
C
       V1A=V1A+U26
       deallocate(U26)
C
       allocate(B1(N1+1:N3,N1+1:N3))
       call relef21(N1,N3,N1,N3,
     & N1,N3,N1,N3,Q25,B1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(Q31(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,B1,B2,Q31)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q25)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(U32(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K3*K1
       call jungemm1(I1,I3,D1,Q31,U32)
       deallocate(D1)
C
       V1A=V1A-U32
       deallocate(U32)
       deallocate(Q31)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,l2B,D1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N0,N1,N1,N3,t3B,F2)
       allocate(Q27(N0+1:N1,N1+1:N3))
       I2=K3*K1
       I3=K3*K4*K1*K2
       call jungemm2(I2,I3,D1,F2,Q27)
       deallocate(D1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(U28(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K3*K1
       call jungemm1(I1,I3,D1,Q27,U28)
       deallocate(D1)
C
       V1A=V1A+U28
       deallocate(U28)
       deallocate(Q27)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,l2B,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(Q33(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,D1,D2,Q33)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N1,N3,N0,N1,IntM,D1)
       allocate(B2(N0+1:N2,N0+1:N2))
       call relef21(N0,N2,N0,N2,
     & N0,N2,N0,N2,Q33,B2)
       allocate(U34(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K2*K2
       call jungemm1(I1,I3,D1,B2,U34)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A-U34
       deallocate(U34)
C
       allocate(B1(N0+1:N2,N0+1:N2))
       call relef21(N0,N2,N0,N2,
     & N0,N2,N0,N2,Q33,B1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q39(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,B1,B2,Q39)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q33)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,Q39,B2)
       allocate(U40(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,U40)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A-U40
       deallocate(U40)
       deallocate(Q39)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,l2B,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,t2B,D2)
       allocate(Q35(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,D2,Q35)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(B2(N2+1:N3,N2+1:N3))
       call relef21(N2,N3,N2,N3,
     & N2,N3,N2,N3,Q35,B2)
       allocate(U36(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K4*K4
       call jungemm1(I1,I3,D1,B2,U36)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A+U36
       deallocate(U36)
C
       allocate(B1(N2+1:N3,N2+1:N3))
       call relef21(N2,N3,N2,N3,
     & N2,N3,N2,N3,Q35,B1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(Q41(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,B1,B2,Q41)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q35)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(U42(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K4*K2
       call jungemm1(I1,I3,D1,Q41,U42)
       deallocate(D1)
C
       V1A=V1A-U42
       deallocate(U42)
       deallocate(Q41)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,l2B,D1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef461352(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N0,N2,N2,N3,t3C,F2)
       allocate(Q37(N0+1:N2,N2+1:N3))
       I2=K4*K2
       I3=K3*K4*K1*K2
       call jungemm2(I2,I3,D1,F2,Q37)
       deallocate(D1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(U38(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K4*K2
       call jungemm1(I1,I3,D1,Q37,U38)
       deallocate(D1)
C
       V1A=V1A+U38
       deallocate(U38)
       deallocate(Q37)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,l2C,D1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N0,N1,N1,N3,t3C,F2)
       allocate(Q43(N0+1:N1,N1+1:N3))
       I2=K3*K1
       I3=K4*K4*K2*K2
       call jungemm2(I2,I3,D1,F2,Q43)
       deallocate(D1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(U44(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K3*K1
       call jungemm1(I1,I3,D1,Q43,U44)
       deallocate(D1)
C
       V1A=V1A+1.0d0/4*U44
       deallocate(U44)
       deallocate(Q43)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,l2C,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(Q45(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,D1,D2,Q45)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N1,N3,N0,N1,IntM,D1)
       allocate(B2(N0+1:N2,N0+1:N2))
       call relef21(N0,N2,N0,N2,
     & N0,N2,N0,N2,Q45,B2)
       allocate(U46(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K2*K2
       call jungemm1(I1,I3,D1,B2,U46)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A-1.0d0/2*U46
       deallocate(U46)
       deallocate(Q45)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,l2C,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(Q47(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,D2,Q47)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(U48(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K4*K4
       call jungemm1(I1,I3,D1,Q47,U48)
       deallocate(D1)
C
       V1A=V1A+1.0d0/2*U48
       deallocate(U48)
       deallocate(Q47)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,l2C,D1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef451263(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N0,N2,N2,N3,t3D,F2)
       allocate(Q49(N0+1:N2,N2+1:N3))
       I2=K4*K2
       I3=K4*K4*K2*K2
       call jungemm2(I2,I3,D1,F2,Q49)
       deallocate(D1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(U50(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K4*K2
       call jungemm1(I1,I3,D1,Q49,U50)
       deallocate(D1)
C
       V1A=V1A+1.0d0/4*U50
       deallocate(U50)
       deallocate(Q49)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,l2C,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(Q51(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,D1,D2,Q51)
       deallocate(D1)
       deallocate(D2)
C
       allocate(B1(N0+1:N2,N0+1:N2))
       call relef21(N0,N2,N0,N2,
     & N0,N2,N0,N2,Q51,B1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q52(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,B1,B2,Q52)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q51)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,Q52,B2)
       allocate(U53(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,U53)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A-1.0d0/2*U53
       deallocate(U53)
       deallocate(Q52)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,l2C,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(Q54(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,D2,Q54)
       deallocate(D1)
       deallocate(D2)
C
       allocate(B1(N2+1:N3,N2+1:N3))
       call relef21(N2,N3,N2,N3,
     & N2,N3,N2,N3,Q54,B1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(Q55(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,B1,B2,Q55)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q54)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(U56(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K4*K2
       call jungemm1(I1,I3,D1,Q55,U56)
       deallocate(D1)
C
       V1A=V1A-1.0d0/2*U56
       deallocate(U56)
       deallocate(Q55)
C
       allocate(F1(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,l3A,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,t3A,F2)
       allocate(Q57(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K3*K3*K1*K1*K1
       call jungemm(I1,I2,I3,F1,F2,Q57)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q77(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q77)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(N1+1:N3,N0+1:N1))
       X1=0.0d0
       X1=X1+Q77
C
       allocate(X2(N1+1:N3,N0+1:N1))
       X2=0.0d0
       X2=X2+Q77
C
       allocate(X3(N1+1:N3,N0+1:N1))
       X3=0.0d0
       X3=X3+Q77
       deallocate(Q77)
C
       call slx12(0,N3,N1,N3,N0,N1,X1,FockR,1.000d0)
C
       allocate(U58(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3
       call jungemm(I1,I2,I3,X1,Q57,U58)
C
       V1A=V1A-1.0d0/12*U58
       deallocate(U58)
       deallocate(Q57)
       deallocate(X1)
C
       call slx12(0,N3,N1,N3,N0,N1,X3,FockR,1.000d0)
C
       allocate(F1(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef456123(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef456123(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,t3C,F2)
       allocate(Q225(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K4*K4*K1*K2*K2
       call jungemm(I1,I2,I3,F1,F2,Q225)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q68(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q68)
       deallocate(D1)
       deallocate(B2)
C
       X2=X2-Q68
C
       allocate(U243(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3
       call jungemm(I1,I2,I3,X2,Q225,U243)
C
       V1A=V1A-1.0d0/4*U243
       deallocate(U243)
       deallocate(Q225)
       deallocate(X2)
C
       allocate(F1(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,t3B,F2)
       allocate(Q126(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K3*K4*K1*K1*K2
       call jungemm(I1,I2,I3,F1,F2,Q126)
       deallocate(F1)
       deallocate(F2)
C
       allocate(U127(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3
       call jungemm(I1,I2,I3,Q68,Q126,U127)
C
       V1A=V1A+1.0d0/2*U127
       deallocate(U127)
       deallocate(Q126)
C
       allocate(F1(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,l3A,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,t3A,F2)
       allocate(Q69(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K3*K3*K1*K1*K1
       call jungemm(I1,I2,I3,F1,F2,Q69)
       deallocate(F1)
       deallocate(F2)
C
       allocate(U70(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3
       call jungemm(I1,I2,I3,Q68,Q69,U70)
C
       V1A=V1A+1.0d0/12*U70
       deallocate(U70)
       deallocate(Q69)
C
       allocate(F1(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,t3B,F2)
       allocate(Q115(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K3*K4*K1*K1*K2
       call jungemm(I1,I2,I3,F1,F2,Q115)
       deallocate(F1)
       deallocate(F2)
C
       allocate(U148(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3
       call jungemm(I1,I2,I3,X3,Q115,U148)
C
       V1A=V1A-1.0d0/2*U148
       deallocate(U148)
       deallocate(X3)
       deallocate(Q115)
C
       allocate(F1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       call relef412563(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,N0,N1,N1,N3,l3A,F1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S1(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1*K1
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,F1,D2,S1)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef2314(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S11(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S11)
       deallocate(D1)
       deallocate(B2)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N0,N1,N1,N3,l3A,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N0,N1,N1,N3,t3A,F2)
       allocate(S12(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K3*K1
       I3=K3*K3*K1*K1
       call jungemm(I1,I2,I3,F1,F2,S12)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3124(N0,N1,N1,N3,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,S12,D2)
       allocate(U66(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,S11,D2,U66)
       deallocate(D2)
C
       V1A=V1A-1.0d0/4*U66
       deallocate(U66)
       deallocate(S12)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N0,N1,N1,N3,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N0,N1,N1,N3,t3C,F2)
       allocate(S128(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K3*K1
       I3=K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S128)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3124(N0,N1,N1,N3,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,S128,D2)
       allocate(U224(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,S11,D2,U224)
       deallocate(D2)
C
       V1A=V1A-1.0d0/4*U224
       deallocate(U224)
       deallocate(S128)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef451623(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,N1,N1,N3,N1,N3,l3A,F1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(S8(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3*K1
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,F1,D2,S8)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1324(N0,N1,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,N0,N1,N0,N1,S11,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef2134(N1,N3,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,S8,D2)
       allocate(S18(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K3*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S18)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X7(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       X7=0.0d0
       call sul2314(N0,N1,N1,N3,N1,N3,N0,N1,X7,S18,1.000d0)
       deallocate(S18)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N1,N3,t2A,D2)
       allocate(S15(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K3*K1
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S15)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X5(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       X5=0.0d0
       call sul1324(N0,N1,N1,N3,N1,N3,N0,N1,X5,S15,1.000d0)
C
       allocate(X6(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       X6=0.0d0
       call sul1324(N0,N1,N1,N3,N1,N3,N0,N1,X6,S15,1.000d0)
       deallocate(S15)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef451623(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,N1,N1,N3,N1,N3,l3B,F1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2B,D2)
       allocate(S51(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3*K1
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,F1,D2,S51)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef2134(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S9(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S9)
       deallocate(D1)
       deallocate(B2)
C
       X6=X6+S9
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef451623(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,N1,N1,N3,N1,N3,l3A,F1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(S10(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3*K1
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,F1,D2,S10)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef2134(N1,N3,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,S10,D2)
       allocate(U65(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,S9,D2,U65)
       deallocate(D2)
C
       V1A=V1A-1.0d0/2*U65
       deallocate(U65)
       deallocate(S10)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S7(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S7)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N1,N1,N3,N1,N3,N0,N1,X5,S7,1.000d0)
C
       call sul3124(N0,N1,N1,N3,N1,N3,N0,N1,X6,S7,1.000d0)
       deallocate(S7)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef1324(N0,N1,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,X5,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef2314(N1,N3,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,S8,D2)
       allocate(U71(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,D1,D2,U71)
       deallocate(D1)
       deallocate(D2)
C
       V1A=V1A-1.0d0/2*U71
       deallocate(U71)
       deallocate(S8)
       deallocate(X5)
C
       call slx1234(N0,N3,N0,N1,N1,N3,N1,N3,N0,N1,X6,IntR,-1.000d0)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef1324(N0,N1,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,X6,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef2314(N1,N3,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,S51,D2)
       allocate(U130(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,D1,D2,U130)
       deallocate(D1)
       deallocate(D2)
C
       V1A=V1A-U130
       deallocate(U130)
       deallocate(X6)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1324(N0,N1,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,N0,N1,N0,N1,S11,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef2134(N1,N3,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,S51,D2)
       allocate(S58(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K3*K3
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S58)
       deallocate(D1)
       deallocate(D2)
       deallocate(S51)
C
       call sul2314(N0,N1,N1,N3,N1,N3,N0,N1,X7,S58,2.000d0)
       deallocate(S58)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N0,N1,N1,N3,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N0,N1,N1,N3,t3B,F2)
       allocate(S54(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K3*K1
       I3=K3*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S54)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3124(N0,N1,N1,N3,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,S54,D2)
       allocate(U124(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,S11,D2,U124)
       deallocate(D2)
C
       V1A=V1A-U124
       deallocate(U124)
       deallocate(S54)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef3124(N0,N1,N0,N1,N1,N3,N0,N1,
     & N1,N3,N0,N1,N0,N1,N0,N1,S11,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S17(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S17)
       deallocate(D1)
       deallocate(B2)
       deallocate(S11)
C
       allocate(X4(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       X4=0.0d0
       call sul2134(N0,N1,N0,N1,N0,N1,N0,N1,X4,S17,1.000d0)
C
       allocate(X8(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       X8=0.0d0
       call sul2134(N0,N1,N0,N1,N0,N1,N0,N1,X8,S17,2.000d0)
       deallocate(S17)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N0,N1,N0,N1,IntR,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(S16(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K1
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,D2,S16)
       deallocate(D1)
       deallocate(D2)
C
       call sul2134(N0,N1,N0,N1,N0,N1,N0,N1,X4,S16,1.0d0/2)
C
       call sul2134(N0,N1,N0,N1,N0,N1,N0,N1,X8,S16,1.000d0)
       deallocate(S16)
C
       call slx2134(N0,N3,N0,N1,N0,N1,N0,N1,N0,N1,X4,IntR,1.000d0)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef2134(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,X4,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       call relef2314(N0,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N0,N1,N1,N3,S1,D2)
       allocate(U59(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K1*K1*K1
       call jungemm(I1,I2,I3,D1,D2,U59)
       deallocate(D1)
       deallocate(D2)
C
       V1A=V1A+1.0d0/4*U59
       deallocate(U59)
       deallocate(S1)
       deallocate(X4)
C
       allocate(F1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       call relef412563(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,N0,N1,N1,N3,l3B,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(S50(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1*K1
       I2=K1
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S50)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef2134(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N0,N1,N0,N1,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S5(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S5)
       deallocate(D1)
       deallocate(B2)
C
       call sul2134(N0,N1,N0,N1,N0,N1,N0,N1,X8,S5,-4.000d0)
C
       call slx2134(N0,N3,N0,N1,N0,N1,N0,N1,N0,N1,X8,IntR,2.000d0)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef2134(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,X8,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       call relef2314(N0,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N0,N1,N1,N3,S50,D2)
       allocate(U129(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K1*K1*K1
       call jungemm(I1,I2,I3,D1,D2,U129)
       deallocate(D1)
       deallocate(D2)
C
       V1A=V1A+1.0d0/4*U129
       deallocate(U129)
       deallocate(S50)
       deallocate(X8)
C
       allocate(F1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       call relef412563(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,N0,N1,N1,N3,l3A,F1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S6(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1*K1
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,F1,D2,S6)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D2(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       call relef2314(N0,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N0,N1,N1,N3,S6,D2)
       allocate(U63(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K1*K1*K1
       call jungemm(I1,I2,I3,S5,D2,U63)
       deallocate(D2)
C
       V1A=V1A-1.0d0/2*U63
       deallocate(U63)
       deallocate(S6)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef451623(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,N1,N1,N3,N1,N3,l3A,F1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(S2(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3*K1
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,F1,D2,S2)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef2134(N1,N3,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,S2,D2)
       allocate(U60(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,D1,D2,U60)
       deallocate(D1)
       deallocate(D2)
C
       V1A=V1A+1.0d0/2*U60
       deallocate(U60)
       deallocate(S2)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N0,N1,N1,N3,l3A,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N0,N1,N1,N3,t3A,F2)
       allocate(S3(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K3*K1
       I3=K3*K3*K1*K1
       call jungemm(I1,I2,I3,F1,F2,S3)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3124(N0,N1,N1,N3,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,S3,D2)
       allocate(U61(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,D2,U61)
       deallocate(D1)
       deallocate(D2)
C
       V1A=V1A+1.0d0/4*U61
       deallocate(U61)
       deallocate(S3)
C
       allocate(F1(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,l3A,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,t3A,F2)
       allocate(S4(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K3*K3
       I3=K3*K1*K1*K1
       call jungemm(I1,I2,I3,F1,F2,S4)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(U62(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3*K3*K3
       call jungemm(I1,I2,I3,D1,S4,U62)
       deallocate(D1)
C
       V1A=V1A-1.0d0/12*U62
       deallocate(U62)
       deallocate(S4)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S13(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S13)
       deallocate(D1)
       deallocate(B2)
C
       allocate(F1(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,l3A,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,t3A,F2)
       allocate(S14(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K3*K3
       I3=K3*K1*K1*K1
       call jungemm(I1,I2,I3,F1,F2,S14)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D2(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef3124(N1,N3,N1,N3,N1,N3,N1,N3,
     & N1,N3,N1,N3,N1,N3,N1,N3,S14,D2)
       allocate(U67(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3*K3*K3
       call jungemm(I1,I2,I3,S13,D2,U67)
       deallocate(D2)
C
       V1A=V1A+1.0d0/12*U67
       deallocate(U67)
       deallocate(S14)
C
       allocate(F1(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,t3B,F2)
       allocate(S55(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K3*K3
       I3=K4*K1*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S55)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D2(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef3124(N1,N3,N1,N3,N1,N3,N1,N3,
     & N1,N3,N1,N3,N1,N3,N1,N3,S55,D2)
       allocate(U125(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3*K3*K3
       call jungemm(I1,I2,I3,S13,D2,U125)
       deallocate(D2)
C
       V1A=V1A+1.0d0/4*U125
       deallocate(U125)
       deallocate(S55)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N0,N1,N1,N3,l3A,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef562341(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N0,N2,N2,N3,t3B,F2)
       allocate(S19(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K4*K2
       I3=K3*K3*K1*K1
       call jungemm(I1,I2,I3,F1,F2,S19)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N0,N1,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef1324(N0,N2,N2,N3,N0,N1,N1,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,S19,D2)
       allocate(U75(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U75)
       deallocate(D1)
       deallocate(D2)
C
       V1A=V1A-1.0d0/4*U75
       deallocate(U75)
       deallocate(S19)
C
       allocate(D1(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       call relef2314(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N2,N2,N3,N0,N1,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S20(N0+1:N1,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S20)
       deallocate(D1)
       deallocate(B2)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N0,N1,N1,N3,l3A,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef562341(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N0,N2,N2,N3,t3B,F2)
       allocate(S21(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K4*K2
       I3=K3*K3*K1*K1
       call jungemm(I1,I2,I3,F1,F2,S21)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef2134(N0,N1,N0,N2,N2,N3,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,N1,S20,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef1324(N0,N2,N2,N3,N0,N1,N1,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,S21,D2)
       allocate(U76(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U76)
       deallocate(D1)
       deallocate(D2)
C
       V1A=V1A-1.0d0/4*U76
       deallocate(U76)
       deallocate(S21)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef451623(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N0,N1,N2,N3,N1,N3,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(S142(N2+1:N3,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4*K1
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,F1,D2,S142)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef1324(N0,N1,N0,N2,N2,N3,N0,N1,
     & N0,N1,N2,N3,N0,N2,N0,N1,S20,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef2134(N2,N3,N0,N1,N2,N3,N1,N3,
     & N0,N1,N2,N3,N2,N3,N1,N3,S142,D2)
       allocate(S146(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K3*K4
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,D2,S146)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X14(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       X14=0.0d0
       call sul2314(N0,N2,N2,N3,N1,N3,N0,N1,X14,S146,1.0d0/2)
       deallocate(S146)
C
       allocate(D1(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
       call relef3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N2,N3,N0,N1,IntM,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N0+1:N1,N2+1:N3))
       call relef3241(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N0,N1,N2,N3,t2B,D2)
       allocate(S77(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K4*K1
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,D2,S77)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X9(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       X9=0.0d0
       call sul1324(N0,N1,N2,N3,N2,N3,N0,N1,X9,S77,1.000d0)
C
       allocate(X10(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       X10=0.0d0
       X10=X10+S77
       deallocate(S77)
C
       call slx3124(N0,N3,N0,N1,N2,N3,N2,N3,N0,N1,X10,IntM,-1.000d0)
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
       allocate(D2(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef2314(N2,N3,N0,N1,N2,N3,N1,N3,
     & N0,N1,N2,N3,N2,N3,N1,N3,S61,D2)
       allocate(U151(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K4*K4*K1
       call jungemm(I1,I2,I3,X10,D2,U151)
       deallocate(D2)
C
       V1A=V1A+U151
       deallocate(U151)
       deallocate(X10)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef2134(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N0,N2,N0,N1,IntM,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef2134(N2,N3,N0,N1,N2,N3,N1,N3,
     & N0,N1,N2,N3,N2,N3,N1,N3,S61,D2)
       allocate(S71(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K3*K4
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,D2,S71)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N0,N2,N2,N3,N1,N3,N0,N1,X14,S71,1.000d0)
       deallocate(S71)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef1324(N0,N1,N0,N2,N2,N3,N0,N1,
     & N0,N1,N2,N3,N0,N2,N0,N1,S20,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef2134(N2,N3,N0,N1,N2,N3,N1,N3,
     & N0,N1,N2,N3,N2,N3,N1,N3,S61,D2)
       allocate(S80(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K3*K4
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,D2,S80)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N0,N2,N2,N3,N1,N3,N0,N1,X14,S80,1.000d0)
       deallocate(S80)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N0,N1,N1,N3,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef451263(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N0,N2,N2,N3,t3D,F2)
       allocate(S138(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K4*K2
       I3=K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S138)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef2134(N0,N1,N0,N2,N2,N3,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,N1,S20,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef1324(N0,N2,N2,N3,N0,N1,N1,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,S138,D2)
       allocate(U236(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U236)
       deallocate(D1)
       deallocate(D2)
C
       V1A=V1A-1.0d0/4*U236
       deallocate(U236)
       deallocate(S138)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N0,N1,N1,N3,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef461352(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N0,N2,N2,N3,t3C,F2)
       allocate(S67(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K4*K2
       I3=K3*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S67)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef2134(N0,N1,N0,N2,N2,N3,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,N1,S20,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef1324(N0,N2,N2,N3,N0,N1,N1,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,S67,D2)
       allocate(U141(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U141)
       deallocate(D1)
       deallocate(D2)
C
       V1A=V1A-U141
       deallocate(U141)
       deallocate(S67)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       call relef3214(N0,N1,N0,N2,N2,N3,N0,N1,
     & N2,N3,N0,N2,N0,N1,N0,N1,S20,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S79(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S79)
       deallocate(D1)
       deallocate(B2)
       deallocate(S20)
C
       allocate(X11(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
       X11=0.0d0
       call sul2134(N0,N2,N0,N2,N0,N1,N0,N1,X11,S79,1.000d0)
C
       allocate(X12(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
       X12=0.0d0
       X12=X12+S79
       deallocate(S79)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       call relef412563(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N0,N2,N0,N1,N1,N3,l3C,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S130(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1*K2
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S130)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N0,N2,N0,N1,IntM,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,t2B,D2)
       allocate(S78(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K1*K2
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,D2,S78)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N0,N2,N0,N2,N0,N1,N0,N1,X11,S78,1.000d0)
C
       call sul1324(N0,N2,N0,N2,N0,N1,N0,N1,X12,S78,1.000d0)
       deallocate(S78)
C
       call slx2314(N0,N3,N0,N2,N0,N2,N0,N1,N0,N1,X11,IntM,1.000d0)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
       call relef2134(N0,N2,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N2,N0,N1,N0,N1,X11,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       call relef2134(N0,N2,N0,N2,N0,N1,N1,N3,
     & N0,N2,N0,N2,N0,N1,N1,N3,S130,D2)
       allocate(U247(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K1*K2*K2
       call jungemm(I1,I2,I3,D1,D2,U247)
       deallocate(D1)
       deallocate(D2)
C
       V1A=V1A+1.0d0/2*U247
       deallocate(U247)
       deallocate(S130)
       deallocate(X11)
C
       allocate(F1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       call relef512463(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,N0,N1,N1,N3,l3B,F1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(S60(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1*K2
       I2=K2
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,F1,D2,S60)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N0,N1,N0,N1,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S70(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S70)
       deallocate(D1)
       deallocate(B2)
C
       X12=X12+S70
C
       allocate(X13(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
       X13=0.0d0
       call sul2134(N0,N2,N0,N2,N0,N1,N0,N1,X13,S70,1.000d0)
       deallocate(S70)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       call relef412563(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N0,N2,N0,N1,N1,N3,l3C,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S135(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1*K2
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S135)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       call relef2134(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N2,N0,N2,N0,N1,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S65(N0+1:N1,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S65)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N2,N0,N2,N0,N1,N0,N1,X12,S65,1.000d0)
C
       call sul3214(N0,N2,N0,N2,N0,N1,N0,N1,X13,S65,1.000d0)
       deallocate(S65)
C
       call slx1324(N0,N3,N0,N2,N0,N2,N0,N1,N0,N1,X12,IntM,1.000d0)
C
       allocate(D2(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       call relef2134(N0,N2,N0,N2,N0,N1,N1,N3,
     & N0,N2,N0,N2,N0,N1,N1,N3,S60,D2)
       allocate(U152(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K1*K2*K2
       call jungemm(I1,I2,I3,X12,D2,U152)
       deallocate(D2)
C
       V1A=V1A+U152
       deallocate(U152)
       deallocate(S60)
       deallocate(X12)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
       call relef2134(N0,N2,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N2,N0,N1,N0,N1,X13,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       call relef2134(N0,N2,N0,N2,N0,N1,N1,N3,
     & N0,N2,N0,N2,N0,N1,N1,N3,S135,D2)
       allocate(U239(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K1*K2*K2
       call jungemm(I1,I2,I3,D1,D2,U239)
       deallocate(D1)
       deallocate(D2)
C
       V1A=V1A+1.0d0/2*U239
       deallocate(U239)
       deallocate(S135)
       deallocate(X13)
C
       allocate(D1(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef3124(N2,N3,N0,N1,N2,N3,N1,N3,
     & N2,N3,N2,N3,N0,N1,N1,N3,S61,D2)
       allocate(S66(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K3*K1
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,D2,S66)
       deallocate(D1)
       deallocate(D2)
       deallocate(S61)
C
       call sul1324(N0,N1,N1,N3,N1,N3,N0,N1,X7,S66,-2.000d0)
       deallocate(S66)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N0,N1,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S141(N2+1:N3,N0+1:N1,N2+1:N3,N0+1:N1))
       I1=K1*K4*K1
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S141)
       deallocate(D1)
       deallocate(B2)
C
       call sul3124(N0,N1,N2,N3,N2,N3,N0,N1,X9,S141,1.000d0)
       deallocate(S141)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       call relef1324(N0,N1,N2,N3,N2,N3,N0,N1,
     & N0,N1,N2,N3,N2,N3,N0,N1,X9,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef2314(N2,N3,N0,N1,N2,N3,N1,N3,
     & N0,N1,N2,N3,N2,N3,N1,N3,S142,D2)
       allocate(U246(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K4*K4*K1
       call jungemm(I1,I2,I3,D1,D2,U246)
       deallocate(D1)
       deallocate(D2)
C
       V1A=V1A+1.0d0/2*U246
       deallocate(U246)
       deallocate(S142)
       deallocate(X9)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N1,N3,t2B,D2)
       allocate(S22(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K3*K1
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S22)
       deallocate(D1)
       deallocate(D2)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef562413(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,N1,N1,N3,N1,N3,l3A,F1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(S23(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3*K1
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,F1,D2,S23)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef2314(N1,N3,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,S23,D2)
       allocate(U79(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,S22,D2,U79)
       deallocate(D2)
C
       V1A=V1A-1.0d0/2*U79
       deallocate(U79)
       deallocate(S23)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef461523(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,N1,N1,N3,N1,N3,l3B,F1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2B,D2)
       allocate(S76(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3*K1
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,F1,D2,S76)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef2314(N1,N3,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,S76,D2)
       allocate(U150(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,S22,D2,U150)
       deallocate(D2)
C
       V1A=V1A-U150
       deallocate(U150)
       deallocate(S76)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,l3A,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,t3A,F2)
       allocate(Q80(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K3*K3*K1*K1
       call jungemm(I1,I2,I3,F1,F2,Q80)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N1,N1,N3,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q100(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q100)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X15(N0+1:N1,N1+1:N3))
       X15=0.0d0
       X15=X15+Q100
C
       allocate(X16(N0+1:N1,N1+1:N3))
       X16=0.0d0
       X16=X16+Q100
C
       allocate(X17(N0+1:N1,N1+1:N3))
       X17=0.0d0
       X17=X17+Q100
       deallocate(Q100)
C
       call slx21(0,N3,N0,N1,N1,N3,X15,FockR,1.000d0)
C
       allocate(U81(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K1
       call jungemm(I1,I2,I3,X15,Q80,U81)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U81,-1.0d0/12)
       deallocate(U81)
       deallocate(Q80)
       deallocate(X15)
C
       call slx21(0,N3,N0,N1,N1,N3,X17,FockR,1.000d0)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,t3C,F2)
       allocate(Q254(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,Q254)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef4132(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N1,N1,N3,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q92(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q92)
       deallocate(D1)
       deallocate(B2)
C
       X16=X16-Q92
C
       allocate(U272(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K1
       call jungemm(I1,I2,I3,X16,Q254,U272)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U272,-1.0d0/4)
       deallocate(U272)
       deallocate(Q254)
       deallocate(X16)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,t3B,F2)
       allocate(Q167(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K3*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,Q167)
       deallocate(F1)
       deallocate(F2)
C
       allocate(U168(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K1
       call jungemm(I1,I2,I3,Q92,Q167,U168)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U168,1.0d0/2)
       deallocate(U168)
       deallocate(Q167)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,l3A,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,t3A,F2)
       allocate(Q91(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K3*K3*K1*K1
       call jungemm(I1,I2,I3,F1,F2,Q91)
       deallocate(F1)
       deallocate(F2)
C
       allocate(U93(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K1
       call jungemm(I1,I2,I3,Q92,Q91,U93)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U93,1.0d0/12)
       deallocate(U93)
       deallocate(Q91)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,t3B,F2)
       allocate(Q156(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K3*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,Q156)
       deallocate(F1)
       deallocate(F2)
C
       allocate(U189(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K1
       call jungemm(I1,I2,I3,X17,Q156,U189)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U189,-1.0d0/2)
       deallocate(U189)
       deallocate(X17)
       deallocate(Q156)
C
       allocate(F1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef412536(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,N1,N3,N0,N1,l3A,F1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S24(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,F1,D2,S24)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef2134(N0,N1,N0,N1,N1,N3,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,N1,S24,D2)
       allocate(U82(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,D2,U82)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U82,-1.0d0/2)
       deallocate(U82)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef3214(N0,N1,N0,N1,N1,N3,N0,N1,
     & N1,N3,N0,N1,N0,N1,N0,N1,S24,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S28(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S28)
       deallocate(D1)
       deallocate(B2)
       deallocate(S24)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N0,N1,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef2314(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,S28,D2)
       allocate(U86(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K1*K1*K1
       call jungemm(I1,I2,I3,D1,D2,U86)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U86,1.0d0/2)
       deallocate(U86)
       deallocate(S28)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,l3A,F1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(S25(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,F1,D2,S25)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N1,N3,N1,N3,IntR,D1)
       allocate(U83(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K3*K3
       call jungemm(I1,I2,I3,D1,S25,U83)
       deallocate(D1)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U83,1.0d0/4)
       deallocate(U83)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef2314(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N1,N3,N1,N3,N0,N1,S25,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S42(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S42)
       deallocate(D1)
       deallocate(B2)
       deallocate(S25)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef2134(N0,N1,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,N1,N1,N3,N0,N1,S42,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S43(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S43)
       deallocate(D1)
       deallocate(B2)
       deallocate(S42)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef2134(N0,N1,N0,N1,N1,N3,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,N1,S43,D2)
       allocate(U97(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,D2,U97)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U97,1.0d0/4)
       deallocate(U97)
       deallocate(S43)
C
       allocate(F1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,l3A,F1)
       allocate(F2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,t3A,F2)
       allocate(S26(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K1
       I3=K3*K3*K3*K1
       call jungemm(I1,I2,I3,F1,F2,S26)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N0,N1,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef3124(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,S26,D2)
       allocate(U84(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K1*K1*K1
       call jungemm(I1,I2,I3,D1,D2,U84)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U84,1.0d0/12)
       deallocate(U84)
       deallocate(S26)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,l3A,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N0,N1,N1,N3,t3A,F2)
       allocate(S27(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K3*K1
       I3=K3*K3*K1*K1
       call jungemm(I1,I2,I3,F1,F2,S27)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,IntR,D1)
       allocate(U85(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,D1,S27,U85)
       deallocate(D1)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U85,1.0d0/4)
       deallocate(U85)
       deallocate(S27)
C
       allocate(F1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef412536(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,N1,N3,N0,N1,l3A,F1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S29(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,F1,D2,S29)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1324(N0,N1,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,N0,N1,N0,N1,S29,D2)
       allocate(S30(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K1*K1
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S30)
       deallocate(D1)
       deallocate(D2)
C
       call sul1423(N0,N1,N1,N3,N1,N3,N0,N1,X7,S30,-1.000d0)
       deallocate(S30)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef2314(N0,N1,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,N0,N1,N0,N1,S29,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N1,N3,t2A,D2)
       allocate(S39(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K3*K1
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S39)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef3124(N0,N1,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,N1,S39,D2)
       allocate(U95(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,D2,U95)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U95,-1.0d0/2)
       deallocate(U95)
       deallocate(S39)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef3214(N0,N1,N0,N1,N1,N3,N0,N1,
     & N1,N3,N0,N1,N0,N1,N0,N1,S29,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S40(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S40)
       deallocate(D1)
       deallocate(B2)
       deallocate(S29)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef2314(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,S40,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S41(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S41)
       deallocate(D1)
       deallocate(B2)
       deallocate(S40)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef2314(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,N1,S41,D2)
       allocate(U96(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,D2,U96)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U96,1.0d0/2)
       deallocate(U96)
       deallocate(S41)
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
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef1432(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N0,N1,N1,N3,IntR,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef1324(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N1,N3,N1,N3,N0,N1,S31,D2)
       allocate(S32(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K1*K3
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,D2,S32)
       deallocate(D1)
       deallocate(D2)
       deallocate(S31)
C
       call sul2413(N0,N1,N1,N3,N1,N3,N0,N1,X7,S32,-1.000d0)
       deallocate(S32)
C
       allocate(F1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,l3A,F1)
       allocate(F2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,t3A,F2)
       allocate(S33(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K1
       I3=K3*K3*K3*K1
       call jungemm(I1,I2,I3,F1,F2,S33)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef3124(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,S33,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S34(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S34)
       deallocate(D1)
       deallocate(B2)
       deallocate(S33)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef2314(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,N1,S34,D2)
       allocate(U89(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,D2,U89)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U89,1.0d0/12)
       deallocate(U89)
       deallocate(S34)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,l3A,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N0,N1,N1,N3,t3A,F2)
       allocate(S35(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K3*K1
       I3=K3*K3*K1*K1
       call jungemm(I1,I2,I3,F1,F2,S35)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef3124(N0,N1,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,N1,N1,N3,N0,N1,S35,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S36(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S36)
       deallocate(D1)
       deallocate(B2)
       deallocate(S35)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef2134(N0,N1,N0,N1,N1,N3,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,N1,S36,D2)
       allocate(U90(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,D2,U90)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U90,-1.0d0/4)
       deallocate(U90)
       deallocate(S36)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef453126(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,l3A,F1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(S37(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,F1,D2,S37)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef2314(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N1,N3,N1,N3,N0,N1,S37,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(S38(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K1
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,D2,S38)
       deallocate(D1)
       deallocate(D2)
       deallocate(S37)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(U94(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,S38,U94)
       deallocate(D1)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U94,1.0d0/8)
       deallocate(U94)
       deallocate(S38)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,l3A,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef562341(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N0,N2,N2,N3,t3B,F2)
       allocate(S44(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K4*K2
       I3=K3*K3*K1*K1
       call jungemm(I1,I2,I3,F1,F2,S44)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,IntM,D1)
       allocate(U98(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,D1,S44,U98)
       deallocate(D1)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U98,1.0d0/4)
       deallocate(U98)
       deallocate(S44)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,l3A,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef562341(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N0,N2,N2,N3,t3B,F2)
       allocate(S45(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K4*K2
       I3=K3*K3*K1*K1
       call jungemm(I1,I2,I3,F1,F2,S45)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       call relef3124(N0,N2,N2,N3,N1,N3,N0,N1,
     & N1,N3,N0,N2,N2,N3,N0,N1,S45,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S46(N0+1:N1,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S46)
       deallocate(D1)
       deallocate(B2)
       deallocate(S45)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef2134(N0,N1,N0,N2,N2,N3,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,N1,S46,D2)
       allocate(U99(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U99)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U99,-1.0d0/4)
       deallocate(U99)
       deallocate(S46)
C
       allocate(F1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef523416(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,N1,N3,N0,N1,l3A,F1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S47(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,F1,D2,S47)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef2314(N0,N1,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,N0,N1,N0,N1,S47,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,N3,t2B,D2)
       allocate(S48(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K4*K2
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S48)
       deallocate(D1)
       deallocate(D2)
       deallocate(S47)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef1324(N0,N2,N2,N3,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,N1,S48,D2)
       allocate(U102(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U102)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U102,-1.0d0/2)
       deallocate(U102)
       deallocate(S48)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,l3A,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,t3A,F2)
       allocate(Q103(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K3*K3*K1*K1
       call jungemm(I1,I2,I3,F1,F2,Q103)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N0+1:N1,N0+1:N1))
       call relef21(N0,N1,N0,N1,
     & N0,N1,N0,N1,Q103,B2)
       allocate(U104(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K1*K1
       call jungemm1(I1,I3,D1,B2,U104)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A-1.0d0/12*U104
       deallocate(U104)
       deallocate(Q103)
C
       allocate(F1(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,l3A,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,t3A,F2)
       allocate(Q105(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K3*K3*K1*K1*K1
       call jungemm(I1,I2,I3,F1,F2,Q105)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(U106(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K3*K3
       call jungemm1(I1,I3,D1,Q105,U106)
       deallocate(D1)
C
       V1A=V1A+1.0d0/12*U106
       deallocate(U106)
       deallocate(Q105)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,l3A,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,t3A,F2)
       allocate(Q107(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K3*K3*K1*K1
       call jungemm(I1,I2,I3,F1,F2,Q107)
       deallocate(F1)
       deallocate(F2)
C
       allocate(B1(N0+1:N1,N0+1:N1))
       call relef21(N0,N1,N0,N1,
     & N0,N1,N0,N1,Q107,B1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q108(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,B1,B2,Q108)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q107)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,Q108,B2)
       allocate(U109(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,U109)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A-1.0d0/12*U109
       deallocate(U109)
       deallocate(Q108)
C
       allocate(F1(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,l3A,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,t3A,F2)
       allocate(Q110(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K3*K3*K1*K1*K1
       call jungemm(I1,I2,I3,F1,F2,Q110)
       deallocate(F1)
       deallocate(F2)
C
       allocate(B1(N1+1:N3,N1+1:N3))
       call relef21(N1,N3,N1,N3,
     & N1,N3,N1,N3,Q110,B1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(Q111(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,B1,B2,Q111)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q110)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(U112(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K3*K1
       call jungemm1(I1,I3,D1,Q111,U112)
       deallocate(D1)
C
       V1A=V1A-1.0d0/12*U112
       deallocate(U112)
       deallocate(Q111)
C
       allocate(F1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       call relef412563(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,N0,N1,N1,N3,l3A,F1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S49(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1*K1
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,F1,D2,S49)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef2341(N0,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,S49,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(Q113(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,D2,Q113)
       deallocate(D1)
       deallocate(D2)
       deallocate(S49)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,Q113,B2)
       allocate(U114(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,U114)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A+1.0d0/4*U114
       deallocate(U114)
       deallocate(Q113)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N0,N1,N1,N3,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N0,N1,N1,N3,t3B,F2)
       allocate(S52(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K3*K1
       I3=K3*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S52)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3124(N0,N1,N1,N3,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,S52,D2)
       allocate(U119(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,D2,U119)
       deallocate(D1)
       deallocate(D2)
C
       V1A=V1A+U119
       deallocate(U119)
       deallocate(S52)
C
       allocate(F1(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,t3B,F2)
       allocate(S53(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K3*K3
       I3=K4*K1*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S53)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(U120(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3*K3*K3
       call jungemm(I1,I2,I3,D1,S53,U120)
       deallocate(D1)
C
       V1A=V1A-1.0d0/4*U120
       deallocate(U120)
       deallocate(S53)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,N3,t2B,D2)
       allocate(S56(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K4*K2
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S56)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X18(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       X18=0.0d0
       X18=X18+S56
C
       allocate(X19(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
       X19=0.0d0
       call sul1324(N0,N2,N1,N3,N2,N3,N0,N1,X19,S56,1.000d0)
       deallocate(S56)
C
       call slx1234(N0,N3,N0,N2,N1,N3,N2,N3,N0,N1,X19,IntM,1.000d0)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3,N1+1:N3))
       call relef562413(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,N2,N2,N3,N1,N3,l3B,F1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(S57(N1+1:N3,N0+1:N2,N2+1:N3,N1+1:N3))
       I1=K3*K4*K2
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,F1,D2,S57)
       deallocate(F1)
       deallocate(D2)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N2,N2+1:N3,N1+1:N3))
       call relef461523(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,N2,N2,N3,N1,N3,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2B,D2)
       allocate(S129(N1+1:N3,N0+1:N2,N2+1:N3,N1+1:N3))
       I1=K3*K4*K2
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,F1,D2,S129)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,X19,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef2314(N1,N3,N0,N2,N2,N3,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,S129,D2)
       allocate(U227(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,D1,D2,U227)
       deallocate(D1)
       deallocate(D2)
C
       V1A=V1A-U227
       deallocate(U227)
       deallocate(X19)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N1,N3,N0,N1,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S72(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S72)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X20(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       X20=0.0d0
       X20=X20+S72
C
       call slx1324(N0,N3,N0,N2,N0,N2,N1,N3,N0,N1,X20,IntM,1.000d0)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1324(N0,N2,N0,N2,N1,N3,N0,N1,
     & N0,N2,N1,N3,N0,N2,N0,N1,S72,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:N3))
       call relef2134(N1,N3,N0,N2,N2,N3,N1,N3,
     & N0,N2,N1,N3,N2,N3,N1,N3,S129,D2)
       allocate(S147(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K3*K4
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,D2,S147)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N0,N2,N2,N3,N1,N3,N0,N1,X14,S147,1.000d0)
       deallocate(S147)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N2,N1+1:N3))
       call relef461253(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N0,N2,N1,N3,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N2,N1+1:N3))
       call relef461253(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N0,N2,N1,N3,t3C,F2)
       allocate(S143(N0+1:N2,N1+1:N3,N0+1:N2,N1+1:N3))
       I1=K3*K2
       I2=K3*K2
       I3=K4*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S143)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D2(N0+1:N2,N0+1:N2,N1+1:N3,N1+1:N3))
       call relef3124(N0,N2,N1,N3,N0,N2,N1,N3,
     & N0,N2,N0,N2,N1,N3,N1,N3,S143,D2)
       allocate(U241(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3*K2*K2
       call jungemm(I1,I2,I3,S72,D2,U241)
       deallocate(D2)
C
       V1A=V1A+1.0d0/2*U241
       deallocate(U241)
       deallocate(S143)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1324(N0,N2,N0,N2,N1,N3,N0,N1,
     & N0,N2,N1,N3,N0,N2,N0,N1,S72,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:N3))
       call relef2134(N1,N3,N0,N2,N2,N3,N1,N3,
     & N0,N2,N1,N3,N2,N3,N1,N3,S57,D2)
       allocate(S81(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K3*K4
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,D2,S81)
       deallocate(D1)
       deallocate(D2)
       deallocate(S72)
C
       call sul2314(N0,N2,N2,N3,N1,N3,N0,N1,X14,S81,1.0d0/2)
       deallocate(S81)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N0,N2,N0,N1,IntM,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:N3))
       call relef2134(N1,N3,N0,N2,N2,N3,N1,N3,
     & N0,N2,N1,N3,N2,N3,N1,N3,S57,D2)
       allocate(S68(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K3*K4
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,D2,S68)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N0,N2,N2,N3,N1,N3,N0,N1,X14,S68,1.0d0/2)
       deallocate(S68)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N2,N3,t2C,D2)
       allocate(S75(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K4*K2
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S75)
       deallocate(D1)
       deallocate(D2)
C
       X18=X18+S75
C
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef2314(N1,N3,N0,N2,N2,N3,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,S57,D2)
       allocate(U128(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,X18,D2,U128)
       deallocate(D2)
C
       V1A=V1A-1.0d0/2*U128
       deallocate(U128)
       deallocate(S57)
       deallocate(X18)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N2,N2+1:N3,N1+1:N3))
       call relef562413(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,N2,N2,N3,N1,N3,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2B,D2)
       allocate(S145(N1+1:N3,N0+1:N2,N2+1:N3,N1+1:N3))
       I1=K3*K4*K2
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,F1,D2,S145)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef2314(N1,N3,N0,N2,N2,N3,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,S145,D2)
       allocate(U244(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,S75,D2,U244)
       deallocate(D2)
C
       V1A=V1A-U244
       deallocate(U244)
       deallocate(S145)
C
       allocate(F1(N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3))
       call relef561243(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N2,N3,N1,N3,N0,N2,N1,N3,l3B,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3))
       call relef561243(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N2,N3,N1,N3,N0,N2,N1,N3,t3B,F2)
       allocate(S62(N0+1:N2,N1+1:N3,N0+1:N2,N1+1:N3))
       I1=K3*K2
       I2=K3*K2
       I3=K3*K4*K1*K1
       call jungemm(I1,I2,I3,F1,F2,S62)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D2(N0+1:N2,N0+1:N2,N1+1:N3,N1+1:N3))
       call relef3124(N0,N2,N1,N3,N0,N2,N1,N3,
     & N0,N2,N0,N2,N1,N3,N1,N3,S62,D2)
       allocate(U146(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3*K2*K2
       call jungemm(I1,I2,I3,X20,D2,U146)
       deallocate(D2)
C
       V1A=V1A+1.0d0/2*U146
       deallocate(U146)
       deallocate(X20)
       deallocate(S62)
C
       allocate(D1(N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N1))
       call relef3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N2,N3,N0,N1,IntM,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3))
       call relef3124(N1,N3,N0,N2,N2,N3,N1,N3,
     & N2,N3,N1,N3,N0,N2,N1,N3,S129,D2)
       allocate(S140(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K3*K2
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,D2,S140)
       deallocate(D1)
       deallocate(D2)
C
       call sul1324(N0,N2,N2,N3,N1,N3,N0,N1,X14,S140,-1.000d0)
       deallocate(S140)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N0,N2,N0,N1,IntM,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:N3))
       call relef2134(N1,N3,N0,N2,N2,N3,N1,N3,
     & N0,N2,N1,N3,N2,N3,N1,N3,S129,D2)
       allocate(S139(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K3*K4
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,D2,S139)
       deallocate(D1)
       deallocate(D2)
       deallocate(S129)
C
       call sul2314(N0,N2,N2,N3,N1,N3,N0,N1,X14,S139,1.000d0)
       deallocate(S139)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3,N1+1:N3))
       call relef562413(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,N2,N2,N3,N1,N3,l3B,F1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(S59(N1+1:N3,N0+1:N2,N2+1:N3,N1+1:N3))
       I1=K3*K4*K2
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,F1,D2,S59)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef2314(N1,N3,N0,N2,N2,N3,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,S59,D2)
       allocate(U133(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,D1,D2,U133)
       deallocate(D1)
       deallocate(D2)
C
       V1A=V1A+1.0d0/2*U133
       deallocate(U133)
C
       allocate(D1(N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N1))
       call relef3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N2,N3,N0,N1,IntM,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3))
       call relef3124(N1,N3,N0,N2,N2,N3,N1,N3,
     & N2,N3,N1,N3,N0,N2,N1,N3,S59,D2)
       allocate(S69(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K3*K2
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,D2,S69)
       deallocate(D1)
       deallocate(D2)
       deallocate(S59)
C
       call sul1324(N0,N2,N2,N3,N1,N3,N0,N1,X14,S69,1.0d0/2)
       deallocate(S69)
C
       allocate(F1(N0+1:N2,N0+1:N1,N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3))
       call relef456213(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N1,N3,N2,N3,N1,N3,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3))
       call relef456312(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N1,N3,N2,N3,N1,N3,t3B,F2)
       allocate(S63(N2+1:N3,N1+1:N3,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K3*K4
       I3=K3*K1*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S63)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef3124(N2,N3,N1,N3,N2,N3,N1,N3,
     & N2,N3,N2,N3,N1,N3,N1,N3,S63,D2)
       allocate(U137(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3*K4*K4
       call jungemm(I1,I2,I3,D1,D2,U137)
       deallocate(D1)
       deallocate(D2)
C
       V1A=V1A+1.0d0/2*U137
       deallocate(U137)
       deallocate(S63)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N0,N1,N1,N3,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef461352(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N0,N2,N2,N3,t3C,F2)
       allocate(S64(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K4*K2
       I3=K3*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S64)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N0,N1,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef1324(N0,N2,N2,N3,N0,N1,N1,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,S64,D2)
       allocate(U138(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U138)
       deallocate(D1)
       deallocate(D2)
C
       V1A=V1A-U138
       deallocate(U138)
       deallocate(S64)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S73(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S73)
       deallocate(D1)
       deallocate(B2)
C
       allocate(F1(N0+1:N2,N0+1:N1,N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3))
       call relef456213(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N1,N3,N2,N3,N1,N3,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3))
       call relef456213(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N1,N3,N2,N3,N1,N3,t3B,F2)
       allocate(S74(N2+1:N3,N1+1:N3,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K3*K4
       I3=K3*K1*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S74)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D2(N2+1:N3,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef3124(N2,N3,N1,N3,N2,N3,N1,N3,
     & N2,N3,N2,N3,N1,N3,N1,N3,S74,D2)
       allocate(U147(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3*K4*K4
       call jungemm(I1,I2,I3,S73,D2,U147)
       deallocate(D2)
C
       V1A=V1A+1.0d0/2*U147
       deallocate(U147)
       deallocate(S74)
C
       allocate(F1(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef456123(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef456123(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,t3C,F2)
       allocate(S144(N2+1:N3,N1+1:N3,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K3*K4
       I3=K4*K1*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S144)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D2(N2+1:N3,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef3124(N2,N3,N1,N3,N2,N3,N1,N3,
     & N2,N3,N2,N3,N1,N3,N1,N3,S144,D2)
       allocate(U242(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3*K4*K4
       call jungemm(I1,I2,I3,S73,D2,U242)
       deallocate(D2)
C
       V1A=V1A+1.0d0/2*U242
       deallocate(U242)
       deallocate(S144)
C
       allocate(F1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef412536(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,N1,N3,N0,N1,l3B,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(S82(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S82)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef2134(N0,N1,N0,N1,N1,N3,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,N1,S82,D2)
       allocate(U158(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,D2,U158)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U158,-1.0d0)
       deallocate(U158)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1324(N0,N1,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,N0,N1,N0,N1,S82,D2)
       allocate(S87(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K1*K1
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S87)
       deallocate(D1)
       deallocate(D2)
C
       call sul1423(N0,N1,N1,N3,N1,N3,N0,N1,X7,S87,-2.000d0)
       deallocate(S87)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef3214(N0,N1,N0,N1,N1,N3,N0,N1,
     & N1,N3,N0,N1,N0,N1,N0,N1,S82,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S86(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S86)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N0,N1,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef2314(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,S86,D2)
       allocate(U162(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K1*K1*K1
       call jungemm(I1,I2,I3,D1,D2,U162)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U162,1.0d0)
       deallocate(U162)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef2314(N0,N1,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,N0,N1,N0,N1,S82,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N1,N3,t2A,D2)
       allocate(S95(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K3*K1
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S95)
       deallocate(D1)
       deallocate(D2)
       deallocate(S82)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef3124(N0,N1,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,N1,S95,D2)
       allocate(U170(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,D2,U170)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U170,-1.0d0)
       deallocate(U170)
       deallocate(S95)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef2314(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,S86,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S97(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S97)
       deallocate(D1)
       deallocate(B2)
       deallocate(S86)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef2314(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,N1,S97,D2)
       allocate(U172(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,D2,U172)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U172,1.0d0)
       deallocate(U172)
       deallocate(S97)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,l3B,F1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2B,D2)
       allocate(S83(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,F1,D2,S83)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N1,N3,N1,N3,IntR,D1)
       allocate(U159(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K3*K3
       call jungemm(I1,I2,I3,D1,S83,U159)
       deallocate(D1)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U159,1.0d0/2)
       deallocate(U159)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef1432(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N0,N1,N1,N3,IntR,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef1324(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N1,N3,N1,N3,N0,N1,S83,D2)
       allocate(S88(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K1*K3
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,D2,S88)
       deallocate(D1)
       deallocate(D2)
C
       call sul2413(N0,N1,N1,N3,N1,N3,N0,N1,X7,S88,-2.000d0)
       deallocate(S88)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef2314(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N1,N3,N1,N3,N0,N1,S83,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,t2A,D2)
       allocate(S96(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K1
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,D2,S96)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(U171(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,S96,U171)
       deallocate(D1)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U171,1.0d0/4)
       deallocate(U171)
       deallocate(S96)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef2314(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N1,N3,N1,N3,N0,N1,S83,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S98(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S98)
       deallocate(D1)
       deallocate(B2)
       deallocate(S83)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef2134(N0,N1,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,N1,N1,N3,N0,N1,S98,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S99(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S99)
       deallocate(D1)
       deallocate(B2)
       deallocate(S98)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef2134(N0,N1,N0,N1,N1,N3,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,N1,S99,D2)
       allocate(U173(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,D2,U173)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U173,1.0d0/2)
       deallocate(U173)
       deallocate(S99)
C
       allocate(F1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N2,N3,N1,N3,N1,N3,N0,N1,N0,N1,l3B,F1)
       allocate(F2(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N2,N3,N1,N3,N1,N3,N0,N1,N0,N1,t3B,F2)
       allocate(S84(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K1
       I3=K3*K3*K4*K2
       call jungemm(I1,I2,I3,F1,F2,S84)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N0,N1,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef3124(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,S84,D2)
       allocate(U160(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K1*K1*K1
       call jungemm(I1,I2,I3,D1,D2,U160)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U160,1.0d0/4)
       deallocate(U160)
       deallocate(S84)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N0,N1,N1,N3,t3B,F2)
       allocate(S85(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K3*K1
       I3=K3*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S85)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,IntR,D1)
       allocate(U161(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,D1,S85,U161)
       deallocate(D1)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U161,1.0d0)
       deallocate(U161)
       deallocate(S85)
C
       allocate(F1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N2,N3,N1,N3,N1,N3,N0,N1,N0,N1,l3B,F1)
       allocate(F2(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef412356(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N2,N3,N1,N3,N1,N3,N0,N1,N0,N1,t3B,F2)
       allocate(S89(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K1
       I3=K3*K3*K4*K2
       call jungemm(I1,I2,I3,F1,F2,S89)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call relef3124(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N0,N1,S89,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S90(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S90)
       deallocate(D1)
       deallocate(B2)
       deallocate(S89)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef2314(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,N1,S90,D2)
       allocate(U165(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,D2,U165)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U165,1.0d0/4)
       deallocate(U165)
       deallocate(S90)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N0,N1,N1,N3,t3B,F2)
       allocate(S91(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K3*K1
       I3=K3*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S91)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef3124(N0,N1,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,N1,N1,N3,N0,N1,S91,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S92(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S92)
       deallocate(D1)
       deallocate(B2)
       deallocate(S91)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef2134(N0,N1,N0,N1,N1,N3,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,N1,S92,D2)
       allocate(U166(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,D2,U166)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U166,-1.0d0)
       deallocate(U166)
       deallocate(S92)
C
       allocate(F1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       call relef523416(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N2,N2,N3,N0,N1,l3B,F1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S93(N0+1:N1,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4*K2
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,F1,D2,S93)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef1432(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef2134(N0,N1,N0,N2,N2,N3,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,N1,S93,D2)
       allocate(U174(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U174)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U174,-1.0d0/2)
       deallocate(U174)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef1324(N0,N1,N0,N2,N2,N3,N0,N1,
     & N0,N1,N2,N3,N0,N2,N0,N1,S93,D2)
       allocate(S110(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K1*K2
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,D2,S110)
       deallocate(D1)
       deallocate(D2)
C
       call sul1423(N0,N2,N2,N3,N1,N3,N0,N1,X14,S110,-1.0d0/2)
       deallocate(S110)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call relef2314(N0,N1,N0,N2,N2,N3,N0,N1,
     & N0,N2,N2,N3,N0,N1,N0,N1,S93,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N1,N3,t2B,D2)
       allocate(S94(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K3*K1
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S94)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef1324(N0,N1,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,N1,S94,D2)
       allocate(U169(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,D2,U169)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U169,-1.0d0/2)
       deallocate(U169)
       deallocate(S94)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call relef2314(N0,N1,N0,N2,N2,N3,N0,N1,
     & N0,N2,N2,N3,N0,N1,N0,N1,S93,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N2,N3,t2C,D2)
       allocate(S115(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K4*K2
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S115)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef1324(N0,N2,N2,N3,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,N1,S115,D2)
       allocate(U190(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U190)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U190,-1.0d0/2)
       deallocate(U190)
       deallocate(S115)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       call relef3214(N0,N1,N0,N2,N2,N3,N0,N1,
     & N2,N3,N0,N2,N0,N1,N0,N1,S93,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S109(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S109)
       deallocate(D1)
       deallocate(B2)
       deallocate(S93)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N0,N1,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
       call relef2134(N0,N2,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N2,N0,N1,N0,N1,S109,D2)
       allocate(U183(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K1*K2*K2
       call jungemm(I1,I2,I3,D1,D2,U183)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U183,1.0d0/2)
       deallocate(U183)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
       call relef2134(N0,N2,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N2,N0,N1,N0,N1,S109,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S123(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S123)
       deallocate(D1)
       deallocate(B2)
       deallocate(S109)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef2314(N2,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,N1,S123,D2)
       allocate(U196(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U196)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U196,1.0d0/2)
       deallocate(U196)
       deallocate(S123)
C
       allocate(F1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef512436(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,N1,N3,N0,N1,l3B,F1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(S100(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,F1,D2,S100)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N1+1:N3,N1+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N1,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef2134(N0,N2,N0,N2,N1,N3,N0,N1,
     & N0,N2,N0,N2,N1,N3,N0,N1,S100,D2)
       allocate(U175(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K2*K2
       call jungemm(I1,I2,I3,D1,D2,U175)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U175,-1.0d0)
       deallocate(U175)
C
       allocate(D1(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1324(N0,N2,N0,N2,N1,N3,N0,N1,
     & N0,N2,N1,N3,N0,N2,N0,N1,S100,D2)
       allocate(S111(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K1*K2
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,D2,S111)
       deallocate(D1)
       deallocate(D2)
C
       call sul1423(N0,N2,N2,N3,N1,N3,N0,N1,X14,S111,-1.000d0)
       deallocate(S111)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       call relef3214(N0,N2,N0,N2,N1,N3,N0,N1,
     & N1,N3,N0,N2,N0,N2,N0,N1,S100,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S105(N0+1:N1,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S105)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N0,N1,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
       call relef2314(N0,N1,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N0,N1,S105,D2)
       allocate(U180(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K1*K2*K2
       call jungemm(I1,I2,I3,D1,D2,U180)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U180,1.0d0)
       deallocate(U180)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef2314(N0,N2,N0,N2,N1,N3,N0,N1,
     & N0,N2,N1,N3,N0,N2,N0,N1,S100,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N0+1:N1,N2+1:N3))
       call relef3241(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N0,N1,N2,N3,t2B,D2)
       allocate(S119(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K4*K1
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,D2,S119)
       deallocate(D1)
       deallocate(D2)
       deallocate(S100)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef3124(N0,N1,N2,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,N1,S119,D2)
       allocate(U193(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U193)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U193,1.0d0)
       deallocate(U193)
       deallocate(S119)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
       call relef2314(N0,N1,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N0,N1,S105,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S120(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S120)
       deallocate(D1)
       deallocate(B2)
       deallocate(S105)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef2314(N2,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,N1,S120,D2)
       allocate(U194(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U194)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U194,1.0d0)
       deallocate(U194)
       deallocate(S120)
C
       allocate(F1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef452136(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,N1,N3,N0,N1,l3B,F1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,t2B,D2)
       allocate(S101(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,F1,D2,S101)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N1,N3,N1,N3,IntM,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef2134(N2,N3,N2,N3,N1,N3,N0,N1,
     & N2,N3,N2,N3,N1,N3,N0,N1,S101,D2)
       allocate(U176(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K4*K4
       call jungemm(I1,I2,I3,D1,D2,U176)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U176,1.0d0)
       deallocate(U176)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3))
       call relef1432(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N0,N2,N1,N3,IntM,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N1))
       call relef1324(N2,N3,N2,N3,N1,N3,N0,N1,
     & N2,N3,N1,N3,N2,N3,N0,N1,S101,D2)
       allocate(S112(N2+1:N3,N0+1:N1,N0+1:N2,N1+1:N3))
       I1=K3*K2
       I2=K1*K4
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,D2,S112)
       deallocate(D1)
       deallocate(D2)
C
       call sul2413(N0,N2,N2,N3,N1,N3,N0,N1,X14,S112,-1.000d0)
       deallocate(S112)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N0,N1,N1,N3,IntM,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef2134(N2,N3,N2,N3,N1,N3,N0,N1,
     & N2,N3,N2,N3,N1,N3,N0,N1,S101,D2)
       allocate(S106(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K1*K3
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,D2,S106)
       deallocate(D1)
       deallocate(D2)
C
       call sul2413(N0,N1,N1,N3,N1,N3,N0,N1,X7,S106,-2.000d0)
       deallocate(S106)
C
       allocate(D1(N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N1))
       call relef2314(N2,N3,N2,N3,N1,N3,N0,N1,
     & N2,N3,N1,N3,N2,N3,N0,N1,S101,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,t2B,D2)
       allocate(S116(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K1*K2
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,D2,S116)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(U191(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,S116,U191)
       deallocate(D1)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U191,1.0d0)
       deallocate(U191)
       deallocate(S116)
C
       allocate(D1(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef2134(N2,N3,N2,N3,N1,N3,N0,N1,
     & N2,N3,N2,N3,N1,N3,N0,N1,S101,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S121(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S121)
       deallocate(D1)
       deallocate(B2)
       deallocate(S101)
C
       allocate(D1(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       call relef3124(N0,N2,N2,N3,N1,N3,N0,N1,
     & N1,N3,N0,N2,N2,N3,N0,N1,S121,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S122(N0+1:N1,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S122)
       deallocate(D1)
       deallocate(B2)
       deallocate(S121)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef2134(N0,N1,N0,N2,N2,N3,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,N1,S122,D2)
       allocate(U195(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U195)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U195,1.0d0)
       deallocate(U195)
       deallocate(S122)
C
       allocate(F1(N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef512346(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,l3B,F1)
       allocate(F2(N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef512346(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,t3B,F2)
       allocate(S102(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K1*K2
       I3=K3*K3*K4*K1
       call jungemm(I1,I2,I3,F1,F2,S102)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N0,N1,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
       call relef3124(N0,N2,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N0,N1,S102,D2)
       allocate(U177(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K1*K2*K2
       call jungemm(I1,I2,I3,D1,D2,U177)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U177,1.0d0/2)
       deallocate(U177)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
       call relef3124(N0,N2,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N0,N1,S102,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S113(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S113)
       deallocate(D1)
       deallocate(B2)
       deallocate(S102)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef2314(N2,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,N1,S113,D2)
       allocate(U187(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U187)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U187,1.0d0/2)
       deallocate(U187)
       deallocate(S113)
C
       allocate(F1(N0+1:N2,N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N1))
       call relef452316(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N1,N3,N1,N3,N2,N3,N0,N1,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N2+1:N3))
       call relef452361(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N1,N3,N1,N3,N0,N1,N2,N3,t3B,F2)
       allocate(S103(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K4*K1
       I3=K3*K3*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S103)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       call relef1324(N0,N1,N2,N3,N2,N3,N0,N1,
     & N0,N1,N2,N3,N2,N3,N0,N1,S103,D2)
       allocate(U178(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K4*K4*K1
       call jungemm(I1,I2,I3,D1,D2,U178)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U178,-1.0d0/2)
       deallocate(U178)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef3124(N0,N1,N2,N3,N2,N3,N0,N1,
     & N2,N3,N0,N1,N2,N3,N0,N1,S103,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S114(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       I1=K1*K4*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S114)
       deallocate(D1)
       deallocate(B2)
       deallocate(S103)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(U188(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,S114,U188)
       deallocate(D1)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U188,1.0d0/2)
       deallocate(U188)
       deallocate(S114)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef461352(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N0,N2,N2,N3,t3C,F2)
       allocate(S104(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K4*K2
       I3=K3*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S104)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,IntM,D1)
       allocate(U179(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,D1,S104,U179)
       deallocate(D1)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U179,1.0d0)
       deallocate(U179)
       deallocate(S104)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef461352(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N0,N2,N2,N3,t3C,F2)
       allocate(S107(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K4*K2
       I3=K3*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S107)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       call relef3124(N0,N2,N2,N3,N1,N3,N0,N1,
     & N1,N3,N0,N2,N2,N3,N0,N1,S107,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S108(N0+1:N1,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S108)
       deallocate(D1)
       deallocate(B2)
       deallocate(S107)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef2134(N0,N1,N0,N2,N2,N3,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,N1,S108,D2)
       allocate(U182(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U182)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U182,-1.0d0)
       deallocate(U182)
       deallocate(S108)
C
       allocate(F1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef413526(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,N1,N3,N0,N1,l3B,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(S117(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S117)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call relef2314(N0,N1,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,N0,N1,N0,N1,S117,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,N3,t2B,D2)
       allocate(S118(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K4*K2
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S118)
       deallocate(D1)
       deallocate(D2)
       deallocate(S117)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef1324(N0,N2,N2,N3,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,N1,S118,D2)
       allocate(U192(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U192)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U192,-1.0d0)
       deallocate(U192)
       deallocate(S118)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,t3B,F2)
       allocate(Q197(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K3*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,Q197)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N0+1:N1,N0+1:N1))
       call relef21(N0,N1,N0,N1,
     & N0,N1,N0,N1,Q197,B2)
       allocate(U198(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K1*K1
       call jungemm1(I1,I3,D1,B2,U198)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A-1.0d0/2*U198
       deallocate(U198)
       deallocate(Q197)
C
       allocate(F1(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,t3B,F2)
       allocate(Q199(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K3*K4*K1*K1*K2
       call jungemm(I1,I2,I3,F1,F2,Q199)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(U200(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K3*K3
       call jungemm1(I1,I3,D1,Q199,U200)
       deallocate(D1)
C
       V1A=V1A+1.0d0/2*U200
       deallocate(U200)
       deallocate(Q199)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,t3B,F2)
       allocate(Q201(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K3*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,Q201)
       deallocate(F1)
       deallocate(F2)
C
       allocate(B1(N0+1:N1,N0+1:N1))
       call relef21(N0,N1,N0,N1,
     & N0,N1,N0,N1,Q201,B1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q202(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,B1,B2,Q202)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q201)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,Q202,B2)
       allocate(U203(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,U203)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A-1.0d0/2*U203
       deallocate(U203)
       deallocate(Q202)
C
       allocate(F1(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,t3B,F2)
       allocate(Q204(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K3*K4*K1*K1*K2
       call jungemm(I1,I2,I3,F1,F2,Q204)
       deallocate(F1)
       deallocate(F2)
C
       allocate(B1(N1+1:N3,N1+1:N3))
       call relef21(N1,N3,N1,N3,
     & N1,N3,N1,N3,Q204,B1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(Q205(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,B1,B2,Q205)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q204)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(U206(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K3*K1
       call jungemm1(I1,I3,D1,Q205,U206)
       deallocate(D1)
C
       V1A=V1A-1.0d0/2*U206
       deallocate(U206)
       deallocate(Q205)
C
       allocate(F1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       call relef412563(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,N0,N1,N1,N3,l3B,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(S124(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1*K1
       I2=K1
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S124)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef2341(N0,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,S124,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(Q207(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,D2,Q207)
       deallocate(D1)
       deallocate(D2)
       deallocate(S124)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,Q207,B2)
       allocate(U208(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,U208)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A+1.0d0/2*U208
       deallocate(U208)
       deallocate(Q207)
C
       allocate(F1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       call relef623451(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N2,N0,N1,N2,N3,l3B,F1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S125(N0+1:N1,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4*K1*K2
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,F1,D2,S125)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef2341(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N0,N1,N2,N3,N0,N1,S125,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2B,D2)
       allocate(Q209(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,Q209)
       deallocate(D1)
       deallocate(D2)
       deallocate(S125)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,Q209,B2)
       allocate(U210(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,U210)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A+1.0d0/2*U210
       deallocate(U210)
       deallocate(Q209)
C
       allocate(F1(N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef561234(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,l3B,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef561234(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,t3B,F2)
       allocate(Q211(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K3*K3*K4*K1*K1
       call jungemm(I1,I2,I3,F1,F2,Q211)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N1,N3,N0,N1,IntM,D1)
       allocate(B2(N0+1:N2,N0+1:N2))
       call relef21(N0,N2,N0,N2,
     & N0,N2,N0,N2,Q211,B2)
       allocate(U212(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K2*K2
       call jungemm1(I1,I3,D1,B2,U212)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A-1.0d0/4*U212
       deallocate(U212)
C
       allocate(B1(N0+1:N2,N0+1:N2))
       call relef21(N0,N2,N0,N2,
     & N0,N2,N0,N2,Q211,B1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q215(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,B1,B2,Q215)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q211)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,Q215,B2)
       allocate(U216(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,U216)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A-1.0d0/4*U216
       deallocate(U216)
       deallocate(Q215)
C
       allocate(F1(N0+1:N2,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3))
       call relef456231(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N1,N3,N1,N3,N2,N3,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3))
       call relef456231(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N1,N3,N1,N3,N2,N3,t3B,F2)
       allocate(Q213(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K3*K3*K1*K1*K2
       call jungemm(I1,I2,I3,F1,F2,Q213)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(B2(N2+1:N3,N2+1:N3))
       call relef21(N2,N3,N2,N3,
     & N2,N3,N2,N3,Q213,B2)
       allocate(U214(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K4*K4
       call jungemm1(I1,I3,D1,B2,U214)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A+1.0d0/4*U214
       deallocate(U214)
C
       allocate(B1(N2+1:N3,N2+1:N3))
       call relef21(N2,N3,N2,N3,
     & N2,N3,N2,N3,Q213,B1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(Q217(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,B1,B2,Q217)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q213)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(U218(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K4*K2
       call jungemm1(I1,I3,D1,Q217,U218)
       deallocate(D1)
C
       V1A=V1A-1.0d0/4*U218
       deallocate(U218)
       deallocate(Q217)
C
       allocate(F1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       call relef512463(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,N0,N1,N1,N3,l3B,F1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(S126(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1*K2
       I2=K2
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,F1,D2,S126)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N2,N0,N1,N1,N3,
     & N0,N2,N0,N1,N1,N3,N0,N2,S126,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,t2B,D2)
       allocate(Q219(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,D2,Q219)
       deallocate(D1)
       deallocate(D2)
       deallocate(S126)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,Q219,B2)
       allocate(U220(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,U220)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A-U220
       deallocate(U220)
       deallocate(Q219)
C
       allocate(F1(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef456123(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef456123(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,t3C,F2)
       allocate(Q221(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K4*K4*K1*K2*K2
       call jungemm(I1,I2,I3,F1,F2,Q221)
       deallocate(F1)
       deallocate(F2)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call relef12(0,N3,0,N3,
     & N1,N3,N0,N1,FockR,B1)
       allocate(U222(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3
       call jungemm(I1,I2,I3,B1,Q221,U222)
       deallocate(B1)
C
       V1A=V1A-1.0d0/4*U222
       deallocate(U222)
       deallocate(Q221)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N0,N1,N1,N3,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N0,N1,N1,N3,t3C,F2)
       allocate(S127(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K3*K1
       I3=K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S127)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3124(N0,N1,N1,N3,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,S127,D2)
       allocate(U223(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,D2,U223)
       deallocate(D1)
       deallocate(D2)
C
       V1A=V1A+1.0d0/4*U223
       deallocate(U223)
       deallocate(S127)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef451623(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N0,N1,N2,N3,N1,N3,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(S131(N2+1:N3,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4*K1
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,F1,D2,S131)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       call relef2134(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N2,N3,N0,N1,IntM,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef2134(N2,N3,N0,N1,N2,N3,N1,N3,
     & N0,N1,N2,N3,N2,N3,N1,N3,S131,D2)
       allocate(U230(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K4*K4*K1
       call jungemm(I1,I2,I3,D1,D2,U230)
       deallocate(D1)
       deallocate(D2)
C
       V1A=V1A-1.0d0/2*U230
       deallocate(U230)
       deallocate(S131)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N2,N1+1:N3))
       call relef461253(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N0,N2,N1,N3,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N2,N1+1:N3))
       call relef461253(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N0,N2,N1,N3,t3C,F2)
       allocate(S132(N0+1:N2,N1+1:N3,N0+1:N2,N1+1:N3))
       I1=K3*K2
       I2=K3*K2
       I3=K4*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S132)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N1,N3,N0,N1,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N1+1:N3,N1+1:N3))
       call relef3124(N0,N2,N1,N3,N0,N2,N1,N3,
     & N0,N2,N0,N2,N1,N3,N1,N3,S132,D2)
       allocate(U231(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3*K2*K2
       call jungemm(I1,I2,I3,D1,D2,U231)
       deallocate(D1)
       deallocate(D2)
C
       V1A=V1A+1.0d0/2*U231
       deallocate(U231)
       deallocate(S132)
C
       allocate(F1(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef456123(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef456123(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,t3C,F2)
       allocate(S133(N2+1:N3,N1+1:N3,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K3*K4
       I3=K4*K1*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S133)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef1324(N2,N3,N1,N3,N2,N3,N1,N3,
     & N2,N3,N2,N3,N1,N3,N1,N3,S133,D2)
       allocate(U232(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3*K4*K4
       call jungemm(I1,I2,I3,D1,D2,U232)
       deallocate(D1)
       deallocate(D2)
C
       V1A=V1A-1.0d0/2*U232
       deallocate(U232)
       deallocate(S133)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N0,N1,N1,N3,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef451263(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N0,N2,N2,N3,t3D,F2)
       allocate(S134(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K4*K2
       I3=K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S134)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N0,N1,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef1324(N0,N2,N2,N3,N0,N1,N1,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,S134,D2)
       allocate(U233(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U233)
       deallocate(D1)
       deallocate(D2)
C
       V1A=V1A-1.0d0/4*U233
       deallocate(U233)
       deallocate(S134)
C
       allocate(D1(N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N1))
       call relef2134(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N2,N3,N2,N3,N0,N1,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S136(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4*K4
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S136)
       deallocate(D1)
       deallocate(B2)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef451623(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N0,N1,N2,N3,N1,N3,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(S137(N2+1:N3,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4*K1
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,F1,D2,S137)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D2(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef2134(N2,N3,N0,N1,N2,N3,N1,N3,
     & N0,N1,N2,N3,N2,N3,N1,N3,S137,D2)
       allocate(U235(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K4*K4*K1
       call jungemm(I1,I2,I3,S136,D2,U235)
       deallocate(D2)
C
       V1A=V1A-1.0d0/2*U235
       deallocate(U235)
       deallocate(S137)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,t3C,F2)
       allocate(Q250(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,Q250)
       deallocate(F1)
       deallocate(F2)
C
       allocate(B1(N0+1:N1,N1+1:N3))
       call relef21(0,N3,0,N3,
     & N0,N1,N1,N3,FockR,B1)
       allocate(U251(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K1
       call jungemm(I1,I2,I3,B1,Q250,U251)
       deallocate(B1)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U251,-1.0d0/4)
       deallocate(U251)
       deallocate(Q250)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N0,N1,N1,N3,t3C,F2)
       allocate(S148(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K3*K1
       I3=K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S148)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,IntR,D1)
       allocate(U252(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,D1,S148,U252)
       deallocate(D1)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U252,1.0d0/4)
       deallocate(U252)
       deallocate(S148)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N0,N1,N1,N3,t3C,F2)
       allocate(S149(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K3*K1
       I3=K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S149)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef3124(N0,N1,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,N1,N1,N3,N0,N1,S149,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S150(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S150)
       deallocate(D1)
       deallocate(B2)
       deallocate(S149)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef2134(N0,N1,N0,N1,N1,N3,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,N1,S150,D2)
       allocate(U253(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,D2,U253)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U253,-1.0d0/4)
       deallocate(U253)
       deallocate(S150)
C
       allocate(F1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       call relef413526(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N2,N2,N3,N0,N1,l3C,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(S151(N0+1:N1,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4*K2
       I2=K1
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S151)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef1432(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef2134(N0,N1,N0,N2,N2,N3,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,N1,S151,D2)
       allocate(U257(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U257)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U257,-1.0d0)
       deallocate(U257)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef1324(N0,N1,N0,N2,N2,N3,N0,N1,
     & N0,N1,N2,N3,N0,N2,N0,N1,S151,D2)
       allocate(S164(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K1*K2
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,D2,S164)
       deallocate(D1)
       deallocate(D2)
C
       call sul1423(N0,N2,N2,N3,N1,N3,N0,N1,X14,S164,-1.000d0)
       deallocate(S164)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call relef2314(N0,N1,N0,N2,N2,N3,N0,N1,
     & N0,N2,N2,N3,N0,N1,N0,N1,S151,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N1,N3,t2B,D2)
       allocate(S152(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K3*K1
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S152)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef3124(N0,N1,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,N1,S152,D2)
       allocate(U256(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,D2,U256)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U256,1.0d0)
       deallocate(U256)
       deallocate(S152)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       call relef3214(N0,N1,N0,N2,N2,N3,N0,N1,
     & N2,N3,N0,N2,N0,N1,N0,N1,S151,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S163(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S163)
       deallocate(D1)
       deallocate(B2)
       deallocate(S151)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N0,N1,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
       call relef2134(N0,N2,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N2,N0,N1,N0,N1,S163,D2)
       allocate(U266(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K1*K2*K2
       call jungemm(I1,I2,I3,D1,D2,U266)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U266,1.0d0)
       deallocate(U266)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
       call relef2134(N0,N2,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N2,N0,N1,N0,N1,S163,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S180(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S180)
       deallocate(D1)
       deallocate(B2)
       deallocate(S163)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef2314(N2,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,N1,S180,D2)
       allocate(U278(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U278)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U278,1.0d0)
       deallocate(U278)
       deallocate(S180)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef412536(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N0,N2,N1,N3,N0,N1,l3C,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S153(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S153)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N1+1:N3,N1+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N1,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef2134(N0,N2,N0,N2,N1,N3,N0,N1,
     & N0,N2,N0,N2,N1,N3,N0,N1,S153,D2)
       allocate(U258(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K2*K2
       call jungemm(I1,I2,I3,D1,D2,U258)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U258,-1.0d0/2)
       deallocate(U258)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       call relef3214(N0,N2,N0,N2,N1,N3,N0,N1,
     & N1,N3,N0,N2,N0,N2,N0,N1,S153,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S158(N0+1:N1,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S158)
       deallocate(D1)
       deallocate(B2)
       deallocate(S153)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N0,N1,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
       call relef2314(N0,N1,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N0,N1,S158,D2)
       allocate(U263(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K1*K2*K2
       call jungemm(I1,I2,I3,D1,D2,U263)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U263,1.0d0/2)
       deallocate(U263)
       deallocate(S158)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(S154(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,F1,D2,S154)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N1,N3,N1,N3,IntM,D1)
       allocate(U259(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K4*K4
       call jungemm(I1,I2,I3,D1,S154,U259)
       deallocate(D1)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U259,1.0d0/2)
       deallocate(U259)
C
       allocate(D1(N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N1))
       call relef2314(N2,N3,N2,N3,N1,N3,N0,N1,
     & N2,N3,N1,N3,N2,N3,N0,N1,S154,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,t2B,D2)
       allocate(S175(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K1*K2
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,D2,S175)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(U275(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,S175,U275)
       deallocate(D1)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U275,1.0d0/2)
       deallocate(U275)
       deallocate(S175)
C
       allocate(D1(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef2134(N2,N3,N2,N3,N1,N3,N0,N1,
     & N2,N3,N2,N3,N1,N3,N0,N1,S154,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S178(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S178)
       deallocate(D1)
       deallocate(B2)
       deallocate(S154)
C
       allocate(D1(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       call relef3124(N0,N2,N2,N3,N1,N3,N0,N1,
     & N1,N3,N0,N2,N2,N3,N0,N1,S178,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S179(N0+1:N1,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S179)
       deallocate(D1)
       deallocate(B2)
       deallocate(S178)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef2134(N0,N1,N0,N2,N2,N3,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,N1,S179,D2)
       allocate(U277(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U277)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U277,1.0d0/2)
       deallocate(U277)
       deallocate(S179)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef412356(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N1,N3,N0,N2,N0,N1,l3C,F1)
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef412356(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N1,N3,N0,N2,N0,N1,t3C,F2)
       allocate(S155(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K1*K2
       I3=K3*K4*K4*K2
       call jungemm(I1,I2,I3,F1,F2,S155)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N0,N1,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
       call relef3124(N0,N2,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N0,N1,S155,D2)
       allocate(U260(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K1*K2*K2
       call jungemm(I1,I2,I3,D1,D2,U260)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U260,1.0d0/2)
       deallocate(U260)
       deallocate(S155)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N1))
       call relef451326(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N1,N3,N2,N3,N0,N1,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1,N2+1:N3))
       call relef451362(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N1,N3,N0,N1,N2,N3,t3C,F2)
       allocate(S156(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K4*K1
       I3=K3*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S156)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       call relef1324(N0,N1,N2,N3,N2,N3,N0,N1,
     & N0,N1,N2,N3,N2,N3,N0,N1,S156,D2)
       allocate(U261(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K4*K4*K1
       call jungemm(I1,I2,I3,D1,D2,U261)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U261,-1.0d0/2)
       deallocate(U261)
       deallocate(S156)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef451263(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N0,N2,N2,N3,t3D,F2)
       allocate(S157(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K4*K2
       I3=K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S157)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,IntM,D1)
       allocate(U262(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,D1,S157,U262)
       deallocate(D1)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U262,1.0d0/4)
       deallocate(U262)
       deallocate(S157)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(S159(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,F1,D2,S159)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N0,N1,N1,N3,IntM,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef2134(N2,N3,N2,N3,N1,N3,N0,N1,
     & N2,N3,N2,N3,N1,N3,N0,N1,S159,D2)
       allocate(S160(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K1*K3
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,D2,S160)
       deallocate(D1)
       deallocate(D2)
C
       call sul2413(N0,N1,N1,N3,N1,N3,N0,N1,X7,S160,-1.000d0)
       deallocate(S160)
C
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(U74(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K3*K1
       call jungemm1(I1,I3,X7,B2,U74)
       deallocate(B2)
C
       V1A=V1A+1.0d0/2*U74
       deallocate(U74)
       deallocate(X7)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3))
       call relef1432(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N0,N2,N1,N3,IntM,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N1))
       call relef1324(N2,N3,N2,N3,N1,N3,N0,N1,
     & N2,N3,N1,N3,N2,N3,N0,N1,S159,D2)
       allocate(S167(N2+1:N3,N0+1:N1,N0+1:N2,N1+1:N3))
       I1=K3*K2
       I2=K1*K4
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,D2,S167)
       deallocate(D1)
       deallocate(D2)
       deallocate(S159)
C
       call sul2413(N0,N2,N2,N3,N1,N3,N0,N1,X14,S167,-1.0d0/2)
       deallocate(S167)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef451263(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N0,N2,N2,N3,t3D,F2)
       allocate(S161(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K4*K2
       I3=K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S161)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       call relef3124(N0,N2,N2,N3,N1,N3,N0,N1,
     & N1,N3,N0,N2,N2,N3,N0,N1,S161,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S162(N0+1:N1,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S162)
       deallocate(D1)
       deallocate(B2)
       deallocate(S161)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef2134(N0,N1,N0,N2,N2,N3,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,N1,S162,D2)
       allocate(U265(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U265)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U265,-1.0d0/4)
       deallocate(U265)
       deallocate(S162)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef412536(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N0,N2,N1,N3,N0,N1,l3C,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S165(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S165)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1324(N0,N2,N0,N2,N1,N3,N0,N1,
     & N0,N2,N1,N3,N0,N2,N0,N1,S165,D2)
       allocate(S166(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K1*K2
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,D2,S166)
       deallocate(D1)
       deallocate(D2)
C
       call sul1423(N0,N2,N2,N3,N1,N3,N0,N1,X14,S166,-1.0d0/2)
       deallocate(S166)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(U154(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K4*K2
       call jungemm1(I1,I3,X14,B2,U154)
       deallocate(B2)
C
       V1A=V1A+U154
       deallocate(U154)
       deallocate(X14)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef2314(N0,N2,N0,N2,N1,N3,N0,N1,
     & N0,N2,N1,N3,N0,N2,N0,N1,S165,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N0+1:N1,N2+1:N3))
       call relef3241(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N0,N1,N2,N3,t2B,D2)
       allocate(S174(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K4*K1
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,D2,S174)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef3124(N0,N1,N2,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,N1,S174,D2)
       allocate(U274(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U274)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U274,1.0d0/2)
       deallocate(U274)
       deallocate(S174)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       call relef3214(N0,N2,N0,N2,N1,N3,N0,N1,
     & N1,N3,N0,N2,N0,N2,N0,N1,S165,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S176(N0+1:N1,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S176)
       deallocate(D1)
       deallocate(B2)
       deallocate(S165)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
       call relef2314(N0,N1,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N0,N1,S176,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S177(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S177)
       deallocate(D1)
       deallocate(B2)
       deallocate(S176)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef2314(N2,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,N1,S177,D2)
       allocate(U276(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U276)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U276,1.0d0/2)
       deallocate(U276)
       deallocate(S177)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef412356(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N1,N3,N0,N2,N0,N1,l3C,F1)
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef412356(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N1,N3,N0,N2,N0,N1,t3C,F2)
       allocate(S168(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K1*K2
       I3=K3*K4*K4*K2
       call jungemm(I1,I2,I3,F1,F2,S168)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
       call relef3124(N0,N2,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N0,N1,S168,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S169(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S169)
       deallocate(D1)
       deallocate(B2)
       deallocate(S168)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef2314(N2,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,N1,S169,D2)
       allocate(U270(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U270)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U270,1.0d0/2)
       deallocate(U270)
       deallocate(S169)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N1))
       call relef451326(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N1,N3,N2,N3,N0,N1,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1,N2+1:N3))
       call relef451362(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N1,N3,N0,N1,N2,N3,t3C,F2)
       allocate(S170(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K4*K1
       I3=K3*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S170)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef3124(N0,N1,N2,N3,N2,N3,N0,N1,
     & N2,N3,N0,N1,N2,N3,N0,N1,S170,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S171(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       I1=K1*K4*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S171)
       deallocate(D1)
       deallocate(B2)
       deallocate(S170)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(U271(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,S171,U271)
       deallocate(D1)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U271,1.0d0/2)
       deallocate(U271)
       deallocate(S171)
C
       allocate(F1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       call relef523416(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N2,N2,N3,N0,N1,l3C,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(S172(N0+1:N1,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4*K2
       I2=K1
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S172)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call relef2314(N0,N1,N0,N2,N2,N3,N0,N1,
     & N0,N2,N2,N3,N0,N1,N0,N1,S172,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N2,N3,t2C,D2)
       allocate(S173(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K4*K2
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S173)
       deallocate(D1)
       deallocate(D2)
       deallocate(S172)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef1324(N0,N2,N2,N3,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,N1,S173,D2)
       allocate(U273(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U273)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U273,-1.0d0)
       deallocate(U273)
       deallocate(S173)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,t3C,F2)
       allocate(Q279(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,Q279)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N0+1:N1,N0+1:N1))
       call relef21(N0,N1,N0,N1,
     & N0,N1,N0,N1,Q279,B2)
       allocate(U280(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K1*K1
       call jungemm1(I1,I3,D1,B2,U280)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A-1.0d0/4*U280
       deallocate(U280)
       deallocate(Q279)
C
       allocate(F1(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef456123(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef456123(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,t3C,F2)
       allocate(Q281(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K4*K4*K1*K2*K2
       call jungemm(I1,I2,I3,F1,F2,Q281)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(U282(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K3*K3
       call jungemm1(I1,I3,D1,Q281,U282)
       deallocate(D1)
C
       V1A=V1A+1.0d0/4*U282
       deallocate(U282)
       deallocate(Q281)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,t3C,F2)
       allocate(Q283(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,Q283)
       deallocate(F1)
       deallocate(F2)
C
       allocate(B1(N0+1:N1,N0+1:N1))
       call relef21(N0,N1,N0,N1,
     & N0,N1,N0,N1,Q283,B1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q284(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,B1,B2,Q284)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q283)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,Q284,B2)
       allocate(U285(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,U285)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A-1.0d0/4*U285
       deallocate(U285)
       deallocate(Q284)
C
       allocate(F1(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef456123(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef456123(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,t3C,F2)
       allocate(Q286(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K4*K4*K1*K2*K2
       call jungemm(I1,I2,I3,F1,F2,Q286)
       deallocate(F1)
       deallocate(F2)
C
       allocate(B1(N1+1:N3,N1+1:N3))
       call relef21(N1,N3,N1,N3,
     & N1,N3,N1,N3,Q286,B1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(Q287(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,B1,B2,Q287)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q286)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(U288(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K3*K1
       call jungemm1(I1,I3,D1,Q287,U288)
       deallocate(D1)
C
       V1A=V1A-1.0d0/4*U288
       deallocate(U288)
       deallocate(Q287)
C
       allocate(F1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       call relef413562(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N2,N0,N1,N2,N3,l3C,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(S181(N0+1:N1,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4*K1*K2
       I2=K1
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S181)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef2341(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N0,N1,N2,N3,N0,N1,S181,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2B,D2)
       allocate(Q289(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,Q289)
       deallocate(D1)
       deallocate(D2)
       deallocate(S181)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,Q289,B2)
       allocate(U290(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,U290)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A-U290
       deallocate(U290)
       deallocate(Q289)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef461235(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef461235(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,t3C,F2)
       allocate(Q291(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K3*K4*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,Q291)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N1,N3,N0,N1,IntM,D1)
       allocate(B2(N0+1:N2,N0+1:N2))
       call relef21(N0,N2,N0,N2,
     & N0,N2,N0,N2,Q291,B2)
       allocate(U292(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K2*K2
       call jungemm1(I1,I3,D1,B2,U292)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A-1.0d0/2*U292
       deallocate(U292)
       deallocate(Q291)
C
       allocate(F1(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef456132(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N1,N3,N2,N3,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef456132(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N1,N3,N2,N3,t3C,F2)
       allocate(Q293(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K3*K4*K1*K2*K2
       call jungemm(I1,I2,I3,F1,F2,Q293)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(B2(N2+1:N3,N2+1:N3))
       call relef21(N2,N3,N2,N3,
     & N2,N3,N2,N3,Q293,B2)
       allocate(U294(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K4*K4
       call jungemm1(I1,I3,D1,B2,U294)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A+1.0d0/2*U294
       deallocate(U294)
       deallocate(Q293)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef461235(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef461235(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,t3C,F2)
       allocate(Q295(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K3*K4*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,Q295)
       deallocate(F1)
       deallocate(F2)
C
       allocate(B1(N0+1:N2,N0+1:N2))
       call relef21(N0,N2,N0,N2,
     & N0,N2,N0,N2,Q295,B1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q296(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,B1,B2,Q296)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q295)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,Q296,B2)
       allocate(U297(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,U297)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A-1.0d0/2*U297
       deallocate(U297)
       deallocate(Q296)
C
       allocate(F1(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef456132(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N1,N3,N2,N3,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef456132(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N1,N3,N2,N3,t3C,F2)
       allocate(Q298(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K3*K4*K1*K2*K2
       call jungemm(I1,I2,I3,F1,F2,Q298)
       deallocate(F1)
       deallocate(F2)
C
       allocate(B1(N2+1:N3,N2+1:N3))
       call relef21(N2,N3,N2,N3,
     & N2,N3,N2,N3,Q298,B1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(Q299(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,B1,B2,Q299)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q298)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(U300(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K4*K2
       call jungemm1(I1,I3,D1,Q299,U300)
       deallocate(D1)
C
       V1A=V1A-1.0d0/2*U300
       deallocate(U300)
       deallocate(Q299)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       call relef412563(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N0,N2,N0,N1,N1,N3,l3C,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S182(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1*K2
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S182)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N2,N0,N1,N1,N3,
     & N0,N2,N0,N1,N1,N3,N0,N2,S182,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,t2B,D2)
       allocate(Q301(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,D2,Q301)
       deallocate(D1)
       deallocate(D2)
       deallocate(S182)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,Q301,B2)
       allocate(U302(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,U302)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A-1.0d0/2*U302
       deallocate(U302)
       deallocate(Q301)
C
       allocate(F1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       call relef623451(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,N0,N2,N2,N3,l3C,F1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(S183(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2*K2
       I2=K2
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,F1,D2,S183)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,S183,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(Q303(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,D2,Q303)
       deallocate(D1)
       deallocate(D2)
       deallocate(S183)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,Q303,B2)
       allocate(U304(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,U304)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A-1.0d0/2*U304
       deallocate(U304)
       deallocate(Q303)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,l3D,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,t3D,F2)
       allocate(Q305(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4*K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,Q305)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N1,N3,N0,N1,IntM,D1)
       allocate(B2(N0+1:N2,N0+1:N2))
       call relef21(N0,N2,N0,N2,
     & N0,N2,N0,N2,Q305,B2)
       allocate(U306(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K2*K2
       call jungemm1(I1,I3,D1,B2,U306)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A-1.0d0/12*U306
       deallocate(U306)
       deallocate(Q305)
C
       allocate(F1(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef456123(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,l3D,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef456123(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,t3D,F2)
       allocate(Q307(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K4*K4*K2*K2*K2
       call jungemm(I1,I2,I3,F1,F2,Q307)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(U308(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K4*K4
       call jungemm1(I1,I3,D1,Q307,U308)
       deallocate(D1)
C
       V1A=V1A+1.0d0/12*U308
       deallocate(U308)
       deallocate(Q307)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,l3D,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,t3D,F2)
       allocate(Q309(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4*K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,Q309)
       deallocate(F1)
       deallocate(F2)
C
       allocate(B1(N0+1:N2,N0+1:N2))
       call relef21(N0,N2,N0,N2,
     & N0,N2,N0,N2,Q309,B1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q310(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,B1,B2,Q310)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q309)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,Q310,B2)
       allocate(U311(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,U311)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A-1.0d0/12*U311
       deallocate(U311)
       deallocate(Q310)
C
       allocate(F1(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef456123(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,l3D,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef456123(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,t3D,F2)
       allocate(Q312(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K4*K4*K2*K2*K2
       call jungemm(I1,I2,I3,F1,F2,Q312)
       deallocate(F1)
       deallocate(F2)
C
       allocate(B1(N2+1:N3,N2+1:N3))
       call relef21(N2,N3,N2,N3,
     & N2,N3,N2,N3,Q312,B1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(Q313(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,B1,B2,Q313)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q312)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(U314(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K4*K2
       call jungemm1(I1,I3,D1,Q313,U314)
       deallocate(D1)
C
       V1A=V1A-1.0d0/12*U314
       deallocate(U314)
       deallocate(Q313)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       call relef412563(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,N0,N2,N2,N3,l3D,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S184(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2*K2
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S184)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,S184,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(Q315(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,D2,Q315)
       deallocate(D1)
       deallocate(D2)
       deallocate(S184)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,Q315,B2)
       allocate(U316(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,U316)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A+1.0d0/4*U316
       deallocate(U316)
       deallocate(Q315)
C
       allocate(B1(N0+1:N1,N0+1:N1))
       call relef12(N0,N3,N0,N3,
     & N0,N1,N0,N1,H1A,B1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,l1A,B2)
       allocate(U1(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,B1,B2,U1)
       deallocate(B1)
       deallocate(B2)
C
       V1A=V1A-U1
       deallocate(U1)
C
       allocate(B1(N1+1:N3,N1+1:N3))
       call relef21(N0,N3,N0,N3,
     & N1,N3,N1,N3,H1A,B1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,l1A,B2)
       allocate(U2(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,B1,B2,U2)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U2,1.0d0)
       deallocate(U2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,H2A,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,l1A,B2)
       allocate(U3(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,U3)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A+U3
       deallocate(U3)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,H2B,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,l1B,B2)
       allocate(U4(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,U4)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A+U4
       deallocate(U4)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,H2A,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,l2A,D2)
       allocate(U5(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,D2,U5)
       deallocate(D1)
       deallocate(D2)
C
       V1A=V1A-1.0d0/2*U5
       deallocate(U5)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N1,N3,H2A,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,l2A,D2)
       allocate(U6(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,D1,D2,U6)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U6,1.0d0/2)
       deallocate(U6)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N2,N3,N0,N1,H2B,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,l2B,D2)
       allocate(U21(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U21)
       deallocate(D1)
       deallocate(D2)
C
       V1A=V1A-U21
       deallocate(U21)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,H2B,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,l2B,D2)
       allocate(U22(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,D1,D2,U22)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N1,N3,N0,N1,V1A,U22,1.0d0)
       deallocate(U22)
C
       end
