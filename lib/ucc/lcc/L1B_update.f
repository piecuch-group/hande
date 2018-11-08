       subroutine L1B_update(N0,N1,N2,N3,V1B,
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
       real*8 V1B(N2+1:N3,N0+1:N2)
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
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::Q9(:,:)
       real*8,allocatable::Q11(:,:)
       real*8,allocatable::Q12(:,:)
       real*8,allocatable::Q14(:,:)
       real*8,allocatable::Q15(:,:)
       real*8,allocatable::Q17(:,:)
       real*8,allocatable::Q21(:,:)
       real*8,allocatable::Q27(:,:)
       real*8,allocatable::Q23(:,:)
       real*8,allocatable::Q29(:,:)
       real*8,allocatable::Q25(:,:)
       real*8,allocatable::Q31(:,:)
       real*8,allocatable::Q37(:,:)
       real*8,allocatable::Q33(:,:)
       real*8,allocatable::Q39(:,:)
       real*8,allocatable::Q35(:,:)
       real*8,allocatable::Q43(:,:)
       real*8,allocatable::Q45(:,:)
       real*8,allocatable::Q47(:,:)
       real*8,allocatable::Q49(:,:)
       real*8,allocatable::Q51(:,:)
       real*8,allocatable::Q52(:,:)
       real*8,allocatable::Q54(:,:)
       real*8,allocatable::Q55(:,:)
       real*8,allocatable::Q57(:,:)
       real*8,allocatable::Q59(:,:)
       real*8,allocatable::Q61(:,:)
       real*8,allocatable::Q62(:,:)
       real*8,allocatable::Q64(:,:)
       real*8,allocatable::Q65(:,:)
       real*8,allocatable::S1(:,:,:,:)
       real*8,allocatable::Q67(:,:)
       real*8,allocatable::Q69(:,:)
       real*8,allocatable::Q97(:,:)
       real*8,allocatable::Q266(:,:)
       real*8,allocatable::Q83(:,:)
       real*8,allocatable::Q169(:,:)
       real*8,allocatable::Q84(:,:)
       real*8,allocatable::S2(:,:,:,:)
       real*8,allocatable::S18(:,:,:,:)
       real*8,allocatable::S68(:,:,:,:)
       real*8,allocatable::S14(:,:,:,:)
       real*8,allocatable::S15(:,:,:,:)
       real*8,allocatable::S81(:,:,:,:)
       real*8,allocatable::S85(:,:,:,:)
       real*8,allocatable::S31(:,:,:,:)
       real*8,allocatable::S4(:,:,:,:)
       real*8,allocatable::S25(:,:,:,:)
       real*8,allocatable::S26(:,:,:,:)
       real*8,allocatable::S78(:,:,:,:)
       real*8,allocatable::S27(:,:,:,:)
       real*8,allocatable::S9(:,:,:,:)
       real*8,allocatable::S8(:,:,:,:)
       real*8,allocatable::S23(:,:,:,:)
       real*8,allocatable::S24(:,:,:,:)
       real*8,allocatable::S13(:,:,:,:)
       real*8,allocatable::S12(:,:,:,:)
       real*8,allocatable::S3(:,:,:,:)
       real*8,allocatable::S19(:,:,:,:)
       real*8,allocatable::S5(:,:,:,:)
       real*8,allocatable::S20(:,:,:,:)
       real*8,allocatable::S155(:,:,:,:)
       real*8,allocatable::S71(:,:,:,:)
       real*8,allocatable::S6(:,:,:,:)
       real*8,allocatable::S7(:,:,:,:)
       real*8,allocatable::S10(:,:,:,:)
       real*8,allocatable::S11(:,:,:,:)
       real*8,allocatable::S69(:,:,:,:)
       real*8,allocatable::S16(:,:,:,:)
       real*8,allocatable::S86(:,:,:,:)
       real*8,allocatable::S79(:,:,:,:)
       real*8,allocatable::S28(:,:,:,:)
       real*8,allocatable::S17(:,:,:,:)
       real*8,allocatable::S80(:,:,:,:)
       real*8,allocatable::S21(:,:,:,:)
       real*8,allocatable::S22(:,:,:,:)
       real*8,allocatable::S82(:,:,:,:)
       real*8,allocatable::S29(:,:,:,:)
       real*8,allocatable::S30(:,:,:,:)
       real*8,allocatable::S156(:,:,:,:)
       real*8,allocatable::S166(:,:,:,:)
       real*8,allocatable::S165(:,:,:,:)
       real*8,allocatable::S83(:,:,:,:)
       real*8,allocatable::S84(:,:,:,:)
       real*8,allocatable::S92(:,:,:,:)
       real*8,allocatable::S100(:,:,:,:)
       real*8,allocatable::S163(:,:,:,:)
       real*8,allocatable::S93(:,:,:,:)
       real*8,allocatable::S99(:,:,:,:)
       real*8,allocatable::S157(:,:,:,:)
       real*8,allocatable::S98(:,:,:,:)
       real*8,allocatable::S87(:,:,:,:)
       real*8,allocatable::S90(:,:,:,:)
       real*8,allocatable::S160(:,:,:,:)
       real*8,allocatable::S162(:,:,:,:)
       real*8,allocatable::S161(:,:,:,:)
       real*8,allocatable::S91(:,:,:,:)
       real*8,allocatable::Q100(:,:)
       real*8,allocatable::Q128(:,:)
       real*8,allocatable::Q287(:,:)
       real*8,allocatable::Q115(:,:)
       real*8,allocatable::Q210(:,:)
       real*8,allocatable::Q114(:,:)
       real*8,allocatable::S32(:,:,:,:)
       real*8,allocatable::S38(:,:,:,:)
       real*8,allocatable::S47(:,:,:,:)
       real*8,allocatable::S33(:,:,:,:)
       real*8,allocatable::S34(:,:,:,:)
       real*8,allocatable::S42(:,:,:,:)
       real*8,allocatable::S41(:,:,:,:)
       real*8,allocatable::S64(:,:,:,:)
       real*8,allocatable::S55(:,:,:,:)
       real*8,allocatable::S56(:,:,:,:)
       real*8,allocatable::S35(:,:,:,:)
       real*8,allocatable::S36(:,:,:,:)
       real*8,allocatable::S37(:,:,:,:)
       real*8,allocatable::S39(:,:,:,:)
       real*8,allocatable::S40(:,:,:,:)
       real*8,allocatable::S48(:,:,:,:)
       real*8,allocatable::S60(:,:,:,:)
       real*8,allocatable::S61(:,:,:,:)
       real*8,allocatable::S43(:,:,:,:)
       real*8,allocatable::S44(:,:,:,:)
       real*8,allocatable::S45(:,:,:,:)
       real*8,allocatable::S46(:,:,:,:)
       real*8,allocatable::S49(:,:,:,:)
       real*8,allocatable::S50(:,:,:,:)
       real*8,allocatable::S51(:,:,:,:)
       real*8,allocatable::S52(:,:,:,:)
       real*8,allocatable::S53(:,:,:,:)
       real*8,allocatable::S54(:,:,:,:)
       real*8,allocatable::S57(:,:,:,:)
       real*8,allocatable::S58(:,:,:,:)
       real*8,allocatable::S59(:,:,:,:)
       real*8,allocatable::S62(:,:,:,:)
       real*8,allocatable::S63(:,:,:,:)
       real*8,allocatable::Q131(:,:)
       real*8,allocatable::Q133(:,:)
       real*8,allocatable::Q135(:,:)
       real*8,allocatable::Q136(:,:)
       real*8,allocatable::Q138(:,:)
       real*8,allocatable::Q139(:,:)
       real*8,allocatable::S65(:,:,:,:)
       real*8,allocatable::Q141(:,:)
       real*8,allocatable::S66(:,:,:,:)
       real*8,allocatable::Q143(:,:)
       real*8,allocatable::Q145(:,:)
       real*8,allocatable::Q149(:,:)
       real*8,allocatable::Q147(:,:)
       real*8,allocatable::Q151(:,:)
       real*8,allocatable::S67(:,:,:,:)
       real*8,allocatable::Q153(:,:)
       real*8,allocatable::Q155(:,:)
       real*8,allocatable::S70(:,:,:,:)
       real*8,allocatable::S72(:,:,:,:)
       real*8,allocatable::S73(:,:,:,:)
       real*8,allocatable::S74(:,:,:,:)
       real*8,allocatable::S75(:,:,:,:)
       real*8,allocatable::S76(:,:,:,:)
       real*8,allocatable::S77(:,:,:,:)
       real*8,allocatable::S88(:,:,:,:)
       real*8,allocatable::S89(:,:,:,:)
       real*8,allocatable::S94(:,:,:,:)
       real*8,allocatable::S95(:,:,:,:)
       real*8,allocatable::S164(:,:,:,:)
       real*8,allocatable::S96(:,:,:,:)
       real*8,allocatable::S97(:,:,:,:)
       real*8,allocatable::Q196(:,:)
       real*8,allocatable::S101(:,:,:,:)
       real*8,allocatable::S126(:,:,:,:)
       real*8,allocatable::S119(:,:,:,:)
       real*8,allocatable::S131(:,:,:,:)
       real*8,allocatable::S102(:,:,:,:)
       real*8,allocatable::S120(:,:,:,:)
       real*8,allocatable::S132(:,:,:,:)
       real*8,allocatable::S133(:,:,:,:)
       real*8,allocatable::S103(:,:,:,:)
       real*8,allocatable::S104(:,:,:,:)
       real*8,allocatable::S105(:,:,:,:)
       real*8,allocatable::S106(:,:,:,:)
       real*8,allocatable::S107(:,:,:,:)
       real*8,allocatable::S108(:,:,:,:)
       real*8,allocatable::S109(:,:,:,:)
       real*8,allocatable::S110(:,:,:,:)
       real*8,allocatable::S125(:,:,:,:)
       real*8,allocatable::S111(:,:,:,:)
       real*8,allocatable::S112(:,:,:,:)
       real*8,allocatable::S113(:,:,:,:)
       real*8,allocatable::S114(:,:,:,:)
       real*8,allocatable::S115(:,:,:,:)
       real*8,allocatable::S116(:,:,:,:)
       real*8,allocatable::S117(:,:,:,:)
       real*8,allocatable::S118(:,:,:,:)
       real*8,allocatable::S121(:,:,:,:)
       real*8,allocatable::S122(:,:,:,:)
       real*8,allocatable::S123(:,:,:,:)
       real*8,allocatable::S124(:,:,:,:)
       real*8,allocatable::S147(:,:,:,:)
       real*8,allocatable::S129(:,:,:,:)
       real*8,allocatable::S130(:,:,:,:)
       real*8,allocatable::S127(:,:,:,:)
       real*8,allocatable::S138(:,:,:,:)
       real*8,allocatable::S128(:,:,:,:)
       real*8,allocatable::S137(:,:,:,:)
       real*8,allocatable::S148(:,:,:,:)
       real*8,allocatable::S134(:,:,:,:)
       real*8,allocatable::S139(:,:,:,:)
       real*8,allocatable::S144(:,:,:,:)
       real*8,allocatable::S149(:,:,:,:)
       real*8,allocatable::S150(:,:,:,:)
       real*8,allocatable::S135(:,:,:,:)
       real*8,allocatable::S136(:,:,:,:)
       real*8,allocatable::S140(:,:,:,:)
       real*8,allocatable::S141(:,:,:,:)
       real*8,allocatable::S142(:,:,:,:)
       real*8,allocatable::S143(:,:,:,:)
       real*8,allocatable::S145(:,:,:,:)
       real*8,allocatable::S146(:,:,:,:)
       real*8,allocatable::Q237(:,:)
       real*8,allocatable::Q239(:,:)
       real*8,allocatable::Q241(:,:)
       real*8,allocatable::Q242(:,:)
       real*8,allocatable::Q244(:,:)
       real*8,allocatable::Q245(:,:)
       real*8,allocatable::S151(:,:,:,:)
       real*8,allocatable::Q247(:,:)
       real*8,allocatable::Q249(:,:)
       real*8,allocatable::Q251(:,:)
       real*8,allocatable::Q253(:,:)
       real*8,allocatable::Q254(:,:)
       real*8,allocatable::Q256(:,:)
       real*8,allocatable::Q257(:,:)
       real*8,allocatable::S152(:,:,:,:)
       real*8,allocatable::Q259(:,:)
       real*8,allocatable::S153(:,:,:,:)
       real*8,allocatable::Q261(:,:)
       real*8,allocatable::Q263(:,:)
       real*8,allocatable::S154(:,:,:,:)
       real*8,allocatable::S158(:,:,:,:)
       real*8,allocatable::S159(:,:,:,:)
       real*8,allocatable::Q284(:,:)
       real*8,allocatable::S167(:,:,:,:)
       real*8,allocatable::S168(:,:,:,:)
       real*8,allocatable::S169(:,:,:,:)
       real*8,allocatable::S170(:,:,:,:)
       real*8,allocatable::S176(:,:,:,:)
       real*8,allocatable::S171(:,:,:,:)
       real*8,allocatable::S185(:,:,:,:)
       real*8,allocatable::S175(:,:,:,:)
       real*8,allocatable::S186(:,:,:,:)
       real*8,allocatable::S172(:,:,:,:)
       real*8,allocatable::S187(:,:,:,:)
       real*8,allocatable::S188(:,:,:,:)
       real*8,allocatable::S173(:,:,:,:)
       real*8,allocatable::S174(:,:,:,:)
       real*8,allocatable::S177(:,:,:,:)
       real*8,allocatable::S178(:,:,:,:)
       real*8,allocatable::S179(:,:,:,:)
       real*8,allocatable::S180(:,:,:,:)
       real*8,allocatable::S181(:,:,:,:)
       real*8,allocatable::S182(:,:,:,:)
       real*8,allocatable::S183(:,:,:,:)
       real*8,allocatable::S184(:,:,:,:)
       real*8,allocatable::Q305(:,:)
       real*8,allocatable::Q307(:,:)
       real*8,allocatable::Q309(:,:)
       real*8,allocatable::Q310(:,:)
       real*8,allocatable::Q312(:,:)
       real*8,allocatable::Q313(:,:)
       real*8,allocatable::S189(:,:,:,:)
       real*8,allocatable::Q315(:,:)
       real*8,allocatable::U1(:,:)
       real*8,allocatable::U2(:,:)
       real*8,allocatable::U3(:,:)
       real*8,allocatable::U4(:,:)
       real*8,allocatable::U6(:,:)
       real*8,allocatable::U8(:,:)
       real*8,allocatable::U10(:,:)
       real*8,allocatable::U13(:,:)
       real*8,allocatable::U16(:,:)
       real*8,allocatable::U18(:,:)
       real*8,allocatable::U19(:,:)
       real*8,allocatable::U20(:,:)
       real*8,allocatable::U22(:,:)
       real*8,allocatable::U28(:,:)
       real*8,allocatable::U24(:,:)
       real*8,allocatable::U30(:,:)
       real*8,allocatable::U26(:,:)
       real*8,allocatable::U32(:,:)
       real*8,allocatable::U38(:,:)
       real*8,allocatable::U34(:,:)
       real*8,allocatable::U40(:,:)
       real*8,allocatable::U36(:,:)
       real*8,allocatable::U41(:,:)
       real*8,allocatable::U42(:,:)
       real*8,allocatable::U44(:,:)
       real*8,allocatable::U46(:,:)
       real*8,allocatable::U48(:,:)
       real*8,allocatable::U50(:,:)
       real*8,allocatable::U53(:,:)
       real*8,allocatable::U56(:,:)
       real*8,allocatable::U58(:,:)
       real*8,allocatable::U60(:,:)
       real*8,allocatable::U63(:,:)
       real*8,allocatable::U66(:,:)
       real*8,allocatable::U68(:,:)
       real*8,allocatable::X1(:,:)
       real*8,allocatable::U70(:,:)
       real*8,allocatable::X2(:,:)
       real*8,allocatable::U279(:,:)
       real*8,allocatable::X3(:,:)
       real*8,allocatable::U170(:,:)
       real*8,allocatable::U85(:,:)
       real*8,allocatable::X4(:,:,:,:)
       real*8,allocatable::U71(:,:)
       real*8,allocatable::X5(:,:,:,:)
       real*8,allocatable::U171(:,:)
       real*8,allocatable::U81(:,:)
       real*8,allocatable::X6(:,:,:,:)
       real*8,allocatable::U193(:,:)
       real*8,allocatable::X7(:,:,:,:)
       real*8,allocatable::U99(:,:)
       real*8,allocatable::U167(:,:)
       real*8,allocatable::X8(:,:,:,:)
       real*8,allocatable::U178(:,:)
       real*8,allocatable::X9(:,:,:,:)
       real*8,allocatable::U93(:,:)
       real*8,allocatable::U90(:,:)
       real*8,allocatable::U72(:,:)
       real*8,allocatable::X10(:,:,:,:)
       real*8,allocatable::U87(:,:)
       real*8,allocatable::X11(:,:,:,:)
       real*8,allocatable::U74(:,:)
       real*8,allocatable::U268(:,:)
       real*8,allocatable::X12(:,:,:,:)
       real*8,allocatable::U173(:,:)
       real*8,allocatable::U75(:,:)
       real*8,allocatable::U76(:,:)
       real*8,allocatable::U78(:,:)
       real*8,allocatable::X13(:,:,:,:)
       real*8,allocatable::U164(:,:)
       real*8,allocatable::U168(:,:)
       real*8,allocatable::U82(:,:)
       real*8,allocatable::U89(:,:)
       real*8,allocatable::U175(:,:)
       real*8,allocatable::X14(:,:,:,:)
       real*8,allocatable::U95(:,:)
       real*8,allocatable::X15(:,:,:,:)
       real*8,allocatable::U280(:,:)
       real*8,allocatable::X16(:,:,:,:)
       real*8,allocatable::U176(:,:)
       real*8,allocatable::U277(:,:)
       real*8,allocatable::U188(:,:)
       real*8,allocatable::X17(:,:,:,:)
       real*8,allocatable::U282(:,:)
       real*8,allocatable::X18(:,:,:,:)
       real*8,allocatable::U192(:,:)
       real*8,allocatable::U274(:,:)
       real*8,allocatable::X19(:,:)
       real*8,allocatable::U101(:,:)
       real*8,allocatable::X20(:,:)
       real*8,allocatable::U300(:,:)
       real*8,allocatable::X21(:,:)
       real*8,allocatable::U211(:,:)
       real*8,allocatable::U116(:,:)
       real*8,allocatable::U102(:,:)
       real*8,allocatable::U117(:,:)
       real*8,allocatable::U103(:,:)
       real*8,allocatable::U104(:,:)
       real*8,allocatable::U110(:,:)
       real*8,allocatable::U130(:,:)
       real*8,allocatable::U122(:,:)
       real*8,allocatable::U123(:,:)
       real*8,allocatable::U105(:,:)
       real*8,allocatable::U106(:,:)
       real*8,allocatable::U107(:,:)
       real*8,allocatable::U125(:,:)
       real*8,allocatable::U112(:,:)
       real*8,allocatable::U113(:,:)
       real*8,allocatable::U119(:,:)
       real*8,allocatable::U120(:,:)
       real*8,allocatable::U121(:,:)
       real*8,allocatable::U124(:,:)
       real*8,allocatable::U126(:,:)
       real*8,allocatable::U127(:,:)
       real*8,allocatable::U132(:,:)
       real*8,allocatable::U134(:,:)
       real*8,allocatable::U137(:,:)
       real*8,allocatable::U140(:,:)
       real*8,allocatable::U142(:,:)
       real*8,allocatable::U144(:,:)
       real*8,allocatable::U146(:,:)
       real*8,allocatable::U150(:,:)
       real*8,allocatable::U148(:,:)
       real*8,allocatable::U152(:,:)
       real*8,allocatable::U154(:,:)
       real*8,allocatable::U156(:,:)
       real*8,allocatable::U159(:,:)
       real*8,allocatable::U161(:,:)
       real*8,allocatable::U162(:,:)
       real*8,allocatable::U165(:,:)
       real*8,allocatable::U166(:,:)
       real*8,allocatable::U183(:,:)
       real*8,allocatable::U184(:,:)
       real*8,allocatable::U189(:,:)
       real*8,allocatable::U278(:,:)
       real*8,allocatable::U191(:,:)
       real*8,allocatable::U197(:,:)
       real*8,allocatable::U198(:,:)
       real*8,allocatable::U217(:,:)
       real*8,allocatable::U212(:,:)
       real*8,allocatable::U220(:,:)
       real*8,allocatable::U199(:,:)
       real*8,allocatable::U221(:,:)
       real*8,allocatable::U200(:,:)
       real*8,allocatable::U201(:,:)
       real*8,allocatable::U202(:,:)
       real*8,allocatable::U203(:,:)
       real*8,allocatable::U216(:,:)
       real*8,allocatable::U206(:,:)
       real*8,allocatable::U208(:,:)
       real*8,allocatable::U209(:,:)
       real*8,allocatable::U214(:,:)
       real*8,allocatable::U215(:,:)
       real*8,allocatable::U234(:,:)
       real*8,allocatable::U219(:,:)
       real*8,allocatable::U222(:,:)
       real*8,allocatable::U218(:,:)
       real*8,allocatable::U226(:,:)
       real*8,allocatable::U235(:,:)
       real*8,allocatable::U223(:,:)
       real*8,allocatable::U232(:,:)
       real*8,allocatable::U236(:,:)
       real*8,allocatable::U224(:,:)
       real*8,allocatable::U225(:,:)
       real*8,allocatable::U229(:,:)
       real*8,allocatable::U230(:,:)
       real*8,allocatable::U233(:,:)
       real*8,allocatable::U238(:,:)
       real*8,allocatable::U240(:,:)
       real*8,allocatable::U243(:,:)
       real*8,allocatable::U246(:,:)
       real*8,allocatable::U248(:,:)
       real*8,allocatable::U250(:,:)
       real*8,allocatable::U252(:,:)
       real*8,allocatable::U255(:,:)
       real*8,allocatable::U258(:,:)
       real*8,allocatable::U260(:,:)
       real*8,allocatable::U262(:,:)
       real*8,allocatable::U264(:,:)
       real*8,allocatable::U265(:,:)
       real*8,allocatable::U272(:,:)
       real*8,allocatable::U273(:,:)
       real*8,allocatable::U285(:,:)
       real*8,allocatable::U286(:,:)
       real*8,allocatable::U289(:,:)
       real*8,allocatable::U291(:,:)
       real*8,allocatable::U290(:,:)
       real*8,allocatable::U302(:,:)
       real*8,allocatable::U295(:,:)
       real*8,allocatable::U303(:,:)
       real*8,allocatable::U292(:,:)
       real*8,allocatable::U304(:,:)
       real*8,allocatable::U293(:,:)
       real*8,allocatable::U294(:,:)
       real*8,allocatable::U298(:,:)
       real*8,allocatable::U299(:,:)
       real*8,allocatable::U301(:,:)
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
       allocate(Q5(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,D1,D2,Q5)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N2,N3,N0,N2,IntM,D1)
       allocate(B2(N0+1:N1,N0+1:N1))
       call relef21(N0,N1,N0,N1,
     & N0,N1,N0,N1,Q5,B2)
       allocate(U6(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K1*K1
       call jungemm1(I1,I3,D1,B2,U6)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B-1.0d0/2*U6
       deallocate(U6)
       deallocate(Q5)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,l2A,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(Q7(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,D2,Q7)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(U8(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K3*K3
       call jungemm1(I1,I3,D1,Q7,U8)
       deallocate(D1)
C
       V1B=V1B+1.0d0/2*U8
       deallocate(U8)
       deallocate(Q7)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,l2A,D1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N0,N1,N1,N3,t3A,F2)
       allocate(Q9(N0+1:N1,N1+1:N3))
       I2=K3*K1
       I3=K3*K3*K1*K1
       call jungemm2(I2,I3,D1,F2,Q9)
       deallocate(D1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(U10(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K3*K1
       call jungemm1(I1,I3,D1,Q9,U10)
       deallocate(D1)
C
       V1B=V1B+1.0d0/4*U10
       deallocate(U10)
       deallocate(Q9)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,l2A,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(Q11(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,D1,D2,Q11)
       deallocate(D1)
       deallocate(D2)
C
       allocate(B1(N0+1:N1,N0+1:N1))
       call relef21(N0,N1,N0,N1,
     & N0,N1,N0,N1,Q11,B1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q12(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,B1,B2,Q12)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q11)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,Q12,B2)
       allocate(U13(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,U13)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B-1.0d0/2*U13
       deallocate(U13)
       deallocate(Q12)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,l2A,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(Q14(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,D2,Q14)
       deallocate(D1)
       deallocate(D2)
C
       allocate(B1(N1+1:N3,N1+1:N3))
       call relef21(N1,N3,N1,N3,
     & N1,N3,N1,N3,Q14,B1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(Q15(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,B1,B2,Q15)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q14)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(U16(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K3*K1
       call jungemm1(I1,I3,D1,Q15,U16)
       deallocate(D1)
C
       V1B=V1B-1.0d0/2*U16
       deallocate(U16)
       deallocate(Q15)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,l2A,D1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef562341(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N0,N2,N2,N3,t3B,F2)
       allocate(Q17(N0+1:N2,N2+1:N3))
       I2=K4*K2
       I3=K3*K3*K1*K1
       call jungemm2(I2,I3,D1,F2,Q17)
       deallocate(D1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U18(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K4*K2
       call jungemm1(I1,I3,D1,Q17,U18)
       deallocate(D1)
C
       V1B=V1B+1.0d0/4*U18
       deallocate(U18)
       deallocate(Q17)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,l2B,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(Q21(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,D1,D2,Q21)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N2,N3,N0,N2,IntM,D1)
       allocate(B2(N0+1:N1,N0+1:N1))
       call relef21(N0,N1,N0,N1,
     & N0,N1,N0,N1,Q21,B2)
       allocate(U22(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K1*K1
       call jungemm1(I1,I3,D1,B2,U22)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B-U22
       deallocate(U22)
C
       allocate(B1(N0+1:N1,N0+1:N1))
       call relef21(N0,N1,N0,N1,
     & N0,N1,N0,N1,Q21,B1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q27(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,B1,B2,Q27)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q21)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,Q27,B2)
       allocate(U28(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,U28)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B-U28
       deallocate(U28)
       deallocate(Q27)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,l2B,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2B,D2)
       allocate(Q23(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,Q23)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(U24(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K3*K3
       call jungemm1(I1,I3,D1,Q23,U24)
       deallocate(D1)
C
       V1B=V1B+U24
       deallocate(U24)
C
       allocate(B1(N1+1:N3,N1+1:N3))
       call relef21(N1,N3,N1,N3,
     & N1,N3,N1,N3,Q23,B1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(Q29(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,B1,B2,Q29)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q23)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(U30(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K3*K1
       call jungemm1(I1,I3,D1,Q29,U30)
       deallocate(D1)
C
       V1B=V1B-U30
       deallocate(U30)
       deallocate(Q29)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,l2B,D1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N0,N1,N1,N3,t3B,F2)
       allocate(Q25(N0+1:N1,N1+1:N3))
       I2=K3*K1
       I3=K3*K4*K1*K2
       call jungemm2(I2,I3,D1,F2,Q25)
       deallocate(D1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(U26(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K3*K1
       call jungemm1(I1,I3,D1,Q25,U26)
       deallocate(D1)
C
       V1B=V1B+U26
       deallocate(U26)
       deallocate(Q25)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,l2B,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(Q31(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,D1,D2,Q31)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N0+1:N2))
       call relef21(N0,N2,N0,N2,
     & N0,N2,N0,N2,Q31,B2)
       allocate(U32(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K2*K2
       call jungemm1(I1,I3,D1,B2,U32)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B-U32
       deallocate(U32)
C
       allocate(B1(N0+1:N2,N0+1:N2))
       call relef21(N0,N2,N0,N2,
     & N0,N2,N0,N2,Q31,B1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q37(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,B1,B2,Q37)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q31)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,Q37,B2)
       allocate(U38(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,U38)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B-U38
       deallocate(U38)
       deallocate(Q37)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,l2B,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,t2B,D2)
       allocate(Q33(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,D2,Q33)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N2+1:N3,N2+1:N3))
       call relef21(N2,N3,N2,N3,
     & N2,N3,N2,N3,Q33,B2)
       allocate(U34(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K4*K4
       call jungemm1(I1,I3,D1,B2,U34)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B+U34
       deallocate(U34)
C
       allocate(B1(N2+1:N3,N2+1:N3))
       call relef21(N2,N3,N2,N3,
     & N2,N3,N2,N3,Q33,B1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(Q39(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,B1,B2,Q39)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q33)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U40(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K4*K2
       call jungemm1(I1,I3,D1,Q39,U40)
       deallocate(D1)
C
       V1B=V1B-U40
       deallocate(U40)
       deallocate(Q39)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,l2B,D1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef461352(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N0,N2,N2,N3,t3C,F2)
       allocate(Q35(N0+1:N2,N2+1:N3))
       I2=K4*K2
       I3=K3*K4*K1*K2
       call jungemm2(I2,I3,D1,F2,Q35)
       deallocate(D1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U36(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K4*K2
       call jungemm1(I1,I3,D1,Q35,U36)
       deallocate(D1)
C
       V1B=V1B+U36
       deallocate(U36)
       deallocate(Q35)
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
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(U44(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K3*K1
       call jungemm1(I1,I3,D1,Q43,U44)
       deallocate(D1)
C
       V1B=V1B+1.0d0/4*U44
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
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N0+1:N2))
       call relef21(N0,N2,N0,N2,
     & N0,N2,N0,N2,Q45,B2)
       allocate(U46(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K2*K2
       call jungemm1(I1,I3,D1,B2,U46)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B-1.0d0/2*U46
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
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U48(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K4*K4
       call jungemm1(I1,I3,D1,Q47,U48)
       deallocate(D1)
C
       V1B=V1B+1.0d0/2*U48
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
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U50(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K4*K2
       call jungemm1(I1,I3,D1,Q49,U50)
       deallocate(D1)
C
       V1B=V1B+1.0d0/4*U50
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
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,Q52,B2)
       allocate(U53(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,U53)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B-1.0d0/2*U53
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
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U56(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K4*K2
       call jungemm1(I1,I3,D1,Q55,U56)
       deallocate(D1)
C
       V1B=V1B-1.0d0/2*U56
       deallocate(U56)
       deallocate(Q55)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,l3A,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,t3A,F2)
       allocate(Q57(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K3*K3*K1*K1
       call jungemm(I1,I2,I3,F1,F2,Q57)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N2,N3,N0,N2,IntM,D1)
       allocate(B2(N0+1:N1,N0+1:N1))
       call relef21(N0,N1,N0,N1,
     & N0,N1,N0,N1,Q57,B2)
       allocate(U58(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K1*K1
       call jungemm1(I1,I3,D1,B2,U58)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B-1.0d0/12*U58
       deallocate(U58)
       deallocate(Q57)
C
       allocate(F1(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,l3A,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,t3A,F2)
       allocate(Q59(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K3*K3*K1*K1*K1
       call jungemm(I1,I2,I3,F1,F2,Q59)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(U60(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K3*K3
       call jungemm1(I1,I3,D1,Q59,U60)
       deallocate(D1)
C
       V1B=V1B+1.0d0/12*U60
       deallocate(U60)
       deallocate(Q59)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,l3A,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,t3A,F2)
       allocate(Q61(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K3*K3*K1*K1
       call jungemm(I1,I2,I3,F1,F2,Q61)
       deallocate(F1)
       deallocate(F2)
C
       allocate(B1(N0+1:N1,N0+1:N1))
       call relef21(N0,N1,N0,N1,
     & N0,N1,N0,N1,Q61,B1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q62(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,B1,B2,Q62)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q61)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,Q62,B2)
       allocate(U63(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,U63)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B-1.0d0/12*U63
       deallocate(U63)
       deallocate(Q62)
C
       allocate(F1(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,l3A,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,t3A,F2)
       allocate(Q64(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K3*K3*K1*K1*K1
       call jungemm(I1,I2,I3,F1,F2,Q64)
       deallocate(F1)
       deallocate(F2)
C
       allocate(B1(N1+1:N3,N1+1:N3))
       call relef21(N1,N3,N1,N3,
     & N1,N3,N1,N3,Q64,B1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(Q65(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,B1,B2,Q65)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q64)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(U66(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K3*K1
       call jungemm1(I1,I3,D1,Q65,U66)
       deallocate(D1)
C
       V1B=V1B-1.0d0/12*U66
       deallocate(U66)
       deallocate(Q65)
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
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef2341(N0,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,S1,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(Q67(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,D2,Q67)
       deallocate(D1)
       deallocate(D2)
       deallocate(S1)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,Q67,B2)
       allocate(U68(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,U68)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B+1.0d0/4*U68
       deallocate(U68)
       deallocate(Q67)
C
       allocate(F1(N0+1:N2,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3))
       call relef456231(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N1,N3,N1,N3,N2,N3,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3))
       call relef456231(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N1,N3,N1,N3,N2,N3,t3B,F2)
       allocate(Q69(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K3*K3*K1*K1*K2
       call jungemm(I1,I2,I3,F1,F2,Q69)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q97(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q97)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(N2+1:N3,N0+1:N2))
       X1=0.0d0
       X1=X1-Q97
C
       allocate(X2(N2+1:N3,N0+1:N2))
       X2=0.0d0
       X2=X2+Q97
C
       allocate(X3(N2+1:N3,N0+1:N2))
       X3=0.0d0
       X3=X3-Q97
       deallocate(Q97)
C
       call slx12(0,N3,N2,N3,N0,N2,X1,FockB,1.000d0)
C
       allocate(U70(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K4
       call jungemm(I1,I2,I3,X1,Q69,U70)
C
       V1B=V1B-1.0d0/4*U70
       deallocate(U70)
       deallocate(Q69)
       deallocate(X1)
C
       allocate(F1(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef456123(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,l3D,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef456123(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,t3D,F2)
       allocate(Q266(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K4*K4*K2*K2*K2
       call jungemm(I1,I2,I3,F1,F2,Q266)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q83(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q83)
       deallocate(D1)
       deallocate(B2)
C
       X2=X2-Q83
C
       X3=X3+Q83
C
       allocate(U279(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K4
       call jungemm(I1,I2,I3,X2,Q266,U279)
C
       V1B=V1B+1.0d0/12*U279
       deallocate(U279)
       deallocate(Q266)
       deallocate(X2)
C
       allocate(F1(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef456132(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N1,N3,N2,N3,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef456132(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N1,N3,N2,N3,t3C,F2)
       allocate(Q169(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K3*K4*K1*K2*K2
       call jungemm(I1,I2,I3,F1,F2,Q169)
       deallocate(F1)
       deallocate(F2)
C
       allocate(U170(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K4
       call jungemm(I1,I2,I3,X3,Q169,U170)
C
       V1B=V1B-1.0d0/2*U170
       deallocate(U170)
       deallocate(X3)
       deallocate(Q169)
C
       allocate(F1(N0+1:N2,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3))
       call relef456231(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N1,N3,N1,N3,N2,N3,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3))
       call relef456231(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N1,N3,N1,N3,N2,N3,t3B,F2)
       allocate(Q84(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K3*K3*K1*K1*K2
       call jungemm(I1,I2,I3,F1,F2,Q84)
       deallocate(F1)
       deallocate(F2)
C
       allocate(U85(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K4
       call jungemm(I1,I2,I3,Q83,Q84,U85)
C
       V1B=V1B-1.0d0/4*U85
       deallocate(U85)
       deallocate(Q84)
C
       allocate(F1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       call relef523461(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N2,N0,N1,N2,N3,l3B,F1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S2(N0+1:N1,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4*K1*K2
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,F1,D2,S2)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N0+1:N1,N0+1:N1,N0+1:N2))
       call relef1243(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N1,N0,N1,N0,N2,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S18(N0+1:N2,N0+1:N1,N0+1:N1,N0+1:N2))
       I1=K2*K1*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S18)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(N0+1:N2,N0+1:N1,N0+1:N1,N0+1:N2))
       X4=0.0d0
       X4=X4+S18
C
       allocate(X5(N0+1:N1,N0+1:N2,N0+1:N1,N0+1:N2))
       X5=0.0d0
       call sul2134(N0,N1,N0,N2,N0,N1,N0,N2,X5,S18,1.000d0)
       deallocate(S18)
C
       call slx1243(N0,N3,N0,N2,N0,N1,N0,N1,N0,N2,X4,IntM,1.000d0)
C
       allocate(D2(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3))
       call relef2314(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N0,N1,N0,N1,N2,N3,S2,D2)
       allocate(U71(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K1*K1*K2
       call jungemm(I1,I2,I3,X4,D2,U71)
       deallocate(D2)
C
       V1B=V1B+1.0d0/2*U71
       deallocate(U71)
       deallocate(S2)
       deallocate(X4)
C
       allocate(F1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       call relef413562(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N2,N0,N1,N2,N3,l3C,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(S68(N0+1:N1,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4*K1*K2
       I2=K1
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S68)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N2,N3,N0,N2,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S14(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4*K1
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S14)
       deallocate(D1)
       deallocate(B2)
C
       allocate(F1(N0+1:N2,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N2+1:N3))
       call relef452361(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N1,N3,N1,N3,N0,N1,N2,N3,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N2+1:N3))
       call relef452361(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N1,N3,N1,N3,N0,N1,N2,N3,t3B,F2)
       allocate(S15(N0+1:N1,N2+1:N3,N0+1:N1,N2+1:N3))
       I1=K4*K1
       I2=K4*K1
       I3=K3*K3*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S15)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D2(N0+1:N1,N0+1:N1,N2+1:N3,N2+1:N3))
       call relef3124(N0,N1,N2,N3,N0,N1,N2,N3,
     & N0,N1,N0,N1,N2,N3,N2,N3,S15,D2)
       allocate(U81(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K4*K1*K1
       call jungemm(I1,I2,I3,S14,D2,U81)
       deallocate(D2)
C
       V1B=V1B+1.0d0/2*U81
       deallocate(U81)
       deallocate(S15)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef451632(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N0,N1,N1,N3,N2,N3,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(S81(N2+1:N3,N0+1:N1,N1+1:N3,N2+1:N3))
       I1=K4*K3*K1
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,F1,D2,S81)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N1,N0+1:N2))
       call relef1324(N0,N1,N0,N1,N2,N3,N0,N2,
     & N0,N1,N2,N3,N0,N1,N0,N2,S14,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef2134(N2,N3,N0,N1,N1,N3,N2,N3,
     & N0,N1,N2,N3,N1,N3,N2,N3,S81,D2)
       allocate(S85(N1+1:N3,N2+1:N3,N0+1:N1,N0+1:N2))
       I1=K2*K1
       I2=K4*K3
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,D2,S85)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X8(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       X8=0.0d0
       call sul2314(N0,N1,N1,N3,N2,N3,N0,N2,X8,S85,1.000d0)
       deallocate(S85)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N1,N3,t2B,D2)
       allocate(S31(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K3*K1
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S31)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X6(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       X6=0.0d0
       call sul1324(N0,N1,N2,N3,N1,N3,N0,N2,X6,S31,1.000d0)
C
       allocate(X7(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       X7=0.0d0
       call sul1324(N0,N1,N2,N3,N1,N3,N0,N2,X7,S31,1.000d0)
       deallocate(S31)
C
       allocate(F1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef452631(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N1,N3,N0,N1,N1,N3,N2,N3,l3B,F1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,t2B,D2)
       allocate(S4(N2+1:N3,N0+1:N1,N1+1:N3,N2+1:N3))
       I1=K4*K3*K1
       I2=K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,F1,D2,S4)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N1,N3,t2A,D2)
       allocate(S25(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K3*K1
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S25)
       deallocate(D1)
       deallocate(D2)
C
       call sul1324(N0,N1,N2,N3,N1,N3,N0,N2,X6,S25,-1.000d0)
C
       call sul1324(N0,N1,N2,N3,N1,N3,N0,N2,X7,S25,-1.000d0)
       deallocate(S25)
C
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef2134(N2,N3,N0,N1,N1,N3,N2,N3,
     & N0,N1,N2,N3,N1,N3,N2,N3,S81,D2)
       allocate(U193(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,X6,D2,U193)
       deallocate(D2)
C
       V1B=V1B+1.0d0/2*U193
       deallocate(U193)
       deallocate(S81)
       deallocate(X6)
C
       call slx2143(N0,N3,N0,N1,N2,N3,N1,N3,N0,N2,X7,IntM,-1.000d0)
C
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef2134(N2,N3,N0,N1,N1,N3,N2,N3,
     & N0,N1,N2,N3,N1,N3,N2,N3,S4,D2)
       allocate(U99(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,X7,D2,U99)
       deallocate(D2)
C
       V1B=V1B+U99
       deallocate(U99)
       deallocate(X7)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N1,N0+1:N2))
       call relef1324(N0,N1,N0,N1,N2,N3,N0,N2,
     & N0,N1,N2,N3,N0,N1,N0,N2,S14,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef2134(N2,N3,N0,N1,N1,N3,N2,N3,
     & N0,N1,N2,N3,N1,N3,N2,N3,S4,D2)
       allocate(S26(N1+1:N3,N2+1:N3,N0+1:N1,N0+1:N2))
       I1=K2*K1
       I2=K4*K3
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,D2,S26)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N0,N1,N1,N3,N2,N3,N0,N2,X8,S26,2.000d0)
       deallocate(S26)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1,N2+1:N3))
       call relef451362(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N1,N3,N0,N1,N2,N3,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1,N2+1:N3))
       call relef451362(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N1,N3,N0,N1,N2,N3,t3C,F2)
       allocate(S78(N0+1:N1,N2+1:N3,N0+1:N1,N2+1:N3))
       I1=K4*K1
       I2=K4*K1
       I3=K3*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S78)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D2(N0+1:N1,N0+1:N1,N2+1:N3,N2+1:N3))
       call relef3124(N0,N1,N2,N3,N0,N1,N2,N3,
     & N0,N1,N0,N1,N2,N3,N2,N3,S78,D2)
       allocate(U167(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K4*K1*K1
       call jungemm(I1,I2,I3,S14,D2,U167)
       deallocate(D2)
C
       V1B=V1B+1.0d0/2*U167
       deallocate(U167)
       deallocate(S78)
C
       allocate(D1(N2+1:N3,N0+1:N1,N0+1:N1,N0+1:N2))
       call relef3124(N0,N1,N0,N1,N2,N3,N0,N2,
     & N2,N3,N0,N1,N0,N1,N0,N2,S14,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S27(N0+1:N2,N0+1:N1,N0+1:N1,N0+1:N2))
       I1=K2*K1*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S27)
       deallocate(D1)
       deallocate(B2)
       deallocate(S14)
C
       call sul2134(N0,N1,N0,N2,N0,N1,N0,N2,X5,S27,1.000d0)
C
       allocate(X9(N0+1:N2,N0+1:N1,N0+1:N1,N0+1:N2))
       X9=0.0d0
       X9=X9+S27
       deallocate(S27)
C
       allocate(F1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       call relef523461(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N2,N0,N1,N2,N3,l3B,F1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S9(N0+1:N1,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4*K1*K2
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,F1,D2,S9)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
       call relef2143(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N2,N0,N1,N0,N2,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S8(N0+1:N1,N0+1:N2,N0+1:N1,N0+1:N2))
       I1=K2*K1*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S8)
       deallocate(D1)
       deallocate(B2)
C
       X5=X5+S8
C
       call sul2134(N0,N2,N0,N1,N0,N1,N0,N2,X9,S8,1.000d0)
       deallocate(S8)
C
       allocate(D2(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3))
       call relef2314(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N0,N1,N0,N1,N2,N3,S9,D2)
       allocate(U93(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K1*K1*K2
       call jungemm(I1,I2,I3,X9,D2,U93)
       deallocate(D2)
C
       V1B=V1B+1.0d0/2*U93
       deallocate(U93)
       deallocate(S9)
       deallocate(X9)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N2))
       call relef1243(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N0,N1,N0,N2,IntM,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,t2B,D2)
       allocate(S23(N0+1:N2,N0+1:N1,N0+1:N1,N0+1:N2))
       I1=K2*K1
       I2=K1*K2
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,D2,S23)
       deallocate(D1)
       deallocate(D2)
C
       call sul2134(N0,N1,N0,N2,N0,N1,N0,N2,X5,S23,1.000d0)
C
       call slx2143(N0,N3,N0,N1,N0,N2,N0,N1,N0,N2,X5,IntM,1.000d0)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:N1,N0+1:N2))
       call relef2134(N0,N1,N0,N2,N0,N1,N0,N2,
     & N0,N2,N0,N1,N0,N1,N0,N2,X5,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3))
       call relef2314(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N0,N1,N0,N1,N2,N3,S68,D2)
       allocate(U171(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K1*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U171)
       deallocate(D1)
       deallocate(D2)
C
       V1B=V1B+U171
       deallocate(U171)
       deallocate(S68)
       deallocate(X5)
C
       allocate(F1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       call relef623451(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N2,N0,N1,N2,N3,l3B,F1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S24(N0+1:N1,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4*K1*K2
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,F1,D2,S24)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D2(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3))
       call relef2314(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N0,N1,N0,N1,N2,N3,S24,D2)
       allocate(U90(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K1*K1*K2
       call jungemm(I1,I2,I3,S23,D2,U90)
       deallocate(D2)
C
       V1B=V1B-1.0d0/2*U90
       deallocate(U90)
       deallocate(S24)
C
       allocate(D1(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef1423(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N1,N3,N0,N2,IntM,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N1,N2+1:N3))
       call relef1324(N2,N3,N0,N1,N1,N3,N2,N3,
     & N2,N3,N1,N3,N0,N1,N2,N3,S4,D2)
       allocate(S13(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3
       I2=K4*K1
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,D2,S13)
       deallocate(D1)
       deallocate(D2)
C
       call sul1324(N0,N1,N1,N3,N2,N3,N0,N2,X8,S13,-2.000d0)
       deallocate(S13)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N1,N0+1:N2))
       call relef2143(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N0,N1,N0,N2,IntM,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef2134(N2,N3,N0,N1,N1,N3,N2,N3,
     & N0,N1,N2,N3,N1,N3,N2,N3,S4,D2)
       allocate(S12(N1+1:N3,N2+1:N3,N0+1:N1,N0+1:N2))
       I1=K2*K1
       I2=K4*K3
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,D2,S12)
       deallocate(D1)
       deallocate(D2)
       deallocate(S4)
C
       call sul2314(N0,N1,N1,N3,N2,N3,N0,N2,X8,S12,2.000d0)
       deallocate(S12)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N2,N1+1:N3,N2+1:N3))
       call relef562431(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,N2,N1,N3,N2,N3,l3B,F1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(S3(N1+1:N3,N0+1:N2,N1+1:N3,N2+1:N3))
       I1=K4*K3*K2
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,F1,D2,S3)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef1243(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N1,N3,N0,N2,IntM,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N1+1:N3,N2+1:N3))
       call relef2134(N1,N3,N0,N2,N1,N3,N2,N3,
     & N0,N2,N1,N3,N1,N3,N2,N3,S3,D2)
       allocate(U72(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K3*K3*K2
       call jungemm(I1,I2,I3,D1,D2,U72)
       deallocate(D1)
       deallocate(D2)
C
       V1B=V1B-1.0d0/2*U72
       deallocate(U72)
C
       allocate(D1(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef1324(N1,N3,N0,N2,N1,N3,N2,N3,
     & N1,N3,N1,N3,N0,N2,N2,N3,S3,D2)
       allocate(S19(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K4*K2
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,D2,S19)
       deallocate(D1)
       deallocate(D2)
       deallocate(S3)
C
       allocate(X10(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       X10=0.0d0
       call sul1324(N0,N2,N2,N3,N2,N3,N0,N2,X10,S19,1.000d0)
       deallocate(S19)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef562341(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N0,N2,N2,N3,l3B,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N0,N1,N1,N3,t3A,F2)
       allocate(S5(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K3*K1
       I3=K3*K3*K1*K1
       call jungemm(I1,I2,I3,F1,F2,S5)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N2+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef1423(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N1,N1,N3,N0,N2,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S20(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S20)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X11(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       X11=0.0d0
       X11=X11+S20
C
       allocate(X12(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       X12=0.0d0
       X12=X12+S20
C
       call slx1342(N0,N3,N0,N2,N0,N1,N1,N3,N0,N2,X11,IntM,1.000d0)
C
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3124(N0,N1,N1,N3,N0,N2,N2,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,S5,D2)
       allocate(U74(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,X11,D2,U74)
       deallocate(D2)
C
       V1B=V1B-1.0d0/4*U74
       deallocate(U74)
       deallocate(S5)
       deallocate(X11)
C
       call slx1342(N0,N3,N0,N2,N0,N1,N1,N3,N0,N2,X12,IntM,1.000d0)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef451263(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N0,N2,N2,N3,l3D,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N0,N1,N1,N3,t3C,F2)
       allocate(S155(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K3*K1
       I3=K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S155)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3124(N0,N1,N1,N3,N0,N2,N2,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,S155,D2)
       allocate(U268(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,S20,D2,U268)
       deallocate(D2)
C
       V1B=V1B-1.0d0/4*U268
       deallocate(U268)
       deallocate(S155)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef461352(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N0,N2,N2,N3,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N0,N1,N1,N3,t3B,F2)
       allocate(S71(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K3*K1
       I3=K3*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S71)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3124(N0,N1,N1,N3,N0,N2,N2,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,S71,D2)
       allocate(U173(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,X12,D2,U173)
       deallocate(D2)
C
       V1B=V1B-U173
       deallocate(U173)
       deallocate(X12)
       deallocate(S71)
C
       allocate(F1(N0+1:N2,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N2+1:N3))
       call relef452361(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N1,N3,N1,N3,N0,N1,N2,N3,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N2+1:N3))
       call relef452361(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N1,N3,N1,N3,N0,N1,N2,N3,t3B,F2)
       allocate(S6(N0+1:N1,N2+1:N3,N0+1:N1,N2+1:N3))
       I1=K4*K1
       I2=K4*K1
       I3=K3*K3*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S6)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N2,N3,N0,N2,IntM,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N2+1:N3,N2+1:N3))
       call relef3124(N0,N1,N2,N3,N0,N1,N2,N3,
     & N0,N1,N0,N1,N2,N3,N2,N3,S6,D2)
       allocate(U75(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K4*K1*K1
       call jungemm(I1,I2,I3,D1,D2,U75)
       deallocate(D1)
       deallocate(D2)
C
       V1B=V1B+1.0d0/2*U75
       deallocate(U75)
       deallocate(S6)
C
       allocate(F1(N0+1:N2,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3))
       call relef456231(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N1,N3,N1,N3,N2,N3,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3))
       call relef456213(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N1,N3,N2,N3,N1,N3,t3B,F2)
       allocate(S7(N2+1:N3,N1+1:N3,N1+1:N3,N2+1:N3))
       I1=K4*K3
       I2=K3*K4
       I3=K3*K1*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S7)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef1243(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N1,N3,N0,N2,IntM,D1)
       allocate(U76(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K3*K3*K4
       call jungemm(I1,I2,I3,D1,S7,U76)
       deallocate(D1)
C
       V1B=V1B-1.0d0/2*U76
       deallocate(U76)
       deallocate(S7)
C
       allocate(D1(N0+1:N1,N0+1:N2,N1+1:N3,N0+1:N2))
       call relef4123(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N2,N1,N3,N0,N2,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S10(N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N2))
       I1=K2*K3*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S10)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X13(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       X13=0.0d0
       call sul2134(N0,N2,N1,N3,N1,N3,N0,N2,X13,S10,1.000d0)
C
       call slx1342(N0,N3,N0,N2,N1,N3,N1,N3,N0,N2,X13,IntM,-1.000d0)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N2,N1+1:N3,N2+1:N3))
       call relef562431(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,N2,N1,N3,N2,N3,l3B,F1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(S11(N1+1:N3,N0+1:N2,N1+1:N3,N2+1:N3))
       I1=K4*K3*K2
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,F1,D2,S11)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef2134(N1,N3,N0,N2,N1,N3,N0,N2,
     & N0,N2,N1,N3,N1,N3,N0,N2,S10,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N1+1:N3,N2+1:N3))
       call relef2314(N1,N3,N0,N2,N1,N3,N2,N3,
     & N0,N2,N1,N3,N1,N3,N2,N3,S11,D2)
       allocate(U78(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K3*K3*K2
       call jungemm(I1,I2,I3,D1,D2,U78)
       deallocate(D1)
       deallocate(D2)
C
       V1B=V1B+1.0d0/2*U78
       deallocate(U78)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N2,N1+1:N3,N2+1:N3))
       call relef461532(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,N2,N1,N3,N2,N3,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2B,D2)
       allocate(S69(N1+1:N3,N0+1:N2,N1+1:N3,N2+1:N3))
       I1=K4*K3*K2
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,F1,D2,S69)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D2(N0+1:N2,N1+1:N3,N1+1:N3,N2+1:N3))
       call relef2314(N1,N3,N0,N2,N1,N3,N2,N3,
     & N0,N2,N1,N3,N1,N3,N2,N3,S69,D2)
       allocate(U164(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K3*K3*K2
       call jungemm(I1,I2,I3,X13,D2,U164)
       deallocate(D2)
C
       V1B=V1B+U164
       deallocate(U164)
       deallocate(X13)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N0,N2,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S16(N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K4
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S16)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef1324(N1,N3,N2,N3,N1,N3,N0,N2,
     & N1,N3,N1,N3,N2,N3,N0,N2,S16,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef3124(N1,N3,N0,N2,N1,N3,N2,N3,
     & N1,N3,N1,N3,N0,N2,N2,N3,S69,D2)
       allocate(S86(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K4*K2
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,D2,S86)
       deallocate(D1)
       deallocate(D2)
C
       call sul1324(N0,N2,N2,N3,N2,N3,N0,N2,X10,S86,-2.000d0)
       deallocate(S86)
C
       allocate(F1(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef456132(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N1,N3,N2,N3,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef456123(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,t3C,F2)
       allocate(S79(N2+1:N3,N1+1:N3,N1+1:N3,N2+1:N3))
       I1=K4*K3
       I2=K3*K4
       I3=K4*K1*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S79)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef2134(N1,N3,N2,N3,N1,N3,N0,N2,
     & N2,N3,N1,N3,N1,N3,N0,N2,S16,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N1+1:N3,N2+1:N3))
       call relef1324(N2,N3,N1,N3,N1,N3,N2,N3,
     & N2,N3,N1,N3,N1,N3,N2,N3,S79,D2)
       allocate(U168(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K3*K3*K4
       call jungemm(I1,I2,I3,D1,D2,U168)
       deallocate(D1)
       deallocate(D2)
C
       V1B=V1B+1.0d0/2*U168
       deallocate(U168)
       deallocate(S79)
C
       allocate(D1(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef1324(N1,N3,N2,N3,N1,N3,N0,N2,
     & N1,N3,N1,N3,N2,N3,N0,N2,S16,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef3124(N1,N3,N0,N2,N1,N3,N2,N3,
     & N1,N3,N1,N3,N0,N2,N2,N3,S11,D2)
       allocate(S28(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K4*K2
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,D2,S28)
       deallocate(D1)
       deallocate(D2)
       deallocate(S11)
C
       call sul1324(N0,N2,N2,N3,N2,N3,N0,N2,X10,S28,-1.000d0)
       deallocate(S28)
C
       allocate(F1(N0+1:N2,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3))
       call relef456231(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N1,N3,N1,N3,N2,N3,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3))
       call relef456213(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N1,N3,N2,N3,N1,N3,t3B,F2)
       allocate(S17(N2+1:N3,N1+1:N3,N1+1:N3,N2+1:N3))
       I1=K4*K3
       I2=K3*K4
       I3=K3*K1*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S17)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef2134(N1,N3,N2,N3,N1,N3,N0,N2,
     & N2,N3,N1,N3,N1,N3,N0,N2,S16,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N1+1:N3,N2+1:N3))
       call relef1324(N2,N3,N1,N3,N1,N3,N2,N3,
     & N2,N3,N1,N3,N1,N3,N2,N3,S17,D2)
       allocate(U82(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K3*K3*K4
       call jungemm(I1,I2,I3,D1,D2,U82)
       deallocate(D1)
       deallocate(D2)
C
       V1B=V1B+1.0d0/2*U82
       deallocate(U82)
       deallocate(S17)
C
       allocate(D1(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef1324(N1,N3,N0,N2,N1,N3,N2,N3,
     & N1,N3,N1,N3,N0,N2,N2,N3,S69,D2)
       allocate(S80(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K4*K2
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,D2,S80)
       deallocate(D1)
       deallocate(D2)
       deallocate(S69)
C
       call sul1324(N0,N2,N2,N3,N2,N3,N0,N2,X10,S80,2.000d0)
       deallocate(S80)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N0,N2,IntM,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N0+1:N2,N1+1:N3))
       call relef4132(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N0,N2,N1,N3,t2B,D2)
       allocate(S21(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3
       I2=K3*K2
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,D2,S21)
       deallocate(D1)
       deallocate(D2)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N2,N1+1:N3,N2+1:N3))
       call relef563421(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,N2,N1,N3,N2,N3,l3B,F1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(S22(N1+1:N3,N0+1:N2,N1+1:N3,N2+1:N3))
       I1=K4*K3*K2
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,F1,D2,S22)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D2(N0+1:N2,N1+1:N3,N1+1:N3,N2+1:N3))
       call relef2314(N1,N3,N0,N2,N1,N3,N2,N3,
     & N0,N2,N1,N3,N1,N3,N2,N3,S22,D2)
       allocate(U89(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K3*K3*K2
       call jungemm(I1,I2,I3,S21,D2,U89)
       deallocate(D2)
C
       V1B=V1B-1.0d0/2*U89
       deallocate(U89)
       deallocate(S22)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N2,N1+1:N3,N2+1:N3))
       call relef561432(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,N2,N1,N3,N2,N3,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2B,D2)
       allocate(S82(N1+1:N3,N0+1:N2,N1+1:N3,N2+1:N3))
       I1=K4*K3*K2
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,F1,D2,S82)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D2(N0+1:N2,N1+1:N3,N1+1:N3,N2+1:N3))
       call relef2314(N1,N3,N0,N2,N1,N3,N2,N3,
     & N0,N2,N1,N3,N1,N3,N2,N3,S82,D2)
       allocate(U175(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K3*K3*K2
       call jungemm(I1,I2,I3,S21,D2,U175)
       deallocate(D2)
C
       V1B=V1B-U175
       deallocate(U175)
       deallocate(S82)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef562341(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N0,N2,N2,N3,l3B,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef562341(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N0,N2,N2,N3,t3B,F2)
       allocate(S29(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K4*K2
       I3=K3*K3*K1*K1
       call jungemm(I1,I2,I3,F1,F2,S29)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef2314(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S30(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S30)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X14(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       X14=0.0d0
       X14=X14-S30
C
       call slx1324(N0,N3,N0,N2,N0,N2,N2,N3,N0,N2,X14,IntB,1.000d0)
C
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3124(N0,N2,N2,N3,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,S29,D2)
       allocate(U95(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,X14,D2,U95)
       deallocate(D2)
C
       V1B=V1B+1.0d0/4*U95
       deallocate(U95)
       deallocate(S29)
       deallocate(X14)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef451623(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N0,N2,N2,N3,N2,N3,l3D,F1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(S156(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4*K2
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,F1,D2,S156)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1324(N0,N2,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N0,N2,N0,N2,S30,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef2134(N2,N3,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,S156,D2)
       allocate(S166(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K4*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S166)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N0,N2,N2,N3,N2,N3,N0,N2,X10,S166,-1.000d0)
       deallocate(S166)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N2,N3,t2C,D2)
       allocate(S165(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K4*K2
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S165)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X15(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       X15=0.0d0
       call sul1324(N0,N2,N2,N3,N2,N3,N0,N2,X15,S165,1.000d0)
       deallocate(S165)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,N3,t2B,D2)
       allocate(S83(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K4*K2
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S83)
       deallocate(D1)
       deallocate(D2)
C
       call sul1324(N0,N2,N2,N3,N2,N3,N0,N2,X15,S83,-1.000d0)
C
       allocate(X16(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       X16=0.0d0
       X16=X16+S83
       deallocate(S83)
C
       call slx1234(N0,N3,N0,N2,N2,N3,N2,N3,N0,N2,X15,IntB,-1.000d0)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef1324(N0,N2,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,X15,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef2314(N2,N3,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,S156,D2)
       allocate(U280(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,D1,D2,U280)
       deallocate(D1)
       deallocate(D2)
C
       V1B=V1B-1.0d0/2*U280
       deallocate(U280)
       deallocate(X15)
C
       call slx1324(N0,N3,N0,N2,N2,N3,N2,N3,N0,N2,X16,IntB,1.000d0)
C
       allocate(F1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef463512(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N0,N2,N2,N3,N2,N3,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,t2B,D2)
       allocate(S84(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4*K2
       I2=K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,F1,D2,S84)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef2314(N2,N3,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,S84,D2)
       allocate(U176(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,X16,D2,U176)
       deallocate(D2)
C
       V1B=V1B+U176
       deallocate(U176)
       deallocate(X16)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef3124(N2,N3,N0,N2,N2,N3,N2,N3,
     & N2,N3,N2,N3,N0,N2,N2,N3,S84,D2)
       allocate(S92(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K4*K2
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,D2,S92)
       deallocate(D1)
       deallocate(D2)
C
       call sul1324(N0,N2,N2,N3,N2,N3,N0,N2,X10,S92,2.000d0)
       deallocate(S92)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1324(N0,N2,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N0,N2,N0,N2,S30,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef2134(N2,N3,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,S84,D2)
       allocate(S100(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K4*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S100)
       deallocate(D1)
       deallocate(D2)
C
       call sul2314(N0,N2,N2,N3,N2,N3,N0,N2,X10,S100,-2.000d0)
       deallocate(S100)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef451263(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N0,N2,N2,N3,l3D,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef451263(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N0,N2,N2,N3,t3D,F2)
       allocate(S163(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K4*K2
       I3=K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S163)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3124(N0,N2,N2,N3,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,S163,D2)
       allocate(U277(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,S30,D2,U277)
       deallocate(D2)
C
       V1B=V1B-1.0d0/4*U277
       deallocate(U277)
       deallocate(S163)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef461352(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N0,N2,N2,N3,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef461352(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N0,N2,N2,N3,t3C,F2)
       allocate(S93(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K4*K2
       I3=K3*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S93)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3124(N0,N2,N2,N3,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,S93,D2)
       allocate(U188(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,S30,D2,U188)
       deallocate(D2)
C
       V1B=V1B-U188
       deallocate(U188)
       deallocate(S93)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef3124(N0,N2,N0,N2,N2,N3,N0,N2,
     & N2,N3,N0,N2,N0,N2,N0,N2,S30,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S99(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S99)
       deallocate(D1)
       deallocate(B2)
       deallocate(S30)
C
       allocate(X17(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       X17=0.0d0
       call sul2134(N0,N2,N0,N2,N0,N2,N0,N2,X17,S99,1.000d0)
C
       allocate(X18(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       X18=0.0d0
       call sul1324(N0,N2,N0,N2,N0,N2,N0,N2,X18,S99,2.000d0)
       deallocate(S99)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       call relef412563(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,N0,N2,N2,N3,l3D,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S157(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2*K2
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S157)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2C,D2)
       allocate(S98(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K2
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,D2,S98)
       deallocate(D1)
       deallocate(D2)
C
       call sul2134(N0,N2,N0,N2,N0,N2,N0,N2,X17,S98,1.0d0/2)
C
       call sul1324(N0,N2,N0,N2,N0,N2,N0,N2,X18,S98,1.000d0)
       deallocate(S98)
C
       call slx2134(N0,N3,N0,N2,N0,N2,N0,N2,N0,N2,X17,IntB,1.000d0)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef2134(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,X17,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       call relef2314(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N0,N2,N2,N3,S157,D2)
       allocate(U282(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K2*K2*K2
       call jungemm(I1,I2,I3,D1,D2,U282)
       deallocate(D1)
       deallocate(D2)
C
       V1B=V1B+1.0d0/4*U282
       deallocate(U282)
       deallocate(S157)
       deallocate(X17)
C
       allocate(F1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       call relef613452(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,N0,N2,N2,N3,l3C,F1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(S87(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2*K2
       I2=K2
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,F1,D2,S87)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef2134(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N0,N2,N0,N2,IntB,D1)
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
C
       call sul1324(N0,N2,N0,N2,N0,N2,N0,N2,X18,S90,-4.000d0)
C
       call slx1324(N0,N3,N0,N2,N0,N2,N0,N2,N0,N2,X18,IntB,2.000d0)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef1324(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,X18,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       call relef2314(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N0,N2,N2,N3,S87,D2)
       allocate(U192(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K2*K2*K2
       call jungemm(I1,I2,I3,D1,D2,U192)
       deallocate(D1)
       deallocate(D2)
C
       V1B=V1B+1.0d0/4*U192
       deallocate(U192)
       deallocate(S87)
       deallocate(X18)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       call relef412563(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,N0,N2,N2,N3,l3D,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S160(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2*K2
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S160)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D2(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       call relef2314(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N0,N2,N2,N3,S160,D2)
       allocate(U274(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K2*K2*K2
       call jungemm(I1,I2,I3,S90,D2,U274)
       deallocate(D2)
C
       V1B=V1B-1.0d0/2*U274
       deallocate(U274)
       deallocate(S160)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef1324(N2,N3,N0,N2,N2,N3,N2,N3,
     & N2,N3,N2,N3,N0,N2,N2,N3,S156,D2)
       allocate(S162(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K4*K2
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,D2,S162)
       deallocate(D1)
       deallocate(D2)
C
       call sul1324(N0,N2,N2,N3,N2,N3,N0,N2,X10,S162,1.000d0)
       deallocate(S162)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef2134(N2,N3,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,S156,D2)
       allocate(S161(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K4*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S161)
       deallocate(D1)
       deallocate(D2)
       deallocate(S156)
C
       call sul2314(N0,N2,N2,N3,N2,N3,N0,N2,X10,S161,1.000d0)
       deallocate(S161)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef2134(N2,N3,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,S84,D2)
       allocate(S91(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K4*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S91)
       deallocate(D1)
       deallocate(D2)
       deallocate(S84)
C
       call sul2314(N0,N2,N2,N3,N2,N3,N0,N2,X10,S91,2.000d0)
       deallocate(S91)
C
       allocate(F1(N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef561234(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,l3B,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef561234(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,t3B,F2)
       allocate(Q100(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K3*K3*K4*K1*K1
       call jungemm(I1,I2,I3,F1,F2,Q100)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef4132(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q128(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,Q128)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X19(N0+1:N2,N2+1:N3))
       X19=0.0d0
       X19=X19-Q128
C
       allocate(X20(N0+1:N2,N2+1:N3))
       X20=0.0d0
       X20=X20+Q128
C
       allocate(X21(N0+1:N2,N2+1:N3))
       X21=0.0d0
       X21=X21-Q128
       deallocate(Q128)
C
       call slx21(0,N3,N0,N2,N2,N3,X19,FockB,1.000d0)
C
       allocate(U101(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K2
       call jungemm(I1,I2,I3,X19,Q100,U101)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U101,-1.0d0/4)
       deallocate(U101)
       deallocate(Q100)
       deallocate(X19)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,l3D,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,t3D,F2)
       allocate(Q287(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4*K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,Q287)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N2,N3,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q115(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,Q115)
       deallocate(D1)
       deallocate(B2)
C
       X20=X20-Q115
C
       X21=X21+Q115
C
       allocate(U300(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K2
       call jungemm(I1,I2,I3,X20,Q287,U300)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U300,1.0d0/12)
       deallocate(U300)
       deallocate(Q287)
       deallocate(X20)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef461235(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef461235(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,t3C,F2)
       allocate(Q210(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K3*K4*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,Q210)
       deallocate(F1)
       deallocate(F2)
C
       allocate(U211(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K2
       call jungemm(I1,I2,I3,X21,Q210,U211)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U211,-1.0d0/2)
       deallocate(U211)
       deallocate(X21)
       deallocate(Q210)
C
       allocate(F1(N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef561234(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,l3B,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef561234(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,t3B,F2)
       allocate(Q114(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K3*K3*K4*K1*K1
       call jungemm(I1,I2,I3,F1,F2,Q114)
       deallocate(F1)
       deallocate(F2)
C
       allocate(U116(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K2
       call jungemm(I1,I2,I3,Q115,Q114,U116)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U116,-1.0d0/4)
       deallocate(U116)
       deallocate(Q114)
C
       allocate(F1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef523614(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,N2,N3,N0,N2,l3B,F1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S32(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4*K1
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,F1,D2,S32)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N2+1:N3,N2+1:N3))
       call relef2431(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N2,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef2134(N0,N1,N0,N1,N2,N3,N0,N2,
     & N0,N1,N0,N1,N2,N3,N0,N2,S32,D2)
       allocate(U102(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4*K1*K1
       call jungemm(I1,I2,I3,D1,D2,U102)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U102,-1.0d0/2)
       deallocate(U102)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N0+1:N1,N0+1:N2))
       call relef1324(N0,N1,N0,N1,N2,N3,N0,N2,
     & N0,N1,N2,N3,N0,N1,N0,N2,S32,D2)
       allocate(S38(N0+1:N1,N0+1:N2,N1+1:N3,N2+1:N3))
       I1=K4*K3
       I2=K2*K1
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,D2,S38)
       deallocate(D1)
       deallocate(D2)
C
       call sul1423(N0,N1,N1,N3,N2,N3,N0,N2,X8,S38,-1.000d0)
       deallocate(S38)
C
       allocate(D1(N2+1:N3,N0+1:N1,N0+1:N1,N0+1:N2))
       call relef3214(N0,N1,N0,N1,N2,N3,N0,N2,
     & N2,N3,N0,N1,N0,N1,N0,N2,S32,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S47(N0+1:N2,N0+1:N1,N0+1:N1,N0+1:N2))
       I1=K2*K1*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S47)
       deallocate(D1)
       deallocate(B2)
       deallocate(S32)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3))
       call relef3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N0,N1,N2,N3,IntM,D1)
       allocate(U117(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K1*K1*K2
       call jungemm(I1,I2,I3,D1,S47,U117)
       deallocate(D1)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U117,1.0d0/2)
       deallocate(U117)
       deallocate(S47)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef562134(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N2,N3,N1,N3,N0,N2,l3B,F1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(S33(N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K4
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,F1,D2,S33)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N1+1:N3,N1+1:N3,N2+1:N3))
       call relef3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N1,N3,N2,N3,IntM,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef2134(N1,N3,N2,N3,N1,N3,N0,N2,
     & N2,N3,N1,N3,N1,N3,N0,N2,S33,D2)
       allocate(U103(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K3*K3*K4
       call jungemm(I1,I2,I3,D1,D2,U103)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U103,-1.0d0/2)
       deallocate(U103)
       deallocate(S33)
C
       allocate(F1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef512634(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N1,N1,N3,N0,N2,l3B,F1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(S34(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3*K1
       I2=K2
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,F1,D2,S34)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(U104(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,S34,U104)
       deallocate(D1)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U104,-1.0d0)
       deallocate(U104)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:N3,N2+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N1,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N0+1:N1,N0+1:N2))
       call relef1324(N0,N2,N0,N1,N1,N3,N0,N2,
     & N0,N2,N1,N3,N0,N1,N0,N2,S34,D2)
       allocate(S42(N0+1:N1,N0+1:N2,N1+1:N3,N2+1:N3))
       I1=K4*K3
       I2=K2*K1
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,D2,S42)
       deallocate(D1)
       deallocate(D2)
C
       call sul1423(N0,N1,N1,N3,N2,N3,N0,N2,X8,S42,-2.000d0)
       deallocate(S42)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
       call relef3124(N0,N2,N0,N1,N1,N3,N0,N2,
     & N1,N3,N0,N2,N0,N1,N0,N2,S34,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S41(N0+1:N1,N0+1:N2,N0+1:N1,N0+1:N2))
       I1=K2*K1*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S41)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3))
       call relef3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N0,N1,N2,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N0+1:N1,N0+1:N2))
       call relef2314(N0,N1,N0,N2,N0,N1,N0,N2,
     & N0,N2,N0,N1,N0,N1,N0,N2,S41,D2)
       allocate(U110(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K1*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U110)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U110,1.0d0)
       deallocate(U110)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call relef2314(N0,N2,N0,N1,N1,N3,N0,N2,
     & N0,N1,N1,N3,N0,N2,N0,N2,S34,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,N3,t2B,D2)
       allocate(S64(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K4*K2
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S64)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef3124(N0,N2,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N0,N2,S64,D2)
       allocate(U130(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,D2,U130)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U130,1.0d0)
       deallocate(U130)
       deallocate(S64)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call relef2314(N0,N2,N0,N1,N1,N3,N0,N2,
     & N0,N1,N1,N3,N0,N2,N0,N2,S34,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N1,N3,t2A,D2)
       allocate(S55(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K3*K1
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S55)
       deallocate(D1)
       deallocate(D2)
       deallocate(S34)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef3124(N0,N1,N1,N3,N0,N2,N0,N2,
     & N0,N2,N0,N1,N1,N3,N0,N2,S55,D2)
       allocate(U122(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U122)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U122,-1.0d0)
       deallocate(U122)
       deallocate(S55)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:N1,N0+1:N2))
       call relef3214(N0,N1,N0,N2,N0,N1,N0,N2,
     & N0,N1,N0,N2,N0,N1,N0,N2,S41,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S56(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
       I1=K2*K1*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S56)
       deallocate(D1)
       deallocate(B2)
       deallocate(S41)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef2314(N1,N3,N0,N2,N0,N1,N0,N2,
     & N0,N2,N0,N1,N1,N3,N0,N2,S56,D2)
       allocate(U123(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U123)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U123,1.0d0)
       deallocate(U123)
       deallocate(S56)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef562314(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N2,N3,N0,N2,l3B,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef451362(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N0,N1,N1,N3,t3A,F2)
       allocate(S35(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K3*K1
       I3=K3*K3*K1*K1
       call jungemm(I1,I2,I3,F1,F2,S35)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef1324(N0,N1,N1,N3,N2,N3,N0,N2,
     & N0,N1,N2,N3,N1,N3,N0,N2,S35,D2)
       allocate(U105(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,D1,D2,U105)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U105,-1.0d0/4)
       deallocate(U105)
       deallocate(S35)
C
       allocate(F1(N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N2))
       call relef512364(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,N0,N2,l3B,F1)
       allocate(F2(N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef512346(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,t3B,F2)
       allocate(S36(N0+1:N2,N0+1:N1,N0+1:N1,N0+1:N2))
       I1=K2*K1
       I2=K1*K2
       I3=K3*K3*K4*K1
       call jungemm(I1,I2,I3,F1,F2,S36)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3))
       call relef3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N0,N1,N2,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N0+1:N1,N0+1:N2))
       call relef1324(N0,N2,N0,N1,N0,N1,N0,N2,
     & N0,N2,N0,N1,N0,N1,N0,N2,S36,D2)
       allocate(U106(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K1*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U106)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U106,1.0d0/2)
       deallocate(U106)
       deallocate(S36)
C
       allocate(F1(N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef561234(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,l3B,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3))
       call relef561243(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N2,N3,N1,N3,N0,N2,N1,N3,t3B,F2)
       allocate(S37(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3
       I2=K3*K2
       I3=K3*K4*K1*K1
       call jungemm(I1,I2,I3,F1,F2,S37)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:N3,N2+1:N3))
       call relef3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N1,N3,N2,N3,IntM,D1)
       allocate(U107(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K3*K3*K2
       call jungemm(I1,I2,I3,D1,S37,U107)
       deallocate(D1)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U107,-1.0d0/2)
       deallocate(U107)
       deallocate(S37)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef562134(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N2,N3,N1,N3,N0,N2,l3B,F1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(S39(N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K4
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,F1,D2,S39)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N1,N2+1:N3))
       call relef3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N0,N1,N2,N3,IntM,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef2134(N1,N3,N2,N3,N1,N3,N0,N2,
     & N2,N3,N1,N3,N1,N3,N0,N2,S39,D2)
       allocate(S40(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4*K1
       I2=K2*K3
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,D2,S40)
       deallocate(D1)
       deallocate(D2)
C
       call sul2413(N0,N1,N1,N3,N2,N3,N0,N2,X8,S40,-1.000d0)
       deallocate(S40)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef2431(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N0,N2,N2,N3,IntM,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef1324(N1,N3,N2,N3,N1,N3,N0,N2,
     & N1,N3,N1,N3,N2,N3,N0,N2,S39,D2)
       allocate(S48(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K4
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,D2,S48)
       deallocate(D1)
       deallocate(D2)
C
       call sul2413(N0,N2,N2,N3,N2,N3,N0,N2,X10,S48,1.000d0)
       deallocate(S48)
C
       allocate(D1(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef2314(N1,N3,N2,N3,N1,N3,N0,N2,
     & N2,N3,N1,N3,N1,N3,N0,N2,S39,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S60(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K3
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S60)
       deallocate(D1)
       deallocate(B2)
       deallocate(S39)
C
       allocate(D1(N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N2))
       call relef2134(N0,N2,N1,N3,N1,N3,N0,N2,
     & N1,N3,N0,N2,N1,N3,N0,N2,S60,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S61(N0+1:N1,N0+1:N2,N1+1:N3,N0+1:N2))
       I1=K2*K3*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S61)
       deallocate(D1)
       deallocate(B2)
       deallocate(S60)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef2134(N0,N1,N0,N2,N1,N3,N0,N2,
     & N0,N2,N0,N1,N1,N3,N0,N2,S61,D2)
       allocate(U125(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U125)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U125,1.0d0/2)
       deallocate(U125)
       deallocate(S61)
C
       allocate(F1(N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N2))
       call relef512364(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,N0,N2,l3B,F1)
       allocate(F2(N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef512346(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,t3B,F2)
       allocate(S43(N0+1:N2,N0+1:N1,N0+1:N1,N0+1:N2))
       I1=K2*K1
       I2=K1*K2
       I3=K3*K3*K4*K1
       call jungemm(I1,I2,I3,F1,F2,S43)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:N1,N0+1:N2))
       call relef3124(N0,N2,N0,N1,N0,N1,N0,N2,
     & N0,N1,N0,N2,N0,N1,N0,N2,S43,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S44(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
       I1=K2*K1*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S44)
       deallocate(D1)
       deallocate(B2)
       deallocate(S43)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef2314(N1,N3,N0,N2,N0,N1,N0,N2,
     & N0,N2,N0,N1,N1,N3,N0,N2,S44,D2)
       allocate(U112(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U112)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U112,1.0d0/2)
       deallocate(U112)
       deallocate(S44)
C
       allocate(F1(N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef561234(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,l3B,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3))
       call relef561243(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N2,N3,N1,N3,N0,N2,N1,N3,t3B,F2)
       allocate(S45(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3
       I2=K3*K2
       I3=K3*K4*K1*K1
       call jungemm(I1,I2,I3,F1,F2,S45)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N2))
       call relef3124(N0,N2,N1,N3,N1,N3,N0,N2,
     & N1,N3,N0,N2,N1,N3,N0,N2,S45,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S46(N0+1:N1,N0+1:N2,N1+1:N3,N0+1:N2))
       I1=K2*K3*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S46)
       deallocate(D1)
       deallocate(B2)
       deallocate(S45)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef2134(N0,N1,N0,N2,N1,N3,N0,N2,
     & N0,N2,N0,N1,N1,N3,N0,N2,S46,D2)
       allocate(U113(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U113)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U113,1.0d0/2)
       deallocate(U113)
       deallocate(S46)
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
       allocate(D1(N2+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef3124(N0,N1,N1,N3,N2,N3,N0,N2,
     & N2,N3,N0,N1,N1,N3,N0,N2,S49,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S50(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S50)
       deallocate(D1)
       deallocate(B2)
       deallocate(S49)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(U119(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,S50,U119)
       deallocate(D1)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U119,-1.0d0/4)
       deallocate(U119)
       deallocate(S50)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef563124(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N2,N3,N1,N3,N0,N2,l3B,F1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(S51(N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K4
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,F1,D2,S51)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef2314(N1,N3,N2,N3,N1,N3,N0,N2,
     & N2,N3,N1,N3,N1,N3,N0,N2,S51,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,t2B,D2)
       allocate(S52(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3
       I2=K1*K2
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,D2,S52)
       deallocate(D1)
       deallocate(D2)
       deallocate(S51)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(U120(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,S52,U120)
       deallocate(D1)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U120,-1.0d0/2)
       deallocate(U120)
       deallocate(S52)
C
       allocate(F1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef623514(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,N2,N3,N0,N2,l3B,F1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S53(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4*K1
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,F1,D2,S53)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N1,N0+1:N2))
       call relef2314(N0,N1,N0,N1,N2,N3,N0,N2,
     & N0,N1,N2,N3,N0,N1,N0,N2,S53,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N0+1:N2,N1+1:N3))
       call relef4132(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N0,N2,N1,N3,t2B,D2)
       allocate(S54(N0+1:N2,N1+1:N3,N0+1:N1,N0+1:N2))
       I1=K2*K1
       I2=K3*K2
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,D2,S54)
       deallocate(D1)
       deallocate(D2)
       deallocate(S53)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef1324(N0,N2,N1,N3,N0,N1,N0,N2,
     & N0,N2,N0,N1,N1,N3,N0,N2,S54,D2)
       allocate(U121(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U121)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U121,-1.0d0/2)
       deallocate(U121)
       deallocate(S54)
C
       allocate(F1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef523614(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,N2,N3,N0,N2,l3B,F1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S57(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4*K1
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,F1,D2,S57)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N0+1:N1,N0+1:N1,N0+1:N2))
       call relef3214(N0,N1,N0,N1,N2,N3,N0,N2,
     & N2,N3,N0,N1,N0,N1,N0,N2,S57,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S58(N0+1:N2,N0+1:N1,N0+1:N1,N0+1:N2))
       I1=K2*K1*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S58)
       deallocate(D1)
       deallocate(B2)
       deallocate(S57)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:N1,N0+1:N2))
       call relef2134(N0,N2,N0,N1,N0,N1,N0,N2,
     & N0,N1,N0,N2,N0,N1,N0,N2,S58,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S59(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
       I1=K2*K1*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S59)
       deallocate(D1)
       deallocate(B2)
       deallocate(S58)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef2314(N1,N3,N0,N2,N0,N1,N0,N2,
     & N0,N2,N0,N1,N1,N3,N0,N2,S59,D2)
       allocate(U124(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U124)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U124,1.0d0/2)
       deallocate(U124)
       deallocate(S59)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef562314(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N2,N3,N0,N2,l3B,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef562341(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,N0,N2,N2,N3,t3B,F2)
       allocate(S62(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K4*K2
       I3=K3*K3*K1*K1
       call jungemm(I1,I2,I3,F1,F2,S62)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef1324(N0,N2,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,S62,D2)
       allocate(U126(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,D1,D2,U126)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U126,1.0d0/4)
       deallocate(U126)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef3124(N0,N2,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,N3,N0,N2,S62,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S63(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S63)
       deallocate(D1)
       deallocate(B2)
       deallocate(S62)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef2134(N0,N2,N0,N2,N2,N3,N0,N2,
     & N0,N2,N0,N2,N2,N3,N0,N2,S63,D2)
       allocate(U127(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,D2,U127)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U127,-1.0d0/4)
       deallocate(U127)
       deallocate(S63)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,t3B,F2)
       allocate(Q131(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K3*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,Q131)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N2,N3,N0,N2,IntM,D1)
       allocate(B2(N0+1:N1,N0+1:N1))
       call relef21(N0,N1,N0,N1,
     & N0,N1,N0,N1,Q131,B2)
       allocate(U132(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K1*K1
       call jungemm1(I1,I3,D1,B2,U132)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B-1.0d0/2*U132
       deallocate(U132)
       deallocate(Q131)
C
       allocate(F1(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,t3B,F2)
       allocate(Q133(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K3*K4*K1*K1*K2
       call jungemm(I1,I2,I3,F1,F2,Q133)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(U134(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K3*K3
       call jungemm1(I1,I3,D1,Q133,U134)
       deallocate(D1)
C
       V1B=V1B+1.0d0/2*U134
       deallocate(U134)
       deallocate(Q133)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,t3B,F2)
       allocate(Q135(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K3*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,Q135)
       deallocate(F1)
       deallocate(F2)
C
       allocate(B1(N0+1:N1,N0+1:N1))
       call relef21(N0,N1,N0,N1,
     & N0,N1,N0,N1,Q135,B1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q136(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,B1,B2,Q136)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q135)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,Q136,B2)
       allocate(U137(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,U137)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B-1.0d0/2*U137
       deallocate(U137)
       deallocate(Q136)
C
       allocate(F1(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3))
       call relef456123(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,t3B,F2)
       allocate(Q138(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K3*K4*K1*K1*K2
       call jungemm(I1,I2,I3,F1,F2,Q138)
       deallocate(F1)
       deallocate(F2)
C
       allocate(B1(N1+1:N3,N1+1:N3))
       call relef21(N1,N3,N1,N3,
     & N1,N3,N1,N3,Q138,B1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(Q139(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,B1,B2,Q139)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q138)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(U140(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K3*K1
       call jungemm1(I1,I3,D1,Q139,U140)
       deallocate(D1)
C
       V1B=V1B-1.0d0/2*U140
       deallocate(U140)
       deallocate(Q139)
C
       allocate(F1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       call relef412563(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,N0,N1,N1,N3,l3B,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(S65(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1*K1
       I2=K1
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S65)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call relef2341(N0,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,N0,N1,S65,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(Q141(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,D1,D2,Q141)
       deallocate(D1)
       deallocate(D2)
       deallocate(S65)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,Q141,B2)
       allocate(U142(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,U142)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B+1.0d0/2*U142
       deallocate(U142)
       deallocate(Q141)
C
       allocate(F1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       call relef623451(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N2,N0,N1,N2,N3,l3B,F1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S66(N0+1:N1,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4*K1*K2
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,F1,D2,S66)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef2341(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N0,N1,N2,N3,N0,N1,S66,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2B,D2)
       allocate(Q143(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,Q143)
       deallocate(D1)
       deallocate(D2)
       deallocate(S66)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,Q143,B2)
       allocate(U144(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,U144)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B+1.0d0/2*U144
       deallocate(U144)
       deallocate(Q143)
C
       allocate(F1(N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef561234(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,l3B,F1)
       allocate(F2(N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef561234(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,t3B,F2)
       allocate(Q145(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K3*K3*K4*K1*K1
       call jungemm(I1,I2,I3,F1,F2,Q145)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N0+1:N2))
       call relef21(N0,N2,N0,N2,
     & N0,N2,N0,N2,Q145,B2)
       allocate(U146(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K2*K2
       call jungemm1(I1,I3,D1,B2,U146)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B-1.0d0/4*U146
       deallocate(U146)
C
       allocate(B1(N0+1:N2,N0+1:N2))
       call relef21(N0,N2,N0,N2,
     & N0,N2,N0,N2,Q145,B1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q149(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,B1,B2,Q149)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q145)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,Q149,B2)
       allocate(U150(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,U150)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B-1.0d0/4*U150
       deallocate(U150)
       deallocate(Q149)
C
       allocate(F1(N0+1:N2,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3))
       call relef456231(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N1,N3,N1,N3,N2,N3,l3B,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3))
       call relef456231(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N0,N1,N1,N3,N1,N3,N2,N3,t3B,F2)
       allocate(Q147(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K3*K3*K1*K1*K2
       call jungemm(I1,I2,I3,F1,F2,Q147)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N2+1:N3,N2+1:N3))
       call relef21(N2,N3,N2,N3,
     & N2,N3,N2,N3,Q147,B2)
       allocate(U148(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K4*K4
       call jungemm1(I1,I3,D1,B2,U148)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B+1.0d0/4*U148
       deallocate(U148)
C
       allocate(B1(N2+1:N3,N2+1:N3))
       call relef21(N2,N3,N2,N3,
     & N2,N3,N2,N3,Q147,B1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(Q151(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,B1,B2,Q151)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q147)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U152(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K4*K2
       call jungemm1(I1,I3,D1,Q151,U152)
       deallocate(D1)
C
       V1B=V1B-1.0d0/4*U152
       deallocate(U152)
       deallocate(Q151)
C
       allocate(F1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       call relef512463(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,N0,N1,N1,N3,l3B,F1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(S67(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1*K2
       I2=K2
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,F1,D2,S67)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N2,N0,N1,N1,N3,
     & N0,N2,N0,N1,N1,N3,N0,N2,S67,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,t2B,D2)
       allocate(Q153(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,D2,Q153)
       deallocate(D1)
       deallocate(D2)
       deallocate(S67)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,Q153,B2)
       allocate(U154(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,U154)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B-U154
       deallocate(U154)
       deallocate(Q153)
C
       allocate(F1(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef456132(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N1,N3,N2,N3,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef456132(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N1,N3,N2,N3,t3C,F2)
       allocate(Q155(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K3*K4*K1*K2*K2
       call jungemm(I1,I2,I3,F1,F2,Q155)
       deallocate(F1)
       deallocate(F2)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call relef12(0,N3,0,N3,
     & N2,N3,N0,N2,FockB,B1)
       allocate(U156(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K4
       call jungemm(I1,I2,I3,B1,Q155,U156)
       deallocate(B1)
C
       V1B=V1B-1.0d0/2*U156
       deallocate(U156)
       deallocate(Q155)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef451632(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N0,N1,N1,N3,N2,N3,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(S70(N2+1:N3,N0+1:N1,N1+1:N3,N2+1:N3))
       I1=K4*K3*K1
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,F1,D2,S70)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef2143(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N0,N2,IntM,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef2134(N2,N3,N0,N1,N1,N3,N2,N3,
     & N0,N1,N2,N3,N1,N3,N2,N3,S70,D2)
       allocate(U159(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,D1,D2,U159)
       deallocate(D1)
       deallocate(D2)
C
       V1B=V1B-1.0d0/2*U159
       deallocate(U159)
       deallocate(S70)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1,N2+1:N3))
       call relef451362(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N1,N3,N0,N1,N2,N3,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1,N2+1:N3))
       call relef451362(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N1,N3,N0,N1,N2,N3,t3C,F2)
       allocate(S72(N0+1:N1,N2+1:N3,N0+1:N1,N2+1:N3))
       I1=K4*K1
       I2=K4*K1
       I3=K3*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S72)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N2,N3,N0,N2,IntM,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N2+1:N3,N2+1:N3))
       call relef3124(N0,N1,N2,N3,N0,N1,N2,N3,
     & N0,N1,N0,N1,N2,N3,N2,N3,S72,D2)
       allocate(U161(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K4*K1*K1
       call jungemm(I1,I2,I3,D1,D2,U161)
       deallocate(D1)
       deallocate(D2)
C
       V1B=V1B+1.0d0/2*U161
       deallocate(U161)
       deallocate(S72)
C
       allocate(F1(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef456132(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N1,N3,N2,N3,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef456123(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,t3C,F2)
       allocate(S73(N2+1:N3,N1+1:N3,N1+1:N3,N2+1:N3))
       I1=K4*K3
       I2=K3*K4
       I3=K4*K1*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S73)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef1243(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N1,N3,N0,N2,IntM,D1)
       allocate(U162(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K3*K3*K4
       call jungemm(I1,I2,I3,D1,S73,U162)
       deallocate(D1)
C
       V1B=V1B-1.0d0/2*U162
       deallocate(U162)
       deallocate(S73)
C
       allocate(D1(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N2,N3,N0,N2,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S74(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4*K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S74)
       deallocate(D1)
       deallocate(B2)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef451632(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N0,N1,N1,N3,N2,N3,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(S75(N2+1:N3,N0+1:N1,N1+1:N3,N2+1:N3))
       I1=K4*K3*K1
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,F1,D2,S75)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef2314(N1,N3,N0,N1,N2,N3,N0,N2,
     & N0,N1,N2,N3,N1,N3,N0,N2,S74,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef2134(N2,N3,N0,N1,N1,N3,N2,N3,
     & N0,N1,N2,N3,N1,N3,N2,N3,S75,D2)
       allocate(U165(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,D1,D2,U165)
       deallocate(D1)
       deallocate(D2)
C
       V1B=V1B+1.0d0/2*U165
       deallocate(U165)
       deallocate(S75)
C
       allocate(D1(N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef2143(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N2,N3,N1,N3,N0,N2,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S76(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K4
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S76)
       deallocate(D1)
       deallocate(B2)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef451632(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N0,N1,N1,N3,N2,N3,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(S77(N2+1:N3,N0+1:N1,N1+1:N3,N2+1:N3))
       I1=K4*K3*K1
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,F1,D2,S77)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef2134(N2,N3,N0,N1,N1,N3,N2,N3,
     & N0,N1,N2,N3,N1,N3,N2,N3,S77,D2)
       allocate(U166(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,S76,D2,U166)
       deallocate(D2)
C
       V1B=V1B-1.0d0/2*U166
       deallocate(U166)
       deallocate(S77)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef461352(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N0,N2,N2,N3,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef461352(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N0,N2,N2,N3,t3C,F2)
       allocate(S88(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K4*K2
       I3=K3*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S88)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3124(N0,N2,N2,N3,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,S88,D2)
       allocate(U183(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,D2,U183)
       deallocate(D1)
       deallocate(D2)
C
       V1B=V1B+U183
       deallocate(U183)
       deallocate(S88)
C
       allocate(F1(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3))
       call relef456312(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N1,N3,N2,N3,N2,N3,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3))
       call relef456312(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N1,N3,N2,N3,N2,N3,t3C,F2)
       allocate(S89(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K4*K4
       I3=K3*K1*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S89)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef3124(N2,N3,N2,N3,N2,N3,N2,N3,
     & N2,N3,N2,N3,N2,N3,N2,N3,S89,D2)
       allocate(U184(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K4*K4*K4
       call jungemm(I1,I2,I3,D1,D2,U184)
       deallocate(D1)
       deallocate(D2)
C
       V1B=V1B-1.0d0/4*U184
       deallocate(U184)
       deallocate(S89)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S94(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S94)
       deallocate(D1)
       deallocate(B2)
C
       allocate(F1(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3))
       call relef456312(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N1,N3,N2,N3,N2,N3,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3))
       call relef456312(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N1,N3,N2,N3,N2,N3,t3C,F2)
       allocate(S95(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K4*K4
       I3=K3*K1*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S95)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D2(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef3124(N2,N3,N2,N3,N2,N3,N2,N3,
     & N2,N3,N2,N3,N2,N3,N2,N3,S95,D2)
       allocate(U189(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K4*K4*K4
       call jungemm(I1,I2,I3,S94,D2,U189)
       deallocate(D2)
C
       V1B=V1B+1.0d0/4*U189
       deallocate(U189)
       deallocate(S95)
C
       allocate(F1(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef456123(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,l3D,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef456123(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,t3D,F2)
       allocate(S164(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K4*K4
       I3=K4*K2*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S164)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D2(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef3124(N2,N3,N2,N3,N2,N3,N2,N3,
     & N2,N3,N2,N3,N2,N3,N2,N3,S164,D2)
       allocate(U278(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K4*K4*K4
       call jungemm(I1,I2,I3,S94,D2,U278)
       deallocate(D2)
C
       V1B=V1B+1.0d0/12*U278
       deallocate(U278)
       deallocate(S164)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N2,N3,t2C,D2)
       allocate(S96(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K4*K2
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S96)
       deallocate(D1)
       deallocate(D2)
C
       allocate(F1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef563412(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N0,N2,N2,N3,N2,N3,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,t2B,D2)
       allocate(S97(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4*K2
       I2=K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,F1,D2,S97)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef2314(N2,N3,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,S97,D2)
       allocate(U191(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,S96,D2,U191)
       deallocate(D2)
C
       V1B=V1B-U191
       deallocate(U191)
       deallocate(S97)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef461235(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef461235(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,t3C,F2)
       allocate(Q196(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K3*K4*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,Q196)
       deallocate(F1)
       deallocate(F2)
C
       allocate(B1(N0+1:N2,N2+1:N3))
       call relef21(0,N3,0,N3,
     & N0,N2,N2,N3,FockB,B1)
       allocate(U197(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K2
       call jungemm(I1,I2,I3,B1,Q196,U197)
       deallocate(B1)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U197,-1.0d0/2)
       deallocate(U197)
       deallocate(Q196)
C
       allocate(F1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef413625(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,N2,N3,N0,N2,l3C,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(S101(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4*K1
       I2=K1
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S101)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N2+1:N3,N2+1:N3))
       call relef2431(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N2,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef2134(N0,N1,N0,N1,N2,N3,N0,N2,
     & N0,N1,N0,N1,N2,N3,N0,N2,S101,D2)
       allocate(U198(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4*K1*K1
       call jungemm(I1,I2,I3,D1,D2,U198)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U198,-1.0d0)
       deallocate(U198)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N1,N0+1:N2))
       call relef2314(N0,N1,N0,N1,N2,N3,N0,N2,
     & N0,N1,N2,N3,N0,N1,N0,N2,S101,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N0+1:N2,N1+1:N3))
       call relef4132(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N0,N2,N1,N3,t2B,D2)
       allocate(S126(N0+1:N2,N1+1:N3,N0+1:N1,N0+1:N2))
       I1=K2*K1
       I2=K3*K2
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,D2,S126)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef1324(N0,N2,N1,N3,N0,N1,N0,N2,
     & N0,N2,N0,N1,N1,N3,N0,N2,S126,D2)
       allocate(U217(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U217)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U217,1.0d0)
       deallocate(U217)
       deallocate(S126)
C
       allocate(D1(N2+1:N3,N0+1:N1,N0+1:N1,N0+1:N2))
       call relef3214(N0,N1,N0,N1,N2,N3,N0,N2,
     & N2,N3,N0,N1,N0,N1,N0,N2,S101,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S119(N0+1:N2,N0+1:N1,N0+1:N1,N0+1:N2))
       I1=K2*K1*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S119)
       deallocate(D1)
       deallocate(B2)
       deallocate(S101)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3))
       call relef3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N0,N1,N2,N3,IntM,D1)
       allocate(U212(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K1*K1*K2
       call jungemm(I1,I2,I3,D1,S119,U212)
       deallocate(D1)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U212,1.0d0)
       deallocate(U212)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:N1,N0+1:N2))
       call relef2134(N0,N2,N0,N1,N0,N1,N0,N2,
     & N0,N1,N0,N2,N0,N1,N0,N2,S119,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S131(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
       I1=K2*K1*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S131)
       deallocate(D1)
       deallocate(B2)
       deallocate(S119)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef2314(N1,N3,N0,N2,N0,N1,N0,N2,
     & N0,N2,N0,N1,N1,N3,N0,N2,S131,D2)
       allocate(U220(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U220)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U220,1.0d0)
       deallocate(U220)
       deallocate(S131)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef461235(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2B,D2)
       allocate(S102(N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K4
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,F1,D2,S102)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N1+1:N3,N1+1:N3,N2+1:N3))
       call relef3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N1,N3,N2,N3,IntM,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef2134(N1,N3,N2,N3,N1,N3,N0,N2,
     & N2,N3,N1,N3,N1,N3,N0,N2,S102,D2)
       allocate(U199(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K3*K3*K4
       call jungemm(I1,I2,I3,D1,D2,U199)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U199,1.0d0)
       deallocate(U199)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef2431(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N0,N2,N2,N3,IntM,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef1324(N1,N3,N2,N3,N1,N3,N0,N2,
     & N1,N3,N1,N3,N2,N3,N0,N2,S102,D2)
       allocate(S120(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K4
       I3=K3*K3
       call jungemm(I1,I2,I3,D1,D2,S120)
       deallocate(D1)
       deallocate(D2)
C
       call sul2413(N0,N2,N2,N3,N2,N3,N0,N2,X10,S120,2.000d0)
       deallocate(S120)
C
       allocate(D1(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef2314(N1,N3,N2,N3,N1,N3,N0,N2,
     & N2,N3,N1,N3,N1,N3,N0,N2,S102,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S132(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K3
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S132)
       deallocate(D1)
       deallocate(B2)
       deallocate(S102)
C
       allocate(D1(N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N2))
       call relef2134(N0,N2,N1,N3,N1,N3,N0,N2,
     & N1,N3,N0,N2,N1,N3,N0,N2,S132,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S133(N0+1:N1,N0+1:N2,N1+1:N3,N0+1:N2))
       I1=K2*K3*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S133)
       deallocate(D1)
       deallocate(B2)
       deallocate(S132)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef2134(N0,N1,N0,N2,N1,N3,N0,N2,
     & N0,N2,N0,N1,N1,N3,N0,N2,S133,D2)
       allocate(U221(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U221)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U221,1.0d0)
       deallocate(U221)
       deallocate(S133)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef412635(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N0,N1,N1,N3,N0,N2,l3C,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S103(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3*K1
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S103)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(U200(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,S103,U200)
       deallocate(D1)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U200,-1.0d0/2)
       deallocate(U200)
       deallocate(S103)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef462315(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N2,N3,N0,N2,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef451362(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N0,N1,N1,N3,t3B,F2)
       allocate(S104(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K3*K1
       I3=K3*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S104)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef1324(N0,N1,N1,N3,N2,N3,N0,N2,
     & N0,N1,N2,N3,N1,N3,N0,N2,S104,D2)
       allocate(U201(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,D1,D2,U201)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U201,1.0d0)
       deallocate(U201)
       deallocate(S104)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N2))
       call relef412365(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,N0,N2,l3C,F1)
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef412356(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N1,N3,N0,N2,N0,N1,t3C,F2)
       allocate(S105(N0+1:N2,N0+1:N1,N0+1:N1,N0+1:N2))
       I1=K2*K1
       I2=K1*K2
       I3=K3*K4*K4*K2
       call jungemm(I1,I2,I3,F1,F2,S105)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3))
       call relef3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N0,N1,N2,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N0+1:N1,N0+1:N2))
       call relef1324(N0,N2,N0,N1,N0,N1,N0,N2,
     & N0,N2,N0,N1,N0,N1,N0,N2,S105,D2)
       allocate(U202(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K1*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U202)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U202,1.0d0/2)
       deallocate(U202)
       deallocate(S105)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef461235(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N2,N1+1:N3))
       call relef461253(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N0,N2,N1,N3,t3C,F2)
       allocate(S106(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3
       I2=K3*K2
       I3=K4*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S106)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:N3,N2+1:N3))
       call relef3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N1,N3,N2,N3,IntM,D1)
       allocate(U203(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K3*K3*K2
       call jungemm(I1,I2,I3,D1,S106,U203)
       deallocate(D1)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U203,-1.0d0/2)
       deallocate(U203)
       deallocate(S106)
C
       allocate(F1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef423615(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,N2,N3,N0,N2,l3C,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(S107(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4*K1
       I2=K1
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S107)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N0+1:N1,N0+1:N2))
       call relef1324(N0,N1,N0,N1,N2,N3,N0,N2,
     & N0,N1,N2,N3,N0,N1,N0,N2,S107,D2)
       allocate(S108(N0+1:N1,N0+1:N2,N1+1:N3,N2+1:N3))
       I1=K4*K3
       I2=K2*K1
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,D2,S108)
       deallocate(D1)
       deallocate(D2)
       deallocate(S107)
C
       call sul1423(N0,N1,N1,N3,N2,N3,N0,N2,X8,S108,2.000d0)
       deallocate(S108)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef462135(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2B,D2)
       allocate(S109(N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K4
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,F1,D2,S109)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N1,N2+1:N3))
       call relef3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N0,N1,N2,N3,IntM,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef2134(N1,N3,N2,N3,N1,N3,N0,N2,
     & N2,N3,N1,N3,N1,N3,N0,N2,S109,D2)
       allocate(S110(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4*K1
       I2=K2*K3
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,D2,S110)
       deallocate(D1)
       deallocate(D2)
C
       call sul2413(N0,N1,N1,N3,N2,N3,N0,N2,X8,S110,2.000d0)
       deallocate(S110)
C
       allocate(D1(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef2314(N1,N3,N2,N3,N1,N3,N0,N2,
     & N2,N3,N1,N3,N1,N3,N0,N2,S109,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,t2B,D2)
       allocate(S125(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3
       I2=K1*K2
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,D2,S125)
       deallocate(D1)
       deallocate(D2)
       deallocate(S109)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(U216(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,S125,U216)
       deallocate(D1)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U216,-1.0d0)
       deallocate(U216)
       deallocate(S125)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef412635(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N0,N1,N1,N3,N0,N2,l3C,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S111(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3*K1
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S111)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
       call relef3124(N0,N2,N0,N1,N1,N3,N0,N2,
     & N1,N3,N0,N2,N0,N1,N0,N2,S111,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S112(N0+1:N1,N0+1:N2,N0+1:N1,N0+1:N2))
       I1=K2*K1*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S112)
       deallocate(D1)
       deallocate(B2)
       deallocate(S111)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3))
       call relef3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N0,N1,N2,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N0+1:N1,N0+1:N2))
       call relef2314(N0,N1,N0,N2,N0,N1,N0,N2,
     & N0,N2,N0,N1,N0,N1,N0,N2,S112,D2)
       allocate(U206(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K1*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U206)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U206,1.0d0/2)
       deallocate(U206)
       deallocate(S112)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef412635(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N0,N1,N1,N3,N0,N2,l3C,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S113(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3*K1
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S113)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:N3,N2+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N1,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N0+1:N1,N0+1:N2))
       call relef1324(N0,N2,N0,N1,N1,N3,N0,N2,
     & N0,N2,N1,N3,N0,N1,N0,N2,S113,D2)
       allocate(S114(N0+1:N1,N0+1:N2,N1+1:N3,N2+1:N3))
       I1=K4*K3
       I2=K2*K1
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,D2,S114)
       deallocate(D1)
       deallocate(D2)
       deallocate(S113)
C
       call sul1423(N0,N1,N1,N3,N2,N3,N0,N2,X8,S114,-1.000d0)
       deallocate(S114)
C
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(U178(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K3*K1
       call jungemm1(I1,I3,X8,B2,U178)
       deallocate(B2)
C
       V1B=V1B+1.0d0/2*U178
       deallocate(U178)
       deallocate(X8)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N2))
       call relef412365(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,N0,N2,l3C,F1)
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef412356(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N1,N3,N0,N2,N0,N1,t3C,F2)
       allocate(S115(N0+1:N2,N0+1:N1,N0+1:N1,N0+1:N2))
       I1=K2*K1
       I2=K1*K2
       I3=K3*K4*K4*K2
       call jungemm(I1,I2,I3,F1,F2,S115)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:N1,N0+1:N2))
       call relef3124(N0,N2,N0,N1,N0,N1,N0,N2,
     & N0,N1,N0,N2,N0,N1,N0,N2,S115,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S116(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
       I1=K2*K1*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S116)
       deallocate(D1)
       deallocate(B2)
       deallocate(S115)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef2314(N1,N3,N0,N2,N0,N1,N0,N2,
     & N0,N2,N0,N1,N1,N3,N0,N2,S116,D2)
       allocate(U208(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U208)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U208,1.0d0/2)
       deallocate(U208)
       deallocate(S116)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef461235(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N2,N1+1:N3))
       call relef461253(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N0,N2,N1,N3,t3C,F2)
       allocate(S117(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3
       I2=K3*K2
       I3=K4*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S117)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N2))
       call relef3124(N0,N2,N1,N3,N1,N3,N0,N2,
     & N1,N3,N0,N2,N1,N3,N0,N2,S117,D1)
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
       deallocate(S117)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef2134(N0,N1,N0,N2,N1,N3,N0,N2,
     & N0,N2,N0,N1,N1,N3,N0,N2,S118,D2)
       allocate(U209(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U209)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U209,1.0d0/2)
       deallocate(U209)
       deallocate(S118)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef461325(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N2,N3,N0,N2,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N0,N1,N1,N3,t3B,F2)
       allocate(S121(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K3*K1
       I3=K3*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S121)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N2+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef3124(N0,N1,N1,N3,N2,N3,N0,N2,
     & N2,N3,N0,N1,N1,N3,N0,N2,S121,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S122(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S122)
       deallocate(D1)
       deallocate(B2)
       deallocate(S121)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(U214(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,S122,U214)
       deallocate(D1)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U214,-1.0d0)
       deallocate(U214)
       deallocate(S122)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef412635(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N0,N1,N1,N3,N0,N2,l3C,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S123(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3*K1
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S123)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call relef2314(N0,N2,N0,N1,N1,N3,N0,N2,
     & N0,N1,N1,N3,N0,N2,N0,N2,S123,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call relef3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N1,N3,t2A,D2)
       allocate(S124(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K3*K1
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S124)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef3124(N0,N1,N1,N3,N0,N2,N0,N2,
     & N0,N2,N0,N1,N1,N3,N0,N2,S124,D2)
       allocate(U215(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U215)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U215,-1.0d0/2)
       deallocate(U215)
       deallocate(S124)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call relef2314(N0,N2,N0,N1,N1,N3,N0,N2,
     & N0,N1,N1,N3,N0,N2,N0,N2,S123,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,N3,t2B,D2)
       allocate(S147(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K4*K2
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,D2,S147)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef3124(N0,N2,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N0,N2,S147,D2)
       allocate(U234(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,D2,U234)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U234,1.0d0/2)
       deallocate(U234)
       deallocate(S147)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
       call relef3124(N0,N2,N0,N1,N1,N3,N0,N2,
     & N1,N3,N0,N2,N0,N1,N0,N2,S123,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S129(N0+1:N1,N0+1:N2,N0+1:N1,N0+1:N2))
       I1=K2*K1*K2
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,D1,B2,S129)
       deallocate(D1)
       deallocate(B2)
       deallocate(S123)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:N1,N0+1:N2))
       call relef3214(N0,N1,N0,N2,N0,N1,N0,N2,
     & N0,N1,N0,N2,N0,N1,N0,N2,S129,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S130(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
       I1=K2*K1*K2
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,D1,B2,S130)
       deallocate(D1)
       deallocate(B2)
       deallocate(S129)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef2314(N1,N3,N0,N2,N0,N1,N0,N2,
     & N0,N2,N0,N1,N1,N3,N0,N2,S130,D2)
       allocate(U219(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U219)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U219,1.0d0/2)
       deallocate(U219)
       deallocate(S130)
C
       allocate(F1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef613425(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,N2,N3,N0,N2,l3C,F1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(S127(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,F1,D2,S127)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef2134(N0,N2,N0,N2,N2,N3,N0,N2,
     & N0,N2,N0,N2,N2,N3,N0,N2,S127,D2)
       allocate(U222(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,D2,U222)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U222,-1.0d0)
       deallocate(U222)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1324(N0,N2,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N0,N2,N0,N2,S127,D2)
       allocate(S138(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K2*K2
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S138)
       deallocate(D1)
       deallocate(D2)
C
       call sul1423(N0,N2,N2,N3,N2,N3,N0,N2,X10,S138,2.000d0)
       deallocate(S138)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef2314(N0,N2,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N0,N2,N0,N2,S127,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N1,N3,t2B,D2)
       allocate(S128(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K3*K1
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S128)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef3124(N0,N1,N1,N3,N0,N2,N0,N2,
     & N0,N2,N0,N1,N1,N3,N0,N2,S128,D2)
       allocate(U218(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U218)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U218,1.0d0)
       deallocate(U218)
       deallocate(S128)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef3214(N0,N2,N0,N2,N2,N3,N0,N2,
     & N2,N3,N0,N2,N0,N2,N0,N2,S127,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S137(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S137)
       deallocate(D1)
       deallocate(B2)
       deallocate(S127)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N0,N2,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef2314(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S137,D2)
       allocate(U226(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K2*K2*K2
       call jungemm(I1,I2,I3,D1,D2,U226)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U226,1.0d0)
       deallocate(U226)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef2314(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S137,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S148(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S148)
       deallocate(D1)
       deallocate(B2)
       deallocate(S137)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef2314(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N0,N2,S148,D2)
       allocate(U235(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,D2,U235)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U235,1.0d0)
       deallocate(U235)
       deallocate(S148)
C
       allocate(F1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef463125(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,N2,N3,N0,N2,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,t2B,D2)
       allocate(S134(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,F1,D2,S134)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef2134(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N2,N3,N2,N3,N0,N2,S134,D2)
       allocate(U223(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4*K4*K4
       call jungemm(I1,I2,I3,D1,D2,U223)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U223,1.0d0/2)
       deallocate(U223)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef4132(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N2,N3,N2,N3,N0,N2,S134,D2)
       allocate(S139(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K4
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,D2,S139)
       deallocate(D1)
       deallocate(D2)
C
       call sul2413(N0,N2,N2,N3,N2,N3,N0,N2,X10,S139,2.000d0)
       deallocate(S139)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef2314(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N2,N3,N2,N3,N0,N2,S134,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2C,D2)
       allocate(S144(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K2
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,D2,S144)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(U232(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,S144,U232)
       deallocate(D1)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U232,1.0d0/4)
       deallocate(U232)
       deallocate(S144)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef2314(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N2,N3,N2,N3,N0,N2,S134,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S149(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S149)
       deallocate(D1)
       deallocate(B2)
       deallocate(S134)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef2134(N0,N2,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,N3,N0,N2,S149,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S150(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S150)
       deallocate(D1)
       deallocate(B2)
       deallocate(S149)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef2134(N0,N2,N0,N2,N2,N3,N0,N2,
     & N0,N2,N0,N2,N2,N3,N0,N2,S150,D2)
       allocate(U236(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,D2,U236)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U236,1.0d0/2)
       deallocate(U236)
       deallocate(S150)
C
       allocate(F1(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2))
       call relef612345(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,l3C,F1)
       allocate(F2(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2))
       call relef612345(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,t3C,F2)
       allocate(S135(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K2
       I3=K3*K4*K4*K1
       call jungemm(I1,I2,I3,F1,F2,S135)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N0,N2,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef3124(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S135,D2)
       allocate(U224(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K2*K2*K2
       call jungemm(I1,I2,I3,D1,D2,U224)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U224,1.0d0/4)
       deallocate(U224)
       deallocate(S135)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef461325(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N2,N3,N0,N2,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef461352(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N0,N2,N2,N3,t3C,F2)
       allocate(S136(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K4*K2
       I3=K3*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S136)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef1324(N0,N2,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,S136,D2)
       allocate(U225(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,D1,D2,U225)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U225,1.0d0)
       deallocate(U225)
       deallocate(S136)
C
       allocate(F1(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2))
       call relef612345(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,l3C,F1)
       allocate(F2(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2))
       call relef612345(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,t3C,F2)
       allocate(S140(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K2
       I3=K3*K4*K4*K1
       call jungemm(I1,I2,I3,F1,F2,S140)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef3124(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S140,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S141(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S141)
       deallocate(D1)
       deallocate(B2)
       deallocate(S140)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef2314(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N0,N2,S141,D2)
       allocate(U229(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,D2,U229)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U229,1.0d0/4)
       deallocate(U229)
       deallocate(S141)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef461325(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N2,N3,N0,N2,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2,N2+1:N3))
       call relef461352(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,N0,N2,N2,N3,t3C,F2)
       allocate(S142(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K4*K2
       I3=K3*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,S142)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef3124(N0,N2,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,N3,N0,N2,S142,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S143(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S143)
       deallocate(D1)
       deallocate(B2)
       deallocate(S142)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef2134(N0,N2,N0,N2,N2,N3,N0,N2,
     & N0,N2,N0,N2,N2,N3,N0,N2,S143,D2)
       allocate(U230(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,D2,U230)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U230,-1.0d0)
       deallocate(U230)
       deallocate(S143)
C
       allocate(F1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef623415(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,N2,N3,N0,N2,l3C,F1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(S145(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,F1,D2,S145)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef2314(N0,N2,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N0,N2,N0,N2,S145,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N2,N3,t2C,D2)
       allocate(S146(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K4*K2
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S146)
       deallocate(D1)
       deallocate(D2)
       deallocate(S145)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef1324(N0,N2,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N0,N2,S146,D2)
       allocate(U233(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,D2,U233)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U233,-1.0d0)
       deallocate(U233)
       deallocate(S146)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,t3C,F2)
       allocate(Q237(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,Q237)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       call relef2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N2,N3,N0,N2,IntM,D1)
       allocate(B2(N0+1:N1,N0+1:N1))
       call relef21(N0,N1,N0,N1,
     & N0,N1,N0,N1,Q237,B2)
       allocate(U238(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K1*K1
       call jungemm1(I1,I3,D1,B2,U238)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B-1.0d0/4*U238
       deallocate(U238)
       deallocate(Q237)
C
       allocate(F1(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef456123(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef456123(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,t3C,F2)
       allocate(Q239(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K4*K4*K1*K2*K2
       call jungemm(I1,I2,I3,F1,F2,Q239)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(U240(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K3*K3
       call jungemm1(I1,I3,D1,Q239,U240)
       deallocate(D1)
C
       V1B=V1B+1.0d0/4*U240
       deallocate(U240)
       deallocate(Q239)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,t3C,F2)
       allocate(Q241(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,Q241)
       deallocate(F1)
       deallocate(F2)
C
       allocate(B1(N0+1:N1,N0+1:N1))
       call relef21(N0,N1,N0,N1,
     & N0,N1,N0,N1,Q241,B1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q242(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K1
       call jungemm(I1,I2,I3,B1,B2,Q242)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q241)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,Q242,B2)
       allocate(U243(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,U243)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B-1.0d0/4*U243
       deallocate(U243)
       deallocate(Q242)
C
       allocate(F1(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef456123(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call relef456123(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,t3C,F2)
       allocate(Q244(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K4*K4*K1*K2*K2
       call jungemm(I1,I2,I3,F1,F2,Q244)
       deallocate(F1)
       deallocate(F2)
C
       allocate(B1(N1+1:N3,N1+1:N3))
       call relef21(N1,N3,N1,N3,
     & N1,N3,N1,N3,Q244,B1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call relef12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(Q245(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3
       call jungemm(I1,I2,I3,B1,B2,Q245)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q244)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(U246(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K3*K1
       call jungemm1(I1,I3,D1,Q245,U246)
       deallocate(D1)
C
       V1B=V1B-1.0d0/4*U246
       deallocate(U246)
       deallocate(Q245)
C
       allocate(F1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       call relef413562(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N2,N0,N1,N2,N3,l3C,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(S151(N0+1:N1,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4*K1*K2
       I2=K1
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S151)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call relef2341(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N0,N1,N2,N3,N0,N1,S151,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2B,D2)
       allocate(Q247(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,D1,D2,Q247)
       deallocate(D1)
       deallocate(D2)
       deallocate(S151)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,Q247,B2)
       allocate(U248(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,U248)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B-U248
       deallocate(U248)
       deallocate(Q247)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef461235(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef461235(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,t3C,F2)
       allocate(Q249(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K3*K4*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,Q249)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N0+1:N2))
       call relef21(N0,N2,N0,N2,
     & N0,N2,N0,N2,Q249,B2)
       allocate(U250(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K2*K2
       call jungemm1(I1,I3,D1,B2,U250)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B-1.0d0/2*U250
       deallocate(U250)
       deallocate(Q249)
C
       allocate(F1(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef456132(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N1,N3,N2,N3,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef456132(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N1,N3,N2,N3,t3C,F2)
       allocate(Q251(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K3*K4*K1*K2*K2
       call jungemm(I1,I2,I3,F1,F2,Q251)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N2+1:N3,N2+1:N3))
       call relef21(N2,N3,N2,N3,
     & N2,N3,N2,N3,Q251,B2)
       allocate(U252(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K4*K4
       call jungemm1(I1,I3,D1,B2,U252)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B+1.0d0/2*U252
       deallocate(U252)
       deallocate(Q251)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef461235(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef461235(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,t3C,F2)
       allocate(Q253(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K3*K4*K4*K1*K2
       call jungemm(I1,I2,I3,F1,F2,Q253)
       deallocate(F1)
       deallocate(F2)
C
       allocate(B1(N0+1:N2,N0+1:N2))
       call relef21(N0,N2,N0,N2,
     & N0,N2,N0,N2,Q253,B1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q254(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,B1,B2,Q254)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q253)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,Q254,B2)
       allocate(U255(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,U255)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B-1.0d0/2*U255
       deallocate(U255)
       deallocate(Q254)
C
       allocate(F1(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef456132(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N1,N3,N2,N3,l3C,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef456132(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N0,N1,N2,N3,N1,N3,N2,N3,t3C,F2)
       allocate(Q256(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K3*K4*K1*K2*K2
       call jungemm(I1,I2,I3,F1,F2,Q256)
       deallocate(F1)
       deallocate(F2)
C
       allocate(B1(N2+1:N3,N2+1:N3))
       call relef21(N2,N3,N2,N3,
     & N2,N3,N2,N3,Q256,B1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(Q257(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,B1,B2,Q257)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q256)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U258(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K4*K2
       call jungemm1(I1,I3,D1,Q257,U258)
       deallocate(D1)
C
       V1B=V1B-1.0d0/2*U258
       deallocate(U258)
       deallocate(Q257)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       call relef412563(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N0,N2,N0,N1,N1,N3,l3C,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S152(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1*K2
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S152)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N2,N0,N1,N1,N3,
     & N0,N2,N0,N1,N1,N3,N0,N2,S152,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,t2B,D2)
       allocate(Q259(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,D2,Q259)
       deallocate(D1)
       deallocate(D2)
       deallocate(S152)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,Q259,B2)
       allocate(U260(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,U260)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B-1.0d0/2*U260
       deallocate(U260)
       deallocate(Q259)
C
       allocate(F1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       call relef623451(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,N0,N2,N2,N3,l3C,F1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(S153(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2*K2
       I2=K2
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,F1,D2,S153)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,S153,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(Q261(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,D2,Q261)
       deallocate(D1)
       deallocate(D2)
       deallocate(S153)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,Q261,B2)
       allocate(U262(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,U262)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B-1.0d0/2*U262
       deallocate(U262)
       deallocate(Q261)
C
       allocate(F1(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef456123(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,l3D,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef456123(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,t3D,F2)
       allocate(Q263(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K4*K4*K2*K2*K2
       call jungemm(I1,I2,I3,F1,F2,Q263)
       deallocate(F1)
       deallocate(F2)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call relef12(0,N3,0,N3,
     & N2,N3,N0,N2,FockB,B1)
       allocate(U264(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K4
       call jungemm(I1,I2,I3,B1,Q263,U264)
       deallocate(B1)
C
       V1B=V1B-1.0d0/12*U264
       deallocate(U264)
       deallocate(Q263)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef451263(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N0,N2,N2,N3,l3D,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N0,N1,N1,N3,t3C,F2)
       allocate(S154(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K3*K1
       I3=K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S154)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef1423(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N0,N2,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3124(N0,N1,N1,N3,N0,N2,N2,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,S154,D2)
       allocate(U265(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U265)
       deallocate(D1)
       deallocate(D2)
C
       V1B=V1B-1.0d0/4*U265
       deallocate(U265)
       deallocate(S154)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef451263(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N0,N2,N2,N3,l3D,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef451263(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N0,N2,N2,N3,t3D,F2)
       allocate(S158(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K4*K2
       I3=K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S158)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3124(N0,N2,N2,N3,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,S158,D2)
       allocate(U272(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,D2,U272)
       deallocate(D1)
       deallocate(D2)
C
       V1B=V1B+1.0d0/4*U272
       deallocate(U272)
       deallocate(S158)
C
       allocate(F1(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef456123(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,l3D,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef456123(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,t3D,F2)
       allocate(S159(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K4*K4
       I3=K4*K2*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S159)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U273(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K4*K4*K4
       call jungemm(I1,I2,I3,D1,S159,U273)
       deallocate(D1)
C
       V1B=V1B-1.0d0/12*U273
       deallocate(U273)
       deallocate(S159)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,l3D,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,t3D,F2)
       allocate(Q284(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4*K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,Q284)
       deallocate(F1)
       deallocate(F2)
C
       allocate(B1(N0+1:N2,N2+1:N3))
       call relef21(0,N3,0,N3,
     & N0,N2,N2,N3,FockB,B1)
       allocate(U285(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K2
       call jungemm(I1,I2,I3,B1,Q284,U285)
       deallocate(B1)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U285,-1.0d0/12)
       deallocate(U285)
       deallocate(Q284)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,l3D,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N0,N1,N1,N3,t3C,F2)
       allocate(S167(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K3*K1
       I3=K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S167)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef4321(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef1324(N0,N1,N1,N3,N2,N3,N0,N2,
     & N0,N1,N2,N3,N1,N3,N0,N2,S167,D2)
       allocate(U286(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,D1,D2,U286)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U286,1.0d0/4)
       deallocate(U286)
       deallocate(S167)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,l3D,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef451263(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N0,N1,N1,N3,t3C,F2)
       allocate(S168(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K3*K1
       I3=K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S168)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N2+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef3124(N0,N1,N1,N3,N2,N3,N0,N2,
     & N2,N3,N0,N1,N1,N3,N0,N2,S168,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S169(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3*K1
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S169)
       deallocate(D1)
       deallocate(B2)
       deallocate(S168)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(U289(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,S169,U289)
       deallocate(D1)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U289,-1.0d0/4)
       deallocate(U289)
       deallocate(S169)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef412536(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,N2,N3,N0,N2,l3D,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S170(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S170)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef2134(N0,N2,N0,N2,N2,N3,N0,N2,
     & N0,N2,N0,N2,N2,N3,N0,N2,S170,D2)
       allocate(U291(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,D2,U291)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U291,-1.0d0/2)
       deallocate(U291)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1324(N0,N2,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N0,N2,N0,N2,S170,D2)
       allocate(S176(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K2*K2
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S176)
       deallocate(D1)
       deallocate(D2)
C
       call sul1423(N0,N2,N2,N3,N2,N3,N0,N2,X10,S176,1.000d0)
       deallocate(S176)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef2314(N0,N2,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N0,N2,N0,N2,S170,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call relef3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N1,N3,t2B,D2)
       allocate(S171(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K3*K1
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S171)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef3124(N0,N1,N1,N3,N0,N2,N0,N2,
     & N0,N2,N0,N1,N1,N3,N0,N2,S171,D2)
       allocate(U290(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U290)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U290,1.0d0/2)
       deallocate(U290)
       deallocate(S171)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef2314(N0,N2,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N0,N2,N0,N2,S170,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N2,N3,t2C,D2)
       allocate(S185(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K4*K2
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,D2,S185)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef3124(N0,N2,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N0,N2,S185,D2)
       allocate(U302(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,D2,U302)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U302,-1.0d0/2)
       deallocate(U302)
       deallocate(S185)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef3214(N0,N2,N0,N2,N2,N3,N0,N2,
     & N2,N3,N0,N2,N0,N2,N0,N2,S170,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S175(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S175)
       deallocate(D1)
       deallocate(B2)
       deallocate(S170)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N0,N2,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef2314(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S175,D2)
       allocate(U295(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K2*K2*K2
       call jungemm(I1,I2,I3,D1,D2,U295)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U295,1.0d0/2)
       deallocate(U295)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef2314(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S175,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S186(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S186)
       deallocate(D1)
       deallocate(B2)
       deallocate(S175)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef2314(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N0,N2,S186,D2)
       allocate(U303(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,D2,U303)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U303,1.0d0/2)
       deallocate(U303)
       deallocate(S186)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,l3D,F1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(S172(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,F1,D2,S172)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N2,N3,N2,N3,IntB,D1)
       allocate(U292(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4*K4*K4
       call jungemm(I1,I2,I3,D1,S172,U292)
       deallocate(D1)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U292,1.0d0/4)
       deallocate(U292)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef2314(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N2,N3,N2,N3,N0,N2,S172,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S187(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S187)
       deallocate(D1)
       deallocate(B2)
       deallocate(S172)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef2134(N0,N2,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,N3,N0,N2,S187,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S188(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S188)
       deallocate(D1)
       deallocate(B2)
       deallocate(S187)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef2134(N0,N2,N0,N2,N2,N3,N0,N2,
     & N0,N2,N0,N2,N2,N3,N0,N2,S188,D2)
       allocate(U304(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,D2,U304)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U304,1.0d0/4)
       deallocate(U304)
       deallocate(S188)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef412356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,l3D,F1)
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef412356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,t3D,F2)
       allocate(S173(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K2
       I3=K4*K4*K4*K2
       call jungemm(I1,I2,I3,F1,F2,S173)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N0,N2,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef3124(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S173,D2)
       allocate(U293(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K2*K2*K2
       call jungemm(I1,I2,I3,D1,D2,U293)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U293,1.0d0/12)
       deallocate(U293)
       deallocate(S173)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,l3D,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef451263(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N0,N2,N2,N3,t3D,F2)
       allocate(S174(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K4*K2
       I3=K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S174)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,IntB,D1)
       allocate(U294(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,D1,S174,U294)
       deallocate(D1)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U294,1.0d0/4)
       deallocate(U294)
       deallocate(S174)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,l3D,F1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(S177(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,F1,D2,S177)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef1432(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef1324(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N2,N3,N2,N3,N0,N2,S177,D2)
       allocate(S178(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K4
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,D2,S178)
       deallocate(D1)
       deallocate(D2)
       deallocate(S177)
C
       call sul2413(N0,N2,N2,N3,N2,N3,N0,N2,X10,S178,1.000d0)
       deallocate(S178)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(U87(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K4*K2
       call jungemm1(I1,I3,X10,B2,U87)
       deallocate(B2)
C
       V1B=V1B-1.0d0/2*U87
       deallocate(U87)
       deallocate(X10)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef412356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,l3D,F1)
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef412356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,t3D,F2)
       allocate(S179(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K2
       I3=K4*K4*K4*K2
       call jungemm(I1,I2,I3,F1,F2,S179)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef3124(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S179,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S180(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,D1,B2,S180)
       deallocate(D1)
       deallocate(B2)
       deallocate(S179)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef2314(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N0,N2,S180,D2)
       allocate(U298(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,D2,U298)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U298,1.0d0/12)
       deallocate(U298)
       deallocate(S180)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,l3D,F1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       call relef451263(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N0,N2,N2,N3,t3D,F2)
       allocate(S181(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K4*K2
       I3=K4*K4*K2*K2
       call jungemm(I1,I2,I3,F1,F2,S181)
       deallocate(F1)
       deallocate(F2)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef3124(N0,N2,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,N3,N0,N2,S181,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S182(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,D1,B2,S182)
       deallocate(D1)
       deallocate(B2)
       deallocate(S181)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef2134(N0,N2,N0,N2,N2,N3,N0,N2,
     & N0,N2,N0,N2,N2,N3,N0,N2,S182,D2)
       allocate(U299(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,D2,U299)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U299,-1.0d0/4)
       deallocate(U299)
       deallocate(S182)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef453126(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,l3D,F1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(S183(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,F1,D2,S183)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef2314(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N2,N3,N2,N3,N0,N2,S183,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2C,D2)
       allocate(S184(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K2
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,D2,S184)
       deallocate(D1)
       deallocate(D2)
       deallocate(S183)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(U301(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,S184,U301)
       deallocate(D1)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U301,1.0d0/8)
       deallocate(U301)
       deallocate(S184)
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
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N0+1:N2))
       call relef21(N0,N2,N0,N2,
     & N0,N2,N0,N2,Q305,B2)
       allocate(U306(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K2*K2
       call jungemm1(I1,I3,D1,B2,U306)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B-1.0d0/12*U306
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
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U308(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K4*K4
       call jungemm1(I1,I3,D1,Q307,U308)
       deallocate(D1)
C
       V1B=V1B+1.0d0/12*U308
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
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,Q310,B2)
       allocate(U311(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,U311)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B-1.0d0/12*U311
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
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U314(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K4*K2
       call jungemm1(I1,I3,D1,Q313,U314)
       deallocate(D1)
C
       V1B=V1B-1.0d0/12*U314
       deallocate(U314)
       deallocate(Q313)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       call relef412563(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,N0,N2,N2,N3,l3D,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S189(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2*K2
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S189)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef2341(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,S189,D1)
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
       deallocate(S189)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,Q315,B2)
       allocate(U316(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,U316)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B+1.0d0/4*U316
       deallocate(U316)
       deallocate(Q315)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,H2B,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call relef21(N1,N3,N0,N1,
     & N0,N1,N1,N3,l1A,B2)
       allocate(U1(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K3*K1
       call jungemm1(I1,I3,D1,B2,U1)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B+U1
       deallocate(U1)
C
       allocate(B1(N0+1:N2,N0+1:N2))
       call relef12(N0,N3,N0,N3,
     & N0,N2,N0,N2,H1B,B1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,l1B,B2)
       allocate(U2(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K2
       call jungemm(I1,I2,I3,B1,B2,U2)
       deallocate(B1)
       deallocate(B2)
C
       V1B=V1B-U2
       deallocate(U2)
C
       allocate(B1(N2+1:N3,N2+1:N3))
       call relef21(N0,N3,N0,N3,
     & N2,N3,N2,N3,H1B,B1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call relef12(N2,N3,N0,N2,
     & N2,N3,N0,N2,l1B,B2)
       allocate(U3(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4
       call jungemm(I1,I2,I3,B1,B2,U3)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U3,1.0d0)
       deallocate(U3)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,H2C,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call relef21(N2,N3,N0,N2,
     & N0,N2,N2,N3,l1B,B2)
       allocate(U4(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K4*K2
       call jungemm1(I1,I3,D1,B2,U4)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B+U4
       deallocate(U4)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call relef1243(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N0,N2,H2B,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,l2B,D2)
       allocate(U19(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,D1,D2,U19)
       deallocate(D1)
       deallocate(D2)
C
       V1B=V1B-U19
       deallocate(U19)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call relef2341(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N2,N3,H2B,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,l2B,D2)
       allocate(U20(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,D1,D2,U20)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U20,1.0d0)
       deallocate(U20)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,H2C,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,l2C,D2)
       allocate(U41(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,D1,D2,U41)
       deallocate(D1)
       deallocate(D2)
C
       V1B=V1B-1.0d0/2*U41
       deallocate(U41)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,H2C,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,l2C,D2)
       allocate(U42(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,D1,D2,U42)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul21(N2,N3,N0,N2,V1B,U42,1.0d0/2)
       deallocate(U42)
C
       end
