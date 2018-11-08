       subroutine L3C_update_cor(N0,N1,N2,N3,V3C,
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
       real*8 V3C(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1)
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
       real*8,allocatable::S1(:,:,:,:)
       real*8,allocatable::S2(:,:,:,:)
       real*8,allocatable::S3(:,:,:,:)
       real*8,allocatable::S4(:,:,:,:)
       real*8,allocatable::S5(:,:,:,:)
       real*8,allocatable::S6(:,:,:,:)
       real*8,allocatable::S7(:,:,:,:)
       real*8,allocatable::S8(:,:,:,:)
       real*8,allocatable::S9(:,:,:,:)
       real*8,allocatable::S10(:,:,:,:)
       real*8,allocatable::S11(:,:,:,:)
       real*8,allocatable::S12(:,:,:,:)
       real*8,allocatable::U1(:,:,:,:,:,:)
       real*8,allocatable::U2(:,:,:,:,:,:)
       real*8,allocatable::U3(:,:,:,:,:,:)
       real*8,allocatable::U4(:,:,:,:,:,:)
       real*8,allocatable::U5(:,:,:,:,:,:)
       real*8,allocatable::U6(:,:,:,:,:,:)
       real*8,allocatable::U7(:,:,:,:,:,:)
       real*8,allocatable::U8(:,:,:,:,:,:)
       real*8,allocatable::U9(:,:,:,:,:,:)
       real*8,allocatable::U10(:,:,:,:,:,:)
       real*8,allocatable::U11(:,:,:,:,:,:)
       real*8,allocatable::U12(:,:,:,:,:,:)
       real*8,allocatable::U13(:,:,:,:,:,:)
       real*8,allocatable::U14(:,:,:,:,:,:)
       real*8,allocatable::U15(:,:,:,:,:,:)
       real*8,allocatable::U16(:,:,:,:,:,:)
       real*8,allocatable::U17(:,:,:,:,:,:)
       real*8,allocatable::U18(:,:,:,:,:,:)
       real*8,allocatable::U19(:,:,:,:,:,:)
       real*8,allocatable::U20(:,:,:,:,:,:)
       real*8,allocatable::U21(:,:,:,:,:,:)
       real*8,allocatable::U22(:,:,:,:,:,:)
       real*8,allocatable::U23(:,:,:,:,:,:)
       real*8,allocatable::U24(:,:,:,:,:,:)
       real*8,allocatable::U25(:,:,:,:,:,:)
       real*8,allocatable::U26(:,:,:,:,:,:)
       real*8,allocatable::U27(:,:,:,:,:,:)
       real*8,allocatable::U28(:,:,:,:,:,:)
       real*8,allocatable::U29(:,:,:,:,:,:)
       real*8,allocatable::U30(:,:,:,:,:,:)
       real*8,allocatable::U31(:,:,:,:,:,:)
       real*8,allocatable::U32(:,:,:,:,:,:)
C
       do i=N0+1,N1;do j=N0+1,N2;do k=N0+1,N2
       do a=N1+1,N3;do b=N2+1,N3;do c=N2+1,N3
        V3C(c,b,a,k,j,i)=-H1B(c,j)*l2B(b,a,k,i)
     &                   +H1B(b,j)*l2B(c,a,k,i)
     &                   +H1B(c,k)*l2B(b,a,j,i)
     &                   -H1B(b,k)*l2B(c,a,j,i)
     &                   +H1A(a,i)*l2C(c,b,k,j)
     &                   -l1B(c,j)*H2B(b,a,k,i)
     &                   +l1B(b,j)*H2B(c,a,k,i)
     &                   +l1B(c,k)*H2B(b,a,j,i)
     &                   -l1B(b,k)*H2B(c,a,j,i)
     &                   +l1A(a,i)*H2C(c,b,k,j)
       enddo;enddo;enddo;enddo;enddo;enddo
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef562134(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N2,N3,N1,N3,N0,N2,l3B,F1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call relef3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(S1(N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K4
       I2=K3
       I3=K3*K1*K1
       call jungemm(I1,I2,I3,F1,D2,S1)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef2134(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N2,N3,N0,N2,N0,N1,IntM,D1)
       allocate(U8(N2+1:N3,N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K4
       I2=K2*K3*K4
       I3=K3
       call jungemm(I1,I2,I3,D1,S1,U8)
       deallocate(D1)
C
       call
     & sul234156(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U8,1.0d0/2)
       call
     & sul134256(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U8,-1.0d0/2)
       call
     & sul235146(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U8,-1.0d0/2)
       call
     & sul135246(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U8,1.0d0/2)
       deallocate(U8)
       deallocate(S1)
C
       allocate(F1(N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef523146(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N2,N3,N0,N2,N0,N1,l3B,F1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S2(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K4
       I2=K1
       I3=K3*K3*K1
       call jungemm(I1,I2,I3,F1,D2,S2)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N0,N2,IntM,D1)
       allocate(U9(N2+1:N3,N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K4
       I2=K1*K2*K4
       I3=K1
       call jungemm(I1,I2,I3,D1,S2,U9)
       deallocate(D1)
C
       call
     & sul246135(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U9,1.0d0/2)
       call
     & sul146235(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U9,-1.0d0/2)
       call
     & sul256134(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U9,-1.0d0/2)
       call
     & sul156234(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U9,1.0d0/2)
       deallocate(U9)
       deallocate(S2)
C
       allocate(F1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef452136(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,N1,N3,N0,N1,l3B,F1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,t2B,D2)
       allocate(S3(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,F1,D2,S3)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(U10(N2+1:N3,N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K1*K3*K4
       I3=K4
       call jungemm(I1,I2,I3,D1,S3,U10)
       deallocate(D1)
C
       call
     & sul236145(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U10,1.0d0)
       call
     & sul136245(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U10,-1.0d0)
       deallocate(U10)
       deallocate(S3)
C
       allocate(F1(N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef512346(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,l3B,F1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(S4(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K3
       I2=K2
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,F1,D2,S4)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U11(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K1*K2*K3
       I3=K2
       call jungemm(I1,I2,I3,D1,S4,U11)
       deallocate(D1)
C
       call
     & sul346125(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U11,-1.0d0)
       call
     & sul356124(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U11,1.0d0)
       deallocate(U11)
       deallocate(S4)
C
       allocate(F1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef463125(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,N2,N3,N0,N2,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,t2B,D2)
       allocate(S5(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,F1,D2,S5)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N0,N2,N0,N1,IntM,D1)
       allocate(U24(N2+1:N3,N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K3
       I2=K2*K4*K4
       I3=K4
       call jungemm(I1,I2,I3,D1,S5,U24)
       deallocate(D1)
C
       call
     & sul124356(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U24,1.0d0)
       call
     & sul125346(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U24,-1.0d0)
       deallocate(U24)
       deallocate(S5)
C
       allocate(F1(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef613245(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N2,N3,N0,N2,N0,N2,l3C,F1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(S6(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,F1,D2,S6)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(U25(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K2*K2*K4
       I3=K2
       call jungemm(I1,I2,I3,D1,S6,U25)
       deallocate(D1)
C
       call
     & sul245136(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U25,-1.0d0)
       call
     & sul145236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U25,1.0d0)
       deallocate(U25)
       deallocate(S6)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef461235(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call relef3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2B,D2)
       allocate(S7(N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K4
       I2=K3
       I3=K4*K1*K2
       call jungemm(I1,I2,I3,F1,D2,S7)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef2134(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N2,N3,N0,N2,N0,N1,IntM,D1)
       allocate(U26(N2+1:N3,N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K4
       I2=K2*K3*K4
       I3=K3
       call jungemm(I1,I2,I3,D1,S7,U26)
       deallocate(D1)
C
       call
     & sul234156(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U26,1.0d0)
       call
     & sul134256(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U26,-1.0d0)
       call
     & sul235146(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U26,-1.0d0)
       call
     & sul135246(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U26,1.0d0)
       deallocate(U26)
       deallocate(S7)
C
       allocate(F1(N0+1:N2,N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef413256(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N2,N3,N0,N2,N0,N1,l3C,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(S8(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K4
       I2=K1
       I3=K3*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S8)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N0,N2,IntM,D1)
       allocate(U27(N2+1:N3,N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K4
       I2=K1*K2*K4
       I3=K1
       call jungemm(I1,I2,I3,D1,S8,U27)
       deallocate(D1)
C
       call
     & sul246135(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U27,1.0d0)
       call
     & sul146235(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U27,-1.0d0)
       call
     & sul256134(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U27,-1.0d0)
       call
     & sul156234(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U27,1.0d0)
       deallocate(U27)
       deallocate(S8)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(S9(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,F1,D2,S9)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(U28(N2+1:N3,N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K1*K3*K4
       I3=K4
       call jungemm(I1,I2,I3,D1,S9,U28)
       deallocate(D1)
C
       call
     & sul236145(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U28,1.0d0/2)
       call
     & sul136245(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U28,-1.0d0/2)
       deallocate(U28)
       deallocate(S9)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef412356(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N1,N3,N0,N2,N0,N1,l3C,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S10(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K3
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S10)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U29(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K1*K2*K3
       I3=K2
       call jungemm(I1,I2,I3,D1,S10,U29)
       deallocate(D1)
C
       call
     & sul346125(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U29,-1.0d0/2)
       call
     & sul356124(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U29,1.0d0/2)
       deallocate(U29)
       deallocate(S10)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,l3D,F1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(S11(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,F1,D2,S11)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N0,N2,N0,N1,IntM,D1)
       allocate(U31(N2+1:N3,N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K3
       I2=K2*K4*K4
       I3=K4
       call jungemm(I1,I2,I3,D1,S11,U31)
       deallocate(D1)
C
       call
     & sul124356(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U31,1.0d0/2)
       call
     & sul125346(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U31,-1.0d0/2)
       deallocate(U31)
       deallocate(S11)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef412356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,l3D,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S12(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S12)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(U32(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K2*K2*K4
       I3=K2
       call jungemm(I1,I2,I3,D1,S12,U32)
       deallocate(D1)
C
       call
     & sul245136(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U32,-1.0d0/2)
       call
     & sul145236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U32,1.0d0/2)
       deallocate(U32)
       deallocate(S12)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef2134(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N0,N2,N0,N1,H2B,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,l2B,D2)
       allocate(U1(N2+1:N3,N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K4
       I2=K2*K3*K4
       I3=K1
       call jungemm(I1,I2,I3,D1,D2,U1)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul234156(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U1,1.0d0)
       call
     & sul134256(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U1,-1.0d0)
       call
     & sul235146(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U1,-1.0d0)
       call
     & sul135246(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U1,1.0d0)
       deallocate(U1)
C
       allocate(D1(N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N2,N3,N1,N3,N0,N2,H2B,D1)
       allocate(D2(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,N3,N0,N2,N0,N1,l2B,D2)
       allocate(U2(N2+1:N3,N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K4
       I2=K1*K2*K4
       I3=K3
       call jungemm(I1,I2,I3,D1,D2,U2)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul246135(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U2,-1.0d0)
       call
     & sul146235(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U2,1.0d0)
       call
     & sul256134(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U2,1.0d0)
       call
     & sul156234(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U2,-1.0d0)
       deallocate(U2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,H2C,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,l2B,D2)
       allocate(U3(N2+1:N3,N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K1*K3*K4
       I3=K2
       call jungemm(I1,I2,I3,D1,D2,U3)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul236145(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U3,1.0d0)
       call
     & sul136245(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U3,-1.0d0)
       deallocate(U3)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N2,N3,N0,N2,H2C,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,l2B,D2)
       allocate(U4(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K1*K2*K3
       I3=K4
       call jungemm(I1,I2,I3,D1,D2,U4)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul346125(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U4,1.0d0)
       call
     & sul356124(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U4,-1.0d0)
       deallocate(U4)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N0,N2,N0,N1,H2B,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,l2C,D2)
       allocate(U5(N2+1:N3,N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K3
       I2=K2*K4*K4
       I3=K2
       call jungemm(I1,I2,I3,D1,D2,U5)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul124356(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U5,1.0d0)
       call
     & sul125346(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U5,-1.0d0)
       deallocate(U5)
C
       allocate(D1(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N1,N3,N0,N1,H2B,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,l2C,D2)
       allocate(U6(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K2*K2*K4
       I3=K4
       call jungemm(I1,I2,I3,D1,D2,U6)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul245136(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U6,1.0d0)
       call
     & sul145236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U6,-1.0d0)
       deallocate(U6)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,H2B,D1)
       allocate(F2(N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef521346(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1,
     & N0,N1,N1,N3,N2,N3,N1,N3,N0,N2,N0,N1,l3B,F2)
       allocate(U7(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K2*K3*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,F2,U7)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul234615(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U7,-1.0d0)
       call
     & sul134625(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U7,1.0d0)
       call
     & sul235614(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U7,1.0d0)
       call
     & sul135624(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U7,-1.0d0)
       deallocate(U7)
C
       allocate(B1(N0+1:N1,N0+1:N1))
       call relef12(N0,N3,N0,N3,
     & N0,N1,N0,N1,H1A,B1)
       allocate(F2(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2))
       call relef612345(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,l3C,F2)
       allocate(U12(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1
       I2=K2*K2*K3*K4*K4
       I3=K1
       call jungemm(I1,I2,I3,B1,F2,U12)
       deallocate(B1)
       deallocate(F2)
C
       V3C=V3C-U12
       deallocate(U12)
C
       allocate(B1(N1+1:N3,N1+1:N3))
       call relef21(N0,N3,N0,N3,
     & N1,N3,N1,N3,H1A,B1)
       allocate(F2(N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       call relef312456(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N1,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N1,l3C,F2)
       allocate(U13(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1*K2*K2*K4*K4
       I3=K3
       call jungemm(I1,I2,I3,B1,F2,U13)
       deallocate(B1)
       deallocate(F2)
C
       call
     & sul124563(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U13,1.0d0)
       deallocate(U13)
C
       allocate(B1(N0+1:N2,N0+1:N2))
       call relef12(N0,N3,N0,N3,
     & N0,N2,N0,N2,H1B,B1)
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef412356(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N1,N3,N0,N2,N0,N1,l3C,F2)
       allocate(U14(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
       I1=K2
       I2=K1*K2*K3*K4*K4
       I3=K2
       call jungemm(I1,I2,I3,B1,F2,U14)
       deallocate(B1)
       deallocate(F2)
C
       call
     & sul123465(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U14,1.0d0)
       call
     & sul123564(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U14,-1.0d0)
       deallocate(U14)
C
       allocate(B1(N2+1:N3,N2+1:N3))
       call relef21(N0,N3,N0,N3,
     & N2,N3,N2,N3,H1B,B1)
       allocate(F2(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       call relef123456(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,l3C,F2)
       allocate(U15(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4
       I2=K1*K2*K2*K3*K4
       I3=K4
       call jungemm(I1,I2,I3,B1,F2,U15)
       deallocate(B1)
       deallocate(F2)
C
       call
     & sul234561(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U15,1.0d0)
       call
     & sul134562(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U15,-1.0d0)
       deallocate(U15)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,H2A,D1)
       allocate(F2(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef631245(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N2,N3,N0,N2,N0,N2,l3C,F2)
       allocate(U16(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K2*K2*K4*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,F2,U16)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul124536(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U16,1.0d0)
       deallocate(U16)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N0,N2,N0,N1,H2B,D1)
       allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef461235(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,l3C,F2)
       allocate(U17(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K2*K3*K4*K4
       I3=K1*K2
       call jungemm(I1,I2,I3,D1,F2,U17)
       deallocate(D1)
       deallocate(F2)
C
       V3C=V3C-U17
       call
     & sul123546(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U17,1.0d0)
       deallocate(U17)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       call relef2314(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N2,N3,N0,N1,H2B,D1)
       allocate(F2(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2))
       call relef612345(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,l3C,F2)
       allocate(U18(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K2*K2*K3*K4
       I3=K4*K1
       call jungemm(I1,I2,I3,D1,F2,U18)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul234516(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U18,-1.0d0)
       call
     & sul134526(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U18,1.0d0)
       deallocate(U18)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       call relef1423(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N1,N3,N0,N2,H2B,D1)
       allocate(F2(N0+1:N2,N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call relef431256(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,N3,N2,N3,N0,N2,N0,N1,l3C,F2)
       allocate(U19(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3
       I2=K1*K2*K4*K4
       I3=K3*K2
       call jungemm(I1,I2,I3,D1,F2,U19)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul124635(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U19,1.0d0)
       call
     & sul125634(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U19,-1.0d0)
       deallocate(U19)
C
       allocate(D1(N2+1:N3,N1+1:N3,N2+1:N3,N1+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N2,N3,N1,N3,H2B,D1)
       allocate(F2(N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       call relef132456(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N2,N3,N1,N3,N2,N3,N0,N2,N0,N2,N0,N1,l3C,F2)
       allocate(U20(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K1*K2*K2*K4
       I3=K3*K4
       call jungemm(I1,I2,I3,D1,F2,U20)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul245613(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U20,1.0d0)
       call
     & sul145623(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U20,-1.0d0)
       deallocate(U20)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N0,N2,N0,N2,H2C,D1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,l3C,F2)
       allocate(U21(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K1*K3*K4*K4
       I3=K2*K2
       call jungemm(I1,I2,I3,D1,F2,U21)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul123645(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U21,1.0d0/2)
       deallocate(U21)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,H2C,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call relef412356(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N2,N3,N2,N3,N1,N3,N0,N2,N0,N1,l3C,F2)
       allocate(U22(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K2*K3*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,F2,U22)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul234615(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U22,-1.0d0)
       call
     & sul134625(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U22,1.0d0)
       call
     & sul235614(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U22,1.0d0)
       call
     & sul135624(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U22,-1.0d0)
       deallocate(U22)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N2,N3,N2,N3,H2C,D1)
       allocate(F2(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       call relef123456(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,l3C,F2)
       allocate(U23(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K1*K2*K2*K3
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,F2,U23)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul345612(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U23,1.0d0/2)
       deallocate(U23)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,H2B,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef412356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,l3D,F2)
       allocate(U30(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K2*K2*K4*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,F2,U30)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul124536(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,V3C,U30,1.0d0)
       deallocate(U30)
C
       end
