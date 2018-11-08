       subroutine L3D_update_cor(N0,N1,N2,N3,V3D,
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
       real*8 V3D(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2)
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
C
       do i=N0+1,N2;do j=N0+1,N2;do k=N0+1,N2
       do a=N2+1,N3;do b=N2+1,N3;do c=N2+1,N3
        V3D(c,b,a,k,j,i)=+H1B(c,i)*l2C(b,a,k,j)
     &                   -H1B(b,i)*l2C(c,a,k,j)
     &                   +H1B(a,i)*l2C(c,b,k,j)
     &                   -H1B(c,j)*l2C(b,a,k,i)
     &                   +H1B(b,j)*l2C(c,a,k,i)
     &                   -H1B(a,j)*l2C(c,b,k,i)
     &                   +H1B(c,k)*l2C(b,a,j,i)
     &                   -H1B(b,k)*l2C(c,a,j,i)
     &                   +H1B(a,k)*l2C(c,b,j,i)
     &                   +l1B(c,i)*H2C(b,a,k,j)
     &                   -l1B(b,i)*H2C(c,a,k,j)
     &                   +l1B(a,i)*H2C(c,b,k,j)
     &                   -l1B(c,j)*H2C(b,a,k,i)
     &                   +l1B(b,j)*H2C(c,a,k,i)
     &                   -l1B(a,j)*H2C(c,b,k,i)
     &                   +l1B(c,k)*H2C(b,a,j,i)
     &                   -l1B(b,k)*H2C(c,a,j,i)
     &                   +l1B(a,k)*H2C(c,b,j,i)
       enddo;enddo;enddo;enddo;enddo;enddo
C
       allocate(F1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef463125(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,N2,N3,N0,N2,l3C,F1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call relef3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,t2B,D2)
       allocate(S1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K3*K1*K2
       call jungemm(I1,I2,I3,F1,D2,S1)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(U4(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K4
       call jungemm(I1,I2,I3,D1,S1,U4)
       deallocate(D1)
C
       call
     & sul234156(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U4,1.0d0)
       call
     & sul134256(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U4,-1.0d0)
       call
     & sul124356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U4,1.0d0)
       call
     & sul235146(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U4,-1.0d0)
       call
     & sul135246(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U4,1.0d0)
       call
     & sul125346(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U4,-1.0d0)
       call
     & sul236145(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U4,1.0d0)
       call
     & sul136245(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U4,-1.0d0)
       call
     & sul126345(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U4,1.0d0)
       deallocate(U4)
       deallocate(S1)
C
       allocate(F1(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef613245(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N2,N3,N0,N2,N0,N2,l3C,F1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call relef4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(S2(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2
       I3=K3*K4*K1
       call jungemm(I1,I2,I3,F1,D2,S2)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U5(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2*K2*K4
       I3=K2
       call jungemm(I1,I2,I3,D1,S2,U5)
       deallocate(D1)
C
       call
     & sul345126(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5,1.0d0)
       call
     & sul245136(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5,-1.0d0)
       call
     & sul145236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5,1.0d0)
       call
     & sul346125(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5,-1.0d0)
       call
     & sul246135(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5,1.0d0)
       call
     & sul146235(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5,-1.0d0)
       call
     & sul356124(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5,1.0d0)
       call
     & sul256134(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5,-1.0d0)
       call
     & sul156234(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5,1.0d0)
       deallocate(U5)
       deallocate(S2)
C
       allocate(F1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,l3D,F1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call relef3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(S3(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K4*K2*K2
       call jungemm(I1,I2,I3,F1,D2,S3)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(U11(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K4
       call jungemm(I1,I2,I3,D1,S3,U11)
       deallocate(D1)
C
       call
     & sul234156(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U11,1.0d0/2)
       call
     & sul134256(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U11,-1.0d0/2)
       call
     & sul124356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U11,1.0d0/2)
       call
     & sul235146(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U11,-1.0d0/2)
       call
     & sul135246(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U11,1.0d0/2)
       call
     & sul125346(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U11,-1.0d0/2)
       call
     & sul236145(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U11,1.0d0/2)
       call
     & sul136245(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U11,-1.0d0/2)
       call
     & sul126345(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U11,1.0d0/2)
       deallocate(U11)
       deallocate(S3)
C
       allocate(F1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef412356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,l3D,F1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S4(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2
       I3=K4*K4*K2
       call jungemm(I1,I2,I3,F1,D2,S4)
       deallocate(F1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U12(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2*K2*K4
       I3=K2
       call jungemm(I1,I2,I3,D1,S4,U12)
       deallocate(D1)
C
       call
     & sul345126(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12,1.0d0/2)
       call
     & sul245136(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12,-1.0d0/2)
       call
     & sul145236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12,1.0d0/2)
       call
     & sul346125(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12,-1.0d0/2)
       call
     & sul246135(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12,1.0d0/2)
       call
     & sul146235(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12,-1.0d0/2)
       call
     & sul356124(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12,1.0d0/2)
       call
     & sul256134(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12,-1.0d0/2)
       call
     & sul156234(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12,1.0d0/2)
       deallocate(U12)
       deallocate(S4)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,H2C,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,l2C,D2)
       allocate(U1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K2
       call jungemm(I1,I2,I3,D1,D2,U1)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul234156(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U1,1.0d0)
       call
     & sul134256(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U1,-1.0d0)
       call
     & sul124356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U1,1.0d0)
       call
     & sul235146(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U1,-1.0d0)
       call
     & sul135246(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U1,1.0d0)
       call
     & sul125346(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U1,-1.0d0)
       call
     & sul236145(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U1,1.0d0)
       call
     & sul136245(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U1,-1.0d0)
       call
     & sul126345(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U1,1.0d0)
       deallocate(U1)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N2,N3,N0,N2,H2C,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,l2C,D2)
       allocate(U2(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2*K2*K4
       I3=K4
       call jungemm(I1,I2,I3,D1,D2,U2)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sul345126(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U2,-1.0d0)
       call
     & sul245136(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U2,1.0d0)
       call
     & sul145236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U2,-1.0d0)
       call
     & sul346125(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U2,1.0d0)
       call
     & sul246135(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U2,-1.0d0)
       call
     & sul146235(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U2,1.0d0)
       call
     & sul356124(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U2,-1.0d0)
       call
     & sul256134(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U2,1.0d0)
       call
     & sul156234(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U2,-1.0d0)
       deallocate(U2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call relef2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,H2B,D1)
       allocate(F2(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef631245(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N2,N3,N0,N2,N0,N2,l3C,F2)
       allocate(U3(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K2*K4*K4
       I3=K3*K1
       call jungemm(I1,I2,I3,D1,F2,U3)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul234516(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U3,1.0d0)
       call
     & sul134526(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U3,-1.0d0)
       call
     & sul124536(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U3,1.0d0)
       call
     & sul234615(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U3,-1.0d0)
       call
     & sul134625(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U3,1.0d0)
       call
     & sul124635(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U3,-1.0d0)
       call
     & sul235614(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U3,1.0d0)
       call
     & sul135624(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U3,-1.0d0)
       call
     & sul125634(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U3,1.0d0)
       deallocate(U3)
C
       allocate(B1(N0+1:N2,N0+1:N2))
       call relef12(N0,N3,N0,N3,
     & N0,N2,N0,N2,H1B,B1)
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef412356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,l3D,F2)
       allocate(U6(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K2*K4*K4*K4
       I3=K2
       call jungemm(I1,I2,I3,B1,F2,U6)
       deallocate(B1)
       deallocate(F2)
C
       V3D=V3D-U6
       call
     & sul123465(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U6,1.0d0)
       call
     & sul123564(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U6,-1.0d0)
       deallocate(U6)
C
       allocate(B1(N2+1:N3,N2+1:N3))
       call relef21(N0,N3,N0,N3,
     & N2,N3,N2,N3,H1B,B1)
       allocate(F2(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef123456(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,l3D,F2)
       allocate(U7(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2*K2*K2*K4*K4
       I3=K4
       call jungemm(I1,I2,I3,B1,F2,U7)
       deallocate(B1)
       deallocate(F2)
C
       call
     & sul234561(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U7,1.0d0)
       call
     & sul134562(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U7,-1.0d0)
       call
     & sul124563(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U7,1.0d0)
       deallocate(U7)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N0,N2,N0,N2,H2C,D1)
       allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,l3D,F2)
       allocate(U8(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4*K4*K4
       I3=K2*K2
       call jungemm(I1,I2,I3,D1,F2,U8)
       deallocate(D1)
       deallocate(F2)
C
       V3D=V3D+1.0d0/2*U8
       call
     & sul123546(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U8,-1.0d0/2)
       call
     & sul123645(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U8,1.0d0/2)
       deallocate(U8)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call relef1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,H2C,D1)
       allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call relef412356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,l3D,F2)
       allocate(U9(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K2*K4*K4
       I3=K4*K2
       call jungemm(I1,I2,I3,D1,F2,U9)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul234516(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U9,1.0d0)
       call
     & sul134526(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U9,-1.0d0)
       call
     & sul124536(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U9,1.0d0)
       call
     & sul234615(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U9,-1.0d0)
       call
     & sul134625(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U9,1.0d0)
       call
     & sul124635(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U9,-1.0d0)
       call
     & sul235614(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U9,1.0d0)
       call
     & sul135624(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U9,-1.0d0)
       call
     & sul125634(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U9,1.0d0)
       deallocate(U9)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       call relef3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N2,N3,N2,N3,H2C,D1)
       allocate(F2(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       call relef123456(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,l3D,F2)
       allocate(U10(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K2*K2*K2*K4
       I3=K4*K4
       call jungemm(I1,I2,I3,D1,F2,U10)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sul345612(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U10,1.0d0/2)
       call
     & sul245613(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U10,-1.0d0/2)
       call
     & sul145623(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U10,1.0d0/2)
       deallocate(U10)
C
       end
