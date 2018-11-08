       subroutine Cal_R0(N1,N2,N3,N0,
     & H1A,H1B,H2A,H2B,H2C,r1A,r1B,r2A,r2B,r2C,
     & Ecor,R0)
C
       real*8 ECor,R0
       integer a,b,c,d,e,f,i,j,k,l,m,n,r,s,t,u
       real*8 H1A(N0+1:N3,N0+1:N3)
       real*8 H1B(N0+1:N3,N0+1:N3)
       real*8 H2A(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 H2B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 H2C(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 r1A(N1+1:N3,N0+1:N1)
       real*8 r1B(N2+1:N3,N0+1:N2)
       real*8 r2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 r2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 r2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
C
       R0=0.0d0
       do m= N0+1,N1
       do e= N1+1,N3
         R0=R0+H1A(e,m)*r1A(e,m)
       enddo
       enddo
       do m= N0+1,N2
       do e= N2+1,N3
         R0=R0+H1B(e,m)*r1B(e,m)
       enddo
       enddo
       do m=N0+1,N1
       do n=N0+1,N1
       do e=N1+1,N3
       do f=N1+1,N3
         R0=R0+0.25*H2A(f,e,n,m)*r2A(f,e,n,m)
       enddo
       enddo
       enddo
       enddo
       do m=N0+1,N1
       do n=N0+1,N2
       do e=N1+1,N3
       do f=N2+1,N3
         R0=R0+H2B(f,e,n,m)*r2B(f,e,n,m)
       enddo
       enddo
       enddo
       enddo
       do m=N0+1,N2
       do n=N0+1,N2
       do e=N2+1,N3
       do f=N2+1,N3
         R0=R0+0.25*H2C(f,e,n,m)*r2C(f,e,n,m)
       enddo
       enddo
       enddo
       enddo
       R0=R0/ECor
C
       end
