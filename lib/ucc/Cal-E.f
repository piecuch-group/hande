       subroutine calculate_energy(N1,N2,N3,N0,
     & FockR,FockB,IntR,IntB,IntM,t1A,t1B,t2A,t2B,t2C,
     & E1A,E1B,E2A,E2B,E2C,E1A1A,E1A1B,E1B1B)
C
       real*8 EqnRight,PP
       integer a,b,c,d,e,f,i,j,k,l,m,n,r,s,t,u
       real*8 E1A,E1B,E2A,E2B,E2C,E1A1A,E1B1B,E1A1B
C
       real*8 IntR(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 IntB(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 IntM(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 FockR(N3,N3)
       real*8 FockB(N3,N3)
       real*8 t1A(N1+1:N3,N0+1:N1)
       real*8 t1B(N2+1:N3,N0+1:N2)
       real*8 t2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 t2B(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
       real*8 t2C(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
C
! Caluculate E2A,E2B,E2C
       E2A=0.0d0;E2B=0.0d0;E2C=0.0d0
       do m=N0+1,N1
       do n=N0+1,N1
       do e=N1+1,N3
       do f=N1+1,N3
         E2A=E2A-0.25*IntR(e,f,m,n)*t2A(f,e,n,m)
       enddo
       enddo
       enddo
       enddo
       do m=N0+1,N2
       do n=N0+1,N2
       do e=N2+1,N3
       do f=N2+1,N3
         E2B=E2B-0.25*IntB(e,f,m,n)*t2B(f,e,n,m)
       enddo
       enddo
       enddo
       enddo
       do m=N0+1,N1
       do n=N0+1,N2
       do e=N1+1,N3
       do f=N2+1,N3
         E2C=E2C-IntM(e,f,m,n)*t2C(f,e,n,m)
       enddo
       enddo
       enddo
       enddo
C
C  Calculate E1A,E1B
           E1A=0.0d0;E1B=0.0d0
           do e= N1+1,N3
           do m= N0+1,N1
             E1A=E1A+FockR(m,e)*t1A(e,m)
           enddo
           enddo
           do e= N2+1,N3
           do m= N0+1,N2
             E1B=E1B+FockB(m,e)*t1B(e,m)
           enddo
           enddo
C
! Caluculate E1A1A,E1A1B,E1B1B
       E1A1A=0.0d0;E1A1B=0.0d0;E1B1B=0.0d0
       do m=N0+1,N1
       do n=N0+1,N1
       do e=N1+1,N3
       do f=N1+1,N3
         E1A1A=E1A1A+0.50*IntR(e,f,m,n)*t1A(f,n)*t1A(e,m)
       enddo
       enddo
       enddo
       enddo
       do m=N0+1,N2
       do n=N0+1,N2
       do e=N2+1,N3
       do f=N2+1,N3
         E1B1B=E1B1B+0.50*IntB(e,f,m,n)*t1B(f,n)*t1B(e,m)
       enddo
       enddo
       enddo
       enddo
       do m=N0+1,N1
       do n=N0+1,N2
       do e=N1+1,N3
       do f=N2+1,N3
         E1A1B=E1A1B+IntM(e,f,m,n)*t1A(e,m)*t1B(f,n)
       enddo
       enddo
       enddo
       enddo
C
       end
