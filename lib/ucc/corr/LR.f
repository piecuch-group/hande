       subroutine LR(N1,N2,N3,N0,res,p_space,
     & r1A,r1B,r2A,r2B,r2C,r3A,r3B,r3C,r3D,
     & l1A,l1B,l2A,l2B,l2C,l3A,l3B,l3C,l3D)
C
       real*8 res
       integer a,b,c,d,e,f,g,i,j,k,l,m,n,o,r,s,t,u
       real*8 r1A(N1+1:N3,N0+1:N1)
       real*8 r1B(N2+1:N3,N0+1:N2)
       real*8 r2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 r2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 r2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
       real*8 r3A(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)
       real*8 r3B(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1)
       real*8 r3C(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1)
       real*8 r3D(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2)
       real*8 l1A(N1+1:N3,N0+1:N1)
       real*8 l1B(N2+1:N3,N0+1:N2)
       real*8 l2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 l2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 l2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
       real*8 l3A(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)
       real*8 l3B(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1)
       real*8 l3C(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1)
       real*8 l3D(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2)
      integer p_space(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)
C
       res=0.0d0
       do m= N0+1,N1
       do e= N1+1,N3
         res=res+l1A(e,m)*r1A(e,m)
       enddo
       enddo
       do m= N0+1,N2
       do e= N2+1,N3
         res=res+l1B(e,m)*r1B(e,m)
       enddo
       enddo
       do m=N0+1,N1
       do n=N0+1,N1
       do e=N1+1,N3
       do f=N1+1,N3
         res=res+0.25*l2A(f,e,n,m)*r2A(f,e,n,m)
       enddo
       enddo
       enddo
       enddo
       do m=N0+1,N1
       do n=N0+1,N2
       do e=N1+1,N3
       do f=N2+1,N3
         res=res+l2B(f,e,n,m)*r2B(f,e,n,m)
       enddo
       enddo
       enddo
       enddo
       do m=N0+1,N2
       do n=N0+1,N2
       do e=N2+1,N3
       do f=N2+1,N3
         res=res+0.25*l2C(f,e,n,m)*r2C(f,e,n,m)
       enddo
       enddo
       enddo
       enddo
       do m=N0+1,N1
       do n=N0+1,N1
       do o=N0+1,N1
       do e=N1+1,N3
       do f=N1+1,N3
       do g=N1+1,N3
         if(p_space(e,f,g,m,n,o) /= 1) then
             cycle
         endif
         res=res+l3A(g,f,e,o,n,m)*r3A(g,f,e,o,n,m)/36.0d0
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       do m=N0+1,N1
       do n=N0+1,N1
       do o=N0+1,N2
       do e=N1+1,N3
       do f=N1+1,N3
       do g=N2+1,N3
         if(p_space(e,f,g,m,n,o) /= 1) then
             cycle
         endif
         res=res+l3B(g,f,e,o,n,m)*r3B(g,f,e,o,n,m)/4.0d0
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       do m=N0+1,N1
       do n=N0+1,N2
       do o=N0+1,N2
       do e=N1+1,N3
       do f=N2+1,N3
       do g=N2+1,N3
         if(p_space(e,f,g,m,n,o) /= 1) then
             cycle
         endif
         res=res+l3C(g,f,e,o,n,m)*r3C(g,f,e,o,n,m)/4.0d0
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       do m=N0+1,N2
       do n=N0+1,N2
       do o=N0+1,N2
       do e=N2+1,N3
       do f=N2+1,N3
       do g=N2+1,N3
         if(p_space(e,f,g,m,n,o) /= 1) then
             cycle
         endif
         res=res+l3D(g,f,e,o,n,m)*r3D(g,f,e,o,n,m)/36.0d0
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
