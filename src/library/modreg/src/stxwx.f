      subroutine stxwx(x,z,w,k,xknot,n,y,hs0,hs1,hs2,hs3)
      implicit double precision(a-h,o-z)
      integer k,n,j,i,ilo,ileft,mflag
      double precision z(k),w(k),x(k),xknot(n+4),y(n),hs0(n),hs1(n),hs2(
&     n),hs3(n),eps,vnikx(4,1),work(16)
      lenxk=n+4
      do 23000 i=1,n 
      y(i)=0e0 
      hs0(i)=0e0 
      hs1(i)=0e0
      hs2(i)=0e0 
      hs3(i)=0e0 
23000 continue
      ilo=1 
      eps = .1e-9
      do 23002 i=1,k 
      call interv(xknot(1),(n+1),x(i),ileft,mflag)
      if(.not.(mflag.eq. 1))goto 23004
      if(.not.(x(i).le.(xknot(ileft)+eps)))goto 23006
      ileft=ileft-1
      goto 23007
23006 continue
      return
23007 continue
23004 continue
      call bsplvd (xknot,lenxk,4,x(i),ileft,work,vnikx,1)
      j= ileft-4+1
      y(j) = y(j)+w(i)**2*z(i)*vnikx(1,1)
      hs0(j)=hs0(j)+w(i)**2*vnikx(1,1)**2
      hs1(j)=hs1(j)+w(i)**2*vnikx(1,1)*vnikx(2,1)
      hs2(j)=hs2(j)+w(i)**2*vnikx(1,1)*vnikx(3,1)
      hs3(j)=hs3(j)+w(i)**2*vnikx(1,1)*vnikx(4,1)
      j= ileft-4+2
      y(j) = y(j)+w(i)**2*z(i)*vnikx(2,1)
      hs0(j)=hs0(j)+w(i)**2*vnikx(2,1)**2
      hs1(j)=hs1(j)+w(i)**2*vnikx(2,1)*vnikx(3,1)
      hs2(j)=hs2(j)+w(i)**2*vnikx(2,1)*vnikx(4,1)
      j= ileft-4+3
      y(j) = y(j)+w(i)**2*z(i)*vnikx(3,1)
      hs0(j)=hs0(j)+w(i)**2*vnikx(3,1)**2
      hs1(j)=hs1(j)+w(i)**2*vnikx(3,1)*vnikx(4,1)
      j= ileft-4+4
      y(j) = y(j)+w(i)**2*z(i)*vnikx(4,1)
      hs0(j)=hs0(j)+w(i)**2*vnikx(4,1)**2 
23002 continue
      return
      end
