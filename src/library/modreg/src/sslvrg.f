      subroutine sslvrg(penalt,dofoff,x,y,w,n,knot,nk,coef,sz,lev,crit,
&     icrit,spar,ratio,xwy,hs0,hs1,hs2,hs3,sg0,sg1,sg2,sg3,abd,p1ip,
&     p2ip,ld4,ldnk,info)
      implicit double precision(a-h,o-z)
      integer n,nk,icrit,ld4,ldnk,i,icoef,ileft,ilo,info,j,mflag
      double precision penalt,dofoff,x(n),y(n),w(n),knot(nk+4),coef(nk),
&     sz(n),lev(n),crit,ratio,spar,xwy(nk),hs0(nk),hs1(nk),hs2(nk),hs3(
&     nk), sg0(nk),sg1(nk),sg2(nk),sg3(nk),abd(ld4,nk),p1ip(ld4,nk),
&     p2ip(ldnk,nk),lambda,b0,b1,b2,b3,eps,vnikx(4,1),work(16),xv,
&     bvalue,rss,df
      lenkno=nk+4
      ilo = 1 
      eps = .1e-10
      lambda = ratio*16.**(-2. + spar*(6.))
      do 23000 i=1,nk 
      coef(i) = xwy(i) 
23000 continue
      do 23002 i=1,nk 
      abd(4,i) = hs0(i)+lambda*sg0(i) 
23002 continue
      do 23004 i=1,(nk-1) 
      abd(3,i+1) = hs1(i)+lambda*sg1(i) 
23004 continue
      do 23006 i=1,(nk-2) 
      abd(2,i+2) = hs2(i)+lambda*sg2(i) 
23006 continue
      do 23008 i=1,(nk-3) 
      abd(1,i+3) = hs3(i)+lambda*sg3(i) 
23008 continue
      call dpbfa(abd,ld4,nk,3,info)
      if(.not.(info.ne.0))goto 23010
      return
23010 continue
      call dpbsl(abd,ld4,nk,3,coef)
      icoef = 1
      do 23012 i=1,n 
      xv = x(i)
      sz(i) = bvalue(knot,lenkno,coef,nk,4,xv,0)
23012 continue
      if(.not.(icrit.eq.0))goto 23014
      return
23014 continue
      call sinerp(abd,ld4,nk,p1ip,p2ip,ldnk,0)
      do 23016 i=1,n 
      xv = x(i)
      call interv(knot(1),(nk+1),xv,ileft,mflag)
      if(.not.(mflag.eq.-1))goto 23018
      ileft = 4 
      xv = knot(4)+eps 
23018 continue
      if(.not.(mflag.eq.1))goto 23020
      ileft = nk 
      xv = knot(nk+1)-eps 
23020 continue
      j=ileft-3
      call bsplvd(knot,lenkno,4,xv,ileft,work,vnikx,1)
      b0=vnikx(1,1)
      b1=vnikx(2,1)
      b2=vnikx(3,1)
      b3=vnikx(4,1)
      lev(i) = (p1ip(4,j)*b0**2 + 2.*p1ip(3,j)*b0*b1 +2.*p1ip(2,j)*b0*
&     b2 + 2.*p1ip(1,j)*b0*b3 +p1ip(4,j+1)*b1**2 + 2.*p1ip(3,j+1)*b1*b2 
&     +2.*p1ip(2,j+1)*b1*b3 +p1ip(4,j+2)*b2**2 + 2.*p1ip(3,j+2)*b2*b3 +
&     p1ip(4,j+3)*b3**2 )*w(i)**2 
23016 continue
      if(.not.(icrit.eq.1))goto 23022
      rss = 0e0 
      df = 0e0 
      sumw=0e0
      do 23024 i=1,n 
      rss = rss + ((y(i)-sz(i))*w(i))**2
23024 continue
      do 23026 i=1,n 
      df = df + lev(i)
23026 continue
      crit = (rss/n)/((1e0-(dofoff+penalt*df)/n)**2)
      goto 23023
23022 continue
      if(.not.(icrit.eq.2))goto 23028
      crit = 0e0
      do 23030 i=1,n 
      crit = crit +(((y(i)-sz(i))*w(i))/(1-lev(i)))**2 
23030 continue
      crit=crit/n
      goto 23029
23028 continue
      crit=0e0
      do 23032 i=1,n 
      crit=crit+lev(i)
23032 continue
      crit=3+(dofoff-crit)**2
23029 continue
23023 continue
      return
23015 continue
      end
