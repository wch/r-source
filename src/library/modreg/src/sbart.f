      subroutine sbart(penalt,dofoff,xs,ys,ws,n,knot,nk,coef,sz,lev,
&     crit,icrit,spar,ispar,lspar,uspar,tol,isetup,xwy,hs0,hs1,hs2,hs3,
&     sg0,sg1,sg2,sg3,abd,p1ip,p2ip,ld4,ldnk,ier)
      implicit double precision(a-h,o-z)
      integer n,nk,isetup,icrit,ispar,ld4,ldnk,ier
      double precision penalt,dofoff,xs(n),ys(n),ws(n),knot(nk+4),coef(
&     nk),sz(n),lev(n),crit,spar,lspar,uspar,tol,xwy(nk),hs0(nk),hs1(nk)
&     ,hs2(nk),hs3(nk), sg0(nk),sg1(nk),sg2(nk),sg3(nk),abd(ld4,nk),
&     p1ip(ld4,nk),p2ip(ldnk,nk)
      double precisiont1,t2,ratio, a,b,c,d,e,eps,xm,p,q,r,tol1,tol2,u,v,
&     w, fu,fv,fw,fx,x,ax,bx
      integer i
      i=1
23000 if(.not.(i.le.n))goto 23002
      if(.not.(ws(i).gt.0))goto 23003
      ws(i)=sqrt(ws(i)) 
23003 continue
      i=i+1
      goto 23000
23002 continue
      if(.not.(isetup.eq.0))goto 23005
      call sgram(sg0,sg1,sg2,sg3,knot,nk)
      call stxwx(xs,ys,ws,n,knot,nk,xwy,hs0,hs1,hs2,hs3)
      t1=0. 
      t2=0.
      do 23007 i=3,nk-3 
      t1 = t1 + hs0(i) 
23007 continue
      do 23009 i=3,nk-3 
      t2 = t2 + sg0(i) 
23009 continue
      ratio = t1/t2
      isetup = 1 
23005 continue
      if(.not.(ispar.eq.1))goto 23011
      call sslvrg(penalt,dofoff,xs,ys,ws,n,knot,nk,coef,sz,lev,crit,
&     icrit,spar,ratio,xwy,hs0,hs1,hs2,hs3,sg0,sg1,sg2,sg3,abd,p1ip,
&     p2ip,ld4,ldnk,ier)
      return
23011 continue
      ax=lspar 
      bx=uspar
      c = 0.5*(3. - sqrt(5e0))
      eps = 1e0
10    eps = eps/2e0
      tol1 = 1e0 + eps
      if(.not.(tol1 .gt. 1e0))goto 23013
      go to 10
23013 continue
      eps = sqrt(eps)
      eps=.000244
      a = ax
      b = bx
      v = a + c*(b - a)
      w = v
      x = v
      e = 0.0
      spar = x
      call sslvrg(penalt,dofoff,xs,ys,ws,n,knot,nk,coef,sz,lev,crit,
&     icrit,spar,ratio,xwy,hs0,hs1,hs2,hs3,sg0,sg1,sg2,sg3,abd,p1ip,
&     p2ip,ld4,ldnk,ier)
      fx = crit
      fv = fx
      fw = fx
20    xm = 0.5*(a + b)
      tol1 = eps*abs(x) + tol/3e0
      tol2 = 2e0*tol1
      if(.not.(abs(x - xm) .le. (tol2 - 0.5*(b - a))))goto 23015
      go to 90
23015 continue
      if(.not.(abs(e) .le. tol1))goto 23017
      go to 40
23017 continue
      r = (x - w)*(fx - fv)
      q = (x - v)*(fx - fw)
      p = (x - v)*q - (x - w)*r
      q = 2.00*(q - r)
      if(.not.(q .gt. 0.0))goto 23019
      p = -p
23019 continue
      q = abs(q)
      r = e
      e = d
30    if(.not.(abs(p) .ge. abs(0.5*q*r)))goto 23021
      go to 40
23021 continue
      if(.not.(p .le. q*(a - x)))goto 23023
      go to 40
23023 continue
      if(.not.(p .ge. q*(b - x)))goto 23025
      go to 40
23025 continue
      d = p/q
      u = x + d
      if(.not.((u - a) .lt. tol2))goto 23027
      d = sign(tol1, xm - x)
23027 continue
      if(.not.((b - u) .lt. tol2))goto 23029
      d = sign(tol1, xm - x)
23029 continue
      go to 50
40    if(.not.(x .ge. xm))goto 23031
      e = a - x
23031 continue
      if(.not.(x .lt. xm))goto 23033
      e = b - x
23033 continue
      d = c*e
50    if(.not.(abs(d) .ge. tol1))goto 23035
      u = x + d
23035 continue
      if(.not.(abs(d) .lt. tol1))goto 23037
      u = x + sign(tol1, d)
23037 continue
      spar = u
      call sslvrg(penalt,dofoff,xs,ys,ws,n,knot,nk,coef,sz,lev,crit,
&     icrit,spar,ratio,xwy,hs0,hs1,hs2,hs3,sg0,sg1,sg2,sg3,abd,p1ip,
&     p2ip,ld4,ldnk,ier)
      fu = crit
      if(.not.(fu .gt. fx))goto 23039
      go to 60
23039 continue
      if(.not.(u .ge. x))goto 23041
      a = x
23041 continue
      if(.not.(u .lt. x))goto 23043
      b = x
23043 continue
      v = w
      fv = fw
      w = x
      fw = fx
      x = u
      fx = fu
      go to 20
60    if(.not.(u .lt. x))goto 23045
      a = u
23045 continue
      if(.not.(u .ge. x))goto 23047
      b = u
23047 continue
      if(.not.(fu .le. fw))goto 23049
      go to 70
23049 continue
      if(.not.(w .eq. x))goto 23051
      go to 70
23051 continue
      if(.not.(fu .le. fv))goto 23053
      go to 80
23053 continue
      if(.not.(v .eq. x))goto 23055
      go to 80
23055 continue
      if(.not.(v .eq. w))goto 23057
      go to 80
23057 continue
      go to 20
70    v = w
      fv = fw
      w = u
      fw = fu
      go to 20
80    v = u
      fv = fu
      go to 20
90    continue 
      spar = x 
      crit = fx
      return
23012 continue
      return
      end
