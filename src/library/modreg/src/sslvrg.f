C Output from Public domain Ratfor, version 1.0
      subroutine sslvrg(penalt,dofoff, x,y,w,n, knot,nk, coef,sz,lev,
     *     crit,icrit, spar,ratio, xwy, hs0,hs1,hs2,hs3,
     *     sg0,sg1,sg2,sg3, abd,p1ip,p2ip,ld4,ldnk,info)

      implicit none
c     -------------
      integer n,nk,icrit,ld4,ldnk,info
      DOUBLE precision penalt,dofoff,x(n),y(n),w(n),
     &     knot(nk+4), coef(nk),sz(n),lev(n), crit, spar,ratio,
     *     xwy(nk), hs0(nk),hs1(nk),hs2(nk),hs3(nk),
     *     sg0(nk),sg1(nk),sg2(nk),sg3(nk), abd(ld4,nk),
     &     p1ip(ld4,nk),p2ip(ldnk,nk)

      EXTERNAL bvalue
      double precision bvalue
C local variables
      double precision vnikx(4,1),work(16)
      integer i,icoef,ileft,ilo,j,mflag, lenkno
      double precision lambda,b0,b1,b2,b3,eps, xv,rss,df, sumw

      lenkno=nk+4
      ilo = 1
      eps = .1d-10
C Purpose : Solves the smoothing problem and computes the
C           criterion function (OCV or GCV).
C The coeficients of estimated smooth
      lambda = ratio*16.**(-2. + spar*(6.))
      do 1 i=1,nk
         coef(i) = xwy(i)
 1    continue
      do   2 i=1,nk
         abd(4,i) = hs0(i)+lambda*sg0(i)
 2    continue

      do   4 i=1,(nk-1)
         abd(3,i+1) = hs1(i)+lambda*sg1(i)
 4    continue

      do   6 i=1,(nk-2)
         abd(2,i+2) = hs2(i)+lambda*sg2(i)
 6    continue

      do   8 i=1,(nk-3)
         abd(1,i+3) = hs3(i)+lambda*sg3(i)
 8    continue

      call dpbfa(abd,ld4,nk,3,info)
      if(info.ne.0)then
         return
      endif
      call dpbsl(abd,ld4,nk,3,coef)
C     Value of smooth at the data points
      icoef = 1
      do 12 i=1,n
         xv = x(i)
C        sz(i) = bvalu (knot,       coef, nk,4,0,xv,icoef,work(1)) 
         sz(i) = bvalue(knot,lenkno,coef, nk,4,xv,0)
C     Compute the criterion function if requested
 12   continue

      if(icrit.eq.0)then
         return
      else
C     Ordinary or Generalized CV
C     Get Leverages First
         call sinerp(abd,ld4,nk,p1ip,p2ip,ldnk,0)
         do    16 i=1,n
            xv = x(i)
C     call intrv(knot(1),(nk+1),xv,ilo,ileft,mflag)
            call interv(knot(1),(nk+1),xv,ileft,mflag)
            if(mflag.eq.-1)then
               ileft = 4
               xv = knot(4)+eps
            endif
            if(mflag.eq.1)then
               ileft = nk
               xv = knot(nk+1)-eps
            endif
            j=ileft-3
C     call bspvd(knot,4,1,xv,ileft,4,vnikx,work)
            call bsplvd(knot,lenkno,4,xv,ileft,work,vnikx,1)
            b0=vnikx(1,1)
            b1=vnikx(2,1)
            b2=vnikx(3,1)
            b3=vnikx(4,1)
            lev(i) = (
     &              p1ip(4,j)*b0**2   + 2.*p1ip(3,j)*b0*b1 +
     *           2.*p1ip(2,j)*b0*b2   + 2.*p1ip(1,j)*b0*b3 +
     *              p1ip(4,j+1)*b1**2 + 2.*p1ip(3,j+1)*b1*b2 +
     *           2.*p1ip(2,j+1)*b1*b3 +    p1ip(4,j+2)*b2**2 +
     &           2.*p1ip(3,j+2)*b2*b3 +    p1ip(4,j+3)*b3**2
     &           )*w(i)**2
C     Evaluate Criterion
 16      continue

         if(icrit.eq.1)then
C     Generalized CV
            rss= 0d0
            df = 0d0
            sumw=0d0
            do    24 i=1,n
               rss = rss + ((y(i)-sz(i))*w(i))**2
 24         continue
            do    26 i=1,n
               df = df + lev(i)
C     do i=1,n { sumw  = sumw  + w(i)}
 26         continue

            crit = (rss/n)/((1d0-(dofoff+penalt*df)/n)**2)
C     crit = (rss/n)/((1d0-(dofoff+penalt*df)/sumw)**2)
         else
            if(icrit.eq.2)then
C     Ordinary CV
               crit = 0d0
               do    30 i=1,n
                  crit = crit + (((y(i)-sz(i))*w(i))/(1-lev(i)))**2
 30            continue
               crit=crit/n
            else
C     df matching
               crit=0d0
               do    32 i=1,n
                  crit=crit+lev(i)
 32            continue
               crit=3+(dofoff-crit)**2
            endif
         endif
         return
      endif
      end

