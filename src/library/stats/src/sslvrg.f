C Output from Public domain Ratfor, version 1.0
      subroutine sslvrg(penalt,dofoff,x,y,w,ssw, n, knot,nk,coef,
     *     sz,lev, crit,icrit, lambda, xwy, hs0,hs1,hs2,hs3,
     *     sg0,sg1,sg2,sg3, abd,p1ip,p2ip,ld4,ldnk,info)

C Purpose :
C       Compute smoothing spline for smoothing parameter lambda
C       and compute one of three `criteria' (OCV , GCV , "df match").
C See comments in ./sbart.c from which this is called

      integer n,nk,icrit,ld4,ldnk,info
      DOUBLE precision penalt,dofoff,x(n),y(n),w(n),ssw,
     &     knot(nk+4), coef(nk),sz(n),lev(n), crit, lambda,
     *     xwy(nk), hs0(nk),hs1(nk),hs2(nk),hs3(nk),
     *     sg0(nk),sg1(nk),sg2(nk),sg3(nk), abd(ld4,nk),
     &     p1ip(ld4,nk),p2ip(ldnk,nk)

      EXTERNAL bvalue
      double precision bvalue
C local variables
      double precision vnikx(4,1),work(16)
      integer i,ileft,j,mflag, lenkno
      double precision b0,b1,b2,b3,eps, xv,rss,df, sumw
c
      integer interv
      external interv

      lenkno = nk+4
      ileft = 1
      eps = 1d-11

C compute the coefficients coef() of estimated smooth

      do 1 i=1,nk
         coef(i) = xwy(i)
         abd(4,i) = hs0(i)+lambda*sg0(i)
 1    continue

      do 4 i=1,(nk-1)
         abd(3,i+1) = hs1(i)+lambda*sg1(i)
 4    continue

      do 6 i=1,(nk-2)
         abd(2,i+2) = hs2(i)+lambda*sg2(i)
 6    continue

      do 8 i=1,(nk-3)
         abd(1,i+3) = hs3(i)+lambda*sg3(i)
 8    continue
c     factorize banded matrix abd:
      call dpbfa(abd,ld4,nk,3,info)
      if(info.ne.0) then
C        matrix could not be factorized -> ier := info
         return
      endif
c     solve linear system (from factorize abd):
      call dpbsl(abd,ld4,nk,3,coef)

C     Value of smooth at the data points
      do 12 i=1,n
         xv = x(i)
         sz(i) = bvalue(knot,coef,nk,4,xv,0)
 12   continue

C     Compute the criterion function if requested

      if(icrit .eq. 0)then
         return
      else
C --- Ordinary or Generalized CV or "df match" ---

C     Get Leverages First
         call sinerp(abd,ld4,nk,p1ip,p2ip,ldnk,0)
         do 16 i=1,n
            xv = x(i)
            ileft = interv(knot(1), nk+1, xv, 0,0, ileft, mflag)
            if(mflag .eq. -1) then
               ileft = 4
               xv = knot(4)+eps
            else if(mflag .eq. 1) then
               ileft = nk
               xv = knot(nk+1) - eps
            endif
            j=ileft-3
C           call bspvd(knot,4,1,xv,ileft,4,vnikx,work)
            call bsplvd(knot,lenkno,4,xv,ileft,work,vnikx,1)
            b0=vnikx(1,1)
            b1=vnikx(2,1)
            b2=vnikx(3,1)
            b3=vnikx(4,1)
            lev(i) = (
     &              p1ip(4,j)*b0**2   + 2.d0*p1ip(3,j)*b0*b1 +
     *           2.d0*p1ip(2,j)*b0*b2   + 2.d0*p1ip(1,j)*b0*b3 +
     *              p1ip(4,j+1)*b1**2 + 2.d0*p1ip(3,j+1)*b1*b2 +
     *           2.d0*p1ip(2,j+1)*b1*b3 +    p1ip(4,j+2)*b2**2 +
     &           2.d0*p1ip(3,j+2)*b2*b3 +    p1ip(4,j+3)*b3**2
     &           )*w(i)**2
 16      continue

C     Evaluate Criterion

         if(icrit .eq. 1)then
C     Generalized CV
            rss = ssw
            df = 0d0
            sumw = 0d0
c       w(i) are sqrt( wt[i] ) weights scaled in ../R/smspline.R such
c       that sumw =  number of observations with w(i) > 0
            do 24 i=1,n
               rss = rss + ((y(i)-sz(i))*w(i))**2
               df = df + lev(i)
               sumw = sumw + w(i)**2
 24         continue

            crit = (rss/sumw)/((1d0-(dofoff + penalt*df)/sumw)**2)
c            call dblepr("spar", 4, spar, 1)
c            call dblepr("crit", 4, crit, 1)

         else if(icrit .eq. 2) then
C     Ordinary CV
               crit = 0d0
               do 30 i = 1,n
                  crit = crit + (((y(i)-sz(i))*w(i))/(1-lev(i)))**2
 30            continue
               crit = crit/n
c            call dblepr("spar", 4, spar, 1)
c            call dblepr("crit", 4, crit, 1)
            else
C     df matching
            crit = 0d0
            do 32 i=1,n
               crit = crit+lev(i)
 32         continue
            crit = 3 + (dofoff-crit)**2
         endif
         return
      endif
C     Criterion evaluation
      end
