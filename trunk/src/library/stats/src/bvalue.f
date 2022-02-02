      double precision function bvalue(t,bcoef,n,k,x,jderiv)

c Calculates value at  x  of  jderiv-th derivative of spline from B-repr.
c The spline is taken to be continuous from the right.
c
C calls  interv()  (from ../../../appl/interv.c )
c
c******  i n p u t ******
c  t, bcoef, n, k......forms the b-representation of the spline  f  to
c        be evaluated. specifically,
c  t.....knot sequence, of length  n+k, assumed nondecreasing.
c  bcoef.....b-coefficient sequence, of length  n .
c  n.....length of  bcoef  and dimension of s(k,t),
c        a s s u m e d  positive .
c  k.....order of the spline .
c
c  w a r n i n g . . .   the restriction  k <= kmax (=20)  is imposed
c        arbitrarily by the dimension statement for  aj, dm, dm  below,
c        but is  n o w h e r e  c h e c k e d  for.
c  however in R, this is only called from bvalus() with k=4 anyway!
c
c  x.....the point at which to evaluate .
c  jderiv.....integer giving the order of the derivative to be evaluated
c        a s s u m e d  to be zero or positive.
c
c******  o u t p u t  ******
c  bvalue.....the value of the (jderiv)-th derivative of  f  at  x .
c
c******  m e t h o d  ******
c     the nontrivial knot interval  (t(i),t(i+1))  containing  x  is lo-
c  cated with the aid of  interv(). the  k  b-coeffs of  f  relevant for
c  this interval are then obtained from  bcoef (or taken to be zero if
c  not explicitly available) and are then differenced  jderiv  times to
c  obtain the b-coeffs of  (d^jderiv)f  relevant for that interval.
c  precisely, with  j = jderiv, we have from x.(12) of the text that
c
c     (d^j)f  =  sum ( bcoef(.,j)*b(.,k-j,t) )
c
c  where
c                   / bcoef(.),                     ,  j .eq. 0
c                   /
c    bcoef(.,j)  =  / bcoef(.,j-1) - bcoef(.-1,j-1)
c                   / ----------------------------- ,  j > 0
c                   /    (t(.+k-j) - t(.))/(k-j)
c
c     then, we use repeatedly the fact that
c
c    sum ( a(.)*b(.,m,t)(x) )  =  sum ( a(.,x)*b(.,m-1,t)(x) )
c  with
c                 (x - t(.))*a(.) + (t(.+m-1) - x)*a(.-1)
c    a(.,x)  =    ---------------------------------------
c                 (x - t(.))      + (t(.+m-1) - x)
c
c  to write  (d^j)f(x)  eventually as a linear combination of b-splines
c  of order  1 , and the coefficient for  b(i,1,t)(x)  must then
c  be the desired number  (d^j)f(x). (see x.(17)-(19) of text).
c
C Arguments
      integer n,k, jderiv
      DOUBLE precision t(*),bcoef(n),x
c     dimension t(n+k)
c  current fortran standard makes it impossible to specify the length of
c  t  precisely without the introduction of otherwise superfluous
c  additional arguments.

C Local Variables
      integer kmax
      parameter(kmax = 20)

      DOUBLE precision aj(kmax),dm(kmax),dp(kmax),fkmj

      integer i,ilo,imk,j,jc,jcmin,jcmax,jj,km1,kmj,mflag,nmi, jdrvp1
c
      integer interv
      external interv

c     initialize
      data i/1/

      bvalue = 0.d0
      if (jderiv .ge. k) go to 99
c
c  *** find  i  s.t.  1 <= i < n+k  and  t(i) < t(i+1) and
c      t(i) <= x < t(i+1) . if no such i can be found,  x  lies
c      outside the support of the spline  f and bvalue = 0.
c  {this case is handled in the calling R code}
c      (the asymmetry in this choice of  i  makes  f  rightcontinuous)
      if( (x.ne.t(n+1)) .or. (t(n+1).ne.t(n+k)) ) then
         i = interv ( t, n+k, x, 0, 0, i, mflag)
         if (mflag .ne. 0) then
            call rwarn('bvalue()  mflag != 0: should never happen!')
            go to 99
         endif
      else
         i = n
      endif

c  *** if k = 1 (and jderiv = 0), bvalue = bcoef(i).
      km1 = k - 1
      if (km1 .le. 0) then
         bvalue = bcoef(i)
         go to 99
      endif
c
c  *** store the k b-spline coefficients relevant for the knot interval
c     (t(i),t(i+1)) in aj(1),...,aj(k) and compute dm(j) = x - t(i+1-j),
c     dp(j) = t(i+j) - x, j=1,...,k-1 . set any of the aj not obtainable
c     from input to zero. set any t.s not obtainable equal to t(1) or
c     to t(n+k) appropriately.
      jcmin = 1
      imk = i - k
      if (imk .ge. 0) then
         do 9 j=1,km1
            dm(j) = x - t(i+1-j)
 9       continue
      else
         jcmin = 1 - imk
         do 5 j=1,i
            dm(j) = x - t(i+1-j)
 5       continue
         do 6 j=i,km1
            aj(k-j) = 0.d0
            dm(j) = dm(i)
 6       continue
      endif
c
      jcmax = k
      nmi = n - i
      if (nmi .ge. 0) then
         do 19 j=1,km1
C     the following if() happens; e.g. in   pp <- predict(cars.spl, xx)
c     -       if( (i+j) .gt. lent) write(6,9911) i+j,lent
c     -  9911         format(' i+j, lent ',2(i6,1x))
            dp(j) = t(i+j) - x
 19      continue
      else
         jcmax = k + nmi
         do 15 j=1,jcmax
            dp(j) = t(i+j) - x
 15      continue
         do 16 j=jcmax,km1
            aj(j+1) = 0.d0
            dp(j) = dp(jcmax)
 16      continue
      endif

c
      do 21 jc=jcmin,jcmax
         aj(jc) = bcoef(imk + jc)
 21   continue
c
c               *** difference the coefficients  jderiv  times.
      if (jderiv .ge. 1) then
         do 23 j=1,jderiv
            kmj = k-j
            fkmj = dble(kmj)
            ilo = kmj
            do 24 jj=1,kmj
               aj(jj) = ((aj(jj+1) - aj(jj))/(dm(ilo) + dp(jj)))*fkmj
               ilo = ilo - 1
 24         continue
 23      continue
      endif

c
c  *** compute value at  x  in (t(i),t(i+1)) of jderiv-th derivative,
c     given its relevant b-spline coeffs in aj(1),...,aj(k-jderiv).

      if (jderiv .ne. km1) then
         jdrvp1 = jderiv + 1
         do 33 j=jdrvp1,km1
            kmj = k-j
            ilo = kmj
            do 34 jj=1,kmj
               aj(jj) = (aj(jj+1)*dm(ilo) + aj(jj)*dp(jj)) /
     *              (dm(ilo)+dp(jj))
               ilo = ilo - 1
 34         continue
 33      continue
      endif

      bvalue = aj(1)
c
   99 return
      end
