      double precision function bvalue(t,lent,bcoef,n,k,x,jderiv)

c Calculates value at  x  of  jderiv-th derivative of spline from b-repr.
c The spline is taken to be continuous from the right.
c
C calls  interv
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
c  cated with the aid of  interv . the  k  b-coeffs of  f  relevant for
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
      integer lent, n,k, jderiv
      DOUBLE precision t(*),bcoef(n),x
c     dimension t(n+k)
c  current fortran standard makes it impossible to specify the length of
c  t  precisely without the introduction of otherwise superfluous
c  additional arguments.

C Local Variables
      integer kmax
      parameter(kmax = 20)

      DOUBLE precision aj(kmax),dm(kmax),dp(kmax),fkmj

      integer i,ilo,imk,j,jc,jcmin,jcmax,jj,km1,kmj,mflag,nmi,
     &     jdrvp1


      bvalue = 0.
      if (jderiv .ge. k)                go to 99
c
c  *** find  i  s.t.  1 <= i < n+k  and  t(i) < t(i+1) and
c      t(i) <= x < t(i+1) . if no such i can be found,  x  lies
c      outside the support of  the spline  f  and  bvalue = 0.
c      (the asymmetry in this choice of  i  makes  f  rightcontinuous)
      if( (x.ne.t(n+1)) .or. (t(n+1).ne.t(n+k)) )  go to 700
      i = n
                                        go to 701
  700 call interv ( t, n+k, x, i, mflag )
      if (mflag .ne. 0)                 go to 99
  701 continue
c  *** if k = 1 (and jderiv = 0), bvalue = bcoef(i).
      km1 = k - 1
      if (km1 .gt. 0)                   go to 1
      bvalue = bcoef(i)
                                        go to 99
c
c  *** store the k b-spline coefficients relevant for the knot interval
c     (t(i),t(i+1)) in aj(1),...,aj(k) and compute dm(j) = x - t(i+1-j),
c     dp(j) = t(i+j) - x, j=1,...,k-1 . set any of the aj not obtainable
c     from input to zero. set any t.s not obtainable equal to t(1) or
c     to t(n+k) appropriately.
    1 jcmin = 1
      imk = i - k
      if (imk .ge. 0)                   go to 8
      jcmin = 1 - imk
      do 5 j=1,i
    5    dm(j) = x - t(i+1-j)
      do 6 j=i,km1
         aj(k-j) = 0.
    6    dm(j) = dm(i)
                                        go to 10
    8 do 9 j=1,km1
    9    dm(j) = x - t(i+1-j)
c
   10 jcmax = k
      nmi = n - i
      if (nmi .ge. 0)                   go to 18
      jcmax = k + nmi
      do 15 j=1,jcmax
   15    dp(j) = t(i+j) - x
      do 16 j=jcmax,km1
         aj(j+1) = 0.
   16    dp(j) = dp(jcmax)
                                        go to 20
   18 do 19 j=1,km1
C the following if() happens; e.g. in   pp <- predict(cars.spl, xx)
c-       if( (i+j) .gt. lent) write(6,9911) i+j,lent
c-  9911         format(' i+j, lent ',2(i6,1x))
         dp(j) = t(i+j) - x
   19 continue

c
   20 do 21 jc=jcmin,jcmax
   21    aj(jc) = bcoef(imk + jc)
c
c               *** difference the coefficients  jderiv  times.
      if (jderiv .eq. 0)                go to 30
      do 23 j=1,jderiv
         kmj = k-j
         fkmj = dble(kmj)
         ilo = kmj
         do 23 jj=1,kmj
            aj(jj) = ((aj(jj+1) - aj(jj))/(dm(ilo) + dp(jj)))*fkmj
   23       ilo = ilo - 1
c
c  *** compute value at  x  in (t(i),t(i+1)) of jderiv-th derivative,
c     given its relevant b-spline coeffs in aj(1),...,aj(k-jderiv).

   30 if (jderiv .eq. km1)              go to 39
      jdrvp1 = jderiv + 1
      do 33 j=jdrvp1,km1
         kmj = k-j
         ilo = kmj
         do 33 jj=1,kmj
            aj(jj) = (aj(jj+1)*dm(ilo) + aj(jj)*dp(jj))/(dm(ilo)+dp(jj))
   33       ilo = ilo - 1
   39 bvalue = aj(1)
c
   99 return
      end

      subroutine interv ( xt, lxt, x, left, mflag )

c computes  left = max( i ; 1 <= i <= lxt  .and.  xt(i) <= x )  .
c
c******  i n p u t  ******
c  xt.....a double sequence, of length  lxt , assumed to be nondecreasing
c  lxt.....number of terms in the sequence  xt .
c  x.....the point whose location with respect to the sequence  xt  is
c        to be determined.
c
c******  o u t p u t  ******
c  left, mflag.....both integers, whose value is
c
c   1     -1      if               x <  xt(1)
c   i      0      if   xt(i)  <= x < xt(i+1)
c  lxt     1      if  xt(lxt) <= x
c
c        in particular,  mflag = 0 is the 'usual' case.  mflag .ne. 0
c        indicates that  x  lies outside the halfopen interval
c        xt(1) <= y < xt(lxt) . the asymmetric treatment of the
c        interval is due to the decision to make all pp functions cont-
c        inuous from the right.
c
c******  m e t h o d  ******
c  the program is designed to be efficient in the common situation that
c  it is called repeatedly, with  x  taken from an increasing or decrea-
c  sing sequence. this will happen, e.g., when a pp function is to be
c  graphed. the first guess for  left  is therefore taken to be the val-
c  ue returned at the previous call and stored in the  l o c a l  varia-
c  ble  ilo . a first check ascertains that  ilo < lxt (this is nec-
c  essary since the present call may have nothing to do with the previ-
c  ous call). then, if  xt(ilo) <= x < xt(ilo+1), we set  left =
c  ilo  and are done after just three comparisons.
c     otherwise, we repeatedly double the difference  istep = ihi - ilo
c  while also moving  ilo  and  ihi  in the direction of  x , until
c                      xt(ilo) <= x < xt(ihi) ,
c  after which we use bisection to get, in addition, ilo+1 = ihi .
c  left = ilo  is then returned.
c
      integer left,lxt,mflag,   ihi,ilo,istep,middle
      double precision x,xt(lxt)
      data ilo /1/
c     save ilo  (a valid fortran statement in the new 1977 standard)
      ihi = ilo + 1
      if (ihi .lt. lxt)                 go to 20
         if (x .ge. xt(lxt))            go to 110
         if (lxt .le. 1)                go to 90
         ilo = lxt - 1
         ihi = lxt
c
   20 if (x .ge. xt(ihi))               go to 40
      if (x .ge. xt(ilo))               go to 100
c
c              **** now x < xt(ilo) . decrease  ilo  to capture  x .
   30 istep = 1
   31    ihi = ilo
         ilo = ihi - istep
         if (ilo .le. 1)                go to 35
         if (x .ge. xt(ilo))            go to 50
         istep = istep*2
                                        go to 31
   35 ilo = 1
      if (x .lt. xt(1))                 go to 90
                                        go to 50
c              **** now x >= xt(ihi) . increase  ihi  to capture  x .
   40 istep = 1
   41    ilo = ihi
         ihi = ilo + istep
         if (ihi .ge. lxt)              go to 45
         if (x .lt. xt(ihi))            go to 50
         istep = istep*2
                                        go to 41
   45 if (x .ge. xt(lxt))               go to 110
      ihi = lxt
c
c           **** now xt(ilo) <= x < xt(ihi) . narrow the interval.
   50 middle = (ilo + ihi)/2
      if (middle .eq. ilo)              go to 100
c     note. it is assumed that middle = ilo in case ihi = ilo+1 .
      if (x .lt. xt(middle))            go to 53
         ilo = middle
                                        go to 50
   53    ihi = middle
                                        go to 50
c**** set output and return.
   90 mflag = -1
      left = 1
                                        return
  100 mflag = 0
      left = ilo
                                        return
  110 mflag = 1
      left = lxt
                                        return
      end
