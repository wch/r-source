C  sbart() : The cubic spline smoother
C  -------
C Calls	 sgram	(sg0,sg1,sg2,sg3,knot,nk)
C	 stxwx	(xs,ys,ws,n,knot,nk,xwy,hs0,hs1,hs2,hs3)
C	 sslvrg (penalt,dofoff,xs,ys,ws,ssw,n,knot,nk,	coef,sz,lev,crit,icrit,
C		 lambda, xwy, hs0,hs1,hs2,hs3, sg0,sg1,sg2,sg3,
C		 abd,p1ip,p2ip,ld4,ldnk,ier)
C
C is itself called from	 qsbart() [./qsbart.f]	 which has only one work array
C
      subroutine sbart(penalt,dofoff,xs,ys,ws,ssw,n,knot,nk,
     &	   coef,sz,lev, crit,
     &	   icrit, spar, ispar, iter,
     &	   lspar, uspar, tol, eps,
     &	   isetup, xwy,
     &	   hs0,hs1,hs2,hs3, sg0,sg1,sg2,sg3,
     &	   abd,p1ip,p2ip,ld4,ldnk,ier)

C A Cubic B-spline Smoothing routine.
C
C  The algorithm minimises:
C
C      (1/n) * sum ws(i)^2 * (ys(i)-sz(i))^2 + lambda* int ( s"(x) )^2 dx
C
C  lambda is a function of the spar which is assumed to be between 0 and 1
C
C INPUT
C -----
C  penalt	A penalty > 1 to be used in the gcv criterion
C  dofoff	either `df.offset' for GCV or `df' (to be matched).
C   n		number of data points
C  ys(n)	vector of length n containing the observations
C  ws(n)	vector containing the weights given to each data point
C  xs(n)	vector containing the ordinates of the observations
C  ssw          `centered weighted sum of y^2'
C   nk		number of b-spline coefficients to be estimated
C		nk <= n+2
C  knot(nk+4)	vector of knot points defining the cubic b-spline basis.
C		To obtain full cubic smoothing splines one might
C		have (provided the xs-values are strictly increasing)
C  spar		penalised likelihood smoothing parameter
C  ispar	indicating if spar is supplied (ispar=1) or to be estimated
C  lspar, uspar lower and upper values for spar search;  0.,1. are good values
C  tol, eps	used in Golden Search routine
C  isetup	setup indicator [initially 0
C  icrit	indicator saying which cross validation score is to be computed
C     		0: none ;  1: GCV ;  2: CV ;  3: 'df matching'
C  ld4		the leading dimension of abd (ie ld4=4)
C  ldnk		the leading dimension of p2ip (not referenced)

C OUTPUT
C ------
C   coef(nk)	vector of spline coefficients
C   sz(n)	vector of smoothed z-values
C   lev(n)	vector of leverages
C   crit	either ordinary or generalized CV score
C   spar        if ispar != 1
C   lspar       == lambda (a function of spar and the design)
C   iter	number of iterations needed for spar search (if ispar != 1)
C   ier		error indicator
C		ier = 0 ___  everything fine
C		ier = 1 ___  spar too small or too big
C				 problem in cholesky decomposition

C Working arrays/matrix
C   xwy			X'Wy
C   hs0,hs1,hs2,hs3	the diagonals of the X'WX matrix
C   sg0,sg1,sg2,sg3	the diagonals of the Gram matrix SIGMA
C   abd(ld4,nk)		[ X'WX + lambda*SIGMA ] in diagonal form
C   p1ip(ld4,nk)	inner products between columns of L inverse
C   p2ip(ldnk,nk)	all inner products between columns of L inverse
C			where  L'L = [X'WX + lambda*SIGMA]  NOT REFERENCED

      integer n, nk,isetup,icrit,ispar,iter, ld4,ldnk,ier
      double precision penalt,dofoff,xs(n),ys(n),ws(n),ssw,
     &	   knot(nk+4), coef(nk),sz(n),lev(n),
     &     crit,spar,lspar,uspar,tol,eps,
     &	   xwy(nk),hs0(nk),hs1(nk),hs2(nk),hs3(nk),
     &	   sg0(nk),sg1(nk),sg2(nk),sg3(nk),
     &	   abd(ld4,nk), p1ip(ld4,nk),p2ip(ldnk,nk)
C Local variables
      double precision t1,t2,ratio, a,b,c,d,e, xm, p, q,
     &	   r, tol1,tol2, u,v,w, fu,fv,fw,fx,x, ax,bx
      integer i, maxit
c#ifdef DEBUG_sbart
      double precision ab(3)
c#endif

      common /XXXsbart/q
c
c     unnecessary initializations to keep g77 -Wall happy
c
      d = 0.
      u = 0.
      ratio = 1.

C  Compute SIGMA, X' W^2 X, X' W^2 z, trace ratio, s0, s1.

C		SIGMA	  -> sg0,sg1,sg2,sg3
C		X' W^2 X  -> hs0,hs1,hs2,hs3
C		X' W^2 Z  -> xwy

C trevor fixed this 4/19/88
C Note: sbart uses the square of the weights
C the following rectifies that
      if(n .ge. 1) then
	 do 5 i=1,n
	    if(ws(i).gt.0)then
	       ws(i)=sqrt(ws(i))
	    endif
 5	 continue
      endif

      if(isetup.eq.0) then
c	 SIGMA[i,j] := Int  B''(i,t) B''(j,t) dt  {B(k,.) = k-th B-spline}
	 call sgram(sg0,sg1,sg2,sg3, knot,nk)
	 call stxwx(xs,ys,ws,n,knot,nk,xwy,hs0,hs1,hs2,hs3)
C        Compute ratio :=  tr(X' W^2 X) / tr(SIGMA)
	 t1=0.
	 t2=0.
	 do 7 i=3,nk-3
	    t1 = t1 + hs0(i)
	    t2 = t2 + sg0(i)
 7	 continue
	 ratio = t1/t2
	 isetup = 1
      endif

C     Compute estimate

      if(ispar.eq.1) then
C        Value of spar supplied
         lspar = ratio * 16.**(-2. + spar * (6.))
	 call sslvrg(penalt,dofoff,xs,ys,ws,ssw,n,knot,nk,
     &	      coef,sz,lev,crit,icrit,
     &	      lspar, xwy, hs0,hs1,hs2,hs3, sg0,sg1,sg2,sg3,
     &	      abd,p1ip,p2ip,ld4,ldnk,ier)

C        got through check 2

      else
c     ---- spar not supplied --> compute it ! -------------------------

C     Use Forsythe Malcom and Moler routine to minimise criterion
C     f denotes the value of the criterion
C
C     an approximation	x  to the point where	f  attains a minimum  on
C     the interval  (ax,bx)  is determined.
C
	 ax = lspar
	 bx = uspar

C INPUT
C
C  ax	 left endpoint of initial interval
C  bx	 right endpoint of initial interval
C  f	 function subprogram which evaluates  f(x)  for any  x
C	 in the interval  (ax,bx)
C  tol	 desired length of the interval of uncertainty of the final
C	 result ( >= 0 )

C OUTPUT
C
C  fmin	 abcissa approximating the point where	f  attains a minimum


C      The method used is a combination of  golden  section  search  and
C  successive parabolic interpolation.	convergence is never much slower
C  than	 that  for  a  fibonacci search.  if  f	 has a continuous second
C  derivative which is positive at the minimum (which is not  at  ax  or
C  bx),	 then  convergence  is	superlinear, and usually of the order of
C  about  1.324....
C      the function  f	is never evaluated at two points closer together
C  than	 eps*abs(fmin) + (tol/3), where eps is	approximately the square
C  root	 of  the  relative  machine  precision.	  if   f   is a unimodal
C  function and the computed values of	 f   are  always  unimodal  when
C  separated by at least  eps*abs(x) + (tol/3), then  fmin  approximates
C  the abcissa of the global minimum of	 f  on the interval  ax,bx  with
C  an error less than  3*eps*abs(fmin) + tol.  if   f	is not unimodal,
C  then fmin may approximate a local, but perhaps non-global, minimum to
C  the same accuracy.
C      this function subprogram is a slightly modified	version	 of  the
C  algol  60 procedure	localmin  given in richard brent, algorithms for
C  minimization without derivatives, prentice - hall, inc. (1973).
C
C
C      DOUBLE precision	 a,b,c,d,e,eps,xm,p,q,r,tol1,tol2,u,v,w
C      DOUBLE precision	 fu,fv,fw,fx,x
C
C  c is the squared inverse of the golden ratio
C
	 c = 0.5*(3. - sqrt(5d0))
C
C  eps is approximately the square root of the relative machine
C  precision.
C
c-	 eps = 1e0
c- 10	 eps = eps/2e0
c-	 tol1 = 1e0 + eps
c-	 if (tol1 .gt. 1e0) go to 10
c-	 eps = sqrt(eps)
c R Version <= 1.3.x had  -- now eps is passed as argument
c sqrt(5.954 e-8) = 0.00244 :
c	 eps = .000244

C
C  initialization
C
         maxit = iter
         iter = 0
	 a = ax
	 b = bx
	 v = a + c*(b - a)
	 w = v
	 x = v
	 e = 0.0
	 spar = x
         lspar = ratio * 16.**(-2. + spar * (6.))
	 call sslvrg(penalt,dofoff,xs,ys,ws,ssw,n,knot,nk,
     &	      coef,sz,lev,crit,icrit,
     &	      lspar, xwy, hs0,hs1,hs2,hs3, sg0,sg1,sg2,sg3,
     &	      abd,p1ip,p2ip,ld4,ldnk,ier)
	 fx = crit
	 fv = fx
	 fw = fx
C
C  main loop starts here
C  ---------
 20	 xm = 0.5*(a + b)
	 tol1 = eps*abs(x) + tol/3.
	 tol2 = 2.*tol1
         iter = iter + 1

c#ifdef DEBUG_sbart
         if(ispar .lt. 0) then
            if(iter .eq. 1) then
               call intpr ('sbart iterations :',-1, iter, 0)
               if(icrit .eq. 1) then
		  call intpr('      x      GCV         b - a',-1, 0,0)
	       else if(icrit .eq. 2) then
		  call intpr('      x       CV         b - a',-1, 0,0)
	       else if(icrit .eq. 3) then
		  call intpr('      x     (df0-df)^2   b - a',-1, 0,0)
	       else
		  call intpr('      x      ?fx?        b - a',-1,  0,0)
	       endif
	       call intpr   ('    ---------------------------',-1, 0,0)
	    endif
	    ab(1) = x
	    ab(2) = fx
c      there's cancellation in (3 + small) - 3, but it's still more informative:
	    if(icrit .eq. 3) ab(2) = ab(2) - 3.
	    ab(3) = b - a
	    call dblepr(' ', 0, ab, 3)
	 endif
c#endif

C
C  Check the (somewhat peculiar) stopping criterion:
C  note that the RHS is negative as long as the interval [a,b] is not small:
         if(abs(x - xm) .le. (tol2 - 0.5*(b - a)) .or. iter .gt. maxit)
     &        go to 990
C
C is golden-section necessary
C
	 if(abs(e) .le. tol1)	   go to 40
C
C  fit parabola
C
	 r = (x - w)*(fx - fv)
	 q = (x - v)*(fx - fw)
	 p = (x - v)*q - (x - w)*r
	 q = 2.0*(q - r)
	 if (q .gt. 0d0) p = -p
	 q = abs(q)
	 r = e
	 e = d
C
C  is parabola acceptable?  Otherwise do golden-section
C
	 if (abs(p) .ge. abs(0.5*q*r)) go to 40
	 if (q .eq. 0d0) go to 40
C     above line added by BDR; [the abs(.) >= abs() = 0 should have branched..]
C     COMMON above ensures q is NOT a register variable
	 if (p .le. q*(a - x)) go to 40
	 if (p .ge. q*(b - x)) go to 40
C
C  a parabolic interpolation step
C
	 d = p/q
	 u = x + d
C
C  f must not be evaluated too close to ax or bx
C
	 if ((u - a) .lt. tol2) d = sign(tol1, xm - x)
	 if ((b - u) .lt. tol2) d = sign(tol1, xm - x)
	 go to 50
C	 --------
C
C  a golden-section step
C
   40	 if (x .ge. xm) e = a - x
	 if (x .lt. xm) e = b - x
	 d = c*e
C
C  f must not be evaluated too close to x
C
   50	 if (abs(d) .ge. tol1) u = x + d
	 if (abs(d) .lt. tol1) u = x + sign(tol1, d)

	 spar = u
         lspar = ratio * 16.**(-2. + spar * (6.))
	 call sslvrg(penalt,dofoff,xs,ys,ws,ssw,n,knot,nk,
     &	      coef,sz,lev,crit,icrit,
     &	      lspar, xwy, hs0,hs1,hs2,hs3, sg0,sg1,sg2,sg3,
     &	      abd,p1ip,p2ip,ld4,ldnk,ier)
	 fu = crit
C
C  update  a, b, v, w, and x
C
	 if (fu .le. fx) then
	    if (u .ge. x) a = x
	    if (u .lt. x) b = x
	    v = w
	    fv = fw
	    w = x
	    fw = fx
	    x = u
	    fx = fu
	    go to 20
	 endif
	 if(u .lt. x) then
	    a = u
	 else
	    b = u
	 endif
	 if (fu .le. fw) go to 70
	 if (w .eq. x) go to 70
	 if (fu .le. fv) go to 80
	 if (v .eq. x) go to 80
	 if (v .eq. w) go to 80
	 go to 20
 70	 v = w
	 fv = fw
	 w = u
	 fw = fu
	 go to 20
 80	 v = u
	 fv = fu
	 go to 20
C
C  end of main loop
C
 990	 continue
	 spar = x
	 crit = fx
      endif

      return
      end

