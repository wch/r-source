CCC--- FIXME:  Instead of   subroutines	  forslv  and  baksol
CCC--- =====   Use ./dtrsl.f  !! [see  dbksl() in ./S_compat.c !]
CCC		   ~~~~~~~~~
CCC    subroutines  mvmlt[lsu] and sclmul  should be REPLACED by BLAS ones!
CCC
CCC--- choldc(nr,n,a,diagmx,tol,addmax)	 is ``choleski + tolerance''
CCC    ------ 
CCC    it should make use of BLAS routines as [linkpack's dpofa!]

      
c     subroutine fdhess
c
c	this subroutine calculates a numerical approximation to the upper
c	triangular portion of the second derivative matrix (the hessian).
c	algorithm a5.6.2 from dennis and schnable (1983), numerical methods
c	for unconstrained optimization and nonlinear equations,
c	prentice-hall, 321-322.
c
c	programmed by richard h. jones, january 11, 1989
c
c	input to subroutine
c
c	   n.....the number of parameters
c	   x.....vector of parameter values
c	   fval..double precision value of function at x
c	   fun...a function provided by the user which must be declared as
c		 external in the calling program.  its call must be of the
c		 call fun(n,x,fval) where fval is the computed value of the
c		 function
c	   nfd...first dimension of h in the calling program
c
c	output from subroutine
c
c	    h.....an n by n matrix of the approximate hessian
c
c	work space
c
c	    step....a real array of length n
c	    f.......a double precision array of length n
c
      subroutine fdhess(n,x,fval,fun,h,nfd,step,f,ndigit,typx)
      implicit none

      external fun
      integer n,nfd,ndigit
      double precision x(n),fval, h(nfd,nfd),step(n),f(n),typx(n)
c
      double precision eta,tempi,tempj,fii,fij
      integer i,j
      eta=(10.0d0**(-ndigit))**(1.0d0/3.0d0)
      do 10 i=1,n
	 step(i)=eta*dmax1(x(i),typx(i))
	 if(typx(i).lt.0.0d0)step(i)=-step(i)
	 tempi=x(i)
	 x(i)=x(i)+step(i)
	 step(i)=x(i)-tempi
	 call fun(n,x,f(i))
	 x(i)=tempi
   10 continue
      do 30 i=1,n
	 tempi=x(i)
	 x(i)=x(i)+2.0d0*step(i)
	 call fun(n,x,fii)
	 h(i,i)=((fval-f(i))+(fii-f(i)))/(step(i)*step(i))
	 x(i)=tempi+step(i)
	 if(i.lt.n)then
	    do 20 j=i+1,n
	       tempj=x(j)
	       x(j)=x(j)+step(j)
	       call fun(n,x,fij)
	       h(i,j)=((fval-f(i))+(fij-f(j)))/(step(i)*step(j))
	       x(j)=tempj
   20	    continue
	 endif
	 x(i)=tempi
   30 continue
      return
      end


c	subroutine baksol
c
c	purpose
c
c	solve  ax=b  where a is upper triangular matrix.
c	note that a is input as a lower triangular matrix and
c	that this routine takes its transpose implicitly.
c
c	parameters
c
c	nr	     --> row dimension of matrix
c	n	     --> dimension of problem
c	a(n,n)	     --> lower triangular matrix (preserved)
c	x(n)	    <--	 solution vector
c	b(n)	     --> right-hand side vector
c
c	note
c
c	if b is no longer required by calling routine,
c	then vectors b and x may share the same storage.
c
cstatic
      subroutine baksol(nr,n,a,x,b)
      implicit double precision (a-h,o-z)
      dimension a(nr,1),x(n),b(n)
c
c	solve (l-transpose)x=b. (back solve)
c
      i=n
      x(i)=b(i)/a(i,i)
      if(n.eq.1) return
   30 ip1=i
      i=i-1
      sum=0.0d0
      do 40 j=ip1,n
	sum=sum+a(j,i)*x(j)
   40 continue
      x(i)=(b(i)-sum)/a(i,i)
      if(i.gt.1) go to 30
      return
      end


c	subroutine chlhsn
c
c	purpose
c
c	find the l(l-transpose) [written ll+] decomposition of the perturbed
c	model hessian matrix a+mu*i(where mu\0 and i is the identity matrix)
c	which is safely positive definite.  if a is safely positive definite
c	upon entry, then mu=0.
c
c	parameters
c
c	nr	     --> row dimension of matrix
c	n	     --> dimension of problem
c	a(n,n)	    <--> on entry; "a" is model hessian (only lower
c			 triangular part and diagonal stored)
c			 on exit:  a contains l of ll+ decomposition of
c			 perturbed model hessian in lower triangular
c			 part and diagonal and contains hessian in upper
c			 triangular part and udiag
c	epsm	     --> machine epsilon
c	sx(n)	     --> diagonal scaling matrix for x
c	udiag(n)    <--	 on exit: contains diagonal of hessian
c
c	internal variables
c
c	tol		 tolerance
c	diagmn		 minimum element on diagonal of a
c	diagmx		 maximum element on diagonal of a
c	offmax		 maximum off-diagonal element of a
c	offrow		 sum of off-diagonal elements in a row of a
c	evmin		 minimum eigenvalue of a
c	evmax		 maximum eigenvalue of a
c
c	description
c
c	1. if "a" has any negative diagonal elements, then choose mu>0
c	such that the diagonal of a:=a+mu*i is all positive
c	with the ratio of its smallest to largest element on the
c	order of sqrt(epsm).
c
c	2. "a" undergoes a perturbed cholesky decomposition which
c	results in an ll+ decomposition of a+d, where d is a
c	non-negative diagonal matrix which is implicitly added to
c	"a" during the decomposition if "a" is not positive definite.
c	"a" is retained and not changed during this process by
c	copying l into the upper triangular part of "a" and the
c	diagonal into udiag.  then the cholesky decomposition routine
c	is called.  on return, addmax contains maximum element of d.
c
c	3. if addmax=0, "a" was positive definite going into step 2
c	and return is made to calling program.	otherwise,
c	the minimum number sdd which must be added to the
c	diagonal of a to make it safely strictly diagonally dominant
c	is calculated.	since a+addmax*i and a+sdd*i are safely
c	positive definite, choose mu=min(addmax,sdd) and decompose
c	a+mu*i to obtain l.
c
cstatic
      subroutine chlhsn(nr,n,a,epsm,sx,udiag)
      implicit double precision (a-h,o-z)
      dimension a(nr,1),sx(n),udiag(n)
c
c	scale hessian
c	pre- and post- multiply "a" by inv(sx)
c
      do 20 j=1,n
	do 10 i=j,n
	  a(i,j)=a(i,j)/(sx(i)*sx(j))
   10	continue
   20 continue
c
c	step1
c	-----
c	note:  if a different tolerance is desired throughout this
c	algorithm, change tolerance here:
c
      tol=sqrt(epsm)
c
      diagmx=a(1,1)
      diagmn=a(1,1)
      if(n.eq.1) go to 35
      do 30 i=2,n
	if(a(i,i).lt.diagmn) diagmn=a(i,i)
	if(a(i,i).gt.diagmx) diagmx=a(i,i)
   30 continue
   35 posmax=max(diagmx,0.0d0)
c
c	diagmn .le. 0
c
      if(diagmn.gt.posmax*tol) go to 100
c     if(diagmn.le.posmax*tol)
c     then
	amu=tol*(posmax-diagmn)-diagmn
	if(amu.ne.0.0d0) go to 60
c	if(amu.eq.0.0d0)
c	then
c
c	find largest off-diagonal element of a
	  offmax=0.0d0
	  if(n.eq.1) go to 50
	  do 45 i=2,n
	    im1=i-1
	    do 40 j=1,im1
	      if(abs(a(i,j)).gt.offmax) offmax=abs(a(i,j))
   40	    continue
   45	  continue
   50	  amu=offmax
	  if(amu.ne.0.0d0) go to 55
c	  if(amu.eq.0.0d0)
c	  then
	    amu=1.0d0
	    go to 60
c	  else
   55	    amu=amu*(1.0d0+tol)
c	  endif
c	endif
c
c	a=a + mu*i
c
   60	do 65 i=1,n
	  a(i,i)=a(i,i)+amu
   65	continue
	diagmx=diagmx+amu
c     endif
c
c	step2
c
c	copy lower triangular part of "a" to upper triangular part
c	and diagonal of "a" to udiag
c
  100 continue
      do 110 j=1,n
	udiag(j)=a(j,j)
	if(j.eq.n) go to 110
	jp1=j+1
	do 105 i=jp1,n
	  a(j,i)=a(i,j)
  105	continue
  110 continue
c
      call choldc(nr,n,a,diagmx,tol,addmax)
c
c
c	step3
c
c	if addmax=0, "a" was positive definite going into step 2,
c	the ll+ decomposition has been done, and we return.
c	otherwise, addmax>0.  perturb "a" so that it is safely
c	diagonally dominant and find ll+ decomposition
c
      if(addmax.le.0.0d0) go to 170
c     if(addmax.gt.0.0d0)
c     then
c
c	restore original "a" (lower triangular part and diagonal)
c
	do 120 j=1,n
	  a(j,j)=udiag(j)
	  if(j.eq.n) go to 120
	  jp1=j+1
	  do 115 i=jp1,n
	    a(i,j)=a(j,i)
  115	  continue
  120	continue
c
c	find sdd such that a+sdd*i is safely positive definite
c	note:  evmin<0 since a is not positive definite;
c
	evmin=0.0d0
	evmax=a(1,1)
	do 150 i=1,n
	  offrow=0.0d0
	  if(i.eq.1) go to 135
	  im1=i-1
	  do 130 j=1,im1
	    offrow=offrow+abs(a(i,j))
  130	  continue
  135	  if(i.eq.n) go to 145
	  ip1=i+1
	  do 140 j=ip1,n
	    offrow=offrow+abs(a(j,i))
  140	  continue
  145	  evmin=min(evmin,a(i,i)-offrow)
	  evmax=max(evmax,a(i,i)+offrow)
  150	continue
	sdd=tol*(evmax-evmin)-evmin
c
c	perturb "a" and decompose again
c
	amu=min(sdd,addmax)
	do 160 i=1,n
	  a(i,i)=a(i,i)+amu
	  udiag(i)=a(i,i)
  160	continue
c
c	 "a" now guaranteed safely positive definite
c
	call choldc(nr,n,a,0.0d0,tol,addmax)
c     endif
c
c	unscale hessian and cholesky decomposition matrix
c
  170 do 190 j=1,n
	do 175 i=j,n
	  a(i,j)=sx(i)*a(i,j)
  175	continue
	if(j.eq.1) go to 185
	jm1=j-1
	do 180 i=1,jm1
	  a(i,j)=sx(i)*sx(j)*a(i,j)
  180	continue
  185	udiag(j)=udiag(j)*sx(j)*sx(j)
  190 continue
      return
      end


c	subroutine choldc
c
c	purpose
c
c	find the perturbed l(l-transpose) [written ll+] decomposition
c	of a+d, where d is a non-negative diagonal matrix added to a if
c	necessary to allow the cholesky decomposition to continue.
c
c	parameters
c
c	nr	     --> row dimension of matrix
c	n	     --> dimension of problem
c	a(n,n)	    <--> on entry: matrix for which to find perturbed
c			      cholesky decomposition
c			 on exit:  contains l of ll+ decomposition
c			 in lower triangular part and diagonal of "a"
c	diagmx	     --> maximum diagonal element of "a"
c	tol	     --> tolerance
c	addmax	    <--	 maximum amount implicitly added to diagonal of "a"
c			 in forming the cholesky decomposition of a+d
c	internal variables
c
c	aminl	 smallest element allowed on diagonal of l
c	amnlsq	 =aminl**2
c	offmax	 maximum off-diagonal element in column of a
c
c
c	description
c
c	the normal cholesky decomposition is performed.	 however, if at any
c	point the algorithm would attempt to set l(i,i)=sqrt(temp)
c	with temp < tol*diagmx, then l(i,i) is set to sqrt(tol*diagmx)
c	instead.  this is equivalent to adding tol*diagmx-temp to a(i,i)
c
c
cstatic
      subroutine choldc(nr,n,a,diagmx,tol,addmax)
      implicit double precision (a-h,o-z)
      dimension a(nr,1)
c
      addmax=0.0d0
      aminl=sqrt(diagmx*tol)
      amnlsq=aminl*aminl
c
c	form column j of l
c
      do 100 j=1,n
c
c	find diagonal elements of l
c
	sum=0.0d0
	if(j.eq.1) go to 20
	jm1=j-1
	do 10 k=1,jm1
	  sum=sum + a(j,k)*a(j,k)
   10	continue
   20	temp=a(j,j)-sum
	if(temp.lt.amnlsq) go to 30
c	if(temp.ge.aminl**2)
c	then
	  a(j,j)=sqrt(temp)
	  go to 40
c	else
c
c	find maximum off-diagonal element in column
c
   30	  offmax=0.0d0
	  if(j.eq.n) go to 37
	  jp1=j+1
	  do 35 i=jp1,n
	    if(abs(a(i,j)).gt.offmax) offmax=abs(a(i,j))
   35	  continue
   37	  if(offmax.le.amnlsq) offmax=amnlsq
c
c	add to diagonal element to allow cholesky decomposition to continue
c
	  a(j,j)=sqrt(offmax)
	  addmax=max(addmax,offmax-temp)
c	endif
c
c	find i,j element of lower triangular matrix
   40	if(j.eq.n) go to 100
	jp1=j+1
	do 70 i=jp1,n
	  sum=0.0d0
	  if(j.eq.1) go to 60
	  jm1=j-1
	  do 50 k=1,jm1
	    sum=sum+a(i,k)*a(j,k)
   50	  continue
   60	  a(i,j)=(a(i,j)-sum)/a(j,j)
   70	continue
  100 continue
      return
      end


c	subroutine d1fcn
c
c	purpose
c
c	dummy routine to prevent unsatisfied external diagnostic
c	when specific analytic gradient function not supplied.
c
cstatic
      subroutine d1fcn(n,x,g)
      implicit double precision (a-h,o-z)
      dimension x(n),g(n)
      g(n)=g(n)
      x(n)=x(n)
      end


c	subroutine d2fcn
c
c	purpose
c
c	dummy routine to prevent unsatisfied external diagnostic
c	when specific analytic hessian function not supplied.
c
cstatic
      subroutine d2fcn(nr,n,x,h)
      implicit double precision (a-h,o-z)
      dimension x(n),h(nr,1)
      h(nr,1)=h(nr,1)
      x(n)=x(n)
      end


c	subroutine dfault
c
c	purpose
c
c	set default values for each input variable to
c	minimization algorithm.
c
c	parameters
c
c	n	     --> dimension of problem
c	x(n)	     --> initial guess to solution (to compute max step size)
c	typsiz(n)   <--	 typical size for each component of x
c	fscale	    <--	 estimate of scale of minimization function
c	method	    <--	 algorithm to use to solve minimization problem
c	iexp	    <--	 =0 if minimization function not expensive to evaluate
c	msg	    <--	 message to inhibit certain automatic checks + output
c	ndigit	    <--	 number of good digits in minimization function
c	itnlim	    <--	 maximum number of allowable iterations
c	iagflg	    <--	 =0 if analytic gradient not supplied
c	iahflg	    <--	 =0 if analytic hessian not supplied
c	ipr	    <--	 device to which to send output
c	dlt	    <--	 trust region radius
c	gradtl	    <--	 tolerance at which gradient considered close enough
c			 to zero to terminate algorithm
c	stepmx	    <--	 value of zero to trip default maximum in optchk
c	steptl	    <--	 tolerance at which successive iterates considered
c			 close enough to terminate algorithm
c
cstatic
      subroutine dfault(n,x,typsiz,fscale,method,iexp,msg,ndigit,
     +	   itnlim,iagflg,iahflg,ipr,dlt,gradtl,stepmx,steptl)
      implicit double precision (a-h,o-z)
      dimension typsiz(n),x(n)
      x(n)=x(n)
c
c	set typical size of x and minimization function
      do 10 i=1,n
	typsiz(i)=1.0d0
   10 continue
      fscale=1.0d0
c
c	set tolerances
c
      dlt=-1.0d0
      epsm=d1mach(4)
      gradtl=epsm**(1.0d0/3.0d0)
      stepmx=0.0d0
      steptl=sqrt(epsm)
c
c	set flags
c
      method=1
      iexp=1
      msg=0
      ndigit=-1
      itnlim=150
      iagflg=0
      iahflg=0
      ipr=i1mach(2)
c
      return
      end


c	subroutine dogdrv
c
c	purpose
c
c	find a next newton iterate (xpls) by the double dogleg method
c
c	parameters
c
c	nr	     --> row dimension of matrix
c	n	     --> dimension of problem
c	x(n)	     --> old iterate x[k-1]
c	f	     --> function value at old iterate, f(x)
c	g(n)	     --> gradient  at old iterate, g(x), or approximate
c	a(n,n)	     --> cholesky decomposition of hessian
c			 in lower triangular part and diagonal
c	p(n)	     --> newton step
c	xpls(n)	    <--	 new iterate x[k]
c	fpls	    <--	 function value at new iterate, f(xpls)
c	fcn	     --> name of subroutine to evaluate function
c	sx(n)	     --> diagonal scaling matrix for x
c	stepmx	     --> maximum allowable step size
c	steptl	     --> relative step size at which successive iterates
c			 considered close enough to terminate algorithm
c	dlt	    <--> trust region radius
c			 [retain value between successive calls]
c	iretcd	    <--	 return code
c			   =0 satisfactory xpls found
c			   =1 failed to find satisfactory xpls sufficiently
c			      distinct from x
c	mxtake	    <--	 boolean flag indicating step of maximum length used
c	sc(n)	     --> workspace [current step]
c	wrk1(n)	     --> workspace (and place holding argument to tregup)
c	wrk2(n)	     --> workspace
c	wrk3(n)	     --> workspace
c	ipr	     --> device to which to send output
c
cstatic
      subroutine dogdrv(nr,n,x,f,g,a,p,xpls,fpls,fcn,sx,stepmx,
     +	   steptl,dlt,iretcd,mxtake,sc,wrk1,wrk2,wrk3,ipr,itncnt)
      implicit double precision (a-h,o-z)
      dimension x(n),xpls(n),g(n),p(n)
      dimension sx(n)
      dimension sc(n),wrk1(n),wrk2(n),wrk3(n)
      dimension a(nr,1)
      logical fstdog,nwtake,mxtake
      integer itncnt
      external fcn
c
      iretcd=4
      fstdog=.true.
      tmp=0.0d0
      do 5 i=1,n
	tmp=tmp+sx(i)*sx(i)*p(i)*p(i)
    5 continue
      rnwtln=sqrt(tmp)
c$    write(ipr,954) rnwtln
c
  100 continue
c
c	find new step by double dogleg algorithm
c
      call dogstp(nr,n,g,a,p,sx,rnwtln,dlt,nwtake,fstdog,
     +	   wrk1,wrk2,cln,eta,sc,ipr,stepmx)
c
c	check new point and update trust region
c
      call tregup(nr,n,x,f,g,a,fcn,sc,sx,nwtake,stepmx,steptl,dlt,
     +	   iretcd,wrk3,fplsp,xpls,fpls,mxtake,ipr,2,wrk1)
      if(iretcd.le.1) return
      go to 100
c%950 format(42h dogdrv	   initial trust region not given.,
c%   +	     22h  compute cauchy step.)
c%951 format(18h dogdrv	   alpha =,e20.13/
c%   +	     18h dogdrv	   beta	 =,e20.13/
c%   +	     18h dogdrv	   dlt	 =,e20.13/
c%   +	     18h dogdrv	   nwtake=,l1	 )
c%952 format(28h dogdrv	   current step (sc))
c%954 format(18h0dogdrv	   rnwtln=,e20.13)
c%955 format(14h dogdrv	      ,5(e20.13,3x))
      end


c	subroutine dogstp
c
c	purpose
c
c	find new step by double dogleg algorithm
c
c	parameters
c
c	nr	     --> row dimension of matrix
c	n	     --> dimension of problem
c	g(n)	     --> gradient at current iterate, g(x)
c	a(n,n)	     --> cholesky decomposition of hessian in
c			 lower part and diagonal
c	p(n)	     --> newton step
c	sx(n)	     --> diagonal scaling matrix for x
c	rnwtln	     --> newton step length
c	dlt	    <--> trust region radius
c	nwtake	    <--> boolean, =.true. if newton step taken
c	fstdog	    <--> boolean, =.true. if on first leg of dogleg
c	ssd(n)	    <--> workspace [cauchy step to the minimum of the
c			 quadratic model in the scaled steepest descent
c			 direction] [retain value between successive calls]
c	v(n)	    <--> workspace  [retain value between successive calls]
c	cln	    <--> cauchy length
c			 [retain value between successive calls]
c	eta		 [retain value between successive calls]
c	sc(n)	    <--	 current step
c	ipr	     --> device to which to send output
c	stepmx	     --> maximum allowable step size
c
c	internal variables
c
c	cln		 length of cauchy step
c
cstatic
      subroutine dogstp(nr,n,g,a,p,sx,rnwtln,dlt,nwtake,fstdog,
     +	   ssd,v,cln,eta,sc,ipr,stepmx)
      implicit double precision (a-h,o-z)
      dimension g(n),p(n)
      dimension sx(n)
      dimension sc(n),ssd(n),v(n)
      dimension a(nr,1)
      logical nwtake,fstdog
      ipr=ipr
c
c	can we take newton step
c
      if(rnwtln.gt.dlt) go to 100
c     if(rnwtln.le.dlt)
c     then
	nwtake=.true.
	do 10 i=1,n
	  sc(i)=p(i)
   10	continue
	dlt=rnwtln
c$	write(ipr,951)
	go to 700
c     else
c
c	newton step too long
c	cauchy step is on double dogleg curve
c
  100	nwtake=.false.
	if(.not.fstdog) go to 200
c	if(fstdog)
c	then
c
c	  calculate double dogleg curve (ssd)
	  fstdog=.false.
	  alpha=0.0d0
	  do 110 i=1,n
	    alpha=alpha + (g(i)*g(i))/(sx(i)*sx(i))
  110	  continue
	  beta=0.0d0
	  do 130 i=1,n
	    tmp=0.0d0
	    do 120 j=i,n
	      tmp=tmp + (a(j,i)*g(j))/(sx(j)*sx(j))
  120	    continue
	    beta=beta+tmp*tmp
  130	  continue
	  do 140 i=1,n
	    ssd(i)=-(alpha/beta)*g(i)/sx(i)
  140	  continue
	  cln=alpha*sqrt(alpha)/beta
	  eta=.2 + (.8*alpha*alpha)/(-beta*ddot(n,g,1,p,1))
	  do 150 i=1,n
	    v(i)=eta*sx(i)*p(i) - ssd(i)
  150	  continue
	  if (dlt .eq. (-1.0d0)) dlt = min(cln, stepmx)
c$	  write(ipr,954) alpha,beta,cln,eta
c$	  write(ipr,955)
c$	  write(ipr,960) (ssd(i),i=1,n)
c$	  write(ipr,956)
c$	  write(ipr,960) (v(i),i=1,n)
c	endif
  200	if(eta*rnwtln.gt.dlt) go to 220
c	if(eta*rnwtln .le. dlt)
c	then
c
c	  take partial step in newton direction
c
	  do 210 i=1,n
	    sc(i)=(dlt/rnwtln)*p(i)
  210	  continue
c$	  write(ipr,957)
	  go to 700
c	else
  220	  if(cln.lt.dlt) go to 240
c	  if(cln.ge.dlt)
c	  then
c	    take step in steepest descent direction
c
	    do 230 i=1,n
	      sc(i)=(dlt/cln)*ssd(i)/sx(i)
  230	    continue
c$	    write(ipr,958)
	    go to 700
c	  else
c
c	    calculate convex combination of ssd and eta*p
c	    which has scaled length dlt
c
  240	    dot1=ddot(n,v,1,ssd,1)
	    dot2=ddot(n,v,1,v,1)
	    alam=(-dot1+sqrt((dot1*dot1)-dot2*(cln*cln-dlt*dlt)))/dot2
	    do 250 i=1,n
	      sc(i)=(ssd(i) + alam*v(i))/sx(i)
  250	    continue
c$	    write(ipr,959)
c	  endif
c	endif
c     endif
  700 continue
c$    write(ipr,952) fstdog,nwtake,rnwtln,dlt
c$    write(ipr,953)
c$    write(ipr,960) (sc(i),i=1,n)
      return
c
c%951 format(27h0dogstp	   take newton step)
c%952 format(18h dogstp	   fstdog=,l1/
c%   +	     18h dogstp	   nwtake=,l1/
c%   +	     18h dogstp	   rnwtln=,e20.13/
c%   +	     18h dogstp	   dlt	 =,e20.13)
c%953 format(28h dogstp	   current step (sc))
c%954 format(18h dogstp	   alpha =,e20.13/
c%   +	     18h dogstp	   beta	 =,e20.13/
c%   +	     18h dogstp	   cln	 =,e20.13/
c%   +	     18h dogstp	   eta	 =,e20.13)
c%955 format(28h dogstp	   cauchy step (ssd))
c%956 format(12h dogstp	   v)
c%957 format(48h0dogstp	   take partial step in newton direction)
c%958 format(50h0dogstp	   take step in steepest descent direction)
c%959 format(39h0dogstp	   take convex combination step)
c%960 format(14h dogstp	      ,5(e20.13,3x))
      end


c	subroutine forslv
c
c	purpose
c
c	solve  ax=b  where a is lower triangular matrix
c
c	parameters
c
c	nr	     --> row dimension of matrix
c	n	     --> dimension of problem
c	a(n,n)	     --> lower triangular matrix (preserved)
c	x(n)	    <--	 solution vector
c	b(n)	     --> right-hand side vector
c
c	note
c
c	if b is no longer required by calling routine,
c	then vectors b and x may share the same storage.
c
cstatic
      subroutine forslv(nr,n,a,x,b)
      implicit double precision (a-h,o-z)
      dimension a(nr,1),x(n),b(n)
c
c	solve lx=b. (foreward solve)
c
      x(1)=b(1)/a(1,1)
      if(n.eq.1) return
      do 20 i=2,n
	sum=0.0d0
	im1=i-1
	do 10 j=1,im1
	  sum=sum+a(i,j)*x(j)
   10	continue
	x(i)=(b(i)-sum)/a(i,i)
   20 continue
      return
      end


c	subroutine fstocd
c
c	purpose
c
c	find central difference approximation g to the first derivative
c	(gradient) of the function defined by fcn at the point x.
c
c	parameters
c
c	n	     --> dimension of problem
c	x	     --> point at which gradient is to be approximated.
c	fcn	     --> name of subroutine to evaluate function.
c	sx	     --> diagonal scaling matrix for x.
c	rnoise	     --> relative noise in fcn [f(x)].
c	g	    <--	 central difference approximation to gradient.
c
c
cstatic
      subroutine fstocd (n, x, fcn, sx, rnoise, g)
      implicit none
      integer n
      external fcn
      double precision x(n), sx(n), g(n), rnoise
c
      integer i
      double precision third, stepi,xtempi, fplus, fminus
c
c	find i th  stepsize, evaluate two neighbors in direction of i th
c	unit vector, and evaluate i th	component of gradient.
c
      third = 1.0d0/3.0d0
      do 10 i = 1, n
	 stepi = rnoise**third * max(abs(x(i)), 1.0d0/sx(i))
	 xtempi = x(i)
	 x(i) = xtempi + stepi
	 call fcn (n, x, fplus)
	 x(i) = xtempi - stepi
	 call fcn (n, x, fminus)
	 x(i) = xtempi
	 g(i) = (fplus - fminus)/(2.0d0*stepi)
   10 continue
      return
      end


c	subroutine fstofd
c
c	purpose
c
c	find first order forward finite difference approximation "a" to the
c	first derivative of the function defined by the subprogram "fname"
c	evaluated at the new iterate "xpls".
c
c	for optimization use this routine to estimate:
c	1) the first derivative (gradient) of the optimization function "fcn
c	   analytic user routine has been supplied;
c	2) the second derivative (hessian) of the optimization function
c	   if no analytic user routine has been supplied for the hessian but
c	   one has been supplied for the gradient ("fcn") and if the
c	   optimization function is inexpensive to evaluate
c
c	note
c
c	_m=1 (optimization) algorithm estimates the gradient of the function
c	     (fcn).   fcn(x) # f: r(n)-->r(1)
c	_m=n (systems) algorithm estimates the jacobian of the function
c	     fcn(x) # f: r(n)-->r(n).
c	_m=n (optimization) algorithm estimates the hessian of the optimizatio
c	     function, where the hessian is the first derivative of "fcn"
c
c	parameters
c
c	nr	     --> row dimension of matrix
c	m	     --> number of rows in a
c	n	     --> number of columns in a; dimension of problem
c	xpls(n)	     --> new iterate:  x[k]
c	fcn	     --> name of subroutine to evaluate function
c	fpls(m)	     --> _m=1 (optimization) function value at new iterate:
c			      fcn(xpls)
c			 _m=n (optimization) value of first derivative
c			      (gradient) given by user function fcn
c			 _m=n (systems)	 function value of associated
c			      minimization function
c	a(nr,n)	    <--	 finite difference approximation (see note).  only
c			 lower triangular matrix and diagonal are returned
c	sx(n)	     --> diagonal scaling matrix for x
c	rnoise	     --> relative noise in fcn [f(x)]
c	fhat(m)	     --> workspace
c	icase	     --> =1 optimization (gradient)
c			 =2 systems
c			 =3 optimization (hessian)
c
c	internal variables
c
c	stepsz - stepsize in the j-th variable direction
c
cstatic
      subroutine fstofd(nr,m,n,xpls,fcn,fpls,a,sx,rnoise,fhat,icase)
      implicit double precision (a-h,o-z)
      dimension xpls(n),fpls(m)
      dimension fhat(m)
      dimension sx(n)
      dimension a(nr,1)
c
c	find j-th column of a
c	each column is derivative of f(fcn) with respect to xpls(j)
c
      do 30 j=1,n
	stepsz=sqrt(rnoise)*max(abs(xpls(j)),1.0d0/sx(j))
	xtmpj=xpls(j)
	xpls(j)=xtmpj+stepsz
	call fcn(n,xpls,fhat)
	xpls(j)=xtmpj
	do 20 i=1,m
	  a(i,j)=(fhat(i)-fpls(i))/stepsz
   20	continue
   30 continue
      if(icase.ne.3) return
c
c	if computing hessian, a must be symmetric
c
      if(n.eq.1) return
      nm1=n-1
      do 50 j=1,nm1
	jp1=j+1
	do 40 i=jp1,m
	  a(i,j)=(a(i,j)+a(j,i))/2.0d0
   40	continue
   50 continue
      return
      end


c	subroutine grdchk
c
c	purpose
c
c	check analytic gradient against estimated gradient
c
c	parameters
c
c	n	     --> dimension of problem
c	x(n)	     --> estimate to a root of fcn
c	fcn	     --> name of subroutine to evaluate optimization function
c			 must be declared external in calling routine
c			      fcn:  r(n) --> r(1)
c	f	     --> function value:  fcn(x)
c	g(n)	     --> gradient:  g(x)
c	typsiz(n)    --> typical size for each component of x
c	sx(n)	     --> diagonal scaling matrix:  sx(i)=1./typsiz(i)
c	fscale	     --> estimate of scale of objective function fcn
c	rnf	     --> relative noise in optimization function fcn
c	analtl	     --> tolerance for comparison of estimated and
c			 analytical gradients
c	wrk1(n)	     --> workspace
c	msg	    <--	 message or error code
c			   on output: =-21, probable coding error of gradient
c	ipr	     --> device to which to send output
c
cstatic
      subroutine grdchk(n,x,fcn,f,g,typsiz,sx,fscale,rnf,
     +	   analtl,wrk1,msg,ipr)
      implicit double precision (a-h,o-z)
      dimension x(n),g(n)
      dimension sx(n),typsiz(n)
      dimension wrk1(n)
      external fcn
c
c	compute first order finite difference gradient and compare to
c	analytic gradient.
c
      call fstofd(1,1,n,x,fcn,f,wrk1,sx,rnf,wrk,1)
      ker=0
      do 5 i=1,n
	gs=max(abs(f),fscale)/max(abs(x(i)),typsiz(i))
	if(abs(g(i)-wrk1(i)).gt.max(abs(g(i)),gs)*analtl) ker=1
    5 continue
      if(ker.eq.0) go to 20
c%	write(ipr,901)
c%	write(ipr,902) (i,g(i),wrk1(i),i=1,n)
	msg=-21
   20 continue
      return
c%901 format(47h0grdchk	   probable error in coding of analytic,
c%   +	     19h gradient function./
c%   +	     16h grdchk	    comp,12x,8hanalytic,12x,8hestimate)
c%902 format(11h grdchk	   ,i5,3x,e20.13,3x,e20.13)
      end


c	subroutine heschk
c
c	purpose
c
c	check analytic hessian against estimated hessian
c	 (this may be done only if the user supplied analytic hessian
c	  d2fcn fills only the lower triangular part and diagonal of a)
c
c	parameters
c
c	nr	     --> row dimension of matrix
c	n	     --> dimension of problem
c	x(n)	     --> estimate to a root of fcn
c	fcn	     --> name of subroutine to evaluate optimization function
c			 must be declared external in calling routine
c			      fcn:  r(n) --> r(1)
c	d1fcn	     --> name of subroutine to evaluate gradient of fcn.
c			 must be declared external in calling routine
c	d2fcn	     --> name of subroutine to evaluate hessian of fcn.
c			 must be declared external in calling routine
c	f	     --> function value:  fcn(x)
c	g(n)	    <--	 gradient:  g(x)
c	a(n,n)	    <--	 on exit:  hessian in lower triangular part and diag
c	typsiz(n)    --> typical size for each component of x
c	sx(n)	     --> diagonal scaling matrix:  sx(i)=1./typsiz(i)
c	rnf	     --> relative noise in optimization function fcn
c	analtl	     --> tolerance for comparison of estimated and
c			 analytical gradients
c	iagflg	     --> =1 if analytic gradient supplied
c	udiag(n)     --> workspace
c	wrk1(n)	     --> workspace
c	wrk2(n)	     --> workspace
c	msg	    <--> message or error code
c			   on input : if =1xx do not compare anal + est hess
c			   on output: =-22, probable coding error of hessian
c	ipr	     --> device to which to send output
c
cstatic
      subroutine heschk(nr,n,x,fcn,d1fcn,d2fcn,f,g,a,typsiz,sx,rnf,
     +	   analtl,iagflg,udiag,wrk1,wrk2,msg,ipr)
      implicit double precision (a-h,o-z)
      dimension x(n),g(n),a(nr,1)
      dimension typsiz(n),sx(n)
      dimension udiag(n),wrk1(n),wrk2(n)
      external fcn,d1fcn
c
c	compute finite difference approximation a to the hessian.
c
      if(iagflg.eq.1) call fstofd(nr,n,n,x,d1fcn,g,a,sx,rnf,wrk1,3)
      if(iagflg.ne.1) call sndofd(nr,n,x,fcn,f,a,sx,rnf,wrk1,wrk2)
c
      ker=0
c
c	copy lower triangular part of "a" to upper triangular part
c	and diagonal of "a" to udiag
c
      do 30 j=1,n
	udiag(j)=a(j,j)
	if(j.eq.n) go to 30
	jp1=j+1
	do 25 i=jp1,n
	  a(j,i)=a(i,j)
   25	continue
   30 continue
c
c	compute analytic hessian and compare to finite difference
c	approximation.
c
      call d2fcn(nr,n,x,a)
      do 40 j=1,n
	hs=max(abs(g(j)),1.0d0)/max(abs(x(j)),typsiz(j))
	if(abs(a(j,j)-udiag(j)).gt.max(abs(udiag(j)),hs)*analtl)
     +	     ker=1
	if(j.eq.n) go to 40
	jp1=j+1
	do 35 i=jp1,n
	  if(abs(a(i,j)-a(j,i)).gt.max(abs(a(i,j)),hs)*analtl) ker=1
   35	continue
   40 continue
c
      if(ker.eq.0) go to 90
c%	write(ipr,901)
c%	do 50 i=1,n
c%	  if(i.eq.1) go to 45
c%	  im1=i-1
c%	  do 43 j=1,im1
c%	    write(ipr,902) i,j,a(i,j),a(j,i)
c% 43	  continue
c% 45	  write(ipr,902) i,i,a(i,i),udiag(i)
c% 50	continue
	msg=-22
c     endif
   90 continue
      return
c%901 format(47h heschk	   probable error in coding of analytic,
c%   +	     18h hessian function./
c%   +	     21h heschk	     row  col,14x,8hanalytic,14x,10h(estimate))
c%902 format(11h heschk	   ,2i5,2x,e20.13,2x,1h(,e20.13,1h))
      end


c	subroutine hookdr
c
c	purpose
c
c	find a next newton iterate (xpls) by the more-hebdon method
c
c	parameters
c
c	nr	     --> row dimension of matrix
c	n	     --> dimension of problem
c	x(n)	     --> old iterate x[k-1]
c	f	     --> function value at old iterate, f(x)
c	g(n)	     --> gradient at old iterate, g(x), or approximate
c	a(n,n)	     --> cholesky decomposition of hessian in lower
c			 triangular part and diagonal.
c			 hessian in upper triangular part and udiag.
c	udiag(n)     --> diagonal of hessian in a(.,.)
c	p(n)	     --> newton step
c	xpls(n)	    <--	 new iterate x[k]
c	fpls	    <--	 function value at new iterate, f(xpls)
c	fcn	     --> name of subroutine to evaluate function
c	sx(n)	     --> diagonal scaling matrix for x
c	stepmx	     --> maximum allowable step size
c	steptl	     --> relative step size at which successive iterates
c			 considered close enough to terminate algorithm
c	dlt	    <--> trust region radius
c	iretcd	    <--	 return code
c			   =0 satisfactory xpls found
c			   =1 failed to find satisfactory xpls sufficiently
c			      distinct from x
c	mxtake	    <--	 boolean flag indicating step of maximum length used
c	amu	    <--> [retain value between successive calls]
c	dltp	    <--> [retain value between successive calls]
c	phi	    <--> [retain value between successive calls]
c	phip0	    <--> [retain value between successive calls]
c	sc(n)	     --> workspace
c	xplsp(n)     --> workspace
c	wrk0(n)	     --> workspace
c	epsm	     --> machine epsilon
c	itncnt	     --> iteration count
c	ipr	     --> device to which to send output
c
cstatic
      subroutine hookdr(nr,n,x,f,g,a,udiag,p,xpls,fpls,fcn,sx,stepmx,
     +	   steptl,dlt,iretcd,mxtake,amu,dltp,phi,phip0,
     +	   sc,xplsp,wrk0,epsm,itncnt,ipr)
      implicit double precision (a-h,o-z)
      dimension x(n),g(n),p(n),xpls(n),sx(n)
      dimension a(nr,1),udiag(n)
      dimension sc(n),xplsp(n),wrk0(n)
      logical mxtake,nwtake
      logical fstime
      external fcn
c
      iretcd=4
      fstime=.true.
      tmp=0.0d0
      do 5 i=1,n
	tmp=tmp+sx(i)*sx(i)*p(i)*p(i)
    5 continue
      rnwtln=sqrt(tmp)
c$    write(ipr,954) rnwtln
c
      if(itncnt.gt.1) go to 100
c     if(itncnt.eq.1)
c     then
	amu=0.0d0
c
c	if first iteration and trust region not provided by user,
c	compute initial trust region.
c
	if(dlt.ne. (-1.0d0)) go to 100
c	if(dlt.eq. (-1.0d0))
c	then
	  alpha=0.0d0
	  do 10 i=1,n
	    alpha=alpha+(g(i)*g(i))/(sx(i)*sx(i))
   10	  continue
	  beta=0.0d0
	  do 30 i=1,n
	    tmp=0.0d0
	    do 20 j=i,n
	      tmp=tmp + (a(j,i)*g(j))/(sx(j)*sx(j))
   20	    continue
	    beta=beta+tmp*tmp
   30	  continue
	  dlt=alpha*sqrt(alpha)/beta
	  dlt = min(dlt, stepmx)
c$	  write(ipr,950)
c$	  write(ipr,951) alpha,beta,dlt
c	endif
c     endif
c
  100 continue
c
c	find new step by more-hebdon algorithm
c
      call hookst(nr,n,g,a,udiag,p,sx,rnwtln,dlt,amu,
     +	   dltp,phi,phip0,fstime,sc,nwtake,wrk0,epsm,ipr)
      dltp=dlt
c
c	check new point and update trust region
c
      call tregup(nr,n,x,f,g,a,fcn,sc,sx,nwtake,stepmx,steptl,
     +	       dlt,iretcd,xplsp,fplsp,xpls,fpls,mxtake,ipr,3,udiag)
      if(iretcd.le.1) return
      go to 100
c
c%950 format(43h hookdr	   initial trust region not given. ,
c%   +	     21h compute cauchy step.)
c%951 format(18h hookdr	   alpha =,e20.13/
c%   +	     18h hookdr	   beta	 =,e20.13/
c%   +	     18h hookdr	   dlt	 =,e20.13)
c%952 format(28h hookdr	   current step (sc))
c%954 format(18h0hookdr	   rnwtln=,e20.13)
c%955 format(14h hookdr	      ,5(e20.13,3x))
      end


c	 subroutine hookst
c
c	 purpose
c
c	 find new step by more-hebdon algorithm
c
c	 parameters
c
c	 nr	      --> row dimension of matrix
c	 n	      --> dimension of problem
c	 g(n)	      --> gradient at current iterate, g(x)
c	 a(n,n)	      --> cholesky decomposition of hessian in
c			  lower triangular part and diagonal.
c			  hessian or approx in upper triangular part
c	 udiag(n)     --> diagonal of hessian in a(.,.)
c	 p(n)	      --> newton step
c	 sx(n)	      --> diagonal scaling matrix for n
c	 rnwtln	      --> newton step length
c	 dlt	     <--> trust region radius
c	 amu	     <--> [retain value between successive calls]
c	 dltp	      --> trust region radius at last exit from this routine
c	 phi	     <--> [retain value between successive calls]
c	 phip0	     <--> [retain value between successive calls]
c	 fstime	     <--> boolean. =.true. if first entry to this routine
c			  during k-th iteration
c	 sc(n)	     <--  current step
c	 nwtake	     <--  boolean, =.true. if newton step taken
c	 wrk0	      --> workspace
c	 epsm	      --> machine epsilon
c	 ipr	      --> device to which to send output
c
cstatic
      subroutine hookst(nr,n,g,a,udiag,p,sx,rnwtln,dlt,amu,
     +	   dltp,phi,phip0,fstime,sc,nwtake,wrk0,epsm,ipr)
      implicit double precision (a-h,o-z)
      dimension g(n),p(n),sx(n),sc(n),wrk0(n)
      dimension a(nr,1),udiag(n)
      logical nwtake,done
      logical fstime
c
c	 hi and alo are constants used in this routine.
c	 change here if other values are to be substituted.
      ipr=ipr
      hi=1.5d0
      alo=0.75d0
c
      if(rnwtln.gt.hi*dlt) go to 15
c     if(rnwtln.le.hi*dlt)
c     then
c
c	take newton step
c
	nwtake=.true.
	do 10 i=1,n
	  sc(i)=p(i)
   10	continue
	dlt=min(dlt,rnwtln)
	amu=0.0d0
c$	write(ipr,951)
	return
c     else
c
c	newton step not taken
c
   15	continue
c$	write(ipr,952)
	nwtake=.false.
	if(amu.le.0.0d0) go to 20
c	if(amu.gt.0.0d0)
c	then
	  amu=amu- (phi+dltp) *((dltp-dlt)+phi)/(dlt*phip)
c$	  write(ipr,956) amu
c	endif
   20	continue
	phi=rnwtln-dlt
	if(.not.fstime) go to 28
c	if(fstime)
c	then
	  do 25 i=1,n
	    wrk0(i)=sx(i)*sx(i)*p(i)
   25	  continue
c
c	  solve l*y = (sx**2)*p
c
	  call forslv(nr,n,a,wrk0,wrk0)
	  phip0=-dnrm2(n,wrk0,1)**2/rnwtln
	  fstime=.false.
c	endif
   28	phip=phip0
	amulo=-phi/phip
	amuup=0.0d0
	do 30 i=1,n
	  amuup=amuup+(g(i)*g(i))/(sx(i)*sx(i))
   30	continue
	amuup=sqrt(amuup)/dlt
	done=.false.
c$	write(ipr,956) amu
c$	write(ipr,959) phi
c$	write(ipr,960) phip
c$	write(ipr,957) amulo
c$	write(ipr,958) amuup
c
c	test value of amu; generate next amu if necessary
c
  100	continue
	if(done) return
c$	write(ipr,962)
	if(amu.ge.amulo .and. amu.le.amuup) go to 110
c	if(amu.lt.amulo .or.  amu.gt.amuup)
c	then
	  amu=max(sqrt(amulo*amuup),amuup*1.0d-3)
c$	  write(ipr,956) amu
c	endif
  110	continue
c
c	copy (h,udiag) to l
c	where h <-- h+amu*(sx**2) [do not actually change (h,udiag)]
	do 130 j=1,n
	  a(j,j)=udiag(j) + amu*sx(j)*sx(j)
	  if(j.eq.n) go to 130
	  jp1=j+1
	  do 120 i=jp1,n
	    a(i,j)=a(j,i)
  120	  continue
  130	continue
c
c	factor h=l(l+)
c
	call choldc(nr,n,a,0.0d0,sqrt(epsm),addmax)
c
c	solve h*p = l(l+)*sc = -g
c
	do 140 i=1,n
	  wrk0(i)=-g(i)
  140	continue
	call lltslv(nr,n,a,sc,wrk0)
c$	write(ipr,955)
c$	write(ipr,963) (sc(i),i=1,n)
c
c	reset h.  note since udiag has not been destroyed we need do
c	nothing here.  h is in the upper part and in udiag, still intact
c
	stepln=0.0d0
	do 150 i=1,n
	  stepln=stepln + sx(i)*sx(i)*sc(i)*sc(i)
  150	continue
	stepln=sqrt(stepln)
	phi=stepln-dlt
	do 160 i=1,n
	  wrk0(i)=sx(i)*sx(i)*sc(i)
  160	continue
	call forslv(nr,n,a,wrk0,wrk0)
	phip=-dnrm2(n,wrk0,1)**2/stepln
c$	write(ipr,961) dlt,stepln
c$	write(ipr,959) phi
c$	write(ipr,960) phip
	if((alo*dlt.gt.stepln .or. stepln.gt.hi*dlt) .and.
     +	     (amuup-amulo.gt.0.0d0)) go to 170
c	if((alo*dlt.le.stepln .and. stepln.le.hi*dlt) .or.
c	     (amuup-amulo.le.0.0d0))
c	then
c
c	  sc is acceptable hookstep
c
c$	  write(ipr,954)
	  done=.true.
	  go to 100
c	else
c
c	  sc not acceptable hookstep.  select new amu
c
  170	  continue
c$	  write(ipr,953)
	  amulo=max(amulo,amu-(phi/phip))
	  if(phi.lt.0.0d0) amuup=min(amuup,amu)
	  amu=amu-(stepln*phi)/(dlt*phip)
c$	  write(ipr,956) amu
c$	  write(ipr,957) amulo
c$	  write(ipr,958) amuup
	  go to 100
c	endif
c     endif
c
c%951 format(27h0hookst	   take newton step)
c%952 format(32h0hookst	   newton step not taken)
c%953 format(31h hookst	   sc is not acceptable)
c%954 format(27h hookst	   sc is acceptable)
c%955 format(28h hookst	   current step (sc))
c%956 format(18h hookst	   amu	 =,e20.13)
c%957 format(18h hookst	   amulo =,e20.13)
c%958 format(18h hookst	   amuup =,e20.13)
c%959 format(18h hookst	   phi	 =,e20.13)
c%960 format(18h hookst	   phip	 =,e20.13)
c%961 format(18h hookst	   dlt	 =,e20.13/
c%   +	     18h hookst	   stepln=,e20.13)
c%962 format(23h0hookst	   find new amu)
c%963 format(14h hookst	      ,5(e20.13,3x))
      end


c	subroutine hsnint
c
c	purpose
c
c	provide initial hessian when using secant updates
c
c	parameters
c
c	nr	     --> row dimension of matrix
c	n	     --> dimension of problem
c	a(n,n)	    <--	 initial hessian (lower triangular matrix)
c	sx(n)	     --> diagonal scaling matrix for x
c	method	     --> algorithm to use to solve minimization problem
c			   =1,2 factored secant method used
c			   =3	unfactored secant method used
c
cstatic
      subroutine hsnint(nr,n,a,sx,method)
      implicit double precision (a-h,o-z)
      dimension a(nr,1),sx(n)
c
      do 100 j=1,n
	if(method.eq.3) a(j,j)=sx(j)*sx(j)
	if(method.ne.3) a(j,j)=sx(j)
	if(j.eq.n) go to 100
	jp1=j+1
	do 90 i=jp1,n
	  a(i,j)=0.0d0
   90	continue
  100 continue
      return
      end


c	subroutine lltslv
c
c	purpose
c
c	solve ax=b where a has the form l(l-transpose)
c	but only the lower triangular part, l, is stored.
c
c	parameters
c
c	nr	     --> row dimension of matrix
c	n	     --> dimension of problem
c	a(n,n)	     --> matrix of form l(l-transpose).
c			 on return a is unchanged.
c	x(n)	    <--	 solution vector
c	b(n)	     --> right-hand side vector
c
c	note
c
c	if b is not required by calling program, then
c	b and x may share the same storage.
c
cstatic
      subroutine lltslv(nr,n,a,x,b)
      implicit double precision (a-h,o-z)
      dimension a(nr,1),x(n),b(n)
c
c	forward solve, result in x
c
      call forslv(nr,n,a,x,b)
c
c	back solve, result in x
c
      call baksol(nr,n,a,x,x)
      return
      end


c	subroutine lnsrch
c
c	purpose
c
c	find a next newton iterate by line search.
c
c	parameters
c
c	n	     --> dimension of problem
c	x(n)	     --> old iterate:	x[k-1]
c	f	     --> function value at old iterate, f(x)
c	g(n)	     --> gradient at old iterate, g(x), or approximate
c	p(n)	     --> non-zero newton step
c	xpls(n)	    <--	 new iterate x[k]
c	fpls	    <--	 function value at new iterate, f(xpls)
c	fcn	     --> name of subroutine to evaluate function
c	iretcd	    <--	 return code
c	mxtake	    <--	 boolean flag indicating step of maximum length used
c	stepmx	     --> maximum allowable step size
c	steptl	     --> relative step size at which successive iterates
c			 considered close enough to terminate algorithm
c	sx(n)	     --> diagonal scaling matrix for x
c	ipr	     --> device to which to send output
c
c	internal variables
c
c	sln		 newton length
c	rln		 relative length of newton step
c
cstatic
      subroutine lnsrch(n,x,f,g,p,xpls,fpls,fcn,mxtake,
     +	 iretcd,stepmx,steptl,sx,ipr)
      implicit double precision (a-h,o-z)
      integer n,iretcd
      dimension sx(n)
      dimension x(n),g(n),p(n)
      dimension xpls(n)
      logical mxtake
c
      ipr=ipr
      mxtake=.false.
      iretcd=2
c$    write(ipr,954)
c$    write(ipr,955) (p(i),i=1,n)
      tmp=0.0d0
      do 5 i=1,n
	tmp=tmp+sx(i)*sx(i)*p(i)*p(i)
    5 continue
      sln=sqrt(tmp)
      if(sln.le.stepmx) go to 10
c
c	newton step longer than maximum allowed
	scl=stepmx/sln
	call sclmul(n,scl,p,p)
	sln=stepmx
c$	write(ipr,954)
c$	write(ipr,955) (p(i),i=1,n)
   10 continue
      slp=ddot(n,g,1,p,1)
      rln=0.0d0
      do 15 i=1,n
	rln=max(rln,abs(p(i))/max(abs(x(i)),1.0d0/sx(i)))
   15 continue
      rmnlmb=steptl/rln
      almbda=1.0d0
c$    write(ipr,952) sln,slp,rmnlmb,stepmx,steptl
c
c	loop
c	check if new iterate satisfactory.  generate new lambda if necessary.
c
  100 continue
      if(iretcd.lt.2) return
      do 105 i=1,n
	xpls(i)=x(i) + almbda*p(i)
  105 continue
      call fcn(n,xpls,fpls)
c$    write(ipr,950) almbda
c$    write(ipr,951)
c$    write(ipr,955) (xpls(i),i=1,n)
c$    write(ipr,953) fpls
      if(fpls.gt. f+slp*1.d-4*almbda) go to 130
c     if(fpls.le. f+slp*1.d-4*almbda)
c     then
c
c	solution found
c
	iretcd=0
	if(almbda.eq.1.0d0 .and. sln.gt. 0.99d0*stepmx) mxtake=.true.
	go to 100
c
c	solution not (yet) found
c
c     else
  130	if(almbda .ge. rmnlmb) go to 140
c	if(almbda .lt. rmnlmb)
c	then
c
c	no satisfactory xpls found sufficiently distinct from x
c
	  iretcd=1
	  go to 100
c	else
c
c	calculate new lambda
c
  140	  if(almbda.ne.1.0d0) go to 150
c	  if(almbda.eq.1.0d0)
c	  then
c
c	first backtrack: quadratic fit
c
	    tlmbda=-slp/(2.0d0*(fpls-f-slp))
	    go to 170
c	  else
c
c	all subsequent backtracks: cubic fit
c
  150	    t1=fpls-f-almbda*slp
	    t2=pfpls-f-plmbda*slp
	    t3=1.0d0/(almbda-plmbda)
	    a=t3*(t1/(almbda*almbda) - t2/(plmbda*plmbda))
	    b=t3*(t2*almbda/(plmbda*plmbda)
     +		 - t1*plmbda/(almbda*almbda) )
	    disc=b*b-3.0d0*a*slp
	    if(disc.le. b*b) go to 160
c	    if(disc.gt. b*b)
c	    then
c
c	only one positive critical point, must be minimum
c
	      tlmbda=(-b+sign(1.0d0,a)*sqrt(disc))/(3.0d0*a)
	      go to 165
c	    else
c
c	both critical points positive, first is minimum
c
  160	      tlmbda=(-b-sign(1.0d0,a)*sqrt(disc))/(3.0d0*a)
c	    endif
  165	    if(tlmbda.gt. .5*almbda) tlmbda=.5*almbda
c	  endif
  170	  plmbda=almbda
	  pfpls=fpls
	  if(tlmbda.ge. almbda*.1) go to 180
c	  if(tlmbda.lt.almbda/10.0d0)
c	  then
	    almbda=almbda*.1
	    go to 190
c	  else
  180	    almbda=tlmbda
c	  endif
c	endif
c     endif
  190 go to 100
c%950 format(18h lnsrch	   almbda=,e20.13)
c%951 format(29h lnsrch	   new iterate (xpls))
c%952 format(18h lnsrch	   sln	 =,e20.13/
c%   +	     18h lnsrch	   slp	 =,e20.13/
c%   +	     18h lnsrch	   rmnlmb=,e20.13/
c%   +	     18h lnsrch	   stepmx=,e20.13/
c%   +	     18h lnsrch	   steptl=,e20.13)
c%953 format(19h lnsrch	   f(xpls)=,e20.13)
c%954 format(26h0lnsrch	   newton step (p))
c%955 format(14h lnsrch	      ,5(e20.13,3x))
      end


c	subroutine mvmltl
c
c	purpose
c
c	compute y=lx
c	where l is a lower triangular matrix stored in a
c
c	parameters
c
c	nr	     --> row dimension of matrix
c	n	     --> dimension of problem
c	a(n,n)	     --> lower triangular (n*n) matrix
c	x(n)	     --> operand vector
c	y(n)	    <--	 result vector
c
c	note
c
c	x and y cannot share storage
c
cstatic
      subroutine mvmltl(nr,n,a,x,y)
      implicit double precision (a-h,o-z)
      dimension a(nr,1),x(n),y(n)
      do 30 i=1,n
	sum=0.0d0
	do 10 j=1,i
	  sum=sum+a(i,j)*x(j)
   10	continue
	y(i)=sum
   30 continue
      return
      end


c	subroutine mvmlts
c
c	purpose
c
c	compute y=ax
c	where "a" is a symmetric (n*n) matrix stored in its lower
c	triangular part and x,y are n-vectors
c
c	parameters
c
c	nr	     --> row dimension of matrix
c	n	     --> dimension of problem
c	a(n,n)	     --> symmetric (n*n) matrix stored in
c			 lower triangular part and diagonal
c	x(n)	     --> operand vector
c	y(n)	    <--	 result vector
c
c	note
c
c	x and y cannot share storage.
c
cstatic
      subroutine mvmlts(nr,n,a,x,y)
      implicit double precision (a-h,o-z)
      dimension a(nr,1),x(n),y(n)
      do 30 i=1,n
	sum=0.0d0
	do 10 j=1,i
	  sum=sum+a(i,j)*x(j)
   10	continue
	if(i.eq.n) go to 25
	ip1=i+1
	do 20 j=ip1,n
	  sum=sum+a(j,i)*x(j)
   20	continue
   25	y(i)=sum
   30 continue
      return
      end


c	subroutine mvmltu
c
c	purpose
c
c	compute y=(l+)x
c	where l is a lower triangular matrix stored in a
c	(l-transpose (l+) is taken implicitly)
c
c	parameters
c
c	nr	     --> row dimension of matrix
c	n	     --> dimension of problem
c	a(nr,1)	      --> lower triangular (n*n) matrix
c	x(n)	     --> operand vector
c	y(n)	    <--	 result vector
c
c	note
c
c	x and y cannot share storage
c
cstatic
      subroutine mvmltu(nr,n,a,x,y)
      implicit double precision (a-h,o-z)
      dimension a(nr,1),x(n),y(n)
      do 30 i=1,n
	sum=0.0d0
	do 10 j=i,n
	  sum=sum+a(j,i)*x(j)
   10	continue
	y(i)=sum
   30 continue
      return
      end


c	subroutine optchk
c
c	purpose
c
c	check input for reasonableness
c
c	parameters
c
c	n	     --> dimension of problem
c	x(n)	     --> on entry, estimate to root of fcn
c	typsiz(n)   <--> typical size of each component of x
c	sx(n)	    <--	 diagonal scaling matrix for x
c	fscale	    <--> estimate of scale of objective function fcn
c	gradtl	     --> tolerance at which gradient considered close
c			 enough to zero to terminate algorithm
c	itnlim	    <--> maximum number of allowable iterations
c	ndigit	    <--> number of good digits in optimization function fcn
c	epsm	     --> machine epsilon
c	dlt	    <--> trust region radius
c	method	    <--> algorithm indicator
c	iexp	    <--> expense flag
c	iagflg	    <--> =1 if analytic gradient supplied
c	iahflg	    <--> =1 if analytic hessian supplied
c	stepmx	    <--> maximum step size
c	msg	    <--> message and error code
c	ipr	     --> device to which to send output
c
cstatic
      subroutine optchk(n,x,typsiz,sx,fscale,gradtl,itnlim,ndigit,epsm,
     +	   dlt,method,iexp,iagflg,iahflg,stepmx,msg,ipr)
      implicit double precision (a-h,o-z)
      dimension x(n),typsiz(n),sx(n)
c
c	check that parameters only take on acceptable values.
c	if not, set them to default values.
      if(method.lt.1 .or. method.gt.3) method=1
      if(iagflg.ne.1) iagflg=0
      if(iahflg.ne.1) iahflg=0
      if(iexp.ne.0) iexp=1
      if(mod(msg/2,2).eq.1 .and. iagflg.eq.0) go to 830
      if(mod(msg/4,2).eq.1 .and. iahflg.eq.0) go to 835
c
c	check dimension of problem
c
      if(n.le.0) go to 805
      if(n.eq.1 .and. mod(msg,2).eq.0) go to 810
c
c	compute scale matrix
c
      do 10 i=1,n
	if(typsiz(i).eq.0.0d0) typsiz(i)=1.0
	if(typsiz(i).lt.0.0d0) typsiz(i)=-typsiz(i)
	sx(i)=1.0d0/typsiz(i)
   10 continue
c
c	check maximum step size
c
      if (stepmx .gt. 0.0d0) go to 20
      stpsiz = 0.0d0
      do 15 i = 1, n
	 stpsiz = stpsiz + x(i)*x(i)*sx(i)*sx(i)
   15 continue
      stpsiz = sqrt(stpsiz)
      stepmx = max(1.0d3*stpsiz, 1.0d3)
   20 continue
c	check function scale
      if(fscale.eq.0.0d0) fscale=1.0d0
      if(fscale.lt.0.0d0) fscale=-fscale
c
c	check gradient tolerance
      if(gradtl.lt.0.0d0) go to 815
c
c	check iteration limit
      if(itnlim.le.0) go to 820
c
c	check number of digits of accuracy in function fcn
      if(ndigit.eq.0) go to 825
      if(ndigit.lt.0) ndigit=-log10(epsm)
c
c	check trust region radius
      if(dlt.le.0.0d0) dlt=-1.0d0
      if (dlt .gt. stepmx) dlt = stepmx
      return
c
c	error exits
c
c%805 write(ipr,901) n
c%    msg=-1
  805 msg=-1
      go to 895
c%810 write(ipr,902)
c%    msg=-2
  810 msg=-2
      go to 895
c%815 write(ipr,903) gradtl
c%    msg=-3
  815 msg=-3
      go to 895
c%820 write(ipr,904) itnlim
c%    msg=-4
  820 msg=-4
      go to 895
c%825 write(ipr,905) ndigit
c%    msg=-5
  825 msg=-5
      go to 895
c%830 write(ipr,906) msg,iagflg
c%    msg=-6
  830 msg=-6
      go to 895
c%835 write(ipr,907) msg,iahflg
c%    msg=-7
  835 msg=-7
  895 return
c%901 format(32h0optchk	   illegal dimension, n=,i5)
c%902 format(55h0optchk	   +++ warning +++  this package is inefficient,
c%   +	     26h for problems of size n=1./
c%   +	     48h optchk	   check installation libraries for more,
c%   +	     22h appropriate routines./
c%   +	     41h optchk	   if none, set msg and resubmit.)
c%903 format(38h0optchk	   illegal tolerance.  gradtl=,e20.13)
c%904 format(44h0optchk	   illegal iteration limit.  itnlim=,i5)
c%905 format(52h0optchk	   minimization function has no good digits.,
c%   +	      9h  ndigit=,i5)
c%906 format(50h0optchk	   user requests that analytic gradient be,
c%   +	     33h accepted as properly coded (msg=,i5, 2h),/
c%   +	     45h optchk	   but analytic gradient not supplied,
c%   +	      9h (iagflg=,i5, 2h).)
c%907 format(49h0optchk	   user requests that analytic hessian be,
c%   +	     33h accepted as properly coded (msg=,i5, 2h),/
c%   +	     44h optchk	   but analytic hessian not supplied,
c%   +	      9h (iahflg=,i5, 2h).)
      end


c	subroutine optdrv
c
c	purpose
c
c	driver for non-linear optimization problem
c
c	parameters
c
c	nr	     --> row dimension of matrix
c	n	     --> dimension of problem
c	x(n)	     --> on entry: estimate to a root of fcn
c	fcn	     --> name of subroutine to evaluate optimization function
c			 must be declared external in calling routine
c				   fcn: r(n) --> r(1)
c	d1fcn	     --> (optional) name of subroutine to evaluate gradient
c			 of fcn.  must be declared external in calling routine
c	d2fcn	     --> (optional) name of subroutine to evaluate hessian of
c			 of fcn.  must be declared external in calling routine
c	typsiz(n)    --> typical size for each component of x
c	fscale	     --> estimate of scale of objective function
c	method	     --> algorithm to use to solve minimization problem
c			   =1 line search
c			   =2 double dogleg
c			   =3 more-hebdon
c	iexp	     --> =1 if optimization function fcn is expensive to
c			 evaluate, =0 otherwise.  if set then hessian will
c			 be evaluated by secant update instead of
c			 analytically or by finite differences
c	msg	    <--> on input:  (.gt.0) message to inhibit certain
c			   automatic checks
c			 on output: (.lt.0) error code; =0 no error
c	ndigit	     --> number of good digits in optimization function fcn
c	itnlim	     --> maximum number of allowable iterations
c	iagflg	     --> =1 if analytic gradient supplied
c	iahflg	     --> =1 if analytic hessian supplied
c	ipr	     --> device to which to send output
c	dlt	     --> trust region radius
c	gradtl	     --> tolerance at which gradient considered close
c			 enough to zero to terminate algorithm
c	stepmx	     --> maximum allowable step size
c	steptl	     --> relative step size at which successive iterates
c			 considered close enough to terminate algorithm
c	xpls(n)	    <--> on exit:  xpls is local minimum
c	fpls	    <--> on exit:  function value at solution, xpls
c	gpls(n)	    <--> on exit:  gradient at solution xpls
c	itrmcd	    <--	 termination code
c	a(n,n)	     --> workspace for hessian (or estimate)
c			 and its cholesky decomposition
c	udiag(n)     --> workspace [for diagonal of hessian]
c	g(n)	     --> workspace (for gradient at current iterate)
c	p(n)	     --> workspace for step
c	sx(n)	     --> workspace (for diagonal scaling matrix)
c	wrk0(n)	     --> workspace
c	wrk1(n)	     --> workspace
c	wrk2(n)	     --> workspace
c	wrk3(n)	     --> workspace
c	itncnt	     current iteration, k  {{was `internal'}}
c
c
c	internal variables
c
c	analtl		 tolerance for comparison of estimated and
c			 analytical gradients and hessians
c	epsm		 machine epsilon
c	f		 function value: fcn(x)
c	rnf		 relative noise in optimization function fcn.
c			      noise=10.**(-ndigit)
c
cstatic
      subroutine optdrv(nr,n,x,fcn,d1fcn,d2fcn,typsiz,fscale,
     +	   method,iexp,msg,ndigit,itnlim,iagflg,iahflg,ipr,
     +	   dlt,gradtl,stepmx,steptl,
     +	   xpls,fpls,gpls,itrmcd,
     +	   a,udiag,g,p,sx,wrk0,wrk1,wrk2,wrk3,itncnt)
      implicit double precision (a-h,o-z)
      dimension x(n),xpls(n),g(n),gpls(n),p(n)
      dimension typsiz(n),sx(n)
      dimension a(nr,1),udiag(n)
      dimension wrk0(n),wrk1(n),wrk2(n),wrk3(n)
      logical mxtake,noupdt
      integer itncnt

      external fcn,d1fcn,d2fcn

      external result
c	for iteration tracing [defined in ../main/optimize.c]

c
c	initialization
c
      do 10 i=1,n
	p(i)=0.0d0
   10 continue
      itncnt=0
      iretcd=-1
      epsm=d1mach(4)
      call optchk(n,x,typsiz,sx,fscale,gradtl,itnlim,ndigit,epsm,
     +	   dlt,method,iexp,iagflg,iahflg,stepmx,msg,ipr)
      if(msg.lt.0) return
      rnf=max(10.0d0**(-ndigit),epsm)
      analtl=max(1.0d-2,sqrt(rnf))
c
c%    if(mod(msg/8,2).eq.1) go to 15
c%    write(ipr,901)
c%    write(ipr,900) (typsiz(i),i=1,n)
c%    write(ipr,902)
c%    write(ipr,900) (sx(i),i=1,n)
c%    write(ipr,903) fscale
c%    write(ipr,904) ndigit,iagflg,iahflg,iexp,method,itnlim,epsm
c%    write(ipr,905) stepmx,steptl,gradtl,dlt,rnf,analtl
c% 15 continue
c
c	evaluate fcn(x)
c
      call fcn(n,x,f)
c
c	evaluate analytic or finite difference gradient and check analytic
c	gradient, if requested.
c
      if (iagflg .eq. 1) go to 20
c     if (iagflg .eq. 0)
c     then
	call fstofd (1, 1, n, x, fcn, f, g, sx, rnf, wrk, 1)
	go to 25
c
   20 call d1fcn (n, x, g)
      if (mod(msg/2,2) .eq. 1) go to 25
c     if (mod(msg/2,2).eq.0)
c     then
	call grdchk (n, x, fcn, f, g, typsiz, sx, fscale,
     1	  rnf, analtl, wrk1, msg, ipr)
	if (msg .lt. 0) return
   25 continue
c
      call optstp(n,x,f,g,wrk1,itncnt,icscmx,
     +		  itrmcd,gradtl,steptl,sx,fscale,itnlim,iretcd,mxtake,
     +		  ipr,msg)
      if(itrmcd.ne.0) go to 700
c
      if(iexp.ne.1) go to 80
c
c	if optimization function expensive to evaluate (iexp=1), then
c	hessian will be obtained by secant updates.  get initial hessian.
c
      call hsnint(nr,n,a,sx,method)
      go to 90
   80 continue
c
c	evaluate analytic or finite difference hessian and check analytic
c	hessian if requested (only if user-supplied analytic hessian
c	routine d2fcn fills only lower triangular part and diagonal of a).
c
      if (iahflg .eq. 1) go to 82
c     if (iahflg .eq. 0)
c     then
	 if (iagflg .eq. 1) call fstofd (nr, n, n, x, d1fcn, g, a, sx,
     1	    rnf, wrk1, 3)
	 if (iagflg .ne. 1) call sndofd (nr, n, x, fcn, f, a, sx, rnf,
     1	    wrk1, wrk2)
	 go to 88
c
c     else
   82	 if (mod(msg/4,2).eq.0) go to 85
c	 if (mod(msg/4, 2) .eq. 1)
c	 then
	    call d2fcn (nr, n, x, a)
	    go to 88
c
c	 else
   85	    call heschk (nr, n, x, fcn, d1fcn, d2fcn, f, g, a, typsiz,
     1	       sx, rnf, analtl, iagflg, udiag, wrk1, wrk2, msg, ipr)
c
c	    heschk evaluates d2fcn and checks it against the finite
c	    difference hessian which it calculates by calling fstofd
c	    (if iagflg .eq. 1) or sndofd (otherwise).
c
	    if (msg .lt. 0) return
   88 continue
c
   90 if(mod(msg/8,2).eq.0)
     +	   call result(nr,n,x,f,g,a,p,itncnt,1,ipr)
c
c
c	iteration
c
  100 itncnt=itncnt+1
c
c	find perturbed local model hessian and its ll+ decomposition
c	(skip this step if line search or dogstep techniques being used with
c	secant updates.	 cholesky decomposition l already obtained from
c	secfac.)
c
      if(iexp.eq.1 .and. method.ne.3) go to 105
  103	call chlhsn(nr,n,a,epsm,sx,udiag)
  105 continue
c
c	solve for newton step:	ap=-g
c
      do 110 i=1,n
	wrk1(i)=-g(i)
  110 continue
      call lltslv(nr,n,a,p,wrk1)
c
c	decide whether to accept newton step  xpls=x + p
c	or to choose xpls by a global strategy.
c
      if (iagflg .ne. 0 .or. method .eq. 1) go to 111
      dltsav = dlt
      if (method .eq. 2) go to 111
      amusav = amu
      dlpsav = dltp
      phisav = phi
      phpsav = phip0
  111 if(method.eq.1)
     +	   call lnsrch(n,x,f,g,p,xpls,fpls,fcn,mxtake,iretcd,
     +	   stepmx,steptl,sx,ipr)
      if(method.eq.2)
     +	   call dogdrv(nr,n,x,f,g,a,p,xpls,fpls,fcn,sx,stepmx,
     +	   steptl,dlt,iretcd,mxtake,wrk0,wrk1,wrk2,wrk3,ipr,itncnt)
      if(method.eq.3)
     +	   call hookdr(nr,n,x,f,g,a,udiag,p,xpls,fpls,fcn,sx,stepmx,
     +	   steptl,dlt,iretcd,mxtake,amu,dltp,phi,phip0,wrk0,
     +	   wrk1,wrk2,epsm,itncnt,ipr)
c
c	if could not find satisfactory step and forward difference
c	gradient was used, retry using central difference gradient.
c
      if (iretcd .ne. 1 .or. iagflg .ne. 0) go to 112
c     if (iretcd .eq. 1 .and. iagflg .eq. 0)
c     then
c
c	 set iagflg for central differences
c
	 iagflg = -1
c%	 write(ipr,906) itncnt
c
	 call fstocd (n, x, fcn, sx, rnf, g)
	 if (method .eq. 1) go to 105
	 dlt = dltsav
	 if (method .eq. 2) go to 105
	 amu = amusav
	 dltp = dlpsav
	 phi = phisav
	 phip0 = phpsav
	 go to 103
c     endif
c
c	calculate step for output
c
  112 continue
      do 114 i = 1, n
	 p(i) = xpls(i) - x(i)
  114 continue
c
c	calculate gradient at xpls
c
      if (iagflg .eq. (-1)) go to 116
      if (iagflg .eq. 0) go to 118
c
c	analytic gradient
      call d1fcn (n, xpls, gpls)
      go to 120
c
c	central difference gradient
  116 call fstocd (n, xpls, fcn, sx, rnf, gpls)
      go to 120
c
c	forward difference gradient
  118 call fstofd (1, 1, n, xpls, fcn, fpls, gpls, sx, rnf, wrk, 1)
  120 continue
c
c	check whether stopping criteria satisfied
c
      call optstp(n,xpls,fpls,gpls,x,itncnt,icscmx,
     +		  itrmcd,gradtl,steptl,sx,fscale,itnlim,iretcd,mxtake,
     +		  ipr,msg)
      if(itrmcd.ne.0) go to 690
c
c	evaluate hessian at xpls
c
      if(iexp.eq.0) go to 130
      if(method.eq.3)
     +	   call secunf(nr,n,x,g,a,udiag,xpls,gpls,epsm,itncnt,rnf,
     +	   iagflg,noupdt,wrk1,wrk2,wrk3)
      if(method.ne.3)
     +	   call secfac(nr,n,x,g,a,xpls,gpls,epsm,itncnt,rnf,iagflg,
     +	   noupdt,wrk0,wrk1,wrk2,wrk3)
      go to 150
  130 if(iahflg.eq.1) go to 140
      if(iagflg.eq.1)
     +	   call fstofd(nr,n,n,xpls,d1fcn,gpls,a,sx,rnf,wrk1,3)
      if(iagflg.ne.1) call sndofd(nr,n,xpls,fcn,fpls,a,sx,rnf,wrk1,wrk2)
      go to 150
  140 call d2fcn(nr,n,xpls,a)
  150 continue
      if(mod(msg/16,2).eq.1)
     +	   call result(nr,n,xpls,fpls,gpls,a,p,itncnt,1,ipr)
c
c	x <-- xpls  and	 g <-- gpls  and  f <-- fpls
c
      f=fpls
      do 160 i=1,n
	x(i)=xpls(i)
	g(i)=gpls(i)
  160 continue
      go to 100
c
c	termination
c
c	reset xpls,fpls,gpls,  if previous iterate solution
c
  690 if(itrmcd.ne.3) go to 710
  700 continue
      fpls=f
      do 705 i=1,n
	xpls(i)=x(i)
	gpls(i)=g(i)
  705 continue
c
c	print results
c
  710 continue
      if(mod(msg/8,2).eq.0)
     +	   call result(nr,n,xpls,fpls,gpls,a,p,itncnt,0,ipr)
      msg=0
      return
c
c%900 format(14h optdrv	      ,5(e20.13,3x))
c%901 format(20h0optdrv	   typical x)
c%902 format(40h optdrv	   diagonal scaling matrix for x)
c%903 format(22h optdrv	   typical f =,e20.13)
c%904 format(40h0optdrv	   number of good digits in fcn=,i5/
c%   +	     27h optdrv	   gradient flag  =,i5,18h   (=1 if analytic,
c%   +	     19h gradient supplied)/
c%   +	     27h optdrv	   hessian flag	  =,i5,18h   (=1 if analytic,
c%   +	     18h hessian supplied)/
c%   +	     27h optdrv	   expense flag	  =,i5, 9h   (=1 if,
c%   +	     45h minimization function expensive to evaluate)/
c%   +	     27h optdrv	   method to use  =,i5,19h   (=1,2,3 for line,
c%   +	     49h search, double dogleg, more-hebdon respectively)/
c%   +	     27h optdrv	   iteration limit=,i5/
c%   +	     27h optdrv	   machine epsilon=,e20.13)
c%905 format(30h0optdrv	   maximum step size =,e20.13/
c%   +	     30h optdrv	   step tolerance    =,e20.13/
c%   +	     30h optdrv	   gradient tolerance=,e20.13/
c%   +	     30h optdrv	   trust reg radius  =,e20.13/
c%   +	     30h optdrv	   rel noise in fcn  =,e20.13/
c%   +	     30h optdrv	   anal-fd tolerance =,e20.13)
c%906 format(52h optdrv	   shift from forward to central differences,
c%   1	 14h in iteration , i5)
      end


c	subroutine optif0
c
c	purpose
c
c	provide simplest interface to minimization package.
c	user has no control over options.
c
c	parameters
c
c	nr	     --> row dimension of matrix
c	n	     --> dimension of problem
c	x(n)	     --> initial estimate of minimum
c	fcn	     --> name of routine to evaluate minimization function.
c			 must be declared external in calling routine.
c	xpls(n)	    <--	 local minimum
c	fpls	    <--	 function value at local minimum xpls
c	gpls(n)	    <--	 gradient at local minimum xpls
c	itrmcd	    <--	 termination code
c	a(n,n)	     --> workspace
c	wrk(n,9)     --> workspace
c
      subroutine optif0(nr,n,x,fcn,xpls,fpls,gpls,itrmcd,a,wrk)
      implicit double precision (a-h,o-z)
      dimension x(n),xpls(n),gpls(n)
      dimension a(nr,1),wrk(nr,9)
      external fcn,d1fcn,d2fcn
      integer itncnt
c
c	equivalence wrk(n,1) = udiag(n)
c		    wrk(n,2) = g(n)
c		    wrk(n,3) = p(n)
c		    wrk(n,4) = typsiz(n)
c		    wrk(n,5) = sx(n)
c		    wrk(n,6) = wrk0(n)
c		    wrk(n,7) = wrk1(n)
c		    wrk(n,8) = wrk2(n)
c		    wrk(n,9) = wrk3(n)
c
      call dfault(n,x,wrk(1,4),fscale,method,iexp,msg,ndigit,
     +	   itnlim,iagflg,iahflg,ipr,dlt,gradtl,stepmx,steptl)
      call optdrv(nr,n,x,fcn,d1fcn,d2fcn,wrk(1,4),fscale,
     +	   method,iexp,msg,ndigit,itnlim,iagflg,iahflg,ipr,
     +	   dlt,gradtl,stepmx,steptl,
     +	   xpls,fpls,gpls,itrmcd,
     +	   a,wrk(1,1),wrk(1,2),wrk(1,3),wrk(1,5),wrk(1,6),
     +	   wrk(1,7),wrk(1,8),wrk(1,9),itncnt)
      return
      end

C---- this one is called from ../main/optimize.c : ---------------

c	subroutine optif9
c
c	purpose
c
c	provide complete interface to minimization package.
c	user has full control over options.
c
c	parameters
c
c	nr	     --> row dimension of matrix
c	n	     --> dimension of problem
c	x(n)	     --> on entry: estimate to a root of fcn
c	fcn	     --> name of subroutine to evaluate optimization function
c			 must be declared external in calling routine
c				   fcn: r(n) --> r(1)
c	d1fcn	     --> (optional) name of subroutine to evaluate gradient
c			 of fcn.  must be declared external in calling routine
c	d2fcn	     --> (optional) name of subroutine to evaluate hessian of
c			 of fcn.  must be declared external in calling routine
c	typsiz(n)    --> typical size for each component of x
c	fscale	     --> estimate of scale of objective function
c	method	     --> algorithm to use to solve minimization problem
c			   =1 line search
c			   =2 double dogleg
c			   =3 more-hebdon
c	iexp	     --> =1 if optimization function fcn is expensive to
c			 evaluate, =0 otherwise.  if set then hessian will
c			 be evaluated by secant update instead of
c			 analytically or by finite differences
c	msg	    <--> on input:  (.gt.0) message to inhibit certain
c			   automatic checks
c			 on output: (.lt.0) error code; =0 no error
c	ndigit	     --> number of good digits in optimization function fcn
c	itnlim	     --> maximum number of allowable iterations
c	iagflg	     --> =1 if analytic gradient supplied
c	iahflg	     --> =1 if analytic hessian supplied
c	ipr	     --> device to which to send output
c	dlt	     --> trust region radius
c	gradtl	     --> tolerance at which gradient considered close
c			 enough to zero to terminate algorithm
c	stepmx	     --> maximum allowable step size
c	steptl	     --> relative step size at which successive iterates
c			 considered close enough to terminate algorithm
c	xpls(n)	    <--> on exit:  xpls is local minimum
c	fpls	    <--> on exit:  function value at solution, xpls
c	gpls(n)	    <--> on exit:  gradient at solution xpls
c	itrmcd	    <--	 termination code
c	a(n,n)	     --> workspace for hessian (or estimate)
c			 and its cholesky decomposition
c	wrk(n,8)     --> workspace
c	itncnt	     --> iteration count
c
      subroutine optif9(nr,n,x,fcn,d1fcn,d2fcn,typsiz,fscale,
     +	   method,iexp,msg,ndigit,itnlim,iagflg,iahflg,ipr,
     +	   dlt,gradtl,stepmx,steptl,
     +	   xpls,fpls,gpls,itrmcd,a,wrk, itncnt)
      implicit double precision (a-h,o-z)
      dimension x(n),xpls(n),gpls(n),typsiz(n)
      dimension a(nr,1),wrk(nr,8)
      external fcn,d1fcn,d2fcn
      integer itncnt
c
c	equivalence wrk(n,1) = udiag(n)
c		    wrk(n,2) = g(n)
c		    wrk(n,3) = p(n)
c		    wrk(n,4) = sx(n)
c		    wrk(n,5) = wrk0(n)
c		    wrk(n,6) = wrk1(n)
c		    wrk(n,7) = wrk2(n)
c		    wrk(n,8) = wrk3(n)
c
      call optdrv(nr,n,x,fcn,d1fcn,d2fcn,typsiz,fscale,
     +	   method,iexp,msg,ndigit,itnlim,iagflg,iahflg,ipr,
     +	   dlt,gradtl,stepmx,steptl,
     +	   xpls,fpls,gpls,itrmcd,
     +	   a,wrk(1,1),wrk(1,2),wrk(1,3),wrk(1,4),wrk(1,5),
     +	   wrk(1,6),wrk(1,7),wrk(1,8),itncnt)
      return
      end


c	subroutine optstp
c
c	unconstrained minimization stopping criteria
c
c	find whether the algorithm should terminate, due to any
c	of the following:
c	1) problem solved within user tolerance
c	2) convergence within user tolerance
c	3) iteration limit reached
c	4) divergence or too restrictive maximum step (stepmx) suspected
c
c	parameters
c
c	n	     --> dimension of problem
c	xpls(n)	     --> new iterate x[k]
c	fpls	     --> function value at new iterate f(xpls)
c	gpls(n)	     --> gradient at new iterate, g(xpls), or approximate
c	x(n)	     --> old iterate x[k-1]
c	itncnt	     --> current iteration k
c	icscmx	    <--> number consecutive steps .ge. stepmx
c			 [retain value between successive calls]
c	itrmcd	    <--	 termination code
c	gradtl	     --> tolerance at which relative gradient considered close
c			 enough to zero to terminate algorithm
c	steptl	     --> relative step size at which successive iterates
c			 considered close enough to terminate algorithm
c	sx(n)	     --> diagonal scaling matrix for x
c	fscale	     --> estimate of scale of objective function
c	itnlim	     --> maximum number of allowable iterations
c	iretcd	     --> return code
c	mxtake	     --> boolean flag indicating step of maximum length used
c	ipr	     --> device to which to send output
c	msg	     --> if msg includes a term 8, suppress output
c
c
cstatic
      subroutine optstp(n,xpls,fpls,gpls,x,itncnt,icscmx,
     +	    itrmcd,gradtl,steptl,sx,fscale,itnlim,iretcd,mxtake,ipr,msg)
      implicit double precision (a-h,o-z)
      integer n,itncnt,icscmx,itrmcd,itnlim
      dimension sx(n)
      dimension xpls(n),gpls(n),x(n)
      logical mxtake
c
      itrmcd=0
c
c	last global step failed to locate a point lower than x
c
      if(iretcd.ne.1) go to 50
c     if(iretcd.eq.1)
c     then
	jtrmcd=3
	go to 600
c     endif
   50 continue
c
c	find direction in which relative gradient maximum.
c	check whether within tolerance
c
      d=max(abs(fpls),fscale)
      rgx=0.0d0
      do 100 i=1,n
	relgrd=abs(gpls(i))*max(abs(xpls(i)),1.0d0/sx(i))/d
	rgx=max(rgx,relgrd)
  100 continue
      jtrmcd=1
      if(rgx.le.gradtl) go to 600
c
      if(itncnt.eq.0) return
c
c	find direction in which relative stepsize maximum
c	check whether within tolerance.
c
      rsx=0.0d0
      do 120 i=1,n
	relstp=abs(xpls(i)-x(i))/max(abs(xpls(i)),1.0d0/sx(i))
	rsx=max(rsx,relstp)
  120 continue
      jtrmcd=2
      if(rsx.le.steptl) go to 600
c
c	check iteration limit
c
      jtrmcd=4
      if(itncnt.ge.itnlim) go to 600
c
c	check number of consecutive steps \ stepmx
c
      if(mxtake) go to 140
c     if(.not.mxtake)
c     then
	icscmx=0
	return
c     else
  140	continue
c%	if (mod(msg/8,2) .eq. 0) write(ipr,900)
	icscmx=icscmx+1
	if(icscmx.lt.5) return
	jtrmcd=5
c     endif
c
c
c	print termination code
c
  600 itrmcd=jtrmcd
c%    if (mod(msg/8,2) .eq. 0) go to(601,602,603,604,605), itrmcd
c%    go to 700
c%601 write(ipr,901)
c%    go to 700
c%602 write(ipr,902)
c%    go to 700
c%603 write(ipr,903)
c%    go to 700
c%604 write(ipr,904)
c%    go to 700
c%605 write(ipr,905)
c
  700 return
c
c%900 format(48h0optstp	   step of maximum length (stepmx) taken)
c%901 format(43h0optstp	   relative gradient close to zero./
c%   +	     48h optstp	   current iterate is probably solution.)
c%902 format(48h0optstp	   successive iterates within tolerance./
c%   +	     48h optstp	   current iterate is probably solution.)
c%903 format(52h0optstp	   last global step failed to locate a point,
c%   +	     14h lower than x./
c%   +	     51h optstp	   either x is an approximate local minimum,
c%   +	     17h of the function,/
c%   +	     50h optstp	   the function is too non-linear for this,
c%   +	     11h algorithm,/
c%   +	     34h optstp	   or steptl is too large.)
c%904 format(36h0optstp	   iteration limit exceeded./
c%   +	     28h optstp	   algorithm failed.)
c%905 format(39h0optstp	   maximum step size exceeded 5,
c%   +	     19h consecutive times./
c%   +	     50h optstp	   either the function is unbounded below,/
c%   +	     47h optstp	   becomes asymptotic to a finite value,
c%   +	     30h from above in some direction,/
c%   +	     33h optstp	   or stepmx is too small)
      end


c	subroutine qraux1
c
c	purpose
c
c	interchange rows i,i+1 of the upper hessenberg matrix r,
c	columns i to n
c
c	parameters
c
c	nr	     --> row dimension of matrix
c	n	     --> dimension of matrix
c	r(n,n)	    <--> upper hessenberg matrix
c	i	     --> index of row to interchange (i.lt.n)
c
cstatic
      subroutine qraux1(nr,n,r,i)
      implicit double precision (a-h,o-z)
      dimension r(nr,1)
      do 10 j=i,n
	tmp=r(i,j)
	r(i,j)=r(i+1,j)
	r(i+1,j)=tmp
   10 continue
      return
      end


c	subroutine qraux2
c
c	purpose
c
c	pre-multiply r by the jacobi rotation j(i,i+1,a,b)
c
c	parameters
c
c	nr	     --> row dimension of matrix
c	n	     --> dimension of matrix
c	r(n,n)	    <--> upper hessenberg matrix
c	i	     --> index of row
c	a	     --> scalar
c	b	     --> scalar
c
cstatic
      subroutine qraux2(nr,n,r,i,a,b)
      implicit double precision (a-h,o-z)
      dimension r(nr,1)
      den=sqrt(a*a + b*b)
      c=a/den
      s=b/den
      do 10 j=i,n
	y=r(i,j)
	z=r(i+1,j)
	r(i,j)=c*y - s*z
	r(i+1,j)=s*y + c*z
   10 continue
      return
      end


c	subroutine qrupdt
c
c	purpose
c
c	find an orthogonal (n*n) matrix (q*) and an upper triangular (n*n)
c	matrix (r*) such that (q*)(r*)=r+u(v+)
c
c	parameters
c
c	nr	     --> row dimension of matrix
c	n	     --> dimension of problem
c	a(n,n)	    <--> on input:  contains r
c			 on output: contains (r*)
c	u(n)	     --> vector
c	v(n)	     --> vector
c
cstatic
      subroutine qrupdt(nr,n,a,u,v)
      implicit double precision (a-h,o-z)
      dimension a(nr,1)
      dimension u(n),v(n)
c
c	determine last non-zero in u(.)
c
      k=n
   10 if(u(k).ne.0.0d0 .or. k.eq.1) go to 20
c     if(u(k).eq.0.0d0 .and. k.gt.1)
c     then
	k=k-1
	go to 10
c     endif
c
c	(k-1) jacobi rotations transform
c	    r + u(v+) --> (r*) + (u(1)*e1)(v+)
c	which is upper hessenberg
c
   20 if(k.le.1) go to 40
	km1=k-1
	do 30 ii=1,km1
	  i=km1-ii+1
	  if(u(i).ne.0.0d0) go to 25
c	  if(u(i).eq.0.0d0)
c	  then
	    call qraux1(nr,n,a,i)
	    u(i)=u(i+1)
	    go to 30
c	  else
   25	    call qraux2(nr,n,a,i,u(i),-u(i+1))
	    u(i)=sqrt(u(i)*u(i) + u(i+1)*u(i+1))
c	  endif
   30	continue
c     endif
c
c	r <-- r + (u(1)*e1)(v+)
c
   40 do 50 j=1,n
	a(1,j)=a(1,j) +u(1)*v(j)
   50 continue
c
c	(k-1) jacobi rotations transform upper hessenberg r
c	to upper triangular (r*)
c
      if(k.le.1) go to 100
	km1=k-1
	do 80 i=1,km1
	  if(a(i,i).ne.0.0d0) go to 70
c	  if(a(i,i).eq.0.0d0)
c	  then
	    call qraux1(nr,n,a,i)
	    go to 80
c	  else
   70	    t1=a(i,i)
	    t2=-a(i+1,i)
	    call qraux2(nr,n,a,i,t1,t2)
c	  endif
   80	continue
c     endif
  100 return
      end


c	subroutine sclmul
c
c	purpose
c
c	multiply vector by scalar
c	result vector may be operand vector
c
c	parameters
c
c	n	     --> dimension of vectors
c	s	     --> scalar
c	v(n)	     --> operand vector
c	z(n)	    <--	 result vector
cstatic
      subroutine sclmul(n,s,v,z)
      implicit double precision (a-h,o-z)
      dimension v(n),z(n)
      do 100 i=1,n
	z(i)=s*v(i)
  100 continue
      return
      end


c	subroutine secfac
c
c	purpose
c
c	update hessian by the bfgs factored method
c
c	parameters
c
c	nr	     --> row dimension of matrix
c	n	     --> dimension of problem
c	x(n)	     --> old iterate, x[k-1]
c	g(n)	     --> gradient or approximate at old iterate
c	a(n,n)	    <--> on entry: cholesky decomposition of hessian in
c			   lower part and diagonal.
c			 on exit:  updated cholesky decomposition of hessian
c			   in lower triangular part and diagonal
c	xpls(n)	     --> new iterate, x[k]
c	gpls(n)	     --> gradient or approximate at new iterate
c	epsm	     --> machine epsilon
c	itncnt	     --> iteration count
c	rnf	     --> relative noise in optimization function fcn
c	iagflg	     --> =1 if analytic gradient supplied, =0 itherwise
c	noupdt	    <--> boolean: no update yet
c			 [retain value between successive calls]
c	s(n)	     --> workspace
c	y(n)	     --> workspace
c	u(n)	     --> workspace
c	w(n)	     --> workspace
c
cstatic
      subroutine secfac(nr,n,x,g,a,xpls,gpls,epsm,itncnt,rnf,
     +	   iagflg,noupdt,s,y,u,w)
      implicit double precision (a-h,o-z)
      dimension x(n),xpls(n),g(n),gpls(n)
      dimension a(nr,1)
      dimension s(n),y(n),u(n),w(n)
      logical noupdt,skpupd
c
      if(itncnt.eq.1) noupdt=.true.
      do 10 i=1,n
	s(i)=xpls(i)-x(i)
	y(i)=gpls(i)-g(i)
   10 continue
      den1=ddot(n,s,1,y,1)
      snorm2=dnrm2(n,s,1)
      ynrm2=dnrm2(n,y,1)
      if(den1.lt.sqrt(epsm)*snorm2*ynrm2) go to 110
c     if(den1.ge.sqrt(epsm)*snorm2*ynrm2)
c     then
	call mvmltu(nr,n,a,s,u)
	den2=ddot(n,u,1,u,1)
c
c	l <-- sqrt(den1/den2)*l
c
	alp=sqrt(den1/den2)
	if(.not.noupdt) go to 50
c	if(noupdt)
c	then
	  do 30 j=1,n
	    u(j)=alp*u(j)
	    do 20 i=j,n
	      a(i,j)=alp*a(i,j)
   20	    continue
   30	  continue
	  noupdt=.false.
	  den2=den1
	  alp=1.0d0
c	endif
   50	skpupd=.true.
c
c	w = l(l+)s = hs
c
	call mvmltl(nr,n,a,u,w)
	i=1
	if(iagflg.ne.0) go to 55
c	if(iagflg.eq.0)
c	then
	  reltol=sqrt(rnf)
	  go to 60
c	else
   55	  reltol=rnf
c	endif
   60	if(i.gt.n .or. .not.skpupd) go to 70
c	if(i.le.n .and. skpupd)
c	then
	  if(abs(y(i)-w(i)) .lt. reltol*max(abs(g(i)),abs(gpls(i))))
     +	       go to 65
c	  if(abs(y(i)-w(i)) .ge. reltol*dmax1(abs(g(i)),abs(gpls(i))))
c	  then
	    skpupd=.false.
	    go to 60
c	  else
   65	    i=i+1
	    go to 60
c	  endif
c	endif
   70	if(skpupd) go to 110
c	if(.not.skpupd)
c	then
c
c	  w=y-alp*l(l+)s
c
	  do 75 i=1,n
	    w(i)=y(i)-alp*w(i)
   75	  continue
c
c	  alp=1/sqrt(den1*den2)
c
	  alp=alp/den1
c
c	  u=(l+)/sqrt(den1*den2) = (l+)s/sqrt((y+)s * (s+)l(l+)s)
c
	  do 80 i=1,n
	    u(i)=alp*u(i)
   80	  continue
c
c	  copy l into upper triangular part.  zero l.
c
	  if(n.eq.1) go to 93
	  do 90 i=2,n
	    im1=i-1
	    do 85 j=1,im1
	      a(j,i)=a(i,j)
	      a(i,j)=0.0d0
   85	    continue
   90	  continue
c
c	  find q, (l+) such that  q(l+) = (l+) + u(w+)
c
   93	  call qrupdt(nr,n,a,u,w)
c
c	  upper triangular part and diagonal of a now contain updated
c	  cholesky decomposition of hessian.  copy back to lower
c	  triangular part.
c
	  if(n.eq.1) go to 110
	  do 100 i=2,n
	    im1=i-1
	    do 95 j=1,im1
	      a(i,j)=a(j,i)
   95	    continue
  100	  continue
c	endif
c     endif
  110 return
      end


c	subroutine secunf
c
c	purpose
c
c	update hessian by the bfgs unfactored method
c
c	parameters
c
c	nr	     --> row dimension of matrix
c	n	     --> dimension of problem
c	x(n)	     --> old iterate, x[k-1]
c	g(n)	     --> gradient or approximate at old iterate
c	a(n,n)	    <--> on entry: approximate hessian at old iterate
c			   in upper triangular part (and udiag)
c			 on exit:  updated approx hessian at new iterate
c			   in lower triangular part and diagonal
c			 [lower triangular part of symmetric matrix]
c	udiag	     --> on entry: diagonal of hessian
c	xpls(n)	     --> new iterate, x[k]
c	gpls(n)	     --> gradient or approximate at new iterate
c	epsm	     --> machine epsilon
c	itncnt	     --> iteration count
c	rnf	     --> relative noise in optimization function fcn
c	iagflg	     --> =1 if analytic gradient supplied, =0 otherwise
c	noupdt	    <--> boolean: no update yet
c			 [retain value between successive calls]
c	s(n)	     --> workspace
c	y(n)	     --> workspace
c	t(n)	     --> workspace
c
cstatic
      subroutine secunf(nr,n,x,g,a,udiag,xpls,gpls,epsm,itncnt,
     +	   rnf,iagflg,noupdt,s,y,t)
      implicit double precision (a-h,o-z)
      dimension x(n),g(n),xpls(n),gpls(n)
      dimension a(nr,1)
      dimension udiag(n)
      dimension s(n),y(n),t(n)
      logical noupdt,skpupd
c
c	copy hessian in upper triangular part and udiag to
c	lower triangular part and diagonal
c
      do 5 j=1,n
	a(j,j)=udiag(j)
	if(j.eq.n) go to 5
	jp1=j+1
	do 4 i=jp1,n
	  a(i,j)=a(j,i)
    4	continue
    5 continue
c
      if(itncnt.eq.1) noupdt=.true.
      do 10 i=1,n
	s(i)=xpls(i)-x(i)
	y(i)=gpls(i)-g(i)
   10 continue
      den1=ddot(n,s,1,y,1)
      snorm2=dnrm2(n,s,1)
      ynrm2=dnrm2(n,y,1)
      if(den1.lt.sqrt(epsm)*snorm2*ynrm2) go to 100
c     if(den1.ge.sqrt(epsm)*snorm2*ynrm2)
c     then
	call mvmlts(nr,n,a,s,t)
	den2=ddot(n,s,1,t,1)
	if(.not. noupdt) go to 50
c	if(noupdt)
c	then
c
c	  h <-- [(s+)y/(s+)hs]h
c
	  gam=den1/den2
	  den2=gam*den2
	  do 30 j=1,n
	    t(j)=gam*t(j)
	    do 20 i=j,n
	      a(i,j)=gam*a(i,j)
   20	    continue
   30	  continue
	  noupdt=.false.
c	endif
   50	skpupd=.true.
c
c	check update condition on row i
c
	do 60 i=1,n
	  tol=rnf*max(abs(g(i)),abs(gpls(i)))
	  if(iagflg.eq.0) tol=tol/sqrt(rnf)
	  if(abs(y(i)-t(i)).lt.tol) go to 60
c	  if(abs(y(i)-t(i)).ge.tol)
c	  then
	    skpupd=.false.
	    go to 70
c	  endif
   60	continue
   70	if(skpupd) go to 100
c	if(.not.skpupd)
c	then
c
c	  bfgs update
c
	  do 90 j=1,n
	    do 80 i=j,n
	      a(i,j)=a(i,j)+y(i)*y(j)/den1-t(i)*t(j)/den2
   80	    continue
   90	  continue
c	endif
c     endif
  100 return
      end


c	subroutine sndofd
c
c	purpose
c
c	find second order forward finite difference approximation "a"
c	to the second derivative (hessian) of the function defined by the subp
c	"fcn" evaluated at the new iterate "xpls"
c
c	for optimization use this routine to estimate
c	1) the second derivative (hessian) of the optimization function
c	   if no analytical user function has been supplied for either
c	   the gradient or the hessian and if the optimization function
c	   "fcn" is inexpensive to evaluate.
c
c	parameters
c
c	nr	     --> row dimension of matrix
c	n	     --> dimension of problem
c	xpls(n)	     --> new iterate:	x[k]
c	fcn	     --> name of subroutine to evaluate function
c	fpls	     --> function value at new iterate, f(xpls)
c	a(n,n)	    <--	 finite difference approximation to hessian
c			 only lower triangular matrix and diagonal
c			 are returned
c	sx(n)	     --> diagonal scaling matrix for x
c	rnoise	     --> relative noise in fname [f(x)]
c	stepsz(n)    --> workspace (stepsize in i-th component direction)
c	anbr(n)	     --> workspace (neighbor in i-th direction)
c
c
cstatic
      subroutine sndofd(nr,n,xpls,fcn,fpls,a,sx,rnoise,stepsz,anbr)
      implicit double precision (a-h,o-z)
      dimension xpls(n)
      dimension sx(n)
      dimension stepsz(n),anbr(n)
      dimension a(nr,1)
c
c	find i-th stepsize and evaluate neighbor in direction
c	of i-th unit vector.
c
      ov3 = 1.0d0/3.0d0
      do 10 i=1,n
	stepsz(i)=rnoise**ov3 * max(abs(xpls(i)),1.0d0/sx(i))
	xtmpi=xpls(i)
	xpls(i)=xtmpi+stepsz(i)
	call fcn(n,xpls,anbr(i))
	xpls(i)=xtmpi
   10 continue
c
c	calculate column i of a
c
      do 30 i=1,n
	xtmpi=xpls(i)
	xpls(i)=xtmpi+2.0d0*stepsz(i)
	call fcn(n,xpls,fhat)
	a(i,i)=((fpls-anbr(i))+(fhat-anbr(i)))/(stepsz(i)*stepsz(i))
c
c	calculate sub-diagonal elements of column
	if(i.eq.n) go to 25
	xpls(i)=xtmpi+stepsz(i)
	ip1=i+1
	do 20 j=ip1,n
	  xtmpj=xpls(j)
	  xpls(j)=xtmpj+stepsz(j)
	  call fcn(n,xpls,fhat)
	  a(j,i)=((fpls-anbr(i))+(fhat-anbr(j)))/(stepsz(i)*stepsz(j))
	  xpls(j)=xtmpj
   20	continue
   25	xpls(i)=xtmpi
   30 continue
      return
      end


c	subroutine tregup
c
c	purpose
c
c	decide whether to accept xpls=x+sc as the next iterate and update the
c	trust region dlt.
c
c	parameters
c
c	nr	     --> row dimension of matrix
c	n	     --> dimension of problem
c	x(n)	     --> old iterate x[k-1]
c	f	     --> function value at old iterate, f(x)
c	g(n)	     --> gradient at old iterate, g(x), or approximate
c	a(n,n)	     --> cholesky decomposition of hessian in
c			 lower triangular part and diagonal.
c			 hessian or approx in upper triangular part
c	fcn	     --> name of subroutine to evaluate function
c	sc(n)	     --> current step
c	sx(n)	     --> diagonal scaling matrix for x
c	nwtake	     --> boolean, =.true. if newton step taken
c	stepmx	     --> maximum allowable step size
c	steptl	     --> relative step size at which successive iterates
c			 considered close enough to terminate algorithm
c	dlt	    <--> trust region radius
c	iretcd	    <--> return code
c			   =0 xpls accepted as next iterate;
c			      dlt trust region for next iteration.
c			   =1 xpls unsatisfactory but accepted as next iterate
c			      because xpls-x .lt. smallest allowable
c			      step length.
c			   =2 f(xpls) too large.  continue current iteration
c			      with new reduced dlt.
c			   =3 f(xpls) sufficiently small, but quadratic model
c			      predicts f(xpls) sufficiently well to continue
c			      current iteration with new doubled dlt.
c	xplsp(n)    <--> workspace [value needs to be retained between
c			 succesive calls of k-th global step]
c	fplsp	    <--> [retain value between successive calls]
c	xpls(n)	    <--	 new iterate x[k]
c	fpls	    <--	 function value at new iterate, f(xpls)
c	mxtake	    <--	 boolean flag indicating step of maximum length used
c	ipr	     --> device to which to send output
c	method	     --> algorithm to use to solve minimization problem
c			   =1 line search
c			   =2 double dogleg
c			   =3 more-hebdon
c	udiag(n)     --> diagonal of hessian in a(.,.)
c
cstatic
      subroutine tregup(nr,n,x,f,g,a,fcn,sc,sx,nwtake,stepmx,steptl,
     +	   dlt,iretcd,xplsp,fplsp,xpls,fpls,mxtake,ipr,method,udiag)
      implicit double precision (a-h,o-z)
      dimension x(n),xpls(n),g(n)
      dimension sx(n),sc(n),xplsp(n)
      dimension a(nr,1)
      logical nwtake,mxtake
      dimension udiag(n)
c
      ipr=ipr
      mxtake=.false.
      do 100 i=1,n
	xpls(i)=x(i)+sc(i)
  100 continue
      call fcn(n,xpls,fpls)
      dltf=fpls-f
      slp=ddot(n,g,1,sc,1)
c
c	next statement added for case of compilers which do not optimize
c	evaluation of next "if" statement (in which case fplsp could be
c	undefined).
c
      if(iretcd.eq.4) fplsp=0.0d0
c$    write(ipr,961) iretcd,fpls,fplsp,dltf,slp
      if(iretcd.ne.3 .or. (fpls.lt.fplsp .and. dltf.le. 1.d-4*slp))
     +							   go to 130
c     if(iretcd.eq.3 .and. (fpls.ge.fplsp .or. dltf.gt. 1.d-4*slp))
c     then
c
c	reset xpls to xplsp and terminate global step
c
	iretcd=0
	do 110 i=1,n
	  xpls(i)=xplsp(i)
  110	continue
	fpls=fplsp
	dlt=.5*dlt
c$	write(ipr,951)
	go to 230
c     else
c
c	fpls too large
c
  130	if(dltf.le. 1.d-4*slp) go to 170
c	if(dltf.gt. 1.d-4*slp)
c	then
c$	  write(ipr,952)
	  rln=0.0d0
	  do 140 i=1,n
	    rln=max(rln,abs(sc(i))/max(abs(xpls(i)),1.0d0/sx(i)))
  140	  continue
c$	  write(ipr,962) rln
	  if(rln.ge.steptl) go to 150
c	  if(rln.lt.steptl)
c	  then
c
c	    cannot find satisfactory xpls sufficiently distinct from x
c
	    iretcd=1
c$	    write(ipr,954)
	    go to 230
c	  else
c
c	    reduce trust region and continue global step
c
  150	    iretcd=2
	    dltmp=-slp*dlt/(2.0d0*(dltf-slp))
c$	    write(ipr,963) dltmp
	    if(dltmp.ge. .1*dlt) go to 155
c	    if(dltmp.lt. .1*dlt)
c	    then
	      dlt=.1*dlt
	      go to 160
c	    else
  155	      dlt=dltmp
c	    endif
  160	    continue
c$	    write(ipr,955)
	    go to 230
c	  endif
c	else
c
c	  fpls sufficiently small
c
  170	  continue
c$	  write(ipr,958)
	  dltfp=0.0d0
	  if (method .eq. 3) go to 180
c
c	  if (method .eq. 2)
c	  then
c
	  do 177 i = 1, n
	     temp = 0.0d0
	     do 173 j = i, n
		temp = temp + (a(j, i)*sc(j))
  173	     continue
	     dltfp = dltfp + temp*temp
  177	  continue
	  go to 190
c
c	  else
c
  180	  do 187 i = 1, n
	     dltfp = dltfp + udiag(i)*sc(i)*sc(i)
	     if (i .eq. n) go to 187
	     temp = 0
	     ip1 = i + 1
	     do 183 j = ip1, n
		temp = temp + a(i, j)*sc(i)*sc(j)
  183	     continue
	     dltfp = dltfp + 2.0d0*temp
  187	  continue
c
c	  end if
c
  190	  dltfp = slp + dltfp/2.0d0
c$	  write(ipr,964) dltfp,nwtake
	  if(iretcd.eq.2 .or. (abs(dltfp-dltf).gt. .1*abs(dltf))
     +	       .or. nwtake .or. (dlt.gt. .99*stepmx)) go to 210
c	  if(iretcd.ne.2 .and. (abs(dltfp-dltf) .le. .1*abs(dltf))
c    +	       .and. (.not.nwtake) .and. (dlt.le. .99*stepmx))
c	  then
c
c	    double trust region and continue global step
c
	    iretcd=3
	    do 200 i=1,n
	      xplsp(i)=xpls(i)
  200	    continue
	    fplsp=fpls
	    dlt=min(2.0d0*dlt,stepmx)
c$	    write(ipr,959)
	    go to 230
c	  else
c
c	    accept xpls as next iterate.  choose new trust region.
c
  210	    continue
c$	    write(ipr,960)
	    iretcd=0
	    if(dlt.gt. .99*stepmx) mxtake=.true.
	    if(dltf.lt. .1*dltfp) go to 220
c	    if(dltf.ge. .1*dltfp)
c	    then
c
c	      decrease trust region for next iteration
c
	      dlt=.5*dlt
	      go to 230
c	    else
c
c	      check whether to increase trust region for next iteration
c
  220	      if(dltf.le. .75*dltfp) dlt=min(2.0d0*dlt,stepmx)
c	    endif
c	  endif
c	endif
c     endif
  230 continue
c$    write(ipr,953)
c$    write(ipr,956) iretcd,mxtake,dlt,fpls
c$    write(ipr,957)
c$    write(ipr,965) (xpls(i),i=1,n)
      return
c
c%951 format(55h tregup	   reset xpls to xplsp. termination global step)
c%952 format(26h tregup	   fpls too large.)
c%953 format(38h0tregup	   values after call to tregup)
c%954 format(54h tregup	   cannot find satisfactory xpls distinct from,
c%   +	     27h x.  terminate global step.)
c%955 format(53h tregup	   reduce trust region. continue global step.)
c%956 format(21h tregup	      iretcd=,i3/
c%   +	     21h tregup	      mxtake=,l1/
c%   +	     21h tregup	      dlt   =,e20.13/
c%   +	     21h tregup	      fpls  =,e20.13)
c%957 format(32h tregup	      new iterate (xpls))
c%958 format(35h tregup	   fpls sufficiently small.)
c%959 format(54h tregup	   double trust region.	 continue global step.)
c%960 format(50h tregup	   accept xpls as new iterate.	choose new,
c%   +	     38h trust region.	terminate global step.)
c%961 format(18h tregup	   iretcd=,i5/
c%   +	     18h tregup	   fpls	 =,e20.13/
c%   +	     18h tregup	   fplsp =,e20.13/
c%   +	     18h tregup	   dltf	 =,e20.13/
c%   +	     18h tregup	   slp	 =,e20.13)
c%962 format(18h tregup	   rln	 =,e20.13)
c%963 format(18h tregup	   dltmp =,e20.13)
c%964 format(18h tregup	   dltfp =,e20.13/
c%   +	     18h tregup	   nwtake=,l1)
c%965 format(14h tregup	      ,5(e20.13,3x))
      end
