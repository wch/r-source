C R's  optimize() :   function	fmin(ax,bx,f,tol)
C =    ==========		~~~~~~~~~~~~~~~~~
c
c      an approximation  x  to the point where  f  attains a minimum  on
c  the interval  (ax,bx)  is determined.
c
c  input..
c
c  ax    left endpoint of initial interval
c  bx    right endpoint of initial interval
c  f     function subprogram which evaluates  f(x)  for any  x
c        in the interval  (ax,bx)
c  tol   desired length of the interval of uncertainty of the final
c        result (.ge.0.)
c
c  output..
c
c  fmin  abcissa approximating the point where  f  attains a
c        minimum
c
c      the method used is a combination of  golden  section  search  and
c  successive parabolic interpolation.  convergence is never much slower
c  than  that  for  a  fibonacci search.  if  f  has a continuous second
c  derivative which is positive at the minimum (which is not  at  ax  or
c  bx),  then  convergence  is  superlinear, and usually of the order of
c  about  1.324....
c      the function  f  is never evaluated at two points closer together
c  than  eps*abs(fmin)+(tol/3), where eps is  approximately  the  square
c  root  of  the  relative  machine  precision.   if   f   is a unimodal
c  function and the computed values of   f   are  always  unimodal  when
c  separated  by  at least  eps*abs(x)+(tol/3), then  fmin  approximates
c  the abcissa of the global minimum of  f  on the interval  ax,bx  with
c  an error less than  3*eps*abs(fmin)+tol.  if   f   is  not  unimodal,
c  then fmin may approximate a local, but perhaps non-global, minimum to
c  the same accuracy.
c      this function subprogram is a slightly modified  version  of  the
c  algol  60 procedure  localmin  given in richard brent, algorithms for
c  minimization without derivatives, prentice-hall, inc. (1973).
c
c
      double precision function fmin(ax,bx,f,tol)

      implicit none
      double precision ax,bx,f,tol
c
      EXTERNAL  d1mach
      double precision  dabs,dsqrt,d1mach

      double precision  a,b,c,d,e,eps,xm,p,q,r,tol1,t2,u,v,w,fu,fv,fw,
     2    fx,x,tol3

c
c  c is the squared inverse of the golden ratio
      c=0.5d0*(3.0d0-dsqrt(5.0d0))
c
c  eps is approximately the square root of the relative machine
c  precision.
c
      eps=d1mach(4)
      tol1=eps+1.0d0
      eps=dsqrt(eps)
c
      a=ax
      b=bx
      v=a+c*(b-a)
      w=v
      x=v
c
c  -Wall indicates that d may be used before being assigned
c
      d=0.0d0
      e=0.0d0
      fx=f(x)
      fv=fx
      fw=fx
      tol3=tol/3.0d0
c
c  main loop starts here -----------------------------------
c
   20 xm=0.5d0*(a+b)
      tol1=eps*dabs(x)+tol3
      t2=2.0d0*tol1
c
c  check stopping criterion
c
      if (dabs(x-xm).le.(t2-0.5d0*(b-a))) go to 900
      p=0.0d0
      q=0.0d0
      r=0.0d0
      if (dabs(e).gt.tol1) then
c     
c     fit parabola
c     
         r=(x-w)*(fx-fv)
         q=(x-v)*(fx-fw)
         p=(x-v)*q-(x-w)*r
         q=2.0d0*(q-r)
         if (q.gt.0.0d0) then
            p=-p
         else
            q=-q
         endif
         r=e
         e=d
      endif

      if ((dabs(p).ge.dabs(0.5d0*q*r)).or.(p.le.q*(a-x))
     2                                .or.(p.ge.q*(b-x))) then

c
c     a golden-section step
c     
         if (x.lt.xm) then
            e=b-x
         else
            e=a-x
         endif
         d=c*e
         
      else
c
c     a parabolic-interpolation step
c     
         d=p/q
         u=x+d
c     
c     f must not be evaluated too close to ax or bx
c     
         if (((u-a).ge.t2).and.((b-u).ge.t2)) go to 90
         d=tol1
         if (x.ge.xm) d=-d
      endif
         
c     
c f must not be evaluated too close to x
c     
 90   if (dabs(d).ge.tol1) then
         u=x+d
      else
         if (d.gt.0.0d0) then
            u=x+tol1
         else
            u=x-tol1
         endif
      endif
      fu=f(u)

c
c  update  a, b, v, w, and x
c
      if (fx.le.fu) then
         if (u.lt.x) then
            a=u
         else
            b=u
         endif
      endif

      if (fu.le.fx) then
         
         if (u.lt.x) then
            b=x
         else
            a=x
         endif
         v=w
         fv=fw
         w=x
         fw=fx
         x=u
         fx=fu
         
      else

 170     if ((fu.le.fw).or.(w.eq.x)) then
            v=w
            fv=fw
            w=u
            fw=fu
         else
            if ((fu.le.fv).or.(v.eq.x).or.(v.eq.w)) then
               v=u
               fv=fu
            endif
         endif
      endif
      
      go to 20
c
c  end of main loop
c
  900 fmin=x
      return
      end
