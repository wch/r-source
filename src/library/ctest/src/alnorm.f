c This file includes the Applied Statistics algorithm AS 66 for calculating
c the tail area under the normal curve, and two alternative routines which
c give higher accuracy.   The latter have been contributed by Alan Miller of
c CSIRO Division of Mathematics & Statistics, Clayton, Victoria.   Notice
c that each function or routine has different call arguments.
c
c
      double precision function alnorm(x,upper)
c
c         Algorithm AS66 Applied Statistics (1973) vol22 no.3
c
c       Evaluates the tail area of the standardised normal curve
c       from x to infinity       ( = 1 -pnorm(x) ) if upper is .true. or
c       from minus infinity to x ( = pnorm(x) )    if upper is .false.
c
      double precision zero,one,half
      double precision con,z,y,x
      double precision p,q,r,a1,a2,a3,b1,b2,c1,c2,c3,c4,c5,c6
      double precision d1,d2,d3,d4,d5
      logical upper,up
c*** machine dependent constants
      double precision ltone,utzero
      data zero/0.0d0/, one/1.0d0/, half/0.5d0/
      data ltone/7.0d0/,utzero/18.66d0/
      data con/1.28d0/
      data p/0.398942280444d0/,q/0.39990348504d0/,r/0.398942280385d0/
      data a1/5.75885480458d0/,a2/2.62433121679d0/,a3/5.92885724438d0/
      data b1/-29.8213557807d0/,b2/48.6959930692d0/
      data c1/-3.8052d-8/,c2/3.98064794d-4/,c3/-0.151679116635d0/
      data c4/4.8385912808d0/,c5/0.742380924027d0/,c6/3.99019417011d0/
      data d1/1.00000615302d0/,d2/1.98615381364d0/,d3/5.29330324926d0/
      data d4/-15.1508972451d0/,d5/30.789933034d0/
c
      up=upper
      z=x
      if(z.lt.zero) then
         up=.not.up
         z=-z
      endif
      if(z.gt.utzero .or. (.not.up .and. z.gt.ltone)) then
         alnorm=zero
      else
         y=half*z*z
         if(z.le.con) then
            alnorm=half-z*(p-q*y/(y+a1+b1/(y+a2+b2/(y+a3))))
         else
            alnorm=r*dexp(-y)/(z+c1+d1/(z+c2+d2/(z+c3+d3/(z+c4+d4/
     2           (z+c5+d5/(z+c6))))))
         endif
      endif
      if(.not.up) alnorm= one-alnorm
      return
      end
