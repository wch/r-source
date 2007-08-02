      subroutine bvalus(n,knot,coef,nk,x,s,order)
C Args
      integer n, nk, order
      double precision knot(*),coef(*),x(*),s(*)
C Local
      double precision bvalue
      integer i

      do 10 i=1,n
         s(i)=bvalue(knot,coef,nk,4,x(i),order)
 10   continue
      return
      end
