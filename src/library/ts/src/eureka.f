      subroutine eureka (lr,r,g,f,var,a)
c
c      solves Toepliz matrix equation toep(r)f=g(1+.)
c      by Levinson's algorithm
c      a is a workspace of size lr, the number
c      of equations
c
      integer lr,l,l1,l2,i,j,k
      double precision r(lr+1), g(lr), f(lr,lr), a(lr), var(lr)
      double precision v,d,q,hold,sum
      v = r(1)
      d = r(2)
      a(1) = 1.
      f(1,1) = g(2)/v
      q = f(1,1)*r(2)
      var(1) = (1 +f(1,1)*f(1,1))*r(1) - 2*q
      if (lr.eq.1) return
      do 60 l = 2,lr
        a(l) = -d/v
        if (l.gt.2) then
          l1 = (l-2)/2
          l2 = l1+1
          do 10 j = 2,l2
            hold = a(j)
            k = l-j+1
            a(j) = a(j)+a(l)*a(k)
   10       a(k) = a(k)+a(l)*hold
          if (2*l1.ne.l-2) a(l2+1) = a(l2+1)*(1.0+a(l))
        endif
        v = v+a(l)*d
        f(l,l) = (g(l+1)-q)/v
        do 40 j = 1,l-1
   40     f(l,j) = f(l-1,j)+f(l,l)*a(l-j+1)
c  estimate the innovations variance
        sum = r(1)
        do 45 i = 1,l
 45        sum = sum - 2.0*f(l,i)*r(i+1)
        do 48 i = 1,l
           do 48 j = 1,l
 48           sum = sum + f(l,i)*f(l,j)*r(1+iabs(i-j))
        var(l) = sum
        if (l.eq.lr) return
        d = 0.0
        q = 0.0
        do 50 i = 1,l
          k = l-i+2
          d = d+a(i)*r(k)
   50     q = q+f(l,i)*r(k)
   60   continue    
      return
      end
