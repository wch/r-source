c
c     dqrfit is a subroutine to compute least squares solutions
c     to the system
c
c     (1)               x * b = y
c
c     which may be either under-determined or over-determined.
c     the user must supply a tolerance to limit the columns of
c     x used in computing the solution.  in effect, a set of
c     columns with a condition number approximately bounded by
c     1/tol is used, the other components of b being set to zero.
c
c     on entry
c
c        x      double precision(n,p).
c               x contains n-by-p coefficient matrix of
c               the system (1), x is destroyed by dqrfit.
c
c        n      the number of rows of the matrix x.
c
c        p      the number of columns of the matrix x.
c
c        y      double precision(n,ny)
c               y contains the right hand side(s) of the system (1).
c
c        ny     the number of right hand sides of the system (1).
c
c        tol    double precision
c               tol is the nonnegative tolerance used to
c               determine the subset of columns of x included
c               in the solution.  columns are pivoted out of
c               decomposition if
c
c        jpvt   integer(p)
c               the values in jpvt are permuted in the same
c               way as the columns of x.  this can be useful
c               in unscrambling coefficients etc.
c
c        work   double precision(2*p)
c               work is an array used by dqrdc2 and dqrsl.
c
c     on return
c
c        x      contains the output array from dqrdc2.
c               namely the qr decomposition of x stored in
c               compact form.
c
c        b      double precision(p,ny)
c               b contains the solution vectors with rows permuted
c               in the same way as the columns of x.  components
c               corresponding to columns not used are set to zero.
c
c        rsd    double precision(n,ny)
c               rsd contains the residual vectors y-x*b.
c
c        qty    double precision(n,ny)     t
c               qty contains the vectors  q y.   note that
c               the initial p elements of this vector are
c               permuted in the same way as the columns of x.
c
c        k      integer
c               k contains the number of columns used in the
c               solution.
c
c        jpvt   has its contents permuted as described above.
c
c        qraux  double precision(p)
c               qraux contains auxiliary information on the
c               qr decomposition of x.
c
c
c     on return the arrays x, jpvt and qraux contain the
c     usual output from dqrdc, so that the qr decomposition
c     of x with pivoting is fully available to the user.
c     in particular, columns jpvt(1), jpvt(2),...,jpvt(k)
c     were used in the solution, and the condition number
c     associated with those columns is estimated by
c     abs(x(1,1)/x(k,k)).
c
c     dqrfit uses the linpack routines dqrdc and dqrsl.
c
      subroutine dqrls(x,n,p,y,ny,tol,b,rsd,qty,k,jpvt,qraux,work)
      integer n,p,ny,k,jpvt(p)
      double precision x(n,p),y(n,ny),tol,b(p,ny),rsd(n,ny),
     .                 qty(n,ny),qraux(p),work(p)
c
c     internal variables.
c
      integer info,j,jj,kk
c
c     reduce x.
c
      call dqrdc2(x,n,n,p,tol,k,qraux,jpvt,work)
c
c     solve the truncated least squares problem for each rhs.
c
      if(k .gt. 0) then
         do 20 jj=1,ny
   20       call dqrsl(x,n,n,k,qraux,y(1,jj),rsd(1,jj),qty(1,jj),
     1           b(1,jj),rsd(1,jj),rsd(1,jj),1110,info)
      else
         do 30 i=1,n
            do 30 jj=1,ny
   30           rsd(i,jj) = y(i,jj)
      endif
c
c     set the unused components of b to zero.
c
      kk = k + 1
      do 50 j=kk,p
         do 40 jj=1,ny
            b(j,jj) = 0.d0
   40    continue
   50 continue
      return
      end
