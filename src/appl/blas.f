c
c     takes the sum of the absolute values.
c     jack dongarra, linpack, 3/11/78.
c     modified 3/93 to return if incx .le. 0.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      double precision function dasum(n,dx,incx)
      double precision dx(*),dtemp
      integer i,incx,m,mp1,n,nincx
c
      dasum = 0.0d0
      dtemp = 0.0d0
      if( n.le.0 .or. incx.le.0 )return

      if(incx.ne.1) then
c     
c     code for increment not equal to 1
c     
         nincx = n*incx
         do 10 i = 1,nincx,incx
            dtemp = dtemp + dabs(dx(i))
 10      continue
         dasum = dtemp
         
      else
c
c     code for increment equal to 1
c
c
c     clean-up loop
c
         m = mod(n,6)
         if( m .ne. 0 ) then
            do 30 i = 1,m
               dtemp = dtemp + dabs(dx(i))
 30         continue
            if( n .lt. 6 ) go to 60
         endif
         mp1 = m + 1
         do 50 i = mp1,n,6
            dtemp = dtemp + dabs(dx(i))    + dabs(dx(i + 1)) +
     &                      dabs(dx(i + 2))+ dabs(dx(i + 3)) +
     &                      dabs(dx(i + 4))+ dabs(dx(i + 5))
 50      continue
 60      dasum = dtemp
      endif
      return
      end


c
c     constant times a vector plus a vector.
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      subroutine daxpy(n,da,dx,incx,dy,incy)
      double precision dx(*),dy(*),da
      integer i,incx,incy,ix,iy,m,mp1,n
c
      if(n.le.0)return
      if (da .eq. 0.0d0) return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dy(iy) = dy(iy) + da*dx(ix)
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,4)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dy(i) = dy(i) + da*dx(i)
   30 continue
      if( n .lt. 4 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,4
        dy(i) = dy(i) + da*dx(i)
        dy(i + 1) = dy(i + 1) + da*dx(i + 1)
        dy(i + 2) = dy(i + 2) + da*dx(i + 2)
        dy(i + 3) = dy(i + 3) + da*dx(i + 3)
   50 continue
      return
      end


c
c     copies a vector, x, to a vector, y.
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      subroutine  dcopy(n,dx,incx,dy,incy)
      double precision dx(*),dy(*)
      integer i,incx,incy,ix,iy,m,mp1,n
c
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dy(iy) = dx(ix)
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,7)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dy(i) = dx(i)
   30 continue
      if( n .lt. 7 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,7
        dy(i) = dx(i)
        dy(i + 1) = dx(i + 1)
        dy(i + 2) = dx(i + 2)
        dy(i + 3) = dx(i + 3)
        dy(i + 4) = dx(i + 4)
        dy(i + 5) = dx(i + 5)
        dy(i + 6) = dx(i + 6)
   50 continue
      return
      end


c
c     forms the dot product of two vectors.
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      double precision function ddot(n,dx,incx,dy,incy)
      double precision dx(*),dy(*),dtemp
      integer i,incx,incy,ix,iy,m,mp1,n
c
      ddot = 0.0d0
      dtemp = 0.0d0
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dtemp = dtemp + dx(ix)*dy(iy)
        ix = ix + incx
        iy = iy + incy
   10 continue
      ddot = dtemp
      return
c
c        code for both increments equal to 1
c
c        clean-up loop
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dtemp = dtemp + dx(i)*dy(i)
   30 continue
      if( n .lt. 5 ) go to 60
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        dtemp = dtemp + dx(i)*dy(i) + dx(i + 1)*dy(i + 1) +
     *   dx(i + 2)*dy(i + 2) + dx(i + 3)*dy(i + 3) + dx(i + 4)*dy(i + 4)
   50 continue
   60 ddot = dtemp
      return
      end


c
c     smach computes machine parameters of floating point
c     arithmetic for use in testing only.  not required by
c     linpack proper.
c
c     if trouble with automatic computation of these quantities,
c     they can be set by direct assignment statements.
c     assume the computer has
c
c        b = base of arithmetic
c        t = number of base  b  digits
c        l = smallest possible exponent
c        u = largest possible exponent
c
c     then
c
c        eps = b**(1-t)
c        tiny = 100.0*b**(-l+t)
c        huge = 0.01*b**(u-t)
c
c     dmach same as smach except t, l, u apply to
c     double precision.
c
c     cmach same as smach except if complex division
c     is done by
c
c        1/(x+i*y) = (x-i*y)/(x**2+y**2)
c
c     then
c
c        tiny = sqrt(tiny)
c        huge = sqrt(huge)
c
c
c     job is 1, 2 or 3 for epsilon, tiny and huge, respectively.
c
      double precision function dmach(job)
      integer job
      double precision eps,tiny,huge,s
c
      eps = 1.0d0
   10 eps = eps/2.0d0
      s = 1.0d0 + eps
      if (s .gt. 1.0d0) go to 10
      eps = 2.0d0*eps
c
      s = 1.0d0
   20 tiny = s
      s = s/16.0d0
      if (s*1.0 .ne. 0.0d0) go to 20
      tiny = (tiny/eps)*100.0
      huge = 1.0d0/tiny
c
      dmach = 0.0d0
      if (job .eq. 1) dmach = eps
      if (job .eq. 2) dmach = tiny
      if (job .eq. 3) dmach = huge
      return
      end


c
c  dnrm2() returns the euclidean norm of a vector via the function
c  name, so that
c
c     dnrm2 := sqrt( x'*x )
c
c
c  -- This version written on 25-October-1982.
c     Modified on 14-October-1993 to inline the call to DLASSQ.
c     Sven Hammarling, Nag Ltd.
c
c
      double precision function dnrm2 ( n, x, incx )
c     .. scalar arguments ..
      integer                           incx, n
c     .. array arguments ..
      double precision                  x( * )
c     .. parameters ..
      double precision      one         , zero
      parameter           ( one = 1.0d+0, zero = 0.0d+0 )
c     .. local scalars ..
      integer               ix
      double precision      absxi, norm, scale, ssq
c     .. intrinsic functions ..
      intrinsic             abs, sqrt
c     ..
c     .. executable statements ..
      if( n.lt.1 .or. incx.lt.1 )then
         norm  = zero
      else if( n.eq.1 )then
         norm  = abs( x( 1 ) )
      else
         scale = zero
         ssq   = one
c        the following loop is equivalent to this call to the lapack
c        auxiliary routine:
c        call dlassq( n, x, incx, scale, ssq )
c
         do 10, ix = 1, 1 + ( n - 1 )*incx, incx
            if( x( ix ).ne.zero )then
               absxi = abs( x( ix ) )
               if( scale.lt.absxi )then
                  ssq   = one   + ssq*( scale/absxi )**2
                  scale = absxi
               else
                  ssq   = ssq   +     ( absxi/scale )**2
               end if
            end if
   10    continue
         norm  = scale * sqrt( ssq )
      end if
c
      dnrm2 = norm
      return
      end
c     end of dnrm2.


c
c     applies a plane rotation.
c     jack dongarra, linpack, 3/11/78.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      subroutine  drot (n,dx,incx,dy,incy,c,s)
      double precision dx(*),dy(*),dtemp,c,s
      integer i,incx,incy,ix,iy,n
c
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c       code for unequal increments or equal increments not equal
c         to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dtemp = c*dx(ix) + s*dy(iy)
        dy(iy) = c*dy(iy) - s*dx(ix)
        dx(ix) = dtemp
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c       code for both increments equal to 1
c
   20 do 30 i = 1,n
        dtemp = c*dx(i) + s*dy(i)
        dy(i) = c*dy(i) - s*dx(i)
        dx(i) = dtemp
   30 continue
      return
      end


c
c     construct givens plane rotation.
c     jack dongarra, linpack, 3/11/78.
c
      subroutine drotg(da,db,c,s)
      double precision da,db,c,s,roe,scale,r,z
c
      roe = db
      if( dabs(da) .gt. dabs(db) ) roe = da
      scale = dabs(da) + dabs(db)
      if( scale .ne. 0.0d0 ) go to 10
         c = 1.0d0
         s = 0.0d0
         r = 0.0d0
         z = 0.0d0
         go to 20
   10 r = scale*dsqrt((da/scale)**2 + (db/scale)**2)
      r = dsign(1.0d0,roe)*r
      c = da/r
      s = db/r
      z = 1.0d0
      if( dabs(da) .gt. dabs(db) ) z = s
      if( dabs(db) .ge. dabs(da) .and. c .ne. 0.0d0 ) z = 1.0d0/c
   20 da = r
      db = z
      return
      end


c
c     scales a vector by a constant.
c     uses unrolled loops for increment equal to one.
c     jack dongarra, linpack, 3/11/78.
c     modified 3/93 to return if incx .le. 0.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      subroutine  dscal(n,da,dx,incx)
      double precision da,dx(*)
      integer i,incx,m,mp1,n,nincx
c
      if( n.le.0 .or. incx.le.0 )return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      nincx = n*incx
      do 10 i = 1,nincx,incx
        dx(i) = da*dx(i)
   10 continue
      return
c
c        code for increment equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dx(i) = da*dx(i)
   30 continue
      if( n .lt. 5 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        dx(i) = da*dx(i)
        dx(i + 1) = da*dx(i + 1)
        dx(i + 2) = da*dx(i + 2)
        dx(i + 3) = da*dx(i + 3)
        dx(i + 4) = da*dx(i + 4)
   50 continue
      return
      end


c
c     interchanges two vectors.
c     uses unrolled loops for increments equal one.
c     jack dongarra, linpack, 3/11/78.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      subroutine  dswap (n,dx,incx,dy,incy)
      double precision dx(*),dy(*),dtemp
      integer i,incx,incy,ix,iy,m,mp1,n
c
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c       code for unequal increments or equal increments not equal
c         to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dtemp = dx(ix)
        dx(ix) = dy(iy)
        dy(iy) = dtemp
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c       code for both increments equal to 1
c
c       clean-up loop
c
   20 m = mod(n,3)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dtemp = dx(i)
        dx(i) = dy(i)
        dy(i) = dtemp
   30 continue
      if( n .lt. 3 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,3
        dtemp = dx(i)
        dx(i) = dy(i)
        dy(i) = dtemp
        dtemp = dx(i + 1)
        dx(i + 1) = dy(i + 1)
        dy(i + 1) = dtemp
        dtemp = dx(i + 2)
        dx(i + 2) = dy(i + 2)
        dy(i + 2) = dtemp
   50 continue
      return
      end


c
c     finds the index of element having max. absolute value.
c     jack dongarra, linpack, 3/11/78.
c     modified 3/93 to return if incx .le. 0.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      integer function idamax(n,dx,incx)
      double precision dx(*),dmax
      integer i,incx,ix,n
c
      idamax = 0
      if( n.lt.1 .or. incx.le.0 ) return
      idamax = 1
      if(n.eq.1)return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      ix = 1
      dmax = dabs(dx(1))
      ix = ix + incx
      do 10 i = 2,n
         if(dabs(dx(ix)).gt.dmax)then
            idamax = i
            dmax = dabs(dx(ix))
         endif
         ix = ix + incx
   10 continue
      return
c
c        code for increment equal to 1
c
   20 dmax = dabs(dx(1))
      do 30 i = 2,n
         if(dabs(dx(i)).le.dmax) go to 30
         idamax = i
         dmax = dabs(dx(i))
   30 continue
      return
      end

      subroutine dgemm ( transa, transb, m, n, k, alpha, a, lda, b, ldb,
     1                   beta, c, ldc )
c     .. scalar arguments ..
      character*1        transa, transb
      integer            m, n, k, lda, ldb, ldc
      double precision   alpha, beta
c     .. array arguments ..
      double precision   a( lda, * ), b( ldb, * ), c( ldc, * )
c     ..
c
c  purpose
c  =======
c
c  dgemm  performs one of the matrix-matrix operations
c
c     c := alpha*op( a )*op( b ) + beta*c,
c
c  where  op( x ) is one of
c
c     op( x ) = x   or   op( x ) = x',
c
c  alpha and beta are scalars, and a, b and c are matrices, with op( a )
c  an m by k matrix,  op( b )  a  k by n matrix and  c an m by n matrix.
c
c  parameters
c  ==========
c
c  transa - character*1.
c           on entry, transa specifies the form of op( a ) to be used in
c           the matrix multiplication as follows:
c
c              transa = 'n' or 'n',  op( a ) = a.
c
c              transa = 't' or 't',  op( a ) = a'.
c
c              transa = 'c' or 'c',  op( a ) = a'.
c
c           unchanged on exit.
c
c  transb - character*1.
c           on entry, transb specifies the form of op( b ) to be used in
c           the matrix multiplication as follows:
c
c              transb = 'n' or 'n',  op( b ) = b.
c
c              transb = 't' or 't',  op( b ) = b'.
c
c              transb = 'c' or 'c',  op( b ) = b'.
c
c           unchanged on exit.
c
c  m      - integer.
c           on entry,  m  specifies  the number  of rows  of the  matrix
c           op( a )  and of the  matrix  c.  m  must  be at least  zero.
c           unchanged on exit.
c
c  n      - integer.
c           on entry,  n  specifies the number  of columns of the matrix
c           op( b ) and the number of columns of the matrix c. n must be
c           at least zero.
c           unchanged on exit.
c
c  k      - integer.
c           on entry,  k  specifies  the number of columns of the matrix
c           op( a ) and the number of rows of the matrix op( b ). k must
c           be at least  zero.
c           unchanged on exit.
c
c  alpha  - double precision.
c           on entry, alpha specifies the scalar alpha.
c           unchanged on exit.
c
c  a      - double precision array of dimension ( lda, ka ), where ka is
c           k  when  transa = 'n' or 'n',  and is  m  otherwise.
c           before entry with  transa = 'n' or 'n',  the leading  m by k
c           part of the array  a  must contain the matrix  a,  otherwise
c           the leading  k by m  part of the array  a  must contain  the
c           matrix a.
c           unchanged on exit.
c
c  lda    - integer.
c           on entry, lda specifies the first dimension of a as declared
c           in the calling (sub) program. when  transa = 'n' or 'n' then
c           lda must be at least  max( 1, m ), otherwise  lda must be at
c           least  max( 1, k ).
c           unchanged on exit.
c
c  b      - double precision array of dimension ( ldb, kb ), where kb is
c           n  when  transb = 'n' or 'n',  and is  k  otherwise.
c           before entry with  transb = 'n' or 'n',  the leading  k by n
c           part of the array  b  must contain the matrix  b,  otherwise
c           the leading  n by k  part of the array  b  must contain  the
c           matrix b.
c           unchanged on exit.
c
c  ldb    - integer.
c           on entry, ldb specifies the first dimension of b as declared
c           in the calling (sub) program. when  transb = 'n' or 'n' then
c           ldb must be at least  max( 1, k ), otherwise  ldb must be at
c           least  max( 1, n ).
c           unchanged on exit.
c
c  beta   - double precision.
c           on entry,  beta  specifies the scalar  beta.  when  beta  is
c           supplied as zero then c need not be set on input.
c           unchanged on exit.
c
c  c      - double precision array of dimension ( ldc, n ).
c           before entry, the leading  m by n  part of the array  c must
c           contain the matrix  c,  except when  beta  is zero, in which
c           case c need not be set on entry.
c           on exit, the array  c  is overwritten by the  m by n  matrix
c           ( alpha*op( a )*op( b ) + beta*c ).
c
c  ldc    - integer.
c           on entry, ldc specifies the first dimension of c as declared
c           in  the  calling  (sub)  program.   ldc  must  be  at  least
c           max( 1, m ).
c           unchanged on exit.
c
c
c  level 3 blas routine.
c
c  -- written on 8-february-1989.
c     jack dongarra, argonne national laboratory.
c     iain duff, aere harwell.
c     jeremy du croz, numerical algorithms group ltd.
c     sven hammarling, numerical algorithms group ltd.
c
c
c     .. external functions ..
      logical            lsame
      external           lsame
c     .. external subroutines ..
      external           xerbla
c     .. intrinsic functions ..
      intrinsic          max
c     .. local scalars ..
      logical            nota, notb
      integer            i, info, j, l, ncola, nrowa, nrowb
      double precision   temp
c     .. parameters ..
      double precision   one         , zero
      parameter        ( one = 1.0d+0, zero = 0.0d+0 )
c     ..
c     .. executable statements ..
c
c     set  nota  and  notb  as  true if  a  and  b  respectively are not
c     transposed and set  nrowa, ncola and  nrowb  as the number of rows
c     and  columns of  a  and the  number of  rows  of  b  respectively.
c
      nota  = lsame( transa, 'n' )
      notb  = lsame( transb, 'n' )
      if( nota )then
         nrowa = m
         ncola = k
      else
         nrowa = k
         ncola = m
      end if
      if( notb )then
         nrowb = k
      else
         nrowb = n
      end if
c
c     test the input parameters.
c
      info = 0
      if(      ( .not.nota                 ).and.
     1         ( .not.lsame( transa, 'c' ) ).and.
     1         ( .not.lsame( transa, 't' ) )      )then
         info = 1
      else if( ( .not.notb                 ).and.
     1         ( .not.lsame( transb, 'c' ) ).and.
     1         ( .not.lsame( transb, 't' ) )      )then
         info = 2
      else if( m  .lt.0               )then
         info = 3
      else if( n  .lt.0               )then
         info = 4
      else if( k  .lt.0               )then
         info = 5
      else if( lda.lt.max( 1, nrowa ) )then
         info = 8
      else if( ldb.lt.max( 1, nrowb ) )then
         info = 10
      else if( ldc.lt.max( 1, m     ) )then
         info = 13
      end if
      if( info.ne.0 )then
         call xerbla( 'dgemm ', info )
         return
      end if
c
c     quick return if possible.
c
      if( ( m.eq.0 ).or.( n.eq.0 ).or.
     1    ( ( ( alpha.eq.zero ).or.( k.eq.0 ) ).and.( beta.eq.one ) ) )
     1   return
c
c     and if  alpha.eq.zero.
c
      if( alpha.eq.zero )then
         if( beta.eq.zero )then
            do 20, j = 1, n
               do 10, i = 1, m
                  c( i, j ) = zero
   10          continue
   20       continue
         else
            do 40, j = 1, n
               do 30, i = 1, m
                  c( i, j ) = beta*c( i, j )
   30          continue
   40       continue
         end if
         return
      end if
c
c     start the operations.
c
      if( notb )then
         if( nota )then
c
c           form  c := alpha*a*b + beta*c.
c
            do 90, j = 1, n
               if( beta.eq.zero )then
                  do 50, i = 1, m
                     c( i, j ) = zero
   50             continue
               else if( beta.ne.one )then
                  do 60, i = 1, m
                     c( i, j ) = beta*c( i, j )
   60             continue
               end if
               do 80, l = 1, k
                  if( b( l, j ).ne.zero )then
                     temp = alpha*b( l, j )
                     do 70, i = 1, m
                        c( i, j ) = c( i, j ) + temp*a( i, l )
   70                continue
                  end if
   80          continue
   90       continue
         else
c
c           form  c := alpha*a'*b + beta*c
c
            do 120, j = 1, n
               do 110, i = 1, m
                  temp = zero
                  do 100, l = 1, k
                     temp = temp + a( l, i )*b( l, j )
  100             continue
                  if( beta.eq.zero )then
                     c( i, j ) = alpha*temp
                  else
                     c( i, j ) = alpha*temp + beta*c( i, j )
                  end if
  110          continue
  120       continue
         end if
      else
         if( nota )then
c
c           form  c := alpha*a*b' + beta*c
c
            do 170, j = 1, n
               if( beta.eq.zero )then
                  do 130, i = 1, m
                     c( i, j ) = zero
  130             continue
               else if( beta.ne.one )then
                  do 140, i = 1, m
                     c( i, j ) = beta*c( i, j )
  140             continue
               end if
               do 160, l = 1, k
                  if( b( j, l ).ne.zero )then
                     temp = alpha*b( j, l )
                     do 150, i = 1, m
                        c( i, j ) = c( i, j ) + temp*a( i, l )
  150                continue
                  end if
  160          continue
  170       continue
         else
c
c           form  c := alpha*a'*b' + beta*c
c
            do 200, j = 1, n
               do 190, i = 1, m
                  temp = zero
                  do 180, l = 1, k
                     temp = temp + a( l, i )*b( j, l )
  180             continue
                  if( beta.eq.zero )then
                     c( i, j ) = alpha*temp
                  else
                     c( i, j ) = alpha*temp + beta*c( i, j )
                  end if
  190          continue
  200       continue
         end if
      end if
c
      return
c
c     end of dgemm .
c
      end

      subroutine dtrsm ( side, uplo, transa, diag, m, n, alpha, a, lda,
     $                   b, ldb )
*     .. scalar arguments ..
      character*1        side, uplo, transa, diag
      integer            m, n, lda, ldb
      double precision   alpha
*     .. array arguments ..
      double precision   a( lda, * ), b( ldb, * )
*     ..
*
*  purpose
*  =======
*
*  dtrsm  solves one of the matrix equations
*
*     op( a )*x = alpha*b,   or   x*op( a ) = alpha*b,
*
*  where alpha is a scalar, x and b are m by n matrices, a is a unit, or
*  non-unit,  upper or lower triangular matrix  and  op( a )  is one  of
*
*     op( a ) = a   or   op( a ) = a'.
*
*  the matrix x is overwritten on b.
*
*  parameters
*  ==========
*
*  side   - character*1.
*           on entry, side specifies whether op( a ) appears on the left
*           or right of x as follows:
*
*              side = 'l' or 'l'   op( a )*x = alpha*b.
*
*              side = 'r' or 'r'   x*op( a ) = alpha*b.
*
*           unchanged on exit.
*
*  uplo   - character*1.
*           on entry, uplo specifies whether the matrix a is an upper or
*           lower triangular matrix as follows:
*
*              uplo = 'u' or 'u'   a is an upper triangular matrix.
*
*              uplo = 'l' or 'l'   a is a lower triangular matrix.
*
*           unchanged on exit.
*
*  transa - character*1.
*           on entry, transa specifies the form of op( a ) to be used in
*           the matrix multiplication as follows:
*
*              transa = 'n' or 'n'   op( a ) = a.
*
*              transa = 't' or 't'   op( a ) = a'.
*
*              transa = 'c' or 'c'   op( a ) = a'.
*
*           unchanged on exit.
*
*  diag   - character*1.
*           on entry, diag specifies whether or not a is unit triangular
*           as follows:
*
*              diag = 'u' or 'u'   a is assumed to be unit triangular.
*
*              diag = 'n' or 'n'   a is not assumed to be unit
*                                  triangular.
*
*           unchanged on exit.
*
*  m      - integer.
*           on entry, m specifies the number of rows of b. m must be at
*           least zero.
*           unchanged on exit.
*
*  n      - integer.
*           on entry, n specifies the number of columns of b.  n must be
*           at least zero.
*           unchanged on exit.
*
*  alpha  - double precision.
*           on entry,  alpha specifies the scalar  alpha. when  alpha is
*           zero then  a is not referenced and  b need not be set before
*           entry.
*           unchanged on exit.
*
*  a      - double precision array of dimension ( lda, k ), where k is m
*           when  side = 'l' or 'l'  and is  n  when  side = 'r' or 'r'.
*           before entry  with  uplo = 'u' or 'u',  the  leading  k by k
*           upper triangular part of the array  a must contain the upper
*           triangular matrix  and the strictly lower triangular part of
*           a is not referenced.
*           before entry  with  uplo = 'l' or 'l',  the  leading  k by k
*           lower triangular part of the array  a must contain the lower
*           triangular matrix  and the strictly upper triangular part of
*           a is not referenced.
*           note that when  diag = 'u' or 'u',  the diagonal elements of
*           a  are not referenced either,  but are assumed to be  unity.
*           unchanged on exit.
*
*  lda    - integer.
*           on entry, lda specifies the first dimension of a as declared
*           in the calling (sub) program.  when  side = 'l' or 'l'  then
*           lda  must be at least  max( 1, m ),  when  side = 'r' or 'r'
*           then lda must be at least max( 1, n ).
*           unchanged on exit.
*
*  b      - double precision array of dimension ( ldb, n ).
*           before entry,  the leading  m by n part of the array  b must
*           contain  the  right-hand  side  matrix  b,  and  on exit  is
*           overwritten by the solution matrix  x.
*
*  ldb    - integer.
*           on entry, ldb specifies the first dimension of b as declared
*           in  the  calling  (sub)  program.   ldb  must  be  at  least
*           max( 1, m ).
*           unchanged on exit.
*
*
*  level 3 blas routine.
*
*
*  -- written on 8-february-1989.
*     jack dongarra, argonne national laboratory.
*     iain duff, aere harwell.
*     jeremy du croz, numerical algorithms group ltd.
*     sven hammarling, numerical algorithms group ltd.
*
*
*     .. external functions ..
      logical            lsame
      external           lsame
*     .. external subroutines ..
      external           xerbla
*     .. intrinsic functions ..
      intrinsic          max
*     .. local scalars ..
      logical            lside, nounit, upper
      integer            i, info, j, k, nrowa
      double precision   temp
*     .. parameters ..
      double precision   one         , zero
      parameter        ( one = 1.0d+0, zero = 0.0d+0 )
*     ..
*     .. executable statements ..
*
*     test the input parameters.
*
      lside  = lsame( side  , 'l' )
      if( lside )then
         nrowa = m
      else
         nrowa = n
      end if
      nounit = lsame( diag  , 'n' )
      upper  = lsame( uplo  , 'u' )
*
      info   = 0
      if(      ( .not.lside                ).and.
     $         ( .not.lsame( side  , 'r' ) )      )then
         info = 1
      else if( ( .not.upper                ).and.
     $         ( .not.lsame( uplo  , 'l' ) )      )then
         info = 2
      else if( ( .not.lsame( transa, 'n' ) ).and.
     $         ( .not.lsame( transa, 't' ) ).and.
     $         ( .not.lsame( transa, 'c' ) )      )then
         info = 3
      else if( ( .not.lsame( diag  , 'u' ) ).and.
     $         ( .not.lsame( diag  , 'n' ) )      )then
         info = 4
      else if( m  .lt.0               )then
         info = 5
      else if( n  .lt.0               )then
         info = 6
      else if( lda.lt.max( 1, nrowa ) )then
         info = 9
      else if( ldb.lt.max( 1, m     ) )then
         info = 11
      end if
      if( info.ne.0 )then
         call xerbla( 'dtrsm ', info )
         return
      end if
*
*     quick return if possible.
*
      if( n.eq.0 )
     $   return
*
*     and when  alpha.eq.zero.
*
      if( alpha.eq.zero )then
         do 20, j = 1, n
            do 10, i = 1, m
               b( i, j ) = zero
   10       continue
   20    continue
         return
      end if
*
*     start the operations.
*
      if( lside )then
         if( lsame( transa, 'n' ) )then
*
*           form  b := alpha*inv( a )*b.
*
            if( upper )then
               do 60, j = 1, n
                  if( alpha.ne.one )then
                     do 30, i = 1, m
                        b( i, j ) = alpha*b( i, j )
   30                continue
                  end if
                  do 50, k = m, 1, -1
                     if( b( k, j ).ne.zero )then
                        if( nounit )
     $                     b( k, j ) = b( k, j )/a( k, k )
                        do 40, i = 1, k - 1
                           b( i, j ) = b( i, j ) - b( k, j )*a( i, k )
   40                   continue
                     end if
   50             continue
   60          continue
            else
               do 100, j = 1, n
                  if( alpha.ne.one )then
                     do 70, i = 1, m
                        b( i, j ) = alpha*b( i, j )
   70                continue
                  end if
                  do 90 k = 1, m
                     if( b( k, j ).ne.zero )then
                        if( nounit )
     $                     b( k, j ) = b( k, j )/a( k, k )
                        do 80, i = k + 1, m
                           b( i, j ) = b( i, j ) - b( k, j )*a( i, k )
   80                   continue
                     end if
   90             continue
  100          continue
            end if
         else
*
*           form  b := alpha*inv( a' )*b.
*
            if( upper )then
               do 130, j = 1, n
                  do 120, i = 1, m
                     temp = alpha*b( i, j )
                     do 110, k = 1, i - 1
                        temp = temp - a( k, i )*b( k, j )
  110                continue
                     if( nounit )
     $                  temp = temp/a( i, i )
                     b( i, j ) = temp
  120             continue
  130          continue
            else
               do 160, j = 1, n
                  do 150, i = m, 1, -1
                     temp = alpha*b( i, j )
                     do 140, k = i + 1, m
                        temp = temp - a( k, i )*b( k, j )
  140                continue
                     if( nounit )
     $                  temp = temp/a( i, i )
                     b( i, j ) = temp
  150             continue
  160          continue
            end if
         end if
      else
         if( lsame( transa, 'n' ) )then
*
*           form  b := alpha*b*inv( a ).
*
            if( upper )then
               do 210, j = 1, n
                  if( alpha.ne.one )then
                     do 170, i = 1, m
                        b( i, j ) = alpha*b( i, j )
  170                continue
                  end if
                  do 190, k = 1, j - 1
                     if( a( k, j ).ne.zero )then
                        do 180, i = 1, m
                           b( i, j ) = b( i, j ) - a( k, j )*b( i, k )
  180                   continue
                     end if
  190             continue
                  if( nounit )then
                     temp = one/a( j, j )
                     do 200, i = 1, m
                        b( i, j ) = temp*b( i, j )
  200                continue
                  end if
  210          continue
            else
               do 260, j = n, 1, -1
                  if( alpha.ne.one )then
                     do 220, i = 1, m
                        b( i, j ) = alpha*b( i, j )
  220                continue
                  end if
                  do 240, k = j + 1, n
                     if( a( k, j ).ne.zero )then
                        do 230, i = 1, m
                           b( i, j ) = b( i, j ) - a( k, j )*b( i, k )
  230                   continue
                     end if
  240             continue
                  if( nounit )then
                     temp = one/a( j, j )
                     do 250, i = 1, m
                       b( i, j ) = temp*b( i, j )
  250                continue
                  end if
  260          continue
            end if
         else
*
*           form  b := alpha*b*inv( a' ).
*
            if( upper )then
               do 310, k = n, 1, -1
                  if( nounit )then
                     temp = one/a( k, k )
                     do 270, i = 1, m
                        b( i, k ) = temp*b( i, k )
  270                continue
                  end if
                  do 290, j = 1, k - 1
                     if( a( j, k ).ne.zero )then
                        temp = a( j, k )
                        do 280, i = 1, m
                           b( i, j ) = b( i, j ) - temp*b( i, k )
  280                   continue
                     end if
  290             continue
                  if( alpha.ne.one )then
                     do 300, i = 1, m
                        b( i, k ) = alpha*b( i, k )
  300                continue
                  end if
  310          continue
            else
               do 360, k = 1, n
                  if( nounit )then
                     temp = one/a( k, k )
                     do 320, i = 1, m
                        b( i, k ) = temp*b( i, k )
  320                continue
                  end if
                  do 340, j = k + 1, n
                     if( a( j, k ).ne.zero )then
                        temp = a( j, k )
                        do 330, i = 1, m
                           b( i, j ) = b( i, j ) - temp*b( i, k )
  330                   continue
                     end if
  340             continue
                  if( alpha.ne.one )then
                     do 350, i = 1, m
                        b( i, k ) = alpha*b( i, k )
  350                continue
                  end if
  360          continue
            end if
         end if
      end if
*
      return
*
*     end of dtrsm .
*
      end

      logical          function lsame( ca, cb )
c
c  -- lapack auxiliary routine (version 2.0) --
c     univ. of tennessee, univ. of california berkeley, nag ltd.,
c     courant institute, argonne national lab, and rice university
c     january 31, 1994
c
c     .. scalar arguments ..
      character          ca, cb
c     ..
c
c  purpose
c  =======
c
c  lsame returns .true. if ca is the same letter as cb regardless of
c  case.
c
c  arguments
c  =========
c
c  ca      (input) character*1
c  cb      (input) character*1
c          ca and cb specify the single characters to be compared.
c
c =====================================================================
c
c     .. intrinsic functions ..
      intrinsic          ichar
c     ..
c     .. local scalars ..
      integer            inta, intb, zcode
c     ..
c     .. executable statements ..
c
c     test if the characters are equal
c
      lsame = ca.eq.cb
      if( lsame )
     1   return
c
c     now test for equivalence if both characters are alphabetic.
c
      zcode = ichar( 'z' )
c
c     use 'z' rather than 'a' so that ascii can be detected on prime
c     machines, on which ichar returns a value with bit 8 set.
c     ichar('a') on prime machines returns 193 which is the same as
c     ichar('a') on an ebcdic machine.
c
      inta = ichar( ca )
      intb = ichar( cb )
c
      if( zcode.eq.90 .or. zcode.eq.122 ) then
c
c        ascii is assumed - zcode is the ascii code of either lower or
c        upper case 'z'.
c
         if( inta.ge.97 .and. inta.le.122 ) inta = inta - 32
         if( intb.ge.97 .and. intb.le.122 ) intb = intb - 32
c
      else if( zcode.eq.233 .or. zcode.eq.169 ) then
c
c        ebcdic is assumed - zcode is the ebcdic code of either lower or
c        upper case 'z'.
c
         if( inta.ge.129 .and. inta.le.137 .or.
     1       inta.ge.145 .and. inta.le.153 .or.
     1       inta.ge.162 .and. inta.le.169 ) inta = inta + 64
         if( intb.ge.129 .and. intb.le.137 .or.
     1       intb.ge.145 .and. intb.le.153 .or.
     1       intb.ge.162 .and. intb.le.169 ) intb = intb + 64
c
      else if( zcode.eq.218 .or. zcode.eq.250 ) then
c
c        ascii is assumed, on prime machines - zcode is the ascii code
c        plus 128 of either lower or upper case 'z'.
c
         if( inta.ge.225 .and. inta.le.250 ) inta = inta - 32
         if( intb.ge.225 .and. intb.le.250 ) intb = intb - 32
      end if
      lsame = inta.eq.intb
c
c     return
c
c     end of lsame
c
      end

c     SUBROUTINE XERBLA( SRNAME, INFO )
c
c  -- LAPACK auxiliary routine (preliminary version) --
c     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
c     Courant Institute, Argonne National Lab, and Rice University
c     February 29, 1992
c
c     .. Scalar Arguments ..
c     CHARACTER*6        SRNAME
c     INTEGER            INFO
c     ..
c
c  Purpose
c  =======
c
c  XERBLA  is an error handler for the LAPACK routines.
c  It is called by an LAPACK routine if an input parameter has an
c  invalid value.  A message is printed and execution stops.
c
c  Installers may consider modifying the STOP statement in order to
c  call system-specific exception-handling facilities.
c
c  Arguments
c  =========
c
c  SRNAME  (input) CHARACTER*6
c          The name of the routine which called XERBLA.
c
c  INFO    (input) INTEGER
c          The position of the invalid parameter in the parameter list
c          of the calling routine.
c
c  This has been replaced by a C routine in src/main/print.c
