c
c     dqrdc2 uses householder transformations to compute the qr
c     factorization of an n by p matrix x.  a limited column
c     pivoting strategy based on the 2-norms of the reduced columns
c     moves columns with near-zero norm to the right-hand edge of
c     the x matrix.  this strategy means that sequential one
c     degree-of-freedom effects can be computed in a natural way.
c
c     i am very nervous about modifying linpack code in this way.
c     if you are a computational linear algebra guru and you really
c     understand how to solve this problem please feel free to
c     suggest improvements to this code.
c
c     Another change was to compute the rank.
c
c     on entry
c
c        x       double precision(ldx,p), where ldx .ge. n.
c                x contains the matrix whose decomposition is to be
c                computed.
c
c        ldx     integer.
c                ldx is the leading dimension of the array x.
c
c        n       integer.
c                n is the number of rows of the matrix x.
c
c        p       integer.
c                p is the number of columns of the matrix x.
c
c        tol     double precision
c                tol is the nonnegative tolerance used to
c                determine the subset of the columns of x
c                included in the solution.
c
c        jpvt    integer(p).
c                integers which are swapped in the same way as the
c                the columns of x during pivoting.  on entry these
c                should be set equal to the column indices of the
c                columns of the x matrix (typically 1 to p).
c
c        work    double precision(p,2).
c                work is a work array.
c
c     on return
c
c        x       x contains in its upper triangle the upper
c                triangular matrix r of the qr factorization.
c                below its diagonal x contains information from
c                which the orthogonal part of the decomposition
c                can be recovered.  note that if pivoting has
c                been requested, the decomposition is not that
c                of the original matrix x but that of x
c                with its columns permuted as described by jpvt.
c
c        k       integer.
c                k contains the number of columns of x judged
c                to be linearly independent.
c
c        qraux   double precision(p).
c                qraux contains further information required to recover
c                the orthogonal part of the decomposition.
c
c        jpvt    jpvt(k) contains the index of the column of the
c                original matrix that has been interchanged into
c                the k-th column.
c
c     original (dqrdc.f) linpack version dated 08/14/78 .
c     g.w. stewart, university of maryland, argonne national lab.
c
c     this version dated 22 august 1995
c     ross ihaka
c
c     bug fixes 29 September 1999 BDR (p > n case, inaccurate ranks)
c
c
c     dqrdc2 uses the following functions and subprograms.
c
c     blas daxpy,ddot,dscal,dnrm2
c     fortran dabs,dmax1,min0,dsqrt
c
      subroutine dqrdc2(x,ldx,n,p,tol,k,qraux,jpvt,work)
      integer ldx,n,p
      integer jpvt(*)
      double precision x(ldx,*),qraux(*),work(p,2),tol
c
c     internal variables
c
      integer i,j,l,lp1,lup,k
      double precision dnrm2,tt,ttt
      double precision ddot,nrmxl,t
c
c
c     compute the norms of the columns of x.
c
      do 70 j = 1, p
         qraux(j) = dnrm2(n,x(1,j),1)
         work(j,1) = qraux(j)
         work(j,2) = qraux(j)
         if(work(j,2) .eq. 0.0d0) work(j,2) = 1.0d0
   70 continue
c
c     perform the householder reduction of x.
c
      lup = min0(n,p)
      k = p + 1
      do 200 l = 1, lup
c
c     previous version only cycled l to lup
c
c     cycle the columns from l to p left-to-right until one
c     with non-negligible norm is located.  a column is considered
c     to have become negligible if its norm has fallen below
c     tol times its original norm.  the check for l .le. k
c     avoids infinite cycling.
c
   80    continue
         if (l .ge. k .or. qraux(l) .ge. work(l,2)*tol) go to 120
            lp1 = l+1
            do 100 i=1,n
               t = x(i,l)
               do 90 j=lp1,p
                  x(i,j-1) = x(i,j)
   90          continue
               x(i,p) = t
  100       continue
            i = jpvt(l)
            t = qraux(l)
            tt = work(l,1)
            ttt = work(l,2)
            do 110 j=lp1,p
               jpvt(j-1) = jpvt(j)
               qraux(j-1) = qraux(j)
               work(j-1,1) = work(j,1)
               work(j-1,2) = work(j,2)
  110       continue
            jpvt(p) = i
            qraux(p) = t
            work(p,1) = tt
            work(p,2) = ttt
            k = k - 1
            go to 80
  120    continue
         if (l .eq. n) go to 190
c
c           compute the householder transformation for column l.
c
            nrmxl = dnrm2(n-l+1,x(l,l),1)
            if (nrmxl .eq. 0.0d0) go to 180
               if (x(l,l) .ne. 0.0d0) nrmxl = dsign(nrmxl,x(l,l))
               call dscal(n-l+1,1.0d0/nrmxl,x(l,l),1)
               x(l,l) = 1.0d0 + x(l,l)
c
c              apply the transformation to the remaining columns,
c              updating the norms.
c
               lp1 = l + 1
               if (p .lt. lp1) go to 170
               do 160 j = lp1, p
                  t = -ddot(n-l+1,x(l,l),1,x(l,j),1)/x(l,l)
                  call daxpy(n-l+1,t,x(l,l),1,x(l,j),1)
                  if (qraux(j) .eq. 0.0d0) go to 150
                     tt = 1.0d0 - (dabs(x(l,j))/qraux(j))**2
                     tt = dmax1(tt,0.0d0)
                     t = tt
c
c modified 9/99 by BDR. Re-compute norms if there is large reduction
c The tolerance here is on the squared norm
c In this version we need accurate norms, so re-compute often.
c  work(j,1) is only updated in one case: looks like a bug -- no longer used
c
c                     tt = 1.0d0 + 0.05d0*tt*(qraux(j)/work(j,1))**2
c                     if (tt .eq. 1.0d0) go to 130
                     if (dabs(t) .lt. 1d-6) go to 130
                        qraux(j) = qraux(j)*dsqrt(t)
                     go to 140
  130                continue
                        qraux(j) = dnrm2(n-l,x(l+1,j),1)
                        work(j,1) = qraux(j)
  140                continue
  150             continue
  160          continue
  170          continue
c
c              save the transformation.
c
               qraux(l) = x(l,l)
               x(l,l) = -nrmxl
  180       continue
  190    continue
  200 continue
      k = min0(k - 1, n)
      return
      end
