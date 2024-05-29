C     Dqrdc2 is a *modification* of Linpack's dqrdc ('DQRDC') for R
c
c     dqrdc2 uses Householder transformations to compute the qr
c     factorization of an n by p matrix x.  A limited column
c     pivoting strategy based on the 2-norms of the reduced columns
c     moves columns with near-zero norm to the right-hand edge of
c     the x matrix.  This strategy means that sequential one
c     degree-of-freedom effects can be computed in a natural way.
c
c     I am very nervous about modifying linpack code in this way.
c     If you are a computational linear algebra guru and you really
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
c                to be linearly independent, i.e., "the rank"
c
c        qraux   double precision(p).
c                qraux contains further information required to recover
c                the orthogonal part of the decomposition.
c
c        jpvt    jpvt(j) contains the index of the column of the
c                original matrix that has been interchanged into
c                the j-th column.  Consequently, jpvt[] codes a
c		 permutation of 1:p; it is called 'pivot' in R

c
c     Original (dqrdc.f) linpack version dated 08/14/78 .
c     G.W. Stewart, University of Maryland, Argonne National Lab.
c
C     This version dated 22 August 1995
C     Ross Ihaka
c
c     Bug fixes 29 September 1999 BDR (p > n case, inaccurate ranks)
c     Fortran modernized to F77, 2024-05 BDR
c
c
c     dqrdc2 uses the following functions and subprograms.
c
c     blas daxpy,ddot,dscal,dnrm2
c     fortran abs,max,min,sqrt
c
      subroutine dqrdc2(x,ldx,n,p,tol,k,qraux,jpvt,work)
      integer ldx,n,p
      integer jpvt(p)
      double precision x(ldx,p),qraux(p),work(p,2),tol
c
c     internal variables
c
      integer i,j,l,lup,k
      double precision dnrm2,tt,ttt
      double precision ddot,nrmxl,t
c
c
c     compute the norms of the columns of x.
c
      if (n .gt. 0) then
c       avoid accessing element beyond the bound
         do j = 1, p
            qraux(j) = dnrm2(n,x(1,j),1)
            work(j,1) = qraux(j)
            work(j,2) = qraux(j)
            if(work(j,2) .eq. 0.0d0) work(j,2) = 1.0d0
         end do                 ! j
      end if
c
c     perform the householder reduction of x.
c
      lup = min(n,p)
      k = p + 1
      do l = 1, lup
c
c     previous version only cycled l to lup
c
c     cycle the columns from l to p left-to-right until one
c     with non-negligible norm is located.  a column is considered
c     to have become negligible if its norm has fallen below
c     tol times its original norm.  the check for l .le. k
c     avoids infinite cycling.
c
         do
            if (l .ge. k .or. qraux(l) .ge. work(l,2)*tol) exit
            do i=1,n
               t = x(i,l)
               do j=l+1,p
                  x(i,j-1) = x(i,j)
               end do           ! j
               x(i,p) = t
            end do              ! i
            i = jpvt(l)
            t = qraux(l)
            tt = work(l,1)
            ttt = work(l,2)
            do j=l+1,p
               jpvt(j-1) = jpvt(j)
               qraux(j-1) = qraux(j)
               work(j-1,1) = work(j,1)
               work(j-1,2) = work(j,2)
            end do              ! j
            jpvt(p) = i
            qraux(p) = t
            work(p,1) = tt
            work(p,2) = ttt
            k = k - 1
         end do                 ! (no index)
         if (l .ne. n) then
c
c           compute the householder transformation for column l.
c
            nrmxl = dnrm2(n-l+1,x(l,l),1)
            if (nrmxl .ne. 0.0d0) then
               if (x(l,l) .ne. 0.0d0) nrmxl = sign(nrmxl,x(l,l))
               call dscal(n-l+1,1.0d0/nrmxl,x(l,l),1)
               x(l,l) = 1.0d0 + x(l,l)
c
c              apply the transformation to the remaining columns,
c              updating the norms.
c
               if (p .ge. l+1) then
                  do j = l+1, p
                     t = -ddot(n-l+1,x(l,l),1,x(l,j),1)/x(l,l)
                     call daxpy(n-l+1,t,x(l,l),1,x(l,j),1)
                     if (qraux(j) .ne. 0.0d0) then
                        tt = 1.0d0 - (abs(x(l,j))/qraux(j))**2
                        tt = max(tt,0.0d0)
                        t = tt
c
c Modified 9/99 by BDR. Re-compute norms if there is large reduction
c The tolerance here is on the squared norm
c In this version we need accurate norms, so re-compute often.
                        if (dabs(t) .ge. 1d-6) then
                           qraux(j) = qraux(j)*sqrt(t)
                        else
                           qraux(j) = dnrm2(n-l,x(l+1,j),1)
                           work(j,1) = qraux(j)
                        end if
                     end if
                  end do        ! j
               end if
c
c              Save the transformation.
c
               qraux(l) = x(l,l)
               x(l,l) = -nrmxl
            end if
         end if
      end do                    ! l
      k = min(k - 1, n)
      return
      end
