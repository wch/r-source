      subroutine dtrco(t,ldt,n,rcond,z,job)
      integer ldt,n,job
      double precision t(ldt,1),z(1)
      double precision rcond
c
c     dtrco estimates the condition of a double precision triangular
c     matrix.
c
c     on entry
c
c        t       double precision(ldt,n)
c                t contains the triangular matrix. the zero
c                elements of the matrix are not referenced, and
c                the corresponding elements of the array can be
c                used to store other information.
c
c        ldt     integer
c                ldt is the leading dimension of the array t.
c
c        n       integer
c                n is the order of the system.
c
c        job     integer
c                = 0         t  is lower triangular.
c                = nonzero   t  is upper triangular.
c
c     on return
c
c        rcond   double precision
c                an estimate of the reciprocal condition of  t .
c                for the system  t*x = b , relative perturbations
c                in  t  and  b  of size  epsilon  may cause
c                relative perturbations in  x  of size  epsilon/rcond .
c                if  rcond  is so small that the logical expression
c                           1.0 + rcond .eq. 1.0
c                is true, then  t  may be singular to working
c                precision.  in particular,  rcond  is zero  if
c                exact singularity is detected or the estimate
c                underflows.
c
c        z       double precision(n)
c                a work vector whose contents are usually unimportant.
c                if  t  is close to a singular matrix, then  z  is
c                an approximate null vector in the sense that
c                norm(a*z) = rcond*norm(a)*norm(z) .
c
c     linpack. this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
c     subroutines and functions
c
c     blas daxpy,dscal,dasum
c     fortran dabs,dmax1,dsign
c
c     internal variables
c
      double precision w,wk,wkm,ek
      double precision tnorm,ynorm,s,sm,dasum
      integer i1,j,j1,j2,k,kk,l
      logical lower
c
      lower = job .eq. 0
c
c     compute 1-norm of t
c
      tnorm = 0.0d0
      do 10 j = 1, n
         l = j
         if (lower) l = n + 1 - j
         i1 = 1
         if (lower) i1 = j
         tnorm = dmax1(tnorm,dasum(l,t(i1,j),1))
   10 continue
c
c     rcond = 1/(norm(t)*(estimate of norm(inverse(t)))) .
c     estimate = norm(z)/norm(y) where  t*z = y  and  trans(t)*y = e .
c     trans(t)  is the transpose of t .
c     the components of  e  are chosen to cause maximum local
c     growth in the elements of y .
c     the vectors are frequently rescaled to avoid overflow.
c
c     solve trans(t)*y = e
c
      ek = 1.0d0
      do 20 j = 1, n
         z(j) = 0.0d0
   20 continue
      do 100 kk = 1, n
         k = kk
         if (lower) k = n + 1 - kk
         if (z(k) .ne. 0.0d0) ek = dsign(ek,-z(k))
         if (dabs(ek-z(k)) .le. dabs(t(k,k))) go to 30
            s = dabs(t(k,k))/dabs(ek-z(k))
            call dscal(n,s,z,1)
            ek = s*ek
   30    continue
         wk = ek - z(k)
         wkm = -ek - z(k)
         s = dabs(wk)
         sm = dabs(wkm)
         if (t(k,k) .eq. 0.0d0) go to 40
            wk = wk/t(k,k)
            wkm = wkm/t(k,k)
         go to 50
   40    continue
            wk = 1.0d0
            wkm = 1.0d0
   50    continue
         if (kk .eq. n) go to 90
            j1 = k + 1
            if (lower) j1 = 1
            j2 = n
            if (lower) j2 = k - 1
            do 60 j = j1, j2
               sm = sm + dabs(z(j)+wkm*t(k,j))
               z(j) = z(j) + wk*t(k,j)
               s = s + dabs(z(j))
   60       continue
            if (s .ge. sm) go to 80
               w = wkm - wk
               wk = wkm
               do 70 j = j1, j2
                  z(j) = z(j) + w*t(k,j)
   70          continue
   80       continue
   90    continue
         z(k) = wk
  100 continue
      s = 1.0d0/dasum(n,z,1)
      call dscal(n,s,z,1)
c
      ynorm = 1.0d0
c
c     solve t*z = y
c
      do 130 kk = 1, n
         k = n + 1 - kk
         if (lower) k = kk
         if (dabs(z(k)) .le. dabs(t(k,k))) go to 110
            s = dabs(t(k,k))/dabs(z(k))
            call dscal(n,s,z,1)
            ynorm = s*ynorm
  110    continue
         if (t(k,k) .ne. 0.0d0) z(k) = z(k)/t(k,k)
         if (t(k,k) .eq. 0.0d0) z(k) = 1.0d0
         i1 = 1
         if (lower) i1 = k + 1
         if (kk .ge. n) go to 120
            w = -z(k)
            call daxpy(n-kk,w,t(i1,k),1,z(i1),1)
  120    continue
  130 continue
c     make znorm = 1.0
      s = 1.0d0/dasum(n,z,1)
      call dscal(n,s,z,1)
      ynorm = s*ynorm
c
      if (tnorm .ne. 0.0d0) rcond = ynorm/tnorm
      if (tnorm .eq. 0.0d0) rcond = 0.0d0
      return
      end
