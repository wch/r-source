c
c     zsisl solves the double complex symmetric system
c     a * x = b
c     using the factors computed by zsifa.
c
c     on entry
c
c        a       double complex(lda,n)
c                the output from zsifa.
c
c        lda     integer
c                the leading dimension of the array  a .
c
c        n       integer
c                the order of the matrix  a .
c
c        kpvt    integer(n)
c                the pivot vector from zsifa.
c
c        b       double complex(n)
c                the right hand side vector.
c
c     on return
c
c        b       the solution vector  x .
c
c     error condition
c
c        a division by zero may occur if  zsico  has set rcond .eq. 0.0
c        or  zsifa  has set info .ne. 0  .
c
c     to compute  inverse(a) * c  where  c  is a matrix
c     with  p  columns
c           call zsifa(a,lda,n,kpvt,info)
c           if (info .ne. 0) go to ...
c           do 10 j = 1, p
c              call zsisl(a,lda,n,kpvt,c(1,j))
c        10 continue
c
c     linpack. this version dated 08/14/78 .
c     james bunch, univ. calif. san diego, argonne nat. lab.
c
c     subroutines and functions
c
c     blas zaxpy,zdotu
c     fortran iabs
c
      subroutine zsisl(a,lda,n,kpvt,b)
      integer lda,n,kpvt(*)
      double complex a(lda,*),b(*)
c
c     internal variables.
c
      double complex ak,akm1,bk,bkm1,zdotu,denom,temp
      integer k,kp
c
c     loop backward applying the transformations and
c     d inverse to b.
c
      k = n
   10 if (k .eq. 0) go to 80
         if (kpvt(k) .lt. 0) go to 40
c
c           1 x 1 pivot block.
c
            if (k .eq. 1) go to 30
               kp = kpvt(k)
               if (kp .eq. k) go to 20
c
c                 interchange.
c
                  temp = b(k)
                  b(k) = b(kp)
                  b(kp) = temp
   20          continue
c
c              apply the transformation.
c
               call zaxpy(k-1,b(k),a(1,k),1,b(1),1)
   30       continue
c
c           apply d inverse.
c
            b(k) = b(k)/a(k,k)
            k = k - 1
         go to 70
   40    continue
c
c           2 x 2 pivot block.
c
            if (k .eq. 2) go to 60
               kp = iabs(kpvt(k))
               if (kp .eq. k - 1) go to 50
c
c                 interchange.
c
                  temp = b(k-1)
                  b(k-1) = b(kp)
                  b(kp) = temp
   50          continue
c
c              apply the transformation.
c
               call zaxpy(k-2,b(k),a(1,k),1,b(1),1)
               call zaxpy(k-2,b(k-1),a(1,k-1),1,b(1),1)
   60       continue
c
c           apply d inverse.
c
            ak = a(k,k)/a(k-1,k)
            akm1 = a(k-1,k-1)/a(k-1,k)
            bk = b(k)/a(k-1,k)
            bkm1 = b(k-1)/a(k-1,k)
            denom = ak*akm1 - 1.0d0
            b(k) = (akm1*bk - bkm1)/denom
            b(k-1) = (ak*bkm1 - bk)/denom
            k = k - 2
   70    continue
      go to 10
   80 continue
c
c     loop forward applying the transformations.
c
      k = 1
   90 if (k .gt. n) go to 160
         if (kpvt(k) .lt. 0) go to 120
c
c           1 x 1 pivot block.
c
            if (k .eq. 1) go to 110
c
c              apply the transformation.
c
               b(k) = b(k) + zdotu(k-1,a(1,k),1,b(1),1)
               kp = kpvt(k)
               if (kp .eq. k) go to 100
c
c                 interchange.
c
                  temp = b(k)
                  b(k) = b(kp)
                  b(kp) = temp
  100          continue
  110       continue
            k = k + 1
         go to 150
  120    continue
c
c           2 x 2 pivot block.
c
            if (k .eq. 1) go to 140
c
c              apply the transformation.
c
               b(k) = b(k) + zdotu(k-1,a(1,k),1,b(1),1)
               b(k+1) = b(k+1) + zdotu(k-1,a(1,k+1),1,b(1),1)
               kp = iabs(kpvt(k))
               if (kp .eq. k) go to 130
c
c                 interchange.
c
                  temp = b(k)
                  b(k) = b(kp)
                  b(kp) = temp
  130          continue
  140       continue
            k = k + 2
  150    continue
      go to 90
  160 continue
      return
      end
