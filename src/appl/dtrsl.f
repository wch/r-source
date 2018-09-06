c Triangular Solve  dtrsl()
c ----------------
c     solves systems of the form
c
c                   t * x = b
c     or
c                   trans(t) * x = b
c
c     where t is a triangular matrix of order n. here trans(t)
c     denotes the transpose of the matrix t.
c
c     on entry
c
c         t         double precision(ldt,n)
c                   t contains the matrix of the system. the zero
c                   elements of the matrix are not referenced, and
c                   the corresponding elements of the array can be
c                   used to store other information.
c
c         ldt       integer
c                   ldt is the leading dimension of the array t.
c
c         n         integer
c                   n is the order of the system.
c
c         b         double precision(n).
c                   b contains the right hand side of the system.
c
c         job       integer
c                   job specifies what kind of system is to be solved.
c                   if job is
c
c                        00   solve t*x=b, t lower triangular,
c                        01   solve t*x=b, t upper triangular,
c                        10   solve trans(t)*x=b, t lower triangular,
c                        11   solve trans(t)*x=b, t upper triangular.
c
c     on return
c
c         b         b contains the solution, if info .eq. 0.
c                   otherwise b is unaltered.
c
c         info      integer
c                   info contains zero if the system is nonsingular.
c                   otherwise info contains the index of
c                   the first zero diagonal element of t.
c
c     linpack. this version dated 08/14/78 .
c     g. w. stewart, university of maryland, argonne national lab.
c
c     subroutines and functions
c
c     blas:     daxpy,ddot
c     fortran   mod
c
      subroutine dtrsl(t,ldt,n,b,job,info)
      integer ldt,n,job,info
      double precision t(ldt,*),b(*)
c
c     internal variables
c
      double precision ddot,temp
      integer case,j,jj
c
c     begin block permitting ...exits to 150
c
c        check for zero diagonal elements.
c
      do 10 info = 1, n
         if (t(info,info) .eq. 0.0d0) go to 150
c     ......exit
 10   continue
      info = 0
c
c     determine the task and go to it.
c
      case = 1
      if (mod(job,10) .ne. 0) case = 2
      if (mod(job,100)/10 .ne. 0) case = case + 2
      go to (20,50,80,110), case
c
C Case 1 (job = 00):
c        solve t*x=b for t lower triangular
c
 20   continue
      b(1) = b(1)/t(1,1)
      if (n .ge. 2) then
      do 30 j = 2, n
         temp = -b(j-1)
         call daxpy(n-j+1,temp,t(j,j-1),1,b(j),1)
         b(j) = b(j)/t(j,j)
 30   continue
      endif
      go to 140
c     
C Case 2 (job = 01):
c        solve t*x=b for t upper triangular.
c
 50   continue
      b(n) = b(n)/t(n,n)
      if (n .ge. 2) then
         do 60 jj = 2, n
            j = n - jj + 1
            temp = -b(j+1)
            call daxpy(j,temp,t(1,j+1),1,b(1),1)
            b(j) = b(j)/t(j,j)
 60      continue
      endif
      go to 140
c     
C Case 3 (job = 10):
c        solve trans(t)*x=b for t lower triangular.
c
 80   continue
      b(n) = b(n)/t(n,n)
      if (n .ge. 2) then
         do 90 jj = 2, n
            j = n - jj + 1
            b(j) = b(j) - ddot(jj-1,t(j+1,j),1,b(j+1),1)
            b(j) = b(j)/t(j,j)
 90      continue
      endif
      go to 140
c     
C Case 4 (job = 11):
c        solve trans(t)*x=b for t upper triangular.
c
 110  continue
      b(1) = b(1)/t(1,1)
      if (n .ge. 2) then
         do 120 j = 2, n
            b(j) = b(j) - ddot(j-1,t(1,j),1,b(1),1)
            b(j) = b(j)/t(j,j)
 120     continue
      endif
C
 140  continue
c     EXIT:
 150  continue
      return
      end
