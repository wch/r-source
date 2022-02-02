c-----------------------------------------------------------------------
c
c  R : A Computer Language for Statistical Data Analysis
c  Copyright (C) 2003--2019  The R Foundation
c  Copyright (C) 1996, 1997  Robert Gentleman and Ross Ihaka
c
c  This program is free software; you can redistribute it and/or modify
c  it under the terms of the GNU General Public License as published by
c  the Free Software Foundation; either version 2 of the License, or
c  (at your option) any later version.
c
c  This program is distributed in the hope that it will be useful,
c  but WITHOUT ANY WARRANTY; without even the implied warranty of
c  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c  GNU General Public License for more details.
c
c  You should have received a copy of the GNU General Public License
c  along with this program; if not, a copy is available at
c  https://www.R-project.org/Licenses/
c
c-----------------------------------------------------------------------
c
c   lminfl computes basic quantities useful for computing
c   regression diagnostics.
c
c   on entry
c
c       x       double precision(ldx,k)
c               the qr decomposition as computed by dqrdc or dqrdc2.
c
c       ldx     integer
c               the leading dimension of the array x.
c
c       n       integer
c               the number of rows of the matrix x.
c
c       k       integer
c               the number of columns in the matrix x.
c
c       q       integer
c               the number of columns of the (multivariate) y[]
c      
c       qraux   double precision(k)
c               auxiliary information about the qr decomposition.
c
c       resid   double precision(n, q)
c               the residuals from the (possibly multivariate) regression.
c
c   on return
c
c       hat     double precision(n)
c               the diagonal of the hat matrix.
c
c       sigma   double precision(n, q)
c               the (i,j)-th element of sigma contains an estimate
c               of the residual standard deviation for the y[,j]-model with
c               the i-th case omitted.
c
c   Initial version dated Aug 24, 1996.
c   Ross Ihaka, University of Auckland.
c   'docoef' option added Feb 17, 2003;  Martin Maechler ETH Zurich.
c   Handle hat == 1 case, Nov 2005.
c   Argument 'tol' was real not double precision, Aug 2007
c   'q' for multivariate (mlm) models added Sep, 2018;  Martin Maechler.
c    docoef replaced by R code and removed Nov 2019.
      
      subroutine lminfl(x, ldx, n, k, q, qraux, resid,
     +     hat, sigma, tol)
      integer ldx, n, k, q
      double precision x(ldx,k), qraux(k), resid(n,q),
     +     hat(n), sigma(n,q), tol

      integer i, j, info
      double precision sum, denom, dummy(1)
c
c     hat matrix diagonal h_ii = hat(i)    [sigma(i,1) as auxiliary] :
c
      do i = 1,n
        hat(i) = 0.0d0
      end do

      do j = 1,k
        do i = 1,n
          sigma(i,1) = 0.0d0
        end do
        sigma(j,1) = 1.0d0
        call dqrsl(x, ldx, n, k, qraux, sigma, sigma, dummy,
     .             dummy, dummy, dummy, 10000, info)
        do i = 1, n
          hat(i) = hat(i) + sigma(i,1)*sigma(i,1)
        end do
      end do
      do i = 1, n
        if(hat(i) .ge. 1.0d0 - tol) hat(i) = 1.0d0
      end do
c
c     estimated residual standard deviation  sigma(j,c)
c
      denom = (n - k - 1)
      do j = 1,q
         sum = 0.0d0
         do i = 1,n
            sum = sum + resid(i,j)*resid(i,j)
         end do
         do i = 1,n
            if(hat(i) .lt. 1.0d0) then
               sigma(i,j) = sqrt((sum -
     +              resid(i,j)*resid(i,j)/(1.0d0-hat(i))) / denom)
            else
               sigma(i,j) = sqrt(sum/denom)
            endif
         end do
      end do

      return
      end
