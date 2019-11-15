c-----------------------------------------------------------------------
c
c  R : A Computer Language for Statistical Data Analysis
c  Copyright (C) 2003--2018  The R Foundation
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
c       docoef  integer (logical) indicating if  coef(*,*) should be computed
c               Computation of coef(.) is O(n^2 * k) which may be too much.
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
c       coef    double precision(n, p, q)
c               an 3d-array which has as i-th row/face the estimated
c               regression coefficients when the i-th case is omitted
c               from the regression.
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

      subroutine lminfl(x, ldx, n, k, q, docoef, qraux, resid,
     +     hat, coef, sigma, tol)
      integer ldx, n, k, q, docoef
      double precision x(ldx,k), qraux(k), resid(n,q),
     +     hat(n), coef(n,k,q), sigma(n,q), tol
c   coef(.,.) can be dummy(1) when docoef is 0(false)

      integer c, i, j, info
      double precision sum, denom, dummy
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
c     changes in the estimated coefficients  coef(i,j,c)  i=1..n, j=1..k, c=1..q
c
c     NB: This code is terribly inefficient. For now, the R code in lm.influence simply
c     bypasses it by calling with docoef = 0 and handling it in R code instead.
c     Specifically, we need to calculate R^{-1}Q'D with D = diag(resid/(1-hat))
c     The code below does this by computing  Q'Y with Y being each column of D in turn,
c     but it is much more efficient to compute  R^{-1}Q' once and scale the columns by the
c     elements of D (-pd, nov.2019)
c
      
      if(docoef .ne. 0) then
         ! use sigma(*,1) as auxiliary
         do  i = 1,n
            do c = 1,q
               do j = 1,n
                  sigma(j,1) = 0.0d0
               end do
c     if hat() is effectively 1, change is zero
               if(hat(i) .lt. 1.0d0) then
                  sigma(i,1) = resid(i,c)/(1.0d0 - hat(i))
                  call dqrsl(x, ldx, n, k, qraux, sigma, dummy, sigma,
     .                       dummy, dummy, dummy, 1000, info)
                  call dtrsl(x, ldx, k, sigma, 1, info)
               endif
               do j = 1,k
                  coef(i,j,c) = sigma(j,1)
               end do
            end do ! c = 1..q
         end do ! i = 1,n
      endif
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
