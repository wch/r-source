c-----------------------------------------------------------------------
c
c  R : A Computer Langage for Statistical Data Analysis
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
c  along with this program; if not, write to the Free Software
c  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
c
c-----------------------------------------------------------------------
c
c     lminfl computes basic quantities useful for computing
c     regression diagnostics.
c
c     on entry
c
c         x         double precision(ldx,k)
c                   the qr decomposition as computed by dqrdc or dqrdc2.
c
c         ldx       integer
c                   the leading dimension of the array x.
c
c         n         integer
c                   the number of rows of the matrix x.
c
c         k         integer
c                   the number of columns in the matrix k.
c
c         qraux     double precision(k)
c                   auxiliary information about the qr decomposition.
c
c         b         double precision(k)
c                   the least-squares parameter estimates.
c
c         resid     double precision(k)
c                   the residuals from the regression.
c
c     on return
c
c         hat       double precision(n)
c                   the diagonal of the hat matrix.
c
c         coef      double precision(n,p)
c                   a matrix which has as i-th row contains the estimated
c                   regression coefficients when the i-th case is omitted
c                   from the regression.
c
c         sigma     double precision(n)
c                   the i-th element of sigma contains an estimate
c                   of the residual standard deviation for the model with
c                   the i-th case omitted.
c
c     This version dated Aug 24, 1996.
c     Ross Ihaka, University of Auckland.
c
      subroutine lminfl(x, ldx, n, k, qraux, resid, hat, coef, sigma)
      integer ldx, n, k
      double precision x(ldx,k), qraux(k), resid(n),
     +     hat(n), coef(n,k), sigma(n)
c
      integer i, j, info
      double precision sum, denom, dummy
c
c     hat matrix diagonal
c
      do 10 i = 1,n
        hat(i) = 0.0d0
   10 continue

      do 40 j = 1,k
        do 20 i = 1,n
          sigma(i) = 0.0d0
   20   continue
        sigma(j) = 1.0d0
        call dqrsl(x, ldx, n, k, qraux, sigma, sigma, dummy,
     .      dummy, dummy, dummy, 10000, info)
        do 30 i = 1, n
          hat(i) = hat(i)+sigma(i)*sigma(i)
   30   continue
   40 continue
c
c     changes in the estimated coefficients
c
      do 70 i = 1,n
        do 50 j = 1,n
          sigma(j) = 0.0d0
   50   continue
        sigma(i) = resid(i)/(1.0d0 - hat(i))
        call dqrsl(x, ldx, n, k, qraux, sigma, dummy, sigma,
     .             dummy, dummy, dummy, 1000, info)
        call dtrsl(x, ldx, k, sigma, 1, info)
        do 60 j = 1,k
          coef(i,j) = sigma(j)
   60   continue
   70 continue
c
c     estimated residual standard deviation
c
      denom = (n - k - 1)
      sum = 0.0d0
      do 80 i = 1,n
        sum = sum + resid(i)*resid(i)
   80 continue
      do 90 i = 1,n
        sigma(i) = sqrt((sum - resid(i)*resid(i)/(1.0d0-hat(i)))/denom)
   90 continue
      return
      end
