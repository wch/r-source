c-----------------------------------------------------------------------
c
c  R : A Computer Langage for Statistical Data Analysis
c  Copyright (C) 1996, 1997  Robert Gentleman and Ross Ihaka
c  Copyright (C) 2001        The R Development Core Team
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
c  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
c
c-----------------------------------------------------------------------
c
c ch2inv(x, ldx, n, v, info) computes the inverse of a positive-definite
c		symmetric matrix from its choleski factorization.
c This can be used, e.g., to compute the dispersion matrix for the estimated
c parameters in a regression analysis.
c
c On Entry
c
c	x	double precision(ldx,n)	 {ldx >= n}
c		the choleski decomposition or the qr decomposition
c		as computed by dqrdc or dqrdc2.	 Only the
c		UPPER TRIANGULAR part,	x(i,j), 1 <= i <= j <= n, is accessed.
c
c	ldx	integer, ldx >= n
c		the leading dimension of the array x
c
c	n	integer
c		the number of rows and columns of the matrix x
c
c On Return
c
c	v	double precision(n,n)
c		the value of inverse(x'x)
c
c This version dated Aug 24, 1996.
c Ross Ihaka, University of Auckland.
c
      subroutine ch2inv(x, ldx, n, v, info)
c     implicit none
      integer n, ldx, info
      double precision x(ldx, n), v(n, n)
c
      double precision d
      integer i, j, im1
c
      do 20 i=1,n
        if(x(i,i) .eq. 0.0d0) then
          info = i
          return
        end if
        do 10 j=i,n
          v(i,j) = x(i,j)
   10   continue
   20 continue
      call dpodi(v, n, n, d, 1)
      do 40 i=1,n
        im1 = i-1
        do 30 j=1,im1
          v(i,j) = v(j,i)
   30   continue
   40 continue
      return
      end
