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
c     chol performs the choleski decomposition of a symmetric
c     positive-definite matrix.  this is just a wrapper for the
c     linpack routine dpofa.
c
c     on entry
c
c         a         double precision(lda,n)
c                   the upper triangle of the matrix to be factorized
c                   is contained in the upper triangle of a.
c
c         lda       integer
c                   the leading dimension of a.
c
c         n         integer
c                   the number or rows and columns of the matrix
c                   to be factorized.
c
c     on return
c
c         v         double precision(n,n)
c                   the square-root (choleski) factor.
c
c         info      integer
c                   the error indicator from dpofa.  this will be
c                   zero unless the matrix being factorized is
c                   not positive definite.
c
c     this version dated aug 25, 1996.
c     ross ihaka, university of auckland.
c
      subroutine chol(a, lda, n, v, info)
c     implicit none
      integer n, lda, info
      double precision a(lda, n), v(n,n)
      integer i, j
c
      do 20 i = 1,n
        do 10 j = 1,n
          if(i .gt. j) then
            v(i,j) = 0.0d0
          else
            v(i,j) = a(i,j)
          end if
   10   continue
   20 continue
      call dpofa(v, n, n, info)
      return
      end
