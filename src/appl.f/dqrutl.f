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
c  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
c
c-----------------------------------------------------------------------
c
      subroutine dqrqty(x, n, k, qraux, y, ny, qty)
      double precision x(n,k), qraux(k), y(n,ny), qty(n,ny)
      integer n, k, ny
      integer info, j
      double precision dummy
      do 10 j = 1,ny
          call dqrsl(x, n, n, k, qraux, y(1,j), dummy, qty(1,j),
     .               dummy, dummy, dummy, 1000, info)
   10 continue
      return
      end
c
      subroutine dqrqy(x, n, k, qraux, y, ny, qy)
      double precision x(n,k), qraux(k), y(n,ny), qy(n,ny)
      integer n, k, ny
      integer info, j
      double precision dummy
      do 10 j = 1,ny
          call dqrsl(x, n, n, k, qraux, y(1,j), qy(1,j),
     .               dummy,  dummy, dummy, dummy, 10000, info)
   10 continue
      return
      end
c
      subroutine dqrcf(x, n, k, qraux, y, ny, b, info)
      double precision x(n,k), qraux(k), y(n,ny), b(k,ny)
      integer n, k, ny, info
      integer j
      double precision dummy
      do 10 j = 1,ny
          call dqrsl(x, n, n, k, qraux, y(1,j), dummy,
     .               y(1,j), b(1,j), dummy, dummy, 100, info)
   10 continue
      return
      end
c
      subroutine dqrrsd(x, n, k, qraux, y, ny, rsd)
      double precision x(n,k), qraux(k), y(n,ny), rsd(n,ny)
      integer n, k, ny
      integer info, j
      double precision dummy
      do 10 j = 1,ny
          call dqrsl(x, n, n, k, qraux, y(1,j), dummy,
     .               y(1,j), dummy, rsd(1,j), dummy, 10, info)
   10 continue
      return
      end
c
      subroutine dqrxb(x, n, k, qraux, y, ny, xb)
      double precision x(n,k), qraux(k), y(n,k), xb(n,ny)
      integer info, j
      double precision dummy
      do 10 j = 1,ny
          call dqrsl(x, n, n, k, qraux, y(1,j), dummy,
     .               y(1,j), dummy, dummy, xb(1,j), 1, info)
   10 continue
      return
      end
