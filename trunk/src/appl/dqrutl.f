c dqr Utilities:  Interface to the different "switches" of  dqrsl().
c
      subroutine dqrqty(x, n, k, qraux, y, ny, qty)

      integer n, k, ny
      double precision x(n,k), qraux(k), y(n,ny), qty(n,ny)
      integer info, j
      double precision dummy(1)
      do 10 j = 1,ny
          call dqrsl(x, n, n, k, qraux, y(1,j), dummy, qty(1,j),
     &               dummy, dummy, dummy, 1000, info)
   10 continue
      return
      end
c
      subroutine dqrqy(x, n, k, qraux, y, ny, qy)

      integer n, k, ny
      double precision x(n,k), qraux(k), y(n,ny), qy(n,ny)
      integer info, j
      double precision dummy(1)
      do 10 j = 1,ny
          call dqrsl(x, n, n, k, qraux, y(1,j), qy(1,j),
     &               dummy,  dummy, dummy, dummy, 10000, info)
   10 continue
      return
      end
c
      subroutine dqrcf(x, n, k, qraux, y, ny, b, info)

      integer n, k, ny, info
      double precision x(n,k), qraux(k), y(n,ny), b(k,ny)
      integer j
      double precision dummy(1)
      do 10 j = 1,ny
          call dqrsl(x, n, n, k, qraux, y(1,j), dummy,
     &               y(1,j), b(1,j), dummy, dummy, 100, info)
   10 continue
      return
      end
c
      subroutine dqrrsd(x, n, k, qraux, y, ny, rsd)

      integer n, k, ny
      double precision x(n,k), qraux(k), y(n,ny), rsd(n,ny)
      integer info, j
      double precision dummy(1)
      do 10 j = 1,ny
          call dqrsl(x, n, n, k, qraux, y(1,j), dummy,
     &               y(1,j), dummy, rsd(1,j), dummy, 10, info)
   10 continue
      return
      end
c
      subroutine dqrxb(x, n, k, qraux, y, ny, xb)

      integer n, k, ny
      double precision x(n,k), qraux(k), y(n,ny), xb(n,ny)
      integer info, j
      double precision dummy(1)
      do 10 j = 1,ny
          call dqrsl(x, n, n, k, qraux, y(1,j), dummy,
     &               y(1,j), dummy, dummy, xb(1,j), 1, info)
   10 continue
      return
      end
