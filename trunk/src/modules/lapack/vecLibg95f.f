      double complex function zdotc(n, zx, incx, zy, incy)
      double complex zx(*), zy(*), z
      integer n, incx, incy

      call rcblas_zdotc_sub(n, zx, incx, zy, incy, z)

      zdotc = z
      return
      end

      double complex function zdotu(n, zx, incx, zy, incy)
      double complex zx(*), zy(*), z
      integer n, incx, incy

      call rcblas_zdotu_sub(n, zx, incx, zy, incy, z)

      zdotu = z
      return
      end

      complex function cdotc(n, cx, incx, cy, incy)
      complex cx(*), cy(*), c
      integer n, incx, incy

      call rcblas_cdotc_sub(n, cx, incx, cy, incy, c)

      cdotc = c
      return
      end

      complex function cdotu(n, cx, incx, cy, incy)
      complex cx(*), cy(*), c
      integer n, incx, incy

      call rcblas_cdotu_sub(n, cx, incx, cy, incy, c)

      cdotu = c
      return
      end
