C Mostly for S compatibility.
C These now all call C functions via F77_NAME(.) in ./print.c :

      subroutine intpr(label, nchar, data, ndata)
      integer nchar, ndata
      character*(*) label
      integer data(ndata)
      integer nc
      nc = nchar
      if(nc .lt. 0) nc = len(label)
      call intpr0(label, nc, data, ndata)
      end

      subroutine realpr(label, nchar, data, ndata)
      integer nchar, ndata
      character*(*) label
      real data(ndata)
      integer nc
      nc = nchar
      if(nc .lt. 0) nc = len(label)
      call realp0(label, nc, data, ndata)
      end

      subroutine dblepr(label, nchar, data, ndata)
      integer nchar, ndata
      character*(*) label
      double precision data(ndata)
      integer nc
      nc = nchar
      if(nc .lt. 0) nc = len(label)
      call dblep0(label, nc, data, ndata)
      end

C R-only Fortran versions of error and warning
      subroutine rexit(msg)
      character*(*) msg
      call rexitc(msg, len(msg))
      end

      subroutine rwarn(msg)
      character*(*) msg
      call rwarnc(msg, len(msg))
      end
