      subroutine intpr(label, nchar, data, ndata)
      character*(*) label
      integer data(ndata)
      nc = nchar
      if(nc .lt. 0) nc = len(label)
      call intpr0(label, nc, data, ndata)
      end

      subroutine realpr(label, nchar, data, ndata)
      character*(*) label
      real data(ndata)
      nc = nchar
      if(nc .lt. 0) nc = len(label)
      call realpr0(label, nc, data, ndata)
      end

      subroutine dblepr(label, nchar, data, ndata)
      character*(*) label
      double precision data(ndata)
      nc = nchar
      if(nc .lt. 0) nc = len(label)
      call dblepr0(label, nc, data, ndata)
      end
