c-----------------------------------------------------------------------
c
c  R : A Computer Language for Statistical Data Analysis
c  Copyright (C) 1999-2001  The R Core Team
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
