c-----------------------------------------------------------------------
c
c  R : A Computer Language for Statistical Data Analysis
c  Copyright (C) 1999-2020  The R Core Team
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
      EXTERNAL intpr0
      nc = nchar
      if(nc .lt. 0) nc = len(label)
      call intpr0(label, nc, data, ndata)
      end

      subroutine realpr(label, nchar, data, ndata)
      integer nchar, ndata
      character*(*) label
      real data(ndata)
      integer nc
      EXTERNAL realp0
      nc = nchar
      if(nc .lt. 0) nc = len(label)
      call realp0(label, nc, data, ndata)
      end

      subroutine dblepr(label, nchar, data, ndata)
      integer nchar, ndata
      character*(*) label
      double precision data(ndata)
      integer nc
      EXTERNAL dblep0
      nc = nchar
      if(nc .lt. 0) nc = len(label)
      call dblep0(label, nc, data, ndata)
      end

c Avoid 'Rank mismatch warning from gcc 10'
      subroutine intpr1(label, nchar, var)
      integer nchar
      character*(*) label
      integer var, data(1)
      integer nc
      EXTERNAL intpr0
      nc = nchar
      if(nc .lt. 0) nc = len(label)
      data(1) = var
      call intpr0(label, nc, data, 1)
      end

      subroutine realpr1(label, nchar, var)
      integer nchar
      character*(*) label
      real var, data(1)
      integer nc
      EXTERNAL realp0
      nc = nchar
      if(nc .lt. 0) nc = len(label)
      data(1) = var
      call realp0(label, nc, data, 1)
      end

      subroutine dblepr1(label, nchar, var)
      integer nchar
      character*(*) label
      double precision var, data(1)
      integer nc
      EXTERNAL dblep0
      nc = nchar
      if(nc .lt. 0) nc = len(label)
      data(1) = var
      call dblep0(label, nc, data, 1)
      end

      subroutine labelpr(label, nchar)
      integer nchar
      character*(*) label
      integer data(1)
      integer nc
      EXTERNAL intpr0
      nc = nchar
      if(nc .lt. 0) nc = len(label)
      data(1) = 0
      call intpr0(label, nc, data, 0)
      end

C R-only Fortran versions of error and warning
      subroutine rexit(msg)
      character*(*) msg
      EXTERNAL rexitc
      call rexitc(msg, len(msg))
      end

      subroutine rwarn(msg)
      character*(*) msg
      EXTERNAL rwarnc
      call rwarnc(msg, len(msg))
      end
