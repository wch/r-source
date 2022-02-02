c-----------------------------------------------------------------------
c
c  R : A Computer Language for Statistical Data Analysis
c  Copyright (C) 1998-2016 The R Core Team
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

C Called from R's smooth.spline in ../R/smspline.R  as .Fortran(C, ..)
C    and from C's

C An interface to sbart() --- fewer arguments BUT unspecified scrtch() dimension
C
C NB: this routine alters ws [and isetup].
C renamed for safety
C
      subroutine rbart(penalt,dofoff,xs,ys,ws,ssw,n,knot,nk,
     &     coef,sz,lev,
     &     crit,iparms,spar,parms,
     &     scrtch, ld4,ldnk,ier)
c Args:
      integer n,nk, iparms(4), ld4,ldnk,ier
      double precision penalt,dofoff, xs(n),ys(n),ws(n), ssw,
     &     knot(nk+4), coef(nk), sz(n), lev(n),
     &     crit, spar, parms(5),
     &     scrtch(*)
C          ^^^^^^^^ dimension (9+2*ld4+ldnk)*nk = (17 + 1)*nk [last nk never accessed]
c Vars:
      integer isetup

      if(iparms(4) .eq. 1) then ! spar is lambda
         isetup = 2
      else
         isetup = 0
      endif
      call sbart(penalt,dofoff,xs,ys,ws,ssw,n,knot,nk,
     &     coef,sz,lev, crit,
     &     iparms(1),spar,iparms(2),iparms(3),
c          = icrit   spar   ispar    iter
     &     parms(1),parms(2),parms(3),parms(4),parms(5),
c          = lspar   uspar    tol      eps      ratio
     &     isetup, scrtch(1),
c          = 0|2    xwy  == X'W y
     &     scrtch(  nk+1),scrtch(2*nk+1),scrtch(3*nk+1),scrtch(4*nk+1),
c          =   hs0	      hs1	     hs2	    hs3		==> X'W X
     &     scrtch(5*nk+1),scrtch(6*nk+1),scrtch(7*nk+1),scrtch(8*nk+1),
c          =   sg0	      sg1	     sg2	    sg3		==> SIGMA
     &     scrtch(9*nk+1),
c          =   abd [ld4 x nk]						==> R
     &     scrtch(9*nk+ ld4*nk+1), scrtch(9*nk+2*ld4*nk+1),
c          =   p1ip[ld4 x nk]          p2ip [ldnk x nk]
     &     ld4,ldnk,ier)

      return
      end
