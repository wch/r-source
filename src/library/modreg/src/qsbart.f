C An interface to sbart() --- less arguments BUT unspecified scrtch() dimension
C
C unused !
C
      subroutine qsbart(penalt,dofoff,xs,ys,ws,n,knot,nk,
     &     coef,sz,lev,
     &     crit,iparms,spar,parms,
     &     isetup, scrtch, ld4,ldnk,ier)
c
      implicit none

      integer n,nk,isetup, iparms(2), ld4,ldnk,ier
      double precision penalt,dofoff, xs(n),ys(n),ws(n),
     &     knot(nk+4), coef(nk),sz(n),lev(n), 
     &     crit,spar,parms(3),
     &     scrtch(1)
C          ^^^^^^^^ dimension (9+2*ld4+nk)*nk

      call sbart(penalt,dofoff,xs,ys,ws,n,knot,nk,
     &     coef,sz,lev, crit,
     &     iparms(1),spar,iparms(2),parms(1),parms(2),parms(3),
     &     isetup,
     &     scrtch(     1),scrtch(  nk+1),scrtch(2*nk+1),scrtch(3*nk+1),
     &     scrtch(4*nk+1),scrtch(5*nk+1),scrtch(6*nk+1),scrtch(7*nk+1),
     &     scrtch(8*nk+1),scrtch(9*nk+1),
     &     scrtch(9*nk+  ld4*nk+1),
     &     scrtch(9*nk+2*ld4*nk+1), ld4,ldnk,ier)

      return
      end

