C An interface to sbart() --- fewer arguments BUT unspecified scrtch() dimension
C
      subroutine qsbart(penalt,dofoff,xs,ys,ws,ssw,n,knot,nk,
     &	   coef,sz,lev,
     &	   crit,iparms,spar,parms,
     &	   isetup, scrtch, ld4,ldnk,ier)
c
      integer n,nk,isetup, iparms(2), ld4,ldnk,ier
      double precision penalt,dofoff, xs(n),ys(n),ws(n),ssw,
     &	   knot(nk+4), coef(nk),sz(n),lev(n), 
     &	   crit,spar,parms(3),
     &	   scrtch(*)
C	   ^^^^^^^^ dimension (9+2*ld4+nk)*nk

      call sbart(penalt,dofoff,xs,ys,ws,ssw,n,knot,nk,
     &	   coef,sz,lev, crit,
     &	   iparms(1),spar,iparms(2),
c	   = icrit   spar   ispar
     &	   parms(1),parms(2),parms(3),
c	   = lspar   uspar    tol
     &	   isetup, scrtch(1),
c	   =  1	    xwy
     &	   scrtch(  nk+1),scrtch(2*nk+1),scrtch(3*nk+1),scrtch(4*nk+1),
c	   =   hs0	      hs1	     hs2	    hs3
     &	   scrtch(5*nk+1),scrtch(6*nk+1),scrtch(7*nk+1),scrtch(8*nk+1),
c	   =   sg0	      sg1	     sg2	    sg3
     &	   scrtch(9*nk+1),scrtch(9*nk+	ld4*nk+1),scrtch(9*nk+2*ld4*nk),
c	   =   abd	      p1ip		      p2ip
     &	   ld4,ldnk,ier)

      return
      end

