/* qsbart.f -- translated by f2c (version 19971204).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* An interface to sbart() --- fewer arguments BUT unspecified scrtch() dimension */


/* Subroutine */ int qsbart(doublereal *penalt, doublereal *dofoff, 
	doublereal *xs, doublereal *ys, doublereal *ws, doublereal *ssw, 
	integer *n, doublereal *knot, integer *nk, doublereal *coef, 
	doublereal *sz, doublereal *lev, doublereal *crit, integer *iparms, 
	doublereal *spar, doublereal *parms, integer *isetup, doublereal *
	scrtch, integer *ld4, integer *ldnk, integer *ier)
{
    extern /* Subroutine */ int sbart(doublereal *, doublereal *, doublereal 
	    *, doublereal *, doublereal *, doublereal *, integer *, 
	    doublereal *, integer *, doublereal *, doublereal *, doublereal *,
	     doublereal *, integer *, doublereal *, integer *, doublereal *, 
	    doublereal *, doublereal *, integer *, doublereal *, doublereal *,
	     doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, integer *, integer *, integer *);


/*          ^^^^^^^^ dimension (9+2*ld4+nk)*nk */
    /* Parameter adjustments */
    --lev;
    --sz;
    --ws;
    --ys;
    --xs;
    --coef;
    --knot;
    --iparms;
    --parms;
    --scrtch;

    /* Function Body */
    sbart(penalt, dofoff, &xs[1], &ys[1], &ws[1], ssw, n, &knot[1], nk, &
	    coef[1], &sz[1], &lev[1], crit, &iparms[1], spar, &iparms[2], &
	    parms[1], &parms[2], &parms[3], isetup, &scrtch[1], &scrtch[*nk + 
	    1], &scrtch[(*nk << 1) + 1], &scrtch[*nk * 3 + 1], &scrtch[(*nk <<
	     2) + 1], &scrtch[*nk * 5 + 1], &scrtch[*nk * 6 + 1], &scrtch[*nk 
	    * 7 + 1], &scrtch[(*nk << 3) + 1], &scrtch[*nk * 9 + 1], &scrtch[*
	    nk * 9 + *ld4 * *nk + 1], &scrtch[*nk * 9 + (*ld4 << 1) * *nk + 1]
	    , ld4, ldnk, ier);
    return 0;
} /* qsbart_ */
