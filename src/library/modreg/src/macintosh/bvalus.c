/* bvalus.f -- translated by f2c (version 19971204).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__4 = 4;

/* Subroutine */ int bvalus(integer *n, doublereal *knot, doublereal *coef, 
	integer *nk, doublereal *x, doublereal *s, integer *order)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer i__;
    extern doublereal bvalue(doublereal *, integer *, doublereal *, integer *
	    , integer *, doublereal *, integer *);

/* Args */
/* Local */
    /* Parameter adjustments */
    --s;
    --x;
    --coef;
    --knot;

    /* Function Body */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = *n + 4;
	s[i__] = bvalue(&knot[1], &i__2, &coef[1], nk, &c__4, &x[i__], order)
		;
/*                        ----  typo corrected from gamfit */
/* L10: */
    }
    return 0;
} /* bvalus_ */
