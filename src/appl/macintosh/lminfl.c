/* lminfl.f -- translated by f2c (version 19971204).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__10000 = 10000;
static integer c__1000 = 1000;
static integer c__1 = 1;

/* ----------------------------------------------------------------------- */

/*  R : A Computer Langage for Statistical Data Analysis */
/*  Copyright (C) 1996, 1997  Robert Gentleman and Ross Ihaka */

/*  This program is free software; you can redistribute it and/or modify */
/*  it under the terms of the GNU General Public License as published by */
/*  the Free Software Foundation; either version 2 of the License, or */
/*  (at your option) any later version. */

/*  This program is distributed in the hope that it will be useful, */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the */
/*  GNU General Public License for more details. */

/*  You should have received a copy of the GNU General Public License */
/*  along with this program; if not, write to the Free Software */
/*  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA */

/* ----------------------------------------------------------------------- */

/*     lminfl computes basic quantities useful for computing */
/*     regression diagnostics. */

/*     on entry */

/*         x         double precision(ldx,k) */
/*                   the qr decomposition as computed by dqrdc or dqrdc2. */

/*         ldx       integer */
/*                   the leading dimension of the array x. */

/*         n         integer */
/*                   the number of rows of the matrix x. */

/*         k         integer */
/*                   the number of columns in the matrix k. */

/*         qraux     double precision(k) */
/*                   auxiliary information about the qr decomposition. */

/*         b         double precision(k) */
/*                   the least-squares parameter estimates. */

/*         resid     double precision(k) */
/*                   the residuals from the regression. */

/*     on return */

/*         hat       double precision(n) */
/*                   the diagonal of the hat matrix. */

/*         coef      double precision(n,p) */
/*                   a matrix which has as i-th row contains the estimated */
/*                   regression coefficients when the i-th case is omitted */
/*                   from the regression. */

/*         sigma     double precision(n) */
/*                   the i-th element of sigma contains an estimate */
/*                   of the residual standard deviation for the model with */
/*                   the i-th case omitted. */

/*     This version dated Aug 24, 1996. */
/*     Ross Ihaka, University of Auckland. */

/* Subroutine */ int lminfl(doublereal *x, integer *ldx, integer *n, integer 
	*k, doublereal *qraux, doublereal *resid, doublereal *hat, doublereal 
	*coef, doublereal *sigma)
{
    /* System generated locals */
    integer x_dim1, x_offset, coef_dim1, coef_offset, i__1, i__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static integer info, i__, j;
    static doublereal denom;
    extern /* Subroutine */ int dqrsl(doublereal *, integer *, integer *, 
	    integer *, doublereal *, doublereal *, doublereal *, doublereal *,
	     doublereal *, doublereal *, doublereal *, integer *, integer *), 
	    dtrsl(doublereal *, integer *, integer *, doublereal *, integer *
	    , integer *);
    static doublereal dummy, sum;



/*     hat matrix diagonal */

    /* Parameter adjustments */
    --sigma;
    --hat;
    --resid;
    coef_dim1 = *n;
    coef_offset = coef_dim1 + 1;
    coef -= coef_offset;
    --qraux;
    x_dim1 = *ldx;
    x_offset = x_dim1 + 1;
    x -= x_offset;

    /* Function Body */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	hat[i__] = 0.;
/* L10: */
    }
    i__1 = *k;
    for (j = 1; j <= i__1; ++j) {
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    sigma[i__] = 0.;
/* L20: */
	}
	sigma[j] = 1.;
	dqrsl(&x[x_offset], ldx, n, k, &qraux[1], &sigma[1], &sigma[1], &
		dummy, &dummy, &dummy, &dummy, &c__10000, &info);
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    hat[i__] += sigma[i__] * sigma[i__];
/* L30: */
	}
/* L40: */
    }

/*     changes in the estimated coefficients */

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
	    sigma[j] = 0.;
/* L50: */
	}
	sigma[i__] = resid[i__] / (1. - hat[i__]);
	dqrsl(&x[x_offset], ldx, n, k, &qraux[1], &sigma[1], &dummy, &sigma[
		1], &dummy, &dummy, &dummy, &c__1000, &info);
	dtrsl(&x[x_offset], ldx, k, &sigma[1], &c__1, &info);
	i__2 = *k;
	for (j = 1; j <= i__2; ++j) {
	    coef[i__ + j * coef_dim1] = sigma[j];
/* L60: */
	}
/* L70: */
    }

/*     estimated residual standard deviation */

    denom = (doublereal) (*n - *k - 1);
    sum = 0.;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sum += resid[i__] * resid[i__];
/* L80: */
    }
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sigma[i__] = sqrt((sum - resid[i__] * resid[i__] / (1. - hat[i__])) / 
		denom);
/* L90: */
    }
    return 0;
} /* lminfl_ */
