/* ch2inv.f -- translated by f2c (version 19971204).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

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

/*     ch2inv computes the inverse of a positive-definite symmetric */
/*     matrix from its choleski factorization.  this can be used (for */
/*     example) to compute the dispersion matrix for the estimated */
/*     parameters in a regression analysis. */

/*     on entry */

/*         x         double precision(ldx,k) */
/*                   the choleski decomposition or the */
/*                   qr decomposition as computed by dqrdc */
/*                   or dqrdc2 */

/*         ldx       integer */
/*                   the leading dimension of the array x */

/*         n         integer */
/*                   the number of rows of the matrix x */

/*         k         integer */
/*                   the number of columns in the matrix k */

/*     on return */

/*         v         double precision(k,k) */
/*                   the value of inverse(x'x) */

/*     This version dated Aug 24, 1996. */
/*     Ross Ihaka, University of Auckland. */

/* Subroutine */ int ch2inv(doublereal *x, integer *ldx, integer *n, 
	doublereal *v, integer *info)
{
    /* System generated locals */
    integer x_dim1, x_offset, v_dim1, v_offset, i__1, i__2;

    /* Local variables */
    static doublereal d__;
    static integer i__, j;
    extern /* Subroutine */ int dpodi(doublereal *, integer *, integer *, 
	    doublereal *, integer *);
    static integer im1;



    /* Parameter adjustments */
    v_dim1 = *n;
    v_offset = v_dim1 + 1;
    v -= v_offset;
    x_dim1 = *ldx;
    x_offset = x_dim1 + 1;
    x -= x_offset;

    /* Function Body */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (x[i__ + i__ * x_dim1] == 0.) {
	    *info = i__;
	    return 0;
	}
	i__2 = *n;
	for (j = i__; j <= i__2; ++j) {
	    v[i__ + j * v_dim1] = x[i__ + j * x_dim1];
/* L10: */
	}
/* L20: */
    }
    dpodi(&v[v_offset], n, n, &d__, &c__1);
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	im1 = i__ - 1;
	i__2 = im1;
	for (j = 1; j <= i__2; ++j) {
	    v[i__ + j * v_dim1] = v[j + i__ * v_dim1];
/* L30: */
	}
/* L40: */
    }
    return 0;
} /* ch2inv_ */
