/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1997  Robert Gentleman and Ross Ihaka
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  This is closely based on EISPACK code
 *  Source cleaned up as far as htribk.
 */

#include "Platform.h"
#include "Fortran.h"

	/* Public Entry Points */

int F77_SYMBOL(cg)(int *nm, int *n, double *ar, double *ai, double *wr, double *wi, int *matz, double *zr, double *zi, double *fv1, double *fv2, double *fv3, int *ierr);
int F77_SYMBOL(ch)(int *nm, int *n, double *ar, double *ai, double *w, int *matz, double *zr, double *zi, double *fv1, double *fv2, double *fm1, int *ierr);
int F77_SYMBOL(rg)(int *nm, int *n, double *a, double *wr, double *wi, int *matz, double *z, int *iv1, double *fv1, int *ierr);
int F77_SYMBOL(rs)(int *nm, int *n, double *a, double *w, int *matz, double *z, double *fv1, double *fv2, int *ierr);

	/* Private Entry Points */

static int balanc_(int *nm, int *n, double *a, int *low, int *igh, double *scale);
static int balbak_(int *nm, int *n, int *low, int *igh, double *scale, int *m, double *z);
static int cbabk2_(int *nm, int *n, int *low, int *igh, double *scale, int *m, double *zr, double *zi);
static int cbal_(int *nm, int *n, double *ar, double *ai, int *low, int *igh, double *scale);
static int cdiv_(double *ar, double *ai, double *br, double *bi, double *cr, double *ci);
static int comqr_(int *nm, int *n, int *low, int *igh, double *hr, double *hi, double *wr, double *wi, int *ierr);
static int comqr2_(int *nm, int *n, int *low, int *igh, double *ortr, double *orti, double *hr, double *hi, double *wr, double *wi, double *zr, double *zi, int *ierr);
static int corth_(int *nm, int *n, int *low, int *igh, double *ar, double *ai, double *ortr, double *orti);
static int csroot_(double *xr, double *xi, double *yr, double *yi);
static int elmhes_(int *nm, int *n, int *low, int *igh, double *a, int *int_);
static int eltran_(int *nm, int *n, int *low, int *igh, double *a, int *int_, double *z);
static double epslon_(double *x);
static int hqr_(int *nm, int *n, int *low, int *igh, double *h, double *wr, double *wi, int *ierr);
static int hqr2_(int *nm, int *n, int *low, int *igh, double *h, double *wr, double *wi, double *z, int *ierr);
static int htribk_(int *nm, int *n, double *ar, double *ai, double *tau, int *m, double *zr, double *zi);
static int htridi_(int *nm, int *n, double *ar, double *ai, double *d, double *e, double *e2, double *tau);
static double pythag_(double *a, double *b);
static int tql1_(int *n, double *d, double *e, int *ierr);
static int tql2_(int *nm, int *n, double *d, double *e, double *z, int *ierr);
static int tqlrat_(int *n, double *d, double *e2, int *ierr);
static int tred1_(int *nm, int *n, double *a, double *d, double *e, double *e2);
static int tred2_(int *nm, int *n, double *a, double *d, double *e, double *z);


/*     subroutine balanc
 *
 *     this subroutine is a translation of the algol procedure balance,
 *     num. math. 13, 293-304(1969) by parlett and reinsch.
 *     handbook for auto. comp., vol.ii-linear algebra, 315-326(1971).
 *
 *     this subroutine balances a real matrix and isolates
 *     eigenvalues whenever possible.
 *
 *     on input
 *
 *        nm must be set to the row dimension of two-dimensional
 *          array parameters as declared in the calling program
 *          dimension statement.
 *
 *        n is the order of the matrix.
 *
 *        a contains the input matrix to be balanced.
 *
 *     on output
 *
 *        a contains the balanced matrix.
 *
 *        low and igh are two ints such that a(i,j)
 *          is equal to zero if
 *           (1) i is greater than j and
 *           (2) j=1,...,low-1 or i=igh+1,...,n.
 *
 *        scale contains information determining the
 *           permutations and scaling factors used.
 *
 *     suppose that the principal submatrix in rows low through igh
 *     has been balanced, that p(j) denotes the index interchanged
 *     with j during the permutation step, and that the elements
 *     of the diagonal matrix used are denoted by d(i,j).  then
 *        scale(j) = p(j),    for j = 1,...,low-1
 *                 = d(j,j),      j = low,...,igh
 *                 = p(j)         j = igh+1,...,n.
 *     the order in which the interchanges are made is n to igh+1,
 *     then 1 to low-1.
 *
 *     note that 1 is returned for igh if igh is zero formally.
 *
 *     the algol procedure exc contained in balance appears in
 *     balanc  in line.  (note that the algol roles of identifiers
 *     k,l have been reversed.)
 *
 *     questions and comments should be directed to burton s. garbow,
 *     mathematics and computer science div, argonne national laboratory 
 *
 *
 *     this version dated august 1983.
 */

	/* Table of constant values */

static double c_b256 = 0.;
static double c_b314 = 1.;

static int balanc_(int *nm, int *n, double *a, int *low,
	int *igh, double *scale)
{
    /* System generated locals */
    int a_dim1, a_offset, i__1, i__2;
    double d__1;

    /* Local variables */
    int iexc;
    double c, f, g;
    int i, j, k, l, m;
    double r, s, radix, b2;
    int jj;
    int noconv;
    /* Parameter adjustments */
    --scale;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    radix = 16.;

    b2 = radix * radix;
    k = 1;
    l = *n;
    goto L100;
/*     .......... in-line procedure for row and */
/*                column exchange .......... */
L20:
    scale[m] = (double) j;
    if (j == m) {
	goto L50;
    }

    i__1 = l;
    for (i = 1; i <= i__1; ++i) {
	f = a[i + j * a_dim1];
	a[i + j * a_dim1] = a[i + m * a_dim1];
	a[i + m * a_dim1] = f;
/* L30: */
    }

    i__1 = *n;
    for (i = k; i <= i__1; ++i) {
	f = a[j + i * a_dim1];
	a[j + i * a_dim1] = a[m + i * a_dim1];
	a[m + i * a_dim1] = f;
/* L40: */
    }

L50:
    switch (iexc) {
	case 1:  goto L80;
	case 2:  goto L130;
    }
/*     .......... search for rows isolating an eigenvalue */
/*                and push them down .......... */
L80:
    if (l == 1) {
	goto L280;
    }
    --l;
/*     .......... for j=l step -1 until 1 do -- .......... */
L100:
    i__1 = l;
    for (jj = 1; jj <= i__1; ++jj) {
	j = l + 1 - jj;

	i__2 = l;
	for (i = 1; i <= i__2; ++i) {
	    if (i == j) {
		goto L110;
	    }
	    if (a[j + i * a_dim1] != 0.) {
		goto L120;
	    }
L110:
	    ;
	}

	m = l;
	iexc = 1;
	goto L20;
L120:
	;
    }

    goto L140;
/*     .......... search for columns isolating an eigenvalue */
/*                and push them left .......... */
L130:
    ++k;

L140:
    i__1 = l;
    for (j = k; j <= i__1; ++j) {

	i__2 = l;
	for (i = k; i <= i__2; ++i) {
	    if (i == j) {
		goto L150;
	    }
	    if (a[i + j * a_dim1] != 0.) {
		goto L170;
	    }
L150:
	    ;
	}

	m = k;
	iexc = 2;
	goto L20;
L170:
	;
    }
/*     .......... now balance the submatrix in rows k to l .......... */
    i__1 = l;
    for (i = k; i <= i__1; ++i) {
/* L180: */
	scale[i] = 1.;
    }
/*     .......... iterative loop for norm reduction .......... */
L190:
    noconv = FALSE;

    i__1 = l;
    for (i = k; i <= i__1; ++i) {
	c = 0.;
	r = 0.;

	i__2 = l;
	for (j = k; j <= i__2; ++j) {
	    if (j == i) {
		goto L200;
	    }
	    c += (d__1 = a[j + i * a_dim1], abs(d__1));
	    r += (d__1 = a[i + j * a_dim1], abs(d__1));
L200:
	    ;
	}
/*     .......... guard against zero c or r due to underflow .........
. */
	if (c == 0. || r == 0.) {
	    goto L270;
	}
	g = r / radix;
	f = 1.;
	s = c + r;
L210:
	if (c >= g) {
	    goto L220;
	}
	f *= radix;
	c *= b2;
	goto L210;
L220:
	g = r * radix;
L230:
	if (c < g) {
	    goto L240;
	}
	f /= radix;
	c /= b2;
	goto L230;
/*     .......... now balance .......... */
L240:
	if ((c + r) / f >= s * .95) {
	    goto L270;
	}
	g = 1. / f;
	scale[i] *= f;
	noconv = TRUE;

	i__2 = *n;
	for (j = k; j <= i__2; ++j) {
/* L250: */
	    a[i + j * a_dim1] *= g;
	}

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
/* L260: */
	    a[j + i * a_dim1] *= f;
	}

L270:
	;
    }

    if (noconv) {
	goto L190;
    }

L280:
    *low = k;
    *igh = l;
    return 0;
}


/*     subroutine balbak
 *
 *     this subroutine is a translation of the algol procedure balbak,
 *     num. math. 13, 293-304(1969) by parlett and reinsch.
 *     handbook for auto. comp., vol.ii-linear algebra, 315-326(1971).
 *
 *     this subroutine forms the eigenvectors of a real general
 *     matrix by back transforming those of the corresponding
 *     balanced matrix determined by  balanc.
 *
 *     on input
 *
 *        nm must be set to the row dimension of two-dimensional
 *          array parameters as declared in the calling program
 *          dimension statement.
 *
 *        n is the order of the matrix.
 *
 *        low and igh are ints determined by  balanc.
 *
 *        scale contains information determining the permutations
 *          and scaling factors used by  balanc.
 *
 *        m is the number of columns of z to be back transformed.
 *
 *        z contains the real and imaginary parts of the eigen-
 *          vectors to be back transformed in its first m columns.
 *
 *     on output
 *
 *        z contains the real and imaginary parts of the
 *          transformed eigenvectors in its first m columns.
 *
 *     questions and comments should be directed to burton s. garbow,
 *     mathematics and computer science div, argonne national laboratory 
 *
 *     this version dated august 1983.
 */

static int balbak_(int *nm, int *n, int *low, int *igh,
	double *scale, int *m, double *z)
{
    /* System generated locals */
    int z_dim1, z_offset, i__1, i__2;

    /* Local variables */
    int i, j, k;
    double s;
    int ii;

    /* Parameter adjustments */
    --scale;
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z -= z_offset;

    /* Function Body */
    if (*m == 0) {
	goto L200;
    }
    if (*igh == *low) {
	goto L120;
    }

    i__1 = *igh;
    for (i = *low; i <= i__1; ++i) {
	s = scale[i];
/*     .......... left hand eigenvectors are back transformed */
/*                if the foregoing statement is replaced by */
/*                s=1.0d0/scale(i). .......... */
	i__2 = *m;
	for (j = 1; j <= i__2; ++j) {
/* L100: */
	    z[i + j * z_dim1] *= s;
	}

/* L110: */
    }
/*     ......... for i=low-1 step -1 until 1, */
/*               igh+1 step 1 until n do -- .......... */
L120:
    i__1 = *n;
    for (ii = 1; ii <= i__1; ++ii) {
	i = ii;
	if (i >= *low && i <= *igh) {
	    goto L140;
	}
	if (i < *low) {
	    i = *low - ii;
	}
	k = (int) scale[i];
	if (k == i) {
	    goto L140;
	}

	i__2 = *m;
	for (j = 1; j <= i__2; ++j) {
	    s = z[i + j * z_dim1];
	    z[i + j * z_dim1] = z[k + j * z_dim1];
	    z[k + j * z_dim1] = s;
/* L130: */
	}

L140:
	;
    }

L200:
    return 0;
}


/*     subroutine cbabk2
 *
 *     this subroutine is a translation of the algol procedure
 *     cbabk2, which is a complex version of balbak,
 *     num. math. 13, 293-304(1969) by parlett and reinsch.
 *     handbook for auto. comp., vol.ii-linear algebra, 315-326(1971).
 *
 *     this subroutine forms the eigenvectors of a complex general
 *     matrix by back transforming those of the corresponding
 *     balanced matrix determined by  cbal.
 *
 *     on input
 *
 *        nm must be set to the row dimension of two-dimensional
 *          array parameters as declared in the calling program
 *          dimension statement.
 *
 *        n is the order of the matrix.
 *
 *        low and igh are ints determined by  cbal.
 *
 *        scale contains information determining the permutations
 *          and scaling factors used by  cbal.
 *
 *        m is the number of eigenvectors to be back transformed.
 *
 *        zr and zi contain the real and imaginary parts,
 *          respectively, of the eigenvectors to be
 *          back transformed in their first m columns.
 *
 *     on output
 *
 *        zr and zi contain the real and imaginary parts,
 *          respectively, of the transformed eigenvectors
 *          in their first m columns.
 *
 *     questions and comments should be directed to burton s. garbow,
 *     mathematics and computer science div, argonne national laboratory 
 *
 *     this version dated august 1983.
 */

static int cbabk2_(int *nm, int *n, int *low, int *igh,
	double *scale, int *m, double *zr, double *zi)
{
    /* System generated locals */
    int zr_dim1, zr_offset, zi_dim1, zi_offset, i__1, i__2;

    /* Local variables */
    int i, j, k;
    double s;
    int ii;

    /* Parameter adjustments */
    --scale;
    zi_dim1 = *nm;
    zi_offset = zi_dim1 + 1;
    zi -= zi_offset;
    zr_dim1 = *nm;
    zr_offset = zr_dim1 + 1;
    zr -= zr_offset;

    /* Function Body */
    if (*m == 0) {
	goto L200;
    }
    if (*igh == *low) {
	goto L120;
    }

    i__1 = *igh;
    for (i = *low; i <= i__1; ++i) {
	s = scale[i];
/*     .......... left hand eigenvectors are back transformed */
/*                if the foregoing statement is replaced by */
/*                s=1.0d0/scale(i). .......... */
	i__2 = *m;
	for (j = 1; j <= i__2; ++j) {
	    zr[i + j * zr_dim1] *= s;
	    zi[i + j * zi_dim1] *= s;
/* L100: */
	}

/* L110: */
    }
/*     .......... for i=low-1 step -1 until 1, */
/*                igh+1 step 1 until n do -- .......... */
L120:
    i__1 = *n;
    for (ii = 1; ii <= i__1; ++ii) {
	i = ii;
	if (i >= *low && i <= *igh) {
	    goto L140;
	}
	if (i < *low) {
	    i = *low - ii;
	}
	k = (int) scale[i];
	if (k == i) {
	    goto L140;
	}

	i__2 = *m;
	for (j = 1; j <= i__2; ++j) {
	    s = zr[i + j * zr_dim1];
	    zr[i + j * zr_dim1] = zr[k + j * zr_dim1];
	    zr[k + j * zr_dim1] = s;
	    s = zi[i + j * zi_dim1];
	    zi[i + j * zi_dim1] = zi[k + j * zi_dim1];
	    zi[k + j * zi_dim1] = s;
/* L130: */
	}

L140:
	;
    }

L200:
    return 0;
}


/*     subroutine cbal
 *
 *     this subroutine is a translation of the algol procedure
 *     cbalance, which is a complex version of balance,
 *     num. math. 13, 293-304(1969) by parlett and reinsch.
 *     handbook for auto. comp., vol.ii-linear algebra, 315-326(1971).
 *
 *     this subroutine balances a complex matrix and isolates
 *     eigenvalues whenever possible.
 *
 *     on input
 *
 *        nm must be set to the row dimension of two-dimensional
 *          array parameters as declared in the calling program
 *          dimension statement.
 *
 *        n is the order of the matrix.
 *
 *        ar and ai contain the real and imaginary parts,
 *          respectively, of the complex matrix to be balanced.
 *
 *     on output
 *
 *        ar and ai contain the real and imaginary parts,
 *          respectively, of the balanced matrix.
 *
 *        low and igh are two ints such that ar(i,j) and ai(i,j)
 *          are equal to zero if
 *           (1) i is greater than j and
 *           (2) j=1,...,low-1 or i=igh+1,...,n.
 *
 *        scale contains information determining the
 *           permutations and scaling factors used.
 *
 *     suppose that the principal submatrix in rows low through igh
 *     has been balanced, that p(j) denotes the index interchanged
 *     with j during the permutation step, and that the elements
 *     of the diagonal matrix used are denoted by d(i,j).  then
 *        scale(j) = p(j),    for j = 1,...,low-1
 *                 = d(j,j)       j = low,...,igh
 *                 = p(j)         j = igh+1,...,n.
 *     the order in which the interchanges are made is n to igh+1,
 *     then 1 to low-1.
 *
 *     note that 1 is returned for igh if igh is zero formally.
 *
 *     the algol procedure exc contained in cbalance appears in
 *     cbal  in line.  (note that the algol roles of identifiers
 *     k,l have been reversed.)
 *
 *     arithmetic is real throughout.
 *
 *     questions and comments should be directed to burton s. garbow,
 *     mathematics and computer science div, argonne national laboratory 
 *
 *     this version dated august 1983.
 */

static int cbal_(int *nm, int *n, double *ar, 
	double *ai, int *low, int *igh, double *scale)
{
    /* System generated locals */
    int ar_dim1, ar_offset, ai_dim1, ai_offset, i__1, i__2;
    double d__1, d__2;

    /* Local variables */
    int iexc;
    double c, f, g;
    int i, j, k, l, m;
    double r, s, radix, b2;
    int jj;
    int noconv;

    /* Parameter adjustments */
    --scale;
    ai_dim1 = *nm;
    ai_offset = ai_dim1 + 1;
    ai -= ai_offset;
    ar_dim1 = *nm;
    ar_offset = ar_dim1 + 1;
    ar -= ar_offset;

    /* Function Body */
    radix = 16.;

    b2 = radix * radix;
    k = 1;
    l = *n;
    goto L100;
/*     .......... in-line procedure for row and */
/*                column exchange .......... */
L20:
    scale[m] = (double) j;
    if (j == m) {
	goto L50;
    }

    i__1 = l;
    for (i = 1; i <= i__1; ++i) {
	f = ar[i + j * ar_dim1];
	ar[i + j * ar_dim1] = ar[i + m * ar_dim1];
	ar[i + m * ar_dim1] = f;
	f = ai[i + j * ai_dim1];
	ai[i + j * ai_dim1] = ai[i + m * ai_dim1];
	ai[i + m * ai_dim1] = f;
/* L30: */
    }

    i__1 = *n;
    for (i = k; i <= i__1; ++i) {
	f = ar[j + i * ar_dim1];
	ar[j + i * ar_dim1] = ar[m + i * ar_dim1];
	ar[m + i * ar_dim1] = f;
	f = ai[j + i * ai_dim1];
	ai[j + i * ai_dim1] = ai[m + i * ai_dim1];
	ai[m + i * ai_dim1] = f;
/* L40: */
    }

L50:
    switch (iexc) {
	case 1:  goto L80;
	case 2:  goto L130;
    }
/*     .......... search for rows isolating an eigenvalue */
/*                and push them down .......... */
L80:
    if (l == 1) {
	goto L280;
    }
    --l;
/*     .......... for j=l step -1 until 1 do -- .......... */
L100:
    i__1 = l;
    for (jj = 1; jj <= i__1; ++jj) {
	j = l + 1 - jj;

	i__2 = l;
	for (i = 1; i <= i__2; ++i) {
	    if (i == j) {
		goto L110;
	    }
	    if (ar[j + i * ar_dim1] != 0. || ai[j + i * ai_dim1] != 0.) {
		goto L120;
	    }
L110:
	    ;
	}

	m = l;
	iexc = 1;
	goto L20;
L120:
	;
    }

    goto L140;
/*     .......... search for columns isolating an eigenvalue */
/*                and push them left .......... */
L130:
    ++k;

L140:
    i__1 = l;
    for (j = k; j <= i__1; ++j) {

	i__2 = l;
	for (i = k; i <= i__2; ++i) {
	    if (i == j) {
		goto L150;
	    }
	    if (ar[i + j * ar_dim1] != 0. || ai[i + j * ai_dim1] != 0.) {
		goto L170;
	    }
L150:
	    ;
	}

	m = k;
	iexc = 2;
	goto L20;
L170:
	;
    }
/*     .......... now balance the submatrix in rows k to l .......... */
    i__1 = l;
    for (i = k; i <= i__1; ++i) {
/* L180: */
	scale[i] = 1.;
    }
/*     .......... iterative loop for norm reduction .......... */
L190:
    noconv = FALSE;

    i__1 = l;
    for (i = k; i <= i__1; ++i) {
	c = 0.;
	r = 0.;

	i__2 = l;
	for (j = k; j <= i__2; ++j) {
	    if (j == i) {
		goto L200;
	    }
	    c = c + (d__1 = ar[j + i * ar_dim1], abs(d__1)) + (d__2 = ai[j + 
		    i * ai_dim1], abs(d__2));
	    r = r + (d__1 = ar[i + j * ar_dim1], abs(d__1)) + (d__2 = ai[i + 
		    j * ai_dim1], abs(d__2));
L200:
	    ;
	}
/*     .......... guard against zero c or r due to underflow .........
. */
	if (c == 0. || r == 0.) {
	    goto L270;
	}
	g = r / radix;
	f = 1.;
	s = c + r;
L210:
	if (c >= g) {
	    goto L220;
	}
	f *= radix;
	c *= b2;
	goto L210;
L220:
	g = r * radix;
L230:
	if (c < g) {
	    goto L240;
	}
	f /= radix;
	c /= b2;
	goto L230;
/*     .......... now balance .......... */
L240:
	if ((c + r) / f >= s * .95) {
	    goto L270;
	}
	g = 1. / f;
	scale[i] *= f;
	noconv = TRUE;

	i__2 = *n;
	for (j = k; j <= i__2; ++j) {
	    ar[i + j * ar_dim1] *= g;
	    ai[i + j * ai_dim1] *= g;
/* L250: */
	}

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    ar[j + i * ar_dim1] *= f;
	    ai[j + i * ai_dim1] *= f;
/* L260: */
	}

L270:
	;
    }

    if (noconv) {
	goto L190;
    }

L280:
    *low = k;
    *igh = l;
    return 0;
}


/*     subroutine cdiv
 *
 *     complex division, (cr,ci) = (ar,ai)/(br,bi)
 */

static int cdiv_(double *ar, double *ai, double *br, 
	double *bi, double *cr, double *ci)
{
    /* System generated locals */
    double d__1, d__2;

    /* Local variables */
    double s, ais, bis, ars, brs;

    s = abs(*br) + abs(*bi);
    ars = *ar / s;
    ais = *ai / s;
    brs = *br / s;
    bis = *bi / s;
	/* Computing 2nd power */
    d__1 = brs;
	/* Computing 2nd power */
    d__2 = bis;
    s = d__1 * d__1 + d__2 * d__2;
    *cr = (ars * brs + ais * bis) / s;
    *ci = (ais * brs - ars * bis) / s;
    return 0;
}


/*     subroutine cg
 *
 *     this subroutine calls the recommended sequence of
 *     subroutines from the eigensystem subroutine package (eispack)
 *     to find the eigenvalues and eigenvectors (if desired)
 *     of a complex general matrix.
 *
 *     on input
 *
 *        nm  must be set to the row dimension of the two-dimensional
 *        array parameters as declared in the calling program
 *        dimension statement.
 *
 *        n  is the order of the matrix  a=(ar,ai).
 *
 *        ar  and  ai  contain the real and imaginary parts,
 *        respectively, of the complex general matrix.
 *
 *        matz  is an int variable set equal to zero if
 *        only eigenvalues are desired.  otherwise it is set to
 *        any non-zero int for both eigenvalues and eigenvectors.
 *
 *     on output
 *
 *        wr  and  wi  contain the real and imaginary parts,
 *        respectively, of the eigenvalues.
 *
 *        zr  and  zi  contain the real and imaginary parts,
 *        respectively, of the eigenvectors if matz is not zero.
 *
 *        ierr  is an int output variable set equal to an error
 *           completion code described in the documentation for comqr
 *           and comqr2.  the normal completion code is zero.
 *
 *        fv1, fv2, and  fv3  are temporary storage arrays.
 *
 *     questions and comments should be directed to burton s. garbow,
 *     mathematics and computer science div, argonne national laboratory 
 *
 *     this version dated august 1983.
 */

int F77_SYMBOL(cg)(int *nm, int *n, double *ar, double *ai,
	double *wr, double *wi, int *matz, double *zr, 
	double *zi, double *fv1, double *fv2, double *fv3, 
	int *ierr)
{
    /* System generated locals */
    int ar_dim1, ar_offset, ai_dim1, ai_offset, zr_dim1, zr_offset, 
	    zi_dim1, zi_offset;

    /* Local variables */
    extern int cbal_(), corth_(), comqr_(), cbabk2_(), 
	    comqr2_();
    int is1, is2;

    /* Parameter adjustments */
    --fv3;
    --fv2;
    --fv1;
    zi_dim1 = *nm;
    zi_offset = zi_dim1 + 1;
    zi -= zi_offset;
    zr_dim1 = *nm;
    zr_offset = zr_dim1 + 1;
    zr -= zr_offset;
    --wi;
    --wr;
    ai_dim1 = *nm;
    ai_offset = ai_dim1 + 1;
    ai -= ai_offset;
    ar_dim1 = *nm;
    ar_offset = ar_dim1 + 1;
    ar -= ar_offset;

    /* Function Body */
    if (*n <= *nm) {
	goto L10;
    }
    *ierr = *n * 10;
    goto L50;

L10:
    cbal_(nm, n, &ar[ar_offset], &ai[ai_offset], &is1, &is2, &fv1[1]);
    corth_(nm, n, &is1, &is2, &ar[ar_offset], &ai[ai_offset], &fv2[1], &fv3[1]
	    );
    if (*matz != 0) {
	goto L20;
    }
/*     .......... find eigenvalues only .......... */
    comqr_(nm, n, &is1, &is2, &ar[ar_offset], &ai[ai_offset], &wr[1], &wi[1], 
	    ierr);
    goto L50;
/*     .......... find both eigenvalues and eigenvectors .......... */
L20:
    comqr2_(nm, n, &is1, &is2, &fv2[1], &fv3[1], &ar[ar_offset], &ai[
	    ai_offset], &wr[1], &wi[1], &zr[zr_offset], &zi[zi_offset], ierr);
    if (*ierr != 0) {
	goto L50;
    }
    cbabk2_(nm, n, &is1, &is2, &fv1[1], n, &zr[zr_offset], &zi[zi_offset]);
L50:
    return 0;
}


/*     subroutine ch
 *
 *     this subroutine calls the recommended sequence of
 *     subroutines from the eigensystem subroutine package (eispack)
 *     to find the eigenvalues and eigenvectors (if desired)
 *     of a complex hermitian matrix.
 *
 *     on input
 *
 *        nm  must be set to the row dimension of the two-dimensional
 *        array parameters as declared in the calling program
 *        dimension statement.
 *
 *        n  is the order of the matrix  a=(ar,ai).
 *
 *        ar  and  ai  contain the real and imaginary parts,
 *        respectively, of the complex hermitian matrix.
 *
 *        matz  is an int variable set equal to zero if
 *        only eigenvalues are desired.  otherwise it is set to
 *        any non-zero int for both eigenvalues and eigenvectors.
 *
 *     on output
 *
 *        w  contains the eigenvalues in ascending order.
 *
 *        zr  and  zi  contain the real and imaginary parts,
 *        respectively, of the eigenvectors if matz is not zero.
 *
 *        ierr  is an int output variable set equal to an error
 *           completion code described in the documentation for tqlrat
 *           and tql2.  the normal completion code is zero.
 *
 *        fv1, fv2, and  fm1  are temporary storage arrays.
 *
 *     questions and comments should be directed to burton s. garbow,
 *     mathematics and computer science div, argonne national laboratory 
 *
 *     this version dated august 1983.
 */

int F77_SYMBOL(ch)(int *nm, int *n, double *ar, double *ai,
	double *w, int *matz, double *zr, double *zi, 
	double *fv1, double *fv2, double *fm1, int *ierr)
{
    /* System generated locals */
    int ar_dim1, ar_offset, ai_dim1, ai_offset, zr_dim1, zr_offset, 
	    zi_dim1, zi_offset, i__1, i__2;

    /* Local variables */
    int i, j;
    extern int htridi_(), htribk_(), tqlrat_(), tql2_();

    /* Parameter adjustments */
    fm1 -= 3;
    --fv2;
    --fv1;
    zi_dim1 = *nm;
    zi_offset = zi_dim1 + 1;
    zi -= zi_offset;
    zr_dim1 = *nm;
    zr_offset = zr_dim1 + 1;
    zr -= zr_offset;
    --w;
    ai_dim1 = *nm;
    ai_offset = ai_dim1 + 1;
    ai -= ai_offset;
    ar_dim1 = *nm;
    ar_offset = ar_dim1 + 1;
    ar -= ar_offset;

    /* Function Body */
    if (*n <= *nm) {
	goto L10;
    }
    *ierr = *n * 10;
    goto L50;

L10:
    htridi_(nm, n, &ar[ar_offset], &ai[ai_offset], &w[1], &fv1[1], &fv2[1], &
	    fm1[3]);
    if (*matz != 0) {
	goto L20;
    }
/*     .......... find eigenvalues only .......... */
    tqlrat_(n, &w[1], &fv2[1], ierr);
    goto L50;
/*     .......... find both eigenvalues and eigenvectors .......... */
L20:
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {

	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
	    zr[j + i * zr_dim1] = 0.;
/* L30: */
	}

	zr[i + i * zr_dim1] = 1.;
/* L40: */
    }

    tql2_(nm, n, &w[1], &fv1[1], &zr[zr_offset], ierr);
    if (*ierr != 0) {
	goto L50;
    }
    htribk_(nm, n, &ar[ar_offset], &ai[ai_offset], &fm1[3], n, &zr[zr_offset],
	     &zi[zi_offset]);
L50:
    return 0;
}


/*     subroutine comqr
 *
 *     this subroutine is a translation of a unitary analogue of the
 *     algol procedure  comlr, num. math. 12, 369-376(1968) by martin
 *     and wilkinson.
 *     handbook for auto. comp., vol.ii-linear algebra, 396-403(1971)
 *     the unitary analogue substitutes the qr algorithm of francis
 *     (comp. jour. 4, 332-345(1962)) for the lr algorithm.
 *
 *     this subroutine finds the eigenvalues of a complex
 *     upper hessenberg matrix by the qr method.
 *
 *     on input
 *
 *        nm must be set to the row dimension of two-dimensional
 *          array parameters as declared in the calling program
 *          dimension statement.
 *
 *        n is the order of the matrix.
 *
 *        low and igh are ints determined by the balancing
 *          subroutine  cbal.  if  cbal  has not been used,
 *          set low=1, igh=n.
 *
 *        hr and hi contain the real and imaginary parts,
 *          respectively, of the complex upper hessenberg matrix.
 *          their lower triangles below the subdiagonal contain
 *          information about the unitary transformations used in
 *          the reduction by  corth, if performed.
 *
 *     on output
 *
 *        the upper hessenberg portions of hr and hi have been
 *          destroyed.  therefore, they must be saved before
 *          calling  comqr  if subsequent calculation of
 *          eigenvectors is to be performed.
 *
 *        wr and wi contain the real and imaginary parts,
 *          respectively, of the eigenvalues.  if an error
 *          exit is made, the eigenvalues should be correct
 *          for indices ierr+1,...,n.
 *
 *        ierr is set to
 *          zero       for normal return,
 *          j          if the limit of 30*n iterations is exhausted
 *                     while the j-th eigenvalue is being sought.
 *
 *     calls cdiv for complex division.
 *     calls csroot for complex square root.
 *     calls pythag for  dsqrt(a*a + b*b) .
 *
 *     questions and comments should be directed to burton s. garbow,
 *     mathematics and computer science div, argonne national laboratory 
 *
 *     this version dated august 1983.
 */

static int comqr_(int *nm, int *n, int *low, int *igh,
	double *hr, double *hi, double *wr, double *wi, 
	int *ierr)
{
    /* System generated locals */
    int hr_dim1, hr_offset, hi_dim1, hi_offset, i__1, i__2;
    double d__1, d__2, d__3, d__4;

    /* Local variables */
    extern int cdiv_();
    double norm;
    int i, j, l, en, ll;
    double si, ti, xi, yi, sr, tr, xr, yr;
    extern double pythag_();
    extern int csroot_();
    int lp1, itn, its;
    double zzi, zzr;
    int enm1;
    double tst1, tst2;

    /* Parameter adjustments */
    --wi;
    --wr;
    hi_dim1 = *nm;
    hi_offset = hi_dim1 + 1;
    hi -= hi_offset;
    hr_dim1 = *nm;
    hr_offset = hr_dim1 + 1;
    hr -= hr_offset;

    /* Function Body */
    *ierr = 0;
    if (*low == *igh) {
	goto L180;
    }
/*     .......... create real subdiagonal elements .......... */
    l = *low + 1;

    i__1 = *igh;
    for (i = l; i <= i__1; ++i) {
/* Computing MIN */
	i__2 = i + 1;
	ll = min(i__2,*igh);
	if (hi[i + (i - 1) * hi_dim1] == 0.) {
	    goto L170;
	}
	norm = pythag_(&hr[i + (i - 1) * hr_dim1], &hi[i + (i - 1) * hi_dim1])
		;
	yr = hr[i + (i - 1) * hr_dim1] / norm;
	yi = hi[i + (i - 1) * hi_dim1] / norm;
	hr[i + (i - 1) * hr_dim1] = norm;
	hi[i + (i - 1) * hi_dim1] = 0.;

	i__2 = *igh;
	for (j = i; j <= i__2; ++j) {
	    si = yr * hi[i + j * hi_dim1] - yi * hr[i + j * hr_dim1];
	    hr[i + j * hr_dim1] = yr * hr[i + j * hr_dim1] + yi * hi[i + j * 
		    hi_dim1];
	    hi[i + j * hi_dim1] = si;
/* L155: */
	}

	i__2 = ll;
	for (j = *low; j <= i__2; ++j) {
	    si = yr * hi[j + i * hi_dim1] + yi * hr[j + i * hr_dim1];
	    hr[j + i * hr_dim1] = yr * hr[j + i * hr_dim1] - yi * hi[j + i * 
		    hi_dim1];
	    hi[j + i * hi_dim1] = si;
/* L160: */
	}

L170:
	;
    }
/*     .......... store roots isolated by cbal .......... */
L180:
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	if (i >= *low && i <= *igh) {
	    goto L200;
	}
	wr[i] = hr[i + i * hr_dim1];
	wi[i] = hi[i + i * hi_dim1];
L200:
	;
    }

    en = *igh;
    tr = 0.;
    ti = 0.;
    itn = *n * 30;
/*     .......... search for next eigenvalue .......... */
L220:
    if (en < *low) {
	goto L1001;
    }
    its = 0;
    enm1 = en - 1;
/*     .......... look for single small sub-diagonal element */
/*                for l=en step -1 until low d0 -- .......... */
L240:
    i__1 = en;
    for (ll = *low; ll <= i__1; ++ll) {
	l = en + *low - ll;
	if (l == *low) {
	    goto L300;
	}
	tst1 = (d__1 = hr[l - 1 + (l - 1) * hr_dim1], abs(d__1)) + (d__2 = hi[
		l - 1 + (l - 1) * hi_dim1], abs(d__2)) + (d__3 = hr[l + l * 
		hr_dim1], abs(d__3)) + (d__4 = hi[l + l * hi_dim1], abs(d__4))
		;
	tst2 = tst1 + (d__1 = hr[l + (l - 1) * hr_dim1], abs(d__1));
	if (tst2 == tst1) {
	    goto L300;
	}
/* L260: */
    }
/*     .......... form shift .......... */
L300:
    if (l == en) {
	goto L660;
    }
    if (itn == 0) {
	goto L1000;
    }
    if (its == 10 || its == 20) {
	goto L320;
    }
    sr = hr[en + en * hr_dim1];
    si = hi[en + en * hi_dim1];
    xr = hr[enm1 + en * hr_dim1] * hr[en + enm1 * hr_dim1];
    xi = hi[enm1 + en * hi_dim1] * hr[en + enm1 * hr_dim1];
    if (xr == 0. && xi == 0.) {
	goto L340;
    }
    yr = (hr[enm1 + enm1 * hr_dim1] - sr) / 2.;
    yi = (hi[enm1 + enm1 * hi_dim1] - si) / 2.;
/* Computing 2nd power */
    d__2 = yr;
/* Computing 2nd power */
    d__3 = yi;
    d__1 = d__2 * d__2 - d__3 * d__3 + xr;
    d__4 = yr * 2. * yi + xi;
    csroot_(&d__1, &d__4, &zzr, &zzi);
    if (yr * zzr + yi * zzi >= 0.) {
	goto L310;
    }
    zzr = -zzr;
    zzi = -zzi;
L310:
    d__1 = yr + zzr;
    d__2 = yi + zzi;
    cdiv_(&xr, &xi, &d__1, &d__2, &xr, &xi);
    sr -= xr;
    si -= xi;
    goto L340;
/*     .......... form exceptional shift .......... */
L320:
    sr = (d__1 = hr[en + enm1 * hr_dim1], abs(d__1)) + (d__2 = hr[enm1 + (en 
	    - 2) * hr_dim1], abs(d__2));
    si = 0.;

L340:
    i__1 = en;
    for (i = *low; i <= i__1; ++i) {
	hr[i + i * hr_dim1] -= sr;
	hi[i + i * hi_dim1] -= si;
/* L360: */
    }

    tr += sr;
    ti += si;
    ++its;
    --itn;
/*     .......... reduce to triangle (rows) .......... */
    lp1 = l + 1;

    i__1 = en;
    for (i = lp1; i <= i__1; ++i) {
	sr = hr[i + (i - 1) * hr_dim1];
	hr[i + (i - 1) * hr_dim1] = 0.;
	d__1 = pythag_(&hr[i - 1 + (i - 1) * hr_dim1], &hi[i - 1 + (i - 1) * 
		hi_dim1]);
	norm = pythag_(&d__1, &sr);
	xr = hr[i - 1 + (i - 1) * hr_dim1] / norm;
	wr[i - 1] = xr;
	xi = hi[i - 1 + (i - 1) * hi_dim1] / norm;
	wi[i - 1] = xi;
	hr[i - 1 + (i - 1) * hr_dim1] = norm;
	hi[i - 1 + (i - 1) * hi_dim1] = 0.;
	hi[i + (i - 1) * hi_dim1] = sr / norm;

	i__2 = en;
	for (j = i; j <= i__2; ++j) {
	    yr = hr[i - 1 + j * hr_dim1];
	    yi = hi[i - 1 + j * hi_dim1];
	    zzr = hr[i + j * hr_dim1];
	    zzi = hi[i + j * hi_dim1];
	    hr[i - 1 + j * hr_dim1] = xr * yr + xi * yi + hi[i + (i - 1) * 
		    hi_dim1] * zzr;
	    hi[i - 1 + j * hi_dim1] = xr * yi - xi * yr + hi[i + (i - 1) * 
		    hi_dim1] * zzi;
	    hr[i + j * hr_dim1] = xr * zzr - xi * zzi - hi[i + (i - 1) * 
		    hi_dim1] * yr;
	    hi[i + j * hi_dim1] = xr * zzi + xi * zzr - hi[i + (i - 1) * 
		    hi_dim1] * yi;
/* L490: */
	}

/* L500: */
    }

    si = hi[en + en * hi_dim1];
    if (si == 0.) {
	goto L540;
    }
    norm = pythag_(&hr[en + en * hr_dim1], &si);
    sr = hr[en + en * hr_dim1] / norm;
    si /= norm;
    hr[en + en * hr_dim1] = norm;
    hi[en + en * hi_dim1] = 0.;
/*     .......... inverse operation (columns) .......... */
L540:
    i__1 = en;
    for (j = lp1; j <= i__1; ++j) {
	xr = wr[j - 1];
	xi = wi[j - 1];

	i__2 = j;
	for (i = l; i <= i__2; ++i) {
	    yr = hr[i + (j - 1) * hr_dim1];
	    yi = 0.;
	    zzr = hr[i + j * hr_dim1];
	    zzi = hi[i + j * hi_dim1];
	    if (i == j) {
		goto L560;
	    }
	    yi = hi[i + (j - 1) * hi_dim1];
	    hi[i + (j - 1) * hi_dim1] = xr * yi + xi * yr + hi[j + (j - 1) * 
		    hi_dim1] * zzi;
L560:
	    hr[i + (j - 1) * hr_dim1] = xr * yr - xi * yi + hi[j + (j - 1) * 
		    hi_dim1] * zzr;
	    hr[i + j * hr_dim1] = xr * zzr + xi * zzi - hi[j + (j - 1) * 
		    hi_dim1] * yr;
	    hi[i + j * hi_dim1] = xr * zzi - xi * zzr - hi[j + (j - 1) * 
		    hi_dim1] * yi;
/* L580: */
	}

/* L600: */
    }

    if (si == 0.) {
	goto L240;
    }

    i__1 = en;
    for (i = l; i <= i__1; ++i) {
	yr = hr[i + en * hr_dim1];
	yi = hi[i + en * hi_dim1];
	hr[i + en * hr_dim1] = sr * yr - si * yi;
	hi[i + en * hi_dim1] = sr * yi + si * yr;
/* L630: */
    }

    goto L240;
/*     .......... a root found .......... */
L660:
    wr[en] = hr[en + en * hr_dim1] + tr;
    wi[en] = hi[en + en * hi_dim1] + ti;
    en = enm1;
    goto L220;
/*     .......... set error -- all eigenvalues have not */
/*                converged after 30*n iterations .......... */
L1000:
    *ierr = en;
L1001:
    return 0;
}


/*     subroutine comqr2
 *
 *     MESHED overflow control WITH vectors of isolated roots (10/19/89 BSG) 
 *     MESHED overflow control WITH triangular multiply (10/30/89 BSG)
 *
 *     this subroutine is a translation of a unitary analogue of the
 *     algol procedure  comlr2, num. math. 16, 181-204(1970) by peters
 *     and wilkinson.
 *     handbook for auto. comp., vol.ii-linear algebra, 372-395(1971).
 *     the unitary analogue substitutes the qr algorithm of francis
 *     (comp. jour. 4, 332-345(1962)) for the lr algorithm.
 *
 *     this subroutine finds the eigenvalues and eigenvectors
 *     of a complex upper hessenberg matrix by the qr
 *     method.  the eigenvectors of a complex general matrix
 *     can also be found if  corth  has been used to reduce
 *     this general matrix to hessenberg form.
 *
 *     on input
 *
 *        nm must be set to the row dimension of two-dimensional
 *          array parameters as declared in the calling program
 *          dimension statement.
 *
 *        n is the order of the matrix.
 *
 *        low and igh are ints determined by the balancing
 *          subroutine  cbal.  if  cbal  has not been used,
 *          set low=1, igh=n.
 *
 *        ortr and orti contain information about the unitary trans-
 *          formations used in the reduction by  corth, if performed.
 *          only elements low through igh are used.  if the eigenvectors 
 *          of the hessenberg matrix are desired, set ortr(j) and
 *          orti(j) to 0.0d0 for these elements.
 *
 *        hr and hi contain the real and imaginary parts,
 *          respectively, of the complex upper hessenberg matrix.
 *          their lower triangles below the subdiagonal contain further
 *          information about the transformations which were used in the 
 *          reduction by  corth, if performed.  if the eigenvectors of
 *          the hessenberg matrix are desired, these elements may be
 *          arbitrary.
 *
 *     on output
 *
 *        ortr, orti, and the upper hessenberg portions of hr and hi
 *          have been destroyed.
 *
 *        wr and wi contain the real and imaginary parts,
 *          respectively, of the eigenvalues.  if an error
 *          exit is made, the eigenvalues should be correct
 *          for indices ierr+1,...,n.
 *
 *        zr and zi contain the real and imaginary parts,
 *          respectively, of the eigenvectors.  the eigenvectors
 *          are unnormalized.  if an error exit is made, none of
 *          the eigenvectors has been found.
 *
 *        ierr is set to
 *          zero       for normal return,
 *          j          if the limit of 30*n iterations is exhausted
 *                     while the j-th eigenvalue is being sought.
 *
 *     calls cdiv for complex division.
 *     calls csroot for complex square root.
 *     calls pythag for  dsqrt(a*a + b*b) .
 *
 *     questions and comments should be directed to burton s. garbow,
 *     mathematics and computer science div, argonne national laboratory 
 *
 *     this version dated october 1989.
 */

static int comqr2_(int *nm, int *n, int *low, int *igh,
	double *ortr, double *orti, double *hr, double *hi,
	double *wr, double *wi, double *zr, double *zi, 
	int *ierr)
{
    /* System generated locals */
    int hr_dim1, hr_offset, hi_dim1, hi_offset, zr_dim1, zr_offset, 
	    zi_dim1, zi_offset, i__1, i__2, i__3;
    double d__1, d__2, d__3, d__4;

    /* Local variables */
    int iend;
    extern int cdiv_();
    double norm;
    int i, j, k, l, m, ii, en, jj, ll, nn;
    double si, ti, xi, yi, sr, tr, xr, yr;
    extern double pythag_();
    extern int csroot_();
    int ip1, lp1, itn, its;
    double zzi, zzr;
    int enm1;
    double tst1, tst2;

    /* Parameter adjustments */
    zi_dim1 = *nm;
    zi_offset = zi_dim1 + 1;
    zi -= zi_offset;
    zr_dim1 = *nm;
    zr_offset = zr_dim1 + 1;
    zr -= zr_offset;
    --wi;
    --wr;
    hi_dim1 = *nm;
    hi_offset = hi_dim1 + 1;
    hi -= hi_offset;
    hr_dim1 = *nm;
    hr_offset = hr_dim1 + 1;
    hr -= hr_offset;
    --orti;
    --ortr;

    /* Function Body */
    *ierr = 0;
/*     .......... initialize eigenvector matrix .......... */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {

	i__2 = *n;
	for (i = 1; i <= i__2; ++i) {
	    zr[i + j * zr_dim1] = 0.;
	    zi[i + j * zi_dim1] = 0.;
/* L100: */
	}
	zr[j + j * zr_dim1] = 1.;
/* L101: */
    }
/*     .......... form the matrix of accumulated transformations */
/*                from the information left by corth .......... */
    iend = *igh - *low - 1;
    if (iend < 0) {
	goto L180;
    } else if (iend == 0) {
	goto L150;
    } else {
	goto L105;
    }
/*     .......... for i=igh-1 step -1 until low+1 do -- .......... */
L105:
    i__1 = iend;
    for (ii = 1; ii <= i__1; ++ii) {
	i = *igh - ii;
	if (ortr[i] == 0. && orti[i] == 0.) {
	    goto L140;
	}
	if (hr[i + (i - 1) * hr_dim1] == 0. && hi[i + (i - 1) * hi_dim1] == 
		0.) {
	    goto L140;
	}
/*     .......... norm below is negative of h formed in corth ........
.. */
	norm = hr[i + (i - 1) * hr_dim1] * ortr[i] + hi[i + (i - 1) * hi_dim1]
		 * orti[i];
	ip1 = i + 1;

	i__2 = *igh;
	for (k = ip1; k <= i__2; ++k) {
	    ortr[k] = hr[k + (i - 1) * hr_dim1];
	    orti[k] = hi[k + (i - 1) * hi_dim1];
/* L110: */
	}

	i__2 = *igh;
	for (j = i; j <= i__2; ++j) {
	    sr = 0.;
	    si = 0.;

	    i__3 = *igh;
	    for (k = i; k <= i__3; ++k) {
		sr = sr + ortr[k] * zr[k + j * zr_dim1] + orti[k] * zi[k + j *
			 zi_dim1];
		si = si + ortr[k] * zi[k + j * zi_dim1] - orti[k] * zr[k + j *
			 zr_dim1];
/* L115: */
	    }

	    sr /= norm;
	    si /= norm;

	    i__3 = *igh;
	    for (k = i; k <= i__3; ++k) {
		zr[k + j * zr_dim1] = zr[k + j * zr_dim1] + sr * ortr[k] - si 
			* orti[k];
		zi[k + j * zi_dim1] = zi[k + j * zi_dim1] + sr * orti[k] + si 
			* ortr[k];
/* L120: */
	    }

/* L130: */
	}

L140:
	;
    }
/*     .......... create real subdiagonal elements .......... */
L150:
    l = *low + 1;

    i__1 = *igh;
    for (i = l; i <= i__1; ++i) {
/* Computing MIN */
	i__2 = i + 1;
	ll = min(i__2,*igh);
	if (hi[i + (i - 1) * hi_dim1] == 0.) {
	    goto L170;
	}
	norm = pythag_(&hr[i + (i - 1) * hr_dim1], &hi[i + (i - 1) * hi_dim1])
		;
	yr = hr[i + (i - 1) * hr_dim1] / norm;
	yi = hi[i + (i - 1) * hi_dim1] / norm;
	hr[i + (i - 1) * hr_dim1] = norm;
	hi[i + (i - 1) * hi_dim1] = 0.;

	i__2 = *n;
	for (j = i; j <= i__2; ++j) {
	    si = yr * hi[i + j * hi_dim1] - yi * hr[i + j * hr_dim1];
	    hr[i + j * hr_dim1] = yr * hr[i + j * hr_dim1] + yi * hi[i + j * 
		    hi_dim1];
	    hi[i + j * hi_dim1] = si;
/* L155: */
	}

	i__2 = ll;
	for (j = 1; j <= i__2; ++j) {
	    si = yr * hi[j + i * hi_dim1] + yi * hr[j + i * hr_dim1];
	    hr[j + i * hr_dim1] = yr * hr[j + i * hr_dim1] - yi * hi[j + i * 
		    hi_dim1];
	    hi[j + i * hi_dim1] = si;
/* L160: */
	}

	i__2 = *igh;
	for (j = *low; j <= i__2; ++j) {
	    si = yr * zi[j + i * zi_dim1] + yi * zr[j + i * zr_dim1];
	    zr[j + i * zr_dim1] = yr * zr[j + i * zr_dim1] - yi * zi[j + i * 
		    zi_dim1];
	    zi[j + i * zi_dim1] = si;
/* L165: */
	}

L170:
	;
    }
/*     .......... store roots isolated by cbal .......... */
L180:
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	if (i >= *low && i <= *igh) {
	    goto L200;
	}
	wr[i] = hr[i + i * hr_dim1];
	wi[i] = hi[i + i * hi_dim1];
L200:
	;
    }

    en = *igh;
    tr = 0.;
    ti = 0.;
    itn = *n * 30;
/*     .......... search for next eigenvalue .......... */
L220:
    if (en < *low) {
	goto L680;
    }
    its = 0;
    enm1 = en - 1;
/*     .......... look for single small sub-diagonal element */
/*                for l=en step -1 until low do -- .......... */
L240:
    i__1 = en;
    for (ll = *low; ll <= i__1; ++ll) {
	l = en + *low - ll;
	if (l == *low) {
	    goto L300;
	}
	tst1 = (d__1 = hr[l - 1 + (l - 1) * hr_dim1], abs(d__1)) + (d__2 = hi[
		l - 1 + (l - 1) * hi_dim1], abs(d__2)) + (d__3 = hr[l + l * 
		hr_dim1], abs(d__3)) + (d__4 = hi[l + l * hi_dim1], abs(d__4))
		;
	tst2 = tst1 + (d__1 = hr[l + (l - 1) * hr_dim1], abs(d__1));
	if (tst2 == tst1) {
	    goto L300;
	}
/* L260: */
    }
/*     .......... form shift .......... */
L300:
    if (l == en) {
	goto L660;
    }
    if (itn == 0) {
	goto L1000;
    }
    if (its == 10 || its == 20) {
	goto L320;
    }
    sr = hr[en + en * hr_dim1];
    si = hi[en + en * hi_dim1];
    xr = hr[enm1 + en * hr_dim1] * hr[en + enm1 * hr_dim1];
    xi = hi[enm1 + en * hi_dim1] * hr[en + enm1 * hr_dim1];
    if (xr == 0. && xi == 0.) {
	goto L340;
    }
    yr = (hr[enm1 + enm1 * hr_dim1] - sr) / 2.;
    yi = (hi[enm1 + enm1 * hi_dim1] - si) / 2.;
/* Computing 2nd power */
    d__2 = yr;
/* Computing 2nd power */
    d__3 = yi;
    d__1 = d__2 * d__2 - d__3 * d__3 + xr;
    d__4 = yr * 2. * yi + xi;
    csroot_(&d__1, &d__4, &zzr, &zzi);
    if (yr * zzr + yi * zzi >= 0.) {
	goto L310;
    }
    zzr = -zzr;
    zzi = -zzi;
L310:
    d__1 = yr + zzr;
    d__2 = yi + zzi;
    cdiv_(&xr, &xi, &d__1, &d__2, &xr, &xi);
    sr -= xr;
    si -= xi;
    goto L340;
/*     .......... form exceptional shift .......... */
L320:
    sr = (d__1 = hr[en + enm1 * hr_dim1], abs(d__1)) + (d__2 = hr[enm1 + (en 
	    - 2) * hr_dim1], abs(d__2));
    si = 0.;

L340:
    i__1 = en;
    for (i = *low; i <= i__1; ++i) {
	hr[i + i * hr_dim1] -= sr;
	hi[i + i * hi_dim1] -= si;
/* L360: */
    }

    tr += sr;
    ti += si;
    ++its;
    --itn;
/*     .......... reduce to triangle (rows) .......... */
    lp1 = l + 1;

    i__1 = en;
    for (i = lp1; i <= i__1; ++i) {
	sr = hr[i + (i - 1) * hr_dim1];
	hr[i + (i - 1) * hr_dim1] = 0.;
	d__1 = pythag_(&hr[i - 1 + (i - 1) * hr_dim1], &hi[i - 1 + (i - 1) * 
		hi_dim1]);
	norm = pythag_(&d__1, &sr);
	xr = hr[i - 1 + (i - 1) * hr_dim1] / norm;
	wr[i - 1] = xr;
	xi = hi[i - 1 + (i - 1) * hi_dim1] / norm;
	wi[i - 1] = xi;
	hr[i - 1 + (i - 1) * hr_dim1] = norm;
	hi[i - 1 + (i - 1) * hi_dim1] = 0.;
	hi[i + (i - 1) * hi_dim1] = sr / norm;

	i__2 = *n;
	for (j = i; j <= i__2; ++j) {
	    yr = hr[i - 1 + j * hr_dim1];
	    yi = hi[i - 1 + j * hi_dim1];
	    zzr = hr[i + j * hr_dim1];
	    zzi = hi[i + j * hi_dim1];
	    hr[i - 1 + j * hr_dim1] = xr * yr + xi * yi + hi[i + (i - 1) * 
		    hi_dim1] * zzr;
	    hi[i - 1 + j * hi_dim1] = xr * yi - xi * yr + hi[i + (i - 1) * 
		    hi_dim1] * zzi;
	    hr[i + j * hr_dim1] = xr * zzr - xi * zzi - hi[i + (i - 1) * 
		    hi_dim1] * yr;
	    hi[i + j * hi_dim1] = xr * zzi + xi * zzr - hi[i + (i - 1) * 
		    hi_dim1] * yi;
/* L490: */
	}

/* L500: */
    }

    si = hi[en + en * hi_dim1];
    if (si == 0.) {
	goto L540;
    }
    norm = pythag_(&hr[en + en * hr_dim1], &si);
    sr = hr[en + en * hr_dim1] / norm;
    si /= norm;
    hr[en + en * hr_dim1] = norm;
    hi[en + en * hi_dim1] = 0.;
    if (en == *n) {
	goto L540;
    }
    ip1 = en + 1;

    i__1 = *n;
    for (j = ip1; j <= i__1; ++j) {
	yr = hr[en + j * hr_dim1];
	yi = hi[en + j * hi_dim1];
	hr[en + j * hr_dim1] = sr * yr + si * yi;
	hi[en + j * hi_dim1] = sr * yi - si * yr;
/* L520: */
    }
/*     .......... inverse operation (columns) .......... */
L540:
    i__1 = en;
    for (j = lp1; j <= i__1; ++j) {
	xr = wr[j - 1];
	xi = wi[j - 1];

	i__2 = j;
	for (i = 1; i <= i__2; ++i) {
	    yr = hr[i + (j - 1) * hr_dim1];
	    yi = 0.;
	    zzr = hr[i + j * hr_dim1];
	    zzi = hi[i + j * hi_dim1];
	    if (i == j) {
		goto L560;
	    }
	    yi = hi[i + (j - 1) * hi_dim1];
	    hi[i + (j - 1) * hi_dim1] = xr * yi + xi * yr + hi[j + (j - 1) * 
		    hi_dim1] * zzi;
L560:
	    hr[i + (j - 1) * hr_dim1] = xr * yr - xi * yi + hi[j + (j - 1) * 
		    hi_dim1] * zzr;
	    hr[i + j * hr_dim1] = xr * zzr + xi * zzi - hi[j + (j - 1) * 
		    hi_dim1] * yr;
	    hi[i + j * hi_dim1] = xr * zzi - xi * zzr - hi[j + (j - 1) * 
		    hi_dim1] * yi;
/* L580: */
	}

	i__2 = *igh;
	for (i = *low; i <= i__2; ++i) {
	    yr = zr[i + (j - 1) * zr_dim1];
	    yi = zi[i + (j - 1) * zi_dim1];
	    zzr = zr[i + j * zr_dim1];
	    zzi = zi[i + j * zi_dim1];
	    zr[i + (j - 1) * zr_dim1] = xr * yr - xi * yi + hi[j + (j - 1) * 
		    hi_dim1] * zzr;
	    zi[i + (j - 1) * zi_dim1] = xr * yi + xi * yr + hi[j + (j - 1) * 
		    hi_dim1] * zzi;
	    zr[i + j * zr_dim1] = xr * zzr + xi * zzi - hi[j + (j - 1) * 
		    hi_dim1] * yr;
	    zi[i + j * zi_dim1] = xr * zzi - xi * zzr - hi[j + (j - 1) * 
		    hi_dim1] * yi;
/* L590: */
	}

/* L600: */
    }

    if (si == 0.) {
	goto L240;
    }

    i__1 = en;
    for (i = 1; i <= i__1; ++i) {
	yr = hr[i + en * hr_dim1];
	yi = hi[i + en * hi_dim1];
	hr[i + en * hr_dim1] = sr * yr - si * yi;
	hi[i + en * hi_dim1] = sr * yi + si * yr;
/* L630: */
    }

    i__1 = *igh;
    for (i = *low; i <= i__1; ++i) {
	yr = zr[i + en * zr_dim1];
	yi = zi[i + en * zi_dim1];
	zr[i + en * zr_dim1] = sr * yr - si * yi;
	zi[i + en * zi_dim1] = sr * yi + si * yr;
/* L640: */
    }

    goto L240;
/*     .......... a root found .......... */
L660:
    hr[en + en * hr_dim1] += tr;
    wr[en] = hr[en + en * hr_dim1];
    hi[en + en * hi_dim1] += ti;
    wi[en] = hi[en + en * hi_dim1];
    en = enm1;
    goto L220;
/*     .......... all roots found.  backsubstitute to find */
/*                vectors of upper triangular form .......... */
L680:
    norm = 0.;

    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {

	i__2 = *n;
	for (j = i; j <= i__2; ++j) {
	    tr = (d__1 = hr[i + j * hr_dim1], abs(d__1)) + (d__2 = hi[i + j * 
		    hi_dim1], abs(d__2));
	    if (tr > norm) {
		norm = tr;
	    }
/* L720: */
	}
    }

    if (*n == 1 || norm == 0.) {
	goto L1001;
    }
/*     .......... for en=n step -1 until 2 do -- .......... */
    i__2 = *n;
    for (nn = 2; nn <= i__2; ++nn) {
	en = *n + 2 - nn;
	xr = wr[en];
	xi = wi[en];
	hr[en + en * hr_dim1] = 1.;
	hi[en + en * hi_dim1] = 0.;
	enm1 = en - 1;
/*     .......... for i=en-1 step -1 until 1 do -- .......... */
	i__1 = enm1;
	for (ii = 1; ii <= i__1; ++ii) {
	    i = en - ii;
	    zzr = 0.;
	    zzi = 0.;
	    ip1 = i + 1;

	    i__3 = en;
	    for (j = ip1; j <= i__3; ++j) {
		zzr = zzr + hr[i + j * hr_dim1] * hr[j + en * hr_dim1] - hi[i 
			+ j * hi_dim1] * hi[j + en * hi_dim1];
		zzi = zzi + hr[i + j * hr_dim1] * hi[j + en * hi_dim1] + hi[i 
			+ j * hi_dim1] * hr[j + en * hr_dim1];
/* L740: */
	    }

	    yr = xr - wr[i];
	    yi = xi - wi[i];
	    if (yr != 0. || yi != 0.) {
		goto L765;
	    }
	    tst1 = norm;
	    yr = tst1;
L760:
	    yr *= .01;
	    tst2 = norm + yr;
	    if (tst2 > tst1) {
		goto L760;
	    }
L765:
	    cdiv_(&zzr, &zzi, &yr, &yi, &hr[i + en * hr_dim1], &hi[i + en * 
		    hi_dim1]);
/*     .......... overflow control .......... */
	    tr = (d__1 = hr[i + en * hr_dim1], abs(d__1)) + (d__2 = hi[i + en 
		    * hi_dim1], abs(d__2));
	    if (tr == 0.) {
		goto L780;
	    }
	    tst1 = tr;
	    tst2 = tst1 + 1. / tst1;
	    if (tst2 > tst1) {
		goto L780;
	    }
	    i__3 = en;
	    for (j = i; j <= i__3; ++j) {
		hr[j + en * hr_dim1] /= tr;
		hi[j + en * hi_dim1] /= tr;
/* L770: */
	    }

L780:
	    ;
	}

/* L800: */
    }
/*     .......... end backsubstitution .......... */
/*     .......... vectors of isolated roots .......... */
    i__2 = *n;
    for (i = 1; i <= i__2; ++i) {
	if (i >= *low && i <= *igh) {
	    goto L840;
	}

	i__1 = *n;
	for (j = i; j <= i__1; ++j) {
	    zr[i + j * zr_dim1] = hr[i + j * hr_dim1];
	    zi[i + j * zi_dim1] = hi[i + j * hi_dim1];
/* L820: */
	}

L840:
	;
    }
/*     .......... multiply by transformation matrix to give */
/*                vectors of original full matrix. */
/*                for j=n step -1 until low do -- .......... */
    i__2 = *n;
    for (jj = *low; jj <= i__2; ++jj) {
	j = *n + *low - jj;
	m = min(j,*igh);

	i__1 = *igh;
	for (i = *low; i <= i__1; ++i) {
	    zzr = 0.;
	    zzi = 0.;

	    i__3 = m;
	    for (k = *low; k <= i__3; ++k) {
		zzr = zzr + zr[i + k * zr_dim1] * hr[k + j * hr_dim1] - zi[i 
			+ k * zi_dim1] * hi[k + j * hi_dim1];
		zzi = zzi + zr[i + k * zr_dim1] * hi[k + j * hi_dim1] + zi[i 
			+ k * zi_dim1] * hr[k + j * hr_dim1];
/* L860: */
	    }

	    zr[i + j * zr_dim1] = zzr;
	    zi[i + j * zi_dim1] = zzi;
/* L880: */
	}
    }

    goto L1001;
/*     .......... set error -- all eigenvalues have not */
/*                converged after 30*n iterations .......... */
L1000:
    *ierr = en;
L1001:
    return 0;
}


/*     subroutine corth
 *
 *     this subroutine is a translation of a complex analogue of
 *     the algol procedure orthes, num. math. 12, 349-368(1968)
 *     by martin and wilkinson.
 *     handbook for auto. comp., vol.ii-linear algebra, 339-358(1971).
 *
 *     given a complex general matrix, this subroutine
 *     reduces a submatrix situated in rows and columns
 *     low through igh to upper hessenberg form by
 *     unitary similarity transformations.
 *
 *     on input
 *
 *        nm must be set to the row dimension of two-dimensional
 *          array parameters as declared in the calling program
 *          dimension statement.
 *
 *        n is the order of the matrix.
 *
 *        low and igh are ints determined by the balancing
 *          subroutine  cbal.  if  cbal  has not been used,
 *          set low=1, igh=n.
 *
 *        ar and ai contain the real and imaginary parts,
 *          respectively, of the complex input matrix.
 *
 *     on output
 *
 *        ar and ai contain the real and imaginary parts,
 *          respectively, of the hessenberg matrix.  information
 *          about the unitary transformations used in the reduction
 *          is stored in the remaining triangles under the
 *          hessenberg matrix.
 *
 *        ortr and orti contain further information about the
 *          transformations.  only elements low through igh are used.
 *
 *     calls pythag for  dsqrt(a*a + b*b) .
 *
 *     questions and comments should be directed to burton s. garbow,
 *     mathematics and computer science div, argonne national laboratory 
 *
 *     this version dated august 1983.
 */

static int corth_(int *nm, int *n, int *low, int *igh,
	double *ar, double *ai, double *ortr, double *orti)
{
    /* System generated locals */
    int ar_dim1, ar_offset, ai_dim1, ai_offset, i__1, i__2, i__3;
    double d__1, d__2;

    /* Builtin functions */
    double sqrt();

    /* Local variables */
    double f, g, h;
    int i, j, m;
    double scale;
    int la;
    double fi;
    int ii, jj;
    double fr;
    int mp;
    extern double pythag_();
    int kp1;

    /* Parameter adjustments */
    ai_dim1 = *nm;
    ai_offset = ai_dim1 + 1;
    ai -= ai_offset;
    ar_dim1 = *nm;
    ar_offset = ar_dim1 + 1;
    ar -= ar_offset;
    --orti;
    --ortr;

    /* Function Body */
    la = *igh - 1;
    kp1 = *low + 1;
    if (la < kp1) {
	goto L200;
    }

    i__1 = la;
    for (m = kp1; m <= i__1; ++m) {
	h = 0.;
	ortr[m] = 0.;
	orti[m] = 0.;
	scale = 0.;
/*     .......... scale column (algol tol then not needed) .......... 
*/
	i__2 = *igh;
	for (i = m; i <= i__2; ++i) {
/* L90: */
	    scale = scale + (d__1 = ar[i + (m - 1) * ar_dim1], abs(d__1)) + (
		    d__2 = ai[i + (m - 1) * ai_dim1], abs(d__2));
	}

	if (scale == 0.) {
	    goto L180;
	}
	mp = m + *igh;
/*     .......... for i=igh step -1 until m do -- .......... */
	i__2 = *igh;
	for (ii = m; ii <= i__2; ++ii) {
	    i = mp - ii;
	    ortr[i] = ar[i + (m - 1) * ar_dim1] / scale;
	    orti[i] = ai[i + (m - 1) * ai_dim1] / scale;
	    h = h + ortr[i] * ortr[i] + orti[i] * orti[i];
/* L100: */
	}

	g = sqrt(h);
	f = pythag_(&ortr[m], &orti[m]);
	if (f == 0.) {
	    goto L103;
	}
	h += f * g;
	g /= f;
	ortr[m] = (g + 1.) * ortr[m];
	orti[m] = (g + 1.) * orti[m];
	goto L105;

L103:
	ortr[m] = g;
	ar[m + (m - 1) * ar_dim1] = scale;
/*     .......... form (i-(u*ut)/h) * a .......... */
L105:
	i__2 = *n;
	for (j = m; j <= i__2; ++j) {
	    fr = 0.;
	    fi = 0.;
/*     .......... for i=igh step -1 until m do -- .......... */
	    i__3 = *igh;
	    for (ii = m; ii <= i__3; ++ii) {
		i = mp - ii;
		fr = fr + ortr[i] * ar[i + j * ar_dim1] + orti[i] * ai[i + j *
			 ai_dim1];
		fi = fi + ortr[i] * ai[i + j * ai_dim1] - orti[i] * ar[i + j *
			 ar_dim1];
/* L110: */
	    }

	    fr /= h;
	    fi /= h;

	    i__3 = *igh;
	    for (i = m; i <= i__3; ++i) {
		ar[i + j * ar_dim1] = ar[i + j * ar_dim1] - fr * ortr[i] + fi 
			* orti[i];
		ai[i + j * ai_dim1] = ai[i + j * ai_dim1] - fr * orti[i] - fi 
			* ortr[i];
/* L120: */
	    }

/* L130: */
	}
/*     .......... form (i-(u*ut)/h)*a*(i-(u*ut)/h) .......... */
	i__2 = *igh;
	for (i = 1; i <= i__2; ++i) {
	    fr = 0.;
	    fi = 0.;
/*     .......... for j=igh step -1 until m do -- .......... */
	    i__3 = *igh;
	    for (jj = m; jj <= i__3; ++jj) {
		j = mp - jj;
		fr = fr + ortr[j] * ar[i + j * ar_dim1] - orti[j] * ai[i + j *
			 ai_dim1];
		fi = fi + ortr[j] * ai[i + j * ai_dim1] + orti[j] * ar[i + j *
			 ar_dim1];
/* L140: */
	    }

	    fr /= h;
	    fi /= h;

	    i__3 = *igh;
	    for (j = m; j <= i__3; ++j) {
		ar[i + j * ar_dim1] = ar[i + j * ar_dim1] - fr * ortr[j] - fi 
			* orti[j];
		ai[i + j * ai_dim1] = ai[i + j * ai_dim1] + fr * orti[j] - fi 
			* ortr[j];
/* L150: */
	    }

/* L160: */
	}

	ortr[m] = scale * ortr[m];
	orti[m] = scale * orti[m];
	ar[m + (m - 1) * ar_dim1] = -g * ar[m + (m - 1) * ar_dim1];
	ai[m + (m - 1) * ai_dim1] = -g * ai[m + (m - 1) * ai_dim1];
L180:
	;
    }

L200:
    return 0;
}

/*     subroutine csroot
 *
 *     (yr,yi) = complex dsqrt(xr,xi)
 *     branch chosen so that yr .ge. 0.0 and sign(yi) .eq. sign(xi)
 */

static int csroot_(double *xr, double *xi, double *yr, 
	double *yi)
{
    /* Builtin functions */
    double sqrt();

    /* Local variables */
    double s, ti, tr;
    extern double pythag_();

    tr = *xr;
    ti = *xi;
    s = sqrt((pythag_(&tr, &ti) + abs(tr)) * .5);
    if (tr >= 0.) {
	*yr = s;
    }
    if (ti < 0.) {
	s = -s;
    }
    if (tr <= 0.) {
	*yi = s;
    }
    if (tr < 0.) {
	*yr = ti / *yi * .5;
    }
    if (tr > 0.) {
	*yi = ti / *yr * .5;
    }
    return 0;
}


/*     subroutine elmhes
 *
 *     this subroutine is a translation of the algol procedure elmhes,
 *     num. math. 12, 349-368(1968) by martin and wilkinson.
 *     handbook for auto. comp., vol.ii-linear algebra, 339-358(1971).
 *
 *     given a real general matrix, this subroutine
 *     reduces a submatrix situated in rows and columns
 *     low through igh to upper hessenberg form by
 *     stabilized elementary similarity transformations.
 *
 *     on input
 *
 *        nm must be set to the row dimension of two-dimensional
 *          array parameters as declared in the calling program
 *          dimension statement.
 *
 *        n is the order of the matrix.
 *
 *        low and igh are ints determined by the balancing
 *          subroutine  balanc.  if  balanc  has not been used,
 *          set low=1, igh=n.
 *
 *        a contains the input matrix.
 *
 *     on output
 *
 *        a contains the hessenberg matrix.  the multipliers
 *          which were used in the reduction are stored in the
 *          remaining triangle under the hessenberg matrix.
 *
 *        int contains information on the rows and columns
 *          interchanged in the reduction.
 *          only elements low through igh are used.
 *
 *     questions and comments should be directed to burton s. garbow,
 *     mathematics and computer science div, argonne national laboratory 
 *
 *     this version dated august 1983.
 */

static int elmhes_(int *nm, int *n, int *low, int *igh,
	double *a, int *int_)
{
    /* System generated locals */
    int a_dim1, a_offset, i__1, i__2, i__3;
    double d__1;

    /* Local variables */
    int i, j, m;
    double x, y;
    int la, mm1, kp1, mp1;

    /* Parameter adjustments */
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    --int_;

    /* Function Body */
    la = *igh - 1;
    kp1 = *low + 1;
    if (la < kp1) {
	goto L200;
    }

    i__1 = la;
    for (m = kp1; m <= i__1; ++m) {
	mm1 = m - 1;
	x = 0.;
	i = m;

	i__2 = *igh;
	for (j = m; j <= i__2; ++j) {
	    if ((d__1 = a[j + mm1 * a_dim1], abs(d__1)) <= abs(x)) {
		goto L100;
	    }
	    x = a[j + mm1 * a_dim1];
	    i = j;
L100:
	    ;
	}

	int_[m] = i;
	if (i == m) {
	    goto L130;
	}
/*     .......... interchange rows and columns of a .......... */
	i__2 = *n;
	for (j = mm1; j <= i__2; ++j) {
	    y = a[i + j * a_dim1];
	    a[i + j * a_dim1] = a[m + j * a_dim1];
	    a[m + j * a_dim1] = y;
/* L110: */
	}

	i__2 = *igh;
	for (j = 1; j <= i__2; ++j) {
	    y = a[j + i * a_dim1];
	    a[j + i * a_dim1] = a[j + m * a_dim1];
	    a[j + m * a_dim1] = y;
/* L120: */
	}
/*     .......... end interchange .......... */
L130:
	if (x == 0.) {
	    goto L180;
	}
	mp1 = m + 1;

	i__2 = *igh;
	for (i = mp1; i <= i__2; ++i) {
	    y = a[i + mm1 * a_dim1];
	    if (y == 0.) {
		goto L160;
	    }
	    y /= x;
	    a[i + mm1 * a_dim1] = y;

	    i__3 = *n;
	    for (j = m; j <= i__3; ++j) {
/* L140: */
		a[i + j * a_dim1] -= y * a[m + j * a_dim1];
	    }

	    i__3 = *igh;
	    for (j = 1; j <= i__3; ++j) {
/* L150: */
		a[j + m * a_dim1] += y * a[j + i * a_dim1];
	    }

L160:
	    ;
	}

L180:
	;
    }

L200:
    return 0;
}



/*     subroutine eltran
 *
 *     this subroutine is a translation of the algol procedure elmtrans, 
 *     num. math. 16, 181-204(1970) by peters and wilkinson.
 *     handbook for auto. comp., vol.ii-linear algebra, 372-395(1971).
 *
 *     this subroutine accumulates the stabilized elementary
 *     similarity transformations used in the reduction of a
 *     real general matrix to upper hessenberg form by  elmhes.
 *
 *     on input
 *
 *        nm must be set to the row dimension of two-dimensional
 *          array parameters as declared in the calling program
 *          dimension statement.
 *
 *        n is the order of the matrix.
 *
 *        low and igh are ints determined by the balancing
 *          subroutine  balanc.  if  balanc  has not been used,
 *          set low=1, igh=n.
 *
 *        a contains the multipliers which were used in the
 *          reduction by  elmhes  in its lower triangle
 *          below the subdiagonal.
 *
 *        int contains information on the rows and columns
 *          interchanged in the reduction by  elmhes.
 *          only elements low through igh are used.
 *
 *     on output
 *
 *        z contains the transformation matrix produced in the
 *          reduction by  elmhes.
 *
 *     questions and comments should be directed to burton s. garbow,
 *     mathematics and computer science div, argonne national laboratory 
 *
 *     this version dated august 1983.
 */

static int eltran_(int *nm, int *n, int *low, int *igh,
	double *a, int *int_, double *z)
{
    /* System generated locals */
    int a_dim1, a_offset, z_dim1, z_offset, i__1, i__2;

    /* Local variables */
    int i, j, kl, mm, mp, mp1;

/*     .......... initialize z to identity matrix .......... */
    /* Parameter adjustments */
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z -= z_offset;
    --int_;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {

	i__2 = *n;
	for (i = 1; i <= i__2; ++i) {
/* L60: */
	    z[i + j * z_dim1] = 0.;
	}

	z[j + j * z_dim1] = 1.;
/* L80: */
    }

    kl = *igh - *low - 1;
    if (kl < 1) {
	goto L200;
    }
/*     .......... for mp=igh-1 step -1 until low+1 do -- .......... */
    i__1 = kl;
    for (mm = 1; mm <= i__1; ++mm) {
	mp = *igh - mm;
	mp1 = mp + 1;

	i__2 = *igh;
	for (i = mp1; i <= i__2; ++i) {
/* L100: */
	    z[i + mp * z_dim1] = a[i + (mp - 1) * a_dim1];
	}

	i = int_[mp];
	if (i == mp) {
	    goto L140;
	}

	i__2 = *igh;
	for (j = mp; j <= i__2; ++j) {
	    z[mp + j * z_dim1] = z[i + j * z_dim1];
	    z[i + j * z_dim1] = 0.;
/* L130: */
	}

	z[i + mp * z_dim1] = 1.;
L140:
	;
    }

L200:
    return 0;
}


/*     function epslon
 *
 *     estimate unit roundoff in quantities of size x.
 *
 *     this program should function properly on all systems
 *     satisfying the following two assumptions,
 *        1.  the base used in representing floating point
 *            numbers is not a power of three.
 *        2.  the quantity  a  in statement 10 is represented to
 *            the accuracy used in floating point variables
 *            that are stored in memory.
 *     the statement number 10 and the go to 10 are intended to
 *     force optimizing compilers to generate code satisfying
 *     assumption 2.
 *     under these assumptions, it should be true that,
 *            a  is not exactly equal to four-thirds,
 *            b  has a zero for its last bit or digit,
 *            c  is not exactly equal to one,
 *            eps  measures the separation of 1.0 from
 *                 the next larger floating point number.
 *     the developers of eispack would appreciate being informed
 *     about any systems where these assumptions do not hold.
 *
 *     this version dated 4/6/83.
 */

double epslon_(double *x)
{
    /* System generated locals */
    double ret_val, d__1;

    /* Local variables */
    double a, b, c, eps;

    a = 1.3333333333333333;
L10:
    b = a - 1.;
    c = b + b + b;
    eps = (d__1 = c - 1., abs(d__1));
    if (eps == 0.) {
	goto L10;
    }
    ret_val = eps * abs(*x);
    return ret_val;
}


/*     subroutine hqr
 *
 *     RESTORED CORRECT INDICES OF LOOPS (200,210,230,240). (9/29/89 BSG)
 *
 *     this subroutine is a translation of the algol procedure hqr,
 *     num. math. 14, 219-231(1970) by martin, peters, and wilkinson.
 *     handbook for auto. comp., vol.ii-linear algebra, 359-371(1971).
 *
 *     this subroutine finds the eigenvalues of a real
 *     upper hessenberg matrix by the qr method.
 *
 *     on input
 *
 *        nm must be set to the row dimension of two-dimensional
 *          array parameters as declared in the calling program
 *          dimension statement.
 *
 *        n is the order of the matrix.
 *
 *        low and igh are ints determined by the balancing
 *          subroutine  balanc.  if  balanc  has not been used,
 *          set low=1, igh=n.
 *
 *        h contains the upper hessenberg matrix.  information about
 *          the transformations used in the reduction to hessenberg
 *          form by  elmhes  or  orthes, if performed, is stored
 *          in the remaining triangle under the hessenberg matrix.
 *
 *     on output
 *
 *        h has been destroyed.  therefore, it must be saved
 *          before calling  hqr  if subsequent calculation and
 *          back transformation of eigenvectors is to be performed.
 *
 *        wr and wi contain the real and imaginary parts,
 *          respectively, of the eigenvalues.  the eigenvalues
 *          are unordered except that complex conjugate pairs
 *          of values appear consecutively with the eigenvalue
 *          having the positive imaginary part first.  if an
 *          error exit is made, the eigenvalues should be correct
 *          for indices ierr+1,...,n.
 *
 *        ierr is set to
 *          zero       for normal return,
 *          j          if the limit of 30*n iterations is exhausted
 *                     while the j-th eigenvalue is being sought.
 *
 *     questions and comments should be directed to burton s. garbow,
 *     mathematics and computer science div, argonne national laboratory 
 *
 *     this version dated september 1989.
 */

static int hqr_(int *nm, int *n, int *low, int *igh,
	 double *h, double *wr, double *wi, int *ierr)
{
    /* System generated locals */
    int h_dim1, h_offset, i__1, i__2, i__3;
    double d__1, d__2;

    /* Builtin functions */
    double sqrt(), DSIGN();

    /* Local variables */
    double norm;
    int i, j, k, l, m;
    double p, q, r, s, t, w, x, y;
    int na, en, ll, mm;
    double zz;
    int notlas;
    int mp2, itn, its, enm2;
    double tst1, tst2;

    /* Parameter adjustments */
    --wi;
    --wr;
    h_dim1 = *nm;
    h_offset = h_dim1 + 1;
    h -= h_offset;

    /* Function Body */
    *ierr = 0;
    norm = 0.;
    k = 1;
/*     .......... store roots isolated by balanc */
/*                and compute matrix norm .......... */
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {

	i__2 = *n;
	for (j = k; j <= i__2; ++j) {
/* L40: */
	    norm += (d__1 = h[i + j * h_dim1], abs(d__1));
	}

	k = i;
	if (i >= *low && i <= *igh) {
	    goto L50;
	}
	wr[i] = h[i + i * h_dim1];
	wi[i] = 0.;
L50:
	;
    }

    en = *igh;
    t = 0.;
    itn = *n * 30;
/*     .......... search for next eigenvalues .......... */
L60:
    if (en < *low) {
	goto L1001;
    }
    its = 0;
    na = en - 1;
    enm2 = na - 1;
/*     .......... look for single small sub-diagonal element */
/*                for l=en step -1 until low do -- .......... */
L70:
    i__1 = en;
    for (ll = *low; ll <= i__1; ++ll) {
	l = en + *low - ll;
	if (l == *low) {
	    goto L100;
	}
	s = (d__1 = h[l - 1 + (l - 1) * h_dim1], abs(d__1)) + (d__2 = h[l + l 
		* h_dim1], abs(d__2));
	if (s == 0.) {
	    s = norm;
	}
	tst1 = s;
	tst2 = tst1 + (d__1 = h[l + (l - 1) * h_dim1], abs(d__1));
	if (tst2 == tst1) {
	    goto L100;
	}
/* L80: */
    }
/*     .......... form shift .......... */
L100:
    x = h[en + en * h_dim1];
    if (l == en) {
	goto L270;
    }
    y = h[na + na * h_dim1];
    w = h[en + na * h_dim1] * h[na + en * h_dim1];
    if (l == na) {
	goto L280;
    }
    if (itn == 0) {
	goto L1000;
    }
    if (its != 10 && its != 20) {
	goto L130;
    }
/*     .......... form exceptional shift .......... */
    t += x;

    i__1 = en;
    for (i = *low; i <= i__1; ++i) {
/* L120: */
	h[i + i * h_dim1] -= x;
    }

    s = (d__1 = h[en + na * h_dim1], abs(d__1)) + (d__2 = h[na + enm2 * 
	    h_dim1], abs(d__2));
    x = s * .75;
    y = x;
    w = s * -.4375 * s;
L130:
    ++its;
    --itn;
/*     .......... look for two consecutive small */
/*                sub-diagonal elements. */
/*                for m=en-2 step -1 until l do -- .......... */
    i__1 = enm2;
    for (mm = l; mm <= i__1; ++mm) {
	m = enm2 + l - mm;
	zz = h[m + m * h_dim1];
	r = x - zz;
	s = y - zz;
	p = (r * s - w) / h[m + 1 + m * h_dim1] + h[m + (m + 1) * h_dim1];
	q = h[m + 1 + (m + 1) * h_dim1] - zz - r - s;
	r = h[m + 2 + (m + 1) * h_dim1];
	s = abs(p) + abs(q) + abs(r);
	p /= s;
	q /= s;
	r /= s;
	if (m == l) {
	    goto L150;
	}
	tst1 = abs(p) * ((d__1 = h[m - 1 + (m - 1) * h_dim1], abs(d__1)) + 
		abs(zz) + (d__2 = h[m + 1 + (m + 1) * h_dim1], abs(d__2)));
	tst2 = tst1 + (d__1 = h[m + (m - 1) * h_dim1], abs(d__1)) * (abs(q) + 
		abs(r));
	if (tst2 == tst1) {
	    goto L150;
	}
/* L140: */
    }

L150:
    mp2 = m + 2;

    i__1 = en;
    for (i = mp2; i <= i__1; ++i) {
	h[i + (i - 2) * h_dim1] = 0.;
	if (i == mp2) {
	    goto L160;
	}
	h[i + (i - 3) * h_dim1] = 0.;
L160:
	;
    }
/*     .......... double qr step involving rows l to en and */
/*                columns m to en .......... */
    i__1 = na;
    for (k = m; k <= i__1; ++k) {
	notlas = k != na;
	if (k == m) {
	    goto L170;
	}
	p = h[k + (k - 1) * h_dim1];
	q = h[k + 1 + (k - 1) * h_dim1];
	r = 0.;
	if (notlas) {
	    r = h[k + 2 + (k - 1) * h_dim1];
	}
	x = abs(p) + abs(q) + abs(r);
	if (x == 0.) {
	    goto L260;
	}
	p /= x;
	q /= x;
	r /= x;
L170:
	d__1 = sqrt(p * p + q * q + r * r);
	s = DSIGN(&d__1, &p);
	if (k == m) {
	    goto L180;
	}
	h[k + (k - 1) * h_dim1] = -s * x;
	goto L190;
L180:
	if (l != m) {
	    h[k + (k - 1) * h_dim1] = -h[k + (k - 1) * h_dim1];
	}
L190:
	p += s;
	x = p / s;
	y = q / s;
	zz = r / s;
	q /= p;
	r /= p;
	if (notlas) {
	    goto L225;
	}
/*     .......... row modification .......... */
	i__2 = en;
	for (j = k; j <= i__2; ++j) {
	    p = h[k + j * h_dim1] + q * h[k + 1 + j * h_dim1];
	    h[k + j * h_dim1] -= p * x;
	    h[k + 1 + j * h_dim1] -= p * y;
/* L200: */
	}

/* Computing MIN */
	i__2 = en, i__3 = k + 3;
	j = min(i__2,i__3);
/*     .......... column modification .......... */
	i__2 = j;
	for (i = l; i <= i__2; ++i) {
	    p = x * h[i + k * h_dim1] + y * h[i + (k + 1) * h_dim1];
	    h[i + k * h_dim1] -= p;
	    h[i + (k + 1) * h_dim1] -= p * q;
/* L210: */
	}
	goto L255;
L225:
/*     .......... row modification .......... */
	i__2 = en;
	for (j = k; j <= i__2; ++j) {
	    p = h[k + j * h_dim1] + q * h[k + 1 + j * h_dim1] + r * h[k + 2 + 
		    j * h_dim1];
	    h[k + j * h_dim1] -= p * x;
	    h[k + 1 + j * h_dim1] -= p * y;
	    h[k + 2 + j * h_dim1] -= p * zz;
/* L230: */
	}

/* Computing MIN */
	i__2 = en, i__3 = k + 3;
	j = min(i__2,i__3);
/*     .......... column modification .......... */
	i__2 = j;
	for (i = l; i <= i__2; ++i) {
	    p = x * h[i + k * h_dim1] + y * h[i + (k + 1) * h_dim1] + zz * h[
		    i + (k + 2) * h_dim1];
	    h[i + k * h_dim1] -= p;
	    h[i + (k + 1) * h_dim1] -= p * q;
	    h[i + (k + 2) * h_dim1] -= p * r;
/* L240: */
	}
L255:

L260:
	;
    }

    goto L70;
/*     .......... one root found .......... */
L270:
    wr[en] = x + t;
    wi[en] = 0.;
    en = na;
    goto L60;
/*     .......... two roots found .......... */
L280:
    p = (y - x) / 2.;
    q = p * p + w;
    zz = sqrt((abs(q)));
    x += t;
    if (q < 0.) {
	goto L320;
    }
/*     .......... real pair .......... */
    zz = p + DSIGN(&zz, &p);
    wr[na] = x + zz;
    wr[en] = wr[na];
    if (zz != 0.) {
	wr[en] = x - w / zz;
    }
    wi[na] = 0.;
    wi[en] = 0.;
    goto L330;
/*     .......... complex pair .......... */
L320:
    wr[na] = x + p;
    wr[en] = x + p;
    wi[na] = zz;
    wi[en] = -zz;
L330:
    en = enm2;
    goto L60;
/*     .......... set error -- all eigenvalues have not */
/*                converged after 30*n iterations .......... */
L1000:
    *ierr = en;
L1001:
    return 0;
}


/*     subroutine hqr2
 *
 *     this subroutine is a translation of the algol procedure hqr2,
 *     num. math. 16, 181-204(1970) by peters and wilkinson.
 *     handbook for auto. comp., vol.ii-linear algebra, 372-395(1971).
 *
 *     this subroutine finds the eigenvalues and eigenvectors
 *     of a real upper hessenberg matrix by the qr method.  the
 *     eigenvectors of a real general matrix can also be found
 *     if  elmhes  and  eltran  or  orthes  and  ortran  have
 *     been used to reduce this general matrix to hessenberg form
 *     and to accumulate the similarity transformations.
 *
 *     on input
 *
 *        nm must be set to the row dimension of two-dimensional
 *          array parameters as declared in the calling program
 *          dimension statement.
 *
 *        n is the order of the matrix.
 *
 *        low and igh are ints determined by the balancing
 *          subroutine  balanc.  if  balanc  has not been used,
 *          set low=1, igh=n.
 *
 *        h contains the upper hessenberg matrix.
 *
 *        z contains the transformation matrix produced by  eltran
 *          after the reduction by  elmhes, or by  ortran  after the
 *          reduction by  orthes, if performed.  if the eigenvectors
 *          of the hessenberg matrix are desired, z must contain the
 *          identity matrix.
 *
 *     on output
 *
 *        h has been destroyed.
 *
 *        wr and wi contain the real and imaginary parts,
 *          respectively, of the eigenvalues.  the eigenvalues
 *          are unordered except that complex conjugate pairs
 *          of values appear consecutively with the eigenvalue
 *          having the positive imaginary part first.  if an
 *          error exit is made, the eigenvalues should be correct
 *          for indices ierr+1,...,n.
 *
 *        z contains the real and imaginary parts of the eigenvectors.
 *          if the i-th eigenvalue is real, the i-th column of z
 *          contains its eigenvector.  if the i-th eigenvalue is complex 
 *          with positive imaginary part, the i-th and (i+1)-th
 *          columns of z contain the real and imaginary parts of its
 *          eigenvector.  the eigenvectors are unnormalized.  if an
 *          error exit is made, none of the eigenvectors has been found. 
 *
 *        ierr is set to
 *          zero       for normal return,
 *          j          if the limit of 30*n iterations is exhausted
 *                     while the j-th eigenvalue is being sought.
 *
 *     calls cdiv for complex division.
 *
 *     questions and comments should be directed to burton s. garbow,
 *     mathematics and computer science div, argonne national laboratory 
 *
 *     this version dated august 1983.
 */

static int hqr2_(int *nm, int *n, int *low, int *igh,
	double *h, double *wr, double *wi, double *z, 
	int *ierr)
{
    /* System generated locals */
    int h_dim1, h_offset, z_dim1, z_offset, i__1, i__2, i__3;
    double d__1, d__2, d__3, d__4;

    /* Builtin functions */
    double sqrt(), DSIGN();

    /* Local variables */
    extern int cdiv_();
    double norm;
    int i, j, k, l, m;
    double p, q, r, s, t, w, x, y;
    int na, ii, en, jj;
    double ra, sa;
    int ll, mm, nn;
    double vi, vr, zz;
    int notlas;
    int mp2, itn, its, enm2;
    double tst1, tst2;

    /* Parameter adjustments */
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z -= z_offset;
    --wi;
    --wr;
    h_dim1 = *nm;
    h_offset = h_dim1 + 1;
    h -= h_offset;

    /* Function Body */
    *ierr = 0;
    norm = 0.;
    k = 1;
/*     .......... store roots isolated by balanc */
/*                and compute matrix norm .......... */
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {

	i__2 = *n;
	for (j = k; j <= i__2; ++j) {
/* L40: */
	    norm += (d__1 = h[i + j * h_dim1], abs(d__1));
	}

	k = i;
	if (i >= *low && i <= *igh) {
	    goto L50;
	}
	wr[i] = h[i + i * h_dim1];
	wi[i] = 0.;
L50:
	;
    }

    en = *igh;
    t = 0.;
    itn = *n * 30;
/*     .......... search for next eigenvalues .......... */
L60:
    if (en < *low) {
	goto L340;
    }
    its = 0;
    na = en - 1;
    enm2 = na - 1;
/*     .......... look for single small sub-diagonal element */
/*                for l=en step -1 until low do -- .......... */
L70:
    i__1 = en;
    for (ll = *low; ll <= i__1; ++ll) {
	l = en + *low - ll;
	if (l == *low) {
	    goto L100;
	}
	s = (d__1 = h[l - 1 + (l - 1) * h_dim1], abs(d__1)) + (d__2 = h[l + l 
		* h_dim1], abs(d__2));
	if (s == 0.) {
	    s = norm;
	}
	tst1 = s;
	tst2 = tst1 + (d__1 = h[l + (l - 1) * h_dim1], abs(d__1));
	if (tst2 == tst1) {
	    goto L100;
	}
/* L80: */
    }
/*     .......... form shift .......... */
L100:
    x = h[en + en * h_dim1];
    if (l == en) {
	goto L270;
    }
    y = h[na + na * h_dim1];
    w = h[en + na * h_dim1] * h[na + en * h_dim1];
    if (l == na) {
	goto L280;
    }
    if (itn == 0) {
	goto L1000;
    }
    if (its != 10 && its != 20) {
	goto L130;
    }
/*     .......... form exceptional shift .......... */
    t += x;

    i__1 = en;
    for (i = *low; i <= i__1; ++i) {
/* L120: */
	h[i + i * h_dim1] -= x;
    }

    s = (d__1 = h[en + na * h_dim1], abs(d__1)) + (d__2 = h[na + enm2 * 
	    h_dim1], abs(d__2));
    x = s * .75;
    y = x;
    w = s * -.4375 * s;
L130:
    ++its;
    --itn;
/*     .......... look for two consecutive small */
/*                sub-diagonal elements. */
/*                for m=en-2 step -1 until l do -- .......... */
    i__1 = enm2;
    for (mm = l; mm <= i__1; ++mm) {
	m = enm2 + l - mm;
	zz = h[m + m * h_dim1];
	r = x - zz;
	s = y - zz;
	p = (r * s - w) / h[m + 1 + m * h_dim1] + h[m + (m + 1) * h_dim1];
	q = h[m + 1 + (m + 1) * h_dim1] - zz - r - s;
	r = h[m + 2 + (m + 1) * h_dim1];
	s = abs(p) + abs(q) + abs(r);
	p /= s;
	q /= s;
	r /= s;
	if (m == l) {
	    goto L150;
	}
	tst1 = abs(p) * ((d__1 = h[m - 1 + (m - 1) * h_dim1], abs(d__1)) + 
		abs(zz) + (d__2 = h[m + 1 + (m + 1) * h_dim1], abs(d__2)));
	tst2 = tst1 + (d__1 = h[m + (m - 1) * h_dim1], abs(d__1)) * (abs(q) + 
		abs(r));
	if (tst2 == tst1) {
	    goto L150;
	}
/* L140: */
    }

L150:
    mp2 = m + 2;

    i__1 = en;
    for (i = mp2; i <= i__1; ++i) {
	h[i + (i - 2) * h_dim1] = 0.;
	if (i == mp2) {
	    goto L160;
	}
	h[i + (i - 3) * h_dim1] = 0.;
L160:
	;
    }
/*     .......... double qr step involving rows l to en and */
/*                columns m to en .......... */
    i__1 = na;
    for (k = m; k <= i__1; ++k) {
	notlas = k != na;
	if (k == m) {
	    goto L170;
	}
	p = h[k + (k - 1) * h_dim1];
	q = h[k + 1 + (k - 1) * h_dim1];
	r = 0.;
	if (notlas) {
	    r = h[k + 2 + (k - 1) * h_dim1];
	}
	x = abs(p) + abs(q) + abs(r);
	if (x == 0.) {
	    goto L260;
	}
	p /= x;
	q /= x;
	r /= x;
L170:
	d__1 = sqrt(p * p + q * q + r * r);
	s = DSIGN(&d__1, &p);
	if (k == m) {
	    goto L180;
	}
	h[k + (k - 1) * h_dim1] = -s * x;
	goto L190;
L180:
	if (l != m) {
	    h[k + (k - 1) * h_dim1] = -h[k + (k - 1) * h_dim1];
	}
L190:
	p += s;
	x = p / s;
	y = q / s;
	zz = r / s;
	q /= p;
	r /= p;
	if (notlas) {
	    goto L225;
	}
/*     .......... row modification .......... */
	i__2 = *n;
	for (j = k; j <= i__2; ++j) {
	    p = h[k + j * h_dim1] + q * h[k + 1 + j * h_dim1];
	    h[k + j * h_dim1] -= p * x;
	    h[k + 1 + j * h_dim1] -= p * y;
/* L200: */
	}

/* Computing MIN */
	i__2 = en, i__3 = k + 3;
	j = min(i__2,i__3);
/*     .......... column modification .......... */
	i__2 = j;
	for (i = 1; i <= i__2; ++i) {
	    p = x * h[i + k * h_dim1] + y * h[i + (k + 1) * h_dim1];
	    h[i + k * h_dim1] -= p;
	    h[i + (k + 1) * h_dim1] -= p * q;
/* L210: */
	}
/*     .......... accumulate transformations .......... */
	i__2 = *igh;
	for (i = *low; i <= i__2; ++i) {
	    p = x * z[i + k * z_dim1] + y * z[i + (k + 1) * z_dim1];
	    z[i + k * z_dim1] -= p;
	    z[i + (k + 1) * z_dim1] -= p * q;
/* L220: */
	}
	goto L255;
L225:
/*     .......... row modification .......... */
	i__2 = *n;
	for (j = k; j <= i__2; ++j) {
	    p = h[k + j * h_dim1] + q * h[k + 1 + j * h_dim1] + r * h[k + 2 + 
		    j * h_dim1];
	    h[k + j * h_dim1] -= p * x;
	    h[k + 1 + j * h_dim1] -= p * y;
	    h[k + 2 + j * h_dim1] -= p * zz;
/* L230: */
	}

/* Computing MIN */
	i__2 = en, i__3 = k + 3;
	j = min(i__2,i__3);
/*     .......... column modification .......... */
	i__2 = j;
	for (i = 1; i <= i__2; ++i) {
	    p = x * h[i + k * h_dim1] + y * h[i + (k + 1) * h_dim1] + zz * h[
		    i + (k + 2) * h_dim1];
	    h[i + k * h_dim1] -= p;
	    h[i + (k + 1) * h_dim1] -= p * q;
	    h[i + (k + 2) * h_dim1] -= p * r;
/* L240: */
	}
/*     .......... accumulate transformations .......... */
	i__2 = *igh;
	for (i = *low; i <= i__2; ++i) {
	    p = x * z[i + k * z_dim1] + y * z[i + (k + 1) * z_dim1] + zz * z[
		    i + (k + 2) * z_dim1];
	    z[i + k * z_dim1] -= p;
	    z[i + (k + 1) * z_dim1] -= p * q;
	    z[i + (k + 2) * z_dim1] -= p * r;
/* L250: */
	}
L255:

L260:
	;
    }

    goto L70;
/*     .......... one root found .......... */
L270:
    h[en + en * h_dim1] = x + t;
    wr[en] = h[en + en * h_dim1];
    wi[en] = 0.;
    en = na;
    goto L60;
/*     .......... two roots found .......... */
L280:
    p = (y - x) / 2.;
    q = p * p + w;
    zz = sqrt((abs(q)));
    h[en + en * h_dim1] = x + t;
    x = h[en + en * h_dim1];
    h[na + na * h_dim1] = y + t;
    if (q < 0.) {
	goto L320;
    }
/*     .......... real pair .......... */
    zz = p + DSIGN(&zz, &p);
    wr[na] = x + zz;
    wr[en] = wr[na];
    if (zz != 0.) {
	wr[en] = x - w / zz;
    }
    wi[na] = 0.;
    wi[en] = 0.;
    x = h[en + na * h_dim1];
    s = abs(x) + abs(zz);
    p = x / s;
    q = zz / s;
    r = sqrt(p * p + q * q);
    p /= r;
    q /= r;
/*     .......... row modification .......... */
    i__1 = *n;
    for (j = na; j <= i__1; ++j) {
	zz = h[na + j * h_dim1];
	h[na + j * h_dim1] = q * zz + p * h[en + j * h_dim1];
	h[en + j * h_dim1] = q * h[en + j * h_dim1] - p * zz;
/* L290: */
    }
/*     .......... column modification .......... */
    i__1 = en;
    for (i = 1; i <= i__1; ++i) {
	zz = h[i + na * h_dim1];
	h[i + na * h_dim1] = q * zz + p * h[i + en * h_dim1];
	h[i + en * h_dim1] = q * h[i + en * h_dim1] - p * zz;
/* L300: */
    }
/*     .......... accumulate transformations .......... */
    i__1 = *igh;
    for (i = *low; i <= i__1; ++i) {
	zz = z[i + na * z_dim1];
	z[i + na * z_dim1] = q * zz + p * z[i + en * z_dim1];
	z[i + en * z_dim1] = q * z[i + en * z_dim1] - p * zz;
/* L310: */
    }

    goto L330;
/*     .......... complex pair .......... */
L320:
    wr[na] = x + p;
    wr[en] = x + p;
    wi[na] = zz;
    wi[en] = -zz;
L330:
    en = enm2;
    goto L60;
/*     .......... all roots found.  backsubstitute to find */
/*                vectors of upper triangular form .......... */
L340:
    if (norm == 0.) {
	goto L1001;
    }
/*     .......... for en=n step -1 until 1 do -- .......... */
    i__1 = *n;
    for (nn = 1; nn <= i__1; ++nn) {
	en = *n + 1 - nn;
	p = wr[en];
	q = wi[en];
	na = en - 1;
	if (q < 0.) {
	    goto L710;
	} else if (q == 0) {
	    goto L600;
	} else {
	    goto L800;
	}
/*     .......... real vector .......... */
L600:
	m = en;
	h[en + en * h_dim1] = 1.;
	if (na == 0) {
	    goto L800;
	}
/*     .......... for i=en-1 step -1 until 1 do -- .......... */
	i__2 = na;
	for (ii = 1; ii <= i__2; ++ii) {
	    i = en - ii;
	    w = h[i + i * h_dim1] - p;
	    r = 0.;

	    i__3 = en;
	    for (j = m; j <= i__3; ++j) {
/* L610: */
		r += h[i + j * h_dim1] * h[j + en * h_dim1];
	    }

	    if (wi[i] >= 0.) {
		goto L630;
	    }
	    zz = w;
	    s = r;
	    goto L700;
L630:
	    m = i;
	    if (wi[i] != 0.) {
		goto L640;
	    }
	    t = w;
	    if (t != 0.) {
		goto L635;
	    }
	    tst1 = norm;
	    t = tst1;
L632:
	    t *= .01;
	    tst2 = norm + t;
	    if (tst2 > tst1) {
		goto L632;
	    }
L635:
	    h[i + en * h_dim1] = -r / t;
	    goto L680;
/*     .......... solve real equations .......... */
L640:
	    x = h[i + (i + 1) * h_dim1];
	    y = h[i + 1 + i * h_dim1];
	    q = (wr[i] - p) * (wr[i] - p) + wi[i] * wi[i];
	    t = (x * s - zz * r) / q;
	    h[i + en * h_dim1] = t;
	    if (abs(x) <= abs(zz)) {
		goto L650;
	    }
	    h[i + 1 + en * h_dim1] = (-r - w * t) / x;
	    goto L680;
L650:
	    h[i + 1 + en * h_dim1] = (-s - y * t) / zz;

/*     .......... overflow control .......... */
L680:
	    t = (d__1 = h[i + en * h_dim1], abs(d__1));
	    if (t == 0.) {
		goto L700;
	    }
	    tst1 = t;
	    tst2 = tst1 + 1. / tst1;
	    if (tst2 > tst1) {
		goto L700;
	    }
	    i__3 = en;
	    for (j = i; j <= i__3; ++j) {
		h[j + en * h_dim1] /= t;
/* L690: */
	    }

L700:
	    ;
	}
/*     .......... end real vector .......... */
	goto L800;
/*     .......... complex vector .......... */
L710:
	m = na;
/*     .......... last vector component chosen imaginary so that */
/*                eigenvector matrix is triangular .......... */
	if ((d__1 = h[en + na * h_dim1], abs(d__1)) <= (d__2 = h[na + en * 
		h_dim1], abs(d__2))) {
	    goto L720;
	}
	h[na + na * h_dim1] = q / h[en + na * h_dim1];
	h[na + en * h_dim1] = -(h[en + en * h_dim1] - p) / h[en + na * h_dim1]
		;
	goto L730;
L720:
	d__1 = -h[na + en * h_dim1];
	d__2 = h[na + na * h_dim1] - p;
	cdiv_(&c_b256, &d__1, &d__2, &q, &h[na + na * h_dim1], &h[na + en * 
		h_dim1]);
L730:
	h[en + na * h_dim1] = 0.;
	h[en + en * h_dim1] = 1.;
	enm2 = na - 1;
	if (enm2 == 0) {
	    goto L800;
	}
/*     .......... for i=en-2 step -1 until 1 do -- .......... */
	i__2 = enm2;
	for (ii = 1; ii <= i__2; ++ii) {
	    i = na - ii;
	    w = h[i + i * h_dim1] - p;
	    ra = 0.;
	    sa = 0.;

	    i__3 = en;
	    for (j = m; j <= i__3; ++j) {
		ra += h[i + j * h_dim1] * h[j + na * h_dim1];
		sa += h[i + j * h_dim1] * h[j + en * h_dim1];
/* L760: */
	    }

	    if (wi[i] >= 0.) {
		goto L770;
	    }
	    zz = w;
	    r = ra;
	    s = sa;
	    goto L795;
L770:
	    m = i;
	    if (wi[i] != 0.) {
		goto L780;
	    }
	    d__1 = -ra;
	    d__2 = -sa;
	    cdiv_(&d__1, &d__2, &w, &q, &h[i + na * h_dim1], &h[i + en * 
		    h_dim1]);
	    goto L790;
/*     .......... solve complex equations .......... */
L780:
	    x = h[i + (i + 1) * h_dim1];
	    y = h[i + 1 + i * h_dim1];
	    vr = (wr[i] - p) * (wr[i] - p) + wi[i] * wi[i] - q * q;
	    vi = (wr[i] - p) * 2. * q;
	    if (vr != 0. || vi != 0.) {
		goto L784;
	    }
	    tst1 = norm * (abs(w) + abs(q) + abs(x) + abs(y) + abs(zz));
	    vr = tst1;
L783:
	    vr *= .01;
	    tst2 = tst1 + vr;
	    if (tst2 > tst1) {
		goto L783;
	    }
L784:
	    d__1 = x * r - zz * ra + q * sa;
	    d__2 = x * s - zz * sa - q * ra;
	    cdiv_(&d__1, &d__2, &vr, &vi, &h[i + na * h_dim1], &h[i + en * 
		    h_dim1]);
	    if (abs(x) <= abs(zz) + abs(q)) {
		goto L785;
	    }
	    h[i + 1 + na * h_dim1] = (-ra - w * h[i + na * h_dim1] + q * h[i 
		    + en * h_dim1]) / x;
	    h[i + 1 + en * h_dim1] = (-sa - w * h[i + en * h_dim1] - q * h[i 
		    + na * h_dim1]) / x;
	    goto L790;
L785:
	    d__1 = -r - y * h[i + na * h_dim1];
	    d__2 = -s - y * h[i + en * h_dim1];
	    cdiv_(&d__1, &d__2, &zz, &q, &h[i + 1 + na * h_dim1], &h[i + 1 + 
		    en * h_dim1]);

/*     .......... overflow control .......... */
L790:
/* Computing MAX */
	    d__3 = (d__1 = h[i + na * h_dim1], abs(d__1)), d__4 = (d__2 = h[i 
		    + en * h_dim1], abs(d__2));
	    t = max(d__3,d__4);
	    if (t == 0.) {
		goto L795;
	    }
	    tst1 = t;
	    tst2 = tst1 + 1. / tst1;
	    if (tst2 > tst1) {
		goto L795;
	    }
	    i__3 = en;
	    for (j = i; j <= i__3; ++j) {
		h[j + na * h_dim1] /= t;
		h[j + en * h_dim1] /= t;
/* L792: */
	    }

L795:
	    ;
	}
/*     .......... end complex vector .......... */
L800:
	;
    }
/*     .......... end back substitution. */
/*                vectors of isolated roots .......... */
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	if (i >= *low && i <= *igh) {
	    goto L840;
	}

	i__2 = *n;
	for (j = i; j <= i__2; ++j) {
/* L820: */
	    z[i + j * z_dim1] = h[i + j * h_dim1];
	}

L840:
	;
    }
/*     .......... multiply by transformation matrix to give */
/*                vectors of original full matrix. */
/*                for j=n step -1 until low do -- .......... */
    i__1 = *n;
    for (jj = *low; jj <= i__1; ++jj) {
	j = *n + *low - jj;
	m = min(j,*igh);

	i__2 = *igh;
	for (i = *low; i <= i__2; ++i) {
	    zz = 0.;

	    i__3 = m;
	    for (k = *low; k <= i__3; ++k) {
/* L860: */
		zz += z[i + k * z_dim1] * h[k + j * h_dim1];
	    }

	    z[i + j * z_dim1] = zz;
/* L880: */
	}
    }

    goto L1001;
/*     .......... set error -- all eigenvalues have not */
/*                converged after 30*n iterations .......... */
L1000:
    *ierr = en;
L1001:
    return 0;
}


static int htribk_(int *nm, int *n, double *ar, 
	double *ai, double *tau, int *m, double *zr, 
	double *zi)
{
    /* System generated locals */
    int ar_dim1, ar_offset, ai_dim1, ai_offset, zr_dim1, zr_offset, 
	    zi_dim1, zi_offset, i__1, i__2, i__3;

    /* Local variables */
    double h;
    int i, j, k, l;
    double s, si;



/*     this subroutine is a translation of a complex analogue of */
/*     the algol procedure trbak1, num. math. 11, 181-195(1968) */
/*     by martin, reinsch, and wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 212-226(1971). */

/*     this subroutine forms the eigenvectors of a complex hermitian */
/*     matrix by back transforming those of the corresponding */
/*     real symmetric tridiagonal matrix determined by  htridi. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrix. */

/*        ar and ai contain information about the unitary trans- */
/*          formations used in the reduction by  htridi  in their */
/*          full lower triangles except for the diagonal of ar. */

/*        tau contains further information about the transformations. */

/*        m is the number of eigenvectors to be back transformed. */

/*        zr contains the eigenvectors to be back transformed */
/*          in its first m columns. */

/*     on output */

/*        zr and zi contain the real and imaginary parts, */
/*          respectively, of the transformed eigenvectors */
/*          in their first m columns. */

/*     note that the last component of each returned vector */
/*     is real and that vector euclidean norms are preserved. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory 
*/

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    tau -= 3;
    ai_dim1 = *nm;
    ai_offset = ai_dim1 + 1;
    ai -= ai_offset;
    ar_dim1 = *nm;
    ar_offset = ar_dim1 + 1;
    ar -= ar_offset;
    zi_dim1 = *nm;
    zi_offset = zi_dim1 + 1;
    zi -= zi_offset;
    zr_dim1 = *nm;
    zr_offset = zr_dim1 + 1;
    zr -= zr_offset;

    /* Function Body */
    if (*m == 0) {
	goto L200;
    }
/*     .......... transform the eigenvectors of the real symmetric */
/*                tridiagonal matrix to those of the hermitian */
/*                tridiagonal matrix. .......... */
    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {

	i__2 = *m;
	for (j = 1; j <= i__2; ++j) {
	    zi[k + j * zi_dim1] = -zr[k + j * zr_dim1] * tau[(k << 1) + 2];
	    zr[k + j * zr_dim1] *= tau[(k << 1) + 1];
/* L50: */
	}
    }

    if (*n == 1) {
	goto L200;
    }
/*     .......... recover and apply the householder matrices .......... */
    i__2 = *n;
    for (i = 2; i <= i__2; ++i) {
	l = i - 1;
	h = ai[i + i * ai_dim1];
	if (h == 0.) {
	    goto L140;
	}

	i__1 = *m;
	for (j = 1; j <= i__1; ++j) {
	    s = 0.;
	    si = 0.;

	    i__3 = l;
	    for (k = 1; k <= i__3; ++k) {
		s = s + ar[i + k * ar_dim1] * zr[k + j * zr_dim1] - ai[i + k *
			 ai_dim1] * zi[k + j * zi_dim1];
		si = si + ar[i + k * ar_dim1] * zi[k + j * zi_dim1] + ai[i + 
			k * ai_dim1] * zr[k + j * zr_dim1];
/* L110: */
	    }
/*     .......... double divisions avoid possible underflow ......
.... */
	    s = s / h / h;
	    si = si / h / h;

	    i__3 = l;
	    for (k = 1; k <= i__3; ++k) {
		zr[k + j * zr_dim1] = zr[k + j * zr_dim1] - s * ar[i + k * 
			ar_dim1] - si * ai[i + k * ai_dim1];
		zi[k + j * zi_dim1] = zi[k + j * zi_dim1] - si * ar[i + k * 
			ar_dim1] + s * ai[i + k * ai_dim1];
/* L120: */
	    }

/* L130: */
	}

L140:
	;
    }

L200:
    return 0;
} /* htribk_ */

static int htridi_(int *nm, int *n, double *ar, 
	double *ai, double *d, double *e, double *e2, 
	double *tau)
{
    /* System generated locals */
    int ar_dim1, ar_offset, ai_dim1, ai_offset, i__1, i__2, i__3;
    double d__1, d__2;

    /* Builtin functions */
    double sqrt();

    /* Local variables */
    double f, g, h;
    int i, j, k, l;
    double scale, fi, gi, hh;
    int ii;
    double si;
    extern double pythag_();
    int jp1;



/*     this subroutine is a translation of a complex analogue of */
/*     the algol procedure tred1, num. math. 11, 181-195(1968) */
/*     by martin, reinsch, and wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 212-226(1971). */

/*     this subroutine reduces a complex hermitian matrix */
/*     to a real symmetric tridiagonal matrix using */
/*     unitary similarity transformations. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrix. */

/*        ar and ai contain the real and imaginary parts, */
/*          respectively, of the complex hermitian input matrix. */
/*          only the lower triangle of the matrix need be supplied. */

/*     on output */

/*        ar and ai contain information about the unitary trans- */
/*          formations used in the reduction in their full lower */
/*          triangles.  their strict upper triangles and the */
/*          diagonal of ar are unaltered. */

/*        d contains the diagonal elements of the the tridiagonal matrix. 
*/

/*        e contains the subdiagonal elements of the tridiagonal */
/*          matrix in its last n-1 positions.  e(1) is set to zero. */

/*        e2 contains the squares of the corresponding elements of e. */
/*          e2 may coincide with e if the squares are not needed. */

/*        tau contains further information about the transformations. */

/*     calls pythag for  dsqrt(a*a + b*b) . */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory 
*/

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    tau -= 3;
    --e2;
    --e;
    --d;
    ai_dim1 = *nm;
    ai_offset = ai_dim1 + 1;
    ai -= ai_offset;
    ar_dim1 = *nm;
    ar_offset = ar_dim1 + 1;
    ar -= ar_offset;

    /* Function Body */
    tau[(*n << 1) + 1] = 1.;
    tau[(*n << 1) + 2] = 0.;

    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
/* L100: */
	d[i] = ar[i + i * ar_dim1];
    }
/*     .......... for i=n step -1 until 1 do -- .......... */
    i__1 = *n;
    for (ii = 1; ii <= i__1; ++ii) {
	i = *n + 1 - ii;
	l = i - 1;
	h = 0.;
	scale = 0.;
	if (l < 1) {
	    goto L130;
	}
/*     .......... scale row (algol tol then not needed) .......... */
	i__2 = l;
	for (k = 1; k <= i__2; ++k) {
/* L120: */
	    scale = scale + (d__1 = ar[i + k * ar_dim1], abs(d__1)) + (d__2 = 
		    ai[i + k * ai_dim1], abs(d__2));
	}

	if (scale != 0.) {
	    goto L140;
	}
	tau[(l << 1) + 1] = 1.;
	tau[(l << 1) + 2] = 0.;
L130:
	e[i] = 0.;
	e2[i] = 0.;
	goto L290;

L140:
	i__2 = l;
	for (k = 1; k <= i__2; ++k) {
	    ar[i + k * ar_dim1] /= scale;
	    ai[i + k * ai_dim1] /= scale;
	    h = h + ar[i + k * ar_dim1] * ar[i + k * ar_dim1] + ai[i + k * 
		    ai_dim1] * ai[i + k * ai_dim1];
/* L150: */
	}

	e2[i] = scale * scale * h;
	g = sqrt(h);
	e[i] = scale * g;
	f = pythag_(&ar[i + l * ar_dim1], &ai[i + l * ai_dim1]);
/*     .......... form next diagonal element of matrix t .......... */
	if (f == 0.) {
	    goto L160;
	}
	tau[(l << 1) + 1] = (ai[i + l * ai_dim1] * tau[(i << 1) + 2] - ar[i + 
		l * ar_dim1] * tau[(i << 1) + 1]) / f;
	si = (ar[i + l * ar_dim1] * tau[(i << 1) + 2] + ai[i + l * ai_dim1] * 
		tau[(i << 1) + 1]) / f;
	h += f * g;
	g = g / f + 1.;
	ar[i + l * ar_dim1] = g * ar[i + l * ar_dim1];
	ai[i + l * ai_dim1] = g * ai[i + l * ai_dim1];
	if (l == 1) {
	    goto L270;
	}
	goto L170;
L160:
	tau[(l << 1) + 1] = -tau[(i << 1) + 1];
	si = tau[(i << 1) + 2];
	ar[i + l * ar_dim1] = g;
L170:
	f = 0.;

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    g = 0.;
	    gi = 0.;
/*     .......... form element of a*u .......... */
	    i__3 = j;
	    for (k = 1; k <= i__3; ++k) {
		g = g + ar[j + k * ar_dim1] * ar[i + k * ar_dim1] + ai[j + k *
			 ai_dim1] * ai[i + k * ai_dim1];
		gi = gi - ar[j + k * ar_dim1] * ai[i + k * ai_dim1] + ai[j + 
			k * ai_dim1] * ar[i + k * ar_dim1];
/* L180: */
	    }

	    jp1 = j + 1;
	    if (l < jp1) {
		goto L220;
	    }

	    i__3 = l;
	    for (k = jp1; k <= i__3; ++k) {
		g = g + ar[k + j * ar_dim1] * ar[i + k * ar_dim1] - ai[k + j *
			 ai_dim1] * ai[i + k * ai_dim1];
		gi = gi - ar[k + j * ar_dim1] * ai[i + k * ai_dim1] - ai[k + 
			j * ai_dim1] * ar[i + k * ar_dim1];
/* L200: */
	    }
/*     .......... form element of p .......... */
L220:
	    e[j] = g / h;
	    tau[(j << 1) + 2] = gi / h;
	    f = f + e[j] * ar[i + j * ar_dim1] - tau[(j << 1) + 2] * ai[i + j 
		    * ai_dim1];
/* L240: */
	}

	hh = f / (h + h);
/*     .......... form reduced a .......... */
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    f = ar[i + j * ar_dim1];
	    g = e[j] - hh * f;
	    e[j] = g;
	    fi = -ai[i + j * ai_dim1];
	    gi = tau[(j << 1) + 2] - hh * fi;
	    tau[(j << 1) + 2] = -gi;

	    i__3 = j;
	    for (k = 1; k <= i__3; ++k) {
		ar[j + k * ar_dim1] = ar[j + k * ar_dim1] - f * e[k] - g * ar[
			i + k * ar_dim1] + fi * tau[(k << 1) + 2] + gi * ai[i 
			+ k * ai_dim1];
		ai[j + k * ai_dim1] = ai[j + k * ai_dim1] - f * tau[(k << 1) 
			+ 2] - g * ai[i + k * ai_dim1] - fi * e[k] - gi * ar[
			i + k * ar_dim1];
/* L260: */
	    }
	}

L270:
	i__3 = l;
	for (k = 1; k <= i__3; ++k) {
	    ar[i + k * ar_dim1] = scale * ar[i + k * ar_dim1];
	    ai[i + k * ai_dim1] = scale * ai[i + k * ai_dim1];
/* L280: */
	}

	tau[(l << 1) + 2] = -si;
L290:
	hh = d[i];
	d[i] = ar[i + i * ar_dim1];
	ar[i + i * ar_dim1] = hh;
	ai[i + i * ai_dim1] = scale * sqrt(h);
/* L300: */
    }

    return 0;
} /* htridi_ */

double pythag_(double *a, double *b)
{
    /* System generated locals */
    double ret_val, d__1, d__2, d__3;

    /* Local variables */
    double p, r, s, t, u;


/*     finds dsqrt(a**2+b**2) without overflow or destructive underflow */

/* Computing MAX */
    d__1 = abs(*a), d__2 = abs(*b);
    p = max(d__1,d__2);
    if (p == 0.) {
	goto L20;
    }
/* Computing MIN */
    d__2 = abs(*a), d__3 = abs(*b);
/* Computing 2nd power */
    d__1 = min(d__2,d__3) / p;
    r = d__1 * d__1;
L10:
    t = r + 4.;
    if (t == 4.) {
	goto L20;
    }
    s = r / t;
    u = s * 2. + 1.;
    p = u * p;
/* Computing 2nd power */
    d__1 = s / u;
    r = d__1 * d__1 * r;
    goto L10;
L20:
    ret_val = p;
    return ret_val;
} /* pythag_ */

int F77_SYMBOL(rg)(int *nm, int *n, double *a, double *
	wr, double *wi, int *matz, double *z, int *iv1, 
	double *fv1, int *ierr)
{
    /* System generated locals */
    int a_dim1, a_offset, z_dim1, z_offset;

    /* Local variables */
    extern int balbak_(), balanc_(), elmhes_(), eltran_();
    int is1, is2;
    extern int hqr_(), hqr2_();



/*     this subroutine calls the recommended sequence of */
/*     subroutines from the eigensystem subroutine package (eispack) */
/*     to find the eigenvalues and eigenvectors (if desired) */
/*     of a real general matrix. */

/*     on input */

/*        nm  must be set to the row dimension of the two-dimensional */
/*        array parameters as declared in the calling program */
/*        dimension statement. */

/*        n  is the order of the matrix  a. */

/*        a  contains the real general matrix. */

/*        matz  is an int variable set equal to zero if */
/*        only eigenvalues are desired.  otherwise it is set to */
/*        any non-zero int for both eigenvalues and eigenvectors. */

/*     on output */

/*        wr  and  wi  contain the real and imaginary parts, */
/*        respectively, of the eigenvalues.  complex conjugate */
/*        pairs of eigenvalues appear consecutively with the */
/*        eigenvalue having the positive imaginary part first. */

/*        z  contains the real and imaginary parts of the eigenvectors */
/*        if matz is not zero.  if the j-th eigenvalue is real, the */
/*        j-th column of  z  contains its eigenvector.  if the j-th */
/*        eigenvalue is complex with positive imaginary part, the */
/*        j-th and (j+1)-th columns of  z  contain the real and */
/*        imaginary parts of its eigenvector.  the conjugate of this */
/*        vector is the eigenvector for the conjugate eigenvalue. */

/*        ierr  is an int output variable set equal to an error */
/*           completion code described in the documentation for hqr */
/*           and hqr2.  the normal completion code is zero. */

/*        iv1  and  fv1  are temporary storage arrays. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory 
*/

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    --fv1;
    --iv1;
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z -= z_offset;
    --wi;
    --wr;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    if (*n <= *nm) {
	goto L10;
    }
    *ierr = *n * 10;
    goto L50;

L10:
    balanc_(nm, n, &a[a_offset], &is1, &is2, &fv1[1]);
    elmhes_(nm, n, &is1, &is2, &a[a_offset], &iv1[1]);
    if (*matz != 0) {
	goto L20;
    }
/*     .......... find eigenvalues only .......... */
    hqr_(nm, n, &is1, &is2, &a[a_offset], &wr[1], &wi[1], ierr);
    goto L50;
/*     .......... find both eigenvalues and eigenvectors .......... */
L20:
    eltran_(nm, n, &is1, &is2, &a[a_offset], &iv1[1], &z[z_offset]);
    hqr2_(nm, n, &is1, &is2, &a[a_offset], &wr[1], &wi[1], &z[z_offset], ierr)
	    ;
    if (*ierr != 0) {
	goto L50;
    }
    balbak_(nm, n, &is1, &is2, &fv1[1], n, &z[z_offset]);
L50:
    return 0;
} /* rg_ */

int F77_SYMBOL(rs)(int *nm, int *n, double *a, double *
	w, int *matz, double *z, double *fv1, double *fv2, 
	int *ierr)
{
    /* System generated locals */
    int a_dim1, a_offset, z_dim1, z_offset;

    /* Local variables */
    extern int tred1_(), tred2_(), tql1_(), tql2_();



/*     this subroutine calls the recommended sequence of */
/*     subroutines from the eigensystem subroutine package (eispack) */
/*     to find the eigenvalues and eigenvectors (if desired) */
/*     of a real symmetric matrix. */

/*     on input */

/*        nm  must be set to the row dimension of the two-dimensional */
/*        array parameters as declared in the calling program */
/*        dimension statement. */

/*        n  is the order of the matrix  a. */

/*        a  contains the real symmetric matrix. */

/*        matz  is an int variable set equal to zero if */
/*        only eigenvalues are desired.  otherwise it is set to */
/*        any non-zero int for both eigenvalues and eigenvectors. */

/*     on output */

/*        w  contains the eigenvalues in ascending order. */

/*        z  contains the eigenvectors if matz is not zero. */

/*        ierr  is an int output variable set equal to an error */
/*           completion code described in the documentation for tqlrat */
/*           and tql2.  the normal completion code is zero. */

/*        fv1  and  fv2  are temporary storage arrays. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory 
*/

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    --fv2;
    --fv1;
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z -= z_offset;
    --w;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    if (*n <= *nm) {
	goto L10;
    }
    *ierr = *n * 10;
    goto L50;

L10:
    if (*matz != 0) {
	goto L20;
    }
/*     .......... find eigenvalues only .......... */
    tred1_(nm, n, &a[a_offset], &w[1], &fv1[1], &fv2[1]);
/*  tqlrat encounters catastrophic underflow on the Vax */
/*     call  tqlrat(n,w,fv2,ierr) */
    tql1_(n, &w[1], &fv1[1], ierr);
    goto L50;
/*     .......... find both eigenvalues and eigenvectors .......... */
L20:
    tred2_(nm, n, &a[a_offset], &w[1], &fv1[1], &z[z_offset]);
    tql2_(nm, n, &w[1], &fv1[1], &z[z_offset], ierr);
L50:
    return 0;
} /* rs_ */

static int tql1_(int *n, double *d, double *e, int *
	ierr)
{
    /* System generated locals */
    int i__1, i__2;
    double d__1, d__2;

    /* Builtin functions */
    double DSIGN();

    /* Local variables */
    double c, f, g, h;
    int i, j, l, m;
    double p, r, s, c2, c3;
    int l1, l2;
    double s2;
    int ii;
    extern double pythag_();
    double dl1, el1;
    int mml;
    double tst1, tst2;



/*     this subroutine is a translation of the algol procedure tql1, */
/*     num. math. 11, 293-306(1968) by bowdler, martin, reinsch, and */
/*     wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 227-240(1971). */

/*     this subroutine finds the eigenvalues of a symmetric */
/*     tridiagonal matrix by the ql method. */

/*     on input */

/*        n is the order of the matrix. */

/*        d contains the diagonal elements of the input matrix. */

/*        e contains the subdiagonal elements of the input matrix */
/*          in its last n-1 positions.  e(1) is arbitrary. */

/*      on output */

/*        d contains the eigenvalues in ascending order.  if an */
/*          error exit is made, the eigenvalues are correct and */
/*          ordered for indices 1,2,...ierr-1, but may not be */
/*          the smallest eigenvalues. */

/*        e has been destroyed. */

/*        ierr is set to */
/*          zero       for normal return, */
/*          j          if the j-th eigenvalue has not been */
/*                     determined after 30 iterations. */

/*     calls pythag for  dsqrt(a*a + b*b) . */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory 
*/

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    --e;
    --d;

    /* Function Body */
    *ierr = 0;
    if (*n == 1) {
	goto L1001;
    }

    i__1 = *n;
    for (i = 2; i <= i__1; ++i) {
/* L100: */
	e[i - 1] = e[i];
    }

    f = 0.;
    tst1 = 0.;
    e[*n] = 0.;

    i__1 = *n;
    for (l = 1; l <= i__1; ++l) {
	j = 0;
	h = (d__1 = d[l], abs(d__1)) + (d__2 = e[l], abs(d__2));
	if (tst1 < h) {
	    tst1 = h;
	}
/*     .......... look for small sub-diagonal element .......... */
	i__2 = *n;
	for (m = l; m <= i__2; ++m) {
	    tst2 = tst1 + (d__1 = e[m], abs(d__1));
	    if (tst2 == tst1) {
		goto L120;
	    }
/*     .......... e(n) is always zero, so there is no exit */
/*                through the bottom of the loop .......... */
/* L110: */
	}

L120:
	if (m == l) {
	    goto L210;
	}
L130:
	if (j == 30) {
	    goto L1000;
	}
	++j;
/*     .......... form shift .......... */
	l1 = l + 1;
	l2 = l1 + 1;
	g = d[l];
	p = (d[l1] - g) / (e[l] * 2.);
	r = pythag_(&p, &c_b314);
	d[l] = e[l] / (p + DSIGN(&r, &p));
	d[l1] = e[l] * (p + DSIGN(&r, &p));
	dl1 = d[l1];
	h = g - d[l];
	if (l2 > *n) {
	    goto L145;
	}

	i__2 = *n;
	for (i = l2; i <= i__2; ++i) {
/* L140: */
	    d[i] -= h;
	}

L145:
	f += h;
/*     .......... ql transformation .......... */
	p = d[m];
	c = 1.;
	c2 = c;
	el1 = e[l1];
	s = 0.;
	mml = m - l;
/*     .......... for i=m-1 step -1 until l do -- .......... */
	i__2 = mml;
	for (ii = 1; ii <= i__2; ++ii) {
	    c3 = c2;
	    c2 = c;
	    s2 = s;
	    i = m - ii;
	    g = c * e[i];
	    h = c * p;
	    r = pythag_(&p, &e[i]);
	    e[i + 1] = s * r;
	    s = e[i] / r;
	    c = p / r;
	    p = c * d[i] - s * g;
	    d[i + 1] = h + s * (c * g + s * d[i]);
/* L200: */
	}

	p = -s * s2 * c3 * el1 * e[l] / dl1;
	e[l] = s * p;
	d[l] = c * p;
	tst2 = tst1 + (d__1 = e[l], abs(d__1));
	if (tst2 > tst1) {
	    goto L130;
	}
L210:
	p = d[l] + f;
/*     .......... order eigenvalues .......... */
	if (l == 1) {
	    goto L250;
	}
/*     .......... for i=l step -1 until 2 do -- .......... */
	i__2 = l;
	for (ii = 2; ii <= i__2; ++ii) {
	    i = l + 2 - ii;
	    if (p >= d[i - 1]) {
		goto L270;
	    }
	    d[i] = d[i - 1];
/* L230: */
	}

L250:
	i = 1;
L270:
	d[i] = p;
/* L290: */
    }

    goto L1001;
/*     .......... set error -- no convergence to an */
/*                eigenvalue after 30 iterations .......... */
L1000:
    *ierr = l;
L1001:
    return 0;
} /* tql1_ */

static int tql2_(int *nm, int *n, double *d, double 
	*e, double *z, int *ierr)
{
    /* System generated locals */
    int z_dim1, z_offset, i__1, i__2, i__3;
    double d__1, d__2;

    /* Builtin functions */
    double DSIGN();

    /* Local variables */
    double c, f, g, h;
    int i, j, k, l, m;
    double p, r, s, c2, c3;
    int l1, l2;
    double s2;
    int ii;
    extern double pythag_();
    double dl1, el1;
    int mml;
    double tst1, tst2;



/*     this subroutine is a translation of the algol procedure tql2, */
/*     num. math. 11, 293-306(1968) by bowdler, martin, reinsch, and */
/*     wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 227-240(1971). */

/*     this subroutine finds the eigenvalues and eigenvectors */
/*     of a symmetric tridiagonal matrix by the ql method. */
/*     the eigenvectors of a full symmetric matrix can also */
/*     be found if  tred2  has been used to reduce this */
/*     full matrix to tridiagonal form. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrix. */

/*        d contains the diagonal elements of the input matrix. */

/*        e contains the subdiagonal elements of the input matrix */
/*          in its last n-1 positions.  e(1) is arbitrary. */

/*        z contains the transformation matrix produced in the */
/*          reduction by  tred2, if performed.  if the eigenvectors */
/*          of the tridiagonal matrix are desired, z must contain */
/*          the identity matrix. */

/*      on output */

/*        d contains the eigenvalues in ascending order.  if an */
/*          error exit is made, the eigenvalues are correct but */
/*          unordered for indices 1,2,...,ierr-1. */

/*        e has been destroyed. */

/*        z contains orthonormal eigenvectors of the symmetric */
/*          tridiagonal (or full) matrix.  if an error exit is made, */
/*          z contains the eigenvectors associated with the stored */
/*          eigenvalues. */

/*        ierr is set to */
/*          zero       for normal return, */
/*          j          if the j-th eigenvalue has not been */
/*                     determined after 30 iterations. */

/*     calls pythag for  dsqrt(a*a + b*b) . */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory 
*/

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z -= z_offset;
    --e;
    --d;

    /* Function Body */
    *ierr = 0;
    if (*n == 1) {
	goto L1001;
    }

    i__1 = *n;
    for (i = 2; i <= i__1; ++i) {
/* L100: */
	e[i - 1] = e[i];
    }

    f = 0.;
    tst1 = 0.;
    e[*n] = 0.;

    i__1 = *n;
    for (l = 1; l <= i__1; ++l) {
	j = 0;
	h = (d__1 = d[l], abs(d__1)) + (d__2 = e[l], abs(d__2));
	if (tst1 < h) {
	    tst1 = h;
	}
/*     .......... look for small sub-diagonal element .......... */
	i__2 = *n;
	for (m = l; m <= i__2; ++m) {
	    tst2 = tst1 + (d__1 = e[m], abs(d__1));
	    if (tst2 == tst1) {
		goto L120;
	    }
/*     .......... e(n) is always zero, so there is no exit */
/*                through the bottom of the loop .......... */
/* L110: */
	}

L120:
	if (m == l) {
	    goto L220;
	}
L130:
	if (j == 30) {
	    goto L1000;
	}
	++j;
/*     .......... form shift .......... */
	l1 = l + 1;
	l2 = l1 + 1;
	g = d[l];
	p = (d[l1] - g) / (e[l] * 2.);
	r = pythag_(&p, &c_b314);
	d[l] = e[l] / (p + DSIGN(&r, &p));
	d[l1] = e[l] * (p + DSIGN(&r, &p));
	dl1 = d[l1];
	h = g - d[l];
	if (l2 > *n) {
	    goto L145;
	}

	i__2 = *n;
	for (i = l2; i <= i__2; ++i) {
/* L140: */
	    d[i] -= h;
	}

L145:
	f += h;
/*     .......... ql transformation .......... */
	p = d[m];
	c = 1.;
	c2 = c;
	el1 = e[l1];
	s = 0.;
	mml = m - l;
/*     .......... for i=m-1 step -1 until l do -- .......... */
	i__2 = mml;
	for (ii = 1; ii <= i__2; ++ii) {
	    c3 = c2;
	    c2 = c;
	    s2 = s;
	    i = m - ii;
	    g = c * e[i];
	    h = c * p;
	    r = pythag_(&p, &e[i]);
	    e[i + 1] = s * r;
	    s = e[i] / r;
	    c = p / r;
	    p = c * d[i] - s * g;
	    d[i + 1] = h + s * (c * g + s * d[i]);
/*     .......... form vector .......... */
	    i__3 = *n;
	    for (k = 1; k <= i__3; ++k) {
		h = z[k + (i + 1) * z_dim1];
		z[k + (i + 1) * z_dim1] = s * z[k + i * z_dim1] + c * h;
		z[k + i * z_dim1] = c * z[k + i * z_dim1] - s * h;
/* L180: */
	    }

/* L200: */
	}

	p = -s * s2 * c3 * el1 * e[l] / dl1;
	e[l] = s * p;
	d[l] = c * p;
	tst2 = tst1 + (d__1 = e[l], abs(d__1));
	if (tst2 > tst1) {
	    goto L130;
	}
L220:
	d[l] += f;
/* L240: */
    }
/*     .......... order eigenvalues and eigenvectors .......... */
    i__1 = *n;
    for (ii = 2; ii <= i__1; ++ii) {
	i = ii - 1;
	k = i;
	p = d[i];

	i__2 = *n;
	for (j = ii; j <= i__2; ++j) {
	    if (d[j] >= p) {
		goto L260;
	    }
	    k = j;
	    p = d[j];
L260:
	    ;
	}

	if (k == i) {
	    goto L300;
	}
	d[k] = d[i];
	d[i] = p;

	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
	    p = z[j + i * z_dim1];
	    z[j + i * z_dim1] = z[j + k * z_dim1];
	    z[j + k * z_dim1] = p;
/* L280: */
	}

L300:
	;
    }

    goto L1001;
/*     .......... set error -- no convergence to an */
/*                eigenvalue after 30 iterations .......... */
L1000:
    *ierr = l;
L1001:
    return 0;
} /* tql2_ */

/* *** for old version, "send otqlrat from eispack" */
/* * From dana!moler Tue, 1 Sep 87 10:15:40 PDT */
/* * New TQLRAT */
static int tqlrat_(int *n, double *d, double *e2, 
	int *ierr)
{
    /* System generated locals */
    int i__1, i__2;
    double d__1, d__2;

    /* Builtin functions */
    double sqrt(), DSIGN();

    /* Local variables */
    double b, c, f, g, h;
    int i, j, l, m;
    double p, r, s, t;
    int l1, ii;
    extern double pythag_(), epslon_();
    int mml;



/*     This subroutine is a translation of the Algol procedure tqlrat, */
/*     Algorithm 464, Comm. ACM 16, 689(1973) by Reinsch. */

/*     This subroutine finds the eigenvalues of a symmetric */
/*     tridiagonal matrix by the rational QL method. */

/*     On input */

/*        N is the order of the matrix. */

/*        D contains the diagonal elements of the input matrix. */

/*        E2 contains the squares of the subdiagonal elements of the */
/*          input matrix in its last N-1 positions.  E2(1) is arbitrary. 
*/

/*      On output */

/*        D contains the eigenvalues in ascending order.  If an */
/*          error exit is made, the eigenvalues are correct and */
/*          ordered for indices 1,2,...IERR-1, but may not be */
/*          the smallest eigenvalues. */

/*        E2 has been destroyed. */

/*        IERR is set to */
/*          zero       for normal return, */
/*          J          if the J-th eigenvalue has not been */
/*                     determined after 30 iterations. */

/*     Calls PYTHAG for  DSQRT(A*A + B*B) . */

/*     Questions and comments should be directed to Burton S. Garbow, */
/*     Mathematics and Computer Science Div, Argonne National Laboratory 
*/

/*     This version dated August 1987. */
/*     Modified by C. Moler to fix underflow/overflow difficulties, */
/*     especially on the VAX and other machines where epslon(1.0d0)**2 */
/*     nearly underflows.  See the loop involving statement 102 and */
/*     the two statements just before statement 200. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    --e2;
    --d;

    /* Function Body */
    *ierr = 0;
    if (*n == 1) {
	goto L1001;
    }

    i__1 = *n;
    for (i = 2; i <= i__1; ++i) {
/* L100: */
	e2[i - 1] = e2[i];
    }

    f = 0.;
    t = 0.;
    e2[*n] = 0.;

    i__1 = *n;
    for (l = 1; l <= i__1; ++l) {
	j = 0;
	h = (d__1 = d[l], abs(d__1)) + sqrt(e2[l]);
	if (t > h) {
	    goto L105;
	}
	t = h;
	b = epslon_(&t);
	c = b * b;
	if (c != 0.) {
	    goto L105;
	}
/*        Spliting tolerance underflowed.  Look for larger value. */
	i__2 = *n;
	for (i = l; i <= i__2; ++i) {
	    h = (d__1 = d[i], abs(d__1)) + sqrt(e2[i]);
	    if (h > t) {
		t = h;
	    }
/* L102: */
	}
	b = epslon_(&t);
	c = b * b;
/*     .......... LOOK FOR SMALL SQUARED SUB-DIAGONAL ELEMENT ........
.. */
L105:
	i__2 = *n;
	for (m = l; m <= i__2; ++m) {
	    if (e2[m] <= c) {
		goto L120;
	    }
/*     .......... E2(N) IS ALWAYS ZERO, SO THERE IS NO EXIT */
/*                THROUGH THE BOTTOM OF THE LOOP .......... */
/* L110: */
	}

L120:
	if (m == l) {
	    goto L210;
	}
L130:
	if (j == 30) {
	    goto L1000;
	}
	++j;
/*     .......... FORM SHIFT .......... */
	l1 = l + 1;
	s = sqrt(e2[l]);
	g = d[l];
	p = (d[l1] - g) / (s * 2.);
	r = pythag_(&p, &c_b314);
	d[l] = s / (p + DSIGN(&r, &p));
	h = g - d[l];

	i__2 = *n;
	for (i = l1; i <= i__2; ++i) {
/* L140: */
	    d[i] -= h;
	}

	f += h;
/*     .......... RATIONAL QL TRANSFORMATION .......... */
	g = d[m];
	if (g == 0.) {
	    g = b;
	}
	h = g;
	s = 0.;
	mml = m - l;
/*     .......... FOR I=M-1 STEP -1 UNTIL L DO -- .......... */
	i__2 = mml;
	for (ii = 1; ii <= i__2; ++ii) {
	    i = m - ii;
	    p = g * h;
	    r = p + e2[i];
	    e2[i + 1] = s * r;
	    s = e2[i] / r;
	    d[i + 1] = h + s * (h + d[i]);
	    g = d[i] - e2[i] / g;
/*           Avoid division by zero on next pass */
	    if (g == 0.) {
		g = epslon_(&d[i]);
	    }
	    h = g * (p / r);
/* L200: */
	}

	e2[l] = s * g;
	d[l] = h;
/*     .......... GUARD AGAINST UNDERFLOW IN CONVERGENCE TEST ........
.. */
	if (h == 0.) {
	    goto L210;
	}
	if ((d__1 = e2[l], abs(d__1)) <= (d__2 = c / h, abs(d__2))) {
	    goto L210;
	}
	e2[l] = h * e2[l];
	if (e2[l] != 0.) {
	    goto L130;
	}
L210:
	p = d[l] + f;
/*     .......... ORDER EIGENVALUES .......... */
	if (l == 1) {
	    goto L250;
	}
/*     .......... FOR I=L STEP -1 UNTIL 2 DO -- .......... */
	i__2 = l;
	for (ii = 2; ii <= i__2; ++ii) {
	    i = l + 2 - ii;
	    if (p >= d[i - 1]) {
		goto L270;
	    }
	    d[i] = d[i - 1];
/* L230: */
	}

L250:
	i = 1;
L270:
	d[i] = p;
/* L290: */
    }

    goto L1001;
/*     .......... SET ERROR -- NO CONVERGENCE TO AN */
/*                EIGENVALUE AFTER 30 ITERATIONS .......... */
L1000:
    *ierr = l;
L1001:
    return 0;
} /* tqlrat_ */

static int tred1_(int *nm, int *n, double *a, 
	double *d, double *e, double *e2)
{
    /* System generated locals */
    int a_dim1, a_offset, i__1, i__2, i__3;
    double d__1;

    /* Builtin functions */
    double sqrt(), DSIGN();

    /* Local variables */
    double f, g, h;
    int i, j, k, l;
    double scale;
    int ii, jp1;



/*     this subroutine is a translation of the algol procedure tred1, */
/*     num. math. 11, 181-195(1968) by martin, reinsch, and wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 212-226(1971). */

/*     this subroutine reduces a real symmetric matrix */
/*     to a symmetric tridiagonal matrix using */
/*     orthogonal similarity transformations. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrix. */

/*        a contains the real symmetric input matrix.  only the */
/*          lower triangle of the matrix need be supplied. */

/*     on output */

/*        a contains information about the orthogonal trans- */
/*          formations used in the reduction in its strict lower */
/*          triangle.  the full upper triangle of a is unaltered. */

/*        d contains the diagonal elements of the tridiagonal matrix. */

/*        e contains the subdiagonal elements of the tridiagonal */
/*          matrix in its last n-1 positions.  e(1) is set to zero. */

/*        e2 contains the squares of the corresponding elements of e. */
/*          e2 may coincide with e if the squares are not needed. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory 
*/

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    --e2;
    --e;
    --d;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	d[i] = a[*n + i * a_dim1];
	a[*n + i * a_dim1] = a[i + i * a_dim1];
/* L100: */
    }
/*     .......... for i=n step -1 until 1 do -- .......... */
    i__1 = *n;
    for (ii = 1; ii <= i__1; ++ii) {
	i = *n + 1 - ii;
	l = i - 1;
	h = 0.;
	scale = 0.;
	if (l < 1) {
	    goto L130;
	}
/*     .......... scale row (algol tol then not needed) .......... */
	i__2 = l;
	for (k = 1; k <= i__2; ++k) {
/* L120: */
	    scale += (d__1 = d[k], abs(d__1));
	}

	if (scale != 0.) {
	    goto L140;
	}

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    d[j] = a[l + j * a_dim1];
	    a[l + j * a_dim1] = a[i + j * a_dim1];
	    a[i + j * a_dim1] = 0.;
/* L125: */
	}

L130:
	e[i] = 0.;
	e2[i] = 0.;
	goto L300;

L140:
	i__2 = l;
	for (k = 1; k <= i__2; ++k) {
	    d[k] /= scale;
	    h += d[k] * d[k];
/* L150: */
	}

	e2[i] = scale * scale * h;
	f = d[l];
	d__1 = sqrt(h);
	g = -DSIGN(&d__1, &f);
	e[i] = scale * g;
	h -= f * g;
	d[l] = f - g;
	if (l == 1) {
	    goto L285;
	}
/*     .......... form a*u .......... */
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
/* L170: */
	    e[j] = 0.;
	}

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    f = d[j];
	    g = e[j] + a[j + j * a_dim1] * f;
	    jp1 = j + 1;
	    if (l < jp1) {
		goto L220;
	    }

	    i__3 = l;
	    for (k = jp1; k <= i__3; ++k) {
		g += a[k + j * a_dim1] * d[k];
		e[k] += a[k + j * a_dim1] * f;
/* L200: */
	    }

L220:
	    e[j] = g;
/* L240: */
	}
/*     .......... form p .......... */
	f = 0.;

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    e[j] /= h;
	    f += e[j] * d[j];
/* L245: */
	}

	h = f / (h + h);
/*     .......... form q .......... */
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
/* L250: */
	    e[j] -= h * d[j];
	}
/*     .......... form reduced a .......... */
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    f = d[j];
	    g = e[j];

	    i__3 = l;
	    for (k = j; k <= i__3; ++k) {
/* L260: */
		a[k + j * a_dim1] = a[k + j * a_dim1] - f * e[k] - g * d[k];
	    }

/* L280: */
	}

L285:
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    f = d[j];
	    d[j] = a[l + j * a_dim1];
	    a[l + j * a_dim1] = a[i + j * a_dim1];
	    a[i + j * a_dim1] = f * scale;
/* L290: */
	}

L300:
	;
    }

    return 0;
} /* tred1_ */

static int tred2_(int *nm, int *n, double *a, 
	double *d, double *e, double *z)
{
    /* System generated locals */
    int a_dim1, a_offset, z_dim1, z_offset, i__1, i__2, i__3;
    double d__1;

    /* Builtin functions */
    double sqrt(), DSIGN();

    /* Local variables */
    double f, g, h;
    int i, j, k, l;
    double scale, hh;
    int ii, jp1;



/*     this subroutine is a translation of the algol procedure tred2, */
/*     num. math. 11, 181-195(1968) by martin, reinsch, and wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 212-226(1971). */

/*     this subroutine reduces a real symmetric matrix to a */
/*     symmetric tridiagonal matrix using and accumulating */
/*     orthogonal similarity transformations. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrix. */

/*        a contains the real symmetric input matrix.  only the */
/*          lower triangle of the matrix need be supplied. */

/*     on output */

/*        d contains the diagonal elements of the tridiagonal matrix. */

/*        e contains the subdiagonal elements of the tridiagonal */
/*          matrix in its last n-1 positions.  e(1) is set to zero. */

/*        z contains the orthogonal transformation matrix */
/*          produced in the reduction. */

/*        a and z may coincide.  if distinct, a is unaltered. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory 
*/

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z -= z_offset;
    --e;
    --d;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {

	i__2 = *n;
	for (j = i; j <= i__2; ++j) {
/* L80: */
	    z[j + i * z_dim1] = a[j + i * a_dim1];
	}

	d[i] = a[*n + i * a_dim1];
/* L100: */
    }

    if (*n == 1) {
	goto L510;
    }
/*     .......... for i=n step -1 until 2 do -- .......... */
    i__1 = *n;
    for (ii = 2; ii <= i__1; ++ii) {
	i = *n + 2 - ii;
	l = i - 1;
	h = 0.;
	scale = 0.;
	if (l < 2) {
	    goto L130;
	}
/*     .......... scale row (algol tol then not needed) .......... */
	i__2 = l;
	for (k = 1; k <= i__2; ++k) {
/* L120: */
	    scale += (d__1 = d[k], abs(d__1));
	}

	if (scale != 0.) {
	    goto L140;
	}
L130:
	e[i] = d[l];

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    d[j] = z[l + j * z_dim1];
	    z[i + j * z_dim1] = 0.;
	    z[j + i * z_dim1] = 0.;
/* L135: */
	}

	goto L290;

L140:
	i__2 = l;
	for (k = 1; k <= i__2; ++k) {
	    d[k] /= scale;
	    h += d[k] * d[k];
/* L150: */
	}

	f = d[l];
	d__1 = sqrt(h);
	g = -DSIGN(&d__1, &f);
	e[i] = scale * g;
	h -= f * g;
	d[l] = f - g;
/*     .......... form a*u .......... */
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
/* L170: */
	    e[j] = 0.;
	}

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    f = d[j];
	    z[j + i * z_dim1] = f;
	    g = e[j] + z[j + j * z_dim1] * f;
	    jp1 = j + 1;
	    if (l < jp1) {
		goto L220;
	    }

	    i__3 = l;
	    for (k = jp1; k <= i__3; ++k) {
		g += z[k + j * z_dim1] * d[k];
		e[k] += z[k + j * z_dim1] * f;
/* L200: */
	    }

L220:
	    e[j] = g;
/* L240: */
	}
/*     .......... form p .......... */
	f = 0.;

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    e[j] /= h;
	    f += e[j] * d[j];
/* L245: */
	}

	hh = f / (h + h);
/*     .......... form q .......... */
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
/* L250: */
	    e[j] -= hh * d[j];
	}
/*     .......... form reduced a .......... */
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    f = d[j];
	    g = e[j];

	    i__3 = l;
	    for (k = j; k <= i__3; ++k) {
/* L260: */
		z[k + j * z_dim1] = z[k + j * z_dim1] - f * e[k] - g * d[k];
	    }

	    d[j] = z[l + j * z_dim1];
	    z[i + j * z_dim1] = 0.;
/* L280: */
	}

L290:
	d[i] = h;
/* L300: */
    }
/*     .......... accumulation of transformation matrices .......... */
    i__1 = *n;
    for (i = 2; i <= i__1; ++i) {
	l = i - 1;
	z[*n + l * z_dim1] = z[l + l * z_dim1];
	z[l + l * z_dim1] = 1.;
	h = d[i];
	if (h == 0.) {
	    goto L380;
	}

	i__2 = l;
	for (k = 1; k <= i__2; ++k) {
/* L330: */
	    d[k] = z[k + i * z_dim1] / h;
	}

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    g = 0.;

	    i__3 = l;
	    for (k = 1; k <= i__3; ++k) {
/* L340: */
		g += z[k + i * z_dim1] * z[k + j * z_dim1];
	    }

	    i__3 = l;
	    for (k = 1; k <= i__3; ++k) {
		z[k + j * z_dim1] -= g * d[k];
/* L360: */
	    }
	}

L380:
	i__3 = l;
	for (k = 1; k <= i__3; ++k) {
/* L400: */
	    z[k + i * z_dim1] = 0.;
	}

/* L500: */
    }

L510:
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	d[i] = z[*n + i * z_dim1];
	z[*n + i * z_dim1] = 0.;
/* L520: */
    }

    z[*n + *n * z_dim1] = 1.;
    e[1] = 0.;
    return 0;
} /* tred2_ */

