/* -----------------------------------------------------------------------
 *
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (c) 1996  Robert Gentleman and Ross Ihaka
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  The Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to The Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * -----------------------------------------------------------------------
 */

#include "Fortran.h"

extern int F77_SYMBOL(fdhess) (int *, double *, double *, int (*) (), double *, int *, double *, double *, int *, double *);
extern int F77_SYMBOL(optif0) (int *, int *, double *, int (*) (), double *, double *, double *, int *, double *, double *);
extern int F77_SYMBOL(optif9) (int *, int *, double *, int (*) (), int (*) (), int (*) (), double *, double *, int *, int *, int *, int *, int *, int *, int *, int *, double *, double *, double *, double *, double *, double *, double *, int *, double *, double *);

static int bakslv_(int *, int *, double *, double *, double *);
static int chlhsn_(int *, int *, double *, double *, double *, double *);
static int choldc_(int *, int *, double *, double *, double *, double *);
static int d1fcn_(int *, double *, double *);
static int d2fcn_(int *, int *, double *, double *);
static int dfault_(int *, double *, double *, double *, int *, int *, int *, int *, int *, int *, int *, int *, double *, double *, double *, double *);
static int dogdrv_(int *, int *, double *, double *, double *, double *, double *, double *, double *, int (*) (), double *, double *, double *, double *, int *, int *, double *, double *, double *, double *, int *);
static int dogstp_(int *, int *, double *, double *, double *, double *, double *, double *, int *, int *, double *, double *, double *, double *, double *, int *, double *);
static int forslv_(int *, int *, double *, double *, double *);
static int fstocd_(int *, double *, int (*) (), double *, double *, double *);
static int fstofd_(int *, int *, int *, double *, int (*) (), double *, double *, double *, double *, double *, int *);
static int grdchk_(int *, double *, int (*) (), double *, double *, double *, double *, double *, double *, double *, double *, int *, int *);
static int heschk_(int *, int *, double *, int (*) (), int (*) (), int (*) (), double *, double *, double *, double *, double *, double *, double *, int *, double *, double *, double *, int *, int *);
static int hookdr_(int *, int *, double *, double *, double *, double *, double *, double *, double *, double *, int (*) (), double *, double *, double *, double *, int *, int *, double *, double *, double *, double *, double *, double *, double *, double *, int *, int *);
static int hookst_(int *, int *, double *, double *, double *, double *, double *, double *, double *, double *, double *, double *, double *, int *, double *, int *, double *, double *, int *);
static int hsnint_(int *, int *, double *, double *, int *);
static int lltslv_(int *, int *, double *, double *, double *);
static int lnsrch_(int *, double *, double *, double *, double *, double *, double *, int (*) (), int *, int *, double *, double *, double *, int *);
static int mvmltl_(int *, int *, double *, double *, double *);
static int mvmlts_(int *, int *, double *, double *, double *);
static int mvmltu_(int *, int *, double *, double *, double *);
static int optchk_(int *, double *, double *, double *, double *, double *, int *, int *, double *, double *, int *, int *, int *, int *, double *, int *, int *);
static int optdrv_(int *, int *, double *, int (*) (), int (*) (), int (*) (), double *, double *, int *, int *, int *, int *, int *, int *, int *, int *, double *, double *, double *, double *, double *, double *, double *, int *, double *, double *, double *, double *, double *, double *, double *, double *, double *);
static int optstp_(int *, double *, double *, double *, double *, int *, int *, int *, double *, double *, double *, double *, int *, int *, int *, int *, int *);
static int qraux1_(int *, int *, double *, int *);
static int qraux2_(int *, int *, double *, int *, double *, double *);
static int qrupdt_(int *, int *, double *, double *, double *);
static int sclmul_(int *, double *, double *, double *);
static int secfac_(int *, int *, double *, double *, double *, double *, double *, double *, int *, double *, int *, int *, double *, double *, double *, double *);
static int secunf_(int *, int *, double *, double *, double *, double *, double *, double *, double *, int *, double *, int *, int *, double *, double *, double *);
static int sndofd_(int *, int *, double *, int (*) (), double *, double *, double *, double *, double *, double *);
static int tregup_(int *, int *, double *, double *, double *, double *, int (*) (), double *, double *, int *, double *, double *, double *, int *, double *, double *, double *, double *, int *, int *, int *, double *);


static double c_b2 = .33333333333333331;
static double c_b3 = 10.;
static double c_b33 = 0.;
static int c__4 = 4;
static int c__2 = 2;
static int c__1 = 1;
static int c__3 = 3;
static double c_b146 = 1.;
static int c__0 = 0;

/*
 *   subroutine fdhess
 *
 *   this subroutine calculates a numerical approximation to the upper
 *   triangular portion of the second derivative matrix (the hessian).
 *   algorithm a5.6.2 from dennis and schnabel (1983), numerical methods
 *   for unconstrained optimization and nonlinear equations,
 *   prentice-hall, 321-322.
 *
 *   programmed by richard h. jones, january 11, 1989
 *
 *   input to subroutine
 *
 *       n.....the number of parameters
 *       x.....vector of parameter values
 *       fval..double precision value of function at x
 *      fun...a function provided by the user which must be declared as
 *            external in the calling program.  its call must be of the
 *            call fun(n,x,fval) where fval is the computed value of the
 *             function
 *       nfd...first dimension of h in the calling program
 *
 *   output from subroutine
 *
 *        h.....an n by n matrix of the approximate hessian
 *
 *    work space
 *
 *        step....a float array of length n
 *        f.......a double precision array of length n
 */

int 
F77_SYMBOL(fdhess) (int *n, double *x, double *fval,
		int (*fun) (), double *h, int *nfd, double *step, double *f,
		    int *ndigit, double *typx)
{
	double d__1, d__2;
	double tempi, tempj, fii, eta, fij;
	int h_dim1, h_offset, i__1, i__2;
	int i, j;

	--typx;
	--f;
	--step;
	--x;
	h_dim1 = *nfd;
	h_offset = h_dim1 + 1;
	h -= h_offset;

	i__1 = -(*ndigit);
	d__1 = POW_DI(&c_b3, &i__1);
	eta = POW_DD(&d__1, &c_b2);
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		d__1 = x[i], d__2 = typx[i];
		step[i] = eta * max(d__1, d__2);
		if (typx[i] < 0.) {
			step[i] = -step[i];
		}
		tempi = x[i];
		x[i] += step[i];
		step[i] = x[i] - tempi;
		(*fun) (n, &x[1], &f[i]);
		x[i] = tempi;
	}
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		tempi = x[i];
		x[i] += step[i] * 2.;
		(*fun) (n, &x[1], &fii);
		h[i + i * h_dim1] = (*fval - f[i] + (fii - f[i])) / (step[i] * step[i]
			);
		x[i] = tempi + step[i];
		if (i < *n) {
			i__2 = *n;
			for (j = i + 1; j <= i__2; ++j) {
				tempj = x[j];
				x[j] += step[j];
				(*fun) (n, &x[1], &fij);
				h[i + j * h_dim1] = (*fval - f[i] + (fij - f[j])) / (step[i] *
								   step[j]);
				x[j] = tempj;
			}
		}
		x[i] = tempi;
	}
	return 0;
}


/*
 *    subroutine bakslv
 *
 *    purpose
 *
 *    solve  ax=b  where a is upper triangular matrix.
 *    note that a is input as a lower triangular matrix and
 *    that this routine takes its transpose implicitly.
 *
 *    parameters
 *
 *    nr           --> row dimension of matrix
 *    n            --> dimension of problem
 *    a(n,n)       --> lower triangular matrix (preserved)
 *    x(n)        <--  solution vector
 *    b(n)         --> right-hand side vector
 *
 *    note
 *
 *    if b is no longer required by calling routine,
 *    then vectors b and x may share the same storage.
 */

static
int 
bakslv_(int *nr, int *n, double *a,
	double *x, double *b)
{
	double sum;
	int a_dim1, a_offset, i__1;
	int i, j, ip1;

	/* solve (l-transpose)x=b. (back solve) */

	a_dim1 = *nr;
	a_offset = a_dim1 + 1;
	a -= a_offset;
	--b;
	--x;

	i = *n;
	x[i] = b[i] / a[i + i * a_dim1];
	if (*n == 1) {
		return 0;
	}
L30:
	ip1 = i;
	--i;
	sum = 0.;
	i__1 = *n;
	for (j = ip1; j <= i__1; ++j) {
		sum += a[j + i * a_dim1] * x[j];
	}
	x[i] = (b[i] - sum) / a[i + i * a_dim1];
	if (i > 1) {
		goto L30;
	}
	return 0;
}


/*
 *    subroutine chlhsn
 *
 *    purpose
 *
 *   find the l(l-transpose) [written ll+] decomposition of the perturbed
 *   model hessian matrix a+mu*i(where mu\0 and i is the identity matrix)
 *   which is safely positive definite.  if a is safely positive definite
 *    upon entry, then mu=0.
 *
 *    parameters
 *
 *    nr           --> row dimension of matrix
 *    n            --> dimension of problem
 *    a(n,n)      <--> on entry; "a" is model hessian (only lower
 *                     triangular part and diagonal stored)
 *                     on exit:  a contains l of ll+ decomposition of
 *                     perturbed model hessian in lower triangular
 *                     part and diagonal and contains hessian in upper
 *                     triangular part and udiag
 *    epsm         --> machine epsilon
 *    sx(n)        --> diagonal scaling matrix for x
 *    udiag(n)    <--  on exit: contains diagonal of hessian
 *
 *    internal variables
 *
 *    tol              tolerance
 *    diagmn           minimum element on diagonal of a
 *    diagmx           maximum element on diagonal of a
 *    offmax           maximum off-diagonal element of a
 *    offrow           sum of off-diagonal elements in a row of a
 *    evmin            minimum eigenvalue of a
 *    evmax            maximum eigenvalue of a
 *
 *    description
 *
 *    1. if "a" has any negative diagonal elements, then choose mu>0
 *    such that the diagonal of a:=a+mu*i is all positive
 *    with the ratio of its smallest to largest element on the
 *    order of sqrt(epsm).
 *
 *    2. "a" undergoes a perturbed cholesky decomposition which
 *    results in an ll+ decomposition of a+d, where d is a
 *    non-negative diagonal matrix which is implicitly added to
 *    "a" during the decomposition if "a" is not positive definite.
 *    "a" is retained and not changed during this process by
 *    copying l into the upper triangular part of "a" and the
 *    diagonal into udiag.  then the cholesky decomposition routine
 *    is called.  on return, addmax contains maximum element of d.
 *
 *    3. if addmax=0, "a" was positive definite going into step 2
 *    and return is made to calling program.  otherwise,
 *    the minimum number sdd which must be added to the
 *    diagonal of a to make it safely strictly diagonally dominant
 *    is calculated.  since a+addmax*i and a+sdd*i are safely
 *    positive definite, choose mu=min(addmax,sdd) and decompose
 *    a+mu*i to obtain l.
 */

static
int 
chlhsn_(int *nr, int *n, double *a,
	double *epsm, double *sx, double *udiag)
{
	int a_dim1, a_offset, i__1, i__2;
	double d__1, d__2;

	double sqrt();

	int i, j;
	double evmin, evmax;
	extern int choldc_();
	double addmax, diagmn, diagmx, offmax, offrow, posmax;
	int im1, jm1, ip1, jp1;
	double sdd, amu, tol;


	/* scale hessian */
	/* pre- and post- multiply "a" by inv(sx) */

	a_dim1 = *nr;
	a_offset = a_dim1 + 1;
	a -= a_offset;
	--udiag;
	--sx;

	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
		i__2 = *n;
		for (i = j; i <= i__2; ++i) {
			a[i + j * a_dim1] /= sx[i] * sx[j];
		}
	}

	/* step1 */
	/* ----- */
	/* note:  if a different tolerance is desired throughout this */
	/* algorithm, change tolerance here: */

	tol = sqrt(*epsm);

	diagmx = a[a_dim1 + 1];
	diagmn = a[a_dim1 + 1];
	if (*n == 1) {
		goto L35;
	}
	i__1 = *n;
	for (i = 2; i <= i__1; ++i) {
		if (a[i + i * a_dim1] < diagmn) {
			diagmn = a[i + i * a_dim1];
		}
		if (a[i + i * a_dim1] > diagmx) {
			diagmx = a[i + i * a_dim1];
		}
	}
L35:
	posmax = max(diagmx, 0.);

	/* diagmn .le. 0 */

	if (diagmn > posmax * tol) {
		goto L100;
	}
/*     if(diagmn.le.posmax*tol) */
/*     then */
	amu = tol * (posmax - diagmn) - diagmn;
	if (amu != 0.) {
		goto L60;
	}
/*       if(amu.eq.0.0d0) */
/*       then */

	/* find largest off-diagonal element of a */

	offmax = 0.;
	if (*n == 1) {
		goto L50;
	}
	i__1 = *n;
	for (i = 2; i <= i__1; ++i) {
		im1 = i - 1;
		i__2 = im1;
		for (j = 1; j <= i__2; ++j) {
			if ((d__1 = a[i + j * a_dim1], abs(d__1)) > offmax) {
				offmax = (d__2 = a[i + j * a_dim1], abs(d__2));
			}
		}
	}
L50:
	amu = offmax;
	if (amu != 0.) {
		goto L55;
	}
/*         if(amu.eq.0.0d0) */
/*         then */
	amu = 1.;
	goto L60;
/*         else */
L55:
	amu *= tol + 1.;
/*         endif */
/*       endif */

/*       a=a + mu*i */

L60:
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		a[i + i * a_dim1] += amu;
	}
	diagmx += amu;
/*     endif */

/*       step2 */

	/* copy lower triangular part of "a" to upper triangular part */
	/* and diagonal of "a" to udiag */

L100:
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
		udiag[j] = a[j + j * a_dim1];
		if (j == *n) {
			goto L110;
		}
		jp1 = j + 1;
		i__2 = *n;
		for (i = jp1; i <= i__2; ++i) {
			a[j + i * a_dim1] = a[i + j * a_dim1];
		}
L110:
		;
	}

	choldc_(nr, n, &a[a_offset], &diagmx, &tol, &addmax);


	/* step3 */

	/* if addmax=0, "a" was positive definite going into step 2, */
	/* the ll+ decomposition has been done, and we return. */
	/* otherwise, addmax>0.  perturb "a" so that it is safely */
	/* diagonally dominant and find ll+ decomposition */

	if (addmax <= 0.) {
		goto L170;
	}
/*     if(addmax.gt.0.0d0) */
/*     then */

	/* restore original "a" (lower triangular part and diagonal) */

	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
		a[j + j * a_dim1] = udiag[j];
		if (j == *n) {
			goto L120;
		}
		jp1 = j + 1;
		i__2 = *n;
		for (i = jp1; i <= i__2; ++i) {
			a[i + j * a_dim1] = a[j + i * a_dim1];
		}
L120:
		;
	}

	/* find sdd such that a+sdd*i is safely positive definite */
	/* note:  evmin<0 since a is not positive definite; */

	evmin = 0.;
	evmax = a[a_dim1 + 1];
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		offrow = 0.;
		if (i == 1) {
			goto L135;
		}
		im1 = i - 1;
		i__2 = im1;
		for (j = 1; j <= i__2; ++j) {
			offrow += (d__1 = a[i + j * a_dim1], abs(d__1));
		}
L135:
		if (i == *n) {
			goto L145;
		}
		ip1 = i + 1;
		i__2 = *n;
		for (j = ip1; j <= i__2; ++j) {
			offrow += (d__1 = a[j + i * a_dim1], abs(d__1));
		}
L145:
		d__1 = evmin, d__2 = a[i + i * a_dim1] - offrow;
		evmin = min(d__1, d__2);
		d__1 = evmax, d__2 = a[i + i * a_dim1] + offrow;
		evmax = max(d__1, d__2);
	}
	sdd = tol * (evmax - evmin) - evmin;

	/* perturb "a" and decompose again */

	amu = min(sdd, addmax);
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		a[i + i * a_dim1] += amu;
		udiag[i] = a[i + i * a_dim1];
	}

	/*  "a" now guaranteed safely positive definite */

	choldc_(nr, n, &a[a_offset], &c_b33, &tol, &addmax);
/*     endif */

	/* unscale hessian and cholesky decomposition matrix */

L170:
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
		i__2 = *n;
		for (i = j; i <= i__2; ++i) {
			a[i + j * a_dim1] = sx[i] * a[i + j * a_dim1];
		}
		if (j == 1) {
			goto L185;
		}
		jm1 = j - 1;
		i__2 = jm1;
		for (i = 1; i <= i__2; ++i) {
			a[i + j * a_dim1] = sx[i] * sx[j] * a[i + j * a_dim1];
		}
L185:
		udiag[j] = udiag[j] * sx[j] * sx[j];
	}
	return 0;
}


/*
 *    subroutine choldc
 *
 *    purpose
 *
 *    find the perturbed l(l-transpose) [written ll+] decomposition
 *    of a+d, where d is a non-negative diagonal matrix added to a if
 *    necessary to allow the cholesky decomposition to continue.
 *
 *    parameters
 *
 *    nr           --> row dimension of matrix
 *    n            --> dimension of problem
 *    a(n,n)      <--> on entry: matrix for which to find perturbed
 *                          cholesky decomposition
 *                     on exit:  contains l of ll+ decomposition
 *                     in lower triangular part and diagonal of "a"
 *    diagmx       --> maximum diagonal element of "a"
 *    tol          --> tolerance
 *   addmax      <--  maximum amount implicitly added to diagonal of "a"
 *                     in forming the cholesky decomposition of a+d
 *    internal variables
 *
 *    aminl    smallest element allowed on diagonal of l
 *    amnlsq   =aminl**2
 *    offmax   maximum off-diagonal element in column of a
 *
 *
 *    description
 *
 *   the normal cholesky decomposition is performed.  however, if at any
 *    point the algorithm would attempt to set l(i,i)=sqrt(temp)
 *    with temp < tol*diagmx, then l(i,i) is set to sqrt(tol*diagmx)
 *    instead.  this is equivalent to adding tol*diagmx-temp to a(i,i)
 */

static
int 
choldc_(int *nr, int *n, double *a,
	double *diagmx, double *tol, double *addmax)
{
	double aminl, offmax, amnlsq;
	double d__1, d__2;
	double sqrt();
	double sum;
	double temp;
	int a_dim1, a_offset, i__1, i__2, i__3;
	int i, j, k;
	int jm1, jp1;

	a_dim1 = *nr;
	a_offset = a_dim1 + 1;
	a -= a_offset;

	*addmax = 0.;
	aminl = sqrt(*diagmx * *tol);
	amnlsq = aminl * aminl;

	/* form column j of l */

	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {

	/* find diagonal elements of l */

		sum = 0.;
		if (j == 1) {
			goto L20;
		}
		jm1 = j - 1;
		i__2 = jm1;
		for (k = 1; k <= i__2; ++k) {
			sum += a[j + k * a_dim1] * a[j + k * a_dim1];
		}
L20:
		temp = a[j + j * a_dim1] - sum;
		if (temp < amnlsq) {
			goto L30;
		}
/*       if(temp.ge.aminl**2) */
/*       then */
		a[j + j * a_dim1] = sqrt(temp);
		goto L40;
/*       else */

		/* find maximum off-diagonal element in column */

L30:
		offmax = 0.;
		if (j == *n) {
			goto L37;
		}
		jp1 = j + 1;
		i__2 = *n;
		for (i = jp1; i <= i__2; ++i) {
			if ((d__1 = a[i + j * a_dim1], abs(d__1)) > offmax) {
				offmax = (d__2 = a[i + j * a_dim1], abs(d__2));
			}
		}
L37:
		if (offmax <= amnlsq) {
			offmax = amnlsq;
		}
	/* add to diagonal element to allow cholesky decomposition to continue*/

		a[j + j * a_dim1] = sqrt(offmax);
		d__1 = *addmax, d__2 = offmax - temp;
		*addmax = max(d__1, d__2);
/*       endif */

	/* find i,j element of lower triangular matrix */
L40:
		if (j == *n) {
			goto L100;
		}
		jp1 = j + 1;
		i__2 = *n;
		for (i = jp1; i <= i__2; ++i) {
			sum = 0.;
			if (j == 1) {
				goto L60;
			}
			jm1 = j - 1;
			i__3 = jm1;
			for (k = 1; k <= i__3; ++k) {
				sum += a[i + k * a_dim1] * a[j + k * a_dim1];
			}
	L60:
			a[i + j * a_dim1] = (a[i + j * a_dim1] - sum) / a[j + j * a_dim1];
		}
L100:
		;
	}
	return 0;
}


/*
 *    subroutine d1fcn
 *
 *    purpose
 *
 *    dummy routine to prevent unsatisfied external diagnostic
 *    when specific analytic gradient function not supplied.
 */

static
int 
d1fcn_(int *n, double *x, double *g)
{
	--g;
	--x;

	g[*n] = g[*n];
	x[*n] = x[*n];
	return 0;
}


/*
 *    subroutine d2fcn
 *
 *    purpose
 *
 *    dummy routine to prevent unsatisfied external diagnostic
 *    when specific analytic hessian function not supplied.
 */

static
int 
d2fcn_(int *nr, int *n, double *x,
       double *h)
{
	int h_dim1, h_offset;

	h_dim1 = *nr;
	h_offset = h_dim1 + 1;
	h -= h_offset;
	--x;

	h[*nr + h_dim1] = h[*nr + h_dim1];
	x[*n] = x[*n];
	return 0;
}


/*
 *    subroutine dfault
 *
 *    purpose
 *
 *    set default values for each input variable to
 *    minimization algorithm.
 *
 *    parameters
 *
 *    n            --> dimension of problem
 *    x(n)         --> initial guess to solution (to compute max step size)
 *
 *    typsiz(n)   <--  typical size for each component of x
 *    fscale      <--  estimate of scale of minimization function
 *    method      <--  algorithm to use to solve minimization problem
 *   iexp        <--  =0 if minimization function not expensive to evaluate
 *
 *    msg         <--  message to inhibit certain automatic checks + output
 *
 *    ndigit      <--  number of good digits in minimization function
 *    itnlim      <--  maximum number of allowable iterations
 *    iagflg      <--  =0 if analytic gradient not supplied
 *    iahflg      <--  =0 if analytic hessian not supplied
 *    ipr         <--  device to which to send output
 *    dlt         <--  trust region radius
 *    gradtl      <--  tolerance at which gradient considered close enough
 *                     to zero to terminate algorithm
 *    stepmx      <--  value of zero to trip default maximum in optchk
 *    steptl      <--  tolerance at which successive iterates considered
 *                     close enough to terminate algorithm
 */

static
int 
dfault_(int *n, double *x, double *typsiz,
	double *fscale, int *method, int *iexp, int *msg,
	int *ndigit, int *itnlim, int *iagflg, int *iahflg,
	int *ipr, double *dlt, double *gradtl, double *stepmx,
	double *steptl)
{
	double epsm;
	extern double F77_SYMBOL(d1mach) ();
	extern int F77_SYMBOL(i1mach) ();
	int i;
	int i__1;

	--typsiz;
	--x;

	x[*n] = x[*n];

	/* set typical size of x and minimization function */

	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		typsiz[i] = 1.;
	}
	*fscale = 1.;

	/* set tolerances */

	*dlt = -1.;
	epsm = F77_SYMBOL(d1mach) (&c__4);
	*gradtl = POW_DD(&epsm, &c_b2);
	*stepmx = 0.;
	*steptl = sqrt(epsm);

	/* set flags */

	*method = 1;
	*iexp = 1;
	*msg = 0;
	*ndigit = -1;
	*itnlim = 150;
	*iagflg = 0;
	*iahflg = 0;
	*ipr = F77_SYMBOL(i1mach) (&c__2);

	return 0;
}


/*
 *    subroutine dogdrv
 *
 *    purpose
 *
 *    find a next newton iterate (xpls) by the double dogleg method
 *
 *    parameters
 *
 *    nr           --> row dimension of matrix
 *    n            --> dimension of problem
 *    x(n)         --> old iterate x[k-1]
 *    f            --> function value at old iterate, f(x)
 *    g(n)         --> gradient  at old iterate, g(x), or approximate
 *    a(n,n)       --> cholesky decomposition of hessian
 *                     in lower triangular part and diagonal
 *    p(n)         --> newton step
 *    xpls(n)     <--  new iterate x[k]
 *    fpls        <--  function value at new iterate, f(xpls)
 *    fcn          --> name of subroutine to evaluate function
 *    sx(n)        --> diagonal scaling matrix for x
 *    stepmx       --> maximum allowable step size
 *    steptl       --> relative step size at which successive iterates
 *                     considered close enough to terminate algorithm
 *    dlt         <--> trust region radius
 *                     [retain value between successive calls]
 *    iretcd      <--  return code
 *                       =0 satisfactory xpls found
 *                      =1 failed to find satisfactory xpls sufficiently
 *                          distinct from x
 *    mxtake      <--  boolean flag indicating step of maximum length used
 *    sc(n)        --> workspace [current step]
 *    wrk1(n)      --> workspace (and place holding argument to tregup)
 *    wrk2(n)      --> workspace
 *    wrk3(n)      --> workspace
 *    ipr          --> device to which to send output
 */

static
int 
dogdrv_(int *nr, int *n, double *x,
	double *f, double *g, double *a, double *p,
	double *xpls, double *fpls, int (*fcn) (),
	double *sx, double *stepmx, double *steptl, double *
	dlt, int *iretcd, int *mxtake, double *sc, double *
	wrk1, double *wrk2, double *wrk3, int *ipr)
{
	int a_dim1, a_offset, i__1;

	double sqrt();

	int i;
	double fplsp;
	int fstdog, nwtake;
	extern int dogstp_(), tregup_();
	double rnwtln, eta, cln, tmp;


	a_dim1 = *nr;
	a_offset = a_dim1 + 1;
	a -= a_offset;
	--wrk3;
	--wrk2;
	--wrk1;
	--sc;
	--sx;
	--xpls;
	--p;
	--g;
	--x;

	*iretcd = 4;
	fstdog = TRUE;
	tmp = 0.;
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		tmp += sx[i] * sx[i] * p[i] * p[i];
	}
	rnwtln = sqrt(tmp);
/* $    write(ipr,954) rnwtln */

L100:

	/* find new step by double dogleg algorithm */

	dogstp_(nr, n, &g[1], &a[a_offset], &p[1], &sx[1], &rnwtln, dlt, &nwtake,
	      &fstdog, &wrk1[1], &wrk2[1], &cln, &eta, &sc[1], ipr, stepmx);

	/* check new point and update trust region */

	tregup_(nr, n, &x[1], f, &g[1], &a[a_offset], fcn, &sc[1], &sx[1], &
	    nwtake, stepmx, steptl, dlt, iretcd, &wrk3[1], &fplsp, &xpls[1],
		fpls, mxtake, ipr, &c__2, &wrk1[1]);
	if (*iretcd <= 1) {
		return 0;
	}
	goto L100;
/* %950 format(42h dogdrv    initial trust region not given., */
/* %   +       22h  compute cauchy step.) */
/* %951 format(18h dogdrv    alpha =,e20.13/ */
/* %   +       18h dogdrv    beta  =,e20.13/ */
/* %   +       18h dogdrv    dlt   =,e20.13/ */
/* %   +       18h dogdrv    nwtake=,l1    ) */
/* %952 format(28h dogdrv    current step (sc)) */
/* %954 format(18h0dogdrv    rnwtln=,e20.13) */
/* %955 format(14h dogdrv       ,5(e20.13,3x)) */
}


/*
 *    subroutine dogstp
 *
 *    purpose
 *
 *    find new step by double dogleg algorithm
 *
 *    parameters
 *
 *    nr           --> row dimension of matrix
 *    n            --> dimension of problem
 *    g(n)         --> gradient at current iterate, g(x)
 *    a(n,n)       --> cholesky decomposition of hessian in
 *                     lower part and diagonal
 *    p(n)         --> newton step
 *    sx(n)        --> diagonal scaling matrix for x
 *    rnwtln       --> newton step length
 *    dlt         <--> trust region radius
 *    nwtake      <--> boolean, =.true. if newton step taken
 *    fstdog      <--> boolean, =.true. if on first leg of dogleg
 *    ssd(n)      <--> workspace [cauchy step to the minimum of the
 *                     quadratic model in the scaled steepest descent
 *                    direction] [retain value between successive calls]
 *   v(n)        <--> workspace  [retain value between successive calls]
 *    cln         <--> cauchy length
 *                     [retain value between successive calls]
 *    eta              [retain value between successive calls]
 *    sc(n)       <--  current step
 *    ipr          --> device to which to send output
 *    stepmx       --> maximum allowable step size
 *
 *    internal variables
 *
 *    cln              length of cauchy step
 */

static
int 
dogstp_(int *nr, int *n, double *g,
	double *a, double *p, double *sx, double *rnwtln,
	double *dlt, int *nwtake, int *fstdog, double *ssd,
	double *v, double *cln, double *eta, double *sc,
	int *ipr, double *stepmx)
{
	int a_dim1, a_offset, i__1, i__2;

	double sqrt();

	double alam, beta;
	extern double F77_SYMBOL(ddot) ();
	int i, j;
	double alpha, tmp, dot1, dot2;

	a_dim1 = *nr;
	a_offset = a_dim1 + 1;
	a -= a_offset;
	--sc;
	--v;
	--ssd;
	--sx;
	--p;
	--g;

	*ipr = *ipr;

	/* can we take newton step */

	if (*rnwtln > *dlt) {
		goto L100;
	}
/*     if(rnwtln.le.dlt) */
/*     then */
	*nwtake = TRUE;
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		sc[i] = p[i];
	}
	*dlt = *rnwtln;
/* $      write(ipr,951) */
	goto L700;
/*     else */

	/* newton step too long */
	/* cauchy step is on double dogleg curve */

L100:
	*nwtake = FALSE;
	if (!(*fstdog)) {
		goto L200;
	}
/*       if(fstdog) */
/*       then */

	/* calculate double dogleg curve (ssd) */

	*fstdog = FALSE;
	alpha = 0.;
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		alpha += g[i] * g[i] / (sx[i] * sx[i]);
	}
	beta = 0.;
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		tmp = 0.;
		i__2 = *n;
		for (j = i; j <= i__2; ++j) {
			tmp += a[j + i * a_dim1] * g[j] / (sx[j] * sx[j]);
		}
		beta += tmp * tmp;
	}
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		ssd[i] = -(alpha / beta) * g[i] / sx[i];
	}
	*cln = alpha * sqrt(alpha) / beta;
	*eta = alpha * .8 * alpha / (-beta * F77_SYMBOL(ddot) (n, &g[1], &c__1, &p[1], &c__1)
		) + .2;
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		v[i] = *eta * sx[i] * p[i] - ssd[i];
	}
	if (*dlt == -1.) {
		*dlt = min(*cln, *stepmx);
	}
/* $        write(ipr,954) alpha,beta,cln,eta */
/* $        write(ipr,955) */
/* $        write(ipr,960) (ssd(i),i=1,n) */
/* $        write(ipr,956) */
/* $        write(ipr,960) (v(i),i=1,n) */
/*       endif */

L200:
	if (*eta * *rnwtln > *dlt) {
		goto L220;
	}
/*       if(eta*rnwtln .le. dlt) */
/*       then */

	/* take partial step in newton direction */

	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		sc[i] = *dlt / *rnwtln * p[i];
	}
/* $        write(ipr,957) */
	goto L700;
/*       else */
L220:
	if (*cln < *dlt) {
		goto L240;
	}
/*         if(cln.ge.dlt) */
/*         then */

	/* take step in steepest descent direction */

	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		sc[i] = *dlt / *cln * ssd[i] / sx[i];
	}
/* $          write(ipr,958) */
	goto L700;
/*         else */

	/* calculate convex combination of ssd and eta*p */
	/* which has scaled length dlt */

L240:
	dot1 = F77_SYMBOL(ddot) (n, &v[1], &c__1, &ssd[1], &c__1);
	dot2 = F77_SYMBOL(ddot) (n, &v[1], &c__1, &v[1], &c__1);
	alam = (-dot1 + sqrt(dot1 * dot1 - dot2 * (*cln * *cln - *dlt * *dlt))) /
		dot2;
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		sc[i] = (ssd[i] + alam * v[i]) / sx[i];
	}
/* $          write(ipr,959) */
/*         endif */
/*       endif */
/*     endif */
L700:
/* $    write(ipr,952) fstdog,nwtake,rnwtln,dlt */
/* $    write(ipr,953) */
/* $    write(ipr,960) (sc(i),i=1,n) */
	return 0;

/* %951 format(27h0dogstp    take newton step) */
/* %952 format(18h dogstp    fstdog=,l1/ */
/* %   +       18h dogstp    nwtake=,l1/ */
/* %   +       18h dogstp    rnwtln=,e20.13/ */
/* %   +       18h dogstp    dlt   =,e20.13) */
/* %953 format(28h dogstp    current step (sc)) */
/* %954 format(18h dogstp    alpha =,e20.13/ */
/* %   +       18h dogstp    beta  =,e20.13/ */
/* %   +       18h dogstp    cln   =,e20.13/ */
/* %   +       18h dogstp    eta   =,e20.13) */
/* %955 format(28h dogstp    cauchy step (ssd)) */
/* %956 format(12h dogstp    v) */
/* %957 format(48h0dogstp    take partial step in newton direction) */
/* %958 format(50h0dogstp    take step in steepest descent direction) */
/* %959 format(39h0dogstp    take convex combination step) */
/* %960 format(14h dogstp       ,5(e20.13,3x)) */
}

/*
 *    subroutine forslv
 *
 *    purpose
 *
 *    solve  ax=b  where a is lower triangular matrix
 *
 *    parameters
 *
 *    nr           --> row dimension of matrix
 *    n            --> dimension of problem
 *    a(n,n)       --> lower triangular matrix (preserved)
 *    x(n)        <--  solution vector
 *    b(n)         --> right-hand side vector
 *
 *    note
 *
 *    if b is no longer required by calling routine,
 *    then vectors b and x may share the same storage.
 */

static
int 
forslv_(int *nr, int *n, double *a,
	double *x, double *b)
{
	int a_dim1, a_offset, i__1, i__2;

	int i, j, im1;
	double sum;


	/* solve lx=b. (foreward solve) */

	a_dim1 = *nr;
	a_offset = a_dim1 + 1;
	a -= a_offset;
	--b;
	--x;

	x[1] = b[1] / a[a_dim1 + 1];
	if (*n == 1) {
		return 0;
	}
	i__1 = *n;
	for (i = 2; i <= i__1; ++i) {
		sum = 0.;
		im1 = i - 1;
		i__2 = im1;
		for (j = 1; j <= i__2; ++j) {
			sum += a[i + j * a_dim1] * x[j];
		}
		x[i] = (b[i] - sum) / a[i + i * a_dim1];
	}
	return 0;
}


/*
 *    subroutine fstocd
 *
 *    purpose
 *
 *    find central difference approximation g to the first derivative
 *    (gradient) of the function defined by fcn at the point x.
 *
 *    parameters
 *
 *    n            --> dimension of problem
 *    x            --> point at which gradient is to be approximated.
 *    fcn          --> name of subroutine to evaluate function.
 *    sx           --> diagonal scaling matrix for x.
 *    rnoise       --> relative noise in fcn [f(x)].
 *    g           <--  central difference approximation to gradient.
 */


static
int 
fstocd_(int *n, double *x, int (
			    *fcn) (), double *sx, double *rnoise, double *g)
{
	int i__1;
	double d__1, d__2, d__3;
	int i;
	double third, stepi, fplus, fminus, xtempi;


	/* find i th  stepsize, evaluate two neighbors in direction of i th */
	/* unit vector, and evaluate i th  component of gradient. */

	--g;
	--sx;
	--x;

	third = .33333333333333331;
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		d__2 = (d__1 = x[i], abs(d__1)), d__3 = 1. / sx[i];
		stepi = POW_DD(rnoise, &third) * max(d__2, d__3);
		xtempi = x[i];
		x[i] = xtempi + stepi;
		(*fcn) (n, &x[1], &fplus);
		x[i] = xtempi - stepi;
		(*fcn) (n, &x[1], &fminus);
		x[i] = xtempi;
		g[i] = (fplus - fminus) / (stepi * 2.);
	}
	return 0;
}


/*
 *    subroutine fstofd
 *
 *    purpose
 *
 *   find first order forward finite difference approximation "a" to the
 *   first derivative of the function defined by the subprogram "fname"
 *    evaluated at the new iterate "xpls".
 *
 *    for optimization use this routine to estimate:
 *   1) the first derivative (gradient) of the optimization function "fcn
 *       analytic user routine has been supplied;
 *    2) the second derivative (hessian) of the optimization function
 *      if no analytic user routine has been supplied for the hessian but
 *       one has been supplied for the gradient ("fcn") and if the
 *       optimization function is inexpensive to evaluate
 *
 *    note
 *
 *   _m=1 (optimization) algorithm estimates the gradient of the function
 *         (fcn).   fcn(x) # f: r(n)-->r(1)
 *   _m=n (systems) algorithm estimates the jacobian of the function
 *         fcn(x) # f: r(n)-->r(n).
 *   _m=n (optimization) algorithm estimates the hessian of the optimization
 *         function, where the hessian is the first derivative of "fcn"
 *
 *    parameters
 *
 *    nr           --> row dimension of matrix
 *    m            --> number of rows in a
 *    n            --> number of columns in a; dimension of problem
 *    xpls(n)      --> new iterate:  x[k]
 *    fcn          --> name of subroutine to evaluate function
 *   fpls(m)      --> _m=1 (optimization) function value at new iterate:
 *                          fcn(xpls)
 *                     _m=n (optimization) value of first derivative
 *                          (gradient) given by user function fcn
 *                     _m=n (systems)  function value of associated
 *                          minimization function
 *   a(nr,n)     <--  finite difference approximation (see note).  only
 *                    lower triangular matrix and diagonal are returned
 *    sx(n)        --> diagonal scaling matrix for x
 *    rnoise       --> relative noise in fcn [f(x)]
 *    fhat(m)      --> workspace
 *    icase        --> =1 optimization (gradient)
 *                     =2 systems
 *                     =3 optimization (hessian)
 *
 *    internal variables
 *
 *    stepsz - stepsize in the j-th variable direction
 */

static
int 
fstofd_(int *nr, int *m, int *n, double *
	xpls, int (*fcn) (), double *fpls, double *a,
	double *sx, double *rnoise, double *fhat, int *icase)
{
	double d__1, d__2, d__3;
	double sqrt();
	double stepsz;
	double xtmpj;
	int a_dim1, a_offset, i__1, i__2;
	int i, j;
	int jp1, nm1;

	/* find j-th column of a */
	/* each column is derivative of f(fcn) with respect to xpls(j) */

	a_dim1 = *nr;
	a_offset = a_dim1 + 1;
	a -= a_offset;
	--fhat;
	--fpls;
	--sx;
	--xpls;

	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
		d__2 = (d__1 = xpls[j], abs(d__1)), d__3 = 1. / sx[j];
		stepsz = sqrt(*rnoise) * max(d__2, d__3);
		xtmpj = xpls[j];
		xpls[j] = xtmpj + stepsz;
		(*fcn) (n, &xpls[1], &fhat[1]);
		xpls[j] = xtmpj;
		i__2 = *m;
		for (i = 1; i <= i__2; ++i) {
			a[i + j * a_dim1] = (fhat[i] - fpls[i]) / stepsz;
		}
	}
	if (*icase != 3) {
		return 0;
	}

	/* if computing hessian, a must be symmetric */

	if (*n == 1) {
		return 0;
	}
	nm1 = *n - 1;
	i__1 = nm1;
	for (j = 1; j <= i__1; ++j) {
		jp1 = j + 1;
		i__2 = *m;
		for (i = jp1; i <= i__2; ++i) {
			a[i + j * a_dim1] = (a[i + j * a_dim1] + a[j + i * a_dim1]) / 2.;
		}
	}
	return 0;
}


/*
 *    subroutine grdchk
 *
 *    purpose
 *
 *    check analytic gradient against estimated gradient
 *
 *    parameters
 *
 *    n            --> dimension of problem
 *    x(n)         --> estimate to a root of fcn
 *    fcn          --> name of subroutine to evaluate optimization function
 *
 *                     must be declared external in calling routine
 *                          fcn:  r(n) --> r(1)
 *    f            --> function value:  fcn(x)
 *    g(n)         --> gradient:  g(x)
 *    typsiz(n)    --> typical size for each component of x
 *    sx(n)        --> diagonal scaling matrix:  sx(i)=1./typsiz(i)
 *    fscale       --> estimate of scale of objective function fcn
 *    rnf          --> relative noise in optimization function fcn
 *    analtl       --> tolerance for comparison of estimated and
 *                     analytical gradients
 *    wrk1(n)      --> workspace
 *    msg         <--  message or error code
 *                      on output: =-21, probable coding error of gradient
 *
 *    ipr          --> device to which to send output
 */

static
int 
grdchk_(int *n, double *x, int (
			     *fcn) (), double *f, double *g, double *typsiz,
	double *sx, double *fscale, double *rnf, double *
	analtl, double *wrk1, int *msg, int *ipr)
{
	int i__1;
	double d__1, d__2, d__3, d__4;

	int i;
	double gs;
	extern int fstofd_();
	int ker;
	double wrk;

	/* compute first order finite difference gradient and compare to */
	/* analytic gradient. */

	--wrk1;
	--sx;
	--typsiz;
	--g;
	--x;

	fstofd_(&c__1, &c__1, n, &x[1], fcn, f, &wrk1[1], &sx[1], rnf, &wrk, &
		c__1);
	ker = 0;
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		d__2 = abs(*f);
		d__3 = (d__1 = x[i], abs(d__1)), d__4 = typsiz[i];
		gs = max(d__2, *fscale) / max(d__3, d__4);
		d__3 = (d__2 = g[i], abs(d__2));
		if ((d__1 = g[i] - wrk1[i], abs(d__1)) > max(d__3, gs) * *analtl) {
			ker = 1;
		}
	}
	if (ker == 0) {
		goto L20;
	}
/* %      write(ipr,901) */
/* %      write(ipr,902) (i,g(i),wrk1(i),i=1,n) */
	*msg = -21;
L20:
	return 0;
/* %901 format(47h0grdchk    probable error in coding of analytic, */
/* %   +       19h gradient function./ */
/* %   +       16h grdchk     comp,12x,8hanalytic,12x,8hestimate) */
/* %902 format(11h grdchk    ,i5,3x,e20.13,3x,e20.13) */
}


/*
 *    subroutine heschk
 *
 *    purpose
 *
 *    check analytic hessian against estimated hessian
 *     (this may be done only if the user supplied analytic hessian
 *      d2fcn fills only the lower triangular part and diagonal of a)
 *
 *    parameters
 *
 *    nr           --> row dimension of matrix
 *    n            --> dimension of problem
 *    x(n)         --> estimate to a root of fcn
 *    fcn          --> name of subroutine to evaluate optimization function
 *
 *                     must be declared external in calling routine
 *                          fcn:  r(n) --> r(1)
 *    d1fcn        --> name of subroutine to evaluate gradient of fcn.
 *                     must be declared external in calling routine
 *    d2fcn        --> name of subroutine to evaluate hessian of fcn.
 *                     must be declared external in calling routine
 *    f            --> function value:  fcn(x)
 *    g(n)        <--  gradient:  g(x)
 *    a(n,n)      <--  on exit:  hessian in lower triangular part and diag
 *    typsiz(n)    --> typical size for each component of x
 *    sx(n)        --> diagonal scaling matrix:  sx(i)=1./typsiz(i)
 *    rnf          --> relative noise in optimization function fcn
 *    analtl       --> tolerance for comparison of estimated and
 *                     analytical gradients
 *    iagflg       --> =1 if analytic gradient supplied
 *    udiag(n)     --> workspace
 *    wrk1(n)      --> workspace
 *    wrk2(n)      --> workspace
 *    msg         <--> message or error code
 *                      on input : if =1xx do not compare anal + est hess
 *                      on output: =-22, probable coding error of hessian
 *    ipr          --> device to which to send output
 */

static
int 
heschk_(int *nr, int *n, double *x, /* Subroutine */ int (*fcn) (), int (*d1fcn) (), /* Subroutine */ int (*d2fcn) (), double *f, double *g,
	double *a, double *typsiz, double *sx, double *rnf,
	double *analtl, int *iagflg, double *udiag, double *
	wrk1, double *wrk2, int *msg, int *ipr)
{
	double d__1, d__2, d__3, d__4, d__5;
	double hs;
	extern int sndofd_(), fstofd_();
	int a_dim1, a_offset, i__1, i__2;
	int i, j;
	int jp1, ker;


	/* compute finite difference approximation a to the hessian. */

	a_dim1 = *nr;
	a_offset = a_dim1 + 1;
	a -= a_offset;
	--wrk2;
	--wrk1;
	--udiag;
	--sx;
	--typsiz;
	--g;
	--x;

	if (*iagflg == 1) {
		fstofd_(nr, n, n, &x[1], d1fcn, &g[1], &a[a_offset], &sx[1], rnf, &
			wrk1[1], &c__3);
	}
	if (*iagflg != 1) {
		sndofd_(nr, n, &x[1], fcn, f, &a[a_offset], &sx[1], rnf, &wrk1[1], &
			wrk2[1]);
	}
	ker = 0;

	/* copy lower triangular part of "a" to upper triangular part */
	/* and diagonal of "a" to udiag */

	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
		udiag[j] = a[j + j * a_dim1];
		if (j == *n) {
			goto L30;
		}
		jp1 = j + 1;
		i__2 = *n;
		for (i = jp1; i <= i__2; ++i) {
			a[j + i * a_dim1] = a[i + j * a_dim1];
		}
L30:
		;
	}

	/* compute analytic hessian and compare to finite difference */
	/* approximation. */

	(*d2fcn) (nr, n, &x[1], &a[a_offset]);
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
		d__3 = (d__1 = g[j], abs(d__1));
		d__4 = (d__2 = x[j], abs(d__2)), d__5 = typsiz[j];
		hs = max(d__3, 1.) / max(d__4, d__5);
		d__3 = (d__2 = udiag[j], abs(d__2));
		if ((d__1 = a[j + j * a_dim1] - udiag[j], abs(d__1)) > max(d__3, hs) *
		    *analtl) {
			ker = 1;
		}
		if (j == *n) {
			goto L40;
		}
		jp1 = j + 1;
		i__2 = *n;
		for (i = jp1; i <= i__2; ++i) {
			d__3 = (d__2 = a[i + j * a_dim1], abs(d__2));
			if ((d__1 = a[i + j * a_dim1] - a[j + i * a_dim1], abs(d__1)) >
			    max(d__3, hs) * *analtl) {
				ker = 1;
			}
		}
L40:
		;
	}

	if (ker == 0) {
		goto L90;
	}
/* %      write(ipr,901) */
/* %      do 50 i=1,n */
/* %        if(i.eq.1) go to 45 */
/* %        im1=i-1 */
/* %        do 43 j=1,im1 */
/* %          write(ipr,902) i,j,a(i,j),a(j,i) */
/* % 43     continue */
/* % 45     write(ipr,902) i,i,a(i,i),udiag(i) */
/* % 50   continue */
	*msg = -22;
/*     endif */
L90:
	return 0;
/* %901 format(47h heschk    probable error in coding of analytic, */
/* %   +       18h hessian function./ */
/* %   +       21h heschk      row  col,14x,8hanalytic,14x,10h(estimate)) */
/* %902 format(11h heschk    ,2i5,2x,e20.13,2x,1h(,e20.13,1h)) */
}


/*
 *    subroutine hookdr
 *
 *    purpose
 *
 *    find a next newton iterate (xpls) by the more-hebdon method
 *
 *    parameters
 *
 *    nr           --> row dimension of matrix
 *    n            --> dimension of problem
 *    x(n)         --> old iterate x[k-1]
 *    f            --> function value at old iterate, f(x)
 *    g(n)         --> gradient at old iterate, g(x), or approximate
 *    a(n,n)       --> cholesky decomposition of hessian in lower
 *                     triangular part and diagonal.
 *                     hessian in upper triangular part and udiag.
 *    udiag(n)     --> diagonal of hessian in a(.,.)
 *    p(n)         --> newton step
 *    xpls(n)     <--  new iterate x[k]
 *    fpls        <--  function value at new iterate, f(xpls)
 *    fcn          --> name of subroutine to evaluate function
 *    sx(n)        --> diagonal scaling matrix for x
 *    stepmx       --> maximum allowable step size
 *    steptl       --> relative step size at which successive iterates
 *                     considered close enough to terminate algorithm
 *    dlt         <--> trust region radius
 *    iretcd      <--  return code
 *                       =0 satisfactory xpls found
 *                      =1 failed to find satisfactory xpls sufficiently
 *                          distinct from x
 *    mxtake      <--  boolean flag indicating step of maximum length used
 *    amu         <--> [retain value between successive calls]
 *    dltp        <--> [retain value between successive calls]
 *    phi         <--> [retain value between successive calls]
 *    phip0       <--> [retain value between successive calls]
 *    sc(n)        --> workspace
 *    xplsp(n)     --> workspace
 *    wrk0(n)      --> workspace
 *    epsm         --> machine epsilon
 *    itncnt       --> iteration count
 *    ipr          --> device to which to send output
 */

static
int 
hookdr_(int *nr, int *n, double *x,
	double *f, double *g, double *a, double *udiag,
	double *p, double *xpls, double *fpls,
	int (*fcn) (), double *sx, double *stepmx, double *steptl,
	double *dlt, int *iretcd, int *mxtake, double *amu,
	double *dltp, double *phi, double *phip0, double *sc,
	double *xplsp, double *wrk0, double *epsm, int *
	itncnt, int *ipr)
{
	int a_dim1, a_offset, i__1, i__2;

	double sqrt();

	double beta;
	int i, j;
	double alpha, fplsp;
	int fstime, nwtake;
	extern int tregup_(), hookst_();
	double rnwtln, tmp;


	a_dim1 = *nr;
	a_offset = a_dim1 + 1;
	a -= a_offset;
	--wrk0;
	--xplsp;
	--sc;
	--sx;
	--xpls;
	--p;
	--udiag;
	--g;
	--x;

	*iretcd = 4;
	fstime = TRUE;
	tmp = 0.;
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		tmp += sx[i] * sx[i] * p[i] * p[i];
	}
	rnwtln = sqrt(tmp);
/* $    write(ipr,954) rnwtln */

	if (*itncnt > 1) {
		goto L100;
	}
/*     if(itncnt.eq.1) */
/*     then */
	*amu = 0.;

	/* if first iteration and trust region not provided by user, */
	/* compute initial trust region. */

	if (*dlt != -1.) {
		goto L100;
	}
/*       if(dlt.eq. (-1.0d0)) */
/*       then */
	alpha = 0.;
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		alpha += g[i] * g[i] / (sx[i] * sx[i]);
	}
	beta = 0.;
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		tmp = 0.;
		i__2 = *n;
		for (j = i; j <= i__2; ++j) {
			tmp += a[j + i * a_dim1] * g[j] / (sx[j] * sx[j]);
		}
		beta += tmp * tmp;
	}
	*dlt = alpha * sqrt(alpha) / beta;
	*dlt = min(*dlt, *stepmx);
/* $        write(ipr,950) */
/* $        write(ipr,951) alpha,beta,dlt */
/*       endif */
/*     endif */

L100:

	/* find new step by more-hebdon algorithm */

	hookst_(nr, n, &g[1], &a[a_offset], &udiag[1], &p[1], &sx[1], &rnwtln,
	     dlt, amu, dltp, phi, phip0, &fstime, &sc[1], &nwtake, &wrk0[1],
		epsm, ipr);
	*dltp = *dlt;

	/* check new point and update trust region */

	tregup_(nr, n, &x[1], f, &g[1], &a[a_offset], fcn, &sc[1], &sx[1], &
	   nwtake, stepmx, steptl, dlt, iretcd, &xplsp[1], &fplsp, &xpls[1],
		fpls, mxtake, ipr, &c__3, &udiag[1]);
	if (*iretcd <= 1) {
		return 0;
	}
	goto L100;

/* %950 format(43h hookdr    initial trust region not given. , */
/* %   +       21h compute cauchy step.) */
/* %951 format(18h hookdr    alpha =,e20.13/ */
/* %   +       18h hookdr    beta  =,e20.13/ */
/* %   +       18h hookdr    dlt   =,e20.13) */
/* %952 format(28h hookdr    current step (sc)) */
/* %954 format(18h0hookdr    rnwtln=,e20.13) */
/* %955 format(14h hookdr       ,5(e20.13,3x)) */
}


/*
 *     subroutine hookst
 *
 *     purpose
 *
 *     find new step by more-hebdon algorithm
 *
 *     parameters
 *
 *     nr           --> row dimension of matrix
 *     n            --> dimension of problem
 *     g(n)         --> gradient at current iterate, g(x)
 *     a(n,n)       --> cholesky decomposition of hessian in
 *                      lower triangular part and diagonal.
 *                      hessian or approx in upper triangular part
 *     udiag(n)     --> diagonal of hessian in a(.,.)
 *     p(n)         --> newton step
 *     sx(n)        --> diagonal scaling matrix for n
 *     rnwtln       --> newton step length
 *     dlt         <--> trust region radius
 *     amu         <--> [retain value between successive calls]
 *    dltp         --> trust region radius at last exit from this routine
 *     phi         <--> [retain value between successive calls]
 *     phip0       <--> [retain value between successive calls]
 *     fstime      <--> boolean. =.true. if first entry to this routine
 *                      during k-th iteration
 *     sc(n)       <--  current step
 *     nwtake      <--  boolean, =.true. if newton step taken
 *     wrk0         --> workspace
 *     epsm         --> machine epsilon
 *     ipr          --> device to which to send output
 */

static
int 
hookst_(int *nr, int *n, double *g,
	double *a, double *udiag, double *p, double *sx,
	double *rnwtln, double *dlt, double *amu, double *
	dltp, double *phi, double *phip0, int *fstime, double
	*sc, int *nwtake, double *wrk0, double *epsm, int *
	ipr)
{
	double addmax, stepln;
	double alo;
	double amulo, amuup, hi;
	double d__1, d__2;
	double phip;
	double sqrt();
	extern double F77_SYMBOL(dnrm2) ();
	extern int choldc_();
	extern int forslv_();
	extern int lltslv_();
	int a_dim1, a_offset, i__1, i__2;
	int done;
	int i, j;
	int jp1;

	a_dim1 = *nr;
	a_offset = a_dim1 + 1;
	a -= a_offset;
	--wrk0;
	--sc;
	--sx;
	--p;
	--udiag;
	--g;

	/* hi and alo are constants used in this routine. */
	/* change here if other values are to be substituted. */

	*ipr = *ipr;
	hi = 1.5;
	alo = .75;

	if (*rnwtln > hi * *dlt) {
		goto L15;
	}
/*     if(rnwtln.le.hi*dlt) */
/*     then */

	/* take newton step */

	*nwtake = TRUE;
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		sc[i] = p[i];
	}
	*dlt = min(*dlt, *rnwtln);
	*amu = 0.;
/* $      write(ipr,951) */
	return 0;
/*     else */

	/* newton step not taken */

L15:
/* $      write(ipr,952) */
	*nwtake = FALSE;
	if (*amu <= 0.) {
		goto L20;
	}
/*       if(amu.gt.0.0d0) */
/*       then */
	*amu -= (*phi + *dltp) * (*dltp - *dlt + *phi) / (*dlt * phip);
/* $        write(ipr,956) amu */
/*       endif */
L20:
	*phi = *rnwtln - *dlt;
	if (!(*fstime)) {
		goto L28;
	}
/*       if(fstime) */
/*       then */
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		wrk0[i] = sx[i] * sx[i] * p[i];
	}

/*         solve l*y = (sx**2)*p */

	forslv_(nr, n, &a[a_offset], &wrk0[1], &wrk0[1]);

	d__1 = F77_SYMBOL(dnrm2) (n, &wrk0[1], &c__1);
	*phip0 = -(d__1 * d__1) / *rnwtln;
	*fstime = FALSE;
/*       endif */
L28:
	phip = *phip0;
	amulo = -(*phi) / phip;
	amuup = 0.;
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		amuup += g[i] * g[i] / (sx[i] * sx[i]);
	}
	amuup = sqrt(amuup) / *dlt;
	done = FALSE;
/* $      write(ipr,956) amu */
/* $      write(ipr,959) phi */
/* $      write(ipr,960) phip */
/* $      write(ipr,957) amulo */
/* $      write(ipr,958) amuup */

	/* test value of amu; generate next amu if necessary */

L100:
	if (done) {
		return 0;
	}
/* $      write(ipr,962) */
	if (*amu >= amulo && *amu <= amuup) {
		goto L110;
	}
/*       if(amu.lt.amulo .or.  amu.gt.amuup) */
/*       then */
	d__1 = sqrt(amulo * amuup), d__2 = amuup * .001;
	*amu = max(d__1, d__2);
/* $        write(ipr,956) amu */
/*       endif */
L110:

	/* copy (h,udiag) to l */
	/* where h <-- h+amu*(sx**2) [do not actually change (h,udiag)] */

	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
		a[j + j * a_dim1] = udiag[j] + *amu * sx[j] * sx[j];
		if (j == *n) {
			goto L130;
		}
		jp1 = j + 1;
		i__2 = *n;
		for (i = jp1; i <= i__2; ++i) {
			a[i + j * a_dim1] = a[j + i * a_dim1];
		}
L130:
		;
	}

	/* factor h=l(l+) */

	d__1 = sqrt(*epsm);
	choldc_(nr, n, &a[a_offset], &c_b33, &d__1, &addmax);

	/* solve h*p = l(l+)*sc = -g */

	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		wrk0[i] = -g[i];
	}
	lltslv_(nr, n, &a[a_offset], &sc[1], &wrk0[1]);
/* $      write(ipr,955) */
/* $      write(ipr,963) (sc(i),i=1,n) */

	/* reset h.  note since udiag has not been destroyed we need do */
	/* nothing here.  h is in the upper part and in udiag, still intact */

	stepln = 0.;
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		stepln += sx[i] * sx[i] * sc[i] * sc[i];
	}
	stepln = sqrt(stepln);
	*phi = stepln - *dlt;
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		wrk0[i] = sx[i] * sx[i] * sc[i];
	}
	forslv_(nr, n, &a[a_offset], &wrk0[1], &wrk0[1]);

	d__1 = F77_SYMBOL(dnrm2) (n, &wrk0[1], &c__1);
	phip = -(d__1 * d__1) / stepln;
/* $      write(ipr,961) dlt,stepln */
/* $      write(ipr,959) phi */
/* $      write(ipr,960) phip */
	if ((alo * *dlt > stepln || stepln > hi * *dlt) && amuup - amulo > 0.) {
		goto L170;
	}
/*       if((alo*dlt.le.stepln .and. stepln.le.hi*dlt) .or. */
/*            (amuup-amulo.le.0.0d0)) */
/*       then */

	/* sc is acceptable hookstep */

/* $        write(ipr,954) */
	done = TRUE;
	goto L100;
/*       else */

	/* sc not acceptable hookstep.  select new amu */

L170:
/* $        write(ipr,953) */
	d__1 = amulo, d__2 = *amu - *phi / phip;
	amulo = max(d__1, d__2);
	if (*phi < 0.) {
		amuup = min(amuup, *amu);
	}
	*amu -= stepln * *phi / (*dlt * phip);
/* $        write(ipr,956) amu */
/* $        write(ipr,957) amulo */
/* $        write(ipr,958) amuup */
	goto L100;
/*       endif */
/*     endif */

/* %951 format(27h0hookst    take newton step) */
/* %952 format(32h0hookst    newton step not taken) */
/* %953 format(31h hookst    sc is not acceptable) */
/* %954 format(27h hookst    sc is acceptable) */
/* %955 format(28h hookst    current step (sc)) */
/* %956 format(18h hookst    amu   =,e20.13) */
/* %957 format(18h hookst    amulo =,e20.13) */
/* %958 format(18h hookst    amuup =,e20.13) */
/* %959 format(18h hookst    phi   =,e20.13) */
/* %960 format(18h hookst    phip  =,e20.13) */
/* %961 format(18h hookst    dlt   =,e20.13/ */
/* %   +       18h hookst    stepln=,e20.13) */
/* %962 format(23h0hookst    find new amu) */
/* %963 format(14h hookst       ,5(e20.13,3x)) */
}


/*
 *    subroutine hsnint
 *
 *    purpose
 *
 *    provide initial hessian when using secant updates
 *
 *    parameters
 *
 *    nr           --> row dimension of matrix
 *    n            --> dimension of problem
 *    a(n,n)      <--  initial hessian (lower triangular matrix)
 *    sx(n)        --> diagonal scaling matrix for x
 *    method       --> algorithm to use to solve minimization problem
 *                       =1,2 factored secant method used
 *                       =3   unfactored secant method used
 */

static
int 
hsnint_(int *nr, int *n, double *a,
	double *sx, int *method)
{
	int a_dim1, a_offset, i__1, i__2;
	int i, j, jp1;

	a_dim1 = *nr;
	a_offset = a_dim1 + 1;
	a -= a_offset;
	--sx;

	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
		if (*method == 3) {
			a[j + j * a_dim1] = sx[j] * sx[j];
		}
		if (*method != 3) {
			a[j + j * a_dim1] = sx[j];
		}
		if (j == *n) {
			goto L100;
		}
		jp1 = j + 1;
		i__2 = *n;
		for (i = jp1; i <= i__2; ++i) {
			a[i + j * a_dim1] = 0.;
		}
L100:
		;
	}
	return 0;
}


/*
 *    subroutine lltslv
 *
 *    purpose
 *
 *    solve ax=b where a has the form l(l-transpose)
 *    but only the lower triangular part, l, is stored.
 *
 *    parameters
 *
 *    nr           --> row dimension of matrix
 *    n            --> dimension of problem
 *    a(n,n)       --> matrix of form l(l-transpose).
 *                     on return a is unchanged.
 *    x(n)        <--  solution vector
 *    b(n)         --> right-hand side vector
 *
 *    note
 *
 *    if b is not required by calling program, then
 *    b and x may share the same storage.
 */

static
int 
lltslv_(int *nr, int *n, double *a,
	double *x, double *b)
{
	extern int bakslv_(), forslv_();
	int a_dim1, a_offset;

	a_dim1 = *nr;
	a_offset = a_dim1 + 1;
	a -= a_offset;
	--b;
	--x;

	/* forward solve, result in x */

	forslv_(nr, n, &a[a_offset], &x[1], &b[1]);

	/* back solve, result in x */

	bakslv_(nr, n, &a[a_offset], &x[1], &x[1]);
	return 0;
}


/*
 *    subroutine lnsrch
 *
 *    purpose
 *
 *    find a next newton iterate by line search.
 *
 *    parameters
 *
 *    n            --> dimension of problem
 *    x(n)         --> old iterate:   x[k-1]
 *    f            --> function value at old iterate, f(x)
 *    g(n)         --> gradient at old iterate, g(x), or approximate
 *    p(n)         --> non-zero newton step
 *    xpls(n)     <--  new iterate x[k]
 *    fpls        <--  function value at new iterate, f(xpls)
 *    fcn          --> name of subroutine to evaluate function
 *    iretcd      <--  return code
 *    mxtake      <--  boolean flag indicating step of maximum length used
 *    stepmx       --> maximum allowable step size
 *    steptl       --> relative step size at which successive iterates
 *                     considered close enough to terminate algorithm
 *    sx(n)        --> diagonal scaling matrix for x
 *    ipr          --> device to which to send output
 *
 *    internal variables
 *
 *    sln              newton length
 *    rln              relative length of newton step
 */

static
int 
lnsrch_(int *n, double *x, double *f,
	double *g, double *p, double *xpls, double *fpls, /* Subroutine */ int (*fcn) (), int *mxtake, int *iretcd,
	double *stepmx, double *steptl, double *sx, int *ipr)
{
	double a, b;
	double d__1, d__2, d__3, d__4, d__5, d__6;
	double disc;
	double pfpls, t1, t2, t3, almbda, plmbda, tlmbda, rmnlmb;
	double scl, rln, sln, slp, tmp;
	extern double F77_SYMBOL(ddot) ();
	extern int sclmul_();
	int i;
	int i__1;

	--sx;
	--xpls;
	--p;
	--g;
	--x;

	*ipr = *ipr;
	*mxtake = FALSE;
	*iretcd = 2;
/* $    write(ipr,954) */
/* $    write(ipr,955) (p(i),i=1,n) */
	tmp = 0.;
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		tmp += sx[i] * sx[i] * p[i] * p[i];
	}
	sln = sqrt(tmp);
	if (sln <= *stepmx) {
		goto L10;
	}
	/* newton step longer than maximum allowed */

	scl = *stepmx / sln;
	sclmul_(n, &scl, &p[1], &p[1]);
	sln = *stepmx;
/* $      write(ipr,954) */
/* $      write(ipr,955) (p(i),i=1,n) */
L10:
	slp = F77_SYMBOL(ddot) (n, &g[1], &c__1, &p[1], &c__1);
	rln = 0.;
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		d__5 = (d__2 = x[i], abs(d__2)), d__6 = 1. / sx[i];
		d__3 = rln, d__4 = (d__1 = p[i], abs(d__1)) / max(d__5, d__6);
		rln = max(d__3, d__4);
	}
	rmnlmb = *steptl / rln;
	almbda = 1.;
/* $    write(ipr,952) sln,slp,rmnlmb,stepmx,steptl */

	/* loop */
	/*
	 * check if new iterate satisfactory.  generate new lambda if
	 * necessary.
	 */

L100:
	if (*iretcd < 2) {
		return 0;
	}
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		xpls[i] = x[i] + almbda * p[i];
	}
	(*fcn) (n, &xpls[1], fpls);
/* $    write(ipr,950) almbda */
/* $    write(ipr,951) */
/* $    write(ipr,955) (xpls(i),i=1,n) */
/* $    write(ipr,953) fpls */
	if (*fpls > *f + slp * 1e-4 * almbda) {
		goto L130;
	}
/*     if(fpls.le. f+slp*1.d-4*almbda) */
/*     then */

	/* solution found */

	*iretcd = 0;
	if (almbda == 1. && sln > *stepmx * .99) {
		*mxtake = TRUE;
	}
	goto L100;

	/* solution not (yet) found */

/*     else */
L130:
	if (almbda >= rmnlmb) {
		goto L140;
	}
/*       if(almbda .lt. rmnlmb) */
/*       then */

/*       no satisfactory xpls found sufficiently distinct from x */

	*iretcd = 1;
	goto L100;
/*       else */

/*       calculate new lambda */

L140:
	if (almbda != 1.) {
		goto L150;
	}
/*         if(almbda.eq.1.0d0) */
/*         then */

/*       first backtrack: quadratic fit */

	tlmbda = -slp / ((*fpls - *f - slp) * 2.);
	goto L170;
/*         else */

/*       all subsequent backtracks: cubic fit */

L150:
	t1 = *fpls - *f - almbda * slp;
	t2 = pfpls - *f - plmbda * slp;
	t3 = 1. / (almbda - plmbda);
	a = t3 * (t1 / (almbda * almbda) - t2 / (plmbda * plmbda));
	b = t3 * (t2 * almbda / (plmbda * plmbda) - t1 * plmbda / (almbda *
								   almbda));
	disc = b * b - a * 3. * slp;
	if (disc <= b * b) {
		goto L160;
	}
/*           if(disc.gt. b*b) */
/*           then */

/*       only one positive critical point, must be minimum */

	tlmbda = (-b + DSIGN(&c_b146, &a) * sqrt(disc)) / (a * 3.);
	goto L165;
/*           else */

/*       both critical points positive, first is minimum */

L160:
	tlmbda = (-b - DSIGN(&c_b146, &a) * sqrt(disc)) / (a * 3.);
/*           endif */
L165:
	if (tlmbda > almbda * .5) {
		tlmbda = almbda * .5;
	}
/*         endif */
L170:
	plmbda = almbda;
	pfpls = *fpls;
	if (tlmbda >= almbda * .1) {
		goto L180;
	}
/*         if(tlmbda.lt.almbda/10.0d0) */
/*         then */
	almbda *= .1;
	goto L190;
/*         else */
L180:
	almbda = tlmbda;
/*         endif */
/*       endif */
/*     endif */
L190:
	goto L100;
/* %950 format(18h lnsrch    almbda=,e20.13) */
/* %951 format(29h lnsrch    new iterate (xpls)) */
/* %952 format(18h lnsrch    sln   =,e20.13/ */
/* %   +       18h lnsrch    slp   =,e20.13/ */
/* %   +       18h lnsrch    rmnlmb=,e20.13/ */
/* %   +       18h lnsrch    stepmx=,e20.13/ */
/* %   +       18h lnsrch    steptl=,e20.13) */
/* %953 format(19h lnsrch    f(xpls)=,e20.13) */
/* %954 format(26h0lnsrch    newton step (p)) */
/* %955 format(14h lnsrch       ,5(e20.13,3x)) */
}

/*
 *    subroutine mvmltl
 *
 *    purpose
 *
 *    compute y=lx
 *    where l is a lower triangular matrix stored in a
 *
 *    parameters
 *
 *    nr           --> row dimension of matrix
 *    n            --> dimension of problem
 *    a(n,n)       --> lower triangular (n*n) matrix
 *    x(n)         --> operand vector
 *    y(n)        <--  result vector
 *
 *    note
 *
 *    x and y cannot share storage
 */

static
int 
mvmltl_(int *nr, int *n, double *a,
	double *x, double *y)
{
	int a_dim1, a_offset, i__1, i__2;

	int i, j;
	double sum;

	a_dim1 = *nr;
	a_offset = a_dim1 + 1;
	a -= a_offset;
	--y;
	--x;

	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		sum = 0.;
		i__2 = i;
		for (j = 1; j <= i__2; ++j) {
			sum += a[i + j * a_dim1] * x[j];
		}
		y[i] = sum;
	}
	return 0;
}


/*
 *    subroutine mvmlts
 *
 *    purpose
 *
 *    compute y=ax
 *    where "a" is a symmetric (n*n) matrix stored in its lower
 *    triangular part and x,y are n-vectors
 *
 *    parameters
 *
 *    nr           --> row dimension of matrix
 *    n            --> dimension of problem
 *    a(n,n)       --> symmetric (n*n) matrix stored in
 *                     lower triangular part and diagonal
 *    x(n)         --> operand vector
 *    y(n)        <--  result vector
 *
 *    note
 *
 *    x and y cannot share storage.
 */

static
int 
mvmlts_(int *nr, int *n, double *a,
	double *x, double *y)
{
	int a_dim1, a_offset, i__1, i__2;

	int i, j, ip1;
	double sum;

	a_dim1 = *nr;
	a_offset = a_dim1 + 1;
	a -= a_offset;
	--y;
	--x;

	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		sum = 0.;
		i__2 = i;
		for (j = 1; j <= i__2; ++j) {
			sum += a[i + j * a_dim1] * x[j];
		}
		if (i == *n) {
			goto L25;
		}
		ip1 = i + 1;
		i__2 = *n;
		for (j = ip1; j <= i__2; ++j) {
			sum += a[j + i * a_dim1] * x[j];
		}
L25:
		y[i] = sum;
	}
	return 0;
}

/*
 *    subroutine mvmltu
 *
 *    purpose
 *
 *    compute y=(l+)x
 *    where l is a lower triangular matrix stored in a
 *    (l-transpose (l+) is taken implicitly)
 *
 *    parameters
 *
 *    nr           --> row dimension of matrix
 *    n            --> dimension of problem
 *    a(nr,1)       --> lower triangular (n*n) matrix
 *    x(n)         --> operand vector
 *    y(n)        <--  result vector
 *
 *    note
 *
 *    x and y cannot share storage
 */

static
int 
mvmltu_(int *nr, int *n, double *a,
	double *x, double *y)
{
	int a_dim1, a_offset, i__1, i__2;

	int i, j;
	double sum;

	a_dim1 = *nr;
	a_offset = a_dim1 + 1;
	a -= a_offset;
	--y;
	--x;

	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		sum = 0.;
		i__2 = *n;
		for (j = i; j <= i__2; ++j) {
			sum += a[j + i * a_dim1] * x[j];
		}
		y[i] = sum;
	}
	return 0;
}


/*
 *    subroutine optchk
 *
 *    purpose
 *
 *    check input for reasonableness
 *
 *    parameters
 *
 *    n            --> dimension of problem
 *    x(n)         --> on entry, estimate to root of fcn
 *    typsiz(n)   <--> typical size of each component of x
 *    sx(n)       <--  diagonal scaling matrix for x
 *    fscale      <--> estimate of scale of objective function fcn
 *    gradtl       --> tolerance at which gradient considered close
 *                     enough to zero to terminate algorithm
 *    itnlim      <--> maximum number of allowable iterations
 *    ndigit      <--> number of good digits in optimization function fcn
 *    epsm         --> machine epsilon
 *    dlt         <--> trust region radius
 *    method      <--> algorithm indicator
 *    iexp        <--> expense flag
 *    iagflg      <--> =1 if analytic gradient supplied
 *    iahflg      <--> =1 if analytic hessian supplied
 *    stepmx      <--> maximum step size
 *    msg         <--> message and error code
 *    ipr          --> device to which to send output
 */

static
int 
optchk_(int *n, double *x, double *typsiz,
	double *sx, double *fscale, double *gradtl, int *
	itnlim, int *ndigit, double *epsm, double *dlt, int *
	method, int *iexp, int *iagflg, int *iahflg, double *
	stepmx, int *msg, int *ipr)
{
	double d__1;
	double stpsiz;
	int i;
	int i__1;

	--sx;
	--typsiz;
	--x;

	/* check that parameters only take on acceptable values. */
	/* if not, set them to default values. */

	if (*method < 1 || *method > 3) {
		*method = 1;
	}
	if (*iagflg != 1) {
		*iagflg = 0;
	}
	if (*iahflg != 1) {
		*iahflg = 0;
	}
	if (*iexp != 0) {
		*iexp = 1;
	}
	if (*msg / 2 % 2 == 1 && *iagflg == 0) {
		goto L830;
	}
	if (*msg / 4 % 2 == 1 && *iahflg == 0) {
		goto L835;
	}
	/* check dimension of problem */

	if (*n <= 0) {
		goto L805;
	}
	if (*n == 1 && *msg % 2 == 0) {
		goto L810;
	}
	/* compute scale matrix */

	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		if (typsiz[i] == 0.) {
			typsiz[i] = 1.;
		}
		if (typsiz[i] < 0.) {
			typsiz[i] = -typsiz[i];
		}
		sx[i] = 1. / typsiz[i];
	}

	/* check maximum step size */

	if (*stepmx > 0.) {
		goto L20;
	}
	stpsiz = 0.;
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		stpsiz += x[i] * x[i] * sx[i] * sx[i];
	}
	stpsiz = sqrt(stpsiz);
	d__1 = stpsiz * 1e3;
	*stepmx = max(d__1, 1e3);
L20:
	/* check function scale */
	if (*fscale == 0.) {
		*fscale = 1.;
	}
	if (*fscale < 0.) {
		*fscale = -(*fscale);
	}
	/* check gradient tolerance */
	if (*gradtl < 0.) {
		goto L815;
	}
	/* check iteration limit */
	if (*itnlim <= 0) {
		goto L820;
	}
	/* check number of digits of accuracy in function fcn */
	if (*ndigit == 0) {
		goto L825;
	}
	if (*ndigit < 0) {
		*ndigit = (int) (-DLOG10(epsm));
	}
	/* check trust region radius */
	if (*dlt <= 0.) {
		*dlt = -1.;
	}
	if (*dlt > *stepmx) {
		*dlt = *stepmx;
	}
	return 0;

	/* error exits */

/* %805 write(ipr,901) n */
/* %    msg=-1 */
L805:
	*msg = -1;
	goto L895;
/* %810 write(ipr,902) */
/* %    msg=-2 */
L810:
	*msg = -2;
	goto L895;
/* %815 write(ipr,903) gradtl */
/* %    msg=-3 */
L815:
	*msg = -3;
	goto L895;
/* %820 write(ipr,904) itnlim */
/* %    msg=-4 */
L820:
	*msg = -4;
	goto L895;
/* %825 write(ipr,905) ndigit */
/* %    msg=-5 */
L825:
	*msg = -5;
	goto L895;
/* %830 write(ipr,906) msg,iagflg */
/* %    msg=-6 */
L830:
	*msg = -6;
	goto L895;
/* %835 write(ipr,907) msg,iahflg */
/* %    msg=-7 */
L835:
	*msg = -7;
L895:
	return 0;
/* %901 format(32h0optchk    illegal dimension, n=,i5) */
/* %902 format(55h0optchk    +++ warning +++  this package is inefficient,
 */
/* %   +       26h for problems of size n=1./ */
/* %   +       48h optchk    check installation libraries for more, */
/* %   +       22h appropriate routines./ */
/* %   +       41h optchk    if none, set msg and resubmit.) */
/* %903 format(38h0optchk    illegal tolerance.  gradtl=,e20.13) */
/* %904 format(44h0optchk    illegal iteration limit.  itnlim=,i5) */
/* %905 format(52h0optchk    minimization function has no good digits., */
/* %   +        9h  ndigit=,i5) */
/* %906 format(50h0optchk    user requests that analytic gradient be, */
/* %   +       33h accepted as properly coded (msg=,i5, 2h),/ */
/* %   +       45h optchk    but analytic gradient not supplied, */
/* %   +        9h (iagflg=,i5, 2h).) */
/* %907 format(49h0optchk    user requests that analytic hessian be, */
/* %   +       33h accepted as properly coded (msg=,i5, 2h),/ */
/* %   +       44h optchk    but analytic hessian not supplied, */
/* %   +        9h (iahflg=,i5, 2h).) */
}


/*
 *    subroutine optdrv
 *
 *    purpose
 *
 *    driver for non-linear optimization problem
 *
 *    parameters
 *
 *    nr           --> row dimension of matrix
 *    n            --> dimension of problem
 *    x(n)         --> on entry: estimate to a root of fcn
 *   fcn          --> name of subroutine to evaluate optimization function
 *
 *                     must be declared external in calling routine
 *                               fcn: r(n) --> r(1)
 *   d1fcn        --> (optional) name of subroutine to evaluate gradient
 *                    of fcn.  must be declared external in calling routine
 *
 *   d2fcn        --> (optional) name of subroutine to evaluate hessian of
 *
 *                    of fcn.  must be declared external in calling routine
 *
 *    typsiz(n)    --> typical size for each component of x
 *    fscale       --> estimate of scale of objective function
 *    method       --> algorithm to use to solve minimization problem
 *                       =1 line search
 *                       =2 double dogleg
 *                       =3 more-hebdon
 *    iexp         --> =1 if optimization function fcn is expensive to
 *                    evaluate, =0 otherwise.  if set then hessian will
 *                     be evaluated by secant update instead of
 *                     analytically or by finite differences
 *    msg         <--> on input:  (.gt.0) message to inhibit certain
 *                       automatic checks
 *                     on output: (.lt.0) error code; =0 no error
 *   ndigit       --> number of good digits in optimization function fcn
 *    itnlim       --> maximum number of allowable iterations
 *    iagflg       --> =1 if analytic gradient supplied
 *    iahflg       --> =1 if analytic hessian supplied
 *    ipr          --> device to which to send output
 *    dlt          --> trust region radius
 *    gradtl       --> tolerance at which gradient considered close
 *                     enough to zero to terminate algorithm
 *    stepmx       --> maximum allowable step size
 *    steptl       --> relative step size at which successive iterates
 *                     considered close enough to terminate algorithm
 *    xpls(n)     <--> on exit:  xpls is local minimum
 *    fpls        <--> on exit:  function value at solution, xpls
 *    gpls(n)     <--> on exit:  gradient at solution xpls
 *    itrmcd      <--  termination code
 *    a(n,n)       --> workspace for hessian (or estimate)
 *                     and its cholesky decomposition
 *    udiag(n)     --> workspace [for diagonal of hessian]
 *    g(n)         --> workspace (for gradient at current iterate)
 *    p(n)         --> workspace for step
 *    sx(n)        --> workspace (for diagonal scaling matrix)
 *    wrk0(n)      --> workspace
 *    wrk1(n)      --> workspace
 *    wrk2(n)      --> workspace
 *    wrk3(n)      --> workspace
 *
 *
 *    internal variables
 *
 *    analtl           tolerance for comparison of estimated and
 *                     analytical gradients and hessians
 *    epsm             machine epsilon
 *    f                function value: fcn(x)
 *    itncnt           current iteration, k
 *    rnf              relative noise in optimization function fcn.
 *                          noise=10.**(-ndigit)
 */

static
int 
optdrv_(int *nr, int *n, double *x, /* Subroutine */ int (*fcn) (), int (*d1fcn) (), /* Subroutine */ int (*d2fcn) (), double *typsiz, double *fscale,
	int *method, int *iexp, int *msg, int *ndigit,
	int *itnlim, int *iagflg, int *iahflg, int *ipr,
	double *dlt, double *gradtl, double *stepmx, double *
	steptl, double *xpls, double *fpls, double *gpls, int
	*itrmcd, double *a, double *udiag, double *g, double *
	p, double *sx, double *wrk0, double *wrk1, double *
	wrk2, double *wrk3)
{
	double analtl;
	double d__1, d__2;
	double dlpsav, phisav, dltsav, amusav;
	double dltp, epsm, phip0, f;
	double phi, amu, rnf, wrk;
	double phpsav;
	extern double F77_SYMBOL(d1mach) ();
	extern int dogdrv_(), optchk_();
	extern int hookdr_(), hsnint_();
	extern int lnsrch_();
	extern int result_(), lltslv_(), optstp_();
	extern int secfac_(), grdchk_(), heschk_();
	extern int sndofd_(), chlhsn_(), fstocd_(), secunf_(), fstofd_();
	int a_dim1, a_offset, i__1;
	int i;
	int icscmx;
	int iretcd;
	int itncnt;
	int mxtake;
	int noupdt;


	a_dim1 = *nr;
	a_offset = a_dim1 + 1;
	a -= a_offset;
	--wrk3;
	--wrk2;
	--wrk1;
	--wrk0;
	--sx;
	--p;
	--g;
	--udiag;
	--gpls;
	--xpls;
	--typsiz;
	--x;

	/* initialization */

	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		p[i] = 0.;
	}
	itncnt = 0;
	iretcd = -1;
	epsm = F77_SYMBOL(d1mach) (&c__4);
	optchk_(n, &x[1], &typsiz[1], &sx[1], fscale, gradtl, itnlim, ndigit, &
		epsm, dlt, method, iexp, iagflg, iahflg, stepmx, msg, ipr);
	if (*msg < 0) {
		return 0;
	}
	i__1 = -(*ndigit);
	d__1 = POW_DI(&c_b3, &i__1);
	rnf = max(d__1, epsm);
	d__1 = .01, d__2 = sqrt(rnf);
	analtl = max(d__1, d__2);

/* %    if(mod(msg/8,2).eq.1) go to 15 */
/* %    write(ipr,901) */
/* %    write(ipr,900) (typsiz(i),i=1,n) */
/* %    write(ipr,902) */
/* %    write(ipr,900) (sx(i),i=1,n) */
/* %    write(ipr,903) fscale */
/* %    write(ipr,904) ndigit,iagflg,iahflg,iexp,method,itnlim,epsm */
/* %    write(ipr,905) stepmx,steptl,gradtl,dlt,rnf,analtl */
/* % 15 continue */

	/* evaluate fcn(x) */

	(*fcn) (n, &x[1], &f);

	/* evaluate analytic or finite difference gradient and check analytic */
	/* gradient, if requested. */

	if (*iagflg == 1) {
		goto L20;
	}
/*     if (iagflg .eq. 0) */
/*     then */
	fstofd_(&c__1, &c__1, n, &x[1], fcn, &f, &g[1], &sx[1], &rnf, &wrk, &c__1);
	goto L25;

L20:
	(*d1fcn) (n, &x[1], &g[1]);
	if (*msg / 2 % 2 == 1) {
		goto L25;
	}
/*     if (mod(msg/2,2).eq.0) */
/*     then */
	grdchk_(n, &x[1], fcn, &f, &g[1], &typsiz[1], &sx[1], fscale, &rnf, &
		analtl, &wrk1[1], msg, ipr);
	if (*msg < 0) {
		return 0;
	}
L25:

	optstp_(n, &x[1], &f, &g[1], &wrk1[1], &itncnt, &icscmx, itrmcd, gradtl,
		steptl, &sx[1], fscale, itnlim, &iretcd, &mxtake, ipr, msg);
	if (*itrmcd != 0) {
		goto L700;
	}
	if (*iexp != 1) {
		goto L80;
	}
	/* if optimization function expensive to evaluate (iexp=1), then */
	/* hessian will be obtained by secant updates.  get initial hessian.  */

	hsnint_(nr, n, &a[a_offset], &sx[1], method);
	goto L90;
L80:

	/* evaluate analytic or finite difference hessian and check analytic */
	/* hessian if requested (only if user-supplied analytic hessian */
	/* routine d2fcn fills only lower triangular part and diagonal of a).  */

	if (*iahflg == 1) {
		goto L82;
	}
/*     if (iahflg .eq. 0) */
/*     then */
	if (*iagflg == 1) {
		fstofd_(nr, n, n, &x[1], d1fcn, &g[1], &a[a_offset], &sx[1], &rnf, &
			wrk1[1], &c__3);
	}
	if (*iagflg != 1) {
		sndofd_(nr, n, &x[1], fcn, &f, &a[a_offset], &sx[1], &rnf, &wrk1[1], &
			wrk2[1]);
	}
	goto L88;

/*     else */
L82:
	if (*msg / 4 % 2 == 0) {
		goto L85;
	}
/*        if (mod(msg/4, 2) .eq. 1) */
/*        then */
	(*d2fcn) (nr, n, &x[1], &a[a_offset]);
	goto L88;

/*        else */
L85:
	heschk_(nr, n, &x[1], fcn, d1fcn, d2fcn, &f, &g[1], &a[a_offset], &typsiz[
	   1], &sx[1], &rnf, &analtl, iagflg, &udiag[1], &wrk1[1], &wrk2[1],
		msg, ipr);

	/* heschk evaluates d2fcn and checks it against the finite */
	/* difference hessian which it calculates by calling fstofd */
	/* (if iagflg .eq. 1) or sndofd (otherwise). */

	if (*msg < 0) {
		return 0;
	}
L88:

L90:
	if (*msg / 8 % 2 == 0) {
		result_(nr, n, &x[1], &f, &g[1], &a[a_offset], &p[1], &itncnt, &c__1,
			ipr);
	}
	/* iteration */

L100:
	++itncnt;

	/* find perturbed local model hessian and its ll+ decomposition */
	/*
	 * (skip this step if line search or dogstep techniques being used
	 * with
	 */
	/* secant updates.  cholesky decomposition l already obtained from */
	/* secfac.) */

	if (*iexp == 1 && *method != 3) {
		goto L105;
	}
L103:
	chlhsn_(nr, n, &a[a_offset], &epsm, &sx[1], &udiag[1]);
L105:

	/* solve for newton step:  ap=-g */

	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		wrk1[i] = -g[i];
	}
	lltslv_(nr, n, &a[a_offset], &p[1], &wrk1[1]);

	/* decide whether to accept newton step  xpls=x + p */
	/* or to choose xpls by a global strategy. */

	if (*iagflg != 0 || *method == 1) {
		goto L111;
	}
	dltsav = *dlt;
	if (*method == 2) {
		goto L111;
	}
	amusav = amu;
	dlpsav = dltp;
	phisav = phi;
	phpsav = phip0;
L111:
	if (*method == 1) {
		lnsrch_(n, &x[1], &f, &g[1], &p[1], &xpls[1], fpls, fcn, &mxtake, &
			iretcd, stepmx, steptl, &sx[1], ipr);
	}
	if (*method == 2) {
		dogdrv_(nr, n, &x[1], &f, &g[1], &a[a_offset], &p[1], &xpls[1], fpls,
		fcn, &sx[1], stepmx, steptl, dlt, &iretcd, &mxtake, &wrk0[1],
			&wrk1[1], &wrk2[1], &wrk3[1], ipr);
	}
	if (*method == 3) {
		hookdr_(nr, n, &x[1], &f, &g[1], &a[a_offset], &udiag[1], &p[1], &
		 xpls[1], fpls, fcn, &sx[1], stepmx, steptl, dlt, &iretcd, &
		mxtake, &amu, &dltp, &phi, &phip0, &wrk0[1], &wrk1[1], &wrk2[
						   1], &epsm, &itncnt, ipr);
	}
	/* if could not find satisfactory step and forward difference */
	/* gradient was used, retry using central difference gradient. */

	if (iretcd != 1 || *iagflg != 0) {
		goto L112;
	}
/*     if (iretcd .eq. 1 .and. iagflg .eq. 0) */
/*     then */

	/* set iagflg for central differences */

	*iagflg = -1;
/* %       write(ipr,906) itncnt */

	fstocd_(n, &x[1], fcn, &sx[1], &rnf, &g[1]);
	if (*method == 1) {
		goto L105;
	}
	*dlt = dltsav;
	if (*method == 2) {
		goto L105;
	}
	amu = amusav;
	dltp = dlpsav;
	phi = phisav;
	phip0 = phpsav;
	goto L103;
/*     endif */

	/* calculate step for output */

L112:
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		p[i] = xpls[i] - x[i];
	}

	/* calculate gradient at xpls */

	if (*iagflg == -1) {
		goto L116;
	}
	if (*iagflg == 0) {
		goto L118;
	}
	/* analytic gradient */

	(*d1fcn) (n, &xpls[1], &gpls[1]);
	goto L120;

	/* central difference gradient */

L116:
	fstocd_(n, &xpls[1], fcn, &sx[1], &rnf, &gpls[1]);
	goto L120;

	/* forward difference gradient */

L118:
	fstofd_(&c__1, &c__1, n, &xpls[1], fcn, fpls, &gpls[1], &sx[1], &rnf, &
		wrk, &c__1);
L120:

	/* check whether stopping criteria satisfied */

	optstp_(n, &xpls[1], fpls, &gpls[1], &x[1], &itncnt, &icscmx, itrmcd,
	      gradtl, steptl, &sx[1], fscale, itnlim, &iretcd, &mxtake, ipr,
		msg);
	if (*itrmcd != 0) {
		goto L690;
	}
	/* evaluate hessian at xpls */

	if (*iexp == 0) {
		goto L130;
	}
	if (*method == 3) {
		secunf_(nr, n, &x[1], &g[1], &a[a_offset], &udiag[1], &xpls[1], &gpls[
										      1], &epsm, &itncnt, &rnf, iagflg, &noupdt, &wrk1[1], &wrk2[1],
			&wrk3[1]);
	}
	if (*method != 3) {
		secfac_(nr, n, &x[1], &g[1], &a[a_offset], &xpls[1], &gpls[1], &epsm,
		&itncnt, &rnf, iagflg, &noupdt, &wrk0[1], &wrk1[1], &wrk2[1],
			&wrk3[1]);
	}
	goto L150;
L130:
	if (*iahflg == 1) {
		goto L140;
	}
	if (*iagflg == 1) {
		fstofd_(nr, n, n, &xpls[1], d1fcn, &gpls[1], &a[a_offset], &sx[1], &
			rnf, &wrk1[1], &c__3);
	}
	if (*iagflg != 1) {
		sndofd_(nr, n, &xpls[1], fcn, fpls, &a[a_offset], &sx[1], &rnf, &wrk1[
							      1], &wrk2[1]);
	}
	goto L150;
L140:
	(*d2fcn) (nr, n, &xpls[1], &a[a_offset]);
L150:
	if (*msg / 16 % 2 == 1) {
		result_(nr, n, &xpls[1], fpls, &gpls[1], &a[a_offset], &p[1], &itncnt,
			&c__1, ipr);
	}
	/* x <-- xpls  and  g <-- gpls  and  f <-- fpls */

	f = *fpls;
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		x[i] = xpls[i];
		g[i] = gpls[i];
	}
	goto L100;

	/* termination */

	/* reset xpls,fpls,gpls,  if previous iterate solution */

L690:
	if (*itrmcd != 3) {
		goto L710;
	}
L700:
	*fpls = f;
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		xpls[i] = x[i];
		gpls[i] = g[i];
	}

	/* print results */

L710:
	if (*msg / 8 % 2 == 0) {
		result_(nr, n, &xpls[1], fpls, &gpls[1], &a[a_offset], &p[1], &itncnt,
			&c__0, ipr);
	}
	*msg = 0;
	return 0;

/* %900 format(14h optdrv       ,5(e20.13,3x)) */
/* %901 format(20h0optdrv    typical x) */
/* %902 format(40h optdrv    diagonal scaling matrix for x) */
/* %903 format(22h optdrv    typical f =,e20.13) */
/* %904 format(40h0optdrv    number of good digits in fcn=,i5/ */
/* %   +       27h optdrv    gradient flag  =,i5,18h   (=1 if analytic, */
/* %   +       19h gradient supplied)/ */
/* %   +       27h optdrv    hessian flag   =,i5,18h   (=1 if analytic, */
/* %   +       18h hessian supplied)/ */
/* %   +       27h optdrv    expense flag   =,i5, 9h   (=1 if, */
/* %   +       45h minimization function expensive to evaluate)/ */
/* %   +       27h optdrv    method to use  =,i5,19h   (=1,2,3 for line,
*/
/* %   +       49h search, double dogleg, more-hebdon respectively)/ */
/* %   +       27h optdrv    iteration limit=,i5/ */
/* %   +       27h optdrv    machine epsilon=,e20.13) */
/* %905 format(30h0optdrv    maximum step size =,e20.13/ */
/* %   +       30h optdrv    step tolerance    =,e20.13/ */
/* %   +       30h optdrv    gradient tolerance=,e20.13/ */
/* %   +       30h optdrv    trust reg radius  =,e20.13/ */
/* %   +       30h optdrv    rel noise in fcn  =,e20.13/ */
/* %   +       30h optdrv    anal-fd tolerance =,e20.13) */
/* %906 format(52h optdrv    shift from forward to central differences, */
/* %   1   14h in iteration , i5) */
}


/*
 *    subroutine optif0
 *
 *    purpose
 *
 *    provide simplest interface to minimization package.
 *    user has no control over options.
 *
 *    parameters
 *
 *    nr           --> row dimension of matrix
 *    n            --> dimension of problem
 *    x(n)         --> initial estimate of minimum
 *    fcn          --> name of routine to evaluate minimization function.
 *                     must be declared external in calling routine.
 *    xpls(n)     <--  local minimum
 *    fpls        <--  function value at local minimum xpls
 *    gpls(n)     <--  gradient at local minimum xpls
 *    itrmcd      <--  termination code
 *    a(n,n)       --> workspace
 *    wrk(n,9)     --> workspace
 */

int 
F77_SYMBOL(optif0) (int *nr, int *n, double *x, /* Subroutine */ int (*fcn) (), double *xpls, double *fpls,
		    double *gpls, int *itrmcd, double *a, double *wrk)
{
	int a_dim1, a_offset, wrk_dim1, wrk_offset;

	int iexp;
	extern int d1fcn_(), d2fcn_();
	int iagflg, iahflg;
	double fscale, gradtl;
	int ndigit;
	extern int dfault_();
	int method, itnlim;
	double steptl;
	extern int optdrv_();
	double stepmx, dlt;
	int msg, ipr;


	/* equivalence wrk(n,1) = udiag(n) */
	/* wrk(n,2) = g(n) */
	/* wrk(n,3) = p(n) */
	/* wrk(n,4) = typsiz(n) */
	/* wrk(n,5) = sx(n) */
	/* wrk(n,6) = wrk0(n) */
	/* wrk(n,7) = wrk1(n) */
	/* wrk(n,8) = wrk2(n) */
	/* wrk(n,9) = wrk3(n) */

	wrk_dim1 = *nr;
	wrk_offset = wrk_dim1 + 1;
	wrk -= wrk_offset;
	a_dim1 = *nr;
	a_offset = a_dim1 + 1;
	a -= a_offset;
	--gpls;
	--xpls;
	--x;

	dfault_(n, &x[1], &wrk[(wrk_dim1 << 2) + 1], &fscale, &method, &iexp, &
	     msg, &ndigit, &itnlim, &iagflg, &iahflg, &ipr, &dlt, &gradtl, &
		stepmx, &steptl);
	optdrv_(nr, n, &x[1], fcn, d1fcn_, d2fcn_, &wrk[(wrk_dim1 << 2) + 1], &
	   fscale, &method, &iexp, &msg, &ndigit, &itnlim, &iagflg, &iahflg,
	    &ipr, &dlt, &gradtl, &stepmx, &steptl, &xpls[1], fpls, &gpls[1],
	    itrmcd, &a[a_offset], &wrk[wrk_dim1 + 1], &wrk[(wrk_dim1 << 1) +
	   1], &wrk[wrk_dim1 * 3 + 1], &wrk[wrk_dim1 * 5 + 1], &wrk[wrk_dim1
	  * 6 + 1], &wrk[wrk_dim1 * 7 + 1], &wrk[(wrk_dim1 << 3) + 1], &wrk[
							 wrk_dim1 * 9 + 1]);
	return 0;
}


/*
 *    subroutine optif9
 *
 *    purpose
 *
 *    provide complete interface to minimization package.
 *    user has full control over options.
 *
 *    parameters
 *
 *    nr           --> row dimension of matrix
 *    n            --> dimension of problem
 *    x(n)         --> on entry: estimate to a root of fcn
 *    fcn          --> name of subroutine to evaluate optimization function
 *                     must be declared external in calling routine
 *                               fcn: r(n) --> r(1)
 *    d1fcn        --> (optional) name of subroutine to evaluate gradient
 *                     of fcn.  must be declared external in calling routine
 *    d2fcn        --> (optional) name of subroutine to evaluate hessian of
 *                     of fcn.  must be declared external in calling routine
 *    typsiz(n)    --> typical size for each component of x
 *    fscale       --> estimate of scale of objective function
 *    method       --> algorithm to use to solve minimization problem
 *                       =1 line search
 *                       =2 double dogleg
 *                       =3 more-hebdon
 *    iexp         --> =1 if optimization function fcn is expensive to
 *                    evaluate, =0 otherwise.  if set then hessian will
 *                     be evaluated by secant update instead of
 *                     analytically or by finite differences
 *    msg         <--> on input:  (.gt.0) message to inhibit certain
 *                       automatic checks
 *                     on output: (.lt.0) error code; =0 no error
 *    ndigit       --> number of good digits in optimization function fcn
 *    itnlim       --> maximum number of allowable iterations
 *    iagflg       --> =1 if analytic gradient supplied
 *    iahflg       --> =1 if analytic hessian supplied
 *    ipr          --> device to which to send output
 *    dlt          --> trust region radius
 *    gradtl       --> tolerance at which gradient considered close
 *                     enough to zero to terminate algorithm
 *    stepmx       --> maximum allowable step size
 *    steptl       --> relative step size at which successive iterates
 *                     considered close enough to terminate algorithm
 *    xpls(n)     <--> on exit:  xpls is local minimum
 *    fpls        <--> on exit:  function value at solution, xpls
 *    gpls(n)     <--> on exit:  gradient at solution xpls
 *    itrmcd      <--  termination code
 *    a(n,n)       --> workspace for hessian (or estimate)
 *                     and its cholesky decomposition
 *    wrk(n,8)     --> workspace
 */

int 
F77_SYMBOL(optif9) (int *nr, int *n, double *x, /* Subroutine */ int (*fcn) (), int (*d1fcn) (), /* Subroutine */ int (*d2fcn) (), double *typsiz, double *fscale,
		    int *method, int *iexp, int *msg, int *ndigit,
		    int *itnlim, int *iagflg, int *iahflg, int *ipr,
		    double *dlt, double *gradtl, double *stepmx, double *
		    steptl, double *xpls, double *fpls, double *gpls, int
		    *itrmcd, double *a, double *wrk)
{
	int a_dim1, a_offset, wrk_dim1, wrk_offset;
	extern int optdrv_();

	/* equivalence wrk(n,1) = udiag(n) */
	/* wrk(n,2) = g(n) */
	/* wrk(n,3) = p(n) */
	/* wrk(n,4) = sx(n) */
	/* wrk(n,5) = wrk0(n) */
	/* wrk(n,6) = wrk1(n) */
	/* wrk(n,7) = wrk2(n) */
	/* wrk(n,8) = wrk3(n) */

	wrk_dim1 = *nr;
	wrk_offset = wrk_dim1 + 1;
	wrk -= wrk_offset;
	a_dim1 = *nr;
	a_offset = a_dim1 + 1;
	a -= a_offset;
	--gpls;
	--xpls;
	--typsiz;
	--x;

	optdrv_(nr, n, &x[1], fcn, d1fcn, d2fcn, &typsiz[1], fscale, method, iexp,
	      msg, ndigit, itnlim, iagflg, iahflg, ipr, dlt, gradtl, stepmx,
		steptl, &xpls[1], fpls, &gpls[1], itrmcd, &a[a_offset], &wrk[
	   wrk_dim1 + 1], &wrk[(wrk_dim1 << 1) + 1], &wrk[wrk_dim1 * 3 + 1],
	  &wrk[(wrk_dim1 << 2) + 1], &wrk[wrk_dim1 * 5 + 1], &wrk[wrk_dim1 *
		 6 + 1], &wrk[wrk_dim1 * 7 + 1], &wrk[(wrk_dim1 << 3) + 1]);
	return 0;
}


/*
 *    subroutine optstp
 *
 *    unconstrained minimization stopping criteria
 *
 *    find whether the algorithm should terminate, due to any
 *    of the following:
 *    1) problem solved within user tolerance
 *    2) convergence within user tolerance
 *    3) iteration limit reached
 *    4) divergence or too restrictive maximum step (stepmx) suspected
 *
 *    parameters
 *
 *    n            --> dimension of problem
 *    xpls(n)      --> new iterate x[k]
 *    fpls         --> function value at new iterate f(xpls)
 *    gpls(n)      --> gradient at new iterate, g(xpls), or approximate
 *    x(n)         --> old iterate x[k-1]
 *    itncnt       --> current iteration k
 *    icscmx      <--> number consecutive steps .ge. stepmx
 *                     [retain value between successive calls]
 *    itrmcd      <--  termination code
 *    gradtl       --> tolerance at which relative gradient considered close
 *                     enough to zero to terminate algorithm
 *    steptl       --> relative step size at which successive iterates
 *                     considered close enough to terminate algorithm
 *    sx(n)        --> diagonal scaling matrix for x
 *    fscale       --> estimate of scale of objective function
 *    itnlim       --> maximum number of allowable iterations
 *    iretcd       --> return code
 *    mxtake       --> boolean flag indicating step of maximum length used
 *    ipr          --> device to which to send output
 *    msg          --> if msg includes a term 8, suppress output
 */


static
int 
optstp_(int *n, double *xpls, double *fpls,
	double *gpls, double *x, int *itncnt, int *icscmx,
	int *itrmcd, double *gradtl, double *steptl, double *
	sx, double *fscale, int *itnlim, int *iretcd, int *
	mxtake, int *ipr, int *msg)
{
	double d;
	double d__1, d__2, d__3, d__4;
	double relgrd;
	double relstp, rgx, rsx;
	int i;
	int i__1;
	int jtrmcd;

	--sx;
	--x;
	--gpls;
	--xpls;

	*itrmcd = 0;

	/* last global step failed to locate a point lower than x */

	if (*iretcd != 1) {
		goto L50;
	}
/*     if(iretcd.eq.1) */
/*     then */
	jtrmcd = 3;
	goto L600;
/*     endif */
L50:

	/* find direction in which relative gradient maximum. */
	/* check whether within tolerance */

	d__1 = abs(*fpls);
	d = max(d__1, *fscale);
	rgx = 0.;
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		d__3 = (d__2 = xpls[i], abs(d__2)), d__4 = 1. / sx[i];
		relgrd = (d__1 = gpls[i], abs(d__1)) * max(d__3, d__4) / d;
		rgx = max(rgx, relgrd);
	}
	jtrmcd = 1;
	if (rgx <= *gradtl) {
		goto L600;
	}
	if (*itncnt == 0) {
		return 0;
	}
	/* find direction in which relative stepsize maximum */
	/* check whether within tolerance. */

	rsx = 0.;
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		d__3 = (d__2 = xpls[i], abs(d__2)), d__4 = 1. / sx[i];
		relstp = (d__1 = xpls[i] - x[i], abs(d__1)) / max(d__3, d__4);
		rsx = max(rsx, relstp);
	}
	jtrmcd = 2;
	if (rsx <= *steptl) {
		goto L600;
	}
	/* check iteration limit */

	jtrmcd = 4;
	if (*itncnt >= *itnlim) {
		goto L600;
	}
	/* check number of consecutive steps \ stepmx */

	if (*mxtake) {
		goto L140;
	}
/*     if(.not.mxtake) */
/*     then */
	*icscmx = 0;
	return 0;
/*     else */
L140:
/* %      if (mod(msg/8,2) .eq. 0) write(ipr,900) */
	++(*icscmx);
	if (*icscmx < 5) {
		return 0;
	}
	jtrmcd = 5;
/*     endif */


	/* print termination code */

L600:
	*itrmcd = jtrmcd;
/* %    if (mod(msg/8,2) .eq. 0) go to(601,602,603,604,605), itrmcd */
/* %    go to 700 */
/* %601 write(ipr,901) */
/* %    go to 700 */
/* %602 write(ipr,902) */
/* %    go to 700 */
/* %603 write(ipr,903) */
/* %    go to 700 */
/* %604 write(ipr,904) */
/* %    go to 700 */
/* %605 write(ipr,905) */

	return 0;

/* %900 format(48h0optstp    step of maximum length (stepmx) taken) */
/* %901 format(43h0optstp    relative gradient close to zero./ */
/* %   +       48h optstp    current iterate is probably solution.) */
/* %902 format(48h0optstp    successive iterates within tolerance./ */
/* %   +       48h optstp    current iterate is probably solution.) */
/* %903 format(52h0optstp    last global step failed to locate a point, */
/* %   +       14h lower than x./ */
/* %   +       51h optstp    either x is an approximate local minimum, */
/* %   +       17h of the function,/ */
/* %   +       50h optstp    the function is too non-linear for this, */
/* %   +       11h algorithm,/ */
/* %   +       34h optstp    or steptl is too large.) */
/* %904 format(36h0optstp    iteration limit exceeded./ */
/* %   +       28h optstp    algorithm failed.) */
/* %905 format(39h0optstp    maximum step size exceeded 5, */
/* %   +       19h consecutive times./ */
/* %   +       50h optstp    either the function is unbounded below,/ */
/* %   +       47h optstp    becomes asymptotic to a finite value, */
/* %   +       30h from above in some direction,/ */
/* %   +       33h optstp    or stepmx is too small) */
}


/*
 *    subroutine qraux1
 *
 *    purpose
 *
 *    interchange rows i,i+1 of the upper hessenberg matrix r,
 *    columns i to n
 *
 *    parameters
 *
 *    nr           --> row dimension of matrix
 *    n            --> dimension of matrix
 *    r(n,n)      <--> upper hessenberg matrix
 *    i            --> index of row to interchange (i.lt.n)
 */

static
int 
qraux1_(int *nr, int *n, double *r, int *i)
{
	int r_dim1, r_offset, i__1;
	int j;
	double tmp;

	r_dim1 = *nr;
	r_offset = r_dim1 + 1;
	r -= r_offset;

	i__1 = *n;
	for (j = *i; j <= i__1; ++j) {
		tmp = r[*i + j * r_dim1];
		r[*i + j * r_dim1] = r[*i + 1 + j * r_dim1];
		r[*i + 1 + j * r_dim1] = tmp;
	}
	return 0;
}


/*
 *    subroutine qraux2
 *
 *    purpose
 *
 *    pre-multiply r by the jacobi rotation j(i,i+1,a,b)
 *
 *    parameters
 *
 *    nr           --> row dimension of matrix
 *    n            --> dimension of matrix
 *    r(n,n)      <--> upper hessenberg matrix
 *    i            --> index of row
 *    a            --> scalar
 *    b            --> scalar
 */

static
int 
qraux2_(int *nr, int *n, double *r, int *
	i, double *a, double *b)
{
	double c;
	double s, y, z, den;
	double sqrt();
	int j;
	int r_dim1, r_offset, i__1;

	r_dim1 = *nr;
	r_offset = r_dim1 + 1;
	r -= r_offset;

	den = sqrt(*a * *a + *b * *b);
	c = *a / den;
	s = *b / den;
	i__1 = *n;
	for (j = *i; j <= i__1; ++j) {
		y = r[*i + j * r_dim1];
		z = r[*i + 1 + j * r_dim1];
		r[*i + j * r_dim1] = c * y - s * z;
		r[*i + 1 + j * r_dim1] = s * y + c * z;
	}
	return 0;
}


/*
 *    subroutine qrupdt
 *
 *    purpose
 *
 *   find an orthogonal (n*n) matrix (q*) and an upper triangular (n*n)
 *    matrix (r*) such that (q*)(r*)=r+u(v+)
 *
 *    parameters
 *
 *    nr           --> row dimension of matrix
 *    n            --> dimension of problem
 *    a(n,n)      <--> on input:  contains r
 *                     on output: contains (r*)
 *    u(n)         --> vector
 *    v(n)         --> vector
 */

static
int 
qrupdt_(int *nr, int *n, double *a,
	double *u, double *v)
{
	double d__1;
	double sqrt();
	double t1, t2;
	extern int qraux1_();
	extern int qraux2_();
	int a_dim1, a_offset, i__1;
	int i, j, k;
	int ii;
	int km1;

	/* determine last non-zero in u(.) */

	a_dim1 = *nr;
	a_offset = a_dim1 + 1;
	a -= a_offset;
	--v;
	--u;

	k = *n;
L10:
	if (u[k] != 0. || k == 1) {
		goto L20;
	}
/*     if(u(k).eq.0.0d0 .and. k.gt.1) */
/*     then */
	--k;
	goto L10;
/*     endif */

	/* (k-1) jacobi rotations transform */
	/* r + u(v+) --> (r*) + (u(1)*e1)(v+) */
	/* which is upper hessenberg */

L20:
	if (k <= 1) {
		goto L40;
	}
	km1 = k - 1;
	i__1 = km1;
	for (ii = 1; ii <= i__1; ++ii) {
		i = km1 - ii + 1;
		if (u[i] != 0.) {
			goto L25;
		}
/*         if(u(i).eq.0.0d0) */
/*         then */
		qraux1_(nr, n, &a[a_offset], &i);
		u[i] = u[i + 1];
		goto L30;
/*         else */
L25:
		d__1 = -u[i + 1];
		qraux2_(nr, n, &a[a_offset], &i, &u[i], &d__1);
		u[i] = sqrt(u[i] * u[i] + u[i + 1] * u[i + 1]);
/*         endif */
L30:
		;
	}
/*     endif */

	/* r <-- r + (u(1)*e1)(v+) */

L40:
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
		a[j * a_dim1 + 1] += u[1] * v[j];
	}

	/* (k-1) jacobi rotations transform upper hessenberg r */
	/* to upper triangular (r*) */

	if (k <= 1) {
		goto L100;
	}
	km1 = k - 1;
	i__1 = km1;
	for (i = 1; i <= i__1; ++i) {
		if (a[i + i * a_dim1] != 0.) {
			goto L70;
		}
/*         if(a(i,i).eq.0.0d0) */
/*         then */
		qraux1_(nr, n, &a[a_offset], &i);
		goto L80;
/*         else */
L70:
		t1 = a[i + i * a_dim1];
		t2 = -a[i + 1 + i * a_dim1];
		qraux2_(nr, n, &a[a_offset], &i, &t1, &t2);
/*         endif */
L80:
		;
	}
/*     endif */
L100:
	return 0;
}


/*
 *    subroutine sclmul
 *
 *    purpose
 *
 *    multiply vector by scalar
 *    result vector may be operand vector
 *
 *    parameters
 *
 *    n            --> dimension of vectors
 *    s            --> scalar
 *    v(n)         --> operand vector
 *    z(n)        <--  result vector
 */

static
int 
sclmul_(int *n, double *s, double *v,
	double *z)
{
	int i__1;
	int i;

	--z;
	--v;

	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		z[i] = *s * v[i];
	}
	return 0;
}


/*
 *    subroutine secfac
 *
 *    purpose
 *
 *    update hessian by the bfgs factored method
 *
 *    parameters
 *
 *    nr           --> row dimension of matrix
 *    n            --> dimension of problem
 *    x(n)         --> old iterate, x[k-1]
 *    g(n)         --> gradient or approximate at old iterate
 *    a(n,n)      <--> on entry: cholesky decomposition of hessian in
 *                       lower part and diagonal.
 *                    on exit:  updated cholesky decomposition of hessian
 *                       in lower triangular part and diagonal
 *    xpls(n)      --> new iterate, x[k]
 *    gpls(n)      --> gradient or approximate at new iterate
 *    epsm         --> machine epsilon
 *    itncnt       --> iteration count
 *    rnf          --> relative noise in optimization function fcn
 *    iagflg       --> =1 if analytic gradient supplied, =0 itherwise
 *    noupdt      <--> boolean: no update yet
 *                     [retain value between successive calls]
 *    s(n)         --> workspace
 *    y(n)         --> workspace
 *    u(n)         --> workspace
 *    w(n)         --> workspace
 */

static
int 
secfac_(int *nr, int *n, double *x,
	double *g, double *a, double *xpls, double *gpls,
	double *epsm, int *itncnt, double *rnf, int *iagflg,
	int *noupdt, double *s, double *y, double *u,
	double *w)
{
	double alp, den1, den2;
	double d__1, d__2, d__3, d__4, d__5;
	double snorm2, reltol;
	double sqrt();
	double ynrm2;
	extern double F77_SYMBOL(ddot) (), F77_SYMBOL(dnrm2) ();
	extern int mvmltl_(), qrupdt_(), mvmltu_();
	int a_dim1, a_offset, i__1, i__2;
	int i, j;
	int im1;
	int skpupd;

	a_dim1 = *nr;
	a_offset = a_dim1 + 1;
	a -= a_offset;
	--w;
	--u;
	--y;
	--s;
	--gpls;
	--xpls;
	--g;
	--x;

	if (*itncnt == 1) {
		*noupdt = TRUE;
	}
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		s[i] = xpls[i] - x[i];
		y[i] = gpls[i] - g[i];
	}
	den1 = F77_SYMBOL(ddot) (n, &s[1], &c__1, &y[1], &c__1);
	snorm2 = F77_SYMBOL(dnrm2) (n, &s[1], &c__1);
	ynrm2 = F77_SYMBOL(dnrm2) (n, &y[1], &c__1);
	if (den1 < sqrt(*epsm) * snorm2 * ynrm2) {
		goto L110;
	}
/*     if(den1.ge.sqrt(epsm)*snorm2*ynrm2) */
/*     then */
	mvmltu_(nr, n, &a[a_offset], &s[1], &u[1]);
	den2 = F77_SYMBOL(ddot) (n, &u[1], &c__1, &u[1], &c__1);

	/* l <-- sqrt(den1/den2)*l */

	alp = sqrt(den1 / den2);
	if (!(*noupdt)) {
		goto L50;
	}
/*       if(noupdt) */
/*       then */
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
		u[j] = alp * u[j];
		i__2 = *n;
		for (i = j; i <= i__2; ++i) {
			a[i + j * a_dim1] = alp * a[i + j * a_dim1];
		}
	}
	*noupdt = FALSE;
	den2 = den1;
	alp = 1.;
/*       endif */
L50:
	skpupd = TRUE;

	/* w = l(l+)s = hs */

	mvmltl_(nr, n, &a[a_offset], &u[1], &w[1]);
	i = 1;
	if (*iagflg != 0) {
		goto L55;
	}
/*       if(iagflg.eq.0) */
/*       then */
	reltol = sqrt(*rnf);
	goto L60;
/*       else */
L55:
	reltol = *rnf;
/*       endif */
L60:
	if (i > *n || !skpupd) {
		goto L70;
	}
/*       if(i.le.n .and. skpupd) */
/*       then */
	d__4 = (d__2 = g[i], abs(d__2)), d__5 = (d__3 = gpls[i], abs(d__3));
	if ((d__1 = y[i] - w[i], abs(d__1)) < reltol * max(d__4, d__5)) {
		goto L65;
	}
/*         if(abs(y(i)-w(i)) .ge. reltol*dmax1(abs(g(i)),abs(gpls(i)))) */
/*         then */
	skpupd = FALSE;
	goto L60;
/*         else */
L65:
	++i;
	goto L60;
/*         endif */
/*       endif */
L70:
	if (skpupd) {
		goto L110;
	}
/*       if(.not.skpupd) */
/*       then */

/*         w=y-alp*l(l+)s */

	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		w[i] = y[i] - alp * w[i];
	}

/*         alp=1/sqrt(den1*den2) */

	alp /= den1;

/*         u=(l+)/sqrt(den1*den2) = (l+)s/sqrt((y+)s * (s+)l(l+)s) */

	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		u[i] = alp * u[i];
	}

/*         copy l into upper triangular part.  zero l. */

	if (*n == 1) {
		goto L93;
	}
	i__1 = *n;
	for (i = 2; i <= i__1; ++i) {
		im1 = i - 1;
		i__2 = im1;
		for (j = 1; j <= i__2; ++j) {
			a[j + i * a_dim1] = a[i + j * a_dim1];
			a[i + j * a_dim1] = 0.;
		}
	}

/*         find q, (l+) such that  q(l+) = (l+) + u(w+) */

L93:
	qrupdt_(nr, n, &a[a_offset], &u[1], &w[1]);

/*         upper triangular part and diagonal of a now contain updated */
/*         cholesky decomposition of hessian.  copy back to lower */
/*         triangular part. */

	if (*n == 1) {
		goto L110;
	}
	i__1 = *n;
	for (i = 2; i <= i__1; ++i) {
		im1 = i - 1;
		i__2 = im1;
		for (j = 1; j <= i__2; ++j) {
			a[i + j * a_dim1] = a[j + i * a_dim1];
		}
	}
/*       endif */
/*     endif */
L110:
	return 0;
}


/*
 *    subroutine secunf
 *
 *    purpose
 *
 *    update hessian by the bfgs unfactored method
 *
 *    parameters
 *
 *    nr           --> row dimension of matrix
 *    n            --> dimension of problem
 *    x(n)         --> old iterate, x[k-1]
 *    g(n)         --> gradient or approximate at old iterate
 *    a(n,n)      <--> on entry: approximate hessian at old iterate
 *                       in upper triangular part (and udiag)
 *                     on exit:  updated approx hessian at new iterate
 *                       in lower triangular part and diagonal
 *                     [lower triangular part of symmetric matrix]
 *    udiag        --> on entry: diagonal of hessian
 *    xpls(n)      --> new iterate, x[k]
 *    gpls(n)      --> gradient or approximate at new iterate
 *    epsm         --> machine epsilon
 *    itncnt       --> iteration count
 *    rnf          --> relative noise in optimization function fcn
 *    iagflg       --> =1 if analytic gradient supplied, =0 otherwise
 *    noupdt      <--> boolean: no update yet
 *                     [retain value between successive calls]
 *    s(n)         --> workspace
 *    y(n)         --> workspace
 *    t(n)         --> workspace
 */

static
int 
secunf_(int *nr, int *n, double *x,
	double *g, double *a, double *udiag, double *xpls,
	double *gpls, double *epsm, int *itncnt, double *rnf,
	int *iagflg, int *noupdt, double *s, double *y,
	double *t)
{
	double d__1, d__2, d__3, d__4;
	double gam, tol, den1, den2;
	double snorm2;
	double sqrt();
	double ynrm2;
	extern double F77_SYMBOL(ddot) (), F77_SYMBOL(dnrm2) ();
	extern int mvmlts_();
	int a_dim1, a_offset, i__1, i__2;
	int i, j;
	int jp1;
	int skpupd;


	/* copy hessian in upper triangular part and udiag to */
	/* lower triangular part and diagonal */

	a_dim1 = *nr;
	a_offset = a_dim1 + 1;
	a -= a_offset;
	--t;
	--y;
	--s;
	--gpls;
	--xpls;
	--udiag;
	--g;
	--x;

	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
		a[j + j * a_dim1] = udiag[j];
		if (j == *n) {
			goto L5;
		}
		jp1 = j + 1;
		i__2 = *n;
		for (i = jp1; i <= i__2; ++i) {
			a[i + j * a_dim1] = a[j + i * a_dim1];
		}
L5:
		;
	}

	if (*itncnt == 1) {
		*noupdt = TRUE;
	}
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		s[i] = xpls[i] - x[i];
		y[i] = gpls[i] - g[i];
	}
	den1 = F77_SYMBOL(ddot) (n, &s[1], &c__1, &y[1], &c__1);
	snorm2 = F77_SYMBOL(dnrm2) (n, &s[1], &c__1);
	ynrm2 = F77_SYMBOL(dnrm2) (n, &y[1], &c__1);
	if (den1 < sqrt(*epsm) * snorm2 * ynrm2) {
		goto L100;
	}
/*     if(den1.ge.sqrt(epsm)*snorm2*ynrm2) */
/*     then */
	mvmlts_(nr, n, &a[a_offset], &s[1], &t[1]);
	den2 = F77_SYMBOL(ddot) (n, &s[1], &c__1, &t[1], &c__1);
	if (!(*noupdt)) {
		goto L50;
	}
/*       if(noupdt) */
/*       then */

	/* h <-- [(s+)y/(s+)hs]h */

	gam = den1 / den2;
	den2 = gam * den2;
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
		t[j] = gam * t[j];
		i__2 = *n;
		for (i = j; i <= i__2; ++i) {
			a[i + j * a_dim1] = gam * a[i + j * a_dim1];
		}
	}
	*noupdt = FALSE;
/*       endif */
L50:
	skpupd = TRUE;

	/* check update condition on row i */

	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		d__3 = (d__1 = g[i], abs(d__1)), d__4 = (d__2 = gpls[i], abs(d__2));
		tol = *rnf * max(d__3, d__4);
		if (*iagflg == 0) {
			tol /= sqrt(*rnf);
		}
		if ((d__1 = y[i] - t[i], abs(d__1)) < tol) {
			goto L60;
		}
/*         if(abs(y(i)-t(i)).ge.tol) */
/*         then */
		skpupd = FALSE;
		goto L70;
/*         endif */
L60:
		;
	}
L70:
	if (skpupd) {
		goto L100;
	}
/*       if(.not.skpupd) */
/*       then */

/*         bfgs update */

	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
		i__2 = *n;
		for (i = j; i <= i__2; ++i) {
			a[i + j * a_dim1] = a[i + j * a_dim1] + y[i] * y[j] / den1 - t[i]
				* t[j] / den2;
		}
	}
/*       endif */
/*     endif */
L100:
	return 0;
}


/*
 *    subroutine sndofd
 *
 *    purpose
 *
 *    find second order forward finite difference approximation "a"
 *   to the second derivative (hessian) of the function defined by the subp
 *
 *    "fcn" evaluated at the new iterate "xpls"
 *
 *    for optimization use this routine to estimate
 *    1) the second derivative (hessian) of the optimization function
 *       if no analytical user function has been supplied for either
 *       the gradient or the hessian and if the optimization function
 *       "fcn" is inexpensive to evaluate.
 *
 *    parameters
 *
 *    nr           --> row dimension of matrix
 *    n            --> dimension of problem
 *    xpls(n)      --> new iterate:   x[k]
 *    fcn          --> name of subroutine to evaluate function
 *    fpls         --> function value at new iterate, f(xpls)
 *    a(n,n)      <--  finite difference approximation to hessian
 *                     only lower triangular matrix and diagonal
 *                     are returned
 *    sx(n)        --> diagonal scaling matrix for x
 *    rnoise       --> relative noise in fname [f(x)]
 *    stepsz(n)    --> workspace (stepsize in i-th component direction)
 *    anbr(n)      --> workspace (neighbor in i-th direction)
 */

static
int 
sndofd_(int *nr, int *n, double *xpls, /* Subroutine */ int (*fcn) (), double *fpls, double *a,
	double *sx, double *rnoise, double *stepsz, double *
	anbr)
{
	double d__1, d__2, d__3;
	double fhat;
	double ov3;
	double xtmpi, xtmpj;
	int a_dim1, a_offset, i__1, i__2;
	int i, j;
	int ip1;

	a_dim1 = *nr;
	a_offset = a_dim1 + 1;
	a -= a_offset;
	--anbr;
	--stepsz;
	--sx;
	--xpls;

	/* find i-th stepsize and evaluate neighbor in direction */
	/* of i-th unit vector. */

	ov3 = .33333333333333331;
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		d__2 = (d__1 = xpls[i], abs(d__1)), d__3 = 1. / sx[i];
		stepsz[i] = POW_DD(rnoise, &ov3) * max(d__2, d__3);
		xtmpi = xpls[i];
		xpls[i] = xtmpi + stepsz[i];
		(*fcn) (n, &xpls[1], &anbr[i]);
		xpls[i] = xtmpi;
	}

	/* calculate column i of a */

	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		xtmpi = xpls[i];
		xpls[i] = xtmpi + stepsz[i] * 2.;
		(*fcn) (n, &xpls[1], &fhat);
		a[i + i * a_dim1] = (*fpls - anbr[i] + (fhat - anbr[i])) / (stepsz[i]
							       * stepsz[i]);

		/* calculate sub-diagonal elements of column */

		if (i == *n) {
			goto L25;
		}
		xpls[i] = xtmpi + stepsz[i];
		ip1 = i + 1;
		i__2 = *n;
		for (j = ip1; j <= i__2; ++j) {
			xtmpj = xpls[j];
			xpls[j] = xtmpj + stepsz[j];
			(*fcn) (n, &xpls[1], &fhat);
			a[j + i * a_dim1] = (*fpls - anbr[i] + (fhat - anbr[j])) / (
						     stepsz[i] * stepsz[j]);
			xpls[j] = xtmpj;
		}
L25:
		xpls[i] = xtmpi;
	}
	return 0;
}


/*
 *    subroutine tregup
 *
 *    purpose
 *
 *   decide whether to accept xpls=x+sc as the next iterate and update the
 *    trust region dlt.
 *
 *    parameters
 *
 *    nr           --> row dimension of matrix
 *    n            --> dimension of problem
 *    x(n)         --> old iterate x[k-1]
 *    f            --> function value at old iterate, f(x)
 *    g(n)         --> gradient at old iterate, g(x), or approximate
 *    a(n,n)       --> cholesky decomposition of hessian in
 *                     lower triangular part and diagonal.
 *                     hessian or approx in upper triangular part
 *    fcn          --> name of subroutine to evaluate function
 *    sc(n)        --> current step
 *    sx(n)        --> diagonal scaling matrix for x
 *    nwtake       --> boolean, =.true. if newton step taken
 *    stepmx       --> maximum allowable step size
 *    steptl       --> relative step size at which successive iterates
 *                     considered close enough to terminate algorithm
 *    dlt         <--> trust region radius
 *    iretcd      <--> return code
 *                       =0 xpls accepted as next iterate;
 *                          dlt trust region for next iteration.
 *                      =1 xpls unsatisfactory but accepted as next iterate
 *                          because xpls-x .lt. smallest allowable
 *                          step length.
 *                      =2 f(xpls) too large.  continue current iteration
 *                          with new reduced dlt.
 *                      =3 f(xpls) sufficiently small, but quadratic model
 *                         predicts f(xpls) sufficiently well to continue
 *                          current iteration with new doubled dlt.
 *    xplsp(n)    <--> workspace [value needs to be retained between
 *                     succesive calls of k-th global step]
 *    fplsp       <--> [retain value between successive calls]
 *    xpls(n)     <--  new iterate x[k]
 *    fpls        <--  function value at new iterate, f(xpls)
 *    mxtake      <--  boolean flag indicating step of maximum length used
 *    ipr          --> device to which to send output
 *    method       --> algorithm to use to solve minimization problem
 *                       =1 line search
 *                       =2 double dogleg
 *                       =3 more-hebdon
 *    udiag(n)     --> diagonal of hessian in a(.,.)
 */

static
int 
tregup_(int *nr, int *n, double *x,
	double *f, double *g, double *a, int (*
		      fcn) (), double *sc, double *sx, int *nwtake, double *
	stepmx, double *steptl, double *dlt, int *iretcd,
	double *xplsp, double *fplsp, double *xpls, double *
	fpls, int *mxtake, int *ipr, int *method, double *
	udiag)
{
	double d__1, d__2, d__3, d__4, d__5, d__6;
	double dltf;
	double dltfp, dltmp;
	double rln, slp;
	double temp;
	extern double F77_SYMBOL(ddot) ();
	int a_dim1, a_offset, i__1, i__2;
	int i, j;
	int ip1;

	a_dim1 = *nr;
	a_offset = a_dim1 + 1;
	a -= a_offset;
	--udiag;
	--xpls;
	--xplsp;
	--sx;
	--sc;
	--g;
	--x;

	*ipr = *ipr;
	*mxtake = FALSE;
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		xpls[i] = x[i] + sc[i];
	}
	(*fcn) (n, &xpls[1], fpls);
	dltf = *fpls - *f;
	slp = F77_SYMBOL(ddot) (n, &g[1], &c__1, &sc[1], &c__1);

	/* next statement added for case of compilers which do not optimize */
	/* evaluation of next "if" statement (in which case fplsp could be */
	/* undefined). */

	if (*iretcd == 4) {
		*fplsp = 0.;
	}
/* $    write(ipr,961) iretcd,fpls,fplsp,dltf,slp */
	if (*iretcd != 3 || *fpls < *fplsp && dltf <= slp * 1e-4) {
		goto L130;
	}
/*     if(iretcd.eq.3 .and. (fpls.ge.fplsp .or. dltf.gt. 1.d-4*slp)) */
/*     then */

	/* reset xpls to xplsp and terminate global step */

	*iretcd = 0;
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		xpls[i] = xplsp[i];
	}
	*fpls = *fplsp;
	*dlt *= .5;
/* $      write(ipr,951) */
	goto L230;
/*     else */

/*       fpls too large */

L130:
	if (dltf <= slp * 1e-4) {
		goto L170;
	}
/*       if(dltf.gt. 1.d-4*slp) */
/*       then */
/* $        write(ipr,952) */
	rln = 0.;
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		d__5 = (d__2 = xpls[i], abs(d__2)), d__6 = 1. / sx[i];
		d__3 = rln, d__4 = (d__1 = sc[i], abs(d__1)) / max(d__5, d__6);
		rln = max(d__3, d__4);
	}
/* $        write(ipr,962) rln */
	if (rln >= *steptl) {
		goto L150;
	}
/*         if(rln.lt.steptl) */
/*         then */

/* cannot find satisfactory xpls sufficiently distinct from x */

	*iretcd = 1;
/* $          write(ipr,954) */
	goto L230;
/*         else */

/*           reduce trust region and continue global step */

L150:
	*iretcd = 2;
	dltmp = -slp * *dlt / ((dltf - slp) * 2.);
/* $          write(ipr,963) dltmp */
	if (dltmp >= *dlt * .1) {
		goto L155;
	}
/*           if(dltmp.lt. .1*dlt) */
/*           then */
	*dlt *= .1;
	goto L160;
/*           else */
L155:
	*dlt = dltmp;
/*           endif */
L160:
/* $          write(ipr,955) */
	goto L230;
/*         endif */
/*       else */

/*         fpls sufficiently small */

L170:
/* $        write(ipr,958) */
	dltfp = 0.;
	if (*method == 3) {
		goto L180;
	}
/*         if (method .eq. 2) */
/*         then */

	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		temp = 0.;
		i__2 = *n;
		for (j = i; j <= i__2; ++j) {
			temp += a[j + i * a_dim1] * sc[j];
		}
		dltfp += temp * temp;
	}
	goto L190;

/*         else */

L180:
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		dltfp += udiag[i] * sc[i] * sc[i];
		if (i == *n) {
			goto L187;
		}
		temp = 0.;
		ip1 = i + 1;
		i__2 = *n;
		for (j = ip1; j <= i__2; ++j) {
			temp += a[i + j * a_dim1] * sc[i] * sc[j];
		}
		dltfp += temp * 2.;
L187:
		;
	}

/*         end if */

L190:
	dltfp = slp + dltfp / 2.;
/* $        write(ipr,964) dltfp,nwtake */
	if (*iretcd == 2 || (d__1 = dltfp - dltf, abs(d__1)) > abs(dltf) * .1 ||
	    *nwtake || *dlt > *stepmx * .99) {
		goto L210;
	}
/*         if(iretcd.ne.2 .and. (abs(dltfp-dltf) .le. .1*abs(dltf)) */
/*    +         .and. (.not.nwtake) .and. (dlt.le. .99*stepmx)) */
/*         then */

	/* double trust region and continue global step */

	*iretcd = 3;
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		xplsp[i] = xpls[i];
	}
	*fplsp = *fpls;
	d__1 = *dlt * 2.;
	*dlt = min(d__1, *stepmx);
/* $          write(ipr,959) */
	goto L230;
/*         else */

	/* accept xpls as next iterate.  choose new trust region. */

L210:
/* $          write(ipr,960) */
	*iretcd = 0;
	if (*dlt > *stepmx * .99) {
		*mxtake = TRUE;
	}
	if (dltf < dltfp * .1) {
		goto L220;
	}
/*           if(dltf.ge. .1*dltfp) */
/*           then */

	/* decrease trust region for next iteration */

	*dlt *= .5;
	goto L230;
/*           else */

	/* check whether to increase trust region for next iteration */

L220:
	if (dltf <= dltfp * .75) {
		d__1 = *dlt * 2.;
		*dlt = min(d__1, *stepmx);
	}
/*           endif */
/*         endif */
/*       endif */
/*     endif */
L230:
/* $    write(ipr,953) */
/* $    write(ipr,956) iretcd,mxtake,dlt,fpls */
/* $    write(ipr,957) */
/* $    write(ipr,965) (xpls(i),i=1,n) */
	return 0;

/* %951 format(55h tregup    reset xpls to xplsp. termination global step) */
/* %952 format(26h tregup    fpls too large.) */
/* %953 format(38h0tregup    values after call to tregup) */
/* %954 format(54h tregup    cannot find satisfactory xpls distinct from, */
/* %   +       27h x.  terminate global step.) */
/* %955 format(53h tregup    reduce trust region. continue global step.) */
/* %956 format(21h tregup       iretcd=,i3/ */
/* %   +       21h tregup       mxtake=,l1/ */
/* %   +       21h tregup       dlt   =,e20.13/ */
/* %   +       21h tregup       fpls  =,e20.13) */
/* %957 format(32h tregup       new iterate (xpls)) */
/* %958 format(35h tregup    fpls sufficiently small.) */
/* %959 format(54h tregup    double trust region.  continue global step.) */
/* %960 format(50h tregup    accept xpls as new iterate.  choose new, */
/* %   +       38h trust region.  terminate global step.) */
/* %961 format(18h tregup    iretcd=,i5/ */
/* %   +       18h tregup    fpls  =,e20.13/ */
/* %   +       18h tregup    fplsp =,e20.13/ */
/* %   +       18h tregup    dltf  =,e20.13/ */
/* %   +       18h tregup    slp   =,e20.13) */
/* %962 format(18h tregup    rln   =,e20.13) */
/* %963 format(18h tregup    dltmp =,e20.13) */
/* %964 format(18h tregup    dltfp =,e20.13/ */
/* %   +       18h tregup    nwtake=,l1) */
/* %965 format(14h tregup       ,5(e20.13,3x)) */
}
