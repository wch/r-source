/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998, 1999   Robert Gentleman, Ross Ihaka
 *                             and the R Development Core Team
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 * Application Routines, typically implemented in  ../appl/
 * ----------------------------------------------  ========
 */

#ifndef APPLIC_H_
#define APPLIC_H_

#include "Rconfig.h"		/* F77... */

void approx(double *, double *, int *, double *, int *,
	    int *, double *, double *, double *);

void bakslv(double *, int *, int *,
	    double *, int *, int *,
	    double *, int *, int *);

void bincode (double *x, int *n, double *breaks, int *nb,
	      int *code, int *right, int *include_border, int *naok);
void bincount(double *x, int *n, double *breaks, int *nb, int *count,
	      int *right, int *include_border, int *naok);
/*unused*/
void bincode2(double *, int *, double *, int *, int *, int *, int *);

/* chisqsim.c: */
void chisqsim(long *nrow, long *ncol, long *nrowt, long *ncolt, long *n,
	      long *b, double *expected, long *observed, double *fact,
	      long *jwork, double *results);

/* chull.c -- comments in the source */
void chull(int *n, double *x, int *m, int *in,
	   int *ia, int *ib,
	   int *ih, int *nh, int *il);

/* cpoly.c : */
int F77_SYMBOL(cpoly)(double *opr, double *opi, int *degree,
		      double *zeror, double *zeroi, int *fail);
void polyev(int *,
	    double *, double *, double *, double *,
	    double *, double *, double *, double *);
double errev(int *, double *, double *, double *,
	     double *, double *, double *);
double cauchy(int *, double *, double *);
void scale(int *, double *, double *, double *, double *, double *, double *);
void cdivid(double *, double *,double *, double *, double *, double *);
double cmod(double *, double *);

void cumsum(double *, int *, double *, double *);

/* distance.c */
double euclidean(double *, int, int, int, int);
double maximum(double *, int, int, int, int);
double manhattan(double *, int, int, int, int);
double canberra(double *, int, int, int, int);
double dist_binary(double *, int, int, int, int);
#define EUCLIDEAN 1
#define MAXIMUM	  2
#define MANHATTAN 3
#define CANBERRA  4
#define BINARY	  5
void distance(double *, int *, int *, double *, int *, int *);

/* eigen.c */
int F77_SYMBOL(cg)(int *nm, int *n, double *ar, double *ai,
		   double *wr, double *wi, int *matz, double *zr, double *zi,
		   double *fv1, double *fv2, double *fv3, int *ierr);
int F77_SYMBOL(ch)(int *nm, int *n, double *ar, double *ai,
		   double *w, int *matz, double *zr, double *zi,
		   double *fv1, double *fv2, double *fm1, int *ierr);
int F77_SYMBOL(rg)(int *nm, int *n, double *a, double *wr, double *wi,
		   int *matz, double *z, int *iv1, double *fv1, int *ierr);
int F77_SYMBOL(rs)(int *nm, int *n, double *a, double *w,
		   int *matz, double *z, double *fv1, double *fv2, int *ierr);

/* fft.c */
/* NOTE:  The following functions use GLOBAL (static) variables !!
 * ----   some of R-core think that this should be changed,
 *        which will INEVITABLY extend the argument lists ...!
 *-- i.e. don't export these yet!
 */
#ifdef NOT_YET
void fft_factor(int n, int *pmaxf, int *pmaxp);

int fft_work(double *a, double *b, int nseg, int n, int nspn, int isn,
	     double *work, int *iwork);
/* returns 1 for success,  0 otherwise */
#endif

/* fortran.c   is covered by ./Fortran.h */

/* lowess.c */
void clowess(double *x, double *y, int n,
	     double f, int nsteps, double delta,
	     double *ys, double *rw, double *res);
void lowess(double *x, double *y, int *n,
	    double *f, int *nsteps, double *delta,
	    double *ys, double *rw, double *res);

/* machar.c */
void machar(int *ibeta, int *it, int *irnd, int *ngrd, int *machep, int *negep,
	int *iexp, int *minexp, int *maxexp, double *eps,
	double *epsneg, double *xmin, double *xmax);

/* massdist.c */
/* ...............*/

/* pretty.c */
double pretty0(double *lo, double *up, int *ndiv, int min_n,
	       double shrink_sml, double high_u_fact[],
	       int eps_correction, int return_bounds);
void pretty(double *lo, double *up, int *ndiv, int *min_n,
	    double *shrink_sml, double *high_u_fact, int *eps_correction);


/* pythag.c */
double pythag(double, double);

/* splines.c */
void spline_coef(int *method, int *n, double *x, double *y,
		 double *b, double *c, double *d, double *e);
void spline_eval(int *method, int *nu, double *u, double *v,
		 int *n, double *x, double *y,
		 double *b, double *c, double *d);

/* stem.c */
int stemleaf(double *x, int *n, double *scale, int *width, double *atom);

/* ...............*/

/* strsignif.c	(??) */
/* ...............*/

/* tabulate.c */
void tabulate(int *x, int *n, int *ans);

/* ../main/optimize.c */
int F77_SYMBOL(result)(int *nr, int *n, double *x, double *f, double *g,
		       double *a, double *p, int *itncnt, int *iflg, int *ipr);


/* ALL ../appl/<foobar>.f	[semi-automatically by
 *				 f2c -A -P *.f; cat *.P > all.h	 and editing]
 */
typedef double (*D_fp)();
typedef /* Subroutine */ int (*S_fp)();

/* ../appl/blas.f */
double F77_SYMBOL(dasum)(int *n, double *dx, int *incx);
int F77_SYMBOL(daxpy)(int *n, double *da, double *dx, int *incx,
		      double *dy, int *incy);
int F77_SYMBOL(dcopy)(int *n, double *dx, int *incx, double *dy, int *incy);
double F77_SYMBOL(ddot)(int *n, double *dx, int *incx, double *dy, int *incy);
double F77_SYMBOL(dmach)(int *job);
double F77_SYMBOL(dnrm2)(int *n, double *x, int *incx);
int F77_SYMBOL(drot)(int *n, double *dx, int *incx, double *dy, int *incy,
		     double *c__, double *s);
int F77_SYMBOL(drotg)(double *da, double *db, double *c__, double *s);
int F77_SYMBOL(dscal)(int *n, double *da, double *dx, int *incx);
int F77_SYMBOL(dswap)(int *n, double *dx, int *incx, double *dy, int *incy);
int F77_SYMBOL(idamax)(int *n, double *dx, int *incx);
/*----*/
int F77_SYMBOL(ch2inv)(double *x, int *ldx, int *n, double *v, int *info);
int F77_SYMBOL(chol)(double *a, int *lda, int *n, double *v, int *info);

int F77_SYMBOL(dpoco)(double *a, int *lda, int *n, double *rcond,
		      double *z__, int *info);
int F77_SYMBOL(dpodi)(double *a, int *lda, int *n, double *det, int *job);
int F77_SYMBOL(dpofa)(double *a, int *lda, int *n, int *info);
int F77_SYMBOL(dposl)(double *a, int *lda, int *n, double *b);
/* find qr decomposition, dqrdc2() is basis of R's qr() */
int F77_SYMBOL(dqrdc)(double *x, int *ldx, int *n, int *p,
		      double *qraux, int *jpvt, double *work, int *job);
int F77_SYMBOL(dqrdc2)(double *x, int *ldx, int *n, int *p,
		       double *tol, int *rank,
		       double *qraux, int *pivot, double *work);
int F77_SYMBOL(dqrls)(double *x, int *n, int *p, double *y, int *ny,
		      double *tol, double *b, double *rsd,
		      double *qty, int *k,
		      int *jpvt, double *qraux, double *work);
/* solve for QR coefficients */
int F77_SYMBOL(dqrsl)(double *x, int *ldx, int *n, int *k,
		      double *qraux, double *y,
		      double *qy, double *qty, double *b,
		      double *rsd, double *xb, int *job, int *info);

/* ../appl/dqrutl.f */
int F77_SYMBOL(dqrqty)(double *x, int *n, int *k, double *qraux,
		       double *y, int *ny, double *qty);
int F77_SYMBOL(dqrqy)(double *x, int *n, int *k, double *qraux,
		      double *y, int *ny, double *qy);
int F77_SYMBOL(dqrcf)(double *x, int *n, int *k, double *qraux,
		      double *y, int *ny, double *b, int *info);
int F77_SYMBOL(dqrrsd)(double *x, int *n, int *k, double *qraux,
		       double *y, int *ny, double *rsd);
int F77_SYMBOL(dqrxb)(double *x, int *n, int *k, double *qraux,
		      double *y, int *ny, double *xb);
/*---*/

int F77_SYMBOL(dsvdc)(double *x, int *ldx, int *n, int *p,
		      double *s, double *e,
		      double *u, int *ldu, double *v, int *ldv,
		      double *work, int *job, int *info);
int F77_SYMBOL(dtrco)(double *t, int *ldt, int *n, double *rcond,
		      double *z__, int *job);
int F77_SYMBOL(dtrsl)(double *t, int *ldt, int *n, double *b, int *job,
		      int *info);

double F77_SYMBOL(fmin)(double *ax, double *bx, D_fp f, double *tol);
int F77_SYMBOL(lminfl)(double *x, int *ldx, int *n, int *k, double *qraux,
		       double *resid, double *hat, double *coef, double *sigma);
/* Only exported headers from ../appl/uncmin.f : */
int F77_SYMBOL(fdhess)(int *n, double *x, double *fval,
		       S_fp fun, double *h__, int *nfd,
		       double *step, double *f, int *ndigit, double *typx);
int F77_SYMBOL(optif9)(int *nr, int *n, double *x,
		       S_fp fcn, S_fp d1fcn, S_fp d2fcn, double *typsiz,
		       double *fscale, int *method, int *iexp, int *msg,
		       int *ndigit, int *itnlim, int *iagflg, int *iahflg,
		       int *ipr, double *dlt, double *gradtl,
		       double *stepmx, double *steptl,
		       double *xpls, double *fpls, double *gpls,
		       int *itrmcd, double *a, double *wrk, int *itncnt);

double zeroin(double ax, double bx, double (*f)(double, void *), void *info,
	      double *tol, int *maxit);

#endif
