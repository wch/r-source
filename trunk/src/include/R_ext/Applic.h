/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2005   Robert Gentleman, Ross Ihaka
 *                             and the R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 *
 *
 * Application Routines, typically implemented in  ../appl/
 * ----------------------------------------------  ========
 */

/* This header file contains routines which are in the R API and ones which
   are not.

   Those which are not can be used only at the user's risk and may change
   or disappear in a future release of R.
*/


#ifndef R_APPLIC_H_
#define R_APPLIC_H_

#include <R_ext/Boolean.h>
#include <R_ext/RS.h>		/* F77_... */
#include <R_ext/BLAS.h>

#ifdef  __cplusplus
extern "C" {
#endif

/* Entry points in the R API */

/* appl/integrate.c */
typedef void integr_fn(double *x, int n, void *ex);
/* vectorizing function   f(x[1:n], ...) -> x[]  {overwriting x[]}. */

void Rdqags(integr_fn f, void *ex, double *a, double *b,
	    double *epsabs, double *epsrel,
	    double *result, double *abserr, int *neval, int *ier,
	    int *limit, int *lenw, int *last, int *iwork, double *work);

void Rdqagi(integr_fn f, void *ex, double *bound, int *inf,
	    double *epsabs, double *epsrel,
	    double *result, double *abserr, int *neval, int *ier,
	    int *limit, int *lenw, int *last,
	    int *iwork, double *work);

/* appl/rcont.c */
void rcont2(int *nrow, int *ncol, int *nrowt, int *ncolt, int *ntotal,
	    double *fact, int *jwork, int *matrix);

/* main/optim.c */
typedef double optimfn(int, double *, void *);
typedef void optimgr(int, double *, double *, void *);

void vmmin(int n, double *b, double *Fmin,
	   optimfn fn, optimgr gr, int maxit, int trace,
	   int *mask, double abstol, double reltol, int nREPORT,
	   void *ex, int *fncount, int *grcount, int *fail);
void nmmin(int n, double *Bvec, double *X, double *Fmin, optimfn fn,
	   int *fail, double abstol, double intol, void *ex,
	   double alpha, double bet, double gamm, int trace,
	   int *fncount, int maxit);
void cgmin(int n, double *Bvec, double *X, double *Fmin,
	   optimfn fn, optimgr gr,
	   int *fail, double abstol, double intol, void *ex,
	   int type, int trace, int *fncount, int *grcount, int maxit);
void lbfgsb(int n, int m, double *x, double *l, double *u, int *nbd,
	    double *Fmin, optimfn fn, optimgr gr, int *fail, void *ex,
	    double factr, double pgtol, int *fncount, int *grcount,
	    int maxit, char *msg, int trace, int nREPORT);
void samin(int n, double *pb, double *yb, optimfn fn, int maxit,
	   int tmax, double ti, int trace, void *ex);



/* Entry points NOT in the R API */

/* appl/approx.c */
void R_approx(double *, double *, int *, double *, int *,
	      int *, double *, double *, double *);

/* appl/bakslv.c */
void bakslv(double *, int *, int *,
	    double *, int *, int *,
	    double *, int *, int *);

/* appl/binning.c */
void bincode (double *x, int *n, double *breaks, int *nb,
	      int *code, int *right, int *include_border, int *naok);
void bincount(double *x, int *n, double *breaks, int *nb, int *count,
	      int *right, int *include_border, int *naok);

/* appl/ch2inv.f */
void F77_NAME(ch2inv)(double *x, int *ldx, int *n, double *v, int *info);

/* appl/chol.f */
void F77_NAME(chol)(double *a, int *lda, int *n, double *v, int *info);

/* appl/cpoly.c : */
void R_cpolyroot(double *opr, double *opi, int *degree,
		 double *zeror, double *zeroi, Rboolean *fail);
/* More `Complex Polynomial Utilities' could be exported:

   polyev(...)
   errev(...)
   cpoly_cauchy(...)
   cpoly_scale(...)
   cdivid(...)
*/


/* appl/cumsum.c : */
void R_cumsum(double *, int *, double *, double *);

/* appl/eigen.f */
int F77_NAME(cg)(int *nm, int *n, double *ar, double *ai,
		 double *wr, double *wi, int *matz, double *zr, double *zi,
		 double *fv1, double *fv2, double *fv3, int *ierr);
int F77_NAME(ch)(int *nm, int *n, double *ar, double *ai,
		 double *w, int *matz, double *zr, double *zi,
		 double *fv1, double *fv2, double *fm1, int *ierr);
int F77_NAME(rg)(int *nm, int *n, double *a, double *wr, double *wi,
		 int *matz, double *z, int *iv1, double *fv1, int *ierr);
int F77_NAME(rs)(int *nm, int *n, double *a, double *w,
		 int *matz, double *z, double *fv1, double *fv2, int *ierr);

/* appl/fft.c */
/* NOTE:  The following functions use GLOBAL (static) variables !!
 * ----   some of R-core think that this should be changed,
 *        which will INEVITABLY extend the argument lists ...!
 */
void fft_factor(int n, int *pmaxf, int *pmaxp);

Rboolean fft_work(double *a, double *b, int nseg, int n, int nspn,
/* TRUE: success */ int isn, double *work, int *iwork);

/* appl/fmin.c */
double Brent_fmin(double ax, double bx, double (*f)(double, void *),
		  void *info, double tol);

/* appl/interv.c: also in Utils.h */
int F77_SUB(interv)(double *xt, int *n, double *x,
		    Rboolean *rightmost_closed, Rboolean *all_inside,
		    int *ilo, int *mflag);
void find_interv_vec(double *xt, int *n, double *x, int *nx,
		     int *rightmost_closed, int *all_inside, int *indx);
int findInterval(double *xt, int n, double x,
		 Rboolean rightmost_closed,  Rboolean all_inside, int ilo,
		 int *mflag);

/* appl/lbfgsb.c */
void setulb(int n, int m, double *x, double *l, double *u, int *nbd,
	    double *f, double *g, double factr, double *pgtol,
	    double *wa, int * iwa, char *task, int iprint,
	    int *lsave, int *isave, double *dsave);

/* appl/lminfl.c */
void F77_NAME(lminfl)(double *x, int *ldx, int *n, int *k, double *qraux,
		      double *resid, double *hat, double *coef, double *sigma);


/* appl/loglin.c */
void loglin(int *nvar, int *dim, int *ncon, int *config, int *ntab,
	    double *table, double *fit, int *locmar, int *nmar, double *marg,
	    int *nu, double *u, double *maxdev, int *maxit,
	    double *dev, int *nlast, int *ifault);

/* appl/lowess.c */
void lowess(double *x, double *y, int *n,
	    double *f, int *nsteps, double *delta,
	    double *ys, double *rw, double *res);

/* appl/machar.c */
void machar(int *ibeta, int *it, int *irnd, int *ngrd, int *machep,
	    int *negep, int *iexp, int *minexp, int *maxexp,
	    double *eps, double *epsneg, double *xmin, double *xmax);

/* appl/massdist.c */
void massdist(double *x, double *xmass, int *nx, double *xlow, double *xhigh,
	      double *y, int *ny);

/* appl/maxcol.c: also in Utils.h */
void R_max_col(double *matrix, int *nr, int *nc, int *maxes, int *ties_meth);
    

/* appl/pretty.c */
double R_pretty0(double *lo, double *up, int *ndiv, int min_n,
		 double shrink_sml, double high_u_fact[],
		 int eps_correction, int return_bounds);
void R_pretty(double *lo, double *up, int *ndiv, int *min_n,
	      double *shrink_sml, double *high_u_fact,
	      int *eps_correction);

/* appl/rowsum.c */
void R_rowsum(int *dim, double *na_x, double *x, double *group);

/* appl/splines.c */
void spline_coef(int *method, int *n, double *x, double *y,
		 double *b, double *c, double *d, double *e);
void spline_eval(int *method, int *nu, double *u, double *v,
		 int *n, double *x, double *y,
		 double *b, double *c, double *d);
void natural_spline(int n, double *x, double *y,
		    double *b, double *c, double *d);
void fmm_spline(int n, double *x, double *y, double *b, double *c, double *d);
void periodic_spline(int n, double *x, double *y,
		     double *b, double *c, double *d, double *e);


/* appl/stem.c */
Rboolean stemleaf(double *x, int *n, double *scale, int *width, double *atom);

/* appl/strsignif.c */
void str_signif(char *x, int *n, char **type, int *width, int *digits,
		char **format, char **flag, char **result);

/* appl/tabulate.c */
void R_tabulate(int *x, int *n, int *nbin, int *ans);

/* appl/uncmin.c : */

/* type of pointer to the target and gradient functions */
typedef void (*fcn_p)(int, double *, double *, void *);

/* type of pointer to the hessian functions */
typedef void (*d2fcn_p)(int, int, double *, double *, void *);

void fdhess(int n, double *x, double fval, fcn_p fun, void *state,
	    double *h, int nfd, double *step, double *f, int ndigit,
	    double *typx);

void optif9(int nr, int n, double *x,
	    fcn_p fcn, fcn_p d1fcn, d2fcn_p d2fcn,
	    void *state, double *typsiz, double fscale, int method,
	    int iexp, int *msg, int ndigit, int itnlim, int iagflg,
	    int iahflg, double dlt, double gradtl, double stepmx,
	    double steptl, double *xpls, double *fpls, double *gpls,
	    int *itrmcd, double *a, double *wrk, int *itncnt);

void optif0(int nr, int n, double *x, fcn_p fcn, void *state,
	    double *xpls, double *fpls, double *gpls, int *itrmcd,
	    double *a, double *wrk);

/* appl/zeroin.c */
double R_zeroin(double ax, double bx, double (*f)(double, void *), void *info,
		double *Tol, int *Maxit);


/* ALL appl/<foobar>.f	[semi-automatically by
 *				 f2c -A -P *.f; cat *.P > all.h	 and editing]
 */

/* This is not in the applications but in the BLAS, and defined in Linpack.h
extern int F77_NAME(lsame)(char *, char *);
*/

/* LINPACK routines also declared in Linpack.h
void F77_NAME(dpoco)(double *a, int *lda, int *n, double *rcond,
		     double *z__, int *info);
void F77_NAME(dpodi)(double *a, int *lda, int *n, double *det, int *job);
void F77_NAME(dpofa)(double *a, int *lda, int *n, int *info);
void F77_NAME(dposl)(double *a, int *lda, int *n, double *b);
void F77_NAME(dqrdc)(double *x, int *ldx, int *n, int *p,
		     double *qraux, int *jpvt, double *work, int *job);
void F77_NAME(dqrsl)(double *x, int *ldx, int *n, int *k,
		     double *qraux, double *y,
		     double *qy, double *qty, double *b,
		     double *rsd, double *xb, int *job, int *info);
void F77_NAME(dsvdc)(double *x, int *ldx, int *n, int *p,
		     double *s, double *e,
		     double *u, int *ldu, double *v, int *ldv,
		     double *work, int *job, int *info);
void F77_NAME(dtrco)(double *t, int *ldt, int *n, double *rcond,
		     double *z__, int *job);
void F77_NAME(dtrsl)(double *t, int *ldt, int *n, double *b, int *job,
		     int *info);
*/

/* find qr decomposition, dqrdc2() is basis of R's qr(), also used by nlme */
void F77_NAME(dqrdc2)(double *x, int *ldx, int *n, int *p,
		      double *tol, int *rank,
		      double *qraux, int *pivot, double *work);
void F77_NAME(dqrls)(double *x, int *n, int *p, double *y, int *ny,
		     double *tol, double *b, double *rsd,
		     double *qty, int *k,
		     int *jpvt, double *qraux, double *work);

/* appl/dqrutl.f: interfaces to dqrsl */
void F77_NAME(dqrqty)(double *x, int *n, int *k, double *qraux,
		      double *y, int *ny, double *qty);
void F77_NAME(dqrqy)(double *x, int *n, int *k, double *qraux,
		     double *y, int *ny, double *qy);
void F77_NAME(dqrcf)(double *x, int *n, int *k, double *qraux,
		     double *y, int *ny, double *b, int *info);
void F77_NAME(dqrrsd)(double *x, int *n, int *k, double *qraux,
		     double *y, int *ny, double *rsd);
void F77_NAME(dqrxb)(double *x, int *n, int *k, double *qraux,
		     double *y, int *ny, double *xb);


#ifdef  __cplusplus
}
#endif

#endif /* R_APPLIC_H_ */
