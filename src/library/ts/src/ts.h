/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001   The R Development Core Team.
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
 */

#ifndef R_TS_H
#define R_TS_H
#include <Rinternals.h>

void acf(double *x, int *n, int *nser, int *nlag, int *correlation,
	 double *acf);
void uni_pacf(double *cor, double *p, int *pnlag);
void artoma(int *pp, double *phi, double *psi, int *npsi);
void burg(int *pn, double*x, int *pp, double *coefs, double *var1,
	  double *var2);
void multi_burg(int *pn, double *x, int *pomax, int *pnser, double *coef,
		double *pacf, double *var, double *aic, int *porder,
		int *useaic, int *vmethod);
void multi_yw(double *acf, int *pn, int *pomax, int *pnser, double *coef,
	      double *pacf, double *var, double *aic, int *porder,
	      int *puseaic);
void R_intgrt_vec (double *x, double *y, int *lag, int *n);
void filter1(double *x, int *n, double *filter, int *nfilt, int *sides,
	     int *circular, double *out);
void filter2(double *x, int *n, double *filter, int *nfilt, double *out);
void R_pp_sum (double *u, int *n, int *l, double *sum);


void
F77_SUB(eureka)(int *lr, double *r__, double *g,
		double *f, double *var, double *a);

void
F77_SUB(stl)(double *y, int *n, int *np, int *ns,
	     int *nt, int *nl, int *isdeg, int *itdeg, int *ildeg,
	     int *nsjump, int *ntjump, int *nljump, int *ni,
	     int *no, double *rw, double *season, double *trend,
	     double *work);

typedef struct
{
    int p, q, r, np, nrbar, n, m, trans;
    int mp, mq, msp, msq, ns;
    double delta, s2;
    double *params, *phi, *theta, *a, *P, *V;
    double *thetab, *xnext, *xrow, *rbar, *w, *wkeep, *resid, *reg;
} starma_struct, *Starma;

void starma(int p, int q, int r, int np, double *phi,
	    double *theta, double *a,
	    double *P, double *V, double *thetab, double *xnext,
	    double *xrow, double *rbar, int nrbar, int *ifault);

void karma(int p, int q, int r, int np, double *phi,
	   double *theta, double *a, double *P,
	   double *V, int n, double *w, double *resid,
	   double *sumlog, double *ssq, int iupd,
	   double delta, int *nit);

void forkal(int p, int q, int r, int np, int ird, 
	    int il, int n, int nrbar, 
	    double *phi, double *theta, double *delta, 
	    double *w, double *y, double *amse,
	    double *V, double *resid,
	    double *xnext, double *xrow, double *rbar, 
	    double *thetab, int *ifault);

SEXP setup_starma(SEXP na, SEXP x, SEXP pn, SEXP xreg, SEXP pm, 
		  SEXP dt, SEXP ptrans);
SEXP free_starma(void);
SEXP set_trans(SEXP ptrans);
SEXP arma0fa(SEXP inparams);
SEXP get_s2(void);
SEXP get_resid(void);
SEXP Dotrans(SEXP x);
SEXP arma0_kfore(SEXP pd, SEXP psd, SEXP n_ahead);
#endif
