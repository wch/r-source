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
void setup_starma(int *na, double *x, int *pn, double *xreg, int *pm,
                  double *dt, int *ptrans);
void Dotrans(double *x, double *y);
void get_s2(double *res);
void get_resid(double *res);
void arma0fa(double *inparams, double *res);
void arma0_kfore(int *pd, int *psd, int *n_ahead, double *x, double *var);
void free_starma(void);
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

#endif
