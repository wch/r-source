/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001, 2004   The R Development Core Team.
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

#include <R.h>
#include <Rinternals.h>

void dblcen(double *a, int *na);

void R_distance(double *x, int *nr, int *nc, double *d, int *diag, 
		int *method, double *p);
/* double R_euclidean(double *x, int nr, int nc, int i1, int i2);
   double R_maximum  (double *x, int nr, int nc, int i1, int i2);
   double R_manhattan(double *x, int nr, int nc, int i1, int i2);
   double R_canberra (double *x, int nr, int nc, int i1, int i2);
   double R_dist_binary(double *x, int nr, int nc, int i1, int i2); */



SEXP R_cutree(SEXP merge, SEXP which);

void
F77_SUB(hclust)(int *n, int *len, int *iopt, int *ia, int *ib,
		double *crit, double *membr, int *nn,
		double *disnn, int *flag, double *diss);

void
F77_SUB(hcass2)(int *n, int *ia, int *ib, int *iorder, int *iia, int *iib);

void
F77_SUB(kmns)(double *a, int *m, int *n, double *c, int *k,
	      int *ic1, int *ic2, int *nc, double * an1, double *an2,
	      int *ncp, double *d, int *itran,
	      int *live, int *iter, double *wss, int *ifault);
