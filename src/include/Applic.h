/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998 ff  Robert Gentleman, Ross Ihaka and the R core team
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
 *
 * Application Routines, typically implemented in  ../appl/
 */

#ifndef APPLIC_H_
#define APPLIC_H_

#include "Platform.h"/* F77... */

void approx(double *, double *, int *, double *, int *,
	    int *, double *, double *, double *);

void bakslv(double *, int *, int *,
	    double *, int *, int *,
	    double *, int *, int *);

void bincode (double *, int *, double *, int *, int *, int *, int *);
void bincode2(double *, int *, double *, int *, int *, int *, int *);
void bincount(double *, int *, double *, int *, int *, int *, int *);

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
double binary(double *, int, int, int, int);
#define EUCLIDEAN 1
#define MAXIMUM   2
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
/* ...............*/

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
/* ...............*/

/* pythag.c */
double pythag(double, double);

/* splines.c */
void spline_coef(int *method, int *n, double *x, double *y,
		 double *b, double *c, double *d, double *e);
void spline_eval(int *method, int *nu, double *u, double *v,
		 int *n, double *x, double *y,
		 double *b, double *c, double *d);

/* stem.c      (??) */
/* ...............*/

/* strsignif.c  (??) */
/* ...............*/

/* tabulate.c */
void tabulate(int *x, int *n, int *ans);

#endif
