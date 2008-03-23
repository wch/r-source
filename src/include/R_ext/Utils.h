/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2005    Robert Gentleman, Ross Ihaka
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
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 *
 *
 * Generally useful  UTILITIES  *NOT* relying on R internals (from Defn.h)
 */

#ifndef R_EXT_UTILS_H_
#define R_EXT_UTILS_H_

#include <R_ext/Boolean.h>
#include <R_ext/Complex.h>

#define revsort       Rf_revsort
#define iPsort        Rf_iPsort
#define rPsort        Rf_rPsort
#define cPsort        Rf_cPsort
#define IndexWidth    Rf_IndexWidth
#define setIVector    Rf_setIVector
#define setRVector    Rf_setRVector
#define StringFalse   Rf_StringFalse
#define StringTrue    Rf_StringTrue
#define isBlankString Rf_isBlankString
#define hsv2rgb       Rf_hsv2rgb
#define rgb2hsv       Rf_rgb2hsv

#ifdef  __cplusplus
extern "C" {
#endif

/* ../../main/sort.c : */
void	R_isort(int*, int);
void	R_rsort(double*, int);
void	R_csort(Rcomplex*, int);
void    rsort_with_index(double *, int *, int);
void	revsort(double*, int*, int);/* reverse; sort i[] alongside */
void	iPsort(int*,    int, int);
void	rPsort(double*, int, int);
void	cPsort(Rcomplex*, int, int);

/* ../../main/qsort.c : */
void R_qsort    (double *v,         int i, int j);
void R_qsort_I  (double *v, int *I, int i, int j);
void R_qsort_int  (int *iv,         int i, int j);
void R_qsort_int_I(int *iv, int *I, int i, int j);
#ifdef R_RS_H
void F77_NAME(qsort4)(double *v, int *indx, int *ii, int *jj);
void F77_NAME(qsort3)(double *v,            int *ii, int *jj);
#endif

/* ../../main/printutils.c : */
int	IndexWidth(int);
/* ../../main/util.c  and others : */
const char *R_ExpandFileName(const char *);
void	setIVector(int*, int, int);
void	setRVector(double*, int, double);
Rboolean StringFalse(const char *);
Rboolean StringTrue(const char *);
Rboolean isBlankString(const char *);

/* These two are guaranteed to use '.' as the decimal point,
   and to accept "NA".
 */
double R_atof(const char *str);
double R_strtod(const char *c, char **end);

char *R_tmpnam(const char *prefix, const char *tempdir);

void	hsv2rgb(double h, double s, double v,
		double *r, double *g, double *b);
void	rgb2hsv(double r, double g, double b,
		double *h, double *s, double *v);

void R_CheckUserInterrupt(void);
void R_CheckStack(void);
    

/* ../../appl/interv.c: also in Applic.h */
int findInterval(double *xt, int n, double x,
		 Rboolean rightmost_closed,  Rboolean all_inside, int ilo,
		 int *mflag);
#ifdef R_RS_H
int F77_SUB(interv)(double *xt, int *n, double *x,
		    Rboolean *rightmost_closed, Rboolean *all_inside,
		    int *ilo, int *mflag);
#endif
void find_interv_vec(double *xt, int *n,	double *x,   int *nx,
		     int *rightmost_closed, int *all_inside, int *indx);

/* ../../appl/maxcol.c: also in Applic.h */
void R_max_col(double *matrix, int *nr, int *nc, int *maxes, int *ties_meth);

#ifdef  __cplusplus
}
#endif

#endif /* R_EXT_UTILS_H_ */
