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
 *  BLAS in C
 *
 *  This is essentially machine translated from Netlib code.
 *  Some minor fixups and formatting has been done.
 */

#ifndef BLAS_H
#define BLAS_H

	/* Double Precision Blas */

extern double F77_SYMBOL(dasum)(int*, double*, int*);
extern int    F77_SYMBOL(daxpy)(int*, double*, double*, int*, double*, int*);
extern int    F77_SYMBOL(dcopy)(int*, double*, int*, double*, int*);
extern double F77_SYMBOL(ddot)(int*, double*, int*, double*, int*);
extern double F77_SYMBOL(dmach)(int*);
extern double F77_SYMBOL(dnrm2)(int*, double*, int*);
extern int    F77_SYMBOL(drot)(int*, double*, int*, double*, int*, double*, double*);
extern int    F77_SYMBOL(drotg)(double*, double*, double*, double*);
extern int    F77_SYMBOL(dscal)(int*, double*, double*, int*);
extern int    F77_SYMBOL(dswap)(int*, double*, int*, double*, int*);
extern int    F77_SYMBOL(idamax)(int*, double*, int*);


	/* Double Precision Complex Blas */

extern double F77_SYMBOL(dznrm2)(int*, complex *x, int*);
extern int    F77_SYMBOL(izamax)(int*, complex *zx, int*);
extern int    F77_SYMBOL(zaxpy)(int*, complex *za, complex *zx, int*, complex *zy, int*);
extern int    F77_SYMBOL(zcopy)(int*, complex *zx, int*, complex *zy, int*);
extern void   F77_SYMBOL(zdotc)(complex * ret_val, int*, complex *zx, int*, complex *zy, int*);
extern void   F77_SYMBOL(zdotu)(complex * ret_val, int*, complex *zx, int*, complex *zy, int*);
extern int    F77_SYMBOL(zdscal)(int*, double*, complex *zx, int*);
extern int    F77_SYMBOL(zrotg)(complex *ca, complex *cb, double*, complex *s);
extern int    F77_SYMBOL(zscal)(int*, complex *za, complex *zx, int*);
extern int    F77_SYMBOL(zswap)(int*, complex *zx, int*, complex *zy, int*);

#endif
