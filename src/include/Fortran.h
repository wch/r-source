/*
 *  R : A Computer Language for Statistical Data Analysis
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifndef FORTRAN_H
#include "R_ext/Mathlib.h"
#define FORTRAN_H

/*#include "config.h"*/
#include "R_ext/Complex.h"
#include "R_ext/Boolean.h" /* TRUE/FALSE */

	/* General F2C Macros */

/* a whole bunch of stuff to keep watcom's C compiler happy */
#ifdef min
#undef min
#endif
#ifdef max
#undef max
#endif
#ifdef huge
#undef huge
#endif

/* not defined in any R header */
#ifndef _STDLIB_H
#define abs(x) ((x) >= 0 ? (x) : -(x))
#endif
/* #define dabs(x) (double)abs(x) */
#define min(a,b) ((a) <= (b) ? (a) : (b))
#define max(a,b) ((a) >= (b) ? (a) : (b))
#define dmin(a,b) (double)min(a,b)
#define dmax(a,b) (double)max(a,b)
/*-- Note that ./Mathlib.h  has  [if]min2(.,.) and [if]max2(.,.) ! */

	/* Versions of Fortran Intrinsics */

/* not defined in any R header */
#ifdef NONSTANDARD_F77LIBS

#define POW_DD	f77_pow_dd
#define POW_DI	f77_pow_di
#define DABS	f77_dabs
#define DSIGN	f77_dsign
#define DLOG10	f77_dlog10

#define ZDIV	f77_zdiv
#define ZABS	f77_zabs
#define ZIMAG	f77_dimag
#define ZREAL	f77_real
#define ZCNJG	f77_dcnjg

#else

#define POW_DD  pow_dd
#define POW_DI  pow_di
#define DABS    dabs
#define DSIGN   dsign
#define DLOG10  dlog10

#define ZDIV    zdiv
#define ZABS    zabs
#define ZIMAG   dimag
#define ZREAL   real
#define ZCNJG   dcnjg

#endif

extern double DABS(double*);
extern double DSIGN(double*, double*);
extern double DLOG10(double*);
extern double POW_DD(double*, double*);
extern double POW_DI(double*, int*);

extern void ZDIV(Rcomplex*, Rcomplex*, Rcomplex*);
extern double ZABS(Rcomplex*);
extern double ZIMAG(Rcomplex*);
extern double ZREAL(Rcomplex*);
extern void ZCNJG(Rcomplex*, Rcomplex*);

#endif
