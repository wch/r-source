/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2001  The R Development Core Team.
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifndef R_ARITH_H_
#define R_ARITH_H_

#ifdef Macintosh
#include <fp.h> 
#else
#include <math.h>
#endif

#include <R_ext/libextern.h>
#ifdef  __cplusplus
extern "C" {
#endif

/* implementation of these : ../../main/arithmetic.c */
LibExtern double R_NaN;		/* IEEE NaN or = NA_REAL */
LibExtern double R_PosInf;	/* IEEE Inf  or	 DBL_MAX */
LibExtern double R_NegInf;	/* IEEE -Inf or -DBL_MAX */
LibExtern double R_NaReal;	/* NA_REAL: IEEE or "almost -DBL_MAX" */
LibExtern int	 R_NaInt;	/* NA_INTEGER:= INT_MIN currently */
#ifdef __MAIN__
#undef extern
#undef LibExtern
#endif

#define NA_LOGICAL	R_NaInt
#define NA_INTEGER	R_NaInt
#define NA_FACTOR	R_NaInt
#define NA_REAL		R_NaReal
/* NA_STRING is a SEXP, so defined in Rinternals.h */

int R_IsNA(double);		/* True for R's NA only */
int R_IsNaN(double);		/* True for special NaN, *not* for NA */
int R_IsNaNorNA(double);	/* True for both */
int R_finite(double);		/* True if none of NA, NaN, +/-Inf */

#define ISNA(x)	       R_IsNA(x)
#define ISNAN(x)       R_IsNaNorNA(x)
#define R_FINITE(x)    R_finite(x)

#ifdef  __cplusplus
}
#endif

#endif /* R_ARITH_H_ */
