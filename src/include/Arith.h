/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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
 */

#ifndef ARITH_H_
#define ARITH_H_

#include "Platform.h"
#ifdef Macintosh
#include <fp.h>
#else
#include <math.h>
#ifndef HAVE_FINITE
int finite(double);
#endif
#endif

extern double	R_tmp;			/* Used in NaN/Inf checks */
extern double	R_NaN;			/* IEEE NaN or -DBL_MAX */
extern double	R_PosInf;		/* IEEE Inf or DBL_MAX */
extern double	R_NegInf;		/* IEEE -Inf or -DBL_MAX */
extern int   	R_NaInt;		/* NA_INTEGER etc */
extern double	R_NaReal;		/* NA_REAL */

#ifdef Win32
extern int isnan(double);
extern int finite(double);
#endif

#ifdef IEEE_754

#define MATH_CHECK(call)	(call)
#define FINITE(x)		finite(x)
#define ISNAN(x)		((x)!=(x))/* True, *both* for NA & NaN */
#define ISNA(x)			R_IsNA(x) /* --> ../main/arithmetic.c */

#else

#define MATH_CHECK(call)	(errno=0,R_tmp=call,(errno==0)?R_tmp:R_NaN)
#define FINITE(x)		((x)!=R_NaReal)
#define ISNAN(x)		((x)!=R_NaReal)/* ?? rather not -- FIXME!! */
#define ISNA(x)			((x)!=R_NaReal)/* ?? rather not -- FIXME!! */
/* never used. in HP-UX' c89 "NAN" is for a double const. :
 * #define NAN(x)			ISNAN(x)
 */
#endif

#endif
