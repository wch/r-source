/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997        Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2000   The R Development Core Team.
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
#ifndef R_LINPACK_H_
#define R_LINPACK_H_

#include "Rconfig.h"		/* for F77_SYMBOL */

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

	/* Double Precision Linpack */

extern int F77_SYMBOL(dchdc)(double*, int*, int*, double*, int*, int*, int*);
extern int F77_SYMBOL(dchdd)(double*, int*, int*, double*, double*, int*, int*, double*, double*, double*, double*, int*);
extern int F77_SYMBOL(dchex)(double*, int*, int*, int*, int*, double*, int*, int*, double*, double*, int*);
extern int F77_SYMBOL(dchud)(double*, int*, int*, double*, double*, int*, int*, double*, double*, double*, double*);
extern int F77_SYMBOL(dgbco)(double*, int*, int*, int*, int*, int*, double*, double*);
extern int F77_SYMBOL(dgbdi)(double*, int*, int*, int*, int*, int*, double*);
extern int F77_SYMBOL(dgbfa)(double*, int*, int*, int*, int*, int*, int*);
extern int F77_SYMBOL(dgbsl)(double*, int*, int*, int*, int*, int*, double*, int*);
extern int F77_SYMBOL(dgeco)(double*, int*, int*, int*, double*, double*);
extern int F77_SYMBOL(dgedi)(double*, int*, int*, int*, double*, double*, int*);
extern int F77_SYMBOL(dgefa)(double*, int*, int*, int*, int*);
extern int F77_SYMBOL(dgesl)(double*, int*, int*, int*, double*, int*);
extern int F77_SYMBOL(dgtsl)(int*, double*, double*, double*, double*, int*);
extern int F77_SYMBOL(dpbco)(double*, int*, int*, int*, double*, double*, int*);
extern int F77_SYMBOL(dpbdi)(double*, int*, int*, int*, double*);
extern int F77_SYMBOL(dpbfa)(double*, int*, int*, int*, int*);
extern int F77_SYMBOL(dpbsl)(double*, int*, int*, int*, double*);
extern int F77_SYMBOL(dpoco)(double*, int*, int*, double*, double*, int*);
extern int F77_SYMBOL(dpodi)(double*, int*, int*, double*, int*);
extern int F77_SYMBOL(dpofa)(double*, int*, int*, int*);
extern int F77_SYMBOL(dposl)(double*, int*, int*, double*);
extern int F77_SYMBOL(dppco)(double*, int*, double*, double*, int*);
extern int F77_SYMBOL(dppdi)(double*, int*, double*, int*);
extern int F77_SYMBOL(dppfa)(double*, int*, int*);
extern int F77_SYMBOL(dppsl)(double*, int*, double*);
extern int F77_SYMBOL(dptsl)(int*, double*, double*, double*);
extern int F77_SYMBOL(dqrdc)(double*, int*, int*, int*, double*, int*, double*, int*);
extern int F77_SYMBOL(dqrsl)(double*, int*, int*, int*, double*, double*, double*, double*, double*, double*, double*, int*, int*);
extern int F77_SYMBOL(dsico)(double*, int*, int*, int*, double*, double*);
extern int F77_SYMBOL(dsidi)(double*, int*, int*, int*, double*, int*, double*, int*);
extern int F77_SYMBOL(dsifa)(double*, int*, int*, int*, int*);
extern int F77_SYMBOL(dsisl)(double*, int*, int*, int*, double*);
extern int F77_SYMBOL(dspco)(double*, int*, int*, double*, double*);
extern int F77_SYMBOL(dspdi)(double*, int*, int*, double*, int*, double*, int*);
extern int F77_SYMBOL(dspfa)(double*, int*, int*, int*);
extern int F77_SYMBOL(dspsl)(double*, int*, int*, double*);
extern int F77_SYMBOL(dsvdc)(double*, int*, int*, int*, double*, double*, double*, int*, double*, int*, double*, int*, int*);
extern int F77_SYMBOL(dtrco)(double*, int*, int*, double*, double*, int*);
extern int F77_SYMBOL(dtrdi)(double*, int*, int*, double*, int*, int*);
extern int F77_SYMBOL(dtrsl)(double*, int*, int*, double*, int*, int*);

#endif
