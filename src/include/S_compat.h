/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2000  R Development Core Team
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

/* S compatibility library headers --
			maps internal functions in S to R equivalents
 */

#ifndef R_S_COMPAT_H_
#define R_S_COMPAT_H_

#include "S.h"
/* #include "Fortran.h"*/
#include "R_ext/Linpack.h"

/* REdefine these for (undocumented!) S compatibility : */
#undef dnorm
#undef pnorm
#undef qnorm
#define dnorm(x,m,s) dnorm4(x,m,s, 0)
#define pnorm(x,m,s) pnorm5(x,m,s, 1, 0)
#define qnorm(x,m,s) qnorm5(x,m,s, 1, 0)

extern void
F77_NAME(dqrdca) (double*, longint*, longint*, longint*,
		  double*, longint*, double*, longint*, double*);

extern void
F77_NAME(dbksl) (double*, longint*, longint*, double*, longint*, longint*);

#endif
