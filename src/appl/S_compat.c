/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--1999  The R Development Core Team
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

/* S compatibility library - mapping some internal functions in S to R
 */

#include "S_compat.h"

extern longint
F77_NAME(dqrdc2) (double*, longint*, longint*, longint*, double*,
		  longint*, double*, longint*, double*);

/* These are based on the argument
   sequences used in the nlme 2.1 code 
   on statlib. */
void
F77_NAME(dqrdca) (double *x, longint *ldx, longint *n, longint *p,
		  double *qraux, longint *jpvt, double *work,
		  longint *rank, double *tol)
{				/* modified calling sequence for dqrdc2 */
    F77_NAME(dqrdc2) (x, ldx, n, p, tol, rank, qraux, jpvt, work);
}

void
F77_NAME(dbksl) (double *x, longint *ldx, longint *n,
		 double *rhs, longint *nrhs, longint *info)
{				/* solve multiple right hand sides on
				   upper triangular system of equations */

    /*--- is almost like bakslv() in ./bakslv.c -- however that one COPIES */
    longint nn = *nrhs, job = 1;
    while (nn-- > 0) {
	F77_NAME(dtrsl) (x, ldx, n, rhs, &job, info);
	if (*info) return;
	rhs += *n;
    }
}
