/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997-1998   Robert Gentleman, Ross Ihaka and the
 *                            R Development Core Team
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

/* ../appl/bakslv.f
   -- translated by f2c (version of 1 June 1993 23:00:00).
   -- and hand edited by Martin Maechler.
   */

#ifdef HAVE_CONFIG_H
#include <Rconfig.h>
#endif

#include "Fortran.h"		/* incl Rconfig.h */
#include "Linpack.h"		/* incl Blas.h    */
#include "Applic.h"

void bakslv(double *t, int *ldt, int *n,
	    double *b, int *ldb, int *nb,
	    double *x, int *job, int *info)
{

/* bakslv is a subroutine to solve triangular systems of
 * the form
 *		     t * x = b
 * or
 *		     t' * x = b		[ t' := transpose(t) ]

 * where t is a triangular matrix of order n.
 * The subroutine handles the multiple right-hand side case.
 * It is really just a wrapper for the linpack subroutine dtrsl.

 * on entry

 *	t      double (ldt,n'). n' >= n (below)
 *	       t[] contains the coefficient matrix of the system
 *	       to be solved.  only the elements above or below
 *	       the diagonal are referenced.

 *	ldt    int;	ldt is the leading dimension of the array t.
 *	n      int;	n is the order of the system.  n <= min(ldt,ldb)


 *	b      double (ldb,nb').  nb' >= nb (below)
 *	       b[] contains the right hand side(s) of the system.

 *	ldb    int;	ldb is the leading dimension of the array b.
 *	nb     int;	the number of right hand sides of the system.

 *	job    int;	job specifies what kind of system is to be solved.
 *
 *	       if job is
 *
 *		    00	 solve t  * x = b,	t lower triangular,
 *		    01	 solve t  * x = b,	t upper triangular,
 *		    10	 solve t' * x = b,	t lower triangular,
 *		    11	 solve t' * x = b,	t upper triangular.

 * on return

 *	x      double precision(ldb, nb)
 *	       contains the solution(s) if info == 0.

 *	info   int
 *	       info contains zero if the system is nonsingular.
 *	       otherwise info contains the index of
 *	       the first zero diagonal element of t.

 * subroutines and functions

 *     linpack: dtrsl (t,ldt,n, b,job,info)
 *     blas:	dcopy
 */

    /* INTERNAL VARIABLES. */

    static int c__1 = 1; /* constant */
    int p, nn, j;

    p = *nb;
    nn = *ldb;

    for (j = 0; j < p; ++j) {/* for each right-hand side */
       F77_SYMBOL(dcopy)(n, &b[j * nn], &c__1,
			    &x[j * nn], &c__1);
       F77_SYMBOL(dtrsl)(t, ldt, n, &x[j * nn], job, info);
       if (*info != 0) {
	 return;
       }
    }
}

