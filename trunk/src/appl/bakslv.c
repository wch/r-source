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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

/* ../appl/bakslv.f
   -- translated by f2c (version of 1 June 1993 23:00:00).
   -- and hand edited by Martin Maechler.
   -- Modified to use level-3 BLAS, Douglas Bates, May, 2001
   */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Rinternals.h>
#include <R_ext/Linpack.h>
#include <R_ext/Applic.h>

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
 * It was just a wrapper for the linpack subroutine dtrsl, now it calls
 * the level-3 blas subroutine dtrsm.

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
 *
 * subroutines and functions
 *     blas:    dcopy
 *     blas3:   dtrsm
 */
    char *side = "L", *uplo, *transa, *diag = "N";
    int i, ione = 1, j, nn = *n;
    double one = 1.0;

    *info = 0;
    for(i = 0; i < nn; i++) {	/* check for zeros on diagonal */
	if (t[i * (*ldt + 1)] == 0.0) {
	    *info = i + 1;
	    return;
	}
    }
    for(j = 0; j < *nb; j++) {  /* copy b to x */
       F77_CALL(dcopy)(n, &b[j * *ldb], &ione, &x[j * *ldb], &ione);
    }
    transa = ((*job) / 10) ? "T" : "N";
    uplo = ((*job) % 10) ? "U" : "L";
    if (*n > 0 && *nb > 0 && *ldt > 0 && *ldb > 0) {
	F77_CALL(dtrsm)(side, uplo, transa, diag, n, nb, &one,
			t, ldt, x, ldb);
    }
}

