/* ../appl/bakslv.f -- translated by f2c (version of 1 June 1993  23:00:00).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* ----------------------------------------------------------------------- */

/*  R : A COMPUTER LANGAGE FOR STATISTICAL DATA ANALYSIS */
/*  COPYRIGHT (C) 1996  ROBERT GENTLEMAN AND ROSS IHAKA */

/*  THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY */
/*  IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY */
/*  THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR */
/*  (AT YOUR OPTION) ANY LATER VERSION. */

/*  THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, */
/*  BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF */
/*  MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  SEE THE */
/*  GNU GENERAL PUBLIC LICENSE FOR MORE DETAILS. */

/*  YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE */
/*  ALONG WITH THIS PROGRAM; IF NOT, WRITE TO THE FREE SOFTWARE */
/*  FOUNDATION, INC., 675 MASS AVE, CAMBRIDGE, MA 02139, USA. */

/* ----------------------------------------------------------------------- */

/* Subroutine */ int bkslv_(doublereal *t, integer *ldt, integer *n, 
	doublereal *b, integer *ldb, doublereal *x, integer *job, integer *
	info)
{
    /* System generated locals */
    integer t_dim1, t_offset, b_dim1, b_offset, x_dim1, x_offset, i__1;

    /* Local variables */
    static integer j;
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *, 
	    doublereal *, integer *), dtrsl_(doublereal *, integer *, integer 
	    *, doublereal *, integer *, integer *);


/*     BACKSLV IS A SUBROUTINE TO SOLVE TRIANGULAR SYSTEMS OF */
/*     THE FORM */

/*                   T * X = B */
/*     OR */
/*                   TRANS(T) * X = B */

/*     WHERE T IS A TRIANGULAR MATRIX OF ORDER N. HERE TRANS(T) */
/*     DENOTES THE TRANSPOSE OF THE MATRIX T.  THE SUBROUTINE */
/*     HANDLES THE MULTIPLE RIGHT-HAND SIDE CASE.  IT IS REALLY */
/*     JUST A WRAPPER FOR THE LINPACK SUBROUTINE DTRSL. */

/*     ON ENTRY */

/*        T      DOUBLE PRECISION(LDT,N). */
/*               X CONTAINS THE COEFFICIENT MATRIX OF THE SYSTEM */
/*               TO BE SOLVED.  ONLY THE ELEMENTS ABOVE OR BELOW */
/*               THE DIAGONAL ARE REFERENCED. */

/*        LDT    INTEGER */
/*               LDT IS THE LEADING DIMENSION OF THE ARRAY T. */


/*        N      INTEGER */
/*               N IS THE ORDER OF THE SYSTEM. */

/*        B      DOUBLE PRECISION(LDB,NB) */
/*               B CONTAINS THE RIGHT HAND SIDE(S) OF THE SYSTEM. */

/*        NB     INTEGER */
/*               THE NUMBER OF RIGHT HAND SIDES OF THE SYSTEM. */

/*        JOB    INTEGER */
/*               JOB SPECIFIES WHAT KIND OF SYSTEM IS TO BE SOLVED. */
/*               IF JOB IS */

/*                    00   SOLVE T*X=B, T LOWER TRIANGULAR, */
/*                    01   SOLVE T*X=B, T UPPER TRIANGULAR, */
/*                    10   SOLVE TRANS(T)*X=B, T LOWER TRIANGULAR, */
/*                    11   SOLVE TRANS(T)*X=B, T UPPER TRIANGULAR. */

/*     ON RETURN */

/*        X      DOUBLE PRECISION(LDB, NB) */
/*               CONTAINS THE SOLUTION(S) IF INFO .NE. 0. */

/*        INFO   INTEGER */
/*               INFO CONTAINS ZERO IF THE SYSTEM IS NONSINGULAR. */
/*               OTHERWISE INFO CONTAINS THE INDEX OF */
/*               THE FIRST ZERO DIAGONAL ELEMENT OF T. */

/*     SUBROUTINES AND FUNCTIONS */

/*     LINPACK DTRSL */
/*     BLAS DAXPY,DCOPY,DDOT */
/*     FORTRAN MOD */

/*     INTERNAL VARIABLES. */


    /* Parameter adjustments */
    x_dim1 = *ldb;
    x_offset = x_dim1 + 1;
    x -= x_offset;
    b_dim1 = *ldb;
    b_offset = b_dim1 + 1;
    b -= b_offset;
    t_dim1 = *ldt;
    t_offset = t_dim1 + 1;
    t -= t_offset;

    /* Function Body */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	dcopy_(n, &b[j * b_dim1 + 1], &c__1, &x[j * x_dim1 + 1], &c__1);
	dtrsl_(&t[t_offset], ldt, n, &x[j * x_dim1 + 1], job, info);
	if (*info != 0) {
	    return 0;
	}
/* L10: */
    }
    return 0;
} /* bkslv_ */

