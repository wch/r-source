/* dpbfa.f -- translated by f2c (version 19971204).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* Subroutine */ int dpbfa(doublereal *abd, integer *lda, integer *n, 
	integer *m, integer *info)
{
    /* System generated locals */
    integer abd_dim1, abd_offset, i__1, i__2, i__3;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    extern doublereal ddot(integer *, doublereal *, integer *, doublereal *, 
	    integer *);
    static integer j, k;
    static doublereal s, t;
    static integer ik, jk, mu;


/*     dpbfa factors a double precision symmetric positive definite */
/*     matrix stored in band form. */

/*     dpbfa is usually called by dpbco, but it can be called */
/*     directly with a saving in time if	 rcond	is not needed. */

/*     on entry */

/* 	 abd	 double precision(lda, n) */
/* 		 the matrix to be factored.  the columns of the upper */
/* 		 triangle are stored in the columns of abd and the */
/* 		 diagonals of the upper triangle are stored in the */
/* 		 rows of abd .	see the comments below for details. */

/* 	 lda	 integer */
/* 		 the leading dimension of the array  abd . */
/* 		 lda must be .ge. m + 1 . */

/* 	 n	 integer */
/* 		 the order of the matrix  a . */

/* 	 m	 integer */
/* 		 the number of diagonals above the main diagonal. */
/* 		 0 .le. m .lt. n . */

/*     on return */

/* 	 abd	 an upper triangular matrix  r , stored in band */
/* 		 form, so that	a = trans(r)*r . */

/* 	 info	 integer */
/* 		 = 0  for normal return. */
/* 		 = k  if the leading minor of order  k	is not */
/* 		      positive definite. */

/*     band storage */

/* 	    if	a  is a symmetric positive definite band matrix, */
/* 	    the following program segment will set up the input. */

/* 		    m = (band width above diagonal) */
/* 		    do 20 j = 1, n */
/* 		       i1 = max0(1, j-m) */
/* 		       do 10 i = i1, j */
/* 			  k = i-j+m+1 */
/* 			  abd(k,j) = a(i,j) */
/* 		 10    continue */
/* 		 20 continue */

/*     linpack.	this version dated 08/14/78 . */
/*     cleve moler, university of new mexico, argonne national lab. */

/*     subroutines and functions */

/*     blas ddot */
/*     fortran max0,sqrt */

/*     internal variables */

/*     begin block with ...exits to 40 */


    /* Parameter adjustments */
    abd_dim1 = *lda;
    abd_offset = abd_dim1 + 1;
    abd -= abd_offset;

    /* Function Body */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	*info = j;
	s = 0.f;
	ik = *m + 1;
/* Computing MAX */
	i__2 = j - *m;
	jk = max(i__2,1);
/* Computing MAX */
	i__2 = *m + 2 - j;
	mu = max(i__2,1);
	if (*m < mu) {
	    goto L20;
	}
	i__2 = *m;
	for (k = mu; k <= i__2; ++k) {
	    i__3 = k - mu;
	    t = abd[k + j * abd_dim1] - ddot(&i__3, &abd[ik + jk * abd_dim1],
		     &c__1, &abd[mu + j * abd_dim1], &c__1);
	    t /= abd[*m + 1 + jk * abd_dim1];
	    abd[k + j * abd_dim1] = t;
	    s += t * t;
	    --ik;
	    ++jk;
/* L10: */
	}
L20:
	s = abd[*m + 1 + j * abd_dim1] - s;
/*     ......exit */
	if (s <= 0.f) {
	    goto L40;
	}
	abd[*m + 1 + j * abd_dim1] = sqrt(s);
/* L30: */
    }
    *info = 0;
L40:
    return 0;
} /* dpbfa_ */
