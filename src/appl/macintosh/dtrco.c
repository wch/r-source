/* dtrco.f -- translated by f2c (version 19971204).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* Subroutine */ int dtrco(doublereal *t, integer *ldt, integer *n, 
	doublereal *rcond, doublereal *z__, integer *job)
{
    /* System generated locals */
    integer t_dim1, t_offset, i__1, i__2;
    doublereal d__1, d__2;

    /* Builtin functions */
    double d_sign(doublereal *, doublereal *);

    /* Local variables */
    static integer j, k, l;
    static doublereal s;
    extern /* Subroutine */ int dscal(integer *, doublereal *, doublereal *, 
	    integer *);
    static doublereal w;
    extern doublereal dasum(integer *, doublereal *, integer *);
    extern /* Subroutine */ int daxpy(integer *, doublereal *, doublereal *, 
	    integer *, doublereal *, integer *);
    static logical lower;
    static integer i1, j1, j2;
    static doublereal tnorm, ynorm, ek;
    static integer kk;
    static doublereal sm, wk, wkm;


/*     dtrco estimates the condition of a double precision triangular */
/*     matrix. */

/*     on entry */

/*        t       double precision(ldt,n) */
/*                t contains the triangular matrix. the zero */
/*                elements of the matrix are not referenced, and */
/*                the corresponding elements of the array can be */
/*                used to store other information. */

/*        ldt     integer */
/*                ldt is the leading dimension of the array t. */

/*        n       integer */
/*                n is the order of the system. */

/*        job     integer */
/*                = 0         t  is lower triangular. */
/*                = nonzero   t  is upper triangular. */

/*     on return */

/*        rcond   double precision */
/*                an estimate of the reciprocal condition of  t . */
/*                for the system  t*x = b , relative perturbations */
/*                in  t  and  b  of size  epsilon  may cause */
/*                relative perturbations in  x  of size  epsilon/rcond . */
/*                if  rcond  is so small that the logical expression */
/*                           1.0 + rcond .eq. 1.0 */
/*                is true, then  t  may be singular to working */
/*                precision.  in particular,  rcond  is zero  if */
/*                exact singularity is detected or the estimate */
/*                underflows. */

/*        z       double precision(n) */
/*                a work vector whose contents are usually unimportant. */
/*                if  t  is close to a singular matrix, then  z  is */
/*                an approximate null vector in the sense that */
/*                norm(a*z) = rcond*norm(a)*norm(z) . */

/*     linpack. this version dated 08/14/78 . */
/*     cleve moler, university of new mexico, argonne national lab. */

/*     subroutines and functions */

/*     blas daxpy,dscal,dasum */
/*     fortran dabs,dmax1,dsign */

/*     internal variables */


    /* Parameter adjustments */
    t_dim1 = *ldt;
    t_offset = t_dim1 + 1;
    t -= t_offset;
    --z__;

    /* Function Body */
    lower = *job == 0;

/*     compute 1-norm of t */

    tnorm = 0.;
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	l = j;
	if (lower) {
	    l = *n + 1 - j;
	}
	i1 = 1;
	if (lower) {
	    i1 = j;
	}
/* Computing MAX */
	d__1 = tnorm, d__2 = dasum(&l, &t[i1 + j * t_dim1], &c__1);
	tnorm = max(d__1,d__2);
/* L10: */
    }

/*     rcond = 1/(norm(t)*(estimate of norm(inverse(t)))) . */
/*     estimate = norm(z)/norm(y) where  t*z = y  and  trans(t)*y = e . */
/*     trans(t)  is the transpose of t . */
/*     the components of  e  are chosen to cause maximum local */
/*     growth in the elements of y . */
/*     the vectors are frequently rescaled to avoid overflow. */

/*     solve trans(t)*y = e */

    ek = 1.;
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	z__[j] = 0.;
/* L20: */
    }
    i__1 = *n;
    for (kk = 1; kk <= i__1; ++kk) {
	k = kk;
	if (lower) {
	    k = *n + 1 - kk;
	}
	if (z__[k] != 0.) {
	    d__1 = -z__[k];
	    ek = d_sign(&ek, &d__1);
	}
	if ((d__1 = ek - z__[k], abs(d__1)) <= (d__2 = t[k + k * t_dim1], abs(
		d__2))) {
	    goto L30;
	}
	s = (d__1 = t[k + k * t_dim1], abs(d__1)) / (d__2 = ek - z__[k], abs(
		d__2));
	dscal(n, &s, &z__[1], &c__1);
	ek = s * ek;
L30:
	wk = ek - z__[k];
	wkm = -ek - z__[k];
	s = abs(wk);
	sm = abs(wkm);
	if (t[k + k * t_dim1] == 0.) {
	    goto L40;
	}
	wk /= t[k + k * t_dim1];
	wkm /= t[k + k * t_dim1];
	goto L50;
L40:
	wk = 1.;
	wkm = 1.;
L50:
	if (kk == *n) {
	    goto L90;
	}
	j1 = k + 1;
	if (lower) {
	    j1 = 1;
	}
	j2 = *n;
	if (lower) {
	    j2 = k - 1;
	}
	i__2 = j2;
	for (j = j1; j <= i__2; ++j) {
	    sm += (d__1 = z__[j] + wkm * t[k + j * t_dim1], abs(d__1));
	    z__[j] += wk * t[k + j * t_dim1];
	    s += (d__1 = z__[j], abs(d__1));
/* L60: */
	}
	if (s >= sm) {
	    goto L80;
	}
	w = wkm - wk;
	wk = wkm;
	i__2 = j2;
	for (j = j1; j <= i__2; ++j) {
	    z__[j] += w * t[k + j * t_dim1];
/* L70: */
	}
L80:
L90:
	z__[k] = wk;
/* L100: */
    }
    s = 1. / dasum(n, &z__[1], &c__1);
    dscal(n, &s, &z__[1], &c__1);

    ynorm = 1.;

/*     solve t*z = y */

    i__1 = *n;
    for (kk = 1; kk <= i__1; ++kk) {
	k = *n + 1 - kk;
	if (lower) {
	    k = kk;
	}
	if ((d__1 = z__[k], abs(d__1)) <= (d__2 = t[k + k * t_dim1], abs(d__2)
		)) {
	    goto L110;
	}
	s = (d__1 = t[k + k * t_dim1], abs(d__1)) / (d__2 = z__[k], abs(d__2))
		;
	dscal(n, &s, &z__[1], &c__1);
	ynorm = s * ynorm;
L110:
	if (t[k + k * t_dim1] != 0.) {
	    z__[k] /= t[k + k * t_dim1];
	}
	if (t[k + k * t_dim1] == 0.) {
	    z__[k] = 1.;
	}
	i1 = 1;
	if (lower) {
	    i1 = k + 1;
	}
	if (kk >= *n) {
	    goto L120;
	}
	w = -z__[k];
	i__2 = *n - kk;
	daxpy(&i__2, &w, &t[i1 + k * t_dim1], &c__1, &z__[i1], &c__1);
L120:
/* L130: */
	;
    }
/*     make znorm = 1.0 */
    s = 1. / dasum(n, &z__[1], &c__1);
    dscal(n, &s, &z__[1], &c__1);
    ynorm = s * ynorm;

    if (tnorm != 0.) {
	*rcond = ynorm / tnorm;
    }
    if (tnorm == 0.) {
	*rcond = 0.;
    }
    return 0;
} /* dtrco_ */
