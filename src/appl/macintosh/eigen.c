/* eigen.f -- translated by f2c (version 19971204).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b246 = 0.;
static doublereal c_b296 = 1.;

/* Subroutine */ int balanc(integer *nm, integer *n, doublereal *a, integer *
	low, integer *igh, doublereal *scale)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;
    doublereal d__1;

    /* Local variables */
    static integer iexc;
    static doublereal c__, f, g;
    static integer i__, j, k, l, m;
    static doublereal r__, s, radix, b2;
    static integer jj;
    static logical noconv;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE BALANCE, */
/*     NUM. MATH. 13, 293-304(1969) BY PARLETT AND REINSCH. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 315-326(1971). */

/*     THIS SUBROUTINE BALANCES A REAL MATRIX AND ISOLATES */
/*     EIGENVALUES WHENEVER POSSIBLE. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        A CONTAINS THE INPUT MATRIX TO BE BALANCED. */

/*     ON OUTPUT */

/*        A CONTAINS THE BALANCED MATRIX. */

/*        LOW AND IGH ARE TWO INTEGERS SUCH THAT A(I,J) */
/*          IS EQUAL TO ZERO IF */
/*           (1) I IS GREATER THAN J AND */
/*           (2) J=1,...,LOW-1 OR I=IGH+1,...,N. */

/*        SCALE CONTAINS INFORMATION DETERMINING THE */
/*           PERMUTATIONS AND SCALING FACTORS USED. */

/*     SUPPOSE THAT THE PRINCIPAL SUBMATRIX IN ROWS LOW THROUGH IGH */
/*     HAS BEEN BALANCED, THAT P(J) DENOTES THE INDEX INTERCHANGED */
/*     WITH J DURING THE PERMUTATION STEP, AND THAT THE ELEMENTS */
/*     OF THE DIAGONAL MATRIX USED ARE DENOTED BY D(I,J).  THEN */
/*        SCALE(J) = P(J),    FOR J = 1,...,LOW-1 */
/*                 = D(J,J),      J = LOW,...,IGH */
/*                 = P(J)         J = IGH+1,...,N. */
/*     THE ORDER IN WHICH THE INTERCHANGES ARE MADE IS N TO IGH+1, */
/*     THEN 1 TO LOW-1. */

/*     NOTE THAT 1 IS RETURNED FOR IGH IF IGH IS ZERO FORMALLY. */

/*     THE ALGOL PROCEDURE EXC CONTAINED IN BALANCE APPEARS IN */
/*     BALANC  IN LINE.  (NOTE THAT THE ALGOL ROLES OF IDENTIFIERS */
/*     K,L HAVE BEEN REVERSED.) */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY */

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    --scale;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    radix = 16.;

    b2 = radix * radix;
    k = 1;
    l = *n;
    goto L100;
/*     .......... IN-LINE PROCEDURE FOR ROW AND */
/*                COLUMN EXCHANGE .......... */
L20:
    scale[m] = (doublereal) j;
    if (j == m) {
	goto L50;
    }

    i__1 = l;
    for (i__ = 1; i__ <= i__1; ++i__) {
	f = a[i__ + j * a_dim1];
	a[i__ + j * a_dim1] = a[i__ + m * a_dim1];
	a[i__ + m * a_dim1] = f;
/* L30: */
    }

    i__1 = *n;
    for (i__ = k; i__ <= i__1; ++i__) {
	f = a[j + i__ * a_dim1];
	a[j + i__ * a_dim1] = a[m + i__ * a_dim1];
	a[m + i__ * a_dim1] = f;
/* L40: */
    }

L50:
    switch (iexc) {
	case 1:  goto L80;
	case 2:  goto L130;
    }
/*     .......... SEARCH FOR ROWS ISOLATING AN EIGENVALUE */
/*                AND PUSH THEM DOWN .......... */
L80:
    if (l == 1) {
	goto L280;
    }
    --l;
/*     .......... FOR J=L STEP -1 UNTIL 1 DO -- .......... */
L100:
    i__1 = l;
    for (jj = 1; jj <= i__1; ++jj) {
	j = l + 1 - jj;

	i__2 = l;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    if (i__ == j) {
		goto L110;
	    }
	    if (a[j + i__ * a_dim1] != 0.) {
		goto L120;
	    }
L110:
	    ;
	}

	m = l;
	iexc = 1;
	goto L20;
L120:
	;
    }

    goto L140;
/*     .......... SEARCH FOR COLUMNS ISOLATING AN EIGENVALUE */
/*                AND PUSH THEM LEFT .......... */
L130:
    ++k;

L140:
    i__1 = l;
    for (j = k; j <= i__1; ++j) {

	i__2 = l;
	for (i__ = k; i__ <= i__2; ++i__) {
	    if (i__ == j) {
		goto L150;
	    }
	    if (a[i__ + j * a_dim1] != 0.) {
		goto L170;
	    }
L150:
	    ;
	}

	m = k;
	iexc = 2;
	goto L20;
L170:
	;
    }
/*     .......... NOW BALANCE THE SUBMATRIX IN ROWS K TO L .......... */
    i__1 = l;
    for (i__ = k; i__ <= i__1; ++i__) {
/* L180: */
	scale[i__] = 1.;
    }
/*     .......... ITERATIVE LOOP FOR NORM REDUCTION .......... */
L190:
    noconv = FALSE_;

    i__1 = l;
    for (i__ = k; i__ <= i__1; ++i__) {
	c__ = 0.;
	r__ = 0.;

	i__2 = l;
	for (j = k; j <= i__2; ++j) {
	    if (j == i__) {
		goto L200;
	    }
	    c__ += (d__1 = a[j + i__ * a_dim1], abs(d__1));
	    r__ += (d__1 = a[i__ + j * a_dim1], abs(d__1));
L200:
	    ;
	}
/*     .......... GUARD AGAINST ZERO C OR R DUE TO UNDERFLOW .......... */
	if (c__ == 0. || r__ == 0.) {
	    goto L270;
	}
	g = r__ / radix;
	f = 1.;
	s = c__ + r__;
L210:
	if (c__ >= g) {
	    goto L220;
	}
	f *= radix;
	c__ *= b2;
	goto L210;
L220:
	g = r__ * radix;
L230:
	if (c__ < g) {
	    goto L240;
	}
	f /= radix;
	c__ /= b2;
	goto L230;
/*     .......... NOW BALANCE .......... */
L240:
	if ((c__ + r__) / f >= s * .95) {
	    goto L270;
	}
	g = 1. / f;
	scale[i__] *= f;
	noconv = TRUE_;

	i__2 = *n;
	for (j = k; j <= i__2; ++j) {
/* L250: */
	    a[i__ + j * a_dim1] *= g;
	}

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
/* L260: */
	    a[j + i__ * a_dim1] *= f;
	}

L270:
	;
    }

    if (noconv) {
	goto L190;
    }

L280:
    *low = k;
    *igh = l;
    return 0;
} /* balanc_ */

/* Subroutine */ int balbak(integer *nm, integer *n, integer *low, integer *
	igh, doublereal *scale, integer *m, doublereal *z__)
{
    /* System generated locals */
    integer z_dim1, z_offset, i__1, i__2;

    /* Local variables */
    static integer i__, j, k;
    static doublereal s;
    static integer ii;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE BALBAK, */
/*     NUM. MATH. 13, 293-304(1969) BY PARLETT AND REINSCH. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 315-326(1971). */

/*     THIS SUBROUTINE FORMS THE EIGENVECTORS OF A REAL GENERAL */
/*     MATRIX BY BACK TRANSFORMING THOSE OF THE CORRESPONDING */
/*     BALANCED MATRIX DETERMINED BY  BALANC. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        LOW AND IGH ARE INTEGERS DETERMINED BY  BALANC. */

/*        SCALE CONTAINS INFORMATION DETERMINING THE PERMUTATIONS */
/*          AND SCALING FACTORS USED BY  BALANC. */

/*        M IS THE NUMBER OF COLUMNS OF Z TO BE BACK TRANSFORMED. */

/*        Z CONTAINS THE REAL AND IMAGINARY PARTS OF THE EIGEN- */
/*          VECTORS TO BE BACK TRANSFORMED IN ITS FIRST M COLUMNS. */

/*     ON OUTPUT */

/*        Z CONTAINS THE REAL AND IMAGINARY PARTS OF THE */
/*          TRANSFORMED EIGENVECTORS IN ITS FIRST M COLUMNS. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY */

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    --scale;
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z__ -= z_offset;

    /* Function Body */
    if (*m == 0) {
	goto L200;
    }
    if (*igh == *low) {
	goto L120;
    }

    i__1 = *igh;
    for (i__ = *low; i__ <= i__1; ++i__) {
	s = scale[i__];
/*     .......... LEFT HAND EIGENVECTORS ARE BACK TRANSFORMED */
/*                IF THE FOREGOING STATEMENT IS REPLACED BY */
/*                S=1.0D0/SCALE(I). .......... */
	i__2 = *m;
	for (j = 1; j <= i__2; ++j) {
/* L100: */
	    z__[i__ + j * z_dim1] *= s;
	}

/* L110: */
    }
/*     ......... FOR I=LOW-1 STEP -1 UNTIL 1, */
/*               IGH+1 STEP 1 UNTIL N DO -- .......... */
L120:
    i__1 = *n;
    for (ii = 1; ii <= i__1; ++ii) {
	i__ = ii;
	if (i__ >= *low && i__ <= *igh) {
	    goto L140;
	}
	if (i__ < *low) {
	    i__ = *low - ii;
	}
	k = (integer) scale[i__];
	if (k == i__) {
	    goto L140;
	}

	i__2 = *m;
	for (j = 1; j <= i__2; ++j) {
	    s = z__[i__ + j * z_dim1];
	    z__[i__ + j * z_dim1] = z__[k + j * z_dim1];
	    z__[k + j * z_dim1] = s;
/* L130: */
	}

L140:
	;
    }

L200:
    return 0;
} /* balbak_ */

/* Subroutine */ int cbabk2(integer *nm, integer *n, integer *low, integer *
	igh, doublereal *scale, integer *m, doublereal *zr, doublereal *zi)
{
    /* System generated locals */
    integer zr_dim1, zr_offset, zi_dim1, zi_offset, i__1, i__2;

    /* Local variables */
    static integer i__, j, k;
    static doublereal s;
    static integer ii;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE */
/*     CBABK2, WHICH IS A COMPLEX VERSION OF BALBAK, */
/*     NUM. MATH. 13, 293-304(1969) BY PARLETT AND REINSCH. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 315-326(1971). */

/*     THIS SUBROUTINE FORMS THE EIGENVECTORS OF A COMPLEX GENERAL */
/*     MATRIX BY BACK TRANSFORMING THOSE OF THE CORRESPONDING */
/*     BALANCED MATRIX DETERMINED BY  CBAL. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        LOW AND IGH ARE INTEGERS DETERMINED BY  CBAL. */

/*        SCALE CONTAINS INFORMATION DETERMINING THE PERMUTATIONS */
/*          AND SCALING FACTORS USED BY  CBAL. */

/*        M IS THE NUMBER OF EIGENVECTORS TO BE BACK TRANSFORMED. */

/*        ZR AND ZI CONTAIN THE REAL AND IMAGINARY PARTS, */
/*          RESPECTIVELY, OF THE EIGENVECTORS TO BE */
/*          BACK TRANSFORMED IN THEIR FIRST M COLUMNS. */

/*     ON OUTPUT */

/*        ZR AND ZI CONTAIN THE REAL AND IMAGINARY PARTS, */
/*          RESPECTIVELY, OF THE TRANSFORMED EIGENVECTORS */
/*          IN THEIR FIRST M COLUMNS. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY */

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    --scale;
    zi_dim1 = *nm;
    zi_offset = zi_dim1 + 1;
    zi -= zi_offset;
    zr_dim1 = *nm;
    zr_offset = zr_dim1 + 1;
    zr -= zr_offset;

    /* Function Body */
    if (*m == 0) {
	goto L200;
    }
    if (*igh == *low) {
	goto L120;
    }

    i__1 = *igh;
    for (i__ = *low; i__ <= i__1; ++i__) {
	s = scale[i__];
/*     .......... LEFT HAND EIGENVECTORS ARE BACK TRANSFORMED */
/*                IF THE FOREGOING STATEMENT IS REPLACED BY */
/*                S=1.0D0/SCALE(I). .......... */
	i__2 = *m;
	for (j = 1; j <= i__2; ++j) {
	    zr[i__ + j * zr_dim1] *= s;
	    zi[i__ + j * zi_dim1] *= s;
/* L100: */
	}

/* L110: */
    }
/*     .......... FOR I=LOW-1 STEP -1 UNTIL 1, */
/*                IGH+1 STEP 1 UNTIL N DO -- .......... */
L120:
    i__1 = *n;
    for (ii = 1; ii <= i__1; ++ii) {
	i__ = ii;
	if (i__ >= *low && i__ <= *igh) {
	    goto L140;
	}
	if (i__ < *low) {
	    i__ = *low - ii;
	}
	k = (integer) scale[i__];
	if (k == i__) {
	    goto L140;
	}

	i__2 = *m;
	for (j = 1; j <= i__2; ++j) {
	    s = zr[i__ + j * zr_dim1];
	    zr[i__ + j * zr_dim1] = zr[k + j * zr_dim1];
	    zr[k + j * zr_dim1] = s;
	    s = zi[i__ + j * zi_dim1];
	    zi[i__ + j * zi_dim1] = zi[k + j * zi_dim1];
	    zi[k + j * zi_dim1] = s;
/* L130: */
	}

L140:
	;
    }

L200:
    return 0;
} /* cbabk2_ */

/* Subroutine */ int cbal(integer *nm, integer *n, doublereal *ar, 
	doublereal *ai, integer *low, integer *igh, doublereal *scale)
{
    /* System generated locals */
    integer ar_dim1, ar_offset, ai_dim1, ai_offset, i__1, i__2;
    doublereal d__1, d__2;

    /* Local variables */
    static integer iexc;
    static doublereal c__, f, g;
    static integer i__, j, k, l, m;
    static doublereal r__, s, radix, b2;
    static integer jj;
    static logical noconv;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE */
/*     CBALANCE, WHICH IS A COMPLEX VERSION OF BALANCE, */
/*     NUM. MATH. 13, 293-304(1969) BY PARLETT AND REINSCH. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 315-326(1971). */

/*     THIS SUBROUTINE BALANCES A COMPLEX MATRIX AND ISOLATES */
/*     EIGENVALUES WHENEVER POSSIBLE. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        AR AND AI CONTAIN THE REAL AND IMAGINARY PARTS, */
/*          RESPECTIVELY, OF THE COMPLEX MATRIX TO BE BALANCED. */

/*     ON OUTPUT */

/*        AR AND AI CONTAIN THE REAL AND IMAGINARY PARTS, */
/*          RESPECTIVELY, OF THE BALANCED MATRIX. */

/*        LOW AND IGH ARE TWO INTEGERS SUCH THAT AR(I,J) AND AI(I,J) */
/*          ARE EQUAL TO ZERO IF */
/*           (1) I IS GREATER THAN J AND */
/*           (2) J=1,...,LOW-1 OR I=IGH+1,...,N. */

/*        SCALE CONTAINS INFORMATION DETERMINING THE */
/*           PERMUTATIONS AND SCALING FACTORS USED. */

/*     SUPPOSE THAT THE PRINCIPAL SUBMATRIX IN ROWS LOW THROUGH IGH */
/*     HAS BEEN BALANCED, THAT P(J) DENOTES THE INDEX INTERCHANGED */
/*     WITH J DURING THE PERMUTATION STEP, AND THAT THE ELEMENTS */
/*     OF THE DIAGONAL MATRIX USED ARE DENOTED BY D(I,J).  THEN */
/*        SCALE(J) = P(J),    FOR J = 1,...,LOW-1 */
/*                 = D(J,J)       J = LOW,...,IGH */
/*                 = P(J)         J = IGH+1,...,N. */
/*     THE ORDER IN WHICH THE INTERCHANGES ARE MADE IS N TO IGH+1, */
/*     THEN 1 TO LOW-1. */

/*     NOTE THAT 1 IS RETURNED FOR IGH IF IGH IS ZERO FORMALLY. */

/*     THE ALGOL PROCEDURE EXC CONTAINED IN CBALANCE APPEARS IN */
/*     CBAL  IN LINE.  (NOTE THAT THE ALGOL ROLES OF IDENTIFIERS */
/*     K,L HAVE BEEN REVERSED.) */

/*     ARITHMETIC IS REAL THROUGHOUT. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY */

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    --scale;
    ai_dim1 = *nm;
    ai_offset = ai_dim1 + 1;
    ai -= ai_offset;
    ar_dim1 = *nm;
    ar_offset = ar_dim1 + 1;
    ar -= ar_offset;

    /* Function Body */
    radix = 16.;

    b2 = radix * radix;
    k = 1;
    l = *n;
    goto L100;
/*     .......... IN-LINE PROCEDURE FOR ROW AND */
/*                COLUMN EXCHANGE .......... */
L20:
    scale[m] = (doublereal) j;
    if (j == m) {
	goto L50;
    }

    i__1 = l;
    for (i__ = 1; i__ <= i__1; ++i__) {
	f = ar[i__ + j * ar_dim1];
	ar[i__ + j * ar_dim1] = ar[i__ + m * ar_dim1];
	ar[i__ + m * ar_dim1] = f;
	f = ai[i__ + j * ai_dim1];
	ai[i__ + j * ai_dim1] = ai[i__ + m * ai_dim1];
	ai[i__ + m * ai_dim1] = f;
/* L30: */
    }

    i__1 = *n;
    for (i__ = k; i__ <= i__1; ++i__) {
	f = ar[j + i__ * ar_dim1];
	ar[j + i__ * ar_dim1] = ar[m + i__ * ar_dim1];
	ar[m + i__ * ar_dim1] = f;
	f = ai[j + i__ * ai_dim1];
	ai[j + i__ * ai_dim1] = ai[m + i__ * ai_dim1];
	ai[m + i__ * ai_dim1] = f;
/* L40: */
    }

L50:
    switch (iexc) {
	case 1:  goto L80;
	case 2:  goto L130;
    }
/*     .......... SEARCH FOR ROWS ISOLATING AN EIGENVALUE */
/*                AND PUSH THEM DOWN .......... */
L80:
    if (l == 1) {
	goto L280;
    }
    --l;
/*     .......... FOR J=L STEP -1 UNTIL 1 DO -- .......... */
L100:
    i__1 = l;
    for (jj = 1; jj <= i__1; ++jj) {
	j = l + 1 - jj;

	i__2 = l;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    if (i__ == j) {
		goto L110;
	    }
	    if (ar[j + i__ * ar_dim1] != 0. || ai[j + i__ * ai_dim1] != 0.) {
		goto L120;
	    }
L110:
	    ;
	}

	m = l;
	iexc = 1;
	goto L20;
L120:
	;
    }

    goto L140;
/*     .......... SEARCH FOR COLUMNS ISOLATING AN EIGENVALUE */
/*                AND PUSH THEM LEFT .......... */
L130:
    ++k;

L140:
    i__1 = l;
    for (j = k; j <= i__1; ++j) {

	i__2 = l;
	for (i__ = k; i__ <= i__2; ++i__) {
	    if (i__ == j) {
		goto L150;
	    }
	    if (ar[i__ + j * ar_dim1] != 0. || ai[i__ + j * ai_dim1] != 0.) {
		goto L170;
	    }
L150:
	    ;
	}

	m = k;
	iexc = 2;
	goto L20;
L170:
	;
    }
/*     .......... NOW BALANCE THE SUBMATRIX IN ROWS K TO L .......... */
    i__1 = l;
    for (i__ = k; i__ <= i__1; ++i__) {
/* L180: */
	scale[i__] = 1.;
    }
/*     .......... ITERATIVE LOOP FOR NORM REDUCTION .......... */
L190:
    noconv = FALSE_;

    i__1 = l;
    for (i__ = k; i__ <= i__1; ++i__) {
	c__ = 0.;
	r__ = 0.;

	i__2 = l;
	for (j = k; j <= i__2; ++j) {
	    if (j == i__) {
		goto L200;
	    }
	    c__ = c__ + (d__1 = ar[j + i__ * ar_dim1], abs(d__1)) + (d__2 = 
		    ai[j + i__ * ai_dim1], abs(d__2));
	    r__ = r__ + (d__1 = ar[i__ + j * ar_dim1], abs(d__1)) + (d__2 = 
		    ai[i__ + j * ai_dim1], abs(d__2));
L200:
	    ;
	}
/*     .......... GUARD AGAINST ZERO C OR R DUE TO UNDERFLOW .......... */
	if (c__ == 0. || r__ == 0.) {
	    goto L270;
	}
	g = r__ / radix;
	f = 1.;
	s = c__ + r__;
L210:
	if (c__ >= g) {
	    goto L220;
	}
	f *= radix;
	c__ *= b2;
	goto L210;
L220:
	g = r__ * radix;
L230:
	if (c__ < g) {
	    goto L240;
	}
	f /= radix;
	c__ /= b2;
	goto L230;
/*     .......... NOW BALANCE .......... */
L240:
	if ((c__ + r__) / f >= s * .95) {
	    goto L270;
	}
	g = 1. / f;
	scale[i__] *= f;
	noconv = TRUE_;

	i__2 = *n;
	for (j = k; j <= i__2; ++j) {
	    ar[i__ + j * ar_dim1] *= g;
	    ai[i__ + j * ai_dim1] *= g;
/* L250: */
	}

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    ar[j + i__ * ar_dim1] *= f;
	    ai[j + i__ * ai_dim1] *= f;
/* L260: */
	}

L270:
	;
    }

    if (noconv) {
	goto L190;
    }

L280:
    *low = k;
    *igh = l;
    return 0;
} /* cbal_ */

/* Subroutine */ int cdiv(doublereal *ar, doublereal *ai, doublereal *br, 
	doublereal *bi, doublereal *cr, doublereal *ci)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Local variables */
    static doublereal s, ais, bis, ars, brs;


/*     COMPLEX DIVISION, (CR,CI) = (AR,AI)/(BR,BI) */

    s = abs(*br) + abs(*bi);
    ars = *ar / s;
    ais = *ai / s;
    brs = *br / s;
    bis = *bi / s;
/* Computing 2nd power */
    d__1 = brs;
/* Computing 2nd power */
    d__2 = bis;
    s = d__1 * d__1 + d__2 * d__2;
    *cr = (ars * brs + ais * bis) / s;
    *ci = (ais * brs - ars * bis) / s;
    return 0;
} /* cdiv_ */

/* Subroutine */ int comqr(integer *nm, integer *n, integer *low, integer *
	igh, doublereal *hr, doublereal *hi, doublereal *wr, doublereal *wi, 
	integer *ierr)
{
    /* System generated locals */
    integer hr_dim1, hr_offset, hi_dim1, hi_offset, i__1, i__2;
    doublereal d__1, d__2, d__3, d__4;

    /* Local variables */
    extern /* Subroutine */ int cdiv(doublereal *, doublereal *, doublereal *
	    , doublereal *, doublereal *, doublereal *);
    static doublereal norm;
    static integer i__, j, l, en, ll;
    static doublereal si, ti, xi, yi, sr, tr, xr, yr;
    extern doublereal pythag(doublereal *, doublereal *);
    extern /* Subroutine */ int csroot(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    static integer lp1, itn, its;
    static doublereal zzi, zzr;
    static integer enm1;
    static doublereal tst1, tst2;



/*     THIS SUBROUTINE IS A TRANSLATION OF A UNITARY ANALOGUE OF THE */
/*     ALGOL PROCEDURE  COMLR, NUM. MATH. 12, 369-376(1968) BY MARTIN */
/*     AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 396-403(1971). */
/*     THE UNITARY ANALOGUE SUBSTITUTES THE QR ALGORITHM OF FRANCIS */
/*     (COMP. JOUR. 4, 332-345(1962)) FOR THE LR ALGORITHM. */

/*     THIS SUBROUTINE FINDS THE EIGENVALUES OF A COMPLEX */
/*     UPPER HESSENBERG MATRIX BY THE QR METHOD. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING */
/*          SUBROUTINE  CBAL.  IF  CBAL  HAS NOT BEEN USED, */
/*          SET LOW=1, IGH=N. */

/*        HR AND HI CONTAIN THE REAL AND IMAGINARY PARTS, */
/*          RESPECTIVELY, OF THE COMPLEX UPPER HESSENBERG MATRIX. */
/*          THEIR LOWER TRIANGLES BELOW THE SUBDIAGONAL CONTAIN */
/*          INFORMATION ABOUT THE UNITARY TRANSFORMATIONS USED IN */
/*          THE REDUCTION BY  CORTH, IF PERFORMED. */

/*     ON OUTPUT */

/*        THE UPPER HESSENBERG PORTIONS OF HR AND HI HAVE BEEN */
/*          DESTROYED.  THEREFORE, THEY MUST BE SAVED BEFORE */
/*          CALLING  COMQR  IF SUBSEQUENT CALCULATION OF */
/*          EIGENVECTORS IS TO BE PERFORMED. */

/*        WR AND WI CONTAIN THE REAL AND IMAGINARY PARTS, */
/*          RESPECTIVELY, OF THE EIGENVALUES.  IF AN ERROR */
/*          EXIT IS MADE, THE EIGENVALUES SHOULD BE CORRECT */
/*          FOR INDICES IERR+1,...,N. */

/*        IERR IS SET TO */
/*          ZERO       FOR NORMAL RETURN, */
/*          J          IF THE LIMIT OF 30*N ITERATIONS IS EXHAUSTED */
/*                     WHILE THE J-TH EIGENVALUE IS BEING SOUGHT. */

/*     CALLS CDIV FOR COMPLEX DIVISION. */
/*     CALLS CSROOT FOR COMPLEX SQUARE ROOT. */
/*     CALLS PYTHAG FOR  DSQRT(A*A + B*B) . */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY */

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ */

/*     unnecessary initialization of L to keep g77 -Wall happy */

    /* Parameter adjustments */
    --wi;
    --wr;
    hi_dim1 = *nm;
    hi_offset = hi_dim1 + 1;
    hi -= hi_offset;
    hr_dim1 = *nm;
    hr_offset = hr_dim1 + 1;
    hr -= hr_offset;

    /* Function Body */
    l = 0;

    *ierr = 0;
    if (*low == *igh) {
	goto L180;
    }
/*     .......... CREATE REAL SUBDIAGONAL ELEMENTS .......... */
    l = *low + 1;

    i__1 = *igh;
    for (i__ = l; i__ <= i__1; ++i__) {
/* Computing MIN */
	i__2 = i__ + 1;
	ll = min(i__2,*igh);
	if (hi[i__ + (i__ - 1) * hi_dim1] == 0.) {
	    goto L170;
	}
	norm = pythag(&hr[i__ + (i__ - 1) * hr_dim1], &hi[i__ + (i__ - 1) * 
		hi_dim1]);
	yr = hr[i__ + (i__ - 1) * hr_dim1] / norm;
	yi = hi[i__ + (i__ - 1) * hi_dim1] / norm;
	hr[i__ + (i__ - 1) * hr_dim1] = norm;
	hi[i__ + (i__ - 1) * hi_dim1] = 0.;

	i__2 = *igh;
	for (j = i__; j <= i__2; ++j) {
	    si = yr * hi[i__ + j * hi_dim1] - yi * hr[i__ + j * hr_dim1];
	    hr[i__ + j * hr_dim1] = yr * hr[i__ + j * hr_dim1] + yi * hi[i__ 
		    + j * hi_dim1];
	    hi[i__ + j * hi_dim1] = si;
/* L155: */
	}

	i__2 = ll;
	for (j = *low; j <= i__2; ++j) {
	    si = yr * hi[j + i__ * hi_dim1] + yi * hr[j + i__ * hr_dim1];
	    hr[j + i__ * hr_dim1] = yr * hr[j + i__ * hr_dim1] - yi * hi[j + 
		    i__ * hi_dim1];
	    hi[j + i__ * hi_dim1] = si;
/* L160: */
	}

L170:
	;
    }
/*     .......... STORE ROOTS ISOLATED BY CBAL .......... */
L180:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (i__ >= *low && i__ <= *igh) {
	    goto L200;
	}
	wr[i__] = hr[i__ + i__ * hr_dim1];
	wi[i__] = hi[i__ + i__ * hi_dim1];
L200:
	;
    }

    en = *igh;
    tr = 0.;
    ti = 0.;
    itn = *n * 30;
/*     .......... SEARCH FOR NEXT EIGENVALUE .......... */
L220:
    if (en < *low) {
	goto L1001;
    }
    its = 0;
    enm1 = en - 1;
/*     .......... LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT */
/*                FOR L=EN STEP -1 UNTIL LOW D0 -- .......... */
L240:
    i__1 = en;
    for (ll = *low; ll <= i__1; ++ll) {
	l = en + *low - ll;
	if (l == *low) {
	    goto L300;
	}
	tst1 = (d__1 = hr[l - 1 + (l - 1) * hr_dim1], abs(d__1)) + (d__2 = hi[
		l - 1 + (l - 1) * hi_dim1], abs(d__2)) + (d__3 = hr[l + l * 
		hr_dim1], abs(d__3)) + (d__4 = hi[l + l * hi_dim1], abs(d__4))
		;
	tst2 = tst1 + (d__1 = hr[l + (l - 1) * hr_dim1], abs(d__1));
	if (tst2 == tst1) {
	    goto L300;
	}
/* L260: */
    }
/*     .......... FORM SHIFT .......... */
L300:
    if (l == en) {
	goto L660;
    }
    if (itn == 0) {
	goto L1000;
    }
    if (its == 10 || its == 20) {
	goto L320;
    }
    sr = hr[en + en * hr_dim1];
    si = hi[en + en * hi_dim1];
    xr = hr[enm1 + en * hr_dim1] * hr[en + enm1 * hr_dim1];
    xi = hi[enm1 + en * hi_dim1] * hr[en + enm1 * hr_dim1];
    if (xr == 0. && xi == 0.) {
	goto L340;
    }
    yr = (hr[enm1 + enm1 * hr_dim1] - sr) / 2.;
    yi = (hi[enm1 + enm1 * hi_dim1] - si) / 2.;
/* Computing 2nd power */
    d__2 = yr;
/* Computing 2nd power */
    d__3 = yi;
    d__1 = d__2 * d__2 - d__3 * d__3 + xr;
    d__4 = yr * 2. * yi + xi;
    csroot(&d__1, &d__4, &zzr, &zzi);
    if (yr * zzr + yi * zzi >= 0.) {
	goto L310;
    }
    zzr = -zzr;
    zzi = -zzi;
L310:
    d__1 = yr + zzr;
    d__2 = yi + zzi;
    cdiv(&xr, &xi, &d__1, &d__2, &xr, &xi);
    sr -= xr;
    si -= xi;
    goto L340;
/*     .......... FORM EXCEPTIONAL SHIFT .......... */
L320:
    sr = (d__1 = hr[en + enm1 * hr_dim1], abs(d__1)) + (d__2 = hr[enm1 + (en 
	    - 2) * hr_dim1], abs(d__2));
    si = 0.;

L340:
    i__1 = en;
    for (i__ = *low; i__ <= i__1; ++i__) {
	hr[i__ + i__ * hr_dim1] -= sr;
	hi[i__ + i__ * hi_dim1] -= si;
/* L360: */
    }

    tr += sr;
    ti += si;
    ++its;
    --itn;
/*     .......... REDUCE TO TRIANGLE (ROWS) .......... */
    lp1 = l + 1;

    i__1 = en;
    for (i__ = lp1; i__ <= i__1; ++i__) {
	sr = hr[i__ + (i__ - 1) * hr_dim1];
	hr[i__ + (i__ - 1) * hr_dim1] = 0.;
	d__1 = pythag(&hr[i__ - 1 + (i__ - 1) * hr_dim1], &hi[i__ - 1 + (i__ 
		- 1) * hi_dim1]);
	norm = pythag(&d__1, &sr);
	xr = hr[i__ - 1 + (i__ - 1) * hr_dim1] / norm;
	wr[i__ - 1] = xr;
	xi = hi[i__ - 1 + (i__ - 1) * hi_dim1] / norm;
	wi[i__ - 1] = xi;
	hr[i__ - 1 + (i__ - 1) * hr_dim1] = norm;
	hi[i__ - 1 + (i__ - 1) * hi_dim1] = 0.;
	hi[i__ + (i__ - 1) * hi_dim1] = sr / norm;

	i__2 = en;
	for (j = i__; j <= i__2; ++j) {
	    yr = hr[i__ - 1 + j * hr_dim1];
	    yi = hi[i__ - 1 + j * hi_dim1];
	    zzr = hr[i__ + j * hr_dim1];
	    zzi = hi[i__ + j * hi_dim1];
	    hr[i__ - 1 + j * hr_dim1] = xr * yr + xi * yi + hi[i__ + (i__ - 1)
		     * hi_dim1] * zzr;
	    hi[i__ - 1 + j * hi_dim1] = xr * yi - xi * yr + hi[i__ + (i__ - 1)
		     * hi_dim1] * zzi;
	    hr[i__ + j * hr_dim1] = xr * zzr - xi * zzi - hi[i__ + (i__ - 1) *
		     hi_dim1] * yr;
	    hi[i__ + j * hi_dim1] = xr * zzi + xi * zzr - hi[i__ + (i__ - 1) *
		     hi_dim1] * yi;
/* L490: */
	}

/* L500: */
    }

    si = hi[en + en * hi_dim1];
    if (si == 0.) {
	goto L540;
    }
    norm = pythag(&hr[en + en * hr_dim1], &si);
    sr = hr[en + en * hr_dim1] / norm;
    si /= norm;
    hr[en + en * hr_dim1] = norm;
    hi[en + en * hi_dim1] = 0.;
/*     .......... INVERSE OPERATION (COLUMNS) .......... */
L540:
    i__1 = en;
    for (j = lp1; j <= i__1; ++j) {
	xr = wr[j - 1];
	xi = wi[j - 1];

	i__2 = j;
	for (i__ = l; i__ <= i__2; ++i__) {
	    yr = hr[i__ + (j - 1) * hr_dim1];
	    yi = 0.;
	    zzr = hr[i__ + j * hr_dim1];
	    zzi = hi[i__ + j * hi_dim1];
	    if (i__ == j) {
		goto L560;
	    }
	    yi = hi[i__ + (j - 1) * hi_dim1];
	    hi[i__ + (j - 1) * hi_dim1] = xr * yi + xi * yr + hi[j + (j - 1) *
		     hi_dim1] * zzi;
L560:
	    hr[i__ + (j - 1) * hr_dim1] = xr * yr - xi * yi + hi[j + (j - 1) *
		     hi_dim1] * zzr;
	    hr[i__ + j * hr_dim1] = xr * zzr + xi * zzi - hi[j + (j - 1) * 
		    hi_dim1] * yr;
	    hi[i__ + j * hi_dim1] = xr * zzi - xi * zzr - hi[j + (j - 1) * 
		    hi_dim1] * yi;
/* L580: */
	}

/* L600: */
    }

    if (si == 0.) {
	goto L240;
    }

    i__1 = en;
    for (i__ = l; i__ <= i__1; ++i__) {
	yr = hr[i__ + en * hr_dim1];
	yi = hi[i__ + en * hi_dim1];
	hr[i__ + en * hr_dim1] = sr * yr - si * yi;
	hi[i__ + en * hi_dim1] = sr * yi + si * yr;
/* L630: */
    }

    goto L240;
/*     .......... A ROOT FOUND .......... */
L660:
    wr[en] = hr[en + en * hr_dim1] + tr;
    wi[en] = hi[en + en * hi_dim1] + ti;
    en = enm1;
    goto L220;
/*     .......... SET ERROR -- ALL EIGENVALUES HAVE NOT */
/*                CONVERGED AFTER 30*N ITERATIONS .......... */
L1000:
    *ierr = en;
L1001:
    return 0;
} /* comqr_ */

/* Subroutine */ int comqr2(integer *nm, integer *n, integer *low, integer *
	igh, doublereal *ortr, doublereal *orti, doublereal *hr, doublereal *
	hi, doublereal *wr, doublereal *wi, doublereal *zr, doublereal *zi, 
	integer *ierr)
{
    /* System generated locals */
    integer hr_dim1, hr_offset, hi_dim1, hi_offset, zr_dim1, zr_offset, 
	    zi_dim1, zi_offset, i__1, i__2, i__3;
    doublereal d__1, d__2, d__3, d__4;

    /* Local variables */
    static integer iend;
    extern /* Subroutine */ int cdiv(doublereal *, doublereal *, doublereal *
	    , doublereal *, doublereal *, doublereal *);
    static doublereal norm;
    static integer i__, j, k, l, m, ii, en, jj, ll, nn;
    static doublereal si, ti, xi, yi, sr, tr, xr, yr;
    extern doublereal pythag(doublereal *, doublereal *);
    extern /* Subroutine */ int csroot(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    static integer ip1, lp1, itn, its;
    static doublereal zzi, zzr;
    static integer enm1;
    static doublereal tst1, tst2;



/*     THIS SUBROUTINE IS A TRANSLATION OF A UNITARY ANALOGUE OF THE */
/*     ALGOL PROCEDURE  COMLR2, NUM. MATH. 16, 181-204(1970) BY PETERS */
/*     AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 372-395(1971). */
/*     THE UNITARY ANALOGUE SUBSTITUTES THE QR ALGORITHM OF FRANCIS */
/*     (COMP. JOUR. 4, 332-345(1962)) FOR THE LR ALGORITHM. */

/*     THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS */
/*     OF A COMPLEX UPPER HESSENBERG MATRIX BY THE QR */
/*     METHOD.  THE EIGENVECTORS OF A COMPLEX GENERAL MATRIX */
/*     CAN ALSO BE FOUND IF  CORTH  HAS BEEN USED TO REDUCE */
/*     THIS GENERAL MATRIX TO HESSENBERG FORM. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING */
/*          SUBROUTINE  CBAL.  IF  CBAL  HAS NOT BEEN USED, */
/*          SET LOW=1, IGH=N. */

/*        ORTR AND ORTI CONTAIN INFORMATION ABOUT THE UNITARY TRANS- */
/*          FORMATIONS USED IN THE REDUCTION BY  CORTH, IF PERFORMED. */
/*          ONLY ELEMENTS LOW THROUGH IGH ARE USED.  IF THE EIGENVECTORS */
/*          OF THE HESSENBERG MATRIX ARE DESIRED, SET ORTR(J) AND */
/*          ORTI(J) TO 0.0D0 FOR THESE ELEMENTS. */

/*        HR AND HI CONTAIN THE REAL AND IMAGINARY PARTS, */
/*          RESPECTIVELY, OF THE COMPLEX UPPER HESSENBERG MATRIX. */
/*          THEIR LOWER TRIANGLES BELOW THE SUBDIAGONAL CONTAIN FURTHER */
/*          INFORMATION ABOUT THE TRANSFORMATIONS WHICH WERE USED IN THE */
/*          REDUCTION BY  CORTH, IF PERFORMED.  IF THE EIGENVECTORS OF */
/*          THE HESSENBERG MATRIX ARE DESIRED, THESE ELEMENTS MAY BE */
/*          ARBITRARY. */

/*     ON OUTPUT */

/*        ORTR, ORTI, AND THE UPPER HESSENBERG PORTIONS OF HR AND HI */
/*          HAVE BEEN DESTROYED. */

/*        WR AND WI CONTAIN THE REAL AND IMAGINARY PARTS, */
/*          RESPECTIVELY, OF THE EIGENVALUES.  IF AN ERROR */
/*          EXIT IS MADE, THE EIGENVALUES SHOULD BE CORRECT */
/*          FOR INDICES IERR+1,...,N. */

/*        ZR AND ZI CONTAIN THE REAL AND IMAGINARY PARTS, */
/*          RESPECTIVELY, OF THE EIGENVECTORS.  THE EIGENVECTORS */
/*          ARE UNNORMALIZED.  IF AN ERROR EXIT IS MADE, NONE OF */
/*          THE EIGENVECTORS HAS BEEN FOUND. */

/*        IERR IS SET TO */
/*          ZERO       FOR NORMAL RETURN, */
/*          J          IF THE LIMIT OF 30*N ITERATIONS IS EXHAUSTED */
/*                     WHILE THE J-TH EIGENVALUE IS BEING SOUGHT. */

/*     CALLS CDIV FOR COMPLEX DIVISION. */
/*     CALLS CSROOT FOR COMPLEX SQUARE ROOT. */
/*     CALLS PYTHAG FOR  DSQRT(A*A + B*B) . */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY */

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ */

/*     unnecessary initialization of L to keep g77 -Wall happy */

    /* Parameter adjustments */
    zi_dim1 = *nm;
    zi_offset = zi_dim1 + 1;
    zi -= zi_offset;
    zr_dim1 = *nm;
    zr_offset = zr_dim1 + 1;
    zr -= zr_offset;
    --wi;
    --wr;
    hi_dim1 = *nm;
    hi_offset = hi_dim1 + 1;
    hi -= hi_offset;
    hr_dim1 = *nm;
    hr_offset = hr_dim1 + 1;
    hr -= hr_offset;
    --orti;
    --ortr;

    /* Function Body */
    l = 0;

    *ierr = 0;
/*     .......... INITIALIZE EIGENVECTOR MATRIX .......... */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {

	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    zr[i__ + j * zr_dim1] = 0.;
	    zi[i__ + j * zi_dim1] = 0.;
/* L100: */
	}
	zr[j + j * zr_dim1] = 1.;
/* L101: */
    }
/*     .......... FORM THE MATRIX OF ACCUMULATED TRANSFORMATIONS */
/*                FROM THE INFORMATION LEFT BY CORTH .......... */
    iend = *igh - *low - 1;
    if (iend < 0) {
	goto L180;
    } else if (iend == 0) {
	goto L150;
    } else {
	goto L105;
    }
/*     .......... FOR I=IGH-1 STEP -1 UNTIL LOW+1 DO -- .......... */
L105:
    i__1 = iend;
    for (ii = 1; ii <= i__1; ++ii) {
	i__ = *igh - ii;
	if (ortr[i__] == 0. && orti[i__] == 0.) {
	    goto L140;
	}
	if (hr[i__ + (i__ - 1) * hr_dim1] == 0. && hi[i__ + (i__ - 1) * 
		hi_dim1] == 0.) {
	    goto L140;
	}
/*     .......... NORM BELOW IS NEGATIVE OF H FORMED IN CORTH .......... */
	norm = hr[i__ + (i__ - 1) * hr_dim1] * ortr[i__] + hi[i__ + (i__ - 1) 
		* hi_dim1] * orti[i__];
	ip1 = i__ + 1;

	i__2 = *igh;
	for (k = ip1; k <= i__2; ++k) {
	    ortr[k] = hr[k + (i__ - 1) * hr_dim1];
	    orti[k] = hi[k + (i__ - 1) * hi_dim1];
/* L110: */
	}

	i__2 = *igh;
	for (j = i__; j <= i__2; ++j) {
	    sr = 0.;
	    si = 0.;

	    i__3 = *igh;
	    for (k = i__; k <= i__3; ++k) {
		sr = sr + ortr[k] * zr[k + j * zr_dim1] + orti[k] * zi[k + j *
			 zi_dim1];
		si = si + ortr[k] * zi[k + j * zi_dim1] - orti[k] * zr[k + j *
			 zr_dim1];
/* L115: */
	    }

	    sr /= norm;
	    si /= norm;

	    i__3 = *igh;
	    for (k = i__; k <= i__3; ++k) {
		zr[k + j * zr_dim1] = zr[k + j * zr_dim1] + sr * ortr[k] - si 
			* orti[k];
		zi[k + j * zi_dim1] = zi[k + j * zi_dim1] + sr * orti[k] + si 
			* ortr[k];
/* L120: */
	    }

/* L130: */
	}

L140:
	;
    }
/*     .......... CREATE REAL SUBDIAGONAL ELEMENTS .......... */
L150:
    l = *low + 1;

    i__1 = *igh;
    for (i__ = l; i__ <= i__1; ++i__) {
/* Computing MIN */
	i__2 = i__ + 1;
	ll = min(i__2,*igh);
	if (hi[i__ + (i__ - 1) * hi_dim1] == 0.) {
	    goto L170;
	}
	norm = pythag(&hr[i__ + (i__ - 1) * hr_dim1], &hi[i__ + (i__ - 1) * 
		hi_dim1]);
	yr = hr[i__ + (i__ - 1) * hr_dim1] / norm;
	yi = hi[i__ + (i__ - 1) * hi_dim1] / norm;
	hr[i__ + (i__ - 1) * hr_dim1] = norm;
	hi[i__ + (i__ - 1) * hi_dim1] = 0.;

	i__2 = *n;
	for (j = i__; j <= i__2; ++j) {
	    si = yr * hi[i__ + j * hi_dim1] - yi * hr[i__ + j * hr_dim1];
	    hr[i__ + j * hr_dim1] = yr * hr[i__ + j * hr_dim1] + yi * hi[i__ 
		    + j * hi_dim1];
	    hi[i__ + j * hi_dim1] = si;
/* L155: */
	}

	i__2 = ll;
	for (j = 1; j <= i__2; ++j) {
	    si = yr * hi[j + i__ * hi_dim1] + yi * hr[j + i__ * hr_dim1];
	    hr[j + i__ * hr_dim1] = yr * hr[j + i__ * hr_dim1] - yi * hi[j + 
		    i__ * hi_dim1];
	    hi[j + i__ * hi_dim1] = si;
/* L160: */
	}

	i__2 = *igh;
	for (j = *low; j <= i__2; ++j) {
	    si = yr * zi[j + i__ * zi_dim1] + yi * zr[j + i__ * zr_dim1];
	    zr[j + i__ * zr_dim1] = yr * zr[j + i__ * zr_dim1] - yi * zi[j + 
		    i__ * zi_dim1];
	    zi[j + i__ * zi_dim1] = si;
/* L165: */
	}

L170:
	;
    }
/*     .......... STORE ROOTS ISOLATED BY CBAL .......... */
L180:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (i__ >= *low && i__ <= *igh) {
	    goto L200;
	}
	wr[i__] = hr[i__ + i__ * hr_dim1];
	wi[i__] = hi[i__ + i__ * hi_dim1];
L200:
	;
    }

    en = *igh;
    tr = 0.;
    ti = 0.;
    itn = *n * 30;
/*     .......... SEARCH FOR NEXT EIGENVALUE .......... */
L220:
    if (en < *low) {
	goto L680;
    }
    its = 0;
    enm1 = en - 1;
/*     .......... LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT */
/*                FOR L=EN STEP -1 UNTIL LOW DO -- .......... */
L240:
    i__1 = en;
    for (ll = *low; ll <= i__1; ++ll) {
	l = en + *low - ll;
	if (l == *low) {
	    goto L300;
	}
	tst1 = (d__1 = hr[l - 1 + (l - 1) * hr_dim1], abs(d__1)) + (d__2 = hi[
		l - 1 + (l - 1) * hi_dim1], abs(d__2)) + (d__3 = hr[l + l * 
		hr_dim1], abs(d__3)) + (d__4 = hi[l + l * hi_dim1], abs(d__4))
		;
	tst2 = tst1 + (d__1 = hr[l + (l - 1) * hr_dim1], abs(d__1));
	if (tst2 == tst1) {
	    goto L300;
	}
/* L260: */
    }
/*     .......... FORM SHIFT .......... */
L300:
    if (l == en) {
	goto L660;
    }
    if (itn == 0) {
	goto L1000;
    }
    if (its == 10 || its == 20) {
	goto L320;
    }
    sr = hr[en + en * hr_dim1];
    si = hi[en + en * hi_dim1];
    xr = hr[enm1 + en * hr_dim1] * hr[en + enm1 * hr_dim1];
    xi = hi[enm1 + en * hi_dim1] * hr[en + enm1 * hr_dim1];
    if (xr == 0. && xi == 0.) {
	goto L340;
    }
    yr = (hr[enm1 + enm1 * hr_dim1] - sr) / 2.;
    yi = (hi[enm1 + enm1 * hi_dim1] - si) / 2.;
/* Computing 2nd power */
    d__2 = yr;
/* Computing 2nd power */
    d__3 = yi;
    d__1 = d__2 * d__2 - d__3 * d__3 + xr;
    d__4 = yr * 2. * yi + xi;
    csroot(&d__1, &d__4, &zzr, &zzi);
    if (yr * zzr + yi * zzi >= 0.) {
	goto L310;
    }
    zzr = -zzr;
    zzi = -zzi;
L310:
    d__1 = yr + zzr;
    d__2 = yi + zzi;
    cdiv(&xr, &xi, &d__1, &d__2, &xr, &xi);
    sr -= xr;
    si -= xi;
    goto L340;
/*     .......... FORM EXCEPTIONAL SHIFT .......... */
L320:
    sr = (d__1 = hr[en + enm1 * hr_dim1], abs(d__1)) + (d__2 = hr[enm1 + (en 
	    - 2) * hr_dim1], abs(d__2));
    si = 0.;

L340:
    i__1 = en;
    for (i__ = *low; i__ <= i__1; ++i__) {
	hr[i__ + i__ * hr_dim1] -= sr;
	hi[i__ + i__ * hi_dim1] -= si;
/* L360: */
    }

    tr += sr;
    ti += si;
    ++its;
    --itn;
/*     .......... REDUCE TO TRIANGLE (ROWS) .......... */
    lp1 = l + 1;

    i__1 = en;
    for (i__ = lp1; i__ <= i__1; ++i__) {
	sr = hr[i__ + (i__ - 1) * hr_dim1];
	hr[i__ + (i__ - 1) * hr_dim1] = 0.;
	d__1 = pythag(&hr[i__ - 1 + (i__ - 1) * hr_dim1], &hi[i__ - 1 + (i__ 
		- 1) * hi_dim1]);
	norm = pythag(&d__1, &sr);
	xr = hr[i__ - 1 + (i__ - 1) * hr_dim1] / norm;
	wr[i__ - 1] = xr;
	xi = hi[i__ - 1 + (i__ - 1) * hi_dim1] / norm;
	wi[i__ - 1] = xi;
	hr[i__ - 1 + (i__ - 1) * hr_dim1] = norm;
	hi[i__ - 1 + (i__ - 1) * hi_dim1] = 0.;
	hi[i__ + (i__ - 1) * hi_dim1] = sr / norm;

	i__2 = *n;
	for (j = i__; j <= i__2; ++j) {
	    yr = hr[i__ - 1 + j * hr_dim1];
	    yi = hi[i__ - 1 + j * hi_dim1];
	    zzr = hr[i__ + j * hr_dim1];
	    zzi = hi[i__ + j * hi_dim1];
	    hr[i__ - 1 + j * hr_dim1] = xr * yr + xi * yi + hi[i__ + (i__ - 1)
		     * hi_dim1] * zzr;
	    hi[i__ - 1 + j * hi_dim1] = xr * yi - xi * yr + hi[i__ + (i__ - 1)
		     * hi_dim1] * zzi;
	    hr[i__ + j * hr_dim1] = xr * zzr - xi * zzi - hi[i__ + (i__ - 1) *
		     hi_dim1] * yr;
	    hi[i__ + j * hi_dim1] = xr * zzi + xi * zzr - hi[i__ + (i__ - 1) *
		     hi_dim1] * yi;
/* L490: */
	}

/* L500: */
    }

    si = hi[en + en * hi_dim1];
    if (si == 0.) {
	goto L540;
    }
    norm = pythag(&hr[en + en * hr_dim1], &si);
    sr = hr[en + en * hr_dim1] / norm;
    si /= norm;
    hr[en + en * hr_dim1] = norm;
    hi[en + en * hi_dim1] = 0.;
    if (en == *n) {
	goto L540;
    }
    ip1 = en + 1;

    i__1 = *n;
    for (j = ip1; j <= i__1; ++j) {
	yr = hr[en + j * hr_dim1];
	yi = hi[en + j * hi_dim1];
	hr[en + j * hr_dim1] = sr * yr + si * yi;
	hi[en + j * hi_dim1] = sr * yi - si * yr;
/* L520: */
    }
/*     .......... INVERSE OPERATION (COLUMNS) .......... */
L540:
    i__1 = en;
    for (j = lp1; j <= i__1; ++j) {
	xr = wr[j - 1];
	xi = wi[j - 1];

	i__2 = j;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    yr = hr[i__ + (j - 1) * hr_dim1];
	    yi = 0.;
	    zzr = hr[i__ + j * hr_dim1];
	    zzi = hi[i__ + j * hi_dim1];
	    if (i__ == j) {
		goto L560;
	    }
	    yi = hi[i__ + (j - 1) * hi_dim1];
	    hi[i__ + (j - 1) * hi_dim1] = xr * yi + xi * yr + hi[j + (j - 1) *
		     hi_dim1] * zzi;
L560:
	    hr[i__ + (j - 1) * hr_dim1] = xr * yr - xi * yi + hi[j + (j - 1) *
		     hi_dim1] * zzr;
	    hr[i__ + j * hr_dim1] = xr * zzr + xi * zzi - hi[j + (j - 1) * 
		    hi_dim1] * yr;
	    hi[i__ + j * hi_dim1] = xr * zzi - xi * zzr - hi[j + (j - 1) * 
		    hi_dim1] * yi;
/* L580: */
	}

	i__2 = *igh;
	for (i__ = *low; i__ <= i__2; ++i__) {
	    yr = zr[i__ + (j - 1) * zr_dim1];
	    yi = zi[i__ + (j - 1) * zi_dim1];
	    zzr = zr[i__ + j * zr_dim1];
	    zzi = zi[i__ + j * zi_dim1];
	    zr[i__ + (j - 1) * zr_dim1] = xr * yr - xi * yi + hi[j + (j - 1) *
		     hi_dim1] * zzr;
	    zi[i__ + (j - 1) * zi_dim1] = xr * yi + xi * yr + hi[j + (j - 1) *
		     hi_dim1] * zzi;
	    zr[i__ + j * zr_dim1] = xr * zzr + xi * zzi - hi[j + (j - 1) * 
		    hi_dim1] * yr;
	    zi[i__ + j * zi_dim1] = xr * zzi - xi * zzr - hi[j + (j - 1) * 
		    hi_dim1] * yi;
/* L590: */
	}

/* L600: */
    }

    if (si == 0.) {
	goto L240;
    }

    i__1 = en;
    for (i__ = 1; i__ <= i__1; ++i__) {
	yr = hr[i__ + en * hr_dim1];
	yi = hi[i__ + en * hi_dim1];
	hr[i__ + en * hr_dim1] = sr * yr - si * yi;
	hi[i__ + en * hi_dim1] = sr * yi + si * yr;
/* L630: */
    }

    i__1 = *igh;
    for (i__ = *low; i__ <= i__1; ++i__) {
	yr = zr[i__ + en * zr_dim1];
	yi = zi[i__ + en * zi_dim1];
	zr[i__ + en * zr_dim1] = sr * yr - si * yi;
	zi[i__ + en * zi_dim1] = sr * yi + si * yr;
/* L640: */
    }

    goto L240;
/*     .......... A ROOT FOUND .......... */
L660:
    hr[en + en * hr_dim1] += tr;
    wr[en] = hr[en + en * hr_dim1];
    hi[en + en * hi_dim1] += ti;
    wi[en] = hi[en + en * hi_dim1];
    en = enm1;
    goto L220;
/*     .......... ALL ROOTS FOUND.  BACKSUBSTITUTE TO FIND */
/*                VECTORS OF UPPER TRIANGULAR FORM .......... */
L680:
    norm = 0.;

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {

	i__2 = *n;
	for (j = i__; j <= i__2; ++j) {
	    tr = (d__1 = hr[i__ + j * hr_dim1], abs(d__1)) + (d__2 = hi[i__ + 
		    j * hi_dim1], abs(d__2));
	    if (tr > norm) {
		norm = tr;
	    }
/* L720: */
	}
    }

    if (*n == 1 || norm == 0.) {
	goto L1001;
    }
/*     .......... FOR EN=N STEP -1 UNTIL 2 DO -- .......... */
    i__2 = *n;
    for (nn = 2; nn <= i__2; ++nn) {
	en = *n + 2 - nn;
	xr = wr[en];
	xi = wi[en];
	hr[en + en * hr_dim1] = 1.;
	hi[en + en * hi_dim1] = 0.;
	enm1 = en - 1;
/*     .......... FOR I=EN-1 STEP -1 UNTIL 1 DO -- .......... */
	i__1 = enm1;
	for (ii = 1; ii <= i__1; ++ii) {
	    i__ = en - ii;
	    zzr = 0.;
	    zzi = 0.;
	    ip1 = i__ + 1;

	    i__3 = en;
	    for (j = ip1; j <= i__3; ++j) {
		zzr = zzr + hr[i__ + j * hr_dim1] * hr[j + en * hr_dim1] - hi[
			i__ + j * hi_dim1] * hi[j + en * hi_dim1];
		zzi = zzi + hr[i__ + j * hr_dim1] * hi[j + en * hi_dim1] + hi[
			i__ + j * hi_dim1] * hr[j + en * hr_dim1];
/* L740: */
	    }

	    yr = xr - wr[i__];
	    yi = xi - wi[i__];
	    if (yr != 0. || yi != 0.) {
		goto L765;
	    }
	    tst1 = norm;
	    yr = tst1;
L760:
	    yr *= .01;
	    tst2 = norm + yr;
	    if (tst2 > tst1) {
		goto L760;
	    }
L765:
	    cdiv(&zzr, &zzi, &yr, &yi, &hr[i__ + en * hr_dim1], &hi[i__ + en 
		    * hi_dim1]);
/*     .......... OVERFLOW CONTROL .......... */
	    tr = (d__1 = hr[i__ + en * hr_dim1], abs(d__1)) + (d__2 = hi[i__ 
		    + en * hi_dim1], abs(d__2));
	    if (tr == 0.) {
		goto L780;
	    }
	    tst1 = tr;
	    tst2 = tst1 + 1. / tst1;
	    if (tst2 > tst1) {
		goto L780;
	    }
	    i__3 = en;
	    for (j = i__; j <= i__3; ++j) {
		hr[j + en * hr_dim1] /= tr;
		hi[j + en * hi_dim1] /= tr;
/* L770: */
	    }

L780:
	    ;
	}

/* L800: */
    }
/*     .......... END BACKSUBSTITUTION .......... */
    enm1 = *n - 1;
/*     .......... VECTORS OF ISOLATED ROOTS .......... */
    i__2 = enm1;
    for (i__ = 1; i__ <= i__2; ++i__) {
	if (i__ >= *low && i__ <= *igh) {
	    goto L840;
	}
	ip1 = i__ + 1;

	i__1 = *n;
	for (j = ip1; j <= i__1; ++j) {
	    zr[i__ + j * zr_dim1] = hr[i__ + j * hr_dim1];
	    zi[i__ + j * zi_dim1] = hi[i__ + j * hi_dim1];
/* L820: */
	}

L840:
	;
    }
/*     .......... MULTIPLY BY TRANSFORMATION MATRIX TO GIVE */
/*                VECTORS OF ORIGINAL FULL MATRIX. */
/*                FOR J=N STEP -1 UNTIL LOW+1 DO -- .......... */
    i__2 = enm1;
    for (jj = *low; jj <= i__2; ++jj) {
	j = *n + *low - jj;
	m = min(j,*igh);

	i__1 = *igh;
	for (i__ = *low; i__ <= i__1; ++i__) {
	    zzr = 0.;
	    zzi = 0.;

	    i__3 = m;
	    for (k = *low; k <= i__3; ++k) {
		zzr = zzr + zr[i__ + k * zr_dim1] * hr[k + j * hr_dim1] - zi[
			i__ + k * zi_dim1] * hi[k + j * hi_dim1];
		zzi = zzi + zr[i__ + k * zr_dim1] * hi[k + j * hi_dim1] + zi[
			i__ + k * zi_dim1] * hr[k + j * hr_dim1];
/* L860: */
	    }

	    zr[i__ + j * zr_dim1] = zzr;
	    zi[i__ + j * zi_dim1] = zzi;
/* L880: */
	}
    }

    goto L1001;
/*     .......... SET ERROR -- ALL EIGENVALUES HAVE NOT */
/*                CONVERGED AFTER 30*N ITERATIONS .......... */
L1000:
    *ierr = en;
L1001:
    return 0;
} /* comqr2_ */

/* Subroutine */ int corth(integer *nm, integer *n, integer *low, integer *
	igh, doublereal *ar, doublereal *ai, doublereal *ortr, doublereal *
	orti)
{
    /* System generated locals */
    integer ar_dim1, ar_offset, ai_dim1, ai_offset, i__1, i__2, i__3;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static doublereal f, g, h__;
    static integer i__, j, m;
    static doublereal scale;
    static integer la;
    static doublereal fi;
    static integer ii, jj;
    static doublereal fr;
    static integer mp;
    extern doublereal pythag(doublereal *, doublereal *);
    static integer kp1;



/*     THIS SUBROUTINE IS A TRANSLATION OF A COMPLEX ANALOGUE OF */
/*     THE ALGOL PROCEDURE ORTHES, NUM. MATH. 12, 349-368(1968) */
/*     BY MARTIN AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 339-358(1971). */

/*     GIVEN A COMPLEX GENERAL MATRIX, THIS SUBROUTINE */
/*     REDUCES A SUBMATRIX SITUATED IN ROWS AND COLUMNS */
/*     LOW THROUGH IGH TO UPPER HESSENBERG FORM BY */
/*     UNITARY SIMILARITY TRANSFORMATIONS. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING */
/*          SUBROUTINE  CBAL.  IF  CBAL  HAS NOT BEEN USED, */
/*          SET LOW=1, IGH=N. */

/*        AR AND AI CONTAIN THE REAL AND IMAGINARY PARTS, */
/*          RESPECTIVELY, OF THE COMPLEX INPUT MATRIX. */

/*     ON OUTPUT */

/*        AR AND AI CONTAIN THE REAL AND IMAGINARY PARTS, */
/*          RESPECTIVELY, OF THE HESSENBERG MATRIX.  INFORMATION */
/*          ABOUT THE UNITARY TRANSFORMATIONS USED IN THE REDUCTION */
/*          IS STORED IN THE REMAINING TRIANGLES UNDER THE */
/*          HESSENBERG MATRIX. */

/*        ORTR AND ORTI CONTAIN FURTHER INFORMATION ABOUT THE */
/*          TRANSFORMATIONS.  ONLY ELEMENTS LOW THROUGH IGH ARE USED. */

/*     CALLS PYTHAG FOR  DSQRT(A*A + B*B) . */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY */

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    ai_dim1 = *nm;
    ai_offset = ai_dim1 + 1;
    ai -= ai_offset;
    ar_dim1 = *nm;
    ar_offset = ar_dim1 + 1;
    ar -= ar_offset;
    --orti;
    --ortr;

    /* Function Body */
    la = *igh - 1;
    kp1 = *low + 1;
    if (la < kp1) {
	goto L200;
    }

    i__1 = la;
    for (m = kp1; m <= i__1; ++m) {
	h__ = 0.;
	ortr[m] = 0.;
	orti[m] = 0.;
	scale = 0.;
/*     .......... SCALE COLUMN (ALGOL TOL THEN NOT NEEDED) .......... */
	i__2 = *igh;
	for (i__ = m; i__ <= i__2; ++i__) {
/* L90: */
	    scale = scale + (d__1 = ar[i__ + (m - 1) * ar_dim1], abs(d__1)) + 
		    (d__2 = ai[i__ + (m - 1) * ai_dim1], abs(d__2));
	}

	if (scale == 0.) {
	    goto L180;
	}
	mp = m + *igh;
/*     .......... FOR I=IGH STEP -1 UNTIL M DO -- .......... */
	i__2 = *igh;
	for (ii = m; ii <= i__2; ++ii) {
	    i__ = mp - ii;
	    ortr[i__] = ar[i__ + (m - 1) * ar_dim1] / scale;
	    orti[i__] = ai[i__ + (m - 1) * ai_dim1] / scale;
	    h__ = h__ + ortr[i__] * ortr[i__] + orti[i__] * orti[i__];
/* L100: */
	}

	g = sqrt(h__);
	f = pythag(&ortr[m], &orti[m]);
	if (f == 0.) {
	    goto L103;
	}
	h__ += f * g;
	g /= f;
	ortr[m] = (g + 1.) * ortr[m];
	orti[m] = (g + 1.) * orti[m];
	goto L105;

L103:
	ortr[m] = g;
	ar[m + (m - 1) * ar_dim1] = scale;
/*     .......... FORM (I-(U*UT)/H) * A .......... */
L105:
	i__2 = *n;
	for (j = m; j <= i__2; ++j) {
	    fr = 0.;
	    fi = 0.;
/*     .......... FOR I=IGH STEP -1 UNTIL M DO -- .......... */
	    i__3 = *igh;
	    for (ii = m; ii <= i__3; ++ii) {
		i__ = mp - ii;
		fr = fr + ortr[i__] * ar[i__ + j * ar_dim1] + orti[i__] * ai[
			i__ + j * ai_dim1];
		fi = fi + ortr[i__] * ai[i__ + j * ai_dim1] - orti[i__] * ar[
			i__ + j * ar_dim1];
/* L110: */
	    }

	    fr /= h__;
	    fi /= h__;

	    i__3 = *igh;
	    for (i__ = m; i__ <= i__3; ++i__) {
		ar[i__ + j * ar_dim1] = ar[i__ + j * ar_dim1] - fr * ortr[i__]
			 + fi * orti[i__];
		ai[i__ + j * ai_dim1] = ai[i__ + j * ai_dim1] - fr * orti[i__]
			 - fi * ortr[i__];
/* L120: */
	    }

/* L130: */
	}
/*     .......... FORM (I-(U*UT)/H)*A*(I-(U*UT)/H) .......... */
	i__2 = *igh;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    fr = 0.;
	    fi = 0.;
/*     .......... FOR J=IGH STEP -1 UNTIL M DO -- .......... */
	    i__3 = *igh;
	    for (jj = m; jj <= i__3; ++jj) {
		j = mp - jj;
		fr = fr + ortr[j] * ar[i__ + j * ar_dim1] - orti[j] * ai[i__ 
			+ j * ai_dim1];
		fi = fi + ortr[j] * ai[i__ + j * ai_dim1] + orti[j] * ar[i__ 
			+ j * ar_dim1];
/* L140: */
	    }

	    fr /= h__;
	    fi /= h__;

	    i__3 = *igh;
	    for (j = m; j <= i__3; ++j) {
		ar[i__ + j * ar_dim1] = ar[i__ + j * ar_dim1] - fr * ortr[j] 
			- fi * orti[j];
		ai[i__ + j * ai_dim1] = ai[i__ + j * ai_dim1] + fr * orti[j] 
			- fi * ortr[j];
/* L150: */
	    }

/* L160: */
	}

	ortr[m] = scale * ortr[m];
	orti[m] = scale * orti[m];
	ar[m + (m - 1) * ar_dim1] = -g * ar[m + (m - 1) * ar_dim1];
	ai[m + (m - 1) * ai_dim1] = -g * ai[m + (m - 1) * ai_dim1];
L180:
	;
    }

L200:
    return 0;
} /* corth_ */

/* Subroutine */ int csroot(doublereal *xr, doublereal *xi, doublereal *yr, 
	doublereal *yi)
{
    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static doublereal s, ti, tr;
    extern doublereal pythag(doublereal *, doublereal *);


/*     (YR,YI) = COMPLEX DSQRT(XR,XI) */
/*     BRANCH CHOSEN SO THAT YR .GE. 0.0 AND SIGN(YI) .EQ. SIGN(XI) */

    tr = *xr;
    ti = *xi;
    s = sqrt((pythag(&tr, &ti) + abs(tr)) * .5);
    if (tr >= 0.) {
	*yr = s;
    }
    if (ti < 0.) {
	s = -s;
    }
    if (tr <= 0.) {
	*yi = s;
    }
    if (tr < 0.) {
	*yr = ti / *yi * .5;
    }
    if (tr > 0.) {
	*yi = ti / *yr * .5;
    }
    return 0;
} /* csroot_ */

/* Subroutine */ int elmhes(integer *nm, integer *n, integer *low, integer *
	igh, doublereal *a, integer *int__)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    doublereal d__1;

    /* Local variables */
    static integer i__, j, m;
    static doublereal x, y;
    static integer la, mm1, kp1, mp1;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE ELMHES, */
/*     NUM. MATH. 12, 349-368(1968) BY MARTIN AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 339-358(1971). */

/*     GIVEN A REAL GENERAL MATRIX, THIS SUBROUTINE */
/*     REDUCES A SUBMATRIX SITUATED IN ROWS AND COLUMNS */
/*     LOW THROUGH IGH TO UPPER HESSENBERG FORM BY */
/*     STABILIZED ELEMENTARY SIMILARITY TRANSFORMATIONS. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING */
/*          SUBROUTINE  BALANC.  IF  BALANC  HAS NOT BEEN USED, */
/*          SET LOW=1, IGH=N. */

/*        A CONTAINS THE INPUT MATRIX. */

/*     ON OUTPUT */

/*        A CONTAINS THE HESSENBERG MATRIX.  THE MULTIPLIERS */
/*          WHICH WERE USED IN THE REDUCTION ARE STORED IN THE */
/*          REMAINING TRIANGLE UNDER THE HESSENBERG MATRIX. */

/*        INT CONTAINS INFORMATION ON THE ROWS AND COLUMNS */
/*          INTERCHANGED IN THE REDUCTION. */
/*          ONLY ELEMENTS LOW THROUGH IGH ARE USED. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY */

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    --int__;

    /* Function Body */
    la = *igh - 1;
    kp1 = *low + 1;
    if (la < kp1) {
	goto L200;
    }

    i__1 = la;
    for (m = kp1; m <= i__1; ++m) {
	mm1 = m - 1;
	x = 0.;
	i__ = m;

	i__2 = *igh;
	for (j = m; j <= i__2; ++j) {
	    if ((d__1 = a[j + mm1 * a_dim1], abs(d__1)) <= abs(x)) {
		goto L100;
	    }
	    x = a[j + mm1 * a_dim1];
	    i__ = j;
L100:
	    ;
	}

	int__[m] = i__;
	if (i__ == m) {
	    goto L130;
	}
/*     .......... INTERCHANGE ROWS AND COLUMNS OF A .......... */
	i__2 = *n;
	for (j = mm1; j <= i__2; ++j) {
	    y = a[i__ + j * a_dim1];
	    a[i__ + j * a_dim1] = a[m + j * a_dim1];
	    a[m + j * a_dim1] = y;
/* L110: */
	}

	i__2 = *igh;
	for (j = 1; j <= i__2; ++j) {
	    y = a[j + i__ * a_dim1];
	    a[j + i__ * a_dim1] = a[j + m * a_dim1];
	    a[j + m * a_dim1] = y;
/* L120: */
	}
/*     .......... END INTERCHANGE .......... */
L130:
	if (x == 0.) {
	    goto L180;
	}
	mp1 = m + 1;

	i__2 = *igh;
	for (i__ = mp1; i__ <= i__2; ++i__) {
	    y = a[i__ + mm1 * a_dim1];
	    if (y == 0.) {
		goto L160;
	    }
	    y /= x;
	    a[i__ + mm1 * a_dim1] = y;

	    i__3 = *n;
	    for (j = m; j <= i__3; ++j) {
/* L140: */
		a[i__ + j * a_dim1] -= y * a[m + j * a_dim1];
	    }

	    i__3 = *igh;
	    for (j = 1; j <= i__3; ++j) {
/* L150: */
		a[j + m * a_dim1] += y * a[j + i__ * a_dim1];
	    }

L160:
	    ;
	}

L180:
	;
    }

L200:
    return 0;
} /* elmhes_ */

/* Subroutine */ int eltran(integer *nm, integer *n, integer *low, integer *
	igh, doublereal *a, integer *int__, doublereal *z__)
{
    /* System generated locals */
    integer a_dim1, a_offset, z_dim1, z_offset, i__1, i__2;

    /* Local variables */
    static integer i__, j, kl, mm, mp, mp1;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE ELMTRANS, */
/*     NUM. MATH. 16, 181-204(1970) BY PETERS AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 372-395(1971). */

/*     THIS SUBROUTINE ACCUMULATES THE STABILIZED ELEMENTARY */
/*     SIMILARITY TRANSFORMATIONS USED IN THE REDUCTION OF A */
/*     REAL GENERAL MATRIX TO UPPER HESSENBERG FORM BY  ELMHES. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING */
/*          SUBROUTINE  BALANC.  IF  BALANC  HAS NOT BEEN USED, */
/*          SET LOW=1, IGH=N. */

/*        A CONTAINS THE MULTIPLIERS WHICH WERE USED IN THE */
/*          REDUCTION BY  ELMHES  IN ITS LOWER TRIANGLE */
/*          BELOW THE SUBDIAGONAL. */

/*        INT CONTAINS INFORMATION ON THE ROWS AND COLUMNS */
/*          INTERCHANGED IN THE REDUCTION BY  ELMHES. */
/*          ONLY ELEMENTS LOW THROUGH IGH ARE USED. */

/*     ON OUTPUT */

/*        Z CONTAINS THE TRANSFORMATION MATRIX PRODUCED IN THE */
/*          REDUCTION BY  ELMHES. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY */

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ */

/*     .......... INITIALIZE Z TO IDENTITY MATRIX .......... */
    /* Parameter adjustments */
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z__ -= z_offset;
    --int__;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {

	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
/* L60: */
	    z__[i__ + j * z_dim1] = 0.;
	}

	z__[j + j * z_dim1] = 1.;
/* L80: */
    }

    kl = *igh - *low - 1;
    if (kl < 1) {
	goto L200;
    }
/*     .......... FOR MP=IGH-1 STEP -1 UNTIL LOW+1 DO -- .......... */
    i__1 = kl;
    for (mm = 1; mm <= i__1; ++mm) {
	mp = *igh - mm;
	mp1 = mp + 1;

	i__2 = *igh;
	for (i__ = mp1; i__ <= i__2; ++i__) {
/* L100: */
	    z__[i__ + mp * z_dim1] = a[i__ + (mp - 1) * a_dim1];
	}

	i__ = int__[mp];
	if (i__ == mp) {
	    goto L140;
	}

	i__2 = *igh;
	for (j = mp; j <= i__2; ++j) {
	    z__[mp + j * z_dim1] = z__[i__ + j * z_dim1];
	    z__[i__ + j * z_dim1] = 0.;
/* L130: */
	}

	z__[i__ + mp * z_dim1] = 1.;
L140:
	;
    }

L200:
    return 0;
} /* eltran_ */

doublereal epslon(doublereal *x)
{
    /* System generated locals */
    doublereal ret_val, d__1;

    /* Local variables */
    static doublereal a, b, c__, eps;


/*     ESTIMATE UNIT ROUNDOFF IN QUANTITIES OF SIZE X. */


/*     THIS PROGRAM SHOULD FUNCTION PROPERLY ON ALL SYSTEMS */
/*     SATISFYING THE FOLLOWING TWO ASSUMPTIONS, */
/*        1.  THE BASE USED IN REPRESENTING FLOATING POINT */
/*            NUMBERS IS NOT A POWER OF THREE. */
/*        2.  THE QUANTITY  A  IN STATEMENT 10 IS REPRESENTED TO */
/*            THE ACCURACY USED IN FLOATING POINT VARIABLES */
/*            THAT ARE STORED IN MEMORY. */
/*     THE STATEMENT NUMBER 10 AND THE GO TO 10 ARE INTENDED TO */
/*     FORCE OPTIMIZING COMPILERS TO GENERATE CODE SATISFYING */
/*     ASSUMPTION 2. */
/*     UNDER THESE ASSUMPTIONS, IT SHOULD BE TRUE THAT, */
/*            A  IS NOT EXACTLY EQUAL TO FOUR-THIRDS, */
/*            B  HAS A ZERO FOR ITS LAST BIT OR DIGIT, */
/*            C  IS NOT EXACTLY EQUAL TO ONE, */
/*            EPS  MEASURES THE SEPARATION OF 1.0 FROM */
/*                 THE NEXT LARGER FLOATING POINT NUMBER. */
/*     THE DEVELOPERS OF EISPACK WOULD APPRECIATE BEING INFORMED */
/*     ABOUT ANY SYSTEMS WHERE THESE ASSUMPTIONS DO NOT HOLD. */

/*     THIS VERSION DATED 4/6/83. */

    a = 1.3333333333333333;
L10:
    b = a - 1.;
    c__ = b + b + b;
    eps = (d__1 = c__ - 1., abs(d__1));
    if (eps == 0.) {
	goto L10;
    }
    ret_val = eps * abs(*x);
    return ret_val;
} /* epslon_ */

/* Subroutine */ int hqr(integer *nm, integer *n, integer *low, integer *igh,
	 doublereal *h__, doublereal *wr, doublereal *wi, integer *ierr)
{
    /* System generated locals */
    integer h_dim1, h_offset, i__1, i__2, i__3;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    static doublereal norm;
    static integer i__, j, k, l, m;
    static doublereal p, q, r__, s, t, w, x, y;
    static integer na, en, ll, mm;
    static doublereal zz;
    static logical notlas;
    static integer mp2, itn, its, enm2;
    static doublereal tst1, tst2;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE HQR, */
/*     NUM. MATH. 14, 219-231(1970) BY MARTIN, PETERS, AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 359-371(1971). */

/*     THIS SUBROUTINE FINDS THE EIGENVALUES OF A REAL */
/*     UPPER HESSENBERG MATRIX BY THE QR METHOD. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING */
/*          SUBROUTINE  BALANC.  IF  BALANC  HAS NOT BEEN USED, */
/*          SET LOW=1, IGH=N. */

/*        H CONTAINS THE UPPER HESSENBERG MATRIX.  INFORMATION ABOUT */
/*          THE TRANSFORMATIONS USED IN THE REDUCTION TO HESSENBERG */
/*          FORM BY  ELMHES  OR  ORTHES, IF PERFORMED, IS STORED */
/*          IN THE REMAINING TRIANGLE UNDER THE HESSENBERG MATRIX. */

/*     ON OUTPUT */

/*        H HAS BEEN DESTROYED.  THEREFORE, IT MUST BE SAVED */
/*          BEFORE CALLING  HQR  IF SUBSEQUENT CALCULATION AND */
/*          BACK TRANSFORMATION OF EIGENVECTORS IS TO BE PERFORMED. */

/*        WR AND WI CONTAIN THE REAL AND IMAGINARY PARTS, */
/*          RESPECTIVELY, OF THE EIGENVALUES.  THE EIGENVALUES */
/*          ARE UNORDERED EXCEPT THAT COMPLEX CONJUGATE PAIRS */
/*          OF VALUES APPEAR CONSECUTIVELY WITH THE EIGENVALUE */
/*          HAVING THE POSITIVE IMAGINARY PART FIRST.  IF AN */
/*          ERROR EXIT IS MADE, THE EIGENVALUES SHOULD BE CORRECT */
/*          FOR INDICES IERR+1,...,N. */

/*        IERR IS SET TO */
/*          ZERO       FOR NORMAL RETURN, */
/*          J          IF THE LIMIT OF 30*N ITERATIONS IS EXHAUSTED */
/*                     WHILE THE J-TH EIGENVALUE IS BEING SOUGHT. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY */

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ */

/*     unnecessary initialization of L M P Q R to keep g77 -Wall happy */

    /* Parameter adjustments */
    --wi;
    --wr;
    h_dim1 = *nm;
    h_offset = h_dim1 + 1;
    h__ -= h_offset;

    /* Function Body */
    l = 0;
    m = 0;
    p = 0.;
    q = 0.;
    r__ = 0.;

    *ierr = 0;
    norm = 0.;
    k = 1;
/*     .......... STORE ROOTS ISOLATED BY BALANC */
/*                AND COMPUTE MATRIX NORM .......... */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {

	i__2 = *n;
	for (j = k; j <= i__2; ++j) {
/* L40: */
	    norm += (d__1 = h__[i__ + j * h_dim1], abs(d__1));
	}

	k = i__;
	if (i__ >= *low && i__ <= *igh) {
	    goto L50;
	}
	wr[i__] = h__[i__ + i__ * h_dim1];
	wi[i__] = 0.;
L50:
	;
    }

    en = *igh;
    t = 0.;
    itn = *n * 30;
/*     .......... SEARCH FOR NEXT EIGENVALUES .......... */
L60:
    if (en < *low) {
	goto L1001;
    }
    its = 0;
    na = en - 1;
    enm2 = na - 1;
/*     .......... LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT */
/*                FOR L=EN STEP -1 UNTIL LOW DO -- .......... */
L70:
    i__1 = en;
    for (ll = *low; ll <= i__1; ++ll) {
	l = en + *low - ll;
	if (l == *low) {
	    goto L100;
	}
	s = (d__1 = h__[l - 1 + (l - 1) * h_dim1], abs(d__1)) + (d__2 = h__[l 
		+ l * h_dim1], abs(d__2));
	if (s == 0.) {
	    s = norm;
	}
	tst1 = s;
	tst2 = tst1 + (d__1 = h__[l + (l - 1) * h_dim1], abs(d__1));
	if (tst2 == tst1) {
	    goto L100;
	}
/* L80: */
    }
/*     .......... FORM SHIFT .......... */
L100:
    x = h__[en + en * h_dim1];
    if (l == en) {
	goto L270;
    }
    y = h__[na + na * h_dim1];
    w = h__[en + na * h_dim1] * h__[na + en * h_dim1];
    if (l == na) {
	goto L280;
    }
    if (itn == 0) {
	goto L1000;
    }
    if (its != 10 && its != 20) {
	goto L130;
    }
/*     .......... FORM EXCEPTIONAL SHIFT .......... */
    t += x;

    i__1 = en;
    for (i__ = *low; i__ <= i__1; ++i__) {
/* L120: */
	h__[i__ + i__ * h_dim1] -= x;
    }

    s = (d__1 = h__[en + na * h_dim1], abs(d__1)) + (d__2 = h__[na + enm2 * 
	    h_dim1], abs(d__2));
    x = s * .75;
    y = x;
    w = s * -.4375 * s;
L130:
    ++its;
    --itn;
/*     .......... LOOK FOR TWO CONSECUTIVE SMALL */
/*                SUB-DIAGONAL ELEMENTS. */
/*                FOR M=EN-2 STEP -1 UNTIL L DO -- .......... */
    i__1 = enm2;
    for (mm = l; mm <= i__1; ++mm) {
	m = enm2 + l - mm;
	zz = h__[m + m * h_dim1];
	r__ = x - zz;
	s = y - zz;
	p = (r__ * s - w) / h__[m + 1 + m * h_dim1] + h__[m + (m + 1) * 
		h_dim1];
	q = h__[m + 1 + (m + 1) * h_dim1] - zz - r__ - s;
	r__ = h__[m + 2 + (m + 1) * h_dim1];
	s = abs(p) + abs(q) + abs(r__);
	p /= s;
	q /= s;
	r__ /= s;
	if (m == l) {
	    goto L150;
	}
	tst1 = abs(p) * ((d__1 = h__[m - 1 + (m - 1) * h_dim1], abs(d__1)) + 
		abs(zz) + (d__2 = h__[m + 1 + (m + 1) * h_dim1], abs(d__2)));
	tst2 = tst1 + (d__1 = h__[m + (m - 1) * h_dim1], abs(d__1)) * (abs(q) 
		+ abs(r__));
	if (tst2 == tst1) {
	    goto L150;
	}
/* L140: */
    }

L150:
    mp2 = m + 2;

    i__1 = en;
    for (i__ = mp2; i__ <= i__1; ++i__) {
	h__[i__ + (i__ - 2) * h_dim1] = 0.;
	if (i__ == mp2) {
	    goto L160;
	}
	h__[i__ + (i__ - 3) * h_dim1] = 0.;
L160:
	;
    }
/*     .......... DOUBLE QR STEP INVOLVING ROWS L TO EN AND */
/*                COLUMNS M TO EN .......... */
    i__1 = na;
    for (k = m; k <= i__1; ++k) {
	notlas = k != na;
	if (k == m) {
	    goto L170;
	}
	p = h__[k + (k - 1) * h_dim1];
	q = h__[k + 1 + (k - 1) * h_dim1];
	r__ = 0.;
	if (notlas) {
	    r__ = h__[k + 2 + (k - 1) * h_dim1];
	}
	x = abs(p) + abs(q) + abs(r__);
	if (x == 0.) {
	    goto L260;
	}
	p /= x;
	q /= x;
	r__ /= x;
L170:
	d__1 = sqrt(p * p + q * q + r__ * r__);
	s = d_sign(&d__1, &p);
	if (k == m) {
	    goto L180;
	}
	h__[k + (k - 1) * h_dim1] = -s * x;
	goto L190;
L180:
	if (l != m) {
	    h__[k + (k - 1) * h_dim1] = -h__[k + (k - 1) * h_dim1];
	}
L190:
	p += s;
	x = p / s;
	y = q / s;
	zz = r__ / s;
	q /= p;
	r__ /= p;
	if (notlas) {
	    goto L225;
	}
/*     .......... ROW MODIFICATION .......... */
	i__2 = *n;
	for (j = k; j <= i__2; ++j) {
	    p = h__[k + j * h_dim1] + q * h__[k + 1 + j * h_dim1];
	    h__[k + j * h_dim1] -= p * x;
	    h__[k + 1 + j * h_dim1] -= p * y;
/* L200: */
	}

/* Computing MIN */
	i__2 = en, i__3 = k + 3;
	j = min(i__2,i__3);
/*     .......... COLUMN MODIFICATION .......... */
	i__2 = j;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    p = x * h__[i__ + k * h_dim1] + y * h__[i__ + (k + 1) * h_dim1];
	    h__[i__ + k * h_dim1] -= p;
	    h__[i__ + (k + 1) * h_dim1] -= p * q;
/* L210: */
	}
	goto L255;
L225:
/*     .......... ROW MODIFICATION .......... */
	i__2 = *n;
	for (j = k; j <= i__2; ++j) {
	    p = h__[k + j * h_dim1] + q * h__[k + 1 + j * h_dim1] + r__ * h__[
		    k + 2 + j * h_dim1];
	    h__[k + j * h_dim1] -= p * x;
	    h__[k + 1 + j * h_dim1] -= p * y;
	    h__[k + 2 + j * h_dim1] -= p * zz;
/* L230: */
	}

/* Computing MIN */
	i__2 = en, i__3 = k + 3;
	j = min(i__2,i__3);
/*     .......... COLUMN MODIFICATION .......... */
	i__2 = j;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    p = x * h__[i__ + k * h_dim1] + y * h__[i__ + (k + 1) * h_dim1] + 
		    zz * h__[i__ + (k + 2) * h_dim1];
	    h__[i__ + k * h_dim1] -= p;
	    h__[i__ + (k + 1) * h_dim1] -= p * q;
	    h__[i__ + (k + 2) * h_dim1] -= p * r__;
/* L240: */
	}
L255:

L260:
	;
    }

    goto L70;
/*     .......... ONE ROOT FOUND .......... */
L270:
    wr[en] = x + t;
    wi[en] = 0.;
    en = na;
    goto L60;
/*     .......... TWO ROOTS FOUND .......... */
L280:
    p = (y - x) / 2.;
    q = p * p + w;
    zz = sqrt((abs(q)));
    x += t;
    if (q < 0.) {
	goto L320;
    }
/*     .......... REAL PAIR .......... */
    zz = p + d_sign(&zz, &p);
    wr[na] = x + zz;
    wr[en] = wr[na];
    if (zz != 0.) {
	wr[en] = x - w / zz;
    }
    wi[na] = 0.;
    wi[en] = 0.;
    goto L330;
/*     .......... COMPLEX PAIR .......... */
L320:
    wr[na] = x + p;
    wr[en] = x + p;
    wi[na] = zz;
    wi[en] = -zz;
L330:
    en = enm2;
    goto L60;
/*     .......... SET ERROR -- ALL EIGENVALUES HAVE NOT */
/*                CONVERGED AFTER 30*N ITERATIONS .......... */
L1000:
    *ierr = en;
L1001:
    return 0;
} /* hqr_ */

/* Subroutine */ int hqr2(integer *nm, integer *n, integer *low, integer *
	igh, doublereal *h__, doublereal *wr, doublereal *wi, doublereal *z__,
	 integer *ierr)
{
    /* System generated locals */
    integer h_dim1, h_offset, z_dim1, z_offset, i__1, i__2, i__3;
    doublereal d__1, d__2, d__3, d__4;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    extern /* Subroutine */ int cdiv(doublereal *, doublereal *, doublereal *
	    , doublereal *, doublereal *, doublereal *);
    static doublereal norm;
    static integer i__, j, k, l, m;
    static doublereal p, q, r__, s, t, w, x, y;
    static integer na, ii, en, jj;
    static doublereal ra, sa;
    static integer ll, mm, nn;
    static doublereal vi, vr, zz;
    static logical notlas;
    static integer mp2, itn, its, enm2;
    static doublereal tst1, tst2;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE HQR2, */
/*     NUM. MATH. 16, 181-204(1970) BY PETERS AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 372-395(1971). */

/*     THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS */
/*     OF A REAL UPPER HESSENBERG MATRIX BY THE QR METHOD.  THE */
/*     EIGENVECTORS OF A REAL GENERAL MATRIX CAN ALSO BE FOUND */
/*     IF  ELMHES  AND  ELTRAN  OR  ORTHES  AND  ORTRAN  HAVE */
/*     BEEN USED TO REDUCE THIS GENERAL MATRIX TO HESSENBERG FORM */
/*     AND TO ACCUMULATE THE SIMILARITY TRANSFORMATIONS. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING */
/*          SUBROUTINE  BALANC.  IF  BALANC  HAS NOT BEEN USED, */
/*          SET LOW=1, IGH=N. */

/*        H CONTAINS THE UPPER HESSENBERG MATRIX. */

/*        Z CONTAINS THE TRANSFORMATION MATRIX PRODUCED BY  ELTRAN */
/*          AFTER THE REDUCTION BY  ELMHES, OR BY  ORTRAN  AFTER THE */
/*          REDUCTION BY  ORTHES, IF PERFORMED.  IF THE EIGENVECTORS */
/*          OF THE HESSENBERG MATRIX ARE DESIRED, Z MUST CONTAIN THE */
/*          IDENTITY MATRIX. */

/*     ON OUTPUT */

/*        H HAS BEEN DESTROYED. */

/*        WR AND WI CONTAIN THE REAL AND IMAGINARY PARTS, */
/*          RESPECTIVELY, OF THE EIGENVALUES.  THE EIGENVALUES */
/*          ARE UNORDERED EXCEPT THAT COMPLEX CONJUGATE PAIRS */
/*          OF VALUES APPEAR CONSECUTIVELY WITH THE EIGENVALUE */
/*          HAVING THE POSITIVE IMAGINARY PART FIRST.  IF AN */
/*          ERROR EXIT IS MADE, THE EIGENVALUES SHOULD BE CORRECT */
/*          FOR INDICES IERR+1,...,N. */

/*        Z CONTAINS THE REAL AND IMAGINARY PARTS OF THE EIGENVECTORS. */
/*          IF THE I-TH EIGENVALUE IS REAL, THE I-TH COLUMN OF Z */
/*          CONTAINS ITS EIGENVECTOR.  IF THE I-TH EIGENVALUE IS COMPLEX */
/*          WITH POSITIVE IMAGINARY PART, THE I-TH AND (I+1)-TH */
/*          COLUMNS OF Z CONTAIN THE REAL AND IMAGINARY PARTS OF ITS */
/*          EIGENVECTOR.  THE EIGENVECTORS ARE UNNORMALIZED.  IF AN */
/*          ERROR EXIT IS MADE, NONE OF THE EIGENVECTORS HAS BEEN FOUND. */

/*        IERR IS SET TO */
/*          ZERO       FOR NORMAL RETURN, */
/*          J          IF THE LIMIT OF 30*N ITERATIONS IS EXHAUSTED */
/*                     WHILE THE J-TH EIGENVALUE IS BEING SOUGHT. */

/*     CALLS CDIV FOR COMPLEX DIVISION. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY */

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ */

/*     unnecessary initialization of L M P R S to keep g77 -Wall happy */

    /* Parameter adjustments */
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z__ -= z_offset;
    --wi;
    --wr;
    h_dim1 = *nm;
    h_offset = h_dim1 + 1;
    h__ -= h_offset;

    /* Function Body */
    l = 0;
    m = 0;
    p = 0.;
    r__ = 0.;
    s = 0.;

    *ierr = 0;
    norm = 0.;
    k = 1;
/*     .......... STORE ROOTS ISOLATED BY BALANC */
/*                AND COMPUTE MATRIX NORM .......... */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {

	i__2 = *n;
	for (j = k; j <= i__2; ++j) {
/* L40: */
	    norm += (d__1 = h__[i__ + j * h_dim1], abs(d__1));
	}

	k = i__;
	if (i__ >= *low && i__ <= *igh) {
	    goto L50;
	}
	wr[i__] = h__[i__ + i__ * h_dim1];
	wi[i__] = 0.;
L50:
	;
    }

    en = *igh;
    t = 0.;
    itn = *n * 30;
/*     .......... SEARCH FOR NEXT EIGENVALUES .......... */
L60:
    if (en < *low) {
	goto L340;
    }
    its = 0;
    na = en - 1;
    enm2 = na - 1;
/*     .......... LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT */
/*                FOR L=EN STEP -1 UNTIL LOW DO -- .......... */
L70:
    i__1 = en;
    for (ll = *low; ll <= i__1; ++ll) {
	l = en + *low - ll;
	if (l == *low) {
	    goto L100;
	}
	s = (d__1 = h__[l - 1 + (l - 1) * h_dim1], abs(d__1)) + (d__2 = h__[l 
		+ l * h_dim1], abs(d__2));
	if (s == 0.) {
	    s = norm;
	}
	tst1 = s;
	tst2 = tst1 + (d__1 = h__[l + (l - 1) * h_dim1], abs(d__1));
	if (tst2 == tst1) {
	    goto L100;
	}
/* L80: */
    }
/*     .......... FORM SHIFT .......... */
L100:
    x = h__[en + en * h_dim1];
    if (l == en) {
	goto L270;
    }
    y = h__[na + na * h_dim1];
    w = h__[en + na * h_dim1] * h__[na + en * h_dim1];
    if (l == na) {
	goto L280;
    }
    if (itn == 0) {
	goto L1000;
    }
    if (its != 10 && its != 20) {
	goto L130;
    }
/*     .......... FORM EXCEPTIONAL SHIFT .......... */
    t += x;

    i__1 = en;
    for (i__ = *low; i__ <= i__1; ++i__) {
/* L120: */
	h__[i__ + i__ * h_dim1] -= x;
    }

    s = (d__1 = h__[en + na * h_dim1], abs(d__1)) + (d__2 = h__[na + enm2 * 
	    h_dim1], abs(d__2));
    x = s * .75;
    y = x;
    w = s * -.4375 * s;
L130:
    ++its;
    --itn;
/*     .......... LOOK FOR TWO CONSECUTIVE SMALL */
/*                SUB-DIAGONAL ELEMENTS. */
/*                FOR M=EN-2 STEP -1 UNTIL L DO -- .......... */
    i__1 = enm2;
    for (mm = l; mm <= i__1; ++mm) {
	m = enm2 + l - mm;
	zz = h__[m + m * h_dim1];
	r__ = x - zz;
	s = y - zz;
	p = (r__ * s - w) / h__[m + 1 + m * h_dim1] + h__[m + (m + 1) * 
		h_dim1];
	q = h__[m + 1 + (m + 1) * h_dim1] - zz - r__ - s;
	r__ = h__[m + 2 + (m + 1) * h_dim1];
	s = abs(p) + abs(q) + abs(r__);
	p /= s;
	q /= s;
	r__ /= s;
	if (m == l) {
	    goto L150;
	}
	tst1 = abs(p) * ((d__1 = h__[m - 1 + (m - 1) * h_dim1], abs(d__1)) + 
		abs(zz) + (d__2 = h__[m + 1 + (m + 1) * h_dim1], abs(d__2)));
	tst2 = tst1 + (d__1 = h__[m + (m - 1) * h_dim1], abs(d__1)) * (abs(q) 
		+ abs(r__));
	if (tst2 == tst1) {
	    goto L150;
	}
/* L140: */
    }

L150:
    mp2 = m + 2;

    i__1 = en;
    for (i__ = mp2; i__ <= i__1; ++i__) {
	h__[i__ + (i__ - 2) * h_dim1] = 0.;
	if (i__ == mp2) {
	    goto L160;
	}
	h__[i__ + (i__ - 3) * h_dim1] = 0.;
L160:
	;
    }
/*     .......... DOUBLE QR STEP INVOLVING ROWS L TO EN AND */
/*                COLUMNS M TO EN .......... */
    i__1 = na;
    for (k = m; k <= i__1; ++k) {
	notlas = k != na;
	if (k == m) {
	    goto L170;
	}
	p = h__[k + (k - 1) * h_dim1];
	q = h__[k + 1 + (k - 1) * h_dim1];
	r__ = 0.;
	if (notlas) {
	    r__ = h__[k + 2 + (k - 1) * h_dim1];
	}
	x = abs(p) + abs(q) + abs(r__);
	if (x == 0.) {
	    goto L260;
	}
	p /= x;
	q /= x;
	r__ /= x;
L170:
	d__1 = sqrt(p * p + q * q + r__ * r__);
	s = d_sign(&d__1, &p);
	if (k == m) {
	    goto L180;
	}
	h__[k + (k - 1) * h_dim1] = -s * x;
	goto L190;
L180:
	if (l != m) {
	    h__[k + (k - 1) * h_dim1] = -h__[k + (k - 1) * h_dim1];
	}
L190:
	p += s;
	x = p / s;
	y = q / s;
	zz = r__ / s;
	q /= p;
	r__ /= p;
	if (notlas) {
	    goto L225;
	}
/*     .......... ROW MODIFICATION .......... */
	i__2 = *n;
	for (j = k; j <= i__2; ++j) {
	    p = h__[k + j * h_dim1] + q * h__[k + 1 + j * h_dim1];
	    h__[k + j * h_dim1] -= p * x;
	    h__[k + 1 + j * h_dim1] -= p * y;
/* L200: */
	}

/* Computing MIN */
	i__2 = en, i__3 = k + 3;
	j = min(i__2,i__3);
/*     .......... COLUMN MODIFICATION .......... */
	i__2 = j;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    p = x * h__[i__ + k * h_dim1] + y * h__[i__ + (k + 1) * h_dim1];
	    h__[i__ + k * h_dim1] -= p;
	    h__[i__ + (k + 1) * h_dim1] -= p * q;
/* L210: */
	}
/*     .......... ACCUMULATE TRANSFORMATIONS .......... */
	i__2 = *igh;
	for (i__ = *low; i__ <= i__2; ++i__) {
	    p = x * z__[i__ + k * z_dim1] + y * z__[i__ + (k + 1) * z_dim1];
	    z__[i__ + k * z_dim1] -= p;
	    z__[i__ + (k + 1) * z_dim1] -= p * q;
/* L220: */
	}
	goto L255;
L225:
/*     .......... ROW MODIFICATION .......... */
	i__2 = *n;
	for (j = k; j <= i__2; ++j) {
	    p = h__[k + j * h_dim1] + q * h__[k + 1 + j * h_dim1] + r__ * h__[
		    k + 2 + j * h_dim1];
	    h__[k + j * h_dim1] -= p * x;
	    h__[k + 1 + j * h_dim1] -= p * y;
	    h__[k + 2 + j * h_dim1] -= p * zz;
/* L230: */
	}

/* Computing MIN */
	i__2 = en, i__3 = k + 3;
	j = min(i__2,i__3);
/*     .......... COLUMN MODIFICATION .......... */
	i__2 = j;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    p = x * h__[i__ + k * h_dim1] + y * h__[i__ + (k + 1) * h_dim1] + 
		    zz * h__[i__ + (k + 2) * h_dim1];
	    h__[i__ + k * h_dim1] -= p;
	    h__[i__ + (k + 1) * h_dim1] -= p * q;
	    h__[i__ + (k + 2) * h_dim1] -= p * r__;
/* L240: */
	}
/*     .......... ACCUMULATE TRANSFORMATIONS .......... */
	i__2 = *igh;
	for (i__ = *low; i__ <= i__2; ++i__) {
	    p = x * z__[i__ + k * z_dim1] + y * z__[i__ + (k + 1) * z_dim1] + 
		    zz * z__[i__ + (k + 2) * z_dim1];
	    z__[i__ + k * z_dim1] -= p;
	    z__[i__ + (k + 1) * z_dim1] -= p * q;
	    z__[i__ + (k + 2) * z_dim1] -= p * r__;
/* L250: */
	}
L255:

L260:
	;
    }

    goto L70;
/*     .......... ONE ROOT FOUND .......... */
L270:
    h__[en + en * h_dim1] = x + t;
    wr[en] = h__[en + en * h_dim1];
    wi[en] = 0.;
    en = na;
    goto L60;
/*     .......... TWO ROOTS FOUND .......... */
L280:
    p = (y - x) / 2.;
    q = p * p + w;
    zz = sqrt((abs(q)));
    h__[en + en * h_dim1] = x + t;
    x = h__[en + en * h_dim1];
    h__[na + na * h_dim1] = y + t;
    if (q < 0.) {
	goto L320;
    }
/*     .......... REAL PAIR .......... */
    zz = p + d_sign(&zz, &p);
    wr[na] = x + zz;
    wr[en] = wr[na];
    if (zz != 0.) {
	wr[en] = x - w / zz;
    }
    wi[na] = 0.;
    wi[en] = 0.;
    x = h__[en + na * h_dim1];
    s = abs(x) + abs(zz);
    p = x / s;
    q = zz / s;
    r__ = sqrt(p * p + q * q);
    p /= r__;
    q /= r__;
/*     .......... ROW MODIFICATION .......... */
    i__1 = *n;
    for (j = na; j <= i__1; ++j) {
	zz = h__[na + j * h_dim1];
	h__[na + j * h_dim1] = q * zz + p * h__[en + j * h_dim1];
	h__[en + j * h_dim1] = q * h__[en + j * h_dim1] - p * zz;
/* L290: */
    }
/*     .......... COLUMN MODIFICATION .......... */
    i__1 = en;
    for (i__ = 1; i__ <= i__1; ++i__) {
	zz = h__[i__ + na * h_dim1];
	h__[i__ + na * h_dim1] = q * zz + p * h__[i__ + en * h_dim1];
	h__[i__ + en * h_dim1] = q * h__[i__ + en * h_dim1] - p * zz;
/* L300: */
    }
/*     .......... ACCUMULATE TRANSFORMATIONS .......... */
    i__1 = *igh;
    for (i__ = *low; i__ <= i__1; ++i__) {
	zz = z__[i__ + na * z_dim1];
	z__[i__ + na * z_dim1] = q * zz + p * z__[i__ + en * z_dim1];
	z__[i__ + en * z_dim1] = q * z__[i__ + en * z_dim1] - p * zz;
/* L310: */
    }

    goto L330;
/*     .......... COMPLEX PAIR .......... */
L320:
    wr[na] = x + p;
    wr[en] = x + p;
    wi[na] = zz;
    wi[en] = -zz;
L330:
    en = enm2;
    goto L60;
/*     .......... ALL ROOTS FOUND.  BACKSUBSTITUTE TO FIND */
/*                VECTORS OF UPPER TRIANGULAR FORM .......... */
L340:
    if (norm == 0.) {
	goto L1001;
    }
/*     .......... FOR EN=N STEP -1 UNTIL 1 DO -- .......... */
    i__1 = *n;
    for (nn = 1; nn <= i__1; ++nn) {
	en = *n + 1 - nn;
	p = wr[en];
	q = wi[en];
	na = en - 1;
	if (q < 0.) {
	    goto L710;
	} else if (q == 0) {
	    goto L600;
	} else {
	    goto L800;
	}
/*     .......... REAL VECTOR .......... */
L600:
	m = en;
	h__[en + en * h_dim1] = 1.;
	if (na == 0) {
	    goto L800;
	}
/*     .......... FOR I=EN-1 STEP -1 UNTIL 1 DO -- .......... */
	i__2 = na;
	for (ii = 1; ii <= i__2; ++ii) {
	    i__ = en - ii;
	    w = h__[i__ + i__ * h_dim1] - p;
	    r__ = 0.;

	    i__3 = en;
	    for (j = m; j <= i__3; ++j) {
/* L610: */
		r__ += h__[i__ + j * h_dim1] * h__[j + en * h_dim1];
	    }

	    if (wi[i__] >= 0.) {
		goto L630;
	    }
	    zz = w;
	    s = r__;
	    goto L700;
L630:
	    m = i__;
	    if (wi[i__] != 0.) {
		goto L640;
	    }
	    t = w;
	    if (t != 0.) {
		goto L635;
	    }
	    tst1 = norm;
	    t = tst1;
L632:
	    t *= .01;
	    tst2 = norm + t;
	    if (tst2 > tst1) {
		goto L632;
	    }
L635:
	    h__[i__ + en * h_dim1] = -r__ / t;
	    goto L680;
/*     .......... SOLVE REAL EQUATIONS .......... */
L640:
	    x = h__[i__ + (i__ + 1) * h_dim1];
	    y = h__[i__ + 1 + i__ * h_dim1];
	    q = (wr[i__] - p) * (wr[i__] - p) + wi[i__] * wi[i__];
	    t = (x * s - zz * r__) / q;
	    h__[i__ + en * h_dim1] = t;
	    if (abs(x) <= abs(zz)) {
		goto L650;
	    }
	    h__[i__ + 1 + en * h_dim1] = (-r__ - w * t) / x;
	    goto L680;
L650:
	    h__[i__ + 1 + en * h_dim1] = (-s - y * t) / zz;

/*     .......... OVERFLOW CONTROL .......... */
L680:
	    t = (d__1 = h__[i__ + en * h_dim1], abs(d__1));
	    if (t == 0.) {
		goto L700;
	    }
	    tst1 = t;
	    tst2 = tst1 + 1. / tst1;
	    if (tst2 > tst1) {
		goto L700;
	    }
	    i__3 = en;
	    for (j = i__; j <= i__3; ++j) {
		h__[j + en * h_dim1] /= t;
/* L690: */
	    }

L700:
	    ;
	}
/*     .......... END REAL VECTOR .......... */
	goto L800;
/*     .......... COMPLEX VECTOR .......... */
L710:
	m = na;
/*     .......... LAST VECTOR COMPONENT CHOSEN IMAGINARY SO THAT */
/*                EIGENVECTOR MATRIX IS TRIANGULAR .......... */
	if ((d__1 = h__[en + na * h_dim1], abs(d__1)) <= (d__2 = h__[na + en *
		 h_dim1], abs(d__2))) {
	    goto L720;
	}
	h__[na + na * h_dim1] = q / h__[en + na * h_dim1];
	h__[na + en * h_dim1] = -(h__[en + en * h_dim1] - p) / h__[en + na * 
		h_dim1];
	goto L730;
L720:
	d__1 = -h__[na + en * h_dim1];
	d__2 = h__[na + na * h_dim1] - p;
	cdiv(&c_b246, &d__1, &d__2, &q, &h__[na + na * h_dim1], &h__[na + en 
		* h_dim1]);
L730:
	h__[en + na * h_dim1] = 0.;
	h__[en + en * h_dim1] = 1.;
	enm2 = na - 1;
	if (enm2 == 0) {
	    goto L800;
	}
/*     .......... FOR I=EN-2 STEP -1 UNTIL 1 DO -- .......... */
	i__2 = enm2;
	for (ii = 1; ii <= i__2; ++ii) {
	    i__ = na - ii;
	    w = h__[i__ + i__ * h_dim1] - p;
	    ra = 0.;
	    sa = 0.;

	    i__3 = en;
	    for (j = m; j <= i__3; ++j) {
		ra += h__[i__ + j * h_dim1] * h__[j + na * h_dim1];
		sa += h__[i__ + j * h_dim1] * h__[j + en * h_dim1];
/* L760: */
	    }

	    if (wi[i__] >= 0.) {
		goto L770;
	    }
	    zz = w;
	    r__ = ra;
	    s = sa;
	    goto L795;
L770:
	    m = i__;
	    if (wi[i__] != 0.) {
		goto L780;
	    }
	    d__1 = -ra;
	    d__2 = -sa;
	    cdiv(&d__1, &d__2, &w, &q, &h__[i__ + na * h_dim1], &h__[i__ + 
		    en * h_dim1]);
	    goto L790;
/*     .......... SOLVE COMPLEX EQUATIONS .......... */
L780:
	    x = h__[i__ + (i__ + 1) * h_dim1];
	    y = h__[i__ + 1 + i__ * h_dim1];
	    vr = (wr[i__] - p) * (wr[i__] - p) + wi[i__] * wi[i__] - q * q;
	    vi = (wr[i__] - p) * 2. * q;
	    if (vr != 0. || vi != 0.) {
		goto L784;
	    }
	    tst1 = norm * (abs(w) + abs(q) + abs(x) + abs(y) + abs(zz));
	    vr = tst1;
L783:
	    vr *= .01;
	    tst2 = tst1 + vr;
	    if (tst2 > tst1) {
		goto L783;
	    }
L784:
	    d__1 = x * r__ - zz * ra + q * sa;
	    d__2 = x * s - zz * sa - q * ra;
	    cdiv(&d__1, &d__2, &vr, &vi, &h__[i__ + na * h_dim1], &h__[i__ + 
		    en * h_dim1]);
	    if (abs(x) <= abs(zz) + abs(q)) {
		goto L785;
	    }
	    h__[i__ + 1 + na * h_dim1] = (-ra - w * h__[i__ + na * h_dim1] + 
		    q * h__[i__ + en * h_dim1]) / x;
	    h__[i__ + 1 + en * h_dim1] = (-sa - w * h__[i__ + en * h_dim1] - 
		    q * h__[i__ + na * h_dim1]) / x;
	    goto L790;
L785:
	    d__1 = -r__ - y * h__[i__ + na * h_dim1];
	    d__2 = -s - y * h__[i__ + en * h_dim1];
	    cdiv(&d__1, &d__2, &zz, &q, &h__[i__ + 1 + na * h_dim1], &h__[
		    i__ + 1 + en * h_dim1]);

/*     .......... OVERFLOW CONTROL .......... */
L790:
/* Computing MAX */
	    d__3 = (d__1 = h__[i__ + na * h_dim1], abs(d__1)), d__4 = (d__2 = 
		    h__[i__ + en * h_dim1], abs(d__2));
	    t = max(d__3,d__4);
	    if (t == 0.) {
		goto L795;
	    }
	    tst1 = t;
	    tst2 = tst1 + 1. / tst1;
	    if (tst2 > tst1) {
		goto L795;
	    }
	    i__3 = en;
	    for (j = i__; j <= i__3; ++j) {
		h__[j + na * h_dim1] /= t;
		h__[j + en * h_dim1] /= t;
/* L792: */
	    }

L795:
	    ;
	}
/*     .......... END COMPLEX VECTOR .......... */
L800:
	;
    }
/*     .......... END BACK SUBSTITUTION. */
/*                VECTORS OF ISOLATED ROOTS .......... */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (i__ >= *low && i__ <= *igh) {
	    goto L840;
	}

	i__2 = *n;
	for (j = i__; j <= i__2; ++j) {
/* L820: */
	    z__[i__ + j * z_dim1] = h__[i__ + j * h_dim1];
	}

L840:
	;
    }
/*     .......... MULTIPLY BY TRANSFORMATION MATRIX TO GIVE */
/*                VECTORS OF ORIGINAL FULL MATRIX. */
/*                FOR J=N STEP -1 UNTIL LOW DO -- .......... */
    i__1 = *n;
    for (jj = *low; jj <= i__1; ++jj) {
	j = *n + *low - jj;
	m = min(j,*igh);

	i__2 = *igh;
	for (i__ = *low; i__ <= i__2; ++i__) {
	    zz = 0.;

	    i__3 = m;
	    for (k = *low; k <= i__3; ++k) {
/* L860: */
		zz += z__[i__ + k * z_dim1] * h__[k + j * h_dim1];
	    }

	    z__[i__ + j * z_dim1] = zz;
/* L880: */
	}
    }

    goto L1001;
/*     .......... SET ERROR -- ALL EIGENVALUES HAVE NOT */
/*                CONVERGED AFTER 30*N ITERATIONS .......... */
L1000:
    *ierr = en;
L1001:
    return 0;
} /* hqr2_ */

/* Subroutine */ int htribk(integer *nm, integer *n, doublereal *ar, 
	doublereal *ai, doublereal *tau, integer *m, doublereal *zr, 
	doublereal *zi)
{
    /* System generated locals */
    integer ar_dim1, ar_offset, ai_dim1, ai_offset, zr_dim1, zr_offset, 
	    zi_dim1, zi_offset, i__1, i__2, i__3;

    /* Local variables */
    static doublereal h__;
    static integer i__, j, k, l;
    static doublereal s, si;



/*     THIS SUBROUTINE IS A TRANSLATION OF A COMPLEX ANALOGUE OF */
/*     THE ALGOL PROCEDURE TRBAK1, NUM. MATH. 11, 181-195(1968) */
/*     BY MARTIN, REINSCH, AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971). */

/*     THIS SUBROUTINE FORMS THE EIGENVECTORS OF A COMPLEX HERMITIAN */
/*     MATRIX BY BACK TRANSFORMING THOSE OF THE CORRESPONDING */
/*     REAL SYMMETRIC TRIDIAGONAL MATRIX DETERMINED BY  HTRIDI. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        AR AND AI CONTAIN INFORMATION ABOUT THE UNITARY TRANS- */
/*          FORMATIONS USED IN THE REDUCTION BY  HTRIDI  IN THEIR */
/*          FULL LOWER TRIANGLES EXCEPT FOR THE DIAGONAL OF AR. */

/*        TAU CONTAINS FURTHER INFORMATION ABOUT THE TRANSFORMATIONS. */

/*        M IS THE NUMBER OF EIGENVECTORS TO BE BACK TRANSFORMED. */

/*        ZR CONTAINS THE EIGENVECTORS TO BE BACK TRANSFORMED */
/*          IN ITS FIRST M COLUMNS. */

/*     ON OUTPUT */

/*        ZR AND ZI CONTAIN THE REAL AND IMAGINARY PARTS, */
/*          RESPECTIVELY, OF THE TRANSFORMED EIGENVECTORS */
/*          IN THEIR FIRST M COLUMNS. */

/*     NOTE THAT THE LAST COMPONENT OF EACH RETURNED VECTOR */
/*     IS REAL AND THAT VECTOR EUCLIDEAN NORMS ARE PRESERVED. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY */

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    tau -= 3;
    ai_dim1 = *nm;
    ai_offset = ai_dim1 + 1;
    ai -= ai_offset;
    ar_dim1 = *nm;
    ar_offset = ar_dim1 + 1;
    ar -= ar_offset;
    zi_dim1 = *nm;
    zi_offset = zi_dim1 + 1;
    zi -= zi_offset;
    zr_dim1 = *nm;
    zr_offset = zr_dim1 + 1;
    zr -= zr_offset;

    /* Function Body */
    if (*m == 0) {
	goto L200;
    }
/*     .......... TRANSFORM THE EIGENVECTORS OF THE REAL SYMMETRIC */
/*                TRIDIAGONAL MATRIX TO THOSE OF THE HERMITIAN */
/*                TRIDIAGONAL MATRIX. .......... */
    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {

	i__2 = *m;
	for (j = 1; j <= i__2; ++j) {
	    zi[k + j * zi_dim1] = -zr[k + j * zr_dim1] * tau[(k << 1) + 2];
	    zr[k + j * zr_dim1] *= tau[(k << 1) + 1];
/* L50: */
	}
    }

    if (*n == 1) {
	goto L200;
    }
/*     .......... RECOVER AND APPLY THE HOUSEHOLDER MATRICES .......... */
    i__2 = *n;
    for (i__ = 2; i__ <= i__2; ++i__) {
	l = i__ - 1;
	h__ = ai[i__ + i__ * ai_dim1];
	if (h__ == 0.) {
	    goto L140;
	}

	i__1 = *m;
	for (j = 1; j <= i__1; ++j) {
	    s = 0.;
	    si = 0.;

	    i__3 = l;
	    for (k = 1; k <= i__3; ++k) {
		s = s + ar[i__ + k * ar_dim1] * zr[k + j * zr_dim1] - ai[i__ 
			+ k * ai_dim1] * zi[k + j * zi_dim1];
		si = si + ar[i__ + k * ar_dim1] * zi[k + j * zi_dim1] + ai[
			i__ + k * ai_dim1] * zr[k + j * zr_dim1];
/* L110: */
	    }
/*     .......... DOUBLE DIVISIONS AVOID POSSIBLE UNDERFLOW .......... */
	    s = s / h__ / h__;
	    si = si / h__ / h__;

	    i__3 = l;
	    for (k = 1; k <= i__3; ++k) {
		zr[k + j * zr_dim1] = zr[k + j * zr_dim1] - s * ar[i__ + k * 
			ar_dim1] - si * ai[i__ + k * ai_dim1];
		zi[k + j * zi_dim1] = zi[k + j * zi_dim1] - si * ar[i__ + k * 
			ar_dim1] + s * ai[i__ + k * ai_dim1];
/* L120: */
	    }

/* L130: */
	}

L140:
	;
    }

L200:
    return 0;
} /* htribk_ */

/* Subroutine */ int htridi(integer *nm, integer *n, doublereal *ar, 
	doublereal *ai, doublereal *d__, doublereal *e, doublereal *e2, 
	doublereal *tau)
{
    /* System generated locals */
    integer ar_dim1, ar_offset, ai_dim1, ai_offset, i__1, i__2, i__3;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static doublereal f, g, h__;
    static integer i__, j, k, l;
    static doublereal scale, fi, gi, hh;
    static integer ii;
    static doublereal si;
    extern doublereal pythag(doublereal *, doublereal *);
    static integer jp1;



/*     THIS SUBROUTINE IS A TRANSLATION OF A COMPLEX ANALOGUE OF */
/*     THE ALGOL PROCEDURE TRED1, NUM. MATH. 11, 181-195(1968) */
/*     BY MARTIN, REINSCH, AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971). */

/*     THIS SUBROUTINE REDUCES A COMPLEX HERMITIAN MATRIX */
/*     TO A REAL SYMMETRIC TRIDIAGONAL MATRIX USING */
/*     UNITARY SIMILARITY TRANSFORMATIONS. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        AR AND AI CONTAIN THE REAL AND IMAGINARY PARTS, */
/*          RESPECTIVELY, OF THE COMPLEX HERMITIAN INPUT MATRIX. */
/*          ONLY THE LOWER TRIANGLE OF THE MATRIX NEED BE SUPPLIED. */

/*     ON OUTPUT */

/*        AR AND AI CONTAIN INFORMATION ABOUT THE UNITARY TRANS- */
/*          FORMATIONS USED IN THE REDUCTION IN THEIR FULL LOWER */
/*          TRIANGLES.  THEIR STRICT UPPER TRIANGLES AND THE */
/*          DIAGONAL OF AR ARE UNALTERED. */

/*        D CONTAINS THE DIAGONAL ELEMENTS OF THE THE TRIDIAGONAL MATRIX. */

/*        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE TRIDIAGONAL */
/*          MATRIX IN ITS LAST N-1 POSITIONS.  E(1) IS SET TO ZERO. */

/*        E2 CONTAINS THE SQUARES OF THE CORRESPONDING ELEMENTS OF E. */
/*          E2 MAY COINCIDE WITH E IF THE SQUARES ARE NOT NEEDED. */

/*        TAU CONTAINS FURTHER INFORMATION ABOUT THE TRANSFORMATIONS. */

/*     CALLS PYTHAG FOR  DSQRT(A*A + B*B) . */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY */

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    tau -= 3;
    --e2;
    --e;
    --d__;
    ai_dim1 = *nm;
    ai_offset = ai_dim1 + 1;
    ai -= ai_offset;
    ar_dim1 = *nm;
    ar_offset = ar_dim1 + 1;
    ar -= ar_offset;

    /* Function Body */
    tau[(*n << 1) + 1] = 1.;
    tau[(*n << 1) + 2] = 0.;

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L100: */
	d__[i__] = ar[i__ + i__ * ar_dim1];
    }
/*     .......... FOR I=N STEP -1 UNTIL 1 DO -- .......... */
    i__1 = *n;
    for (ii = 1; ii <= i__1; ++ii) {
	i__ = *n + 1 - ii;
	l = i__ - 1;
	h__ = 0.;
	scale = 0.;
	if (l < 1) {
	    goto L130;
	}
/*     .......... SCALE ROW (ALGOL TOL THEN NOT NEEDED) .......... */
	i__2 = l;
	for (k = 1; k <= i__2; ++k) {
/* L120: */
	    scale = scale + (d__1 = ar[i__ + k * ar_dim1], abs(d__1)) + (d__2 
		    = ai[i__ + k * ai_dim1], abs(d__2));
	}

	if (scale != 0.) {
	    goto L140;
	}
	tau[(l << 1) + 1] = 1.;
	tau[(l << 1) + 2] = 0.;
L130:
	e[i__] = 0.;
	e2[i__] = 0.;
	goto L290;

L140:
	i__2 = l;
	for (k = 1; k <= i__2; ++k) {
	    ar[i__ + k * ar_dim1] /= scale;
	    ai[i__ + k * ai_dim1] /= scale;
	    h__ = h__ + ar[i__ + k * ar_dim1] * ar[i__ + k * ar_dim1] + ai[
		    i__ + k * ai_dim1] * ai[i__ + k * ai_dim1];
/* L150: */
	}

	e2[i__] = scale * scale * h__;
	g = sqrt(h__);
	e[i__] = scale * g;
	f = pythag(&ar[i__ + l * ar_dim1], &ai[i__ + l * ai_dim1]);
/*     .......... FORM NEXT DIAGONAL ELEMENT OF MATRIX T .......... */
	if (f == 0.) {
	    goto L160;
	}
	tau[(l << 1) + 1] = (ai[i__ + l * ai_dim1] * tau[(i__ << 1) + 2] - ar[
		i__ + l * ar_dim1] * tau[(i__ << 1) + 1]) / f;
	si = (ar[i__ + l * ar_dim1] * tau[(i__ << 1) + 2] + ai[i__ + l * 
		ai_dim1] * tau[(i__ << 1) + 1]) / f;
	h__ += f * g;
	g = g / f + 1.;
	ar[i__ + l * ar_dim1] = g * ar[i__ + l * ar_dim1];
	ai[i__ + l * ai_dim1] = g * ai[i__ + l * ai_dim1];
	if (l == 1) {
	    goto L270;
	}
	goto L170;
L160:
	tau[(l << 1) + 1] = -tau[(i__ << 1) + 1];
	si = tau[(i__ << 1) + 2];
	ar[i__ + l * ar_dim1] = g;
L170:
	f = 0.;

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    g = 0.;
	    gi = 0.;
/*     .......... FORM ELEMENT OF A*U .......... */
	    i__3 = j;
	    for (k = 1; k <= i__3; ++k) {
		g = g + ar[j + k * ar_dim1] * ar[i__ + k * ar_dim1] + ai[j + 
			k * ai_dim1] * ai[i__ + k * ai_dim1];
		gi = gi - ar[j + k * ar_dim1] * ai[i__ + k * ai_dim1] + ai[j 
			+ k * ai_dim1] * ar[i__ + k * ar_dim1];
/* L180: */
	    }

	    jp1 = j + 1;
	    if (l < jp1) {
		goto L220;
	    }

	    i__3 = l;
	    for (k = jp1; k <= i__3; ++k) {
		g = g + ar[k + j * ar_dim1] * ar[i__ + k * ar_dim1] - ai[k + 
			j * ai_dim1] * ai[i__ + k * ai_dim1];
		gi = gi - ar[k + j * ar_dim1] * ai[i__ + k * ai_dim1] - ai[k 
			+ j * ai_dim1] * ar[i__ + k * ar_dim1];
/* L200: */
	    }
/*     .......... FORM ELEMENT OF P .......... */
L220:
	    e[j] = g / h__;
	    tau[(j << 1) + 2] = gi / h__;
	    f = f + e[j] * ar[i__ + j * ar_dim1] - tau[(j << 1) + 2] * ai[i__ 
		    + j * ai_dim1];
/* L240: */
	}

	hh = f / (h__ + h__);
/*     .......... FORM REDUCED A .......... */
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    f = ar[i__ + j * ar_dim1];
	    g = e[j] - hh * f;
	    e[j] = g;
	    fi = -ai[i__ + j * ai_dim1];
	    gi = tau[(j << 1) + 2] - hh * fi;
	    tau[(j << 1) + 2] = -gi;

	    i__3 = j;
	    for (k = 1; k <= i__3; ++k) {
		ar[j + k * ar_dim1] = ar[j + k * ar_dim1] - f * e[k] - g * ar[
			i__ + k * ar_dim1] + fi * tau[(k << 1) + 2] + gi * ai[
			i__ + k * ai_dim1];
		ai[j + k * ai_dim1] = ai[j + k * ai_dim1] - f * tau[(k << 1) 
			+ 2] - g * ai[i__ + k * ai_dim1] - fi * e[k] - gi * 
			ar[i__ + k * ar_dim1];
/* L260: */
	    }
	}

L270:
	i__3 = l;
	for (k = 1; k <= i__3; ++k) {
	    ar[i__ + k * ar_dim1] = scale * ar[i__ + k * ar_dim1];
	    ai[i__ + k * ai_dim1] = scale * ai[i__ + k * ai_dim1];
/* L280: */
	}

	tau[(l << 1) + 2] = -si;
L290:
	hh = d__[i__];
	d__[i__] = ar[i__ + i__ * ar_dim1];
	ar[i__ + i__ * ar_dim1] = hh;
	ai[i__ + i__ * ai_dim1] = scale * sqrt(h__);
/* L300: */
    }

    return 0;
} /* htridi_ */

doublereal pythag(doublereal *a, doublereal *b)
{
    /* System generated locals */
    doublereal ret_val, d__1, d__2, d__3;

    /* Local variables */
    static doublereal p, r__, s, t, u;


/*     FINDS DSQRT(A**2+B**2) WITHOUT OVERFLOW OR DESTRUCTIVE UNDERFLOW */

/* Computing MAX */
    d__1 = abs(*a), d__2 = abs(*b);
    p = max(d__1,d__2);
    if (p == 0.) {
	goto L20;
    }
/* Computing MIN */
    d__2 = abs(*a), d__3 = abs(*b);
/* Computing 2nd power */
    d__1 = min(d__2,d__3) / p;
    r__ = d__1 * d__1;
L10:
    t = r__ + 4.;
    if (t == 4.) {
	goto L20;
    }
    s = r__ / t;
    u = s * 2. + 1.;
    p = u * p;
/* Computing 2nd power */
    d__1 = s / u;
    r__ = d__1 * d__1 * r__;
    goto L10;
L20:
    ret_val = p;
    return ret_val;
} /* pythag_ */

/* Subroutine */ int tql1(integer *n, doublereal *d__, doublereal *e, 
	integer *ierr)
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1, d__2;

    /* Builtin functions */
    double d_sign(doublereal *, doublereal *);

    /* Local variables */
    static doublereal c__, f, g, h__;
    static integer i__, j, l, m;
    static doublereal p, r__, s, c2, c3;
    static integer l1, l2;
    static doublereal s2;
    static integer ii;
    extern doublereal pythag(doublereal *, doublereal *);
    static doublereal dl1, el1;
    static integer mml;
    static doublereal tst1, tst2;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TQL1, */
/*     NUM. MATH. 11, 293-306(1968) BY BOWDLER, MARTIN, REINSCH, AND */
/*     WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 227-240(1971). */

/*     THIS SUBROUTINE FINDS THE EIGENVALUES OF A SYMMETRIC */
/*     TRIDIAGONAL MATRIX BY THE QL METHOD. */

/*     ON INPUT */

/*        N IS THE ORDER OF THE MATRIX. */

/*        D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX. */

/*        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE INPUT MATRIX */
/*          IN ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY. */

/*      ON OUTPUT */

/*        D CONTAINS THE EIGENVALUES IN ASCENDING ORDER.  IF AN */
/*          ERROR EXIT IS MADE, THE EIGENVALUES ARE CORRECT AND */
/*          ORDERED FOR INDICES 1,2,...IERR-1, BUT MAY NOT BE */
/*          THE SMALLEST EIGENVALUES. */

/*        E HAS BEEN DESTROYED. */

/*        IERR IS SET TO */
/*          ZERO       FOR NORMAL RETURN, */
/*          J          IF THE J-TH EIGENVALUE HAS NOT BEEN */
/*                     DETERMINED AFTER 30 ITERATIONS. */

/*     CALLS PYTHAG FOR  DSQRT(A*A + B*B) . */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY */

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ */

/*     unnecessary initialization of C3 and S2 to keep g77 -Wall happy */

    /* Parameter adjustments */
    --e;
    --d__;

    /* Function Body */
    c3 = 0.;
    s2 = 0.;

    *ierr = 0;
    if (*n == 1) {
	goto L1001;
    }

    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
/* L100: */
	e[i__ - 1] = e[i__];
    }

    f = 0.;
    tst1 = 0.;
    e[*n] = 0.;

    i__1 = *n;
    for (l = 1; l <= i__1; ++l) {
	j = 0;
	h__ = (d__1 = d__[l], abs(d__1)) + (d__2 = e[l], abs(d__2));
	if (tst1 < h__) {
	    tst1 = h__;
	}
/*     .......... LOOK FOR SMALL SUB-DIAGONAL ELEMENT .......... */
	i__2 = *n;
	for (m = l; m <= i__2; ++m) {
	    tst2 = tst1 + (d__1 = e[m], abs(d__1));
	    if (tst2 == tst1) {
		goto L120;
	    }
/*     .......... E(N) IS ALWAYS ZERO, SO THERE IS NO EXIT */
/*                THROUGH THE BOTTOM OF THE LOOP .......... */
/* L110: */
	}

L120:
	if (m == l) {
	    goto L210;
	}
L130:
	if (j == 30) {
	    goto L1000;
	}
	++j;
/*     .......... FORM SHIFT .......... */
	l1 = l + 1;
	l2 = l1 + 1;
	g = d__[l];
	p = (d__[l1] - g) / (e[l] * 2.);
	r__ = pythag(&p, &c_b296);
	d__[l] = e[l] / (p + d_sign(&r__, &p));
	d__[l1] = e[l] * (p + d_sign(&r__, &p));
	dl1 = d__[l1];
	h__ = g - d__[l];
	if (l2 > *n) {
	    goto L145;
	}

	i__2 = *n;
	for (i__ = l2; i__ <= i__2; ++i__) {
/* L140: */
	    d__[i__] -= h__;
	}

L145:
	f += h__;
/*     .......... QL TRANSFORMATION .......... */
	p = d__[m];
	c__ = 1.;
	c2 = c__;
	el1 = e[l1];
	s = 0.;
	mml = m - l;
/*     .......... FOR I=M-1 STEP -1 UNTIL L DO -- .......... */
	i__2 = mml;
	for (ii = 1; ii <= i__2; ++ii) {
	    c3 = c2;
	    c2 = c__;
	    s2 = s;
	    i__ = m - ii;
	    g = c__ * e[i__];
	    h__ = c__ * p;
	    r__ = pythag(&p, &e[i__]);
	    e[i__ + 1] = s * r__;
	    s = e[i__] / r__;
	    c__ = p / r__;
	    p = c__ * d__[i__] - s * g;
	    d__[i__ + 1] = h__ + s * (c__ * g + s * d__[i__]);
/* L200: */
	}

	p = -s * s2 * c3 * el1 * e[l] / dl1;
	e[l] = s * p;
	d__[l] = c__ * p;
	tst2 = tst1 + (d__1 = e[l], abs(d__1));
	if (tst2 > tst1) {
	    goto L130;
	}
L210:
	p = d__[l] + f;
/*     .......... ORDER EIGENVALUES .......... */
	if (l == 1) {
	    goto L250;
	}
/*     .......... FOR I=L STEP -1 UNTIL 2 DO -- .......... */
	i__2 = l;
	for (ii = 2; ii <= i__2; ++ii) {
	    i__ = l + 2 - ii;
	    if (p >= d__[i__ - 1]) {
		goto L270;
	    }
	    d__[i__] = d__[i__ - 1];
/* L230: */
	}

L250:
	i__ = 1;
L270:
	d__[i__] = p;
/* L290: */
    }

    goto L1001;
/*     .......... SET ERROR -- NO CONVERGENCE TO AN */
/*                EIGENVALUE AFTER 30 ITERATIONS .......... */
L1000:
    *ierr = l;
L1001:
    return 0;
} /* tql1_ */

/* Subroutine */ int tql2(integer *nm, integer *n, doublereal *d__, 
	doublereal *e, doublereal *z__, integer *ierr)
{
    /* System generated locals */
    integer z_dim1, z_offset, i__1, i__2, i__3;
    doublereal d__1, d__2;

    /* Builtin functions */
    double d_sign(doublereal *, doublereal *);

    /* Local variables */
    static doublereal c__, f, g, h__;
    static integer i__, j, k, l, m;
    static doublereal p, r__, s, c2, c3;
    static integer l1, l2;
    static doublereal s2;
    static integer ii;
    extern doublereal pythag(doublereal *, doublereal *);
    static doublereal dl1, el1;
    static integer mml;
    static doublereal tst1, tst2;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TQL2, */
/*     NUM. MATH. 11, 293-306(1968) BY BOWDLER, MARTIN, REINSCH, AND */
/*     WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 227-240(1971). */

/*     THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS */
/*     OF A SYMMETRIC TRIDIAGONAL MATRIX BY THE QL METHOD. */
/*     THE EIGENVECTORS OF A FULL SYMMETRIC MATRIX CAN ALSO */
/*     BE FOUND IF  TRED2  HAS BEEN USED TO REDUCE THIS */
/*     FULL MATRIX TO TRIDIAGONAL FORM. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX. */

/*        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE INPUT MATRIX */
/*          IN ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY. */

/*        Z CONTAINS THE TRANSFORMATION MATRIX PRODUCED IN THE */
/*          REDUCTION BY  TRED2, IF PERFORMED.  IF THE EIGENVECTORS */
/*          OF THE TRIDIAGONAL MATRIX ARE DESIRED, Z MUST CONTAIN */
/*          THE IDENTITY MATRIX. */

/*      ON OUTPUT */

/*        D CONTAINS THE EIGENVALUES IN ASCENDING ORDER.  IF AN */
/*          ERROR EXIT IS MADE, THE EIGENVALUES ARE CORRECT BUT */
/*          UNORDERED FOR INDICES 1,2,...,IERR-1. */

/*        E HAS BEEN DESTROYED. */

/*        Z CONTAINS ORTHONORMAL EIGENVECTORS OF THE SYMMETRIC */
/*          TRIDIAGONAL (OR FULL) MATRIX.  IF AN ERROR EXIT IS MADE, */
/*          Z CONTAINS THE EIGENVECTORS ASSOCIATED WITH THE STORED */
/*          EIGENVALUES. */

/*        IERR IS SET TO */
/*          ZERO       FOR NORMAL RETURN, */
/*          J          IF THE J-TH EIGENVALUE HAS NOT BEEN */
/*                     DETERMINED AFTER 30 ITERATIONS. */

/*     CALLS PYTHAG FOR  DSQRT(A*A + B*B) . */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY */

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ */

/*     unnecessary initialization of C3 and S2 to keep g77 -Wall happy */

    /* Parameter adjustments */
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z__ -= z_offset;
    --e;
    --d__;

    /* Function Body */
    c3 = 0.;
    s2 = 0.;

    *ierr = 0;
    if (*n == 1) {
	goto L1001;
    }

    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
/* L100: */
	e[i__ - 1] = e[i__];
    }

    f = 0.;
    tst1 = 0.;
    e[*n] = 0.;

    i__1 = *n;
    for (l = 1; l <= i__1; ++l) {
	j = 0;
	h__ = (d__1 = d__[l], abs(d__1)) + (d__2 = e[l], abs(d__2));
	if (tst1 < h__) {
	    tst1 = h__;
	}
/*     .......... LOOK FOR SMALL SUB-DIAGONAL ELEMENT .......... */
	i__2 = *n;
	for (m = l; m <= i__2; ++m) {
	    tst2 = tst1 + (d__1 = e[m], abs(d__1));
	    if (tst2 == tst1) {
		goto L120;
	    }
/*     .......... E(N) IS ALWAYS ZERO, SO THERE IS NO EXIT */
/*                THROUGH THE BOTTOM OF THE LOOP .......... */
/* L110: */
	}

L120:
	if (m == l) {
	    goto L220;
	}
L130:
	if (j == 30) {
	    goto L1000;
	}
	++j;
/*     .......... FORM SHIFT .......... */
	l1 = l + 1;
	l2 = l1 + 1;
	g = d__[l];
	p = (d__[l1] - g) / (e[l] * 2.);
	r__ = pythag(&p, &c_b296);
	d__[l] = e[l] / (p + d_sign(&r__, &p));
	d__[l1] = e[l] * (p + d_sign(&r__, &p));
	dl1 = d__[l1];
	h__ = g - d__[l];
	if (l2 > *n) {
	    goto L145;
	}

	i__2 = *n;
	for (i__ = l2; i__ <= i__2; ++i__) {
/* L140: */
	    d__[i__] -= h__;
	}

L145:
	f += h__;
/*     .......... QL TRANSFORMATION .......... */
	p = d__[m];
	c__ = 1.;
	c2 = c__;
	el1 = e[l1];
	s = 0.;
	mml = m - l;
/*     .......... FOR I=M-1 STEP -1 UNTIL L DO -- .......... */
	i__2 = mml;
	for (ii = 1; ii <= i__2; ++ii) {
	    c3 = c2;
	    c2 = c__;
	    s2 = s;
	    i__ = m - ii;
	    g = c__ * e[i__];
	    h__ = c__ * p;
	    r__ = pythag(&p, &e[i__]);
	    e[i__ + 1] = s * r__;
	    s = e[i__] / r__;
	    c__ = p / r__;
	    p = c__ * d__[i__] - s * g;
	    d__[i__ + 1] = h__ + s * (c__ * g + s * d__[i__]);
/*     .......... FORM VECTOR .......... */
	    i__3 = *n;
	    for (k = 1; k <= i__3; ++k) {
		h__ = z__[k + (i__ + 1) * z_dim1];
		z__[k + (i__ + 1) * z_dim1] = s * z__[k + i__ * z_dim1] + c__ 
			* h__;
		z__[k + i__ * z_dim1] = c__ * z__[k + i__ * z_dim1] - s * h__;
/* L180: */
	    }

/* L200: */
	}

	p = -s * s2 * c3 * el1 * e[l] / dl1;
	e[l] = s * p;
	d__[l] = c__ * p;
	tst2 = tst1 + (d__1 = e[l], abs(d__1));
	if (tst2 > tst1) {
	    goto L130;
	}
L220:
	d__[l] += f;
/* L240: */
    }
/*     .......... ORDER EIGENVALUES AND EIGENVECTORS .......... */
    i__1 = *n;
    for (ii = 2; ii <= i__1; ++ii) {
	i__ = ii - 1;
	k = i__;
	p = d__[i__];

	i__2 = *n;
	for (j = ii; j <= i__2; ++j) {
	    if (d__[j] >= p) {
		goto L260;
	    }
	    k = j;
	    p = d__[j];
L260:
	    ;
	}

	if (k == i__) {
	    goto L300;
	}
	d__[k] = d__[i__];
	d__[i__] = p;

	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
	    p = z__[j + i__ * z_dim1];
	    z__[j + i__ * z_dim1] = z__[j + k * z_dim1];
	    z__[j + k * z_dim1] = p;
/* L280: */
	}

L300:
	;
    }

    goto L1001;
/*     .......... SET ERROR -- NO CONVERGENCE TO AN */
/*                EIGENVALUE AFTER 30 ITERATIONS .......... */
L1000:
    *ierr = l;
L1001:
    return 0;
} /* tql2_ */

/* Subroutine */ int tqlrat(integer *n, doublereal *d__, doublereal *e2, 
	integer *ierr)
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    static doublereal b, c__, f, g, h__;
    static integer i__, j, l, m;
    static doublereal p, r__, s, t;
    static integer l1, ii;
    extern doublereal pythag(doublereal *, doublereal *), epslon(doublereal 
	    *);
    static integer mml;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TQLRAT, */
/*     ALGORITHM 464, COMM. ACM 16, 689(1973) BY REINSCH. */

/*     THIS SUBROUTINE FINDS THE EIGENVALUES OF A SYMMETRIC */
/*     TRIDIAGONAL MATRIX BY THE RATIONAL QL METHOD. */

/*     ON INPUT */

/*        N IS THE ORDER OF THE MATRIX. */

/*        D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX. */

/*        E2 CONTAINS THE SQUARES OF THE SUBDIAGONAL ELEMENTS OF THE */
/*          INPUT MATRIX IN ITS LAST N-1 POSITIONS.  E2(1) IS ARBITRARY. */

/*      ON OUTPUT */

/*        D CONTAINS THE EIGENVALUES IN ASCENDING ORDER.  IF AN */
/*          ERROR EXIT IS MADE, THE EIGENVALUES ARE CORRECT AND */
/*          ORDERED FOR INDICES 1,2,...IERR-1, BUT MAY NOT BE */
/*          THE SMALLEST EIGENVALUES. */

/*        E2 HAS BEEN DESTROYED. */

/*        IERR IS SET TO */
/*          ZERO       FOR NORMAL RETURN, */
/*          J          IF THE J-TH EIGENVALUE HAS NOT BEEN */
/*                     DETERMINED AFTER 30 ITERATIONS. */

/*     CALLS PYTHAG FOR  DSQRT(A*A + B*B) . */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY */

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ */

/*     unnecessary initialization of B and C to keep g77 -Wall happy */

    /* Parameter adjustments */
    --e2;
    --d__;

    /* Function Body */
    b = 0.;
    c__ = 0.;

    *ierr = 0;
    if (*n == 1) {
	goto L1001;
    }

    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
/* L100: */
	e2[i__ - 1] = e2[i__];
    }

    f = 0.;
    t = 0.;
    e2[*n] = 0.;

    i__1 = *n;
    for (l = 1; l <= i__1; ++l) {
	j = 0;
	h__ = (d__1 = d__[l], abs(d__1)) + sqrt(e2[l]);
	if (t > h__) {
	    goto L105;
	}
	t = h__;
	b = epslon(&t);
	c__ = b * b;
/*     .......... LOOK FOR SMALL SQUARED SUB-DIAGONAL ELEMENT .......... */
L105:
	i__2 = *n;
	for (m = l; m <= i__2; ++m) {
	    if (e2[m] <= c__) {
		goto L120;
	    }
/*     .......... E2(N) IS ALWAYS ZERO, SO THERE IS NO EXIT */
/*                THROUGH THE BOTTOM OF THE LOOP .......... */
/* L110: */
	}

L120:
	if (m == l) {
	    goto L210;
	}
L130:
	if (j == 30) {
	    goto L1000;
	}
	++j;
/*     .......... FORM SHIFT .......... */
	l1 = l + 1;
	s = sqrt(e2[l]);
	g = d__[l];
	p = (d__[l1] - g) / (s * 2.);
	r__ = pythag(&p, &c_b296);
	d__[l] = s / (p + d_sign(&r__, &p));
	h__ = g - d__[l];

	i__2 = *n;
	for (i__ = l1; i__ <= i__2; ++i__) {
/* L140: */
	    d__[i__] -= h__;
	}

	f += h__;
/*     .......... RATIONAL QL TRANSFORMATION .......... */
	g = d__[m];
	if (g == 0.) {
	    g = b;
	}
	h__ = g;
	s = 0.;
	mml = m - l;
/*     .......... FOR I=M-1 STEP -1 UNTIL L DO -- .......... */
	i__2 = mml;
	for (ii = 1; ii <= i__2; ++ii) {
	    i__ = m - ii;
	    p = g * h__;
	    r__ = p + e2[i__];
	    e2[i__ + 1] = s * r__;
	    s = e2[i__] / r__;
	    d__[i__ + 1] = h__ + s * (h__ + d__[i__]);
	    g = d__[i__] - e2[i__] / g;
	    if (g == 0.) {
		g = b;
	    }
	    h__ = g * p / r__;
/* L200: */
	}

	e2[l] = s * g;
	d__[l] = h__;
/*     .......... GUARD AGAINST UNDERFLOW IN CONVERGENCE TEST .......... */
	if (h__ == 0.) {
	    goto L210;
	}
	if ((d__1 = e2[l], abs(d__1)) <= (d__2 = c__ / h__, abs(d__2))) {
	    goto L210;
	}
	e2[l] = h__ * e2[l];
	if (e2[l] != 0.) {
	    goto L130;
	}
L210:
	p = d__[l] + f;
/*     .......... ORDER EIGENVALUES .......... */
	if (l == 1) {
	    goto L250;
	}
/*     .......... FOR I=L STEP -1 UNTIL 2 DO -- .......... */
	i__2 = l;
	for (ii = 2; ii <= i__2; ++ii) {
	    i__ = l + 2 - ii;
	    if (p >= d__[i__ - 1]) {
		goto L270;
	    }
	    d__[i__] = d__[i__ - 1];
/* L230: */
	}

L250:
	i__ = 1;
L270:
	d__[i__] = p;
/* L290: */
    }

    goto L1001;
/*     .......... SET ERROR -- NO CONVERGENCE TO AN */
/*                EIGENVALUE AFTER 30 ITERATIONS .......... */
L1000:
    *ierr = l;
L1001:
    return 0;
} /* tqlrat_ */

/* Subroutine */ int tred1(integer *nm, integer *n, doublereal *a, 
	doublereal *d__, doublereal *e, doublereal *e2)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    doublereal d__1;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    static doublereal f, g, h__;
    static integer i__, j, k, l;
    static doublereal scale;
    static integer ii, jp1;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TRED1, */
/*     NUM. MATH. 11, 181-195(1968) BY MARTIN, REINSCH, AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971). */

/*     THIS SUBROUTINE REDUCES A REAL SYMMETRIC MATRIX */
/*     TO A SYMMETRIC TRIDIAGONAL MATRIX USING */
/*     ORTHOGONAL SIMILARITY TRANSFORMATIONS. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        A CONTAINS THE REAL SYMMETRIC INPUT MATRIX.  ONLY THE */
/*          LOWER TRIANGLE OF THE MATRIX NEED BE SUPPLIED. */

/*     ON OUTPUT */

/*        A CONTAINS INFORMATION ABOUT THE ORTHOGONAL TRANS- */
/*          FORMATIONS USED IN THE REDUCTION IN ITS STRICT LOWER */
/*          TRIANGLE.  THE FULL UPPER TRIANGLE OF A IS UNALTERED. */

/*        D CONTAINS THE DIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX. */

/*        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE TRIDIAGONAL */
/*          MATRIX IN ITS LAST N-1 POSITIONS.  E(1) IS SET TO ZERO. */

/*        E2 CONTAINS THE SQUARES OF THE CORRESPONDING ELEMENTS OF E. */
/*          E2 MAY COINCIDE WITH E IF THE SQUARES ARE NOT NEEDED. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY */

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    --e2;
    --e;
    --d__;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	d__[i__] = a[*n + i__ * a_dim1];
	a[*n + i__ * a_dim1] = a[i__ + i__ * a_dim1];
/* L100: */
    }
/*     .......... FOR I=N STEP -1 UNTIL 1 DO -- .......... */
    i__1 = *n;
    for (ii = 1; ii <= i__1; ++ii) {
	i__ = *n + 1 - ii;
	l = i__ - 1;
	h__ = 0.;
	scale = 0.;
	if (l < 1) {
	    goto L130;
	}
/*     .......... SCALE ROW (ALGOL TOL THEN NOT NEEDED) .......... */
	i__2 = l;
	for (k = 1; k <= i__2; ++k) {
/* L120: */
	    scale += (d__1 = d__[k], abs(d__1));
	}

	if (scale != 0.) {
	    goto L140;
	}

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    d__[j] = a[l + j * a_dim1];
	    a[l + j * a_dim1] = a[i__ + j * a_dim1];
	    a[i__ + j * a_dim1] = 0.;
/* L125: */
	}

L130:
	e[i__] = 0.;
	e2[i__] = 0.;
	goto L300;

L140:
	i__2 = l;
	for (k = 1; k <= i__2; ++k) {
	    d__[k] /= scale;
	    h__ += d__[k] * d__[k];
/* L150: */
	}

	e2[i__] = scale * scale * h__;
	f = d__[l];
	d__1 = sqrt(h__);
	g = -d_sign(&d__1, &f);
	e[i__] = scale * g;
	h__ -= f * g;
	d__[l] = f - g;
	if (l == 1) {
	    goto L285;
	}
/*     .......... FORM A*U .......... */
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
/* L170: */
	    e[j] = 0.;
	}

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    f = d__[j];
	    g = e[j] + a[j + j * a_dim1] * f;
	    jp1 = j + 1;
	    if (l < jp1) {
		goto L220;
	    }

	    i__3 = l;
	    for (k = jp1; k <= i__3; ++k) {
		g += a[k + j * a_dim1] * d__[k];
		e[k] += a[k + j * a_dim1] * f;
/* L200: */
	    }

L220:
	    e[j] = g;
/* L240: */
	}
/*     .......... FORM P .......... */
	f = 0.;

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    e[j] /= h__;
	    f += e[j] * d__[j];
/* L245: */
	}

	h__ = f / (h__ + h__);
/*     .......... FORM Q .......... */
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
/* L250: */
	    e[j] -= h__ * d__[j];
	}
/*     .......... FORM REDUCED A .......... */
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    f = d__[j];
	    g = e[j];

	    i__3 = l;
	    for (k = j; k <= i__3; ++k) {
/* L260: */
		a[k + j * a_dim1] = a[k + j * a_dim1] - f * e[k] - g * d__[k];
	    }

/* L280: */
	}

L285:
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    f = d__[j];
	    d__[j] = a[l + j * a_dim1];
	    a[l + j * a_dim1] = a[i__ + j * a_dim1];
	    a[i__ + j * a_dim1] = f * scale;
/* L290: */
	}

L300:
	;
    }

    return 0;
} /* tred1_ */

/* Subroutine */ int tred2(integer *nm, integer *n, doublereal *a, 
	doublereal *d__, doublereal *e, doublereal *z__)
{
    /* System generated locals */
    integer a_dim1, a_offset, z_dim1, z_offset, i__1, i__2, i__3;
    doublereal d__1;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    static doublereal f, g, h__;
    static integer i__, j, k, l;
    static doublereal scale, hh;
    static integer ii, jp1;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TRED2, */
/*     NUM. MATH. 11, 181-195(1968) BY MARTIN, REINSCH, AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971). */

/*     THIS SUBROUTINE REDUCES A REAL SYMMETRIC MATRIX TO A */
/*     SYMMETRIC TRIDIAGONAL MATRIX USING AND ACCUMULATING */
/*     ORTHOGONAL SIMILARITY TRANSFORMATIONS. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        A CONTAINS THE REAL SYMMETRIC INPUT MATRIX.  ONLY THE */
/*          LOWER TRIANGLE OF THE MATRIX NEED BE SUPPLIED. */

/*     ON OUTPUT */

/*        D CONTAINS THE DIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX. */

/*        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE TRIDIAGONAL */
/*          MATRIX IN ITS LAST N-1 POSITIONS.  E(1) IS SET TO ZERO. */

/*        Z CONTAINS THE ORTHOGONAL TRANSFORMATION MATRIX */
/*          PRODUCED IN THE REDUCTION. */

/*        A AND Z MAY COINCIDE.  IF DISTINCT, A IS UNALTERED. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY */

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z__ -= z_offset;
    --e;
    --d__;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {

	i__2 = *n;
	for (j = i__; j <= i__2; ++j) {
/* L80: */
	    z__[j + i__ * z_dim1] = a[j + i__ * a_dim1];
	}

	d__[i__] = a[*n + i__ * a_dim1];
/* L100: */
    }

    if (*n == 1) {
	goto L510;
    }
/*     .......... FOR I=N STEP -1 UNTIL 2 DO -- .......... */
    i__1 = *n;
    for (ii = 2; ii <= i__1; ++ii) {
	i__ = *n + 2 - ii;
	l = i__ - 1;
	h__ = 0.;
	scale = 0.;
	if (l < 2) {
	    goto L130;
	}
/*     .......... SCALE ROW (ALGOL TOL THEN NOT NEEDED) .......... */
	i__2 = l;
	for (k = 1; k <= i__2; ++k) {
/* L120: */
	    scale += (d__1 = d__[k], abs(d__1));
	}

	if (scale != 0.) {
	    goto L140;
	}
L130:
	e[i__] = d__[l];

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    d__[j] = z__[l + j * z_dim1];
	    z__[i__ + j * z_dim1] = 0.;
	    z__[j + i__ * z_dim1] = 0.;
/* L135: */
	}

	goto L290;

L140:
	i__2 = l;
	for (k = 1; k <= i__2; ++k) {
	    d__[k] /= scale;
	    h__ += d__[k] * d__[k];
/* L150: */
	}

	f = d__[l];
	d__1 = sqrt(h__);
	g = -d_sign(&d__1, &f);
	e[i__] = scale * g;
	h__ -= f * g;
	d__[l] = f - g;
/*     .......... FORM A*U .......... */
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
/* L170: */
	    e[j] = 0.;
	}

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    f = d__[j];
	    z__[j + i__ * z_dim1] = f;
	    g = e[j] + z__[j + j * z_dim1] * f;
	    jp1 = j + 1;
	    if (l < jp1) {
		goto L220;
	    }

	    i__3 = l;
	    for (k = jp1; k <= i__3; ++k) {
		g += z__[k + j * z_dim1] * d__[k];
		e[k] += z__[k + j * z_dim1] * f;
/* L200: */
	    }

L220:
	    e[j] = g;
/* L240: */
	}
/*     .......... FORM P .......... */
	f = 0.;

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    e[j] /= h__;
	    f += e[j] * d__[j];
/* L245: */
	}

	hh = f / (h__ + h__);
/*     .......... FORM Q .......... */
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
/* L250: */
	    e[j] -= hh * d__[j];
	}
/*     .......... FORM REDUCED A .......... */
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    f = d__[j];
	    g = e[j];

	    i__3 = l;
	    for (k = j; k <= i__3; ++k) {
/* L260: */
		z__[k + j * z_dim1] = z__[k + j * z_dim1] - f * e[k] - g * 
			d__[k];
	    }

	    d__[j] = z__[l + j * z_dim1];
	    z__[i__ + j * z_dim1] = 0.;
/* L280: */
	}

L290:
	d__[i__] = h__;
/* L300: */
    }
/*     .......... ACCUMULATION OF TRANSFORMATION MATRICES .......... */
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	l = i__ - 1;
	z__[*n + l * z_dim1] = z__[l + l * z_dim1];
	z__[l + l * z_dim1] = 1.;
	h__ = d__[i__];
	if (h__ == 0.) {
	    goto L380;
	}

	i__2 = l;
	for (k = 1; k <= i__2; ++k) {
/* L330: */
	    d__[k] = z__[k + i__ * z_dim1] / h__;
	}

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    g = 0.;

	    i__3 = l;
	    for (k = 1; k <= i__3; ++k) {
/* L340: */
		g += z__[k + i__ * z_dim1] * z__[k + j * z_dim1];
	    }

	    i__3 = l;
	    for (k = 1; k <= i__3; ++k) {
		z__[k + j * z_dim1] -= g * d__[k];
/* L360: */
	    }
	}

L380:
	i__3 = l;
	for (k = 1; k <= i__3; ++k) {
/* L400: */
	    z__[k + i__ * z_dim1] = 0.;
	}

/* L500: */
    }

L510:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	d__[i__] = z__[*n + i__ * z_dim1];
	z__[*n + i__ * z_dim1] = 0.;
/* L520: */
    }

    z__[*n + *n * z_dim1] = 1.;
    e[1] = 0.;
    return 0;
} /* tred2_ */

/* Subroutine */ int rg(integer *nm, integer *n, doublereal *a, doublereal *
	wr, doublereal *wi, integer *matz, doublereal *z__, integer *iv1, 
	doublereal *fv1, integer *ierr)
{
    /* System generated locals */
    integer a_dim1, a_offset, z_dim1, z_offset;

    /* Local variables */
    extern /* Subroutine */ int balbak(integer *, integer *, integer *, 
	    integer *, doublereal *, integer *, doublereal *), balanc(
	    integer *, integer *, doublereal *, integer *, integer *, 
	    doublereal *), elmhes(integer *, integer *, integer *, integer *,
	     doublereal *, integer *), eltran(integer *, integer *, integer *
	    , integer *, doublereal *, integer *, doublereal *);
    static integer is1, is2;
    extern /* Subroutine */ int hqr(integer *, integer *, integer *, integer 
	    *, doublereal *, doublereal *, doublereal *, integer *), hqr2(
	    integer *, integer *, integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, integer *);



/*     THIS SUBROUTINE CALLS THE RECOMMENDED SEQUENCE OF */
/*     SUBROUTINES FROM THE EIGENSYSTEM SUBROUTINE PACKAGE (EISPACK) */
/*     TO FIND THE EIGENVALUES AND EIGENVECTORS (IF DESIRED) */
/*     OF A REAL GENERAL MATRIX. */

/*     ON INPUT */

/*        NM  MUST BE SET TO THE ROW DIMENSION OF THE TWO-DIMENSIONAL */
/*        ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*        DIMENSION STATEMENT. */

/*        N  IS THE ORDER OF THE MATRIX  A. */

/*        A  CONTAINS THE REAL GENERAL MATRIX. */

/*        MATZ  IS AN INTEGER VARIABLE SET EQUAL TO ZERO IF */
/*        ONLY EIGENVALUES ARE DESIRED.  OTHERWISE IT IS SET TO */
/*        ANY NON-ZERO INTEGER FOR BOTH EIGENVALUES AND EIGENVECTORS. */

/*     ON OUTPUT */

/*        WR  AND  WI  CONTAIN THE REAL AND IMAGINARY PARTS, */
/*        RESPECTIVELY, OF THE EIGENVALUES.  COMPLEX CONJUGATE */
/*        PAIRS OF EIGENVALUES APPEAR CONSECUTIVELY WITH THE */
/*        EIGENVALUE HAVING THE POSITIVE IMAGINARY PART FIRST. */

/*        Z  CONTAINS THE REAL AND IMAGINARY PARTS OF THE EIGENVECTORS */
/*        IF MATZ IS NOT ZERO.  IF THE J-TH EIGENVALUE IS REAL, THE */
/*        J-TH COLUMN OF  Z  CONTAINS ITS EIGENVECTOR.  IF THE J-TH */
/*        EIGENVALUE IS COMPLEX WITH POSITIVE IMAGINARY PART, THE */
/*        J-TH AND (J+1)-TH COLUMNS OF  Z  CONTAIN THE REAL AND */
/*        IMAGINARY PARTS OF ITS EIGENVECTOR.  THE CONJUGATE OF THIS */
/*        VECTOR IS THE EIGENVECTOR FOR THE CONJUGATE EIGENVALUE. */

/*        IERR  IS AN INTEGER OUTPUT VARIABLE SET EQUAL TO AN ERROR */
/*           COMPLETION CODE DESCRIBED IN THE DOCUMENTATION FOR HQR */
/*           AND HQR2.  THE NORMAL COMPLETION CODE IS ZERO. */

/*        IV1  AND  FV1  ARE TEMPORARY STORAGE ARRAYS. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY */

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    --fv1;
    --iv1;
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z__ -= z_offset;
    --wi;
    --wr;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    if (*n <= *nm) {
	goto L10;
    }
    *ierr = *n * 10;
    goto L50;

L10:
    balanc(nm, n, &a[a_offset], &is1, &is2, &fv1[1]);
    elmhes(nm, n, &is1, &is2, &a[a_offset], &iv1[1]);
    if (*matz != 0) {
	goto L20;
    }
/*     .......... FIND EIGENVALUES ONLY .......... */
    hqr(nm, n, &is1, &is2, &a[a_offset], &wr[1], &wi[1], ierr);
    goto L50;
/*     .......... FIND BOTH EIGENVALUES AND EIGENVECTORS .......... */
L20:
    eltran(nm, n, &is1, &is2, &a[a_offset], &iv1[1], &z__[z_offset]);
    hqr2(nm, n, &is1, &is2, &a[a_offset], &wr[1], &wi[1], &z__[z_offset], 
	    ierr);
    if (*ierr != 0) {
	goto L50;
    }
    balbak(nm, n, &is1, &is2, &fv1[1], n, &z__[z_offset]);
L50:
    return 0;
} /* rg_ */

/* Subroutine */ int rs(integer *nm, integer *n, doublereal *a, doublereal *
	w, integer *matz, doublereal *z__, doublereal *fv1, doublereal *fv2, 
	integer *ierr)
{
    /* System generated locals */
    integer a_dim1, a_offset, z_dim1, z_offset;

    /* Local variables */
    extern /* Subroutine */ int tred1(integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *), tred2(integer *, 
	    integer *, doublereal *, doublereal *, doublereal *, doublereal *)
	    , tqlrat(integer *, doublereal *, doublereal *, integer *), 
	    tql2(integer *, integer *, doublereal *, doublereal *, 
	    doublereal *, integer *);



/*     THIS SUBROUTINE CALLS THE RECOMMENDED SEQUENCE OF */
/*     SUBROUTINES FROM THE EIGENSYSTEM SUBROUTINE PACKAGE (EISPACK) */
/*     TO FIND THE EIGENVALUES AND EIGENVECTORS (IF DESIRED) */
/*     OF A REAL SYMMETRIC MATRIX. */

/*     ON INPUT */

/*        NM  MUST BE SET TO THE ROW DIMENSION OF THE TWO-DIMENSIONAL */
/*        ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*        DIMENSION STATEMENT. */

/*        N  IS THE ORDER OF THE MATRIX  A. */

/*        A  CONTAINS THE REAL SYMMETRIC MATRIX. */

/*        MATZ  IS AN INTEGER VARIABLE SET EQUAL TO ZERO IF */
/*        ONLY EIGENVALUES ARE DESIRED.  OTHERWISE IT IS SET TO */
/*        ANY NON-ZERO INTEGER FOR BOTH EIGENVALUES AND EIGENVECTORS. */

/*     ON OUTPUT */

/*        W  CONTAINS THE EIGENVALUES IN ASCENDING ORDER. */

/*        Z  CONTAINS THE EIGENVECTORS IF MATZ IS NOT ZERO. */

/*        IERR  IS AN INTEGER OUTPUT VARIABLE SET EQUAL TO AN ERROR */
/*           COMPLETION CODE DESCRIBED IN THE DOCUMENTATION FOR TQLRAT */
/*           AND TQL2.  THE NORMAL COMPLETION CODE IS ZERO. */

/*        FV1  AND  FV2  ARE TEMPORARY STORAGE ARRAYS. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY */

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    --fv2;
    --fv1;
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z__ -= z_offset;
    --w;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    if (*n <= *nm) {
	goto L10;
    }
    *ierr = *n * 10;
    goto L50;

L10:
    if (*matz != 0) {
	goto L20;
    }
/*     .......... FIND EIGENVALUES ONLY .......... */
    tred1(nm, n, &a[a_offset], &w[1], &fv1[1], &fv2[1]);
    tqlrat(n, &w[1], &fv2[1], ierr);
    goto L50;
/*     .......... FIND BOTH EIGENVALUES AND EIGENVECTORS .......... */
L20:
    tred2(nm, n, &a[a_offset], &w[1], &fv1[1], &z__[z_offset]);
    tql2(nm, n, &w[1], &fv1[1], &z__[z_offset], ierr);
L50:
    return 0;
} /* rs_ */

/* Subroutine */ int cg(integer *nm, integer *n, doublereal *ar, doublereal *
	ai, doublereal *wr, doublereal *wi, integer *matz, doublereal *zr, 
	doublereal *zi, doublereal *fv1, doublereal *fv2, doublereal *fv3, 
	integer *ierr)
{
    /* System generated locals */
    integer ar_dim1, ar_offset, ai_dim1, ai_offset, zr_dim1, zr_offset, 
	    zi_dim1, zi_offset;

    /* Local variables */
    extern /* Subroutine */ int cbal(integer *, integer *, doublereal *, 
	    doublereal *, integer *, integer *, doublereal *), corth(integer 
	    *, integer *, integer *, integer *, doublereal *, doublereal *, 
	    doublereal *, doublereal *), comqr(integer *, integer *, integer 
	    *, integer *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, integer *), cbabk2(integer *, integer *, integer *,
	     integer *, doublereal *, integer *, doublereal *, doublereal *), 
	    comqr2(integer *, integer *, integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, integer *);
    static integer is1, is2;



/*     THIS SUBROUTINE CALLS THE RECOMMENDED SEQUENCE OF */
/*     SUBROUTINES FROM THE EIGENSYSTEM SUBROUTINE PACKAGE (EISPACK) */
/*     TO FIND THE EIGENVALUES AND EIGENVECTORS (IF DESIRED) */
/*     OF A COMPLEX GENERAL MATRIX. */

/*     ON INPUT */

/*        NM  MUST BE SET TO THE ROW DIMENSION OF THE TWO-DIMENSIONAL */
/*        ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*        DIMENSION STATEMENT. */

/*        N  IS THE ORDER OF THE MATRIX  A=(AR,AI). */

/*        AR  AND  AI  CONTAIN THE REAL AND IMAGINARY PARTS, */
/*        RESPECTIVELY, OF THE COMPLEX GENERAL MATRIX. */

/*        MATZ  IS AN INTEGER VARIABLE SET EQUAL TO ZERO IF */
/*        ONLY EIGENVALUES ARE DESIRED.  OTHERWISE IT IS SET TO */
/*        ANY NON-ZERO INTEGER FOR BOTH EIGENVALUES AND EIGENVECTORS. */

/*     ON OUTPUT */

/*        WR  AND  WI  CONTAIN THE REAL AND IMAGINARY PARTS, */
/*        RESPECTIVELY, OF THE EIGENVALUES. */

/*        ZR  AND  ZI  CONTAIN THE REAL AND IMAGINARY PARTS, */
/*        RESPECTIVELY, OF THE EIGENVECTORS IF MATZ IS NOT ZERO. */

/*        IERR  IS AN INTEGER OUTPUT VARIABLE SET EQUAL TO AN ERROR */
/*           COMPLETION CODE DESCRIBED IN THE DOCUMENTATION FOR COMQR */
/*           AND COMQR2.  THE NORMAL COMPLETION CODE IS ZERO. */

/*        FV1, FV2, AND  FV3  ARE TEMPORARY STORAGE ARRAYS. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY */

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    --fv3;
    --fv2;
    --fv1;
    zi_dim1 = *nm;
    zi_offset = zi_dim1 + 1;
    zi -= zi_offset;
    zr_dim1 = *nm;
    zr_offset = zr_dim1 + 1;
    zr -= zr_offset;
    --wi;
    --wr;
    ai_dim1 = *nm;
    ai_offset = ai_dim1 + 1;
    ai -= ai_offset;
    ar_dim1 = *nm;
    ar_offset = ar_dim1 + 1;
    ar -= ar_offset;

    /* Function Body */
    if (*n <= *nm) {
	goto L10;
    }
    *ierr = *n * 10;
    goto L50;

L10:
    cbal(nm, n, &ar[ar_offset], &ai[ai_offset], &is1, &is2, &fv1[1]);
    corth(nm, n, &is1, &is2, &ar[ar_offset], &ai[ai_offset], &fv2[1], &fv3[1]
	    );
    if (*matz != 0) {
	goto L20;
    }
/*     .......... FIND EIGENVALUES ONLY .......... */
    comqr(nm, n, &is1, &is2, &ar[ar_offset], &ai[ai_offset], &wr[1], &wi[1], 
	    ierr);
    goto L50;
/*     .......... FIND BOTH EIGENVALUES AND EIGENVECTORS .......... */
L20:
    comqr2(nm, n, &is1, &is2, &fv2[1], &fv3[1], &ar[ar_offset], &ai[
	    ai_offset], &wr[1], &wi[1], &zr[zr_offset], &zi[zi_offset], ierr);
    if (*ierr != 0) {
	goto L50;
    }
    cbabk2(nm, n, &is1, &is2, &fv1[1], n, &zr[zr_offset], &zi[zi_offset]);
L50:
    return 0;
} /* cg_ */

/* Subroutine */ int ch(integer *nm, integer *n, doublereal *ar, doublereal *
	ai, doublereal *w, integer *matz, doublereal *zr, doublereal *zi, 
	doublereal *fv1, doublereal *fv2, doublereal *fm1, integer *ierr)
{
    /* System generated locals */
    integer ar_dim1, ar_offset, ai_dim1, ai_offset, zr_dim1, zr_offset, 
	    zi_dim1, zi_offset, i__1, i__2;

    /* Local variables */
    static integer i__, j;
    extern /* Subroutine */ int htridi(integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *), htribk(integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, integer *, doublereal *, doublereal *)
	    , tqlrat(integer *, doublereal *, doublereal *, integer *), 
	    tql2(integer *, integer *, doublereal *, doublereal *, 
	    doublereal *, integer *);



/*     THIS SUBROUTINE CALLS THE RECOMMENDED SEQUENCE OF */
/*     SUBROUTINES FROM THE EIGENSYSTEM SUBROUTINE PACKAGE (EISPACK) */
/*     TO FIND THE EIGENVALUES AND EIGENVECTORS (IF DESIRED) */
/*     OF A COMPLEX HERMITIAN MATRIX. */

/*     ON INPUT */

/*        NM  MUST BE SET TO THE ROW DIMENSION OF THE TWO-DIMENSIONAL */
/*        ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*        DIMENSION STATEMENT. */

/*        N  IS THE ORDER OF THE MATRIX  A=(AR,AI). */

/*        AR  AND  AI  CONTAIN THE REAL AND IMAGINARY PARTS, */
/*        RESPECTIVELY, OF THE COMPLEX HERMITIAN MATRIX. */

/*        MATZ  IS AN INTEGER VARIABLE SET EQUAL TO ZERO IF */
/*        ONLY EIGENVALUES ARE DESIRED.  OTHERWISE IT IS SET TO */
/*        ANY NON-ZERO INTEGER FOR BOTH EIGENVALUES AND EIGENVECTORS. */

/*     ON OUTPUT */

/*        W  CONTAINS THE EIGENVALUES IN ASCENDING ORDER. */

/*        ZR  AND  ZI  CONTAIN THE REAL AND IMAGINARY PARTS, */
/*        RESPECTIVELY, OF THE EIGENVECTORS IF MATZ IS NOT ZERO. */

/*        IERR  IS AN INTEGER OUTPUT VARIABLE SET EQUAL TO AN ERROR */
/*           COMPLETION CODE DESCRIBED IN THE DOCUMENTATION FOR TQLRAT */
/*           AND TQL2.  THE NORMAL COMPLETION CODE IS ZERO. */

/*        FV1, FV2, AND  FM1  ARE TEMPORARY STORAGE ARRAYS. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY */

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ */

    /* Parameter adjustments */
    fm1 -= 3;
    --fv2;
    --fv1;
    zi_dim1 = *nm;
    zi_offset = zi_dim1 + 1;
    zi -= zi_offset;
    zr_dim1 = *nm;
    zr_offset = zr_dim1 + 1;
    zr -= zr_offset;
    --w;
    ai_dim1 = *nm;
    ai_offset = ai_dim1 + 1;
    ai -= ai_offset;
    ar_dim1 = *nm;
    ar_offset = ar_dim1 + 1;
    ar -= ar_offset;

    /* Function Body */
    if (*n <= *nm) {
	goto L10;
    }
    *ierr = *n * 10;
    goto L50;

L10:
    htridi(nm, n, &ar[ar_offset], &ai[ai_offset], &w[1], &fv1[1], &fv2[1], &
	    fm1[3]);
    if (*matz != 0) {
	goto L20;
    }
/*     .......... FIND EIGENVALUES ONLY .......... */
    tqlrat(n, &w[1], &fv2[1], ierr);
    goto L50;
/*     .......... FIND BOTH EIGENVALUES AND EIGENVECTORS .......... */
L20:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {

	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
	    zr[j + i__ * zr_dim1] = 0.;
/* L30: */
	}

	zr[i__ + i__ * zr_dim1] = 1.;
/* L40: */
    }

    tql2(nm, n, &w[1], &fv1[1], &zr[zr_offset], ierr);
    if (*ierr != 0) {
	goto L50;
    }
    htribk(nm, n, &ar[ar_offset], &ai[ai_offset], &fm1[3], n, &zr[zr_offset],
	     &zi[zi_offset]);
L50:
    return 0;
} /* ch_ */
