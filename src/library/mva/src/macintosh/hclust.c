/* hclust.f -- translated by f2c (version 19971204).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C */
/*                                                            C */
/*  HIERARCHICAL CLUSTERING using (user-specified) criterion. C */
/*                                                            C */
/*  Parameters:                                               C */
/*                                                            C */
/*  N                 the number of points being clustered    C */
/*  DISS(LEN)         dissimilarities in lower half diagonal  C */
/*                    storage; LEN = N.N-1/2,                 C */
/*  IOPT              clustering criterion to be used,        C */
/*  IA, IB, CRIT      history of agglomerations; dimensions   C */
/*                    N, first N-1 locations only used,       C */
/*  MEMBR, NN, DISNN  vectors of length N, used to store      C */
/*                    cluster cardinalities, current nearest  C */
/*                    neighbour, and the dissimilarity assoc. C */
/*                    with the latter.                        C */
/*                    MEMBR must be initialized by R to the   C */
/*                    default of  rep(1, N)                   C */
/*  FLAG              boolean indicator of agglomerable obj./ C */
/*                    clusters.                               C */
/*                                                            C */
/*  F. Murtagh, ESA/ESO/STECF, Garching, February 1986.       C */
/*  Modifications for R: Ross Ihaka, Dec 1996                 C */
/*                       Fritz Leisch, Jun 2000               C */
/* ------------------------------------------------------------C */
/* Subroutine */ int hclust(integer *n, integer *len, integer *iopt, integer 
	*ia, integer *ib, doublereal *crit, doublereal *membr, integer *nn, 
	doublereal *disnn, logical *flag__, doublereal *diss)
{
    /* Initialized data */

    static doublereal inf = 1e20;

    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1, d__2;

    /* Local variables */
    static doublereal dmin__;
    static integer i__, j, k;
    static doublereal x;
    static integer i2, j2, jj, im, jm;
    static doublereal xx;
    static integer ind, ncl;
    extern integer ioffset(integer *, integer *, integer *);
    static integer ind1, ind2, ind3;

/*     IMPLICIT UNDEFINED(A-H,O-Z) */
    /* Parameter adjustments */
    --flag__;
    --disnn;
    --nn;
    --membr;
    --crit;
    --ib;
    --ia;
    --diss;

    /* Function Body */

/*     unnecessary initialization of im jj jm to keep g77 -Wall happy */

    im = 0;
    jj = 0;
    jm = 0;

/*  Initializations */

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*        We do not initialize MEMBR in order to be able to restart the */
/*        algorithm from a cut. */
/*        MEMBR(I)=1. */
	flag__[i__] = TRUE_;
    }
    ncl = *n;

/*  Carry out an agglomeration - first create list of NNs */

    i__1 = *n - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	dmin__ = inf;
	i__2 = *n;
	for (j = i__ + 1; j <= i__2; ++j) {
	    ind = ioffset(n, &i__, &j);
	    if (diss[ind] >= dmin__) {
		goto L500;
	    }
	    dmin__ = diss[ind];
	    jm = j;
L500:
	    ;
	}
	nn[i__] = jm;
	disnn[i__] = dmin__;
    }

L400:
/*     Next, determine least diss. using list of NNs */
    dmin__ = inf;
    i__1 = *n - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (! flag__[i__]) {
	    goto L600;
	}
	if (disnn[i__] >= dmin__) {
	    goto L600;
	}
	dmin__ = disnn[i__];
	im = i__;
	jm = nn[i__];
L600:
	;
    }
    --ncl;

/*  This allows an agglomeration to be carried out. */

    i2 = min(im,jm);
    j2 = max(im,jm);
    ia[*n - ncl] = i2;
    ib[*n - ncl] = j2;
    crit[*n - ncl] = dmin__;

/*  Update dissimilarities from new cluster. */

    flag__[j2] = FALSE_;
    dmin__ = inf;
    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {
	if (! flag__[k]) {
	    goto L800;
	}
	if (k == i2) {
	    goto L800;
	}
	x = membr[i2] + membr[j2] + membr[k];
	if (i2 < k) {
	    ind1 = ioffset(n, &i2, &k);
	} else {
	    ind1 = ioffset(n, &k, &i2);
	}
	if (j2 < k) {
	    ind2 = ioffset(n, &j2, &k);
	} else {
	    ind2 = ioffset(n, &k, &j2);
	}
	ind3 = ioffset(n, &i2, &j2);
	xx = diss[ind3];

/*  WARD'S MINIMUM VARIANCE METHOD - IOPT=1. */

	if (*iopt == 1) {
	    diss[ind1] = (membr[i2] + membr[k]) * diss[ind1] + (membr[j2] + 
		    membr[k]) * diss[ind2] - membr[k] * xx;
	    diss[ind1] /= x;
	}

/*  SINGLE LINK METHOD - IOPT=2. */

	if (*iopt == 2) {
/* Computing MIN */
	    d__1 = diss[ind1], d__2 = diss[ind2];
	    diss[ind1] = min(d__1,d__2);
	}

/*  COMPLETE LINK METHOD - IOPT=3. */

	if (*iopt == 3) {
/* Computing MAX */
	    d__1 = diss[ind1], d__2 = diss[ind2];
	    diss[ind1] = max(d__1,d__2);
	}

/*  AVERAGE LINK (OR GROUP AVERAGE) METHOD - IOPT=4. */

	if (*iopt == 4) {
	    diss[ind1] = (membr[i2] * diss[ind1] + membr[j2] * diss[ind2]) / (
		    membr[i2] + membr[j2]);
	}

/*  MCQUITTY'S METHOD - IOPT=5. */

	if (*iopt == 5) {
	    diss[ind1] = diss[ind1] * .5f + diss[ind2] * .5f;
	}

/*  MEDIAN (GOWER'S) METHOD - IOPT=6. */

	if (*iopt == 6) {
	    diss[ind1] = diss[ind1] * .5f + diss[ind2] * .5f - xx * .25f;
	}

/*  CENTROID METHOD - IOPT=7. */

	if (*iopt == 7) {
	    diss[ind1] = (membr[i2] * diss[ind1] + membr[j2] * diss[ind2] - 
		    membr[i2] * membr[j2] * xx / (membr[i2] + membr[j2])) / (
		    membr[i2] + membr[j2]);
	}

	if (i2 > k) {
	    goto L800;
	}
	if (diss[ind1] >= dmin__) {
	    goto L800;
	}
	dmin__ = diss[ind1];
	jj = k;
L800:
	;
    }
    membr[i2] += membr[j2];
    disnn[i2] = dmin__;
    nn[i2] = jj;

/*  Update list of NNs insofar as this is required. */

    i__1 = *n - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (! flag__[i__]) {
	    goto L900;
	}
	if (nn[i__] == i2) {
	    goto L850;
	}
	if (nn[i__] == j2) {
	    goto L850;
	}
	goto L900;
L850:
/*        (Redetermine NN of I:) */
	dmin__ = inf;
	i__2 = *n;
	for (j = i__ + 1; j <= i__2; ++j) {
	    ind = ioffset(n, &i__, &j);
	    if (! flag__[j]) {
		goto L870;
	    }
	    if (i__ == j) {
		goto L870;
	    }
	    if (diss[ind] >= dmin__) {
		goto L870;
	    }
	    dmin__ = diss[ind];
	    jj = j;
L870:
	    ;
	}
	nn[i__] = jj;
	disnn[i__] = dmin__;
L900:
	;
    }

/*  Repeat previous steps until N-1 agglomerations carried out. */

    if (ncl > 1) {
	goto L400;
    }


    return 0;
} /* hclust_ */



integer ioffset(integer *n, integer *i__, integer *j)
{
    /* System generated locals */
    integer ret_val;

/*  Map row I and column J of upper half diagonal symmetric matrix */
/*  onto vector. */
    ret_val = *j + (*i__ - 1) * *n - *i__ * (*i__ + 1) / 2;
    return ret_val;
} /* ioffset_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C */
/*                                                               C */
/*  Given a HIERARCHIC CLUSTERING, described as a sequence of    C */
/*  agglomerations, prepare the seq. of aggloms. and "horiz."    C */
/*  order of objects for plotting the dendrogram using S routine C */
/*  'plclust'.                                                   C */
/*                                                               C */
/*  Parameters:                                                  C */
/*                                                               C */
/*  IA, IB:       vectors of dimension N defining the agglomer-  C */
/*                 ations.                                       C */
/*  IIA, IIB:     used to store IA and IB values differently     C */
/*                (in form needed for S command `plclust`        C */
/*  IORDER:       "horiz." order of objects for dendrogram       C */
/*                                                               C */
/*  F. Murtagh, ESA/ESO/STECF, Garching, June 1991               C */
/*                                                               C */
/*  HISTORY                                                      C */
/*                                                               C */
/*  Adapted from routine HCASS, which additionally determines    C */
/*   cluster assignments at all levels, at extra comput. expense C */
/*                                                               C */
/* ---------------------------------------------------------------C */
/* Subroutine */ int hcass2(integer *n, integer *ia, integer *ib, integer *
	iorder, integer *iia, integer *iib)
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Local variables */
    static integer i__, j, k, k1, k2, loc;


/*     Following bit is to get seq. of merges into format acceptable to plclust */
/*     I coded clusters as lowest seq. no. of constituents; S's `hclust' codes */
/*     singletons as -ve numbers, and non-singletons with their seq. nos. */

    /* Parameter adjustments */
    --iib;
    --iia;
    --iorder;
    --ib;
    --ia;

    /* Function Body */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	iia[i__] = ia[i__];
	iib[i__] = ib[i__];
/* L912: */
    }
    i__1 = *n - 2;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*        In the following, smallest (+ve or -ve) seq. no. wanted */
/* Computing MIN */
	i__2 = ia[i__], i__3 = ib[i__];
	k = min(i__2,i__3);
	i__2 = *n - 1;
	for (j = i__ + 1; j <= i__2; ++j) {
	    if (ia[j] == k) {
		iia[j] = -i__;
	    }
	    if (ib[j] == k) {
		iib[j] = -i__;
	    }
/* L913: */
	}
/* L915: */
    }
    i__1 = *n - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	iia[i__] = -iia[i__];
	iib[i__] = -iib[i__];
/* L916: */
    }
    i__1 = *n - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (iia[i__] > 0 && iib[i__] < 0) {
	    k = iia[i__];
	    iia[i__] = iib[i__];
	    iib[i__] = k;
	}
	if (iia[i__] > 0 && iib[i__] > 0) {
/* Computing MIN */
	    i__2 = iia[i__], i__3 = iib[i__];
	    k1 = min(i__2,i__3);
/* Computing MAX */
	    i__2 = iia[i__], i__3 = iib[i__];
	    k2 = max(i__2,i__3);
	    iia[i__] = k1;
	    iib[i__] = k2;
	}
/* L917: */
    }


/*     NEW PART FOR `ORDER' */

    iorder[1] = iia[*n - 1];
    iorder[2] = iib[*n - 1];
    loc = 2;
    for (i__ = *n - 2; i__ >= 1; --i__) {
	i__1 = loc;
	for (j = 1; j <= i__1; ++j) {
	    if (iorder[j] == i__) {
/*           REPLACE IORDER(J) WITH IIA(I) AND IIB(I) */
		iorder[j] = iia[i__];
		if (j == loc) {
		    ++loc;
		    iorder[loc] = iib[i__];
		    goto L171;
		}
		++loc;
		i__2 = j + 2;
		for (k = loc; k >= i__2; --k) {
		    iorder[k] = iorder[k - 1];
/* L95: */
		}
		iorder[j + 1] = iib[i__];
		goto L171;
	    }
/* L169: */
	}
/*       SHOULD NEVER REACH HERE */
L171:
/* L175: */
	;
    }


    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	iorder[i__] = -iorder[i__];
/* L181: */
    }


    return 0;
} /* hcass2_ */
