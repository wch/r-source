/* ../appl/hclust.f -- translated by f2c (version 19931217).
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
/*  FLAG              boolean indicator of agglomerable obj./ C */
/*                    clusters.                               C */
/*                                                            C */
/*  F. Murtagh, ESA/ESO/STECF, Garching, February 1986.       C */
/*  R. Ihaka, Modifications for R, December 1996              C */
/*                                                            C */
/* ------------------------------------------------------------C */
/* Subroutine */ int hclust_(n, len, iopt, ia, ib, crit, membr, nn, disnn, 
	flag_, diss)
integer *n, *len, *iopt, *ia, *ib;
doublereal *crit, *membr;
integer *nn;
doublereal *disnn;
logical *flag_;
doublereal *diss;
{
    /* Initialized data */

    static doublereal inf = 1e20;

    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1, d__2;

    /* Local variables */
    static doublereal dmin_;
    static integer i, j, k;
    static doublereal x;
    static integer i2, j2, jj, im, jm;
    static doublereal xx;
    static integer ind, ncl;
    extern integer ioffset_();
    static integer ind1, ind2, ind3;

    /* Parameter adjustments */
    --flag_;
    --disnn;
    --nn;
    --membr;
    --crit;
    --ib;
    --ia;
    --diss;

    /* Function Body */

/*  Initializations */

    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	membr[i] = (float)1.;
	flag_[i] = TRUE_;
    }
    ncl = *n;

/*  Carry out an agglomeration - first create list of NNs */

    i__1 = *n - 1;
    for (i = 1; i <= i__1; ++i) {
	dmin_ = inf;
	i__2 = *n;
	for (j = i + 1; j <= i__2; ++j) {
	    ind = ioffset_(n, &i, &j);
	    if (diss[ind] >= dmin_) {
		goto L500;
	    }
	    dmin_ = diss[ind];
	    jm = j;
L500:
	    ;
	}
	nn[i] = jm;
	disnn[i] = dmin_;
    }

L400:
/*     Next, determine least diss. using list of NNs */
    dmin_ = inf;
    i__1 = *n - 1;
    for (i = 1; i <= i__1; ++i) {
	if (! flag_[i]) {
	    goto L600;
	}
	if (disnn[i] >= dmin_) {
	    goto L600;
	}
	dmin_ = disnn[i];
	im = i;
	jm = nn[i];
L600:
	;
    }
    --ncl;

/*  This allows an agglomeration to be carried out. */

    i2 = min(im,jm);
    j2 = max(im,jm);
    ia[*n - ncl] = i2;
    ib[*n - ncl] = j2;
    crit[*n - ncl] = dmin_;

/*  Update dissimilarities from new cluster. */

    flag_[j2] = FALSE_;
    dmin_ = inf;
    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {
	if (! flag_[k]) {
	    goto L800;
	}
	if (k == i2) {
	    goto L800;
	}
	x = membr[i2] + membr[j2] + membr[k];
	if (i2 < k) {
	    ind1 = ioffset_(n, &i2, &k);
	} else {
	    ind1 = ioffset_(n, &k, &i2);
	}
	if (j2 < k) {
	    ind2 = ioffset_(n, &j2, &k);
	} else {
	    ind2 = ioffset_(n, &k, &j2);
	}
	ind3 = ioffset_(n, &i2, &j2);
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
	    diss[ind1] = diss[ind1] * (float).5 + diss[ind2] * (float).5;
	}

/*  MEDIAN (GOWER'S) METHOD - IOPT=6. */

	if (*iopt == 6) {
	    diss[ind1] = diss[ind1] * (float).5 + diss[ind2] * (float).5 - xx 
		    * (float).25;
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
	if (diss[ind1] >= dmin_) {
	    goto L800;
	}
	dmin_ = diss[ind1];
	jj = k;
L800:
	;
    }
    membr[i2] += membr[j2];
    disnn[i2] = dmin_;
    nn[i2] = jj;

/*  Update list of NNs insofar as this is required. */

    i__1 = *n - 1;
    for (i = 1; i <= i__1; ++i) {
	if (! flag_[i]) {
	    goto L900;
	}
	if (nn[i] == i2) {
	    goto L850;
	}
	if (nn[i] == j2) {
	    goto L850;
	}
	goto L900;
L850:
/*        (Redetermine NN of I:) */
	dmin_ = inf;
	i__2 = *n;
	for (j = i + 1; j <= i__2; ++j) {
	    ind = ioffset_(n, &i, &j);
	    if (! flag_[j]) {
		goto L870;
	    }
	    if (i == j) {
		goto L870;
	    }
	    if (diss[ind] >= dmin_) {
		goto L870;
	    }
	    dmin_ = diss[ind];
	    jj = j;
L870:
	    ;
	}
	nn[i] = jj;
	disnn[i] = dmin_;
L900:
	;
    }

/*  Repeat previous steps until N-1 agglomerations carried out. */

    if (ncl > 1) {
	goto L400;
    }


    return 0;
} /* hclust_ */



integer ioffset_(n, i, j)
integer *n, *i, *j;
{
    /* System generated locals */
    integer ret_val;

/*  Map row I and column J of upper half diagonal symmetric matrix */
/*  onto vector. */
    ret_val = *j + (*i - 1) * *n - *i * (*i + 1) / 2;
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
/* Subroutine */ int hcass2_(n, ia, ib, iorder, iia, iib)
integer *n, *ia, *ib, *iorder, *iia, *iib;
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Local variables */
    static integer i, j, k, k1, k2, loc;


/*    Following bit is to get seq. of merges into format acceptable to plc
lust*/
/*    I coded clusters as lowest seq. no. of constituents; S's `hclust' co
des*/
/*     singletons as -ve numbers, and non-singletons with their seq. nos. 
*/

    /* Parameter adjustments */
    --iib;
    --iia;
    --iorder;
    --ib;
    --ia;

    /* Function Body */
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	iia[i] = ia[i];
	iib[i] = ib[i];
/* L912: */
    }
    i__1 = *n - 2;
    for (i = 1; i <= i__1; ++i) {
/*        In the following, smallest (+ve or -ve) seq. no. wanted */
/* Computing MIN */
	i__2 = ia[i], i__3 = ib[i];
	k = min(i__2,i__3);
	i__2 = *n - 1;
	for (j = i + 1; j <= i__2; ++j) {
	    if (ia[j] == k) {
		iia[j] = -i;
	    }
	    if (ib[j] == k) {
		iib[j] = -i;
	    }
/* L913: */
	}
/* L915: */
    }
    i__1 = *n - 1;
    for (i = 1; i <= i__1; ++i) {
	iia[i] = -iia[i];
	iib[i] = -iib[i];
/* L916: */
    }
    i__1 = *n - 1;
    for (i = 1; i <= i__1; ++i) {
	if (iia[i] > 0 && iib[i] < 0) {
	    k = iia[i];
	    iia[i] = iib[i];
	    iib[i] = k;
	}
	if (iia[i] > 0 && iib[i] > 0) {
/* Computing MIN */
	    i__2 = iia[i], i__3 = iib[i];
	    k1 = min(i__2,i__3);
/* Computing MAX */
	    i__2 = iia[i], i__3 = iib[i];
	    k2 = max(i__2,i__3);
	    iia[i] = k1;
	    iib[i] = k2;
	}
/* L917: */
    }


/*     NEW PART FOR `ORDER' */

    iorder[1] = iia[*n - 1];
    iorder[2] = iib[*n - 1];
    loc = 2;
    for (i = *n - 2; i >= 1; --i) {
	i__1 = loc;
	for (j = 1; j <= i__1; ++j) {
	    if (iorder[j] == i) {
/*           REPLACE IORDER(J) WITH IIA(I) AND IIB(I) */
		iorder[j] = iia[i];
		if (j == loc) {
		    ++loc;
		    iorder[loc] = iib[i];
		    goto L171;
		}
		++loc;
		i__2 = j + 2;
		for (k = loc; k >= i__2; --k) {
		    iorder[k] = iorder[k - 1];
/* L95: */
		}
		iorder[j + 1] = iib[i];
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
    for (i = 1; i <= i__1; ++i) {
	iorder[i] = -iorder[i];
/* L181: */
    }


    return 0;
} /* hcass2_ */

