/*
 *	chull finds the convex hull of a set of points in the plane.
 *
 *	It is based on a C translation (by f2c) of
 *	ACM TOMS algorithm 523 by W. F. Eddy, vol 3 (1977), 398-403, 411-2.
 *
 *	converted to double precision, output order altered
 *	by B.D. Ripley, March 1999
 *
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <R_ext/Boolean.h>	/* TRUE,... */

static void split(int n, double *x,
		  int m, int *in,
		  int ii, int jj,
		  int s,
		  int *iabv, int *na, int *maxa,
		  int *ibel, int *nb, int *maxb)
{
/* split() takes the m points of array x whose
 subscripts are in array in and partitions them by the
 line joining the two points in array x whose subscripts are ii and jj.
 The subscripts of the points above the line are put into array
 iabv, and the subscripts of the points below are put into array ibel.

 na and nb are, respectively, the number of points
 above the line and the number below.
 maxa and maxb are the subscripts for array
 x of the point furthest above the line and the point
 furthest below, respectively. if either subset is null
 the corresponding subscript (maxa or maxb) is set to zero.

 formal parameters
 INPUT
	n    integer		total number of data points
	x    real array (2,n)	(x,y) co-ordinates of the data
	m    integer		number of points in input subset
	in   integer array (m)	subscripts for array x of the
				points in the input subset
	ii   integer		subscript for array x of one point
				on the partitioning line
	jj   integer		subscript for array x of another
				point on the partitioning line
	s    integer		switch to determine output.
				refer to comments below
 OUTPUT
	iabv integer array (m)	subscripts for array x of the
				points above the partitioning line
	na   integer		number of elements in iabv
	maxa integer		subscript for array x of point
				furthest above the line.
				set to zero if na is zero
	ibel integer array (m)	subscripts for array x of the
				points below the partitioning line
	nb   integer		number of elements in ibel
	maxb integer		subscript for array x of point
				furthest below the line.
				set to zero if nb is zero

 if s = 2 dont save ibel,nb,maxb.
 if s =-2 dont save iabv,na,maxa.
 otherwise save everything
 if s is positive the array being partitioned is above
 the initial partitioning line.
 if it is negative, then the set of points is below.
*/

    /* Local variables (=0 : -Wall) */
    double a=0, b=0, down, d1, up, xt, z;
    int i, is;
    Rboolean vert, neg_dir=0;

    /* Parameter adjustments */
    --x;

    xt = x[ii];
    /* Check to see if the line is vertical */
    vert = (x[jj] == xt);
    d1 = x[jj + n] - x[ii + n];
    if (vert) {
	neg_dir = ((s > 0 && d1 < 0.) || (s < 0 && d1 > 0.));
    } else {
	a = d1 / (x[jj] - xt);
	b = x[ii + n] - a * xt;
    }
    up	 = 0.; *na = 0; *maxa = 0;
    down = 0.; *nb = 0; *maxb = 0;
    for (i = 0; i < m; ++i) {
	is = in[i];
	if (vert) {
	    if(neg_dir) z = xt - x[is];
	    else	z = x[is] - xt;
	} else {
	    z = x[is + n] - a * x[is] - b;
	}
	if (z > 0.) {			/* the point is ABOVE the line */
	    if (s == -2) continue;
	    iabv[*na] = is;
	    ++(*na);
	    if (z >= up) {
		up = z;
		*maxa = *na;
	    }
	}
	else if (s != 2 && z < 0.) {	/* the point is BELOW the line */
	    ibel[*nb] = is;
	    ++(*nb);
	    if (z <= down) {
		down = z;
		*maxb = *nb;
	    }
	}
    }
}

static void in_chull(int *n, double *x, int *m, int *in,
		  int *ia, int *ib, int *ih, int *nh, int *il)
{
/* this subroutine determines which of the m points of array
 x whose subscripts are in array in are vertices of the
 minimum area convex polygon containing the m points. the
 subscripts of the vertices are placed in array ih in the
 order they are found. nh is the number of elements in
 array ih and array il. array il is a linked list giving
 the order of the elements of array ih in a counter
 clockwise direction. this algorithm corresponds to a
 preorder traversal of a certain binary tree. each vertex
 of the binary tree represents a subset of the m points.
 at each step the subset of points corresponding to the
 current vertex of the tree is partitioned by a line
 joining two vertices of the convex polygon. the left son
 vertex in the binary tree represents the subset of points
 above the partitioning line and the right son vertex, the
 subset below the line. the leaves of the tree represent
 either null subsets or subsets inside a triangle whose
 vertices coincide with vertices of the convex polygon.

 formal parameters
 INPUT
	n  integer		total number of data points (= nrow(x))
	x  real array (2,n)	(x,y) co-ordinates of the data
	m  integer		number of points in the input subset
	in integer array (m)	subscripts for array x of the points
				in the input subset
 work area
	ia integer array (m)	subscripts for array x of left son subsets.
				see comments after dimension statements
	ib integer array (m)	subscripts for array x of right son subsets

 OUTPUT
	ih integer array (m)	subscripts for array x of the
				vertices of the convex hull
	nh integer		number of elements in arrays ih and il.
				== number of vertices of the convex polygon
 il is used internally here.
	il integer array (m)	a linked list giving in order in a
				counter-clockwise direction the
				elements of array ih
 the upper end of array ia is used to store temporarily
 the sizes of the subsets which correspond to right son
 vertices, while traversing down the left sons when on the
 left half of the tree, and to store the sizes of the left
 sons while traversing the right sons(down the right half)
 */
#define y(k) x[k + x_dim1]

    Rboolean mine, maxe;
    int i, j, ilinh, ma, mb, kn, mm, kx, mx, mp1, mbb, nia, nib,
	inh, min, mxa, mxb, mxbb;
    int x_dim1, x_offset;
    double d1;

    /* Parameter adjustments */
    x_dim1 = *n;
    x_offset = 1;
    x -= x_offset;
    --il;
    --ih;
    --ib;
    --ia;
    --in;

    if (*m == 1) {
	goto L_1pt;
    }
    il[1] = 2;
    il[2] = 1;
    kn = in[1];
    kx = in[2];
    if (*m == 2) {
	goto L_2pts;
    }
    mp1 = *m + 1;
    min = 1;
    mx = 1;
    kx = in[1];
    maxe = FALSE;
    mine = FALSE;
    /* find two vertices of the convex hull for the initial partition */
    for (i = 2; i <= *m; ++i) {
	j = in[i];
	if ((d1 = x[j] - x[kx]) < 0.) {
	} else if (d1 == 0) {
	    maxe = TRUE;
	} else {
	    maxe = FALSE;
	    mx = i;
	    kx = j;
	}
	if ((d1 = x[j] - x[kn]) < 0.) {
	    mine = FALSE;
	    min = i;
	    kn = j;
	} else if (d1 == 0) {
	    mine = TRUE;
	}
    }

    if (kx == kn) { /* if the max and min are equal,
		     * all m points lie on a vertical line */
	goto L_vertical;
    }

    if (maxe || mine) {/* if maxe (or mine) is TRUE, there are several
			  maxima (or minima) with equal first coordinates */

	if (maxe) {/* have several points with the (same) largest x[] */
	    for (i = 1; i <= *m; ++i) {
		j = in[i];
		if (x[j] != x[kx]) continue;
		if (y(j) <= y(kx)) continue;
		mx = i;
		kx = j;
	    }
	}

	if (mine) {/* have several points with the (same) smallest x[] */
	    for (i = 1; i <= *m; ++i) {
		j = in[i];
		if (x[j] != x[kn]) continue;
		if (y(j) >= y(kn)) continue;
		min = i;
		kn = j;
	    }
	}

    }

/* L7:*/
    ih[1] = kx;
    ih[2] = kn;
    *nh = 3;
    inh = 1;
    nib = 1;
    ma = *m;
    in[mx] = in[*m];
    in[*m] = kx;
    mm = *m - 2;
    if (min == *m) {
	min = mx;
    }
    in[min] = in[*m - 1];
    in[*m - 1] = kn;
/* begin by partitioning the root of the tree */
    split(*n, &x[x_offset], mm, &in[1],
	  ih[1], ih[2],
	  0,
	  &ia[1], &mb, &mxa,
	  &ib[1], &ia[ma], &mxbb);

/*	first traverse the LEFT HALF of the tree */

/* start with the left son */
 L8:
    nib += ia[ma];
    --ma;
    do {
	if (mxa != 0) {
	    il[*nh] = il[inh];
	    il[inh] = *nh;
	    ih[*nh] = ia[mxa];
	    ia[mxa] = ia[mb];
	    --mb;
	    ++(*nh);
	    if (mb != 0) {
		ilinh = il[inh];
		split(*n, &x[x_offset], mb, &ia[1],
		      ih[inh], ih[ilinh],
		      1,
		      &ia[1], &mbb, &mxa,
		      &ib[nib], &ia[ma], &mxb);
		mb = mbb;
		goto L8;
	    }
/* then the right son */
	    inh = il[inh];
	}

	do {
	    inh = il[inh];
	    ++ma;
	    nib -= ia[ma];
	    if (ma >= *m) goto L12;
	} while(ia[ma] == 0);
	ilinh = il[inh];
/* on the left side of the tree, the right son of a right son */
/* must represent a subset of points which is inside a */
/* triangle with vertices which are also vertices of the */
/* convex polygon and hence the subset may be neglected. */
	split(*n, &x[x_offset], ia[ma], &ib[nib],
	      ih[inh], ih[ilinh],
	      2,
	      &ia[1], &mb, &mxa,
	      &ib[nib], &mbb, &mxb);
	ia[ma] = mbb;
    } while(TRUE);

/*	 now traverse the RIGHT HALF of the tree */
 L12:
    mxb = mxbb;
    ma = *m;
    mb = ia[ma];
    nia = 1;
    ia[ma] = 0;
/* start with the right son */
 L13:
    nia += ia[ma];
    --ma;

    do {
	if (mxb != 0) {
	    il[*nh] = il[inh];
	    il[inh] = *nh;
	    ih[*nh] = ib[mxb];
	    ib[mxb] = ib[mb];
	    --mb;
	    ++(*nh);
	    if (mb != 0) {
		ilinh = il[inh];
		split(*n, &x[x_offset], mb, &ib[nib],
		      ih[inh], ih[ilinh],
		      -1,
		      &ia[nia], &ia[ma], &mxa,
		      &ib[nib], &mbb, &mxb);
		mb = mbb;
		goto L13;
	    }

/* then the left son */
	    inh = il[inh];
	}

	do {
	    inh = il[inh];
	    ++ma;
	    nia -= ia[ma];
	    if (ma == mp1) goto Finis;
	} while(ia[ma] == 0);
	ilinh = il[inh];
/* on the right side of the tree, the left son of a left son */
/* must represent a subset of points which is inside a */
/* triangle with vertices which are also vertices of the */
/* convex polygon and hence the subset may be neglected. */
	split(*n, &x[x_offset], ia[ma], &ia[nia],
	      ih[inh], ih[ilinh],
	      -2,
	      &ia[nia], &mbb, &mxa,
	      &ib[nib], &mb, &mxb);
    } while(TRUE);

/* -------------------------------------------------------------- */

 L_vertical:/* all the points lie on a vertical line */

    kx = in[1];
    kn = in[1];
    for (i = 1; i <= *m; ++i) {
	j = in[i];
	if (y(j) > y(kx)) {
	    mx = i;
	    kx = j;
	}
	if (y(j) < y(kn)) {
	    min = i;
	    kn = j;
	}
    }
    if (kx == kn) goto L_1pt;

 L_2pts:/* only two points */
    ih[1] = kx;
    ih[2] = kn;
    if (x[kn] == x[kx] && y(kn) == y(kx))
	*nh = 2;
    else
	*nh = 3;
    goto Finis;

 L_1pt:/* only one point */
    *nh = 2;
    ih[1] = in[1];
    il[1] = 1;

 Finis:
    --(*nh);
    /* put the results in order, as given by IH */
    for (i = 1; i <= *nh; ++i) {
	ia[i] = ih[i];
    }
    j = il[1];
    for (i = 2; i <= *nh; ++i) {
	ih[i] = ia[j];
	j = il[j];
    }
    return;

#undef y
} /* chull */

#include <Rinternals.h>
SEXP chull(SEXP x)
{
    // x is a two-column matrix
    int n = nrows(x), nh;
    int *in = (int*)R_alloc(n, sizeof(int));
    for (int i = 0; i < n; i++) in[i] = i+1;
    int *ih = (int*)R_alloc(4*n, sizeof(int));
    x = PROTECT(coerceVector(x, REALSXP));
    if(TYPEOF(x) != REALSXP) error("'x' is not numeric");
    in_chull(&n, REAL(x), &n, in, ih+n, ih+2*n, ih, &nh, ih+3*n);
    SEXP ans = allocVector(INTSXP, nh);
    int *ians = INTEGER(ans);
    for (int i = 0; i < nh; i++) ians[i] = ih[nh - 1 -i];
    UNPROTECT(1);
    return ans;
}
