/*
 *	chull finds the convex hull of a set of points in the plane.
 *
 *      It is based on a C translation (by f2c) of
 *      ACM TOMS algorithm 523 by W. F. Eddy, vol 3 (1977), 398-403, 411-2.
 *
 *      converted to double precision, output order altered
 *      by B.D. Ripley, March 1999
 *
 */


typedef int logical;
#define TRUE_ (1)
#define FALSE_ (0)


static void split(int n, double *x, int m, int *in, int ii, 
		  int jj, int s, int *iabv, int *na, int *maxa,
		  int *ibel, int *nb, int *maxb)
{
    /* System generated locals */
    int x_dim1, x_offset;

    /* Local variables */
    double a, b, dir, down, d1, up, xt, z;
    int i, is;
    logical t;

/* THIS SUBROUTINE TAKES THE M POINTS OF ARRAY X WHOSE */
/* SUBSCRIPTS ARE IN ARRAY IN AND PARTITIONS THEM BY THE */
/* LINE JOINING THE TWO POINTS IN ARRAY X WHOSE SUBSCRIPTS */
/* ARE II AND JJ. THE SUBSCRIPTS OF THE POINTS ABOVE THE */
/* LINE ARE PUT INTO ARRAY IABV, AND THE SUBSCRIPTS OF THE */
/* POINTS BELOW ARE PUT INTO ARRAY IBEL. NA AND NB ARE, */
/* RESPECTIVELY, THE NUMBER OF POINTS ABOVE THE LINE AND THE */
/* NUMBER BELOW. MAXA AND MAXB ARE THE SUBSCRIPTS FOR ARRAY */
/* X OF THE POINT FURTHEST ABOVE THE LINE AND THE POINT */
/* FURTHEST BELOW, RESPECTIVELY. IF EITHER SUBSET IS NULL */
/* THE CORRESPONDING SUBSCRIPT (MAXA OR MAXB) IS SET TO ZERO */
/* FORMAL PARAMETERS */
/* INPUT */
/* N    INTEGER           TOTAL NUMBER OF DATA POINTS */
/* X    REAL ARRAY (2,N)  (X,Y) CO-ORDINATES OF THE DATA */
/* M    INTEGER           NUMBER OF POINTS IN INPUT SUBSET */
/* IN   INTEGER ARRAY (M) SUBSCRIPTS FOR ARRAY X OF THE */
/*                        POINTS IN THE INPUT SUBSET */
/* II   INTEGER           SUBSCRIPT FOR ARRAY X OF ONE POINT */
/*                        ON THE PARTITIONING LINE */
/* JJ   INTEGER           SUBSCRIPT FOR ARRAY X OF ANOTHER */
/*                        POINT ON THE PARTITIONING LINE */
/* S    INTEGER           SWITCH TO DETERMINE OUTPUT. REFER */
/*                        TO COMMENTS BELOW */
/* OUTPUT */
/* IABV INTEGER ARRAY (M) SUBSCRIPTS FOR ARRAY X OF THE */
/*                        POINTS ABOVE THE PARTITIONING LINE */
/* NA   INTEGER           NUMBER OF ELEMENTS IN IABV */
/* MAXA INTEGER           SUBSCRIPT FOR ARRAY X OF POINT */
/*                        FURTHEST ABOVE THE LINE. SET TO */
/*                        ZERO IF NA IS ZERO */
/* IBEL INTEGER ARRAY (M) SUBSCRIPTS FOR ARRAY X OF THE */
/*                        POINTS BELOW THE PARTITIONING LINE */
/* NB   INTEGER           NUMBER OF ELEMENTS IN IBEL */
/* MAXB INTEGER           SUBSCRIPT FOR ARRAY X OF POINT */
/*                        FURTHEST BELOW THE LINE. SET TO */
/*                        ZERO IF NB IS ZERO */
/* IF S = 2 DONT SAVE IBEL,NB,MAXB. */
/* IF S =-2 DONT SAVE IABV,NA,MAXA. */
/* OTHERWISE SAVE EVERYTHING */
/* IF S IS POSITIVE THE ARRAY BEING PARTITIONED IS ABOVE */
/* THE INITIAL PARTITIONING LINE. IF IT IS NEGATIVE, THEN */
/* THE SET OF POINTS IS BELOW. */
    /* Parameter adjustments */
    x_dim1 = n;
    x_offset = 1;
    x -= x_offset;
    --ibel;
    --iabv;
    --in;

    /* Function Body */
    t = FALSE_;
/* CHECK TO SEE IF THE LINE IS VERTICAL */
    if (x[jj] == x[ii]) {
	xt = x[ii];
	d1 = x[jj + x_dim1] - x[ii + x_dim1];
	dir = 1.;
	if ((s > 0 && d1 < 0.) || (s < 0 && d1 > 0.)) dir = -1.;
	t = TRUE_;
    } else {
	a = (x[jj + x_dim1] - x[ii + x_dim1]) / (x[jj] - x[ii]);
	b = x[ii + x_dim1] - a * x[ii];
    }
    up = 0.; *na = 0; *maxa = 0; down = 0.; *nb = 0; *maxb = 0;
    for (i = 1; i <= m; ++i) {
	is = in[i];
	if (t) {
	    z = dir * (x[is] - xt);
	} else {
	    z = x[is + x_dim1] - a * x[is] - b;
	}
	if (z > 0.) {
/* THE POINT IS ABOVE THE LINE */
	    if (s == -2) continue;
	    ++(*na);
	    iabv[*na] = is;
	    if (z >= up) {  
		up = z;
		*maxa = *na;
	    }
        }
	else if (s != 2 && z < 0.) {
/* THE POINT IS BELOW THE LINE */
	    ++(*nb);
	    ibel[*nb] = is;
	    if (z <= down) {
		down = z;
		*maxb = *nb;
	    }
	}
    }
}

void chull(int *n, double *x, int *m, int *in, int *ia, int *ib, 
	   int *ih, int *nh, int *il)
{
    /* Local variables */
    logical mine, maxe;
    int i, j, ilinh, ma, mb, kn, mm, kx, mx, mp1, mbb, nia, nib, 
	inh, min, mxa, mxb, mxbb;
    int x_dim1, x_offset;
    double d1;

/* THIS SUBROUTINE DETERMINES WHICH OF THE M POINTS OF ARRAY */
/* X WHOSE SUBSCRIPTS ARE IN ARRAY IN ARE VERTICES OF THE */
/* MINIMUM AREA CONVEX POLYGON CONTAINING THE M POINTS. THE */
/* SUBSCRIPTS OF THE VERTICES ARE PLACED IN ARRAY IH IN THE */
/* ORDER THEY ARE FOUND. NH IS THE NUMBER OF ELEMENTS IN */
/* ARRAY IH AND ARRAY IL. ARRAY IL IS A LINKED LIST GIVING */
/* THE ORDER OF THE ELEMENTS OF ARRAY IH IN A COUNTER */
/* CLOCKWISE DIRECTION. THIS ALGORITHM CORRESPONDS TO A */
/* PREORDER TRAVERSAL OF A CERTAIN BINARY TREE. EACH VERTEX */
/* OF THE BINARY TREE REPRESENTS A SUBSET OF THE M POINTS. */
/* AT EACH STEP THE SUBSET OF POINTS CORRESPONDING TO THE */
/* CURRENT VERTEX OF THE TREE IS PARTITIONED BY A LINE */
/* JOINING TWO VERTICES OF THE CONVEX POLYGON. THE LEFT SON */
/* VERTEX IN THE BINARY TREE REPRESENTS THE SUBSET OF POINTS */
/* ABOVE THE PARTITIONING LINE AND THE RIGHT SON VERTEX, THE */
/* SUBSET BELOW THE LINE. THE LEAVES OF THE TREE REPRESENT */
/* EITHER NULL SUBSETS OR SUBSETS INSIDE A TRIANGLE WHOSE */
/* VERTICES COINCIDE WITH VERTICES OF THE CONVEX POLYGON. */
/* FORMAL PARAMETERS */
/* INPUT */
/* N  INTEGER           TOTAL NUMBER OF DATA POINTS */
/* X  REAL ARRAY (2,N)  (X,Y) CO-ORDINATES OF THE DATA */
/* M  INTEGER           NUMBER OF POINTS IN THE INPUT SUBSET */
/* IN INTEGER ARRAY (M) SUBSCRIPTS FOR ARRAY X OF THE POINTS */
/*                      IN THE INPUT SUBSET */
/* WORK AREA */
/* IA INTEGER ARRAY (M) SUBSCRIPTS FOR ARRAY X OF LEFT SON */
/*                      SUBSETS. SEE COMMENTS AFTER DIMENSION */
/*                      STATEMENTS */
/* IB INTEGER ARRAY (M) SUBSCRIPTS FOR ARRAY X OF RIGHT SON */
/*                      SUBSETS */
/* OUTPUT */
/* IH INTEGER ARRAY (M) SUBSCRIPTS FOR ARRAY X OF THE */
/*                      VERTICES OF THE CONVEX HULL */
/* NH INTEGER           NUMBER OF ELEMENTS IN ARRAY IH AND */
/*                      ARRAY IL. SAME AS NUMBER OF VERTICES */
/*                      OF THE CONVEX POLYGON */
/* il is used internally here. */
/* IL INTEGER ARRAY (M) A LINKED LIST GIVING IN ORDER IN A */
/*                      COUNTER-CLOCKWISE DIRECTION THE */
/*                      ELEMENTS OF ARRAY IH */
/* THE UPPER END OF ARRAY IA IS USED TO STORE TEMPORARILY */
/* THE SIZES OF THE SUBSETS WHICH CORRESPOND TO RIGHT SON */
/* VERTICES, WHILE TRAVERSING DOWN THE LEFT SONS WHEN ON THE */
/* LEFT HALF OF THE TREE, AND TO STORE THE SIZES OF THE LEFT */
/* SONS WHILE TRAVERSING THE RIGHT SONS(DOWN THE RIGHT HALF) */
    /* Parameter adjustments */
    x_dim1 = *n;
    x_offset = 1;
    x -= x_offset;
    --il;
    --ih;
    --ib;
    --ia;
    --in;

    /* Function Body */
    if (*m == 1) {
	goto L22;
    }
    il[1] = 2;
    il[2] = 1;
    kn = in[1];
    kx = in[2];
    if (*m == 2) {
	goto L21;
    }
    mp1 = *m + 1;
    min = 1;
    mx = 1;
    kx = in[1];
    maxe = FALSE_;
    mine = FALSE_;
/* FIND TWO VERTICES OF THE CONVEX HULL FOR THE INITIAL PARTITION */
    for (i = 2; i <= *m; ++i) {
	j = in[i];
	if ((d1 = x[j] - x[kx]) < 0.) {
	} else if (d1 == 0) {
	    maxe = TRUE_;
	} else {
	    maxe = FALSE_;
	    mx = i;
	    kx = j;
	}
	if ((d1 = x[j] - x[kn]) < 0.) {
	    mine = FALSE_;
	    min = i;
	    kn = j;
	} else if (d1 == 0) {
	    mine = TRUE_;
	}
    }
/* IF THE MAX AND MIN ARE EQUAL, ALL M POINTS LIE ON A */
/* VERTICAL LINE */
    if (kx == kn) {
	goto L18;
    }
/* IF MAXE (OR MINE) HAS THE VALUE TRUE THERE ARE SEVERAL */
/* MAXIMA (OR MINIMA) WITH EQUAL FIRST COORDINATES */
    if (maxe || mine) {
	goto L23;
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
/* BEGIN BY PARTITIONING THE ROOT OF THE TREE */
    split(*n, &x[x_offset], mm, &in[1], ih[1], ih[2], 0, &ia[1], &mb,
	  &mxa, &ib[1], &ia[ma], &mxbb);
/* FIRST TRAVERSE THE LEFT HALF OF THE TREE */
/* START WITH THE LEFT SON */
 L8:
    nib += ia[ma];
    --ma;
 L9:
    if (mxa == 0) goto L11;
    il[*nh] = il[inh];
    il[inh] = *nh;
    ih[*nh] = ia[mxa];
    ia[mxa] = ia[mb];
    --mb;
    ++(*nh);
    if (mb == 0) goto L10;
    ilinh = il[inh];
    split(*n, &x[x_offset], mb, &ia[1], ih[inh], ih[ilinh], 1, &ia[1], 
	  &mbb, &mxa, &ib[nib], &ia[ma], &mxb);
    mb = mbb;
    goto L8;
/* THEN THE RIGHT SON */
 L10:
    inh = il[inh];
 L11:
    inh = il[inh];
    ++ma;
    nib -= ia[ma];
    if (ma >= *m) goto L12;
    if (ia[ma] == 0) goto L11;
    ilinh = il[inh];
/* ON THE LEFT SIDE OF THE TREE, THE RIGHT SON OF A RIGHT SON */
/* MUST REPRESENT A SUBSET OF POINTS WHICH IS INSIDE A */
/* TRIANGLE WITH VERTICES WHICH ARE ALSO VERTICES OF THE */
/* CONVEX POLYGON AND HENCE THE SUBSET MAY BE NEGLECTED. */
    split(*n, &x[x_offset], ia[ma], &ib[nib], ih[inh], ih[ilinh], 2, 
	  &ia[1], &mb, &mxa, &ib[nib], &mbb, &mxb);
    ia[ma] = mbb;
    goto L9;
/* NOW TRAVERSE THE RIGHT HALF OF THE TREE */
 L12:
    mxb = mxbb;
    ma = *m;
    mb = ia[ma];
    nia = 1;
    ia[ma] = 0;
/* START WITH THE RIGHT SON */
 L13:
    nia += ia[ma];
    --ma;
 L14:
    if (mxb == 0) goto L16;
    il[*nh] = il[inh];
    il[inh] = *nh;
    ih[*nh] = ib[mxb];
    ib[mxb] = ib[mb];
    --mb;
    ++(*nh);
    if (mb == 0) goto L15;
    ilinh = il[inh];
    split(*n, &x[x_offset], mb, &ib[nib], ih[inh], ih[ilinh], -1, 
	  &ia[nia], &ia[ma], &mxa, &ib[nib], &mbb, &mxb);
    mb = mbb;
    goto L13;
/* THEN THE LEFT SON */
 L15:
    inh = il[inh];
 L16:
    inh = il[inh];
    ++ma;
    nia -= ia[ma];
    if (ma == mp1) goto L17;
    if (ia[ma] == 0) goto L16;
    ilinh = il[inh];
/* ON THE RIGHT SIDE OF THE TREE, THE LEFT SON OF A LEFT SON */
/* MUST REPRESENT A SUBSET OF POINTS WHICH IS INSIDE A */
/* TRIANGLE WITH VERTICES WHICH ARE ALSO VERTICES OF THE */
/* CONVEX POLYGON AND HENCE THE SUBSET MAY BE NEGLECTED. */
    split(*n, &x[x_offset], ia[ma], &ia[nia], ih[inh], ih[ilinh], -2, 
	  &ia[nia], &mbb, &mxa, &ib[nib], &mb, &mxb);
    goto L14;
 L17:
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
/* ALL THE SPECIAL CASES ARE HANDLED DOWN HERE */
/* IF ALL THE POINTS LIE ON A VERTICAL LINE */
 L18:
    kx = in[1];
    kn = in[1];
    for (i = 1; i <= *m; ++i) {
	j = in[i];
	if (x[j + x_dim1] <= x[kx + x_dim1]) {
	    goto L19;
	}
	mx = i;
	kx = j;
    L19:
	if (x[j + x_dim1] >= x[kn + x_dim1]) {
	    goto L20;
	}
	min = i;
	kn = j;
    L20:
	;
    }
    if (kx == kn) {
	goto L22;
    }
/* IF THERE ARE ONLY TWO POINTS */
 L21:
    ih[1] = kx;
    ih[2] = kn;
    *nh = 3;
    if (x[kn] == x[kx] && x[kn + x_dim1] == x[kx + x_dim1]) {
	*nh = 2;
    }
    goto L17;
/* IF THERE IS ONLY ONE POINT */
 L22:
    *nh = 2;
    ih[1] = in[1];
    il[1] = 1;
    goto L17;
/* MULTIPLE EXTREMES ARE HANDLED HERE */
/* IF THERE ARE SEVERAL POINTS WITH THE (SAME) LARGEST */
/* FIRST COORDINATE */
 L23:
    if (maxe) {
	for (i = 1; i <= *m; ++i) {
	    j = in[i];
	    if (x[j] != x[kx]) continue;
	    if (x[j + x_dim1] <= x[kx + x_dim1]) continue;
	    mx = i;
	    kx = j;
	}
    }
    
/* IF THERE ARE SEVERAL POINTS WITH THE (SAME) SMALLEST */
/* FIRST COORDINATE */
    if (mine) {
	for (i = 1; i <= *m; ++i) {
	    j = in[i];
	    if (x[j] != x[kn]) continue;
	    if (x[j + x_dim1] >= x[kn + x_dim1]) continue;
	    min = i;
	    kn = j;
	}
    }
} /* chull */

