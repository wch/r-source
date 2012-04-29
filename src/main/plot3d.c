/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2012  The R Core Team
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
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <float.h>  /* for DBL_MAX */
#include <Rmath.h>
#include <Graphics.h>
#include <Colors.h> /* for isNAcol */
#include <Print.h>
#include <R_ext/Boolean.h>

/* filled contours and perspective plots were originally here,
   now in graphics/src/plot3d.c .
 */

/* Stuff for labels on contour plots
   Originally written by Nicholas Hildreth
   Adapted by Paul Murrell
*/

static
void FindCorners(double width, double height, SEXP label,
		 double x0, double y0, double x1, double y1,
		 pGEDevDesc dd) {
    double delta = height / width;
    double dx = GConvertXUnits(x1 - x0, USER, INCHES, dd) * delta;
    double dy = GConvertYUnits(y1 - y0, USER, INCHES, dd) * delta;
    dx = GConvertYUnits(dx, INCHES, USER, dd);
    dy = GConvertXUnits(dy, INCHES, USER, dd);

    REAL(label)[0] = x0 + dy;
    REAL(label)[4] = y0 - dx;
    REAL(label)[1] = x0 - dy;
    REAL(label)[5] = y0 + dx;
    REAL(label)[3] = x1 + dy;
    REAL(label)[7] = y1 - dx;
    REAL(label)[2] = x1 - dy;
    REAL(label)[6] = y1 + dx;
}

static
int TestLabelIntersection(SEXP label1, SEXP label2) {

    int i, j, l1, l2;
    double Ax, Bx, Ay, By, ax, ay, bx, by;
    double dom;
    double result1, result2;

    for (i = 0; i < 4; i++) {
	Ax = REAL(label1)[i];
	Ay = REAL(label1)[i+4];
	Bx = REAL(label1)[(i+1)%4];
	By = REAL(label1)[(i+1)%4+4];
	for (j = 0; j < 4; j++) {
	    ax = REAL(label2)[j];
	    ay = REAL(label2)[j+4];
	    bx = REAL(label2)[(j+1)%4];
	    by = REAL(label2)[(j+1)%4+4];

	    dom = Bx*by - Bx*ay - Ax*by + Ax*ay - bx*By + bx*Ay + ax*By - ax*Ay;
	    if (dom == 0.0) {
		result1 = -1;
		result2 = -1;
	    }
	    else {
		result1 = (bx*Ay - ax*Ay - ay*bx - Ax*by + Ax*ay + by*ax) / dom;

		if (bx - ax == 0.0) {
		    if (by - ay == 0.0)
			result2 = -1;
		    else
			result2 = (Ay + (By - Ay) * result1 - ay) / (by - ay);
		}
		else
		    result2 = (Ax + (Bx - Ax) * result1 - ax) / (bx - ax);

	    }
	    l1 = (result1 >= 0.0) && (result1 <= 1.0);
	    l2 = (result2 >= 0.0) && (result2 <= 1.0);
	    if (l1 && l2) return 1;
	}
    }

    return 0;
}

/*** Checks whether a label window is inside view region ***/
static int LabelInsideWindow(SEXP label, pGEDevDesc dd) {
    int i = 0;
    double x, y;

    while (i < 4) {
	x = REAL(label)[i];
	y = REAL(label)[i+4];
	GConvert(&x, &y, USER, NDC, dd);
	/*	x = GConvertXUnits(REAL(label)[i], USER, NDC, dd);
		y = GConvertYUnits(REAL(label)[i+4], USER, NDC, dd); */

	if ((x < 0) || (x > 1) ||
	    (y < 0) || (y > 1))
	    return 1;
	i += 1;
    }
    return 0;
}


	/*  C o n t o u r   P l o t t i n g  */

typedef struct SEG {
    struct SEG *next;
    double x0;
    double y0;
    double x1;
    double y1;
} SEG, *SEGP;

static SEGP *ctr_SegDB;

static int ctr_intersect(double z0, double z1, double zc, double *f)
{
    if ((z0 - zc) * (z1 - zc) < 0.0) {
	*f = (zc - z0) / (z1 -	z0);
	return 1;
    }
    return 0;
}

static SEGP ctr_newseg(double x0, double y0, double x1, double y1, SEGP prev)
{
    SEGP seg = (SEGP)R_alloc(1, sizeof(SEG));
    seg->x0 = x0;
    seg->y0 = y0;
    seg->x1 = x1;
    seg->y1 = y1;
    seg->next = prev;
    return seg;
}

static void ctr_swapseg(SEGP seg)
{
    double x, y;
    x = seg->x0;
    y = seg->y0;
    seg->x0 = seg->x1;
    seg->y0 = seg->y1;
    seg->x1 = x;
    seg->y1 = y;
}

	/* ctr_segdir(): Determine the entry direction to the next cell */
	/* and update the cell indices */

#define XMATCH(x0,x1) (fabs(x0-x1) == 0)
#define YMATCH(y0,y1) (fabs(y0-y1) == 0)

static int ctr_segdir(double xend, double yend, double *x, double *y,
		      int *i, int *j, int nx, int ny)
{
    if (YMATCH(yend, y[*j])) {
	if (*j == 0)
	    return 0;
	*j = *j - 1;
	return 3;
    }
    if (XMATCH(xend, x[*i])) {
	if (*i == 0)
	    return 0;
	*i = *i - 1;
	return 4;
    }
    if (YMATCH(yend, y[*j + 1])) {
	if (*j >= ny - 1)
	    return 0;
	*j = *j + 1;
	return 1;
    }
    if (XMATCH(xend, x[*i + 1])) {
	if (*i >= nx - 1)
	    return 0;
	*i = *i + 1;
	return 2;
    }
    return 0;
}

/* Search seglist for a segment with endpoint (xend, yend). */
/* The cell entry direction is dir, and if tail=1/0 we are */
/* building the tail/head of a contour.	 The matching segment */
/* is pointed to by seg and the updated segment list (with */
/* the matched segment stripped) is returned by the funtion. */

static SEGP ctr_segupdate(double xend, double yend, int dir, Rboolean tail,
			  SEGP seglist, SEGP* seg)
{
    if (seglist == NULL) {
	*seg = NULL;
	return NULL;
    }
    switch (dir) {
    case 1:
    case 3:
	if (YMATCH(yend,seglist->y0)) {
	    if (!tail)
		ctr_swapseg(seglist);
	    *seg = seglist;
	    return seglist->next;
	}
	if (YMATCH(yend,seglist->y1)) {
	    if (tail)
		ctr_swapseg(seglist);
	    *seg = seglist;
	    return seglist->next;
	}
	break;
    case 2:
    case 4:
	if (XMATCH(xend,seglist->x0)) {
	    if (!tail)
		ctr_swapseg(seglist);
	    *seg = seglist;
	    return seglist->next;
	}
	if (XMATCH(xend,seglist->x1)) {
	    if (tail)
		ctr_swapseg(seglist);
	    *seg = seglist;
	    return seglist->next;
	}
	break;
    }
    seglist->next = ctr_segupdate(xend, yend, dir, tail, seglist->next, seg);
    return seglist;
}

/* labelList, label1, and label2 are all SEXPs rather than being allocated
   using R_alloc because they need to persist across calls to contour().
   In do_contour() there is a vmaxget() ... vmaxset() around each call to
   contour() to release all of the memory used in the drawing of the
   contour _lines_ at each contour level.  We need to keep track of the
   contour _labels_ for _all_ contour levels, hence we have to use a
   different memory allocation mechanism.
*/
static SEXP labelList;

static
double distFromEdge(double *xxx, double *yyy, int iii, pGEDevDesc dd) {
    return fmin2(fmin2(xxx[iii]-gpptr(dd)->usr[0], gpptr(dd)->usr[1]-xxx[iii]),
		 fmin2(yyy[iii]-gpptr(dd)->usr[2], gpptr(dd)->usr[3]-yyy[iii]));
}

static
Rboolean useStart(double *xxx, double *yyy, int ns, pGEDevDesc dd) {
    if (distFromEdge(xxx, yyy, 0, dd) < distFromEdge(xxx, yyy, ns-1, dd))
	return TRUE;
    else
	return FALSE;
}

static
int findGapUp(double *xxx, double *yyy, int ns, double labelDistance,
	      pGEDevDesc dd) {
    double dX, dY;
    double dXC, dYC;
    double distanceSum = 0;
    int n = 0;
    int jjj = 1;
    while ((jjj < ns) && (distanceSum < labelDistance)) {
	/* Find a gap big enough for the label
	   use several segments if necessary
	*/
	dX = xxx[jjj] - xxx[jjj - n - 1]; /* jjj - n - 1 == 0 */
	dY = yyy[jjj] - yyy[jjj - n - 1];
	dXC = GConvertXUnits(dX, USER, INCHES, dd);
	dYC = GConvertYUnits(dY, USER, INCHES, dd);
	distanceSum = hypot(dXC, dYC);
	jjj++;
	n++;
    }
    if (distanceSum < labelDistance)
	return 0;
    else
	return n;
}

static
int findGapDown(double *xxx, double *yyy, int ns, double labelDistance,
		pGEDevDesc dd) {
    double dX, dY;
    double dXC, dYC;
    double distanceSum = 0;
    int n = 0;
    int jjj = ns - 2;
    while ((jjj > -1) && (distanceSum < labelDistance)) {
	/* Find a gap big enough for the label
	   use several segments if necessary
	*/
	dX = xxx[jjj] - xxx[jjj + n + 1]; /*jjj + n + 1 == ns -1 */
	dY = yyy[jjj] - yyy[jjj + n + 1];
	dXC = GConvertXUnits(dX, USER, INCHES, dd);
	dYC = GConvertYUnits(dY, USER, INCHES, dd);
	distanceSum = hypot(dXC, dYC);
	jjj--;
	n++;
    }
    if (distanceSum < labelDistance)
	return 0;
    else
	return n;
}

/*
 * Generate a list of segments for a single level
 */
static SEGP* contourLines(double *x, int nx, double *y, int ny,
			 double *z, double zc, double atom)
{
    double f, xl, xh, yl, yh, zll, zhl, zlh, zhh, xx[4], yy[4];
    int i, j, k, l, m, nacode;
    SEGP seglist;
    SEGP *segmentDB;
    /* Initialize the segment data base */
    /* Note we must be careful about resetting */
    /* the top of the stack, otherwise we run out of */
    /* memory after a sequence of displaylist replays */
    /*
     * This reset is done out in GEcontourLines
     */
    segmentDB = (SEGP*)R_alloc(nx*ny, sizeof(SEGP));
    for (i = 0; i < nx; i++)
	for (j = 0; j < ny; j++)
	    segmentDB[i + j * nx] = NULL;
    for (i = 0; i < nx - 1; i++) {
	xl = x[i];
	xh = x[i + 1];
	for (j = 0; j < ny - 1; j++) {
	    yl = y[j];
	    yh = y[j + 1];
	    k = i + j * nx;
	    zll = z[k];
	    zhl = z[k + 1];
	    zlh = z[k + nx];
	    zhh = z[k + nx + 1];

	    /* If the value at a corner is exactly equal to a contour level,
	     * change that value by a tiny amount */

	    if (zll == zc) zll += atom;
	    if (zhl == zc) zhl += atom;
	    if (zlh == zc) zlh += atom;
	    if (zhh == zc) zhh += atom;
#ifdef DEBUG_contour
	    /* Haven't seen this happening (MM): */
	    if (zll == zc) REprintf(" [%d,%d] ll: %g\n",i,j, zll);
	    if (zhl == zc) REprintf(" [%d,%d] hl: %g\n",i,j, zhl);
	    if (zlh == zc) REprintf(" [%d,%d] lh: %g\n",i,j, zlh);
	    if (zhh == zc) REprintf(" [%d,%d] hh: %g\n",i,j, zhh);
#endif
	    /* Check for intersections with sides */

	    nacode = 0;
	    if (R_FINITE(zll)) nacode += 1;
	    if (R_FINITE(zhl)) nacode += 2;
	    if (R_FINITE(zlh)) nacode += 4;
	    if (R_FINITE(zhh)) nacode += 8;

	    k = 0;
	    switch (nacode) {
	    case 15:
		if (ctr_intersect(zll, zhl, zc, &f)) {
		    xx[k] = xl + f * (xh - xl);
		    yy[k] = yl; k++;
		}
		if (ctr_intersect(zll, zlh, zc, &f)) {
		    yy[k] = yl + f * (yh - yl);
		    xx[k] = xl; k++;
		}
		if (ctr_intersect(zhl, zhh, zc, &f)) {
		    yy[k] = yl + f * (yh - yl);
		    xx[k] = xh; k++;
		}
		if (ctr_intersect(zlh, zhh, zc, &f)) {
		    xx[k] = xl + f * (xh - xl);
		    yy[k] = yh; k++;
		}
		break;
	    case 14:
		if (ctr_intersect(zhl, zhh, zc, &f)) {
		    yy[k] = yl + f * (yh - yl);
		    xx[k] = xh; k++;
		}
		if (ctr_intersect(zlh, zhh, zc, &f)) {
		    xx[k] = xl + f * (xh - xl);
		    yy[k] = yh; k++;
		}
		if (ctr_intersect(zlh, zhl, zc, &f)) {
		    xx[k] = xl + f * (xh - xl);
		    yy[k] = yh + f * (yl - yh);
		    k++;
		}
		break;
	    case 13:
		if (ctr_intersect(zll, zlh, zc, &f)) {
		    yy[k] = yl + f * (yh - yl);
		    xx[k] = xl; k++;
		}
		if (ctr_intersect(zlh, zhh, zc, &f)) {
		    xx[k] = xl + f * (xh - xl);
		    yy[k] = yh; k++;
		}
		if (ctr_intersect(zll, zhh, zc, &f)) {
		    xx[k] = xl + f * (xh - xl);
		    yy[k] = yl + f * (yh - yl);
		    k++;
		}
		break;
	    case 11:
		if (ctr_intersect(zhl, zhh, zc, &f)) {
		    yy[k] = yl + f * (yh - yl);
		    xx[k] = xh; k++;
		}
		if (ctr_intersect(zll, zhl, zc, &f)) {
		    xx[k] = xl + f * (xh - xl);
		    yy[k] = yl; k++;
		}
		if (ctr_intersect(zll, zhh, zc, &f)) {
		    xx[k] = xl + f * (xh - xl);
		    yy[k] = yl + f * (yh - yl);
		    k++;
		}
		break;
	    case 7:
		if (ctr_intersect(zll, zlh, zc, &f)) {
		    yy[k] = yl + f * (yh - yl);
		    xx[k] = xl; k++;
		}
		if (ctr_intersect(zll, zhl, zc, &f)) {
		    xx[k] = xl + f * (xh - xl);
		    yy[k] = yl; k++;
		}
		if (ctr_intersect(zlh, zhl, zc, &f)) {
		    xx[k] = xl + f * (xh - xl);
		    yy[k] = yh + f * (yl - yh);
		    k++;
		}
		break;
	    }

	    /* We now have k(=2,4) endpoints */
	    /* Decide which to join */

	    seglist = NULL;

	    if (k > 0) {
		if (k == 2) {
		    seglist = ctr_newseg(xx[0], yy[0], xx[1], yy[1], seglist);
		}
		else if (k == 4) {
		    for (k = 3; k >= 1; k--) {
			m = k;
			xl = xx[k];
			for (l = 0; l < k; l++) {
			    if (xx[l] > xl) {
				xl = xx[l];
				m = l;
			    }
			}
			if (m != k) {
			    xl = xx[k];
			    yl = yy[k];
			    xx[k] = xx[m];
			    yy[k] = yy[m];
			    xx[m] = xl;
			    yy[m] = yl;
			}
		    }
		    seglist = ctr_newseg(xx[0], yy[0], xx[1], yy[1], seglist);
		    seglist = ctr_newseg(xx[2], yy[2], xx[3], yy[3], seglist);
		}
		else error("k != 2 or 4");
	    }
	    segmentDB[i + j * nx] = seglist;
	}
    }
    return segmentDB;
}

#define CONTOUR_LIST_STEP 100
#define CONTOUR_LIST_LEVEL 0
#define CONTOUR_LIST_X 1
#define CONTOUR_LIST_Y 2

static SEXP growList(SEXP oldlist) {
    int i, len;
    SEXP templist;
    len = LENGTH(oldlist);
    templist = PROTECT(allocVector(VECSXP, len + CONTOUR_LIST_STEP));
    for (i=0; i<len; i++)
	SET_VECTOR_ELT(templist, i, VECTOR_ELT(oldlist, i));
    UNPROTECT(1);
    return templist;
}

/*
 * Store the list of segments for a single level in the SEXP
 * list that will be returned to the user
 */
static
int addContourLines(double *x, int nx, double *y, int ny,
		     double *z, double zc, double atom,
		     SEGP* segmentDB, int nlines, SEXP container)
{
    double xend, yend;
    int i, ii, j, jj, ns, dir, nc;
    SEGP seglist, seg, s, start, end;
    SEXP ctr, level, xsxp, ysxp, names;
    /* Begin following contours. */
    /* 1. Grab a segment */
    /* 2. Follow its tail */
    /* 3. Follow its head */
    /* 4. Save the contour */
    for (i = 0; i < nx - 1; i++)
	for (j = 0; j < ny - 1; j++) {
	    while ((seglist = segmentDB[i + j * nx])) {
		ii = i; jj = j;
		start = end = seglist;
		segmentDB[i + j * nx] = seglist->next;
		xend = seglist->x1;
		yend = seglist->y1;
		while ((dir = ctr_segdir(xend, yend, x, y,
					 &ii, &jj, nx, ny))) {
		    segmentDB[ii + jj * nx]
			= ctr_segupdate(xend, yend, dir, TRUE,/* = tail */
					segmentDB[ii + jj * nx], &seg);
		    if (!seg) break;
		    end->next = seg;
		    end = seg;
		    xend = end->x1;
		    yend = end->y1;
		}
		end->next = NULL; /* <<< new for 1.2.3 */
		ii = i; jj = j;
		xend = seglist->x0;
		yend = seglist->y0;
		while ((dir = ctr_segdir(xend, yend, x, y,
					 &ii, &jj, nx, ny))) {
		    segmentDB[ii + jj * nx]
			= ctr_segupdate(xend, yend, dir, FALSE,/* ie. head */
					segmentDB[ii+jj*nx], &seg);
		    if (!seg) break;
		    seg->next = start;
		    start = seg;
		    xend = start->x0;
		    yend = start->y0;
		}

		/* ns := #{segments of polyline} -- need to allocate */
		s = start;
		ns = 0;
		/* max_contour_segments: prevent inf.loop (shouldn't be needed) */
		while (s && ns < max_contour_segments) {
		    ns++;
		    s = s->next;
		}
		if(ns == max_contour_segments)
		    warning(_("contour(): circular/long seglist -- set %s > %d?"), 
		            "options(\"max.contour.segments\")", max_contour_segments);
		/*
		 * "write" the contour locations into the list of contours
		 */
		ctr = PROTECT(allocVector(VECSXP, 3));
		level = PROTECT(allocVector(REALSXP, 1));
		xsxp = PROTECT(allocVector(REALSXP, ns + 1));
		ysxp = PROTECT(allocVector(REALSXP, ns + 1));
		REAL(level)[0] = zc;
		SET_VECTOR_ELT(ctr, CONTOUR_LIST_LEVEL, level);
		s = start;
		REAL(xsxp)[0] = s->x0;
		REAL(ysxp)[0] = s->y0;
		ns = 1;
		while (s->next && ns < max_contour_segments) {
		    s = s->next;
		    REAL(xsxp)[ns] = s->x0;
		    REAL(ysxp)[ns++] = s->y0;
		}
		REAL(xsxp)[ns] = s->x1;
		REAL(ysxp)[ns] = s->y1;
		SET_VECTOR_ELT(ctr, CONTOUR_LIST_X, xsxp);
		SET_VECTOR_ELT(ctr, CONTOUR_LIST_Y, ysxp);
		/*
		 * Set the names attribute for the contour
		 * So that users can extract components using
		 * meaningful names
		 */
		PROTECT(names = allocVector(STRSXP, 3));
		SET_STRING_ELT(names, 0, mkChar("level"));
		SET_STRING_ELT(names, 1, mkChar("x"));
		SET_STRING_ELT(names, 2, mkChar("y"));
		setAttrib(ctr, R_NamesSymbol, names);
		/*
		 * We're about to add another line to the list ...
		 */
		nlines += 1;
		nc = LENGTH(VECTOR_ELT(container, 0));
		if (nlines == nc)
		    /* Where does this get UNPROTECTed? */
		    SET_VECTOR_ELT(container, 0,
				   growList(VECTOR_ELT(container, 0)));
		SET_VECTOR_ELT(VECTOR_ELT(container, 0), nlines - 1, ctr);
		UNPROTECT(5);
	    }
	}
    return nlines;
}

/*
 * Given nx x values, ny y values, nx*ny z values,
 * and nl cut-values in z ...
 * ... produce a list of contour lines:
 *   list of sub-lists
 *     sub-list = x vector, y vector, and cut-value.
 */
SEXP GEcontourLines(double *x, int nx, double *y, int ny,
		    double *z, double *levels, int nl)
{
    const void *vmax;
    int i, nlines, len;
    double atom, zmin, zmax;
    SEGP* segmentDB;
    SEXP container, mainlist, templist;
    /*
     * "tie-breaker" values
     */
    zmin = DBL_MAX;
    zmax = DBL_MIN;
    for (i = 0; i < nx * ny; i++)
	if (R_FINITE(z[i])) {
	    if (zmax < z[i]) zmax =  z[i];
	    if (zmin > z[i]) zmin =  z[i];
	}

    if (zmin >= zmax) {
	if (zmin == zmax)
	    warning(_("all z values are equal"));
	else
	    warning(_("all z values are NA"));
	return R_NilValue;
    }
    /* change to 1e-3, reconsidered because of PR#897
     * but 1e-7, and even  2*DBL_EPSILON do not prevent inf.loop in contour().
     * maybe something like   16 * DBL_EPSILON * (..).
     * see also max_contour_segments above */
    atom = 1e-3 * (zmax - zmin);
    /*
     * Create a "container" which is a list with only 1 element.
     * The element is the list of lines that will be built up.
     * I create the container because this allows me to PROTECT
     * the container once here and then UNPROTECT it at the end of
     * this function and, as long as I always work with
     * VECTOR_ELT(container, 0) and SET_VECTOR_ELT(container, 0)
     * in functions called from here, I don't need to worry about
     * protectin the list that I am building up.
     * Why bother?  Because the list I am building can potentially
     * grow and it's awkward to get the PROTECTs/UNPROTECTs right
     * when you're in a loop and growing a list.
     */
    container = PROTECT(allocVector(VECSXP, 1));
    /*
     * Create "large" list (will trim excess at the end if necesary)
     */
    SET_VECTOR_ELT(container, 0, allocVector(VECSXP, CONTOUR_LIST_STEP));
    nlines = 0;
    /*
     * Add lines for each contour level
     */
    for (i = 0; i < nl; i++) {
	/*
	 * The vmaxget/set is to manage the memory that gets
	 * R_alloc'ed in the creation of the segmentDB structure
	 */
	vmax = vmaxget();
	/*
	 * Generate a segment database
	 */
	segmentDB = contourLines(x, nx, y, ny, z, levels[i], atom);
	/*
	 * Add lines to the list based on the segment database
	 */
	nlines = addContourLines(x, nx, y, ny, z, levels[i],
				 atom, segmentDB, nlines,
				 container);
	vmaxset(vmax);
    }
    /*
     * Trim the list of lines to the appropriate length.
     */
    len = LENGTH(VECTOR_ELT(container, 0));
    if (nlines < len) {
	mainlist = VECTOR_ELT(container, 0);
	templist = PROTECT(allocVector(VECSXP, nlines));
	for (i=0; i<nlines; i++)
	    SET_VECTOR_ELT(templist, i, VECTOR_ELT(mainlist, i));
	mainlist = templist;
	UNPROTECT(1);  /* UNPROTECT templist */
    } else
	mainlist = VECTOR_ELT(container, 0);
    UNPROTECT(1);  /* UNPROTECT container */
    return mainlist;
}

SEXP attribute_hidden do_contourLines(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP c, x, y, z;
    int nx, ny, nc;

    x = CAR(args);
    internalTypeCheck(call, x, REALSXP);
    nx = LENGTH(x);
    args = CDR(args);

    y = CAR(args);
    internalTypeCheck(call, y, REALSXP);
    ny = LENGTH(y);
    args = CDR(args);

    z = CAR(args);
    internalTypeCheck(call, z, REALSXP);
    args = CDR(args);

    /* levels */
    c = CAR(args);
    internalTypeCheck(call, c, REALSXP);
    nc = LENGTH(c);
    args = CDR(args);

    return GEcontourLines(REAL(x), nx, REAL(y), ny, REAL(z), REAL(c), nc);
}

/*
 * The *base graphics* function contour() and the *general base*
 * function contourLines() use the same code to generate contour lines
 * (i.e., the function contourLines())
 *
 * I had a look at extracting the code that draws the labels
 * into a *general base* function
 * (e.g., into some sort of labelLines() function),
 * but the code is too base-graphics-specific (e.g., one of the
 * labelling methods seeks the location closest to the edge of the
 * plotting region) so I've left it alone for now.
 *
 * This does mean that the contourLines() function is part of the
 * graphics engine, but the contour() function is part of the
 * base graphics system.
 */

static void contour(SEXP x, int nx, SEXP y, int ny, SEXP z,
		    double zc,
		    SEXP labels, int cnum,
		    Rboolean drawLabels, int method,
		    double atom, pGEDevDesc dd)
{
/* draw a contour for one given contour level 'zc' */

    const void *vmax;

    double xend, yend;
    int i, ii, j, jj, ns, dir;
    SEGP seglist, seg, s, start, end;
    double *xxx, *yyy;

    double variance, dX, dY, deltaX, deltaY;
    double dXC, dYC;
    int range=0, indx=0, n; /* -Wall */
    double lowestVariance;
    double squareSum;
    int iii, jjj;
    double distanceSum, labelDistance, avgGradient;
    char buffer[255];
    int result;
    double ux, uy, vx, vy;
    double xStart, yStart;
    double dx, dy, dxy;
    double labelHeight;
    SEXP label1 = PROTECT(allocVector(REALSXP, 8));
    SEXP label2;
    SEXP lab;
    Rboolean gotLabel = FALSE;
    Rboolean ddl;/* Don't draw label -- currently unused, i.e. always FALSE*/

#ifdef DEBUG_contour
    Rprintf("contour(lev = %g):\n", zc);
#endif

    vmax = vmaxget();
    ctr_SegDB = contourLines(REAL(x), nx, REAL(y), ny, REAL(z), zc, atom);
    /* we need to keep ctr_SegDB available, so vmaxset(vmax); was wrong */

    /* The segment database is now assembled. */
    /* Begin following contours. */
    /* 1. Grab a segment */
    /* 2. Follow its tail */
    /* 3. Follow its head */
    /* 4. Draw the contour */

    for (i = 0; i < nx - 1; i++)
      for (j = 0; j < ny - 1; j++) {
	while ((seglist = ctr_SegDB[i + j * nx])) {
	    ii = i; jj = j;
	    start = end = seglist;
	    ctr_SegDB[i + j * nx] = seglist->next;
	    xend = seglist->x1;
	    yend = seglist->y1;
	    while ((dir = ctr_segdir(xend, yend, REAL(x), REAL(y),
				     &ii, &jj, nx, ny))) {
		ctr_SegDB[ii + jj * nx]
		    = ctr_segupdate(xend, yend, dir, TRUE,/* = tail */
				    ctr_SegDB[ii + jj * nx], &seg);
		if (!seg) break;
		end->next = seg;
		end = seg;
		xend = end->x1;
		yend = end->y1;
	    }
	    end->next = NULL; /* <<< new for 1.2.3 */
	    ii = i; jj = j;
	    xend = seglist->x0;
	    yend = seglist->y0;
	    while ((dir = ctr_segdir(xend, yend, REAL(x), REAL(y),
				     &ii, &jj, nx, ny))) {
		ctr_SegDB[ii + jj * nx]
		    = ctr_segupdate(xend, yend, dir, FALSE,/* ie. head */
				    ctr_SegDB[ii+jj*nx], &seg);
		if (!seg) break;
		seg->next = start;
		start = seg;
		xend = start->x0;
		yend = start->y0;
	    }

	    /* ns := #{segments of polyline} -- need to allocate */
	    s = start;
	    ns = 0;
	    /* max_contour_segments: prevent inf.loop (shouldn't be needed) */
	    while (s && ns < max_contour_segments) {
		ns++;
		s = s->next;
	    }
	    if(ns == max_contour_segments)
		warning(_("contour(): circular/long seglist -- set %s > %d?"), 
		        "options(\"max.contour.segments\")", max_contour_segments);

	    /* contour midpoint : use for labelling sometime (not yet!)
	       int ns2;
	       if (ns > 3) ns2 = ns/2; else ns2 = -1;
	    */

	    vmax = vmaxget();
	    xxx = (double *) R_alloc(ns + 1, sizeof(double));
	    yyy = (double *) R_alloc(ns + 1, sizeof(double));
	    /* now have the space, go through again: */
	    s = start;
	    ns = 0;
	    xxx[ns] = s->x0;
	    yyy[ns++] = s->y0;
	    while (s->next && ns < max_contour_segments) {
		s = s->next;
		xxx[ns] = s->x0;
		yyy[ns++] = s->y0;
	    }
	    xxx[ns] = s->x1;
	    yyy[ns++] = s->y1;
#ifdef DEBUG_contour
	    Rprintf("  [%2d,%2d]: (x,y)[1:%d] = ", i,j, ns);
	    if(ns >= 5)
		Rprintf(" (%g,%g), (%g,%g), ..., (%g,%g)\n",
			xxx[0],yyy[0], xxx[1],yyy[1], xxx[ns-1],yyy[ns-1]);
	    else
		for(iii = 0; iii < ns; iii++)
		    Rprintf(" (%g,%g)%s", xxx[iii],yyy[iii],
			    (iii < ns-1) ? "," : "\n");
#endif

//	    GMode(1, dd);

	    if (drawLabels) {
		/* If user supplied labels, use i'th one of them
		   Otherwise stringify the z-value of the contour */
		cetype_t enc = CE_NATIVE;
		buffer[0] = ' ';
		if (!isNull(labels)) {
		    int numl = length(labels);
		    strcpy(&buffer[1], CHAR(STRING_ELT(labels, cnum % numl)));
		    enc = getCharCE(STRING_ELT(labels, cnum % numl));
		}
		else {
		    PROTECT(lab = allocVector(REALSXP, 1));
		    REAL(lab)[0] = zc;
		    lab = labelformat(lab);
		    strcpy(&buffer[1], CHAR(STRING_ELT(lab, 0))); /* ASCII */
		    UNPROTECT(1);
		}
		buffer[strlen(buffer)+1] = '\0';
		buffer[strlen(buffer)] = ' ';

		labelDistance = GStrWidth(buffer, enc, INCHES, dd);
		labelHeight = GStrHeight(buffer, enc, INCHES, dd);

		if (labelDistance > 0) {
		    /* Try to find somewhere to draw the label */
		    switch (method) {
		    case 0: /* draw label at one end of contour
			       overwriting contour line
			    */
			if (useStart(xxx, yyy, ns, dd) )
			    indx = 0;
			else
			    indx = ns - 1;
			break;
		    case 1: /* draw label at one end of contour
			       embedded in contour
			       no overlapping labels
			    */
			indx = 0;
			range = 0;
			gotLabel = FALSE;
			if (useStart(xxx, yyy, ns, dd)) {
			    iii = 0;
			    n = findGapUp(xxx, yyy, ns, labelDistance, dd);
			}
			else {
			    n = findGapDown(xxx, yyy, ns, labelDistance, dd);
			    iii = ns - n - 1;
			}
			if (n > 0) {
			    /** Find 4 corners of label extents **/
			    FindCorners(labelDistance, labelHeight, label1,
					xxx[iii], yyy[iii],
					xxx[iii+n], yyy[iii+n], dd);

			    /** Test corners for intersection with previous labels **/
			    label2 = labelList;
			    result = 0;
			    while ((result == 0) && (label2 != R_NilValue)) {
				result = TestLabelIntersection(label1, CAR(label2));
				label2 = CDR(label2);
			    }
			    if (result == 0) {
				result = LabelInsideWindow(label1, dd);
				if (result == 0) {
				    indx = iii;
				    range = n;
				    gotLabel = TRUE;
				}
			    }
			}
			break;
		    case 2: /* draw label on flattest portion of contour
			       embedded in contour line
			       no overlapping labels
			    */
			/* Look for flatest sequence of contour gradients */
			lowestVariance = 9999999;   /* A large number */
			indx = 0;
			range = 0;
			gotLabel = FALSE;
			for (iii = 0; iii < ns; iii++) {
			    distanceSum = 0;
			    avgGradient = 0;
			    squareSum = 0;
			    n = 0;
			    jjj = (iii + 1);
			    while ((jjj < ns-1) &&
				   (distanceSum < labelDistance)) {

				/* Find a gap big enough for the label
				   use several segments if necessary
				*/
				dX = xxx[jjj] - xxx[jjj - n - 1];
				dY = yyy[jjj] - yyy[jjj - n - 1];
				dXC = GConvertXUnits(dX, USER, INCHES, dd);
				dYC = GConvertYUnits(dY, USER, INCHES, dd);
				distanceSum = hypot(dXC, dYC);

				/* Calculate the variance of the gradients
				   of the segments that will make way for the
				   label
				*/
				deltaX = xxx[jjj] - xxx[jjj - 1];
				deltaY = yyy[jjj] - yyy[jjj - 1];
				if (deltaX == 0) {deltaX = 1;}
				avgGradient += (deltaY/deltaX);
				squareSum += avgGradient * avgGradient;
				jjj = (jjj + 1);
				n += 1;
			    }
			    if (distanceSum < labelDistance)
				break;

			    /** Find 4 corners of label extents **/
			    FindCorners(labelDistance, labelHeight, label1,
					xxx[iii], yyy[iii],
					xxx[iii+n], yyy[iii+n], dd);

			    /** Test corners for intersection with previous labels **/
			    label2 = labelList;
			    result = 0;
			    while ((result == 0) && (label2 != R_NilValue)) {
				result = TestLabelIntersection(label1, CAR(label2));
				label2 = CDR(label2);
			    }
			    if (result == 0)
				result = LabelInsideWindow(label1, dd);
			    if (result == 0) {
				variance = (squareSum - (avgGradient * avgGradient) / n) / n;
				avgGradient /= n;
				if (variance < lowestVariance) {
				    lowestVariance = variance;
				    indx = iii;
				    range = n;
				}
			    }
			    if (lowestVariance < 9999999)
				gotLabel = TRUE;
			}
		    } /* switch (method) */

		    if (method == 0) {
			GPolyline(ns, xxx, yyy, USER, dd);
			GText(xxx[indx], yyy[indx], USER, buffer,
			      CE_NATIVE/*FIX*/,
			      .5, .5, 0, dd);
		    }
		    else {
			if (indx > 0)
		            GPolyline(indx+1, xxx, yyy, USER, dd);
			if (ns-1-indx-range > 0)
			    GPolyline(ns-indx-range, xxx+indx+range, yyy+indx+range,
			          USER, dd);
			if (gotLabel) {
			    /* find which plot edge we are closest to */
			    int closest; /* 0 = indx,  1 = indx+range */
			    double dx1, dx2, dy1, dy2, dmin;
			    dx1 = fmin2((xxx[indx] - gpptr(dd)->usr[0]),
					(gpptr(dd)->usr[1] - xxx[indx]));
			    dx2 = fmin2((gpptr(dd)->usr[1] - xxx[indx+range]),
					(xxx[indx+range] - gpptr(dd)->usr[0]));
			    if (dx1 < dx2) {
				closest = 0;
				dmin = dx1;
			    } else {
				closest = 1;
				dmin = dx2;
			    }
			    dy1 = fmin2((yyy[indx] - gpptr(dd)->usr[2]),
					(gpptr(dd)->usr[3] - yyy[indx]));
			    if (closest && (dy1 < dmin)) {
				closest = 0;
				dmin = dy1;
			    } else if (dy1 < dmin)
				dmin = dy1;
			    dy2 = fmin2((gpptr(dd)->usr[3] - yyy[indx+range]),
					(yyy[indx+range] - gpptr(dd)->usr[2]));
			    if (!closest && (dy2 < dmin))
				closest = 1;

			    dx = GConvertXUnits(xxx[indx+range] - xxx[indx],
						USER, INCHES, dd);
			    dy = GConvertYUnits(yyy[indx+range] - yyy[indx],
						USER, INCHES, dd);
			    dxy = hypot(dx, dy);

			    /* save the current label for checking overlap */
			    label2 = allocVector(REALSXP, 8);

			    FindCorners(labelDistance, labelHeight, label2,
					xxx[indx], yyy[indx],
					xxx[indx+range], yyy[indx+range], dd);
			    UNPROTECT_PTR(labelList);
			    labelList = PROTECT(CONS(label2, labelList));

			    ddl = FALSE;
			    /* draw an extra bit of segment if the label
			       doesn't fill the gap */
			    if (closest) {
				xStart = xxx[indx+range] -
				    (xxx[indx+range] - xxx[indx]) *
				    labelDistance / dxy;
				yStart = yyy[indx+range] -
				    (yyy[indx+range] - yyy[indx]) *
				    labelDistance / dxy;
				if (labelDistance / dxy < 1)
				    GLine(xxx[indx], yyy[indx],
					  xStart, yStart,
					  USER, dd);
			    } else {
				xStart = xxx[indx] +
				    (xxx[indx+range] - xxx[indx]) *
				    labelDistance / dxy;
				yStart = yyy[indx] +
				    (yyy[indx+range] - yyy[indx]) *
				    labelDistance / dxy;
				if (labelDistance / dxy < 1)
				    GLine(xStart, yStart,
					  xxx[indx+range], yyy[indx+range],
					  USER, dd);
			    }

			    /*** Draw contour labels ***/
			    if (xxx[indx] < xxx[indx+range]) {
				if (closest) {
				    ux = xStart;
				    uy = yStart;
				    vx = xxx[indx+range];
				    vy = yyy[indx+range];
				} else {
				    ux = xxx[indx];
				    uy = yyy[indx];
				    vx = xStart;
				    vy = yStart;
				}
			    }
			    else {
				if (closest) {
				    ux = xxx[indx+range];
				    uy = yyy[indx+range];
				    vx = xStart;
				    vy = yStart;
				} else {
				    ux = xStart;
				    uy = yStart;
				    vx = xxx[indx];
				    vy = yyy[indx];
				}
			    }

			    if (!ddl) {
				/* convert to INCHES for calculation of
				   angle to draw text
				*/
				GConvert(&ux, &uy, USER, INCHES, dd);
				GConvert(&vx, &vy, USER, INCHES, dd);
				/* 0, .5 => left, centre justified */
				GText (ux, uy, INCHES, buffer,
				       CE_NATIVE/*FIX*/,0, .5,
				       (180 / 3.14) * atan2(vy - uy, vx - ux),
				       dd);
			    }
			} /* if (gotLabel) */
		    } /* if (method == 0) else ... */
		} /* if (labelDistance > 0) */

	    } /* if (drawLabels) */
	    else {
		GPolyline(ns, xxx, yyy, USER, dd);
	    }

//	    GMode(0, dd);
	    vmaxset(vmax);
	} /* while */
      } /* for(i .. )  for(j ..) */
    vmaxset(vmax); /* now we are done with ctr_SegDB */
    UNPROTECT_PTR(label1); /* pwwwargh! This is messy, but last thing
			      protected is likely labelList, and that needs
			      to be preserved across calls */
}


SEXP attribute_hidden Rg_contourDef(void)
{
    return ScalarLogical(GEcurrentDevice()->dev->useRotatedTextInContour);
}

/* contour(x, y, z, levels, labels, labcex, drawlabels,
 *         method, vfont, col = col, lty = lty, lwd = lwd)
 */
SEXP attribute_hidden do_contour(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP oargs, c, x, y, z, vfont, col, rawcol, lty, lwd, labels;
    int i, j, nx, ny, nc, ncol, nlty, nlwd;
    int ltysave, fontsave = 1 /* -Wall */;
    rcolor colsave;
    double cexsave, lwdsave;
    double atom, zmin, zmax;
    const void *vmax, *vmax0;
    char familysave[201];
    int method;
    Rboolean drawLabels;
    double labcex;
    pGEDevDesc dd = GEcurrentDevice();
    SEXP result = R_NilValue;

    GCheckState(dd);

    if (length(args) < 4)
	error(_("too few arguments"));
    PrintDefaults(); /* prepare for labelformat */

    oargs = args;

    x = CAR(args);
    internalTypeCheck(call, x, REALSXP);
    nx = LENGTH(x);
    args = CDR(args);

    y = CAR(args);
    internalTypeCheck(call, y, REALSXP);
    ny = LENGTH(y);
    args = CDR(args);

    z = CAR(args);
    internalTypeCheck(call, z, REALSXP);
    args = CDR(args);

    /* levels */
    c = CAR(args);
    internalTypeCheck(call, c, REALSXP);
    nc = LENGTH(c);
    args = CDR(args);

    labels = CAR(args);
    if (!isNull(labels))
	internalTypeCheck(call, labels, STRSXP);
    args = CDR(args);

    labcex = asReal(CAR(args));
    args = CDR(args);

    drawLabels = (Rboolean)asLogical(CAR(args));
    args = CDR(args);

    method = asInteger(CAR(args)); args = CDR(args);
    if (method < 1 || method > 3)
	error(_("invalid '%s' value"), "method");

    PROTECT(vfont = FixupVFont(CAR(args)));
    if (!isNull(vfont)) {
	strncpy(familysave, gpptr(dd)->family, 201);
	strncpy(gpptr(dd)->family, "Her ", 201);
	gpptr(dd)->family[3] = (char) INTEGER(vfont)[0];
	fontsave = gpptr(dd)->font;
	gpptr(dd)->font = INTEGER(vfont)[1];
    }
    args = CDR(args);

    rawcol = CAR(args);
    PROTECT(col = FixupCol(rawcol, R_TRANWHITE));
    ncol = length(col);
    args = CDR(args);

    PROTECT(lty = FixupLty(CAR(args), gpptr(dd)->lty));
    nlty = length(lty);
    args = CDR(args);

    PROTECT(lwd = FixupLwd(CAR(args), gpptr(dd)->lwd));
    nlwd = length(lwd);
    args = CDR(args);

    if (nx < 2 || ny < 2)
	error(_("insufficient 'x' or 'y' values"));

    if (nrows(z) != nx || ncols(z) != ny)
	error(_("dimension mismatch"));

    if (nc < 1)
	error(_("no contour values"));

    for (i = 0; i < nx; i++) {
	if (!R_FINITE(REAL(x)[i]))
	    error(_("missing 'x' values"));
	if (i > 0 && REAL(x)[i] < REAL(x)[i - 1])
	    error(_("increasing 'x' values expected"));
    }

    for (i = 0; i < ny; i++) {
	if (!R_FINITE(REAL(y)[i]))
	    error(_("missing 'y' values"));
	if (i > 0 && REAL(y)[i] < REAL(y)[i - 1])
	    error(_("increasing 'y' values expected"));
    }

    for (i = 0; i < nc; i++)
	if (!R_FINITE(REAL(c)[i]))
	    error(_("invalid NA contour values"));

    zmin = DBL_MAX;
    zmax = DBL_MIN;
    for (i = 0; i < nx * ny; i++)
	if (R_FINITE(REAL(z)[i])) {
	    if (zmax < REAL(z)[i]) zmax =  REAL(z)[i];
	    if (zmin > REAL(z)[i]) zmin =  REAL(z)[i];
	}

    if (zmin >= zmax) {
	if (zmin == zmax)
	    warning(_("all z values are equal"));
	else
	    warning(_("all z values are NA"));
	UNPROTECT(4);
	return R_NilValue;
    }

    /* change to 1e-3, reconsidered because of PR#897
     * but 1e-7, and even  2*DBL_EPSILON do not prevent inf.loop in contour().
     * maybe something like   16 * DBL_EPSILON * (..).
     * see also max_contour_segments above */
    atom = 1e-3 * (zmax - zmin);

    /* Initialize the segment data base */

    /* Note we must be careful about resetting */
    /* the top of the stack, otherwise we run out of */
    /* memory after a sequence of displaylist replays */

    vmax0 = vmaxget();
    ctr_SegDB = (SEGP*)R_alloc(nx*ny, sizeof(SEGP));

    for (i = 0; i < nx; i++)
	for (j = 0; j < ny; j++)
	    ctr_SegDB[i + j * nx] = NULL;

    /* Draw the contours -- note the heap release */

    ltysave = gpptr(dd)->lty;
    colsave = gpptr(dd)->col;
    lwdsave = gpptr(dd)->lwd;
    cexsave = gpptr(dd)->cex;
    labelList = PROTECT(R_NilValue);


    /* draw contour for levels[i] */
    GMode(1, dd);
    for (i = 0; i < nc; i++) {
	vmax = vmaxget();
	gpptr(dd)->lty = INTEGER(lty)[i % nlty];
	if (gpptr(dd)->lty == NA_INTEGER)
	    gpptr(dd)->lty = ltysave;
	if (isNAcol(rawcol, i, ncol))
	    gpptr(dd)->col = colsave;
	else
	    gpptr(dd)->col = INTEGER(col)[i % ncol];
	gpptr(dd)->lwd = REAL(lwd)[i % nlwd];
	if (!R_FINITE(gpptr(dd)->lwd))
	    gpptr(dd)->lwd = lwdsave;
	gpptr(dd)->cex = labcex;
	contour(x, nx, y, ny, z, REAL(c)[i], labels, i,
		drawLabels, method - 1, atom, dd);
	vmaxset(vmax);
    }
    GMode(0, dd);
    vmaxset(vmax0);
    gpptr(dd)->lty = ltysave;
    gpptr(dd)->col = colsave;
    gpptr(dd)->lwd = lwdsave;
    gpptr(dd)->cex = cexsave;
    if(!isNull(vfont)) {
	strncpy(gpptr(dd)->family, familysave, 201);
	gpptr(dd)->font = fontsave;
    }
    UNPROTECT(5);
    /* NOTE: only record operation if no "error"  */
    /* NOTE: on replay, call == R_NilValue */
    if (GRecording(call, dd))
	GErecordGraphicOperation(op, oargs, dd);
    return result;
}
