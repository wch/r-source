/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2013  The R Core Team
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


/* Stuff for labels on contour plots
   Originally written by Nicholas Hildreth
   Adapted by Paul Murrell
*/

/* Included by src/main/plot3d.c and src/library/graphics/src/plot3d  */

	/*  C o n t o u r   P l o t t i n g  */

typedef struct SEG {
    struct SEG *next;
    double x0;
    double y0;
    double x1;
    double y1;
} SEG, *SEGP;


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



/*
 * Generate a list of segments for a single level
 *
 * NB this R_allocs its return value, so callers need to manage R_alloc stack.
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
