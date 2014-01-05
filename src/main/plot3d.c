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


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Internal.h>
#include <float.h>  /* for DBL_MAX */
#include <Rmath.h>
#include <Graphics.h>
#include <Print.h>
#include <R_ext/Boolean.h>

/* filled contours and perspective plots were originally here,
   now in graphics/src/plot3d.c .
 */

#include "contour-common.h"

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

/* This is for contourLines() in package grDevices */
SEXP do_contourLines(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP c, x, y, z;
    int nx, ny, nc;

    x = PROTECT(coerceVector(CAR(args), REALSXP));
    nx = LENGTH(x);
    args = CDR(args);

    y = PROTECT(coerceVector(CAR(args), REALSXP));
    ny = LENGTH(y);
    args = CDR(args);

    z = PROTECT(coerceVector(CAR(args), REALSXP));
    args = CDR(args);

    /* levels */
    c = PROTECT(coerceVector(CAR(args), REALSXP));
    nc = LENGTH(c);
    args = CDR(args);

    SEXP res = GEcontourLines(REAL(x), nx, REAL(y), ny, REAL(z), REAL(c), nc);
    UNPROTECT(4);
    return res;
}
