/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "Defn.h"
#include "Mathlib.h"
#include "Graphics.h"

typedef struct SEG {
	struct SEG *next;
	double x0;
	double y0;
	double x1;
	double y1;
} SEG, *SEGP;

static SEGP *SegDB;

static int intersect(double z0, double z1, double zc, double *f)
{
	if((z0 - zc) * (z1 - zc) < 0.0) {
		*f = (zc - z0) / (z1 -  z0);
		return 1;
	}
	return 0;
}

static SEGP NewSeg(double x0, double y0, double x1, double y1, SEGP prev)
{
	SEGP seg = (SEGP)R_alloc(1, sizeof(SEG));
	seg->x0 = x0;
	seg->y0 = y0;
	seg->x1 = x1;
	seg->y1 = y1;
	seg->next = prev;
	return seg;
}

static void SwapSeg(SEGP seg)
{
	double x, y;
	x = seg->x0;
	y = seg->y0;
	seg->x0 = seg->x1;
	seg->y0 = seg->y1;
	seg->x1 = x;
	seg->y1 = y;
}

	/* Determine the entry direction to the next cell */
	/* and update the cell indices */

static int SegDir(double xend, double yend, double *x, double *y, int *i, int *j, int nx, int ny)
{
	if(yend == y[*j]) {
		if(*j == 0) return 0;
		*j = *j - 1;
		return 3;
	}
	if(xend == x[*i]) {
		if(*i == 0) return 0;
		*i = *i - 1;
		return 4;
	}
	if(yend == y[*j+1]) {
		if(*j >= ny - 1) return 0;
		*j = *j + 1;
		return 1;
	}
	if(xend == x[*i+1]) {
		if(*i >= nx - 1) return 0;
		*i = *i + 1;
		return 2;
	}
	return 0;
}

	/* Search seglist for a segment with endpoint (xend, yend). */
	/* The cell entry direction is dir, and if tail=1/0 we are */
	/* building the tail/head of a contour.  The matching segment */
	/* is pointed to by seg and the updated segment list (with
	/* the matched segment stripped is returned by the funtion. */

static SEGP SegUpdate(double xend, double yend, int dir, int tail, SEGP seglist, SEGP* seg)
{
	if(seglist == NULL) {
		*seg = NULL;
		return NULL;
	}
	switch(dir) {
	case 1:
	case 3:
		if(yend == seglist->y0) {
			if(!tail) SwapSeg(seglist);
			*seg = seglist;
			return seglist->next;
		}
		if(yend == seglist->y1) {
			if(tail) SwapSeg(seglist);
			*seg = seglist;
			return seglist->next;
		}
		break;
	case 2:
	case 4:
		if(xend == seglist->x0) {
			if(!tail) SwapSeg(seglist);
			*seg = seglist;
			return seglist->next;
		}
		if(xend == seglist->x1) {
			if(tail) SwapSeg(seglist);
			*seg = seglist;
			return seglist->next;
		}
		break;
	}
	seglist->next = SegUpdate(xend, yend, dir, tail, seglist->next, seg);
	return seglist;
}

static void contour(SEXP x, int nx, SEXP y, int ny, SEXP z, double zc, double atom)
{
	double f, xl, xh, yl, yh, zll, zhl, zlh, zhh, xx[4], yy[4];
	double xend, yend;
	int i, ii, j, jj, k, l, m, nacode, ns, ns2, dir;
	SEGP seglist, seg, s, start, end;

	for(i=0 ; i<nx-1 ; i++) {
		xl = REAL(x)[i];
		xh = REAL(x)[i+1];
		for(j=0 ; j<ny-1 ; j++) {
			yl = REAL(y)[j];
			yh = REAL(y)[j+1];
			k = i+j*nx;
			zll = REAL(z)[k];
			zhl = REAL(z)[k+1];
			zlh = REAL(z)[k+nx];
			zhh = REAL(z)[k+nx+1];
			k = 0;

				/* If the value at a corner is */
				/* exactly equal to a contour */
				/* level, change the value at */
				/* corner by a tiny amount. */

			if(zll == zc) zll = zll + atom;
			if(zhl == zc) zhl = zhl + atom;
			if(zlh == zc) zlh = zlh + atom;
			if(zhh == zc) zhh = zhh + atom;

				/* Check for intersections with sides */

			nacode = 0;
			if(FINITE(zll)) nacode += 1;
			if(FINITE(zhl)) nacode += 2;
			if(FINITE(zlh)) nacode += 4;
			if(FINITE(zhh)) nacode += 8;

			switch(nacode) {
			case 15:
				if(intersect(zll, zhl, zc, &f)) {
					xx[k] = xl + f * (xh - xl);
					yy[k] = yl; k++;
				}
				if(intersect(zll, zlh, zc, &f)) {
					yy[k] = yl + f * (yh - yl);
					xx[k] = xl; k++;
				}
				if(intersect(zhl, zhh, zc, &f)) {
					yy[k] = yl + f * (yh - yl);
					xx[k] = xh; k++;
				}
				if(intersect(zlh, zhh, zc, &f)) {
					xx[k] = xl + f * (xh - xl);
					yy[k] = yh; k++;
				}
				break;
			case 14:
				if(intersect(zhl, zhh, zc, &f)) {
					yy[k] = yl + f * (yh - yl);
					xx[k] = xh; k++;
				}
				if(intersect(zlh, zhh, zc, &f)) {
					xx[k] = xl + f * (xh - xl);
					yy[k] = yh; k++;
				}
				if(intersect(zlh, zhl, zc, &f)) {
					xx[k] = xl + f * (xh - xl);
					yy[k] = yh + f * (yl - yh);
					k++;
				}
				break;
			case 13:
				if(intersect(zll, zlh, zc, &f)) {
					yy[k] = yl + f * (yh - yl);
					xx[k] = xl; k++;
				}
				if(intersect(zlh, zhh, zc, &f)) {
					xx[k] = xl + f * (xh - xl);
					yy[k] = yh; k++;
				}
				if(intersect(zll, zhh, zc, &f)) {
					xx[k] = xl + f * (xh - xl);
					yy[k] = yl + f * (yh - yl);
					k++;
				}
				break;
			case 11:
				if(intersect(zhl, zhh, zc, &f)) {
					yy[k] = yl + f * (yh - yl);
					xx[k] = xh; k++;
				}
				if(intersect(zll, zhl, zc, &f)) {
					xx[k] = xl + f * (xh - xl);
					yy[k] = yl; k++;
				}
				if(intersect(zll, zhh, zc, &f)) {
					xx[k] = xl + f * (xh - xl);
					yy[k] = yl + f * (yh - yl);
					k++;
				}
				break;
			case 7:
				if(intersect(zll, zlh, zc, &f)) {
					yy[k] = yl + f * (yh - yl);
					xx[k] = xl; k++;
				}
				if(intersect(zll, zhl, zc, &f)) {
					xx[k] = xl + f * (xh - xl);
					yy[k] = yl; k++;
				}
				if(intersect(zlh, zhl, zc, &f)) {
					xx[k] = xl + f * (xh - xl);
					yy[k] = yh + f * (yl - yh);
					k++;
				}
				break;
			}

				/* We now have k(=2,4) endpoints */
				/* Decide which to join */

			seglist = NULL;

			if(k > 0) {
				if(k == 2) {
					seglist = NewSeg(xx[0], yy[0], xx[1], yy[1], seglist);
				}
				else if(k == 4) {
					for(k=3 ; k>=1 ; k--) {
						m = k;
						xl = xx[k];
						for(l=0 ; l<k ; l++) {
							if(xx[l] > xl) {
								xl = xx[l];
								m = l;
							}
						}
						if(m != k) {
							xl = xx[k];
							yl = yy[k];
							xx[k] = xx[m];
							yy[k] = yy[m];
							xx[m] = xl;
							yy[m] = yl;
						}
					}
					seglist = NewSeg(xx[0], yy[0], xx[1], yy[1], seglist);
					seglist = NewSeg(xx[2], yy[2], xx[3], yy[3], seglist);
				}
			}
			SegDB[i+j*nx] = seglist;
		}
	}

		/* The segment database is now assembled. */
		/* Begin following contours. */
		/* 1. Grab a segment */
		/* 2. Follow its tail */
		/* 3. Follow its head */
		/* 4. Draw the contour */

	for(i=0 ; i<nx-1 ; i++)
		for(j=0 ; j<ny-1 ; j++) {
			while(seglist = SegDB[i+j*nx]) {
				ii = i; jj = j;
				start = end = seglist;
				SegDB[i+j*nx] = seglist->next;
				xend = seglist->x1;
				yend = seglist->y1;
				while(dir=SegDir(xend, yend, REAL(x), REAL(y), &ii, &jj, nx, ny)) {
					SegDB[ii+jj*nx] = SegUpdate(xend, yend, dir, 1, SegDB[ii+jj*nx], &seg);
					if(!seg) break;
					end->next = seg;
					end = seg;
					xend = end->x1;
					yend = end->y1;
				}
				ii = i; jj = j;
				xend = seglist->x0;
				yend = seglist->y0;
				while(dir=SegDir(xend, yend, REAL(x), REAL(y), &ii, &jj, nx, ny)) {
					SegDB[ii+jj*nx] = SegUpdate(xend, yend, dir, 0, SegDB[ii+jj*nx], &seg);
					if(!seg) break;
					seg->next = start;
					start = seg;
					xend = start->x0;
					yend = start->y0;
				}
				s = start;
				ns = 0;
				while(s) {
					ns++;
					s = s->next;
				}
				if(ns > 3) ns2 = ns/2;
				else ns2 = -1;

				s = start;
				ns = 0;
				GMode(1);
				GStartPath();
				GMoveTo(XMAP(s->x0), YMAP(s->y0));
				while(s) {
					GLineTo(XMAP(s->x1), YMAP(s->y1));
					s = s->next;
				}
				GEndPath();
				GMode(0);
			}
		}

}


SEXP do_contour(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP c, x, y, z, col, lty;
	int i, j, nx, ny, nc, ncol, nlty;
	int ltysave, colsave;
	double atom, zmin, zmax;
	char *vmax, *vmax0;

	GCheckState();

	if(length(args) < 4) errorcall(call, "too few arguments\n");

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

	c = CAR(args);
	internalTypeCheck(call, c, REALSXP);
	nc = LENGTH(c);
	args = CDR(args);

	PROTECT(col = FixupCol(GetPar("col", args)));
	ncol = length(col);

	PROTECT(lty = FixupLty(GetPar("lty", args)));
	nlty = length(lty);

		/* col, lwd and lty vectors here */

	if(nx < 2 || ny < 2)
		errorcall(call, "insufficient x or y values\n");

	if(nrows(z) != nx || ncols(z) != ny)
		errorcall(call, "dimension mismatch\n");


	if(nc < 1)
		errorcall(call, "no contour values\n");

	for(i=0 ; i<nx ; i++) {
		if(!FINITE(REAL(x)[i]))
			errorcall(call, "missing x values\n");
		if(i > 0 && REAL(x)[i] < REAL(x)[i-1])
			errorcall(call, "increasing x values expected\n");
	}

	for(i=0 ; i<ny ; i++) {
		if(!FINITE(REAL(y)[i]))
			errorcall(call, "missing y values\n");
		if(i > 0 && REAL(y)[i] < REAL(y)[i-1])
			errorcall(call, "increasing y values expected\n");
	}

	for(i=0 ; i<nc ; i++)
		if(!FINITE(REAL(c)[i]))
			errorcall(call, "illegal NA contour values\n");

	zmin = DBL_MAX;
	zmax = DBL_MIN;
	for(i=0 ; i<nx*ny ; i++)
		if(FINITE(REAL(z)[i])) {
			if(zmax < REAL(z)[i]) zmax =  REAL(z)[i];
			if(zmin > REAL(z)[i]) zmin =  REAL(z)[i];
		}

	if(zmin >= zmax) {
		if(zmin == zmax)
			warning("all z values are equal\n");
		else
			warning("all z values are NA\n");
		return R_NilValue;
	}

	atom = DBL_EPSILON * (zmax - zmin);

		/* Initialize the segment data base */
		/* Note we must be careful about resetting */
		/* the top of the stack, otherwise we run out of */
		/* memory after a sequence of displaylist replays */

	vmax0 = vmaxget();
	SegDB = (SEGP*)R_alloc(nx*ny, sizeof(SEGP));

	for(i=0 ; i<nx ; i++)
		for(j=0 ; j<ny ; j++)
			SegDB[i+j*nx] = NULL;

		/* Draw the contours -- note the heap release */

	ltysave = GP->lty;
	colsave = GP->col;
	for(i=0 ; i<nc ; i++) {
		vmax = vmaxget();
		GP->lty = INTEGER(lty)[i%nlty];
		if(GP->lty == NA_INTEGER) GP->lty = ltysave;
		GP->col = INTEGER(col)[i%ncol];
		if(GP->col == NA_INTEGER) GP->col = colsave;
		contour(x, nx, y, ny, z, REAL(c)[i], atom);
		vmaxset(vmax);
	}
	vmaxset(vmax0);
	GP->lty = ltysave;
	GP->col = colsave;
	UNPROTECT(2);
	return R_NilValue;
}


SEXP do_image(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP sx, sy, sz, szlim, sc;
	double *x, *y, *z;
	unsigned *c;
	double xlow, xhigh, ylow, yhigh, zmin, zmax;
	int i, j, nx, ny, nz, ic, nc, colsave, xpdsave;

	GCheckState();

	checkArity(op,args);

	sx = CAR(args);
	internalTypeCheck(call, sx, REALSXP);
	nx = LENGTH(sx);
	args = CDR(args);

	sy = CAR(args);
	internalTypeCheck(call, sy, REALSXP);
	ny = LENGTH(sy);
	args = CDR(args);

	sz = CAR(args);
	internalTypeCheck(call, sz, REALSXP);
	nz = length(sz);
	args = CDR(args);

	szlim = CAR(args);
	internalTypeCheck(call, szlim, REALSXP);
	if(length(szlim) != 2 ||
		!FINITE(REAL(szlim)[0]) ||
		!FINITE(REAL(szlim)[1]) ||
		REAL(szlim)[0] >= REAL(szlim)[1])
			errorcall(call, "invalid z limits\n");
	zmin = REAL(szlim)[0];
	zmax = REAL(szlim)[1];
	args = CDR(args);

	PROTECT(sc = FixupCol(CAR(args)));
	nc = length(sc);

		/* Shorthand Pointers */

	x = REAL(sx);
	y = REAL(sy);
	z = REAL(sz);
	c = (unsigned*)INTEGER(sc);

		/* Check of grid coordinates */
		/* We want them to all be finite and */
		/* in strictly ascending order */

	if(nx < 2 || ny < 2) goto badxy;
	if(!FINITE(x[0])) goto badxy;
	if(!FINITE(y[0])) goto badxy;
	for(i=1 ; i<nx ; i++)
		if(!FINITE(x[i]) || x[i] <= x[i-1]) goto badxy;
	for(j=1 ; j<ny ; j++)
		if(!FINITE(y[j]) || y[j] <= y[j-1]) goto badxy;

	colsave = GP->col;
	xpdsave = GP->xpd;
	GP->xpd = 0;

	GMode(1);

	for(i=0 ; i<nx ; i++) {
		if(i == 0)
			xlow = XMAP(x[0]);
		else
			xlow = XMAP(0.5 * (x[i] + x[i-1]));
		if(i == nx-1)
			xhigh = XMAP(x[nx-1]);
		else
			xhigh = XMAP(0.5 * (x[i] + x[i+1]));

		for(j=0 ; j<ny ; j++) {
			if(FINITE(z[i+j*nx])) {
				ic = floor((nc - 1) * (z[i+j*nx]-zmin)/(zmax - zmin) + 0.5);
				if(ic >= 0 && ic < nc) {
					if(j == 0)
						ylow = YMAP(y[0]);
					else
						ylow = YMAP(0.5 * (y[j] + y[j-1]));
					if(j == ny-1)
						yhigh = YMAP(y[ny-1]);
					else
						yhigh = YMAP(0.5 * (y[j] + y[j+1]));
					GRect(xlow, ylow, xhigh, yhigh, c[ic], NA_INTEGER);
				}
			}
		}
	}
	GMode(0);
	GP->col = colsave;
	GP->xpd = xpdsave;
	R_Visible = 0;
	UNPROTECT(1);
	return R_NilValue;

badxy:
	errorcall(call, "invalid x / y limits\n");
}
