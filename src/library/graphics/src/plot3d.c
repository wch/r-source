/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2014  The R Core Team
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
 *  https://www.R-project.org/Licenses/
 */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <float.h>  /* for DBL_MAX */
#include <Rmath.h>
#include <Graphics.h>
#include <Print.h>
#include <R_ext/Boolean.h>

#include "graphics.h"

static void TypeCheck(SEXP s, SEXPTYPE type)
{
    if (TYPEOF(s) != type)
	error("invalid type passed to graphics function");
}


	/*  F i l l e d   C o n t o u r   P l o t s  */

	/*  R o s s  I h a k a,  M a r c h  1 9 9 9  */

static void
FindCutPoints(double low, double high,
	      double x1, double y1, double z1,
	      double x2, double y2, double z2,
	      double *x, double *y, double *z,
	      int *npt)
{
    double c;

    if (z1 > z2 ) {
	if (z2 > high || z1 < low) return;
	if (z1 < high) {
	    x[*npt] = x1;
	    y[*npt] = y1;
	    z[*npt] = z1;
	    ++*npt;
	} else if (z1 == R_PosInf) {
	    x[*npt] = x2;
	    y[*npt] = y1;
	    z[*npt] = z2;
	    ++*npt;
	} else { /* z1 >= high, z2 in range */
	    c = (z1 - high) / (z1 - z2);
	    x[*npt] = x1 + c * (x2 - x1);
	    y[*npt] = y1;
	    z[*npt] = z1 + c * (z2 - z1);
	    ++*npt;
	}
	if (z2 == R_NegInf) {
	    x[*npt] = x1;
	    y[*npt] = y1;
	    z[*npt] = z1;
	    ++*npt;
	} else if (z2 <= low) { /* and z1 in range */
	    c = (z2 -low) / (z2 - z1);
	    x[*npt] = x2 - c * (x2 - x1);
	    y[*npt] = y1;
	    z[*npt] = z2 - c * (z2 - z1);
	    ++*npt;
	}
    } else if (z1 < z2) {
	if (z2 < low || z1 > high) return;
	if (z1 > low) {
	    x[*npt] = x1;
	    y[*npt] = y1;
	    z[*npt] = z1;
	    ++*npt;
	} else if (z1 == R_NegInf) {
	    x[*npt] = x2;
	    y[*npt] = y1;
	    z[*npt] = z2;;
	    ++*npt;
	} else { /* and z2 in range */
	    c = (z1 - low) / (z1 - z2);
	    x[*npt] = x1 + c * (x2 - x1);
	    y[*npt] = y1;
	    z[*npt] = z1 + c * (z2 - z1);
	    ++*npt;
	}
	if (z2 < high) {
#ifdef OMIT
	    /* Don't repeat corner vertices */
	    x[*npt] = x2;
	    y[*npt] = y2;
	    z[*npt] = z2;
	    ++*npt;
#endif
	} else if (z2 == R_PosInf) {
	    x[*npt] = x1;
	    y[*npt] = y1;
	    z[*npt] = z1;
	    ++*npt;
	} else { /* z2 high, z1 in range */
	    c = (z2 - high) / (z2 - z1);
	    x[*npt] = x2 - c * (x2 - x1);
	    y[*npt] = y1;
	    z[*npt] = z2 - c * (z2 - z1);
	    ++*npt;
	}
    } else {
	if(low <= z1 && z1 <= high) {
	    x[*npt] = x1;
	    y[*npt] = y1;
	    z[*npt] = z1;
	    ++*npt;
#ifdef OMIT
	    /* Don't repeat corner vertices */
	    x[*npt] = x2;
	    y[*npt] = y2;
	    z[*npt] = z2;
	    ++*npt;
#endif
	}
    }
}

/* FIXME - This could pretty easily be adapted to handle NA */
/* values on the grid.  Just search the diagonals for cutpoints */
/* instead of the cell sides.  Use the same switch idea as in */
/* contour above.  There are 5 cases to handle. */

static void
FindPolygonVertices(double low, double high,
		    double x1, double x2, double y1, double y2,
		    double z11, double z21, double z12, double z22,
		    double *x, double *y, double *z, int *npt)
{
    *npt = 0;
    FindCutPoints(low, high, x1,  y1,  z11, x2,  y1,  z21, x, y, z, npt);
    FindCutPoints(low, high, y1,  x2,  z21, y2,  x2,  z22, y, x, z, npt);
    FindCutPoints(low, high, x2,  y2,  z22, x1,  y2,  z12, x, y, z, npt);
    FindCutPoints(low, high, y2,  x1,  z12, y1,  x1,  z11, y, x, z, npt);
}

/* filledcontour(x, y, z, levels, col) */
SEXP C_filledcontour(SEXP args)
{
    SEXP sx, sy, sz, sc, scol;
    double *x, *y, *z, *c;
    rcolor *col;
    int i, j, k, npt, nx, ny, nc, ncol, colsave, xpdsave;
    double px[8], py[8], pz[8];
    pGEDevDesc dd = GEcurrentDevice();

    GCheckState(dd);

    PrintDefaults(); /* prepare for labelformat */

    args = CDR(args);
    sx = PROTECT(coerceVector(CAR(args), REALSXP));
    nx = LENGTH(sx);
    args = CDR(args);

    sy = PROTECT(coerceVector(CAR(args), REALSXP));
    ny = LENGTH(sy);
    args = CDR(args);
    if (nx < 2 || ny < 2) error(_("insufficient 'x' or 'y' values"));

    // do it this way as coerceVector can lose dims, e.g. for a list matrix
    sz = CAR(args);
    if (nrows(sz) != nx || ncols(sz) != ny) error(_("dimension mismatch"));
    sz = PROTECT(coerceVector(sz, REALSXP));
    args = CDR(args);

    sc = PROTECT(coerceVector(CAR(args), REALSXP)); /* levels */
    nc = length(sc);
    args = CDR(args);

    if (nc < 1) error(_("no contour values"));

    PROTECT(scol = FixupCol(CAR(args), R_TRANWHITE));
    ncol = length(scol);

    /* Shorthand Pointers */

    x = REAL(sx);
    y = REAL(sy);
    z = REAL(sz);
    c = REAL(sc);
    col = (rcolor *) INTEGER(scol);

    /* Check of grid coordinates */
    /* We want them to all be finite */
    /* and in strictly ascending order */

    if (nx < 1 || ny < 1) goto badxy;
    if (!R_FINITE(x[0])) goto badxy;
    if (!R_FINITE(y[0])) goto badxy;
    for (i = 1; i < nx; i++)
	if (!R_FINITE(x[i]) || x[i] <= x[i - 1]) goto badxy;
    for (j = 1; j < ny; j++)
	if (!R_FINITE(y[j]) || y[j] <= y[j - 1]) goto badxy;

    /* Check of the contour levels */

    if (!R_FINITE(c[0])) goto badlev;
    for (k = 1; k < nc; k++)
	if (!R_FINITE(c[k]) || c[k] <= c[k - 1]) goto badlev;

    colsave = gpptr(dd)->col;
    xpdsave = gpptr(dd)->xpd;
    /* override par("xpd") and force clipping to plot region */
    gpptr(dd)->xpd = 0;

    GMode(1, dd);

    for (i = 1; i < nx; i++) {
	for (j = 1; j < ny; j++) {
	    for (k = 1; k < nc ; k++) {
		FindPolygonVertices(c[k - 1], c[k],
				    x[i - 1], x[i],
				    y[j - 1], y[j],
				    z[i - 1 + (j - 1) * nx],
				    z[i + (j - 1) * nx],
				    z[i - 1 + j * nx],
				    z[i + j * nx],
				    px, py, pz, &npt);
		if (npt > 2)
		    GPolygon(npt, px, py, USER, col[(k-1) % ncol],
			     R_TRANWHITE, dd);
	    }
	}
    }
    GMode(0, dd);
    gpptr(dd)->col = colsave;
    gpptr(dd)->xpd = xpdsave;
    UNPROTECT(5);
    return R_NilValue;

 badxy:
    error(_("invalid x / y values or limits"));
 badlev:
    error(_("invalid contour levels: must be strictly increasing"));
    return R_NilValue;  /* never used; to keep -Wall happy */
}



	/*  I m a g e   R e n d e r i n g  */


/* image(x, y, z, col, breaks) */
SEXP C_image(SEXP args)
{
    SEXP sx, sy, sz, sc;
    double *x, *y;
    int *z, tmp;
    unsigned *c;
    int i, j, nx, ny, nc, xpdsave;
    rcolor colsave;
    pGEDevDesc dd = GEcurrentDevice();

    GCheckState(dd);

    args = CDR(args);

    sx = PROTECT(coerceVector(CAR(args), REALSXP));
    nx = LENGTH(sx);
    args = CDR(args);

    sy = PROTECT(coerceVector(CAR(args), REALSXP));
    ny = LENGTH(sy);
    args = CDR(args);

    sz = PROTECT(coerceVector(CAR(args), INTSXP));
    args = CDR(args);

    PROTECT(sc = FixupCol(CAR(args), R_TRANWHITE));
    nc = LENGTH(sc);

    /* Shorthand Pointers */

    x = REAL(sx);
    y = REAL(sy);
    z = INTEGER(sz);
    c = (unsigned*)INTEGER(sc);

    /* Check of grid coordinates now done in C code */

    colsave = gpptr(dd)->col;
    xpdsave = gpptr(dd)->xpd;
    /* override par("xpd") and force clipping to plot region */
    gpptr(dd)->xpd = 0;

    GMode(1, dd);

    for (i = 0; i < nx - 1 ; i++) {
	for (j = 0; j < ny - 1; j++) {
	    tmp = z[i + j * (nx - 1)];
	    if (tmp >= 0 && tmp < nc && tmp != NA_INTEGER)
		GRect(x[i], y[j], x[i+1], y[j+1], USER, c[tmp],
		      R_TRANWHITE, dd);
	}
    }
    GMode(0, dd);
    gpptr(dd)->col = colsave;
    gpptr(dd)->xpd = xpdsave;
    UNPROTECT(4);
    return R_NilValue;
}

	/*  P e r s p e c t i v e   S u r f a c e   P l o t s  */

/* Conversion of degrees to radians */

#define DegToRad(x) (DEG2RAD * x)

/* Definitions of data structures for vectors and */
/* transformations in homogeneous 3d coordinates */

typedef double Vector3d[4];
typedef double Trans3d[4][4];

/* The viewing transformation matrix. */

static Trans3d VT;

static void TransVector (Vector3d u, Trans3d T, Vector3d v)
{
    double sum;
    int i, j;

    for (i = 0; i < 4; i++) {
	sum = 0;
	for (j = 0; j < 4; j++)
	    sum = sum + u[j] * T[j][i];
	v[i] = sum;
    }
}

static void Accumulate (Trans3d T)
{
    Trans3d U;
    double sum;
    int i, j, k;

    for (i = 0; i < 4; i++) {
	for (j = 0; j < 4; j++) {
	    sum = 0;
	    for (k = 0; k < 4; k++)
		sum = sum + VT[i][k] * T[k][j];
	    U[i][j] = sum;
	}
    }
    for (i = 0; i < 4; i++)
	for (j = 0; j < 4; j++)
	    VT[i][j] = U[i][j];
}

static void SetToIdentity (Trans3d T)
{
    int i, j;
    for (i = 0; i < 4; i++) {
	for (j = 0; j < 4; j++)
	    T[i][j] = 0;
	T[i][i] = 1;
    }
}

static void Translate (double x, double y, double z)
{
    Trans3d T;
    SetToIdentity(T);
    T[3][0] = x;
    T[3][1] = y;
    T[3][2] = z;
    Accumulate(T);
}

static void Scale (double x, double y, double z)
{
    Trans3d T;
    SetToIdentity(T);
    T[0][0] = x;
    T[1][1] = y;
    T[2][2] = z;
    Accumulate(T);
}

static void XRotate (double angle)
{
    double c, s;
    Trans3d T;
    SetToIdentity(T);
    c = cos(DegToRad(angle));
    s = sin(DegToRad(angle));
    T[1][1] = c;
    T[2][1] = -s;
    T[2][2] = c;
    T[1][2] = s;
    Accumulate(T);
}

static void YRotate (double angle)
{
    double c, s;
    Trans3d T;
    SetToIdentity(T);
    c = cos(DegToRad(angle));
    s = sin(DegToRad(angle));
    T[0][0] = c;
    T[2][0] = s;
    T[2][2] = c;
    T[0][2] = -s;
    Accumulate(T);
}

static void ZRotate (double angle)
{
    double c, s;
    Trans3d T;
    SetToIdentity(T);
    c = cos(DegToRad(angle));
    s = sin(DegToRad(angle));
    T[0][0] = c;
    T[1][0] = -s;
    T[1][1] = c;
    T[0][1] = s;
    Accumulate(T);
}

static void Perspective (double d)
{
    Trans3d T;

    SetToIdentity(T);
    T[2][3] = -1 / d;
    Accumulate(T);
}


/* Set up the light source */
static double Light[4];
static double Shade;
static Rboolean DoLighting;

static void SetUpLight(double theta, double phi)
{
    double u[4];
    u[0] = 0; u[1] = -1; u[2] = 0; u[3] = 1;
    SetToIdentity(VT);             /* Initialization */
    XRotate(-phi);                 /* colatitude rotation */
    ZRotate(theta);                /* azimuthal rotation */
    TransVector(u, VT, Light);	   /* transform */
}

static double FacetShade(double *u, double *v)
{
    double nx, ny, nz, sum;
    nx = u[1] * v[2] - u[2] * v[1];
    ny = u[2] * v[0] - u[0] * v[2];
    nz = u[0] * v[1] - u[1] * v[0];
    sum = sqrt(nx * nx + ny * ny + nz * nz);
    if (sum == 0) sum = 1;
    nx /= sum;
    ny /= sum;
    nz /= sum;
    sum = 0.5 * (nx * Light[0] + ny * Light[1] + nz * Light[2] + 1);
    return pow(sum, Shade);
}


/* For each facet, determine the farthest point from the eye. */
/* Sorting the facets so that these depths are decreasing */
/* yields an occlusion compatible ordering. */
/* Note that we ignore z values when doing this. */

static void DepthOrder(double *z, double *x, double *y, int nx, int ny,
		       double *depth, int *indx)
{
    int i, ii, j, jj, nx1, ny1;
    Vector3d u, v;
    double d;
    nx1 = nx - 1;
    ny1 = ny - 1;
    for (i = 0; i < nx1 * ny1; i++)
	indx[i] = i;
    for (i = 0; i < nx1; i++)
	for (j = 0; j < ny1; j++) {
	    d = -DBL_MAX;
	    for (ii = 0; ii <= 1; ii++)
		for (jj = 0; jj <= 1; jj++) {
		    u[0] = x[i + ii];
		    u[1] = y[j + jj];
		    /* Originally I had the following line here: */
		    /* u[2] = z[i+ii+(j+jj)*nx]; */
		    /* But this leads to artifacts. */
		    /* It has been replaced by the following line: */
		    u[2] = 0;
		    u[3] = 1;
		    if (R_FINITE(u[0]) &&  R_FINITE(u[1]) && R_FINITE(u[2])) {
			TransVector(u, VT, v);
			if (v[3] > d) d = v[3];
		    }
		}
	    depth[i+j*nx1] = -d;

	}
    /* Determine the depth ordering of the facets to ensure
       that they are drawn in an occlusion compatible order. */
    rsort_with_index(depth, indx, nx1 * ny1);
}


static void DrawFacets(double *z, double *x, double *y, int nx, int ny,
		       int *indx, double xs, double ys, double zs,
		       int *col, int ncol, int border)
{
    double xx[4], yy[4], shade = 0;
    Vector3d u, v;
    int i, j, k, n, nx1, ny1, icol, nv;
    unsigned int newcol, r, g, b;
    pGEDevDesc dd;
    dd = GEcurrentDevice();
    nx1 = nx - 1;
    ny1 = ny - 1;
    n = nx1 * ny1;
    for (k = 0; k < n; k++) {
	nv = 0;
	i = indx[k] % nx1;
	j = indx[k] / nx1;
	icol = (i + j * nx1) % ncol;
	if (DoLighting) {
	    /* Note we must scale here */
	    u[0] = xs * (x[i+1] - x[i]);
	    u[1] = ys * (y[j] - y[j+1]);
	    u[2] = zs * (z[(i+1)+j*nx] - z[i+(j+1)*nx]);
	    v[0] = xs * (x[i+1] - x[i]);
	    v[1] = ys * (y[j+1] - y[j]);
	    v[2] = zs * (z[(i+1)+(j+1)*nx] - z[i+j*nx]);
	    shade = FacetShade(u, v);
	}
	u[0] = x[i]; u[1] = y[j];
	u[2] = z[i + j * nx]; u[3] = 1;
	if (R_FINITE(u[0]) &&  R_FINITE(u[1]) && R_FINITE(u[2])) {
	    TransVector(u, VT, v);
	    xx[nv] = v[0] / v[3];
	    yy[nv] = v[1] / v[3];
	    nv++;
	}

	u[0] = x[i + 1]; u[1] = y[j];
	u[2] = z[i + 1 + j * nx]; u[3] = 1;
	if (R_FINITE(u[0]) &&  R_FINITE(u[1]) && R_FINITE(u[2])) {
	    TransVector(u, VT, v);
	    xx[nv] = v[0] / v[3];
	    yy[nv] = v[1] / v[3];
	    nv++;
	}

	u[0] = x[i + 1]; u[1] = y[j + 1];
	u[2] = z[i + 1 + (j + 1) * nx]; u[3] = 1;
	if (R_FINITE(u[0]) &&  R_FINITE(u[1]) && R_FINITE(u[2])) {
	    TransVector(u, VT, v);
	    xx[nv] = v[0] / v[3];
	    yy[nv] = v[1] / v[3];
	    nv++;
	}

	u[0] = x[i]; u[1] = y[j + 1];
	u[2] = z[i + (j + 1) * nx]; u[3] = 1;
	if (R_FINITE(u[0]) &&  R_FINITE(u[1]) && R_FINITE(u[2])) {
	    TransVector(u, VT, v);
	    xx[nv] = v[0] / v[3];
	    yy[nv] = v[1] / v[3];
	    nv++;
	}

	if (nv > 2) {
	    newcol = col[icol];
	    if (DoLighting) {
		// shade can degenerate to NaN
		if(R_FINITE(shade)) {
		    r = (int)(shade * R_RED(newcol));
		    g = (int)(shade * R_GREEN(newcol));
		    b = (int)(shade * R_BLUE(newcol));
		    newcol = R_RGB(r, g, b);
		    GPolygon(nv, xx, yy, USER, newcol, border, dd);
		}
	    } else 
		GPolygon(nv, xx, yy, USER, newcol, border, dd);
	}
    }
}


static void PerspWindow(double *xlim, double *ylim, double *zlim, pGEDevDesc dd)
{
    double pin1, pin2, scale, xdelta, ydelta, xscale, yscale, xadd, yadd;
    double xmax, xmin, ymax, ymin, xx, yy;
    Vector3d u, v;
    int i, j, k;

    xmax = xmin = ymax = ymin = 0;
    u[3] = 1;
    for (i = 0; i < 2; i++) {
	u[0] = xlim[i];
	for (j = 0; j < 2; j++) {
	    u[1] = ylim[j];
	    for (k = 0; k < 2; k++) {
		u[2] = zlim[k];
		TransVector(u, VT, v);
		xx = v[0] / v[3];
		yy = v[1] / v[3];
		if (xx > xmax) xmax = xx;
		if (xx < xmin) xmin = xx;
		if (yy > ymax) ymax = yy;
		if (yy < ymin) ymin = yy;
	    }
	}
    }
    pin1 = GConvertXUnits(1.0, NPC, INCHES, dd);
    pin2 = GConvertYUnits(1.0, NPC, INCHES, dd);
    xdelta = fabs(xmax - xmin);
    ydelta = fabs(ymax - ymin);
    xscale = pin1 / xdelta;
    yscale = pin2 / ydelta;
    scale = (xscale < yscale) ? xscale : yscale;
    xadd = .5 * (pin1 / scale - xdelta);
    yadd = .5 * (pin2 / scale - ydelta);
    GScale(xmin - xadd, xmax + xadd, 1, dd);
    GScale(ymin - yadd, ymax + yadd, 2, dd);
    GMapWin2Fig(dd);
}

static int LimitCheck(double *lim, double *c, double *s)
{
    if (!R_FINITE(lim[0]) || !R_FINITE(lim[1]) || lim[0] >= lim[1])
	return 0;
    *s = 0.5 * fabs(lim[1] - lim[0]);
    *c = 0.5 * (lim[1] + lim[0]);
    return 1;
}

/* PerspBox: The following code carries out a visibility test
   on the surfaces of the xlim/ylim/zlim box around the plot.
   If front = 0, only the faces with their inside toward the
   eyepoint are drawn.  If front = 1, only the faces with
   their outside toward the eye are drawn.  This lets us carry
   out hidden line removal by drawing any faces which will be
   obscured before the surface, and those which will not be
   obscured after the surface. 

   Unfortunately as PR#202 showed, this is simplistic as the surface
   can go outside the box.
*/

/* The vertices of the box */
static short int Vertex[8][3] = {
    {0, 0, 0},
    {0, 0, 1},
    {0, 1, 0},
    {0, 1, 1},
    {1, 0, 0},
    {1, 0, 1},
    {1, 1, 0},
    {1, 1, 1},
};

/* The vertices visited when tracing a face */
static short int Face[6][4] = {
    {0, 1, 5, 4},
    {2, 6, 7, 3},
    {0, 2, 3, 1},
    {4, 5, 7, 6},
    {0, 4, 6, 2},
    {1, 3, 7, 5},
};

/* The edges drawn when tracing a face */
static short int Edge[6][4] = {
    { 0, 1, 2, 3},
    { 4, 5, 6, 7},
    { 8, 7, 9, 0},
    { 2,10, 5,11},
    { 3,11, 4, 8},
    { 9, 6,10, 1},
};


static void PerspBox(int front, double *x, double *y, double *z, 
		     char *EdgeDone, pGEDevDesc dd)
{
    Vector3d u0, v0, u1, v1, u2, v2, u3, v3;
    double d[3], e[3];
    int f, i, p0, p1, p2, p3, nearby;
    int ltysave = gpptr(dd)->lty;
    
    gpptr(dd)->lty = front ? LTY_DOTTED : LTY_SOLID;

    for (f = 0; f < 6; f++) {
	p0 = Face[f][0];
	p1 = Face[f][1];
	p2 = Face[f][2];
	p3 = Face[f][3];

	u0[0] = x[Vertex[p0][0]];
	u0[1] = y[Vertex[p0][1]];
	u0[2] = z[Vertex[p0][2]];
	u0[3] = 1;
	u1[0] = x[Vertex[p1][0]];
	u1[1] = y[Vertex[p1][1]];
	u1[2] = z[Vertex[p1][2]];
	u1[3] = 1;
	u2[0] = x[Vertex[p2][0]];
	u2[1] = y[Vertex[p2][1]];
	u2[2] = z[Vertex[p2][2]];
	u2[3] = 1;
	u3[0] = x[Vertex[p3][0]];
	u3[1] = y[Vertex[p3][1]];
	u3[2] = z[Vertex[p3][2]];
	u3[3] = 1;

	TransVector(u0, VT, v0);
	TransVector(u1, VT, v1);
	TransVector(u2, VT, v2);
	TransVector(u3, VT, v3);

	/* Visibility test. */
	/* Determine whether the surface normal is toward the eye. */
	/* Note that we only draw lines once. */

	for (i = 0; i < 3; i++) {
	    d[i] = v1[i]/v1[3] - v0[i]/v0[3];
	    e[i] = v2[i]/v2[3] - v1[i]/v1[3];
	}
	nearby = (d[0]*e[1] - d[1]*e[0]) < 0;

	if ((front && nearby) || (!front && !nearby)) {
	    if (!EdgeDone[Edge[f][0]]++)
		GLine(v0[0]/v0[3], v0[1]/v0[3],
		      v1[0]/v1[3], v1[1]/v1[3], USER, dd);
	    if (!EdgeDone[Edge[f][1]]++)
		GLine(v1[0]/v1[3], v1[1]/v1[3],
		      v2[0]/v2[3], v2[1]/v2[3], USER, dd);
	    if (!EdgeDone[Edge[f][2]]++)
		GLine(v2[0]/v2[3], v2[1]/v2[3],
		      v3[0]/v3[3], v3[1]/v3[3], USER, dd);
	    if (!EdgeDone[Edge[f][3]]++)
		GLine(v3[0]/v3[3], v3[1]/v3[3],
		      v0[0]/v0[3], v0[1]/v0[3], USER, dd);
	}
    }
    gpptr(dd)->lty = ltysave;
}

/* PerspAxes:
 */

/* Starting vertex for possible axes */
static short int AxisStart[8] = { 0, 0, 2, 4, 0, 4, 2, 6 };

/* Tick vector for possible axes */
static short int TickVector[8][3] = {
    {0, -1, -1},
    {-1, 0, -1},
    {0, 1, -1},
    {1, 0, -1},
    {-1, -1, 0},
    {1, -1, 0},
    {-1, 1, 0},
    {1, 1, 0}};

static int lowest(double y1, double y2, double y3, double y4) {
    return ((y1 <= y2) && (y1 <= y3) && (y1 <= y4));
}

static double labelAngle(double x1, double y1, double x2, double y2) {
    double dx, dy;
    double angle;
    dx = fabs(x2 - x1);
    if (x2 > x1)
	dy = y2 - y1;
    else
	dy = y1 - y2;
    if (dx == 0) {
	if (dy > 0)
	    angle = 90.;
	else
	    angle = 270.;
    } else {
#ifdef HAVE_ATAN2PI
	angle = 180. * atan2(dy, dx);
#else
	angle = (180. / M_PI) * atan2(dy, dx);
#endif
    }
    return angle;
}

static void PerspAxis(double *x, double *y, double *z,
		      int axis, int axisType, int nTicks, int tickType,
		      const char *label, cetype_t enc, pGEDevDesc dd)
{
    Vector3d u1={0.,0.,0.,0.}, u2={0.,0.,0.,0.}, u3={0.,0.,0.,0.}, v1, v2, v3;
    double tickLength = .03; /* proportion of axis length */
    double min, max, d_frac;
    double *range = NULL; /* -Wall */
    double axp[3];
    int nint, i;
    SEXP at, lab;
    double cexsave = gpptr(dd)->cex;
    int fontsave = gpptr(dd)->font;


    switch (axisType) {
    case 0:
	min = x[0];	max = x[1];	range = x;	break;
    case 1:
	min = y[0];	max = y[1];	range = y;	break;
    case 2:
	min = z[0];	max = z[1];	range = z;	break;
    }
    d_frac = 0.1*(max - min);
    nint = nTicks - 1; if(!nint) nint++;
    i = nint;
    GPretty(&min, &max, &nint);
    /* GPretty() rarely gives values too much outside range ..
       2D axis() clip these, we play cheaper */
    while((min < range[0] - d_frac || range[1] + d_frac < max) && i < 20) {
	nint = ++i;
	min = range[0];
	max = range[1];
	GPretty(&min, &max, &nint);
    }
    axp[0] = min;
    axp[1] = max;
    axp[2] = nint;
    /* Do the following calculations for both ticktypes */
    switch (axisType) {
    case 0:
	u1[0] = min;
	u1[1] = y[Vertex[AxisStart[axis]][1]];
	u1[2] = z[Vertex[AxisStart[axis]][2]];
	break;
    case 1:
	u1[0] = x[Vertex[AxisStart[axis]][0]];
	u1[1] = min;
	u1[2] = z[Vertex[AxisStart[axis]][2]];
	break;
    case 2:
	u1[0] = x[Vertex[AxisStart[axis]][0]];
	u1[1] = y[Vertex[AxisStart[axis]][1]];
	u1[2] = min;
	break;
    }
    u1[0] = u1[0] + tickLength*(x[1]-x[0])*TickVector[axis][0];
    u1[1] = u1[1] + tickLength*(y[1]-y[0])*TickVector[axis][1];
    u1[2] = u1[2] + tickLength*(z[1]-z[0])*TickVector[axis][2];
    u1[3] = 1;
    switch (axisType) {
    case 0:
	u2[0] = max;
	u2[1] = u1[1];
	u2[2] = u1[2];
	break;
    case 1:
	u2[0] = u1[0];
	u2[1] = max;
	u2[2] = u1[2];
	break;
    case 2:
	u2[0] = u1[0];
	u2[1] = u1[1];
	u2[2] = max;
	break;
    }
    u2[3] = 1;
    /* The axis label has to be further out for "detailed" ticks
       in order to leave room for the tick labels */
    switch (tickType) {
    case 1: /* "simple": just an arrow parallel to axis, indicating direction
	       of increase */
	u3[0] = u1[0] + tickLength*(x[1]-x[0])*TickVector[axis][0];
	u3[1] = u1[1] + tickLength*(y[1]-y[0])*TickVector[axis][1];
	u3[2] = u1[2] + tickLength*(z[1]-z[0])*TickVector[axis][2];
	break;
    case 2:
	u3[0] = u1[0] + 2.5*tickLength*(x[1]-x[0])*TickVector[axis][0];
	u3[1] = u1[1] + 2.5*tickLength*(y[1]-y[0])*TickVector[axis][1];
	u3[2] = u1[2] + 2.5*tickLength*(z[1]-z[0])*TickVector[axis][2];
	break;
    }
    switch (axisType) {
    case 0:
	u3[0] = (min + max)/2;
	break;
    case 1:
	u3[1] = (min + max)/2;
	break;
    case 2:
	u3[2] = (min + max)/2;
	break;
    }
    u3[3] = 1;
    TransVector(u1, VT, v1);
    TransVector(u2, VT, v2);
    TransVector(u3, VT, v3);
    /* Draw axis label */
    /* change in 2.5.0 to use cex.lab and font.lab */
    gpptr(dd)->cex = gpptr(dd)->cexbase * gpptr(dd)->cexlab;
    gpptr(dd)->font = gpptr(dd)->fontlab;
    GText(v3[0]/v3[3], v3[1]/v3[3], USER, label, enc, .5, .5,
	  labelAngle(v1[0]/v1[3], v1[1]/v1[3], v2[0]/v2[3], v2[1]/v2[3]),
	  dd);
    /* Draw axis ticks */
    /* change in 2.5.0 to use cex.axis and font.axis */
    gpptr(dd)->cex = gpptr(dd)->cexbase * gpptr(dd)->cexaxis;
    gpptr(dd)->font = gpptr(dd)->fontaxis;
    switch (tickType) {
    case 1: /* "simple": just an arrow parallel to axis, indicating direction
	       of increase */
	/* arrow head is 0.25 inches long, with angle 30 degrees,
	   and drawn at v2 end of line */
	GArrow(v1[0]/v1[3], v1[1]/v1[3],
	       v2[0]/v2[3], v2[1]/v2[3], USER,
	       0.1, 10, 2, dd);
	break;
    case 2: /* "detailed": normal ticks as per 2D plots */
	PROTECT(at = CreateAtVector(axp, range, 7, FALSE));
	PROTECT(lab = labelformat(at));
	for (i=0; i<length(at); i++) {
	    switch (axisType) {
	    case 0:
		u1[0] = REAL(at)[i];
		u1[1] = y[Vertex[AxisStart[axis]][1]];
		u1[2] = z[Vertex[AxisStart[axis]][2]];
		break;
	    case 1:
		u1[0] = x[Vertex[AxisStart[axis]][0]];
		u1[1] = REAL(at)[i];
		u1[2] = z[Vertex[AxisStart[axis]][2]];
		break;
	    case 2:
		u1[0] = x[Vertex[AxisStart[axis]][0]];
		u1[1] = y[Vertex[AxisStart[axis]][1]];
		u1[2] = REAL(at)[i];
		break;
	    }
	    u1[3] = 1;
	    u2[0] = u1[0] + tickLength*(x[1]-x[0])*TickVector[axis][0];
	    u2[1] = u1[1] + tickLength*(y[1]-y[0])*TickVector[axis][1];
	    u2[2] = u1[2] + tickLength*(z[1]-z[0])*TickVector[axis][2];
	    u2[3] = 1;
	    u3[0] = u2[0] + tickLength*(x[1]-x[0])*TickVector[axis][0];
	    u3[1] = u2[1] + tickLength*(y[1]-y[0])*TickVector[axis][1];
	    u3[2] = u2[2] + tickLength*(z[1]-z[0])*TickVector[axis][2];
	    u3[3] = 1;
	    TransVector(u1, VT, v1);
	    TransVector(u2, VT, v2);
	    TransVector(u3, VT, v3);
	    /* Draw tick line */
	    GLine(v1[0]/v1[3], v1[1]/v1[3],
		  v2[0]/v2[3], v2[1]/v2[3], USER, dd);
	    /* Draw tick label */
	    GText(v3[0]/v3[3], v3[1]/v3[3], USER,
		  CHAR(STRING_ELT(lab, i)),
		  getCharCE(STRING_ELT(lab, i)),
		  .5, .5, 0, dd);
	}
	UNPROTECT(2);
	break;
    }
    gpptr(dd)->cex = cexsave;
    gpptr(dd)->font = fontsave;
}

/* Determine the transformed (x, y) coordinates (in USER space)
 * for the four corners of the x-y plane of the persp plot
 * These will be used to determine which sides of the persp
 * plot to label with axes
 * The strategy is to determine which corner has the lowest y-value
 * to decide which of the x- and y-axes to label AND which corner
 * has the lowest x-value to decide which of the z-axes to label
 */
static void PerspAxes(double *x, double *y, double *z,
		      const char *xlab, cetype_t xenc,
		      const char *ylab, cetype_t yenc,
		      const char *zlab, cetype_t zenc,
		      int nTicks, int tickType, pGEDevDesc dd)
{
    int xAxis=0, yAxis=0, zAxis=0; /* -Wall */
    int xpdsave;
    Vector3d u0, u1, u2, u3;
    Vector3d v0, v1, v2, v3;
    u0[0] = x[0];
    u0[1] = y[0];
    u0[2] = z[0];
    u0[3] = 1;
    u1[0] = x[1];
    u1[1] = y[0];
    u1[2] = z[0];
    u1[3] = 1;
    u2[0] = x[0];
    u2[1] = y[1];
    u2[2] = z[0];
    u2[3] = 1;
    u3[0] = x[1];
    u3[1] = y[1];
    u3[2] = z[0];
    u3[3] = 1;
    TransVector(u0, VT, v0);
    TransVector(u1, VT, v1);
    TransVector(u2, VT, v2);
    TransVector(u3, VT, v3);

    /* to fit in the axis labels */
    xpdsave = gpptr(dd)->xpd;
    gpptr(dd)->xpd = 1;

    /* Figure out which X and Y axis to draw */
    if (lowest(v0[1]/v0[3], v1[1]/v1[3], v2[1]/v2[3], v3[1]/v3[3])) {
	xAxis = 0;
	yAxis = 1;
    } else if (lowest(v1[1]/v1[3], v0[1]/v0[3], v2[1]/v2[3], v3[1]/v3[3])) {
	xAxis = 0;
	yAxis = 3;
    } else if (lowest(v2[1]/v2[3], v1[1]/v1[3], v0[1]/v0[3], v3[1]/v3[3])) {
	xAxis = 2;
	yAxis = 1;
    } else if (lowest(v3[1]/v3[3], v1[1]/v1[3], v2[1]/v2[3], v0[1]/v0[3])) {
	xAxis = 2;
	yAxis = 3;
    } else
	warning(_("Axis orientation not calculated"));
    PerspAxis(x, y, z, xAxis, 0, nTicks, tickType, xlab, xenc, dd);
    PerspAxis(x, y, z, yAxis, 1, nTicks, tickType, ylab, yenc, dd);
    /* Figure out which Z axis to draw */
    if (lowest(v0[0]/v0[3], v1[0]/v1[3], v2[0]/v2[3], v3[0]/v3[3])) {
	zAxis = 4;
    } else if (lowest(v1[0]/v1[3], v0[0]/v0[3], v2[0]/v2[3], v3[0]/v3[3])) {
	zAxis = 5;
    } else if (lowest(v2[0]/v2[3], v1[0]/v1[3], v0[0]/v0[3], v3[0]/v3[3])) {
	zAxis = 6;
    } else if (lowest(v3[0]/v3[3], v1[0]/v1[3], v2[0]/v2[3], v0[0]/v0[3])) {
	zAxis = 7;
    } else
	warning(_("Axis orientation not calculated"));
    PerspAxis(x, y, z, zAxis, 2, nTicks, tickType, zlab, zenc, dd);

    gpptr(dd)->xpd = xpdsave;
}

SEXP C_persp(SEXP args)
{
    SEXP x, y, z, xlim, ylim, zlim;
    SEXP depth, indx;
    SEXP col, border, xlab, ylab, zlab;
    double theta, phi, r, d;
    double ltheta, lphi;
    double expand, xc = 0.0, yc = 0.0, zc = 0.0, xs = 0.0, ys = 0.0, zs = 0.0;
    int i, j, scale, ncol, dobox, doaxes, nTicks, tickType;
    char EdgeDone[12]; /* Which edges have been drawn previously */
    pGEDevDesc dd;

    args = CDR(args);
    if (length(args) < 24)  /* 24 plus any inline par()s */
	error(_("too few parameters"));

    PROTECT(x = coerceVector(CAR(args), REALSXP));
    if (length(x) < 2) error(_("invalid '%s' argument"), "x");
    args = CDR(args);

    PROTECT(y = coerceVector(CAR(args), REALSXP));
    if (length(y) < 2) error(_("invalid '%s' argument"), "y");
    args = CDR(args);

    PROTECT(z = coerceVector(CAR(args), REALSXP));
    if (!isMatrix(z) || nrows(z) != length(x) || ncols(z) != length(y))
	error(_("invalid '%s' argument"), "z");
    args = CDR(args);

    PROTECT(xlim = coerceVector(CAR(args), REALSXP));
    if (length(xlim) != 2) error(_("invalid '%s' argument"), "xlim");
    args = CDR(args);

    PROTECT(ylim = coerceVector(CAR(args), REALSXP));
    if (length(ylim) != 2) error(_("invalid '%s' argument"), "ylim");
    args = CDR(args);

    PROTECT(zlim = coerceVector(CAR(args), REALSXP));
    if (length(zlim) != 2) error(_("invalid '%s' argument"), "zlim");
    args = CDR(args);

    /* Checks on x/y/z Limits */

    if (!LimitCheck(REAL(xlim), &xc, &xs))
	error(_("invalid 'x' limits"));
    if (!LimitCheck(REAL(ylim), &yc, &ys))
	error(_("invalid 'y' limits"));
    if (!LimitCheck(REAL(zlim), &zc, &zs))
	error(_("invalid 'z' limits"));

    theta = asReal(CAR(args));	args = CDR(args);
    phi	  = asReal(CAR(args));	args = CDR(args);
    r	= asReal(CAR(args));	args = CDR(args);
    d	= asReal(CAR(args));	args = CDR(args);
    scale  = asLogical(CAR(args)); args = CDR(args);
    expand = asReal(CAR(args)); args = CDR(args);
    col	   = CAR(args);		args = CDR(args);
    border = CAR(args);		args = CDR(args);
    ltheta = asReal(CAR(args)); args = CDR(args);
    lphi   = asReal(CAR(args)); args = CDR(args);
    Shade  = asReal(CAR(args)); args = CDR(args);
    dobox  = asLogical(CAR(args)); args = CDR(args);
    doaxes = asLogical(CAR(args)); args = CDR(args);
    nTicks = asInteger(CAR(args)); args = CDR(args);
    tickType = asInteger(CAR(args)); args = CDR(args);
    xlab = CAR(args); args = CDR(args);
    ylab = CAR(args); args = CDR(args);
    zlab = CAR(args); args = CDR(args);
    if (!isString(xlab) || length(xlab) < 1)
	error(_("'xlab' must be a character vector of length 1"));
    if (!isString(ylab) || length(ylab) < 1)
	error(_("'ylab' must be a character vector of length 1"));
    if (!isString(zlab) || length(zlab) < 1)
	error(_("'zlab' must be a character vector of length 1"));

    if (R_FINITE(Shade) && Shade <= 0) Shade = 1;
    if (R_FINITE(ltheta) && R_FINITE(lphi) && R_FINITE(Shade))
	DoLighting = TRUE;
    else
	DoLighting = FALSE;

    if (!scale) {
	double s;
	s = xs;
	if (s < ys) s = ys;
	if (s < zs) s = zs;
	xs = s; ys = s; zs = s;
    }

    /* Parameter Checks */

    if (!R_FINITE(theta) || !R_FINITE(phi) || !R_FINITE(r) || !R_FINITE(d) ||
	d < 0 || r < 0)
	error(_("invalid viewing parameters"));
    if (!R_FINITE(expand) || expand < 0)
	error(_("invalid '%s' value"), "expand");
    if (scale == NA_LOGICAL)
	scale = 0;
    if ((nTicks == NA_INTEGER) || (nTicks < 0))
	error(_("invalid '%s' value"), "nticks");
    if ((tickType == NA_INTEGER) || (tickType < 1) || (tickType > 2))
	error(_("invalid '%s' value"), "ticktype");

    dd = GEcurrentDevice();

#if 0
    GNewPlot(GRecording(call, dd));
#endif

    PROTECT(col = FixupCol(col, gpptr(dd)->bg));
    ncol = LENGTH(col);
    if (ncol < 1) error(_("invalid '%s' specification"), "col");
    if(!R_OPAQUE(INTEGER(col)[0])) DoLighting = FALSE;
    PROTECT(border = FixupCol(border, gpptr(dd)->fg));
    if (length(border) < 1)
	error(_("invalid '%s' specification"), "border");

    GSetState(1, dd);
    GSavePars(dd);
    ProcessInlinePars(args, dd);
    if (length(border) > 1)
	gpptr(dd)->fg = INTEGER(border)[0];
    gpptr(dd)->xlog = gpptr(dd)->ylog = FALSE;

    /* Set up the light vector (if any) */
    if (DoLighting)
	SetUpLight(ltheta, lphi);

    /* Mark box edges as undrawn */
    for (i = 0; i< 12; i++)
	EdgeDone[i] = 0;

    /* Specify the viewing transformation. */

    SetToIdentity(VT);             /* Initialization */
    Translate(-xc, -yc, -zc);      /* center at the origin */
    Scale(1/xs, 1/ys, expand/zs);  /* scale extents to [-1,1] */
    XRotate(-90.0);                /* rotate x-y plane to horizontal */
    YRotate(-theta);               /* azimuthal rotation */
    XRotate(phi);                  /* elevation rotation */
    Translate(0.0, 0.0, -r - d);   /* translate the eyepoint to the origin */
    Perspective(d);                /* perspective */

    /* Specify the plotting window. */
    /* Here we map the vertices of the cube */
    /* [xmin,xmax]*[ymin,ymax]*[zmin,zmax] */
    /* to the screen and then chose a window */
    /* which is symmetric about (0,0). */

    PerspWindow(REAL(xlim), REAL(ylim), REAL(zlim), dd);

    /* Compute facet order:
       We order the facets by depth and then draw them back to front.
       This is the "painters" algorithm. */

    PROTECT(depth = allocVector(REALSXP, (nrows(z) - 1)*(ncols(z) - 1)));
    PROTECT(indx = allocVector(INTSXP, (nrows(z) - 1)*(ncols(z) - 1)));
    DepthOrder(REAL(z), REAL(x), REAL(y), nrows(z), ncols(z),
	       REAL(depth), INTEGER(indx));

    GMode(1, dd);

    if (dobox) {
	/* Draw (solid) faces which face away from the viewer */
	PerspBox(0, REAL(xlim), REAL(ylim), REAL(zlim), EdgeDone, dd);
	if (doaxes) {
	    SEXP xl = STRING_ELT(xlab, 0), yl = STRING_ELT(ylab, 0),
		zl = STRING_ELT(zlab, 0);
	    PerspAxes(REAL(xlim), REAL(ylim), REAL(zlim),
		      (xl == NA_STRING) ? "" : CHAR(xl), getCharCE(xl),
		      (yl == NA_STRING) ? "" : CHAR(yl), getCharCE(yl),
		      (zl == NA_STRING) ? "" : CHAR(zl), getCharCE(zl),
		      nTicks, tickType, dd);
	}
    }

    DrawFacets(REAL(z), REAL(x), REAL(y), nrows(z), ncols(z), INTEGER(indx),
	       1/xs, 1/ys, expand/zs,
	       INTEGER(col), ncol, INTEGER(border)[0]);

    /* Draw (dotted) not-already-plotted edges of faces which face
       towards from the viewer */
    if (dobox)
	PerspBox(1, REAL(xlim), REAL(ylim), REAL(zlim), EdgeDone, dd);
    GMode(0, dd);

    GRestorePars(dd);
    UNPROTECT(10);

    PROTECT(x = allocVector(REALSXP, 16));
    PROTECT(y = allocVector(INTSXP, 2));
    for (i = 0; i < 4; i++)
	for (j = 0; j < 4; j++)
	    REAL(x)[i + j * 4] = VT[i][j];
    INTEGER(y)[0] = 4;
    INTEGER(y)[1] = 4;
    setAttrib(x, R_DimSymbol, y);
    UNPROTECT(2);
    return x;
}

/* in src/main */
#include "contour-common.h"

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

/* labelList, label1, and label2 are all SEXPs rather than being allocated
   using R_alloc because they need to persist across calls to contour().
   In do_contour() there is a vmaxget() ... vmaxset() around each call to
   contour() to release all of the memory used in the drawing of the
   contour _lines_ at each contour level.  We need to keep track of the
   contour _labels_ for _all_ contour levels, hence we have to use a
   different memory allocation mechanism.
*/

static
double distFromEdge(double *xxx, double *yyy, int iii, pGEDevDesc dd) {
    return fmin2(fmin2(xxx[iii]-gpptr(dd)->usr[0], gpptr(dd)->usr[1]-xxx[iii]),
		 fmin2(yyy[iii]-gpptr(dd)->usr[2], gpptr(dd)->usr[3]-yyy[iii]));
}

static SEXP labelList;
static SEGP *ctr_SegDB;

static
Rboolean useStart(double *xxx, double *yyy, int ns, pGEDevDesc dd) {
    if (distFromEdge(xxx, yyy, 0, dd) < distFromEdge(xxx, yyy, ns-1, dd))
	return TRUE;
    else
	return FALSE;
}


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
    /* This R-allocs ctr_SegDB */
    ctr_SegDB = contourLines(REAL(x), nx, REAL(y), ny, REAL(z), zc, atom);

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

	    vmaxset(vmax);
	} /* while */
      } /* for(i .. )  for(j ..) */
    vmaxset(vmax); /* now we are done with ctr_SegDB */
    UNPROTECT_PTR(label1); /* pwwwargh! This is messy, but last thing
			      protected is likely labelList, and that needs
			      to be preserved across calls */
}


SEXP C_contourDef(void)
{
    return ScalarLogical(GEcurrentDevice()->dev->useRotatedTextInContour);
}

/* contour(x, y, z, levels, labels, labcex, drawlabels,
 *         method, vfont, col = col, lty = lty, lwd = lwd)
 */
SEXP C_contour(SEXP args)
{
    SEXP c, x, y, z, vfont, col, rawcol, lty, lwd, labels;
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

    args = CDR(args);
    if (length(args) < 12) error(_("too few arguments"));
    PrintDefaults(); /* prepare for labelformat */

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

    labels = CAR(args);
    if (!isNull(labels)) TypeCheck(labels, STRSXP);
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
	strncpy(gpptr(dd)->family, "Hershey ", 201);
	gpptr(dd)->family[7] = (char) INTEGER(vfont)[0];
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
	UNPROTECT(8);
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
    UNPROTECT(9); /* x y z c vfont col lty lwd labellist */
    return result;
}
