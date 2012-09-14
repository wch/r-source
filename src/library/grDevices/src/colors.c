/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997-2012  The R Core Team
 *  Copyright (C) 2003	     The R Foundation
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

/* This should be regarded as part of the graphics engine */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Graphics.h>  /* for dpptr */
#include <Colors.h>
#include <R_ext/GraphicsEngine.h>
#include <Rmath.h>
#include <ctype.h> /* for tolower, isdigit */

static char ColBuf[10];
static char HexDigits[] = "0123456789ABCDEF";

static
char *RGB2rgb(unsigned int r, unsigned int g, unsigned int b)
{
    ColBuf[0] = '#';
    ColBuf[1] = HexDigits[(r >> 4) & 15];
    ColBuf[2] = HexDigits[r & 15];
    ColBuf[3] = HexDigits[(g >> 4) & 15];
    ColBuf[4] = HexDigits[g & 15];
    ColBuf[5] = HexDigits[(b >> 4) & 15];
    ColBuf[6] = HexDigits[b & 15];
    ColBuf[7] = '\0';
    return &ColBuf[0];
}

static
char *RGBA2rgb(unsigned int r, unsigned int g, unsigned int b, unsigned int a)
{
    ColBuf[0] = '#';
    ColBuf[1] = HexDigits[(r >> 4) & 15];
    ColBuf[2] = HexDigits[r & 15];
    ColBuf[3] = HexDigits[(g >> 4) & 15];
    ColBuf[4] = HexDigits[g & 15];
    ColBuf[5] = HexDigits[(b >> 4) & 15];
    ColBuf[6] = HexDigits[b & 15];
    ColBuf[7] = HexDigits[(a >> 4) & 15];
    ColBuf[8] = HexDigits[a & 15];
    ColBuf[9] = '\0';
    return &ColBuf[0];
}


static unsigned int ScaleColor(double x)
{
    if (!R_FINITE(x) || x < 0.0 || x > 1.0)
	error(_("color intensity %g, not in [0,1]"), x);
    return (unsigned int)(255*x + 0.5);
}

static unsigned int CheckColor(int x)
{
    if (x == NA_INTEGER || x < 0 || x > 255)
	error(_("color intensity %d, not in 0:255"), x);
    return (unsigned int)x;
}

static unsigned int ScaleAlpha(double x)
{
    if (!R_FINITE(x) || x < 0.0 || x > 1.0)
	error(_("alpha level %g, not in [0,1]"), x);
    return (unsigned int)(255*x + 0.5);
}

static unsigned int CheckAlpha(int x)
{
    if (x == NA_INTEGER || x < 0 || x > 255)
	error(_("alpha level %d, not in 0:255"), x);
    return (unsigned int)x;
}


SEXP hsv(SEXP h, SEXP s, SEXP v, SEXP a)
{
    double hh, ss, vv, aa, r=0., g=0., b=0.; /* -Wall */
    R_xlen_t i, max, nh, ns, nv, na = 1;

    PROTECT(h = coerceVector(h,REALSXP));
    PROTECT(s = coerceVector(s,REALSXP));
    PROTECT(v = coerceVector(v,REALSXP));
    if (!isNull(a)) {
	a = coerceVector(a, REALSXP);
	na = XLENGTH(a);
    }
    PROTECT(a);

    nh = XLENGTH(h);
    ns = XLENGTH(s);
    nv = XLENGTH(v);
    if (nh <= 0 || ns <= 0 || nv <= 0 || na <= 0) {
	UNPROTECT(4);
	return allocVector(STRSXP, 0);
    }
    max = nh;
    if (max < ns) max = ns;
    if (max < nv) max = nv;
    if (max < na) max = na;
    SEXP c = PROTECT(allocVector(STRSXP, max));
    if(max == 0) return(c);

    if(isNull(a)) {
	for (i = 0; i < max; i++) {
	hh = REAL(h)[i % nh];
	ss = REAL(s)[i % ns];
	vv = REAL(v)[i % nv];
	if (hh < 0 || hh > 1 || ss < 0 || ss > 1 || vv < 0 || vv > 1)
	    error(_("invalid hsv color"));
	hsv2rgb(hh, ss, vv, &r, &g, &b);
	SET_STRING_ELT(c, i, mkChar(RGB2rgb(ScaleColor(r), ScaleColor(g),
					    ScaleColor(b))));

	}
    } else {
	for (i = 0; i < max; i++) {
	hh = REAL(h)[i % nh];
	ss = REAL(s)[i % ns];
	vv = REAL(v)[i % nv];
	aa = REAL(a)[i % na];
	if (hh < 0 || hh > 1 || ss < 0 || ss > 1 || vv < 0 || vv > 1 ||
	    aa < 0 || aa > 1)
	    error(_("invalid hsv color"));
	hsv2rgb(hh, ss, vv, &r, &g, &b);
	SET_STRING_ELT(c, i, mkChar(RGBA2rgb(ScaleColor(r), ScaleColor(g),
					     ScaleColor(b), ScaleAlpha(aa))));
	}
    }
    UNPROTECT(5);
    return c;
}

/* D65 White Point */

#define WHITE_X 95.047
#define WHITE_Y 100.000
#define WHITE_Z 108.883
#define WHITE_u 0.1978398
#define WHITE_v 0.4683363

/* Standard CRT Gamma */

#define GAMMA 2.4

static double gtrans(double u)
{
    if (u > 0.00304)
	return 1.055 * pow(u, (1 / GAMMA)) - 0.055;
    else
	return 12.92 * u;
}

static int FixupColor(int *r, int *g, int *b)
{
    int fix = 0;
    if (*r < 0) { *r = 0; fix = 1; } else if (*r > 255) { *r = 255; fix = 1; }
    if (*g < 0) { *g = 0; fix = 1; } else if (*g > 255) { *g = 255; fix = 1; }
    if (*b < 0) { *b = 0; fix = 1; } else if (*b > 255) { *b = 255; fix = 1; }
    return fix;
}

static void
hcl2rgb(double h, double c, double l, double *R, double *G, double *B)
{
    double L, U, V;
    double u, v;
    double X, Y, Z;

    /* Step 1 : Convert to CIE-LUV */

    h = DEG2RAD * h;
    L = l;
    U = c * cos(h);
    V = c * sin(h);

    /* Step 2 : Convert to CIE-XYZ */

    if (L <= 0 && U == 0 && V == 0) {
	X = 0; Y = 0; Z = 0;
    }
    else {
	Y = WHITE_Y * ((L > 7.999592) ? pow((L + 16)/116, 3) : L / 903.3);
	u = U / (13 * L) + WHITE_u;
	v = V / (13 * L) + WHITE_v;
	X =  9.0 * Y * u / (4 * v);
	Z =  - X / 3 - 5 * Y + 3 * Y / v;
    }

    /* Step 4 : CIE-XYZ to sRGB */

    *R = gtrans(( 3.240479 * X - 1.537150 * Y - 0.498535 * Z) / WHITE_Y);
    *G = gtrans((-0.969256 * X + 1.875992 * Y + 0.041556 * Z) / WHITE_Y);
    *B = gtrans(( 0.055648 * X - 0.204043 * Y + 1.057311 * Z) / WHITE_Y);
}

SEXP hcl(SEXP h, SEXP c, SEXP l, SEXP a, SEXP sfixup)
{
    double H, C, L, A, r, g, b;
    R_xlen_t nh, nc, nl, na = 1, max, i;
    int ir, ig, ib;
    int fixup;

    PROTECT(h = coerceVector(h,REALSXP));
    PROTECT(c = coerceVector(c,REALSXP));
    PROTECT(l = coerceVector(l,REALSXP));
    if (!isNull(a)) {
	a = coerceVector(a,REALSXP);
	na = XLENGTH(a);
    }
    PROTECT(a);
    fixup = asLogical(sfixup);
    nh = XLENGTH(h);
    nc = XLENGTH(c);
    nl = XLENGTH(l);
    if (nh <= 0 || nc <= 0 || nl <= 0 || na <= 0) {
	UNPROTECT(4);
	return(allocVector(STRSXP, 0));
    }
    max = nh;
    if (max < nc) max = nc;
    if (max < nl) max = nl;
    if (max < na) max = na;
    SEXP ans = PROTECT(allocVector(STRSXP, max));
    if (isNull(a)) {
	for (i = 0; i < max; i++) {
	    H = REAL(h)[i % nh];
	    C = REAL(c)[i % nc];
	    L = REAL(l)[i % nl];
	    if (L < 0 || L > WHITE_Y || C < 0) error(_("invalid hcl color"));
	    hcl2rgb(H, C, L, &r, &g, &b);
	    ir = (int) (255 * r + .5);
	    ig = (int) (255 * g + .5);
	    ib = (int) (255 * b + .5);
	    if (FixupColor(&ir, &ig, &ib) && !fixup)
		SET_STRING_ELT(ans, i, NA_STRING);
	    else
		SET_STRING_ELT(ans, i, mkChar(RGB2rgb(ir, ig, ib)));
	}
    } else {
	for (i = 0; i < max; i++) {
	    H = REAL(h)[i % nh];
	    C = REAL(c)[i % nc];
	    L = REAL(l)[i % nl];
	    A = REAL(a)[i % na];
	    if (!R_FINITE(A)) A = 1;
	    if (L < 0 || L > WHITE_Y || C < 0 || A < 0 || A > 1)
		error(_("invalid hcl color"));
	    hcl2rgb(H, C, L, &r, &g, &b);
	    ir = (int) (255 * r + .5);
	    ig = (int) (255 * g + .5);
	    ib = (int) (255 * b + .5);
	    if (FixupColor(&ir, &ig, &ib) && !fixup)
		SET_STRING_ELT(ans, i, NA_STRING);
	    else
		SET_STRING_ELT(ans, i, mkChar(RGBA2rgb(ir, ig, ib,
						       ScaleAlpha(A))));
	}
    }
    UNPROTECT(5);
    return ans;
}

#define _R_set_c_RGB(_R,_G,_B) \
    { for (i = 0; i < l_max; i++)  \
	    SET_STRING_ELT(c, i, mkChar(RGB2rgb(_R,_G,_B))); }

#define _R_set_c_RGBA(_R,_G,_B,_A) \
    { for (i = 0; i < l_max; i++)  \
	    SET_STRING_ELT(c, i, mkChar(RGBA2rgb(_R,_G,_B,_A))); }

SEXP rgb(SEXP r, SEXP g, SEXP b, SEXP a, SEXP MCV, SEXP nam)
{
    R_xlen_t i, l_max, nr, ng, nb, na = 1;
    Rboolean max_1 = FALSE;
    double mV = asReal(MCV);

    if(mV == 255.) {
	PROTECT(r = coerceVector(r, INTSXP));
	PROTECT(g = coerceVector(g, INTSXP));
	PROTECT(b = coerceVector(b, INTSXP));
	if(!isNull(a)) a = coerceVector(a, INTSXP);
    } else {
	PROTECT(r = coerceVector(r, REALSXP));
	PROTECT(g = coerceVector(g, REALSXP));
	PROTECT(b = coerceVector(b, REALSXP));
	if(!isNull(a)) a = coerceVector(a, REALSXP);
	max_1 = (mV == 1.);
    }
    PROTECT(a);

    nr = XLENGTH(r); ng = XLENGTH(g); nb = XLENGTH(b); 
    if (!isNull(a)) na = XLENGTH(a);
    if (nr <= 0 || ng <= 0 || nb <= 0 || na <= 0) {
	UNPROTECT(4);
	return allocVector(STRSXP, 0);
    }
    l_max = nr;
    if (l_max < ng) l_max = ng;
    if (l_max < nb) l_max = nb;
    if (l_max < na) l_max = na;

    PROTECT(nam = coerceVector(nam, STRSXP));
    if (length(nam) != 0 && length(nam) != l_max)
	error(_("invalid 'names' vector"));
    SEXP c = PROTECT(allocVector(STRSXP, l_max));

    if(mV == 255.0) {
	if(isNull(a)) {
	    _R_set_c_RGB(CheckColor(INTEGER(r)[i%nr]),
			  CheckColor(INTEGER(g)[i%ng]),
			  CheckColor(INTEGER(b)[i%nb]));
	} else {
	    _R_set_c_RGBA(CheckColor(INTEGER(r)[i%nr]),
			  CheckColor(INTEGER(g)[i%ng]),
			  CheckColor(INTEGER(b)[i%nb]),
			  CheckAlpha(INTEGER(a)[i%na]));
	}
    }
    else if(max_1) {
	if(isNull(a)) {
	    _R_set_c_RGB(ScaleColor(REAL(r)[i%nr]),
			  ScaleColor(REAL(g)[i%ng]),
			 ScaleColor(REAL(b)[i%nb]));
	} else {
	    _R_set_c_RGBA(ScaleColor(REAL(r)[i%nr]),
			  ScaleColor(REAL(g)[i%ng]),
			  ScaleColor(REAL(b)[i%nb]),
			  ScaleAlpha(REAL(a)[i%na]));
	}
    }
    else { /* maxColorVal not in {1, 255} */
	if(isNull(a)) {
	    _R_set_c_RGB(ScaleColor(REAL(r)[i%nr] / mV),
			  ScaleColor(REAL(g)[i%ng] / mV),
			 ScaleColor(REAL(b)[i%nb] / mV));
	} else {
	    _R_set_c_RGBA(ScaleColor(REAL(r)[i%nr] / mV),
			  ScaleColor(REAL(g)[i%ng] / mV),
			  ScaleColor(REAL(b)[i%nb] / mV),
			  ScaleAlpha(REAL(a)[i%na] / mV));
	}
    }
    if (length(nam) != 0) setAttrib(c, R_NamesSymbol, nam);
    UNPROTECT(6);
    return c;
}

SEXP gray(SEXP lev, SEXP a)
{
    SEXP ans;
    double level;
    int i, ilevel, nlev;

    lev  = PROTECT(coerceVector(lev,REALSXP));
    if(!isNull(a)) a = coerceVector(a,REALSXP);
    PROTECT(a);
    nlev = LENGTH(lev);
    PROTECT(ans = allocVector(STRSXP, nlev));
    if(isNull(a)) {
	for (i = 0; i < nlev; i++) {
	    level = REAL(lev)[i];
	    if (ISNAN(level) || level < 0 || level > 1)
		error(_("invalid gray level, must be in [0,1]."));
	    ilevel = (int)(255 * level + 0.5);
	    SET_STRING_ELT(ans, i, mkChar(RGB2rgb(ilevel, ilevel, ilevel)));
	}
    } else {
	int na = length(a);
	for (i = 0; i < (nlev > na ? nlev : na); i++) {
	    level = REAL(lev)[i % nlev];
	    if (ISNAN(level) || level < 0 || level > 1)
		error(_("invalid gray level, must be in [0,1]."));
	    ilevel = (int)(255 * level + 0.5);
	    double aa = REAL(a)[i % na];
	    SET_STRING_ELT(ans, i, mkChar(RGBA2rgb(ilevel, ilevel, ilevel,
						   ScaleAlpha(aa))));
	}
    }
    UNPROTECT(3);
    return ans;
}

SEXP RGB2hsv(SEXP rgb)
{
/* (r,g,b) -> (h,s,v) conversion */
    SEXP dd, ans, names, dmns;
    int n, i, i3;

    rgb = PROTECT(coerceVector(rgb, REALSXP));
    if(!isMatrix(rgb)) error("rgb is not a matrix (internally)");
    dd = getAttrib(rgb, R_DimSymbol);
    if(INTEGER(dd)[0] != 3) error("rgb must have 3 rows (internally)");
    n = INTEGER(dd)[1];

    ans = PROTECT(allocMatrix(REALSXP, 3, n));
    PROTECT(dmns = allocVector(VECSXP, 2));
    /* row names: */
    PROTECT(names = allocVector(STRSXP, 3));
    SET_STRING_ELT(names, 0, mkChar("h"));
    SET_STRING_ELT(names, 1, mkChar("s"));
    SET_STRING_ELT(names, 2, mkChar("v"));
    SET_VECTOR_ELT(dmns, 0, names);
    /* column names if input has: */
    if ((dd = getAttrib(rgb, R_DimNamesSymbol)) != R_NilValue &&
	(names = VECTOR_ELT(dd, 1)) != R_NilValue)
	SET_VECTOR_ELT(dmns, 1, names);
    setAttrib(ans, R_DimNamesSymbol, dmns);
    UNPROTECT(2);/* names, dmns */

    for(i = i3 = 0; i < n; i++, i3 += 3) {
	rgb2hsv(REAL(rgb)[i3+ 0],  REAL(rgb)[i3+ 1],  REAL(rgb)[i3+ 2],
		&REAL(ans)[i3+ 0], &REAL(ans)[i3+ 1], &REAL(ans)[i3 +2]);
    }
    UNPROTECT(2);
    return ans;
}
