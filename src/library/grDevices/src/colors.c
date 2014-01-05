/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997-2013  The R Core Team
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
#include <R_ext/GraphicsEngine.h>

#include "grDevices.h"

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

/* hsv2rgb -- HSV to RGB conversion  */
/* Based on HSV_TO_RGB from Foley and Van Dam First Ed. Page 616 */
/* See Alvy Ray Smith, Color Gamut Transform Pairs, SIGGRAPH '78 */

static void hsv2rgb(double h, double s, double v, 
		    double *r, double *g, double *b)
{
    double f, p, q, t;
    int i;

    f = modf(h * 6.0, &t);
    i = ((int) t) % 6;

    p = v * (1 - s);
    q = v * (1 - s * f);
    t = v * (1 - (s * (1 - f)));
    switch (i) {
    case 0:	*r = v;		*g = t;		*b = p;	break;
    case 1:	*r = q;		*g = v;		*b = p;	break;
    case 2:	*r = p;		*g = v;		*b = t;	break;
    case 3:	*r = p;		*g = q;		*b = v; break;
    case 4:	*r = t;		*g = p;		*b = v; break;
    case 5:	*r = v;		*g = p;		*b = q;	break;
    default:
	error(_("bad hsv to rgb color conversion"));
    }
}

/* rgb2hsv() -- the reverse (same reference as above)
 *	this implementation is adapted from code by Nicholas Lewin-Koh.
 */
static void rgb2hsv(double r, double g, double b,
		    double *h, double *s, double *v)
    /* all (r,g,b, h,s,v) values in [0,1] */
{
    double min, max, delta;
    Rboolean r_max = TRUE, b_max = FALSE;
    /* Compute  min(r,g,b) and max(r,g,b) and remember where max is: */
    min = max = r;
    if(min > g) { /* g < r */
	if(b < g)
	    min = b;/* &  max = r */
	else { /* g <= b, g < r */
	    min = g;
	    if(b > r) { max = b; b_max = TRUE; r_max = FALSE; }
	    /* else : g <= b <=r */
	}
    } else { /* r <= g */
	if(b > g) {
	    max = b; b_max = TRUE; r_max = FALSE; /* &  min = r */
	} else { /* b,r <= g */
	    max = g; r_max = FALSE; /* &  min = r */
	    if(b < r) min = b; /* else : r <= b <= g */
	}
    }

    *v = max;
    if( max == 0 || (delta = max - min) == 0) {
	/*   r = g = b : "gray" : s = h = 0 */
	*s = *h = 0;
	return;
    }
    /* else : */
    *s = delta / max;

    if(r_max)
	*h =     ( g - b ) / delta; /* between yellow & magenta */
    else if(b_max)
	*h = 4 + ( r - g ) / delta; /* between magenta & cyan */
    else /* g == max */
	*h = 2 + ( b - r ) / delta; /* between cyan & yellow*/

    *h /= 6;
    if(*h < 0)
	*h += 1.;
    return;
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
    if (l <= 0.0) {
	*R = *G = *B = 0.0;
	return;
    }
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

// People call this with non-finite inputs.
SEXP hcl(SEXP h, SEXP c, SEXP l, SEXP a, SEXP sfixup)
{
    double H, C, L, A, r, g, b;
    R_xlen_t nh, nc, nl, na = 1, max, i;
    int ir, ig, ib;
    int fixup;

    PROTECT(h = coerceVector(h, REALSXP));
    PROTECT(c = coerceVector(c, REALSXP));
    PROTECT(l = coerceVector(l, REALSXP));
    if (!isNull(a)) {
	a = coerceVector(a, REALSXP);
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
	    if (R_FINITE(H) && R_FINITE(C) && R_FINITE(L)) {
		if (L < 0 || L > WHITE_Y || C < 0) error(_("invalid hcl color"));
		hcl2rgb(H, C, L, &r, &g, &b);
		ir = (int) (255 * r + .5);
		ig = (int) (255 * g + .5);
		ib = (int) (255 * b + .5);
		if (FixupColor(&ir, &ig, &ib) && !fixup)
		    SET_STRING_ELT(ans, i, NA_STRING);
		else
		    SET_STRING_ELT(ans, i, mkChar(RGB2rgb(ir, ig, ib)));
	    } else SET_STRING_ELT(ans, i, NA_STRING);
	}
    } else {
	for (i = 0; i < max; i++) {
	    H = REAL(h)[i % nh];
	    C = REAL(c)[i % nc];
	    L = REAL(l)[i % nl];
	    A = REAL(a)[i % na];
	    if (!R_FINITE(A)) A = 1;
	    if (R_FINITE(H) && R_FINITE(C) && R_FINITE(L)) {
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
	    } else SET_STRING_ELT(ans, i, NA_STRING);
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

    if(!R_FINITE(mV) || mV == 0.)
	error(_("invalid value of 'maxColorValue'"));
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


SEXP col2rgb(SEXP colors, SEXP alpha)
{
    SEXP ans, names, dmns;

    int alph = asLogical(alpha);
    if(alph == NA_LOGICAL) error(_("invalid '%s' value"), "alpha");
    switch(TYPEOF(colors)) {
    case INTSXP:
    case STRSXP:
	break;
    case REALSXP:
	colors = coerceVector(colors, INTSXP);
	break;
    default:
	colors = coerceVector(colors, STRSXP);
	break;
    }
    PROTECT(colors);
    int n = LENGTH(colors);

    /* First set up the output matrix */
    PROTECT(ans = allocMatrix(INTSXP, 3+alph, n));
    PROTECT(dmns = allocVector(VECSXP, 2));
    PROTECT(names = allocVector(STRSXP, 3+alph));
    SET_STRING_ELT(names, 0, mkChar("red"));
    SET_STRING_ELT(names, 1, mkChar("green"));
    SET_STRING_ELT(names, 2, mkChar("blue"));
    if(alph) SET_STRING_ELT(names, 3, mkChar("alpha"));
    SET_VECTOR_ELT(dmns, 0, names);
    if ((names = getAttrib(colors, R_NamesSymbol)) != R_NilValue)
	SET_VECTOR_ELT(dmns, 1, names);
    setAttrib(ans, R_DimNamesSymbol, dmns);

    for(int i = 0, j = 0; i < n; i++) {
	rcolor icol = inRGBpar3(colors, i, R_TRANWHITE);
	INTEGER(ans)[j++] = R_RED(icol);
	INTEGER(ans)[j++] = R_GREEN(icol);
	INTEGER(ans)[j++] = R_BLUE(icol);
	if(alph) INTEGER(ans)[j++] = R_ALPHA(icol);
    }
    UNPROTECT(4);
    return ans;
}


// ------------------ code for tables to export to main executable --------

#include <ctype.h> /* for tolower, isdigit */

#define MAX_PALETTE_SIZE 1024
static int PaletteSize = 8;
static rcolor Palette[MAX_PALETTE_SIZE] = {
    0xff000000,
    0xff0000ff,
    0xff00cd00,
    0xffff0000,
    0xffffff00,
    0xffff00ff,
    0xff00ffff,
    0xffbebebe
};

static rcolor Palette0[MAX_PALETTE_SIZE];


/* String comparison ignoring case and squeezing out blanks */
static int StrMatch(const char *s, const char *t)
{
    for(;;) {
	if(*s == '\0' && *t == '\0') return 1;
	if(*s == ' ') { s++; continue; }
	if(*t == ' ') { t++; continue; }
	if(tolower(*s++) != tolower(*t++)) return 0;
    }
}


/*
 *  Color Specification
 *
 *  Colors are stored internally in integers.  Each integer is
 *  broken into four bytes.	 The three least significant bytes
 *  are used to contain levels of red, green and blue.	These
 *  levels are integers in the range [0,255].
 *
 *  Externally, colors are specified either:
 *
 *    a) by name, using a large table of color names,
 *
 *    b) by RGB values using a string of the form "#rrggbb"
 *	 where rr, gg and bb are hex integers giving the level
 *	 of red green and blue,
 *
 *    c) as an index into a user setable palette of colors.
 *
 */

/* Default Color Palette */
/* Paul Murrell 05/06/02 (2002, probably)
 * Changed "white" to "grey" in the default palette
 * in response to user suggestion
 */
attribute_hidden
const char *DefaultPalette[] = {
    "black",
    "red",
    "green3",
    "blue",
    "cyan",
    "magenta",
    "yellow",
    "grey",
    NULL
};

/* The Table of Known Color Names */
/* Adapted from the X11 RGB database */
/* Note: the color "white" was moved to the top of the database 
   to avoid its being looked up by col2name as "gray100" */

typedef
struct colorDataBaseEntry {
    char *name;	  // X11 Color Name
    char *rgb;	  // #RRGGBB String, no longer used
    rcolor code;  // Internal R Color Code
} ColorDataBaseEntry;

static ColorDataBaseEntry ColorDataBase[] = {
    /* name		rgb         code */
    {"white",	"#FFFFFF",   0xffffffff},
    {"aliceblue",	"#F0F8FF",   0xfffff8f0},
    {"antiquewhite",	"#FAEBD7",   0xffd7ebfa},
    {"antiquewhite1",	"#FFEFDB",   0xffdbefff},
    {"antiquewhite2",	"#EEDFCC",   0xffccdfee},
    {"antiquewhite3",	"#CDC0B0",   0xffb0c0cd},
    {"antiquewhite4",	"#8B8378",   0xff78838b},
    {"aquamarine",	"#7FFFD4",   0xffd4ff7f},
    {"aquamarine1",	"#7FFFD4",   0xffd4ff7f},
    {"aquamarine2",	"#76EEC6",   0xffc6ee76},
    {"aquamarine3",	"#66CDAA",   0xffaacd66},
    {"aquamarine4",	"#458B74",   0xff748b45},
    {"azure",	"#F0FFFF",   0xfffffff0},
    {"azure1",	"#F0FFFF",   0xfffffff0},
    {"azure2",	"#E0EEEE",   0xffeeeee0},
    {"azure3",	"#C1CDCD",   0xffcdcdc1},
    {"azure4",	"#838B8B",   0xff8b8b83},
    {"beige",	"#F5F5DC",   0xffdcf5f5},
    {"bisque",	"#FFE4C4",   0xffc4e4ff},
    {"bisque1",	"#FFE4C4",   0xffc4e4ff},
    {"bisque2",	"#EED5B7",   0xffb7d5ee},
    {"bisque3",	"#CDB79E",   0xff9eb7cd},
    {"bisque4",	"#8B7D6B",   0xff6b7d8b},
    {"black",	"#000000",   0xff000000},
    {"blanchedalmond",	"#FFEBCD",   0xffcdebff},
    {"blue",	"#0000FF",   0xffff0000},
    {"blue1",	"#0000FF",   0xffff0000},
    {"blue2",	"#0000EE",   0xffee0000},
    {"blue3",	"#0000CD",   0xffcd0000},
    {"blue4",	"#00008B",   0xff8b0000},
    {"blueviolet",	"#8A2BE2",   0xffe22b8a},
    {"brown",	"#A52A2A",   0xff2a2aa5},
    {"brown1",	"#FF4040",   0xff4040ff},
    {"brown2",	"#EE3B3B",   0xff3b3bee},
    {"brown3",	"#CD3333",   0xff3333cd},
    {"brown4",	"#8B2323",   0xff23238b},
    {"burlywood",	"#DEB887",   0xff87b8de},
    {"burlywood1",	"#FFD39B",   0xff9bd3ff},
    {"burlywood2",	"#EEC591",   0xff91c5ee},
    {"burlywood3",	"#CDAA7D",   0xff7daacd},
    {"burlywood4",	"#8B7355",   0xff55738b},
    {"cadetblue",	"#5F9EA0",   0xffa09e5f},
    {"cadetblue1",	"#98F5FF",   0xfffff598},
    {"cadetblue2",	"#8EE5EE",   0xffeee58e},
    {"cadetblue3",	"#7AC5CD",   0xffcdc57a},
    {"cadetblue4",	"#53868B",   0xff8b8653},
    {"chartreuse",	"#7FFF00",   0xff00ff7f},
    {"chartreuse1",	"#7FFF00",   0xff00ff7f},
    {"chartreuse2",	"#76EE00",   0xff00ee76},
    {"chartreuse3",	"#66CD00",   0xff00cd66},
    {"chartreuse4",	"#458B00",   0xff008b45},
    {"chocolate",	"#D2691E",   0xff1e69d2},
    {"chocolate1",	"#FF7F24",   0xff247fff},
    {"chocolate2",	"#EE7621",   0xff2176ee},
    {"chocolate3",	"#CD661D",   0xff1d66cd},
    {"chocolate4",	"#8B4513",   0xff13458b},
    {"coral",	"#FF7F50",   0xff507fff},
    {"coral1",	"#FF7256",   0xff5672ff},
    {"coral2",	"#EE6A50",   0xff506aee},
    {"coral3",	"#CD5B45",   0xff455bcd},
    {"coral4",	"#8B3E2F",   0xff2f3e8b},
    {"cornflowerblue",	"#6495ED",   0xffed9564},
    {"cornsilk",	"#FFF8DC",   0xffdcf8ff},
    {"cornsilk1",	"#FFF8DC",   0xffdcf8ff},
    {"cornsilk2",	"#EEE8CD",   0xffcde8ee},
    {"cornsilk3",	"#CDC8B1",   0xffb1c8cd},
    {"cornsilk4",	"#8B8878",   0xff78888b},
    {"cyan",	"#00FFFF",   0xffffff00},
    {"cyan1",	"#00FFFF",   0xffffff00},
    {"cyan2",	"#00EEEE",   0xffeeee00},
    {"cyan3",	"#00CDCD",   0xffcdcd00},
    {"cyan4",	"#008B8B",   0xff8b8b00},
    {"darkblue",	"#00008B",   0xff8b0000},
    {"darkcyan",	"#008B8B",   0xff8b8b00},
    {"darkgoldenrod",	"#B8860B",   0xff0b86b8},
    {"darkgoldenrod1",	"#FFB90F",   0xff0fb9ff},
    {"darkgoldenrod2",	"#EEAD0E",   0xff0eadee},
    {"darkgoldenrod3",	"#CD950C",   0xff0c95cd},
    {"darkgoldenrod4",	"#8B6508",   0xff08658b},
    {"darkgray",	"#A9A9A9",   0xffa9a9a9},
    {"darkgreen",	"#006400",   0xff006400},
    {"darkgrey",	"#A9A9A9",   0xffa9a9a9},
    {"darkkhaki",	"#BDB76B",   0xff6bb7bd},
    {"darkmagenta",	"#8B008B",   0xff8b008b},
    {"darkolivegreen",	"#556B2F",   0xff2f6b55},
    {"darkolivegreen1",	"#CAFF70",   0xff70ffca},
    {"darkolivegreen2",	"#BCEE68",   0xff68eebc},
    {"darkolivegreen3",	"#A2CD5A",   0xff5acda2},
    {"darkolivegreen4",	"#6E8B3D",   0xff3d8b6e},
    {"darkorange",	"#FF8C00",   0xff008cff},
    {"darkorange1",	"#FF7F00",   0xff007fff},
    {"darkorange2",	"#EE7600",   0xff0076ee},
    {"darkorange3",	"#CD6600",   0xff0066cd},
    {"darkorange4",	"#8B4500",   0xff00458b},
    {"darkorchid",	"#9932CC",   0xffcc3299},
    {"darkorchid1",	"#BF3EFF",   0xffff3ebf},
    {"darkorchid2",	"#B23AEE",   0xffee3ab2},
    {"darkorchid3",	"#9A32CD",   0xffcd329a},
    {"darkorchid4",	"#68228B",   0xff8b2268},
    {"darkred",	"#8B0000",   0xff00008b},
    {"darksalmon",	"#E9967A",   0xff7a96e9},
    {"darkseagreen",	"#8FBC8F",   0xff8fbc8f},
    {"darkseagreen1",	"#C1FFC1",   0xffc1ffc1},
    {"darkseagreen2",	"#B4EEB4",   0xffb4eeb4},
    {"darkseagreen3",	"#9BCD9B",   0xff9bcd9b},
    {"darkseagreen4",	"#698B69",   0xff698b69},
    {"darkslateblue",	"#483D8B",   0xff8b3d48},
    {"darkslategray",	"#2F4F4F",   0xff4f4f2f},
    {"darkslategray1",	"#97FFFF",   0xffffff97},
    {"darkslategray2",	"#8DEEEE",   0xffeeee8d},
    {"darkslategray3",	"#79CDCD",   0xffcdcd79},
    {"darkslategray4",	"#528B8B",   0xff8b8b52},
    {"darkslategrey",	"#2F4F4F",   0xff4f4f2f},
    {"darkturquoise",	"#00CED1",   0xffd1ce00},
    {"darkviolet",	"#9400D3",   0xffd30094},
    {"deeppink",	"#FF1493",   0xff9314ff},
    {"deeppink1",	"#FF1493",   0xff9314ff},
    {"deeppink2",	"#EE1289",   0xff8912ee},
    {"deeppink3",	"#CD1076",   0xff7610cd},
    {"deeppink4",	"#8B0A50",   0xff500a8b},
    {"deepskyblue",	"#00BFFF",   0xffffbf00},
    {"deepskyblue1",	"#00BFFF",   0xffffbf00},
    {"deepskyblue2",	"#00B2EE",   0xffeeb200},
    {"deepskyblue3",	"#009ACD",   0xffcd9a00},
    {"deepskyblue4",	"#00688B",   0xff8b6800},
    {"dimgray",	"#696969",   0xff696969},
    {"dimgrey",	"#696969",   0xff696969},
    {"dodgerblue",	"#1E90FF",   0xffff901e},
    {"dodgerblue1",	"#1E90FF",   0xffff901e},
    {"dodgerblue2",	"#1C86EE",   0xffee861c},
    {"dodgerblue3",	"#1874CD",   0xffcd7418},
    {"dodgerblue4",	"#104E8B",   0xff8b4e10},
    {"firebrick",	"#B22222",   0xff2222b2},
    {"firebrick1",	"#FF3030",   0xff3030ff},
    {"firebrick2",	"#EE2C2C",   0xff2c2cee},
    {"firebrick3",	"#CD2626",   0xff2626cd},
    {"firebrick4",	"#8B1A1A",   0xff1a1a8b},
    {"floralwhite",	"#FFFAF0",   0xfff0faff},
    {"forestgreen",	"#228B22",   0xff228b22},
    {"gainsboro",	"#DCDCDC",   0xffdcdcdc},
    {"ghostwhite",	"#F8F8FF",   0xfffff8f8},
    {"gold",	"#FFD700",   0xff00d7ff},
    {"gold1",	"#FFD700",   0xff00d7ff},
    {"gold2",	"#EEC900",   0xff00c9ee},
    {"gold3",	"#CDAD00",   0xff00adcd},
    {"gold4",	"#8B7500",   0xff00758b},
    {"goldenrod",	"#DAA520",   0xff20a5da},
    {"goldenrod1",	"#FFC125",   0xff25c1ff},
    {"goldenrod2",	"#EEB422",   0xff22b4ee},
    {"goldenrod3",	"#CD9B1D",   0xff1d9bcd},
    {"goldenrod4",	"#8B6914",   0xff14698b},
    {"gray",	"#BEBEBE",   0xffbebebe},
    {"gray0",	"#000000",   0xff000000},
    {"gray1",	"#030303",   0xff030303},
    {"gray2",	"#050505",   0xff050505},
    {"gray3",	"#080808",   0xff080808},
    {"gray4",	"#0A0A0A",   0xff0a0a0a},
    {"gray5",	"#0D0D0D",   0xff0d0d0d},
    {"gray6",	"#0F0F0F",   0xff0f0f0f},
    {"gray7",	"#121212",   0xff121212},
    {"gray8",	"#141414",   0xff141414},
    {"gray9",	"#171717",   0xff171717},
    {"gray10",	"#1A1A1A",   0xff1a1a1a},
    {"gray11",	"#1C1C1C",   0xff1c1c1c},
    {"gray12",	"#1F1F1F",   0xff1f1f1f},
    {"gray13",	"#212121",   0xff212121},
    {"gray14",	"#242424",   0xff242424},
    {"gray15",	"#262626",   0xff262626},
    {"gray16",	"#292929",   0xff292929},
    {"gray17",	"#2B2B2B",   0xff2b2b2b},
    {"gray18",	"#2E2E2E",   0xff2e2e2e},
    {"gray19",	"#303030",   0xff303030},
    {"gray20",	"#333333",   0xff333333},
    {"gray21",	"#363636",   0xff363636},
    {"gray22",	"#383838",   0xff383838},
    {"gray23",	"#3B3B3B",   0xff3b3b3b},
    {"gray24",	"#3D3D3D",   0xff3d3d3d},
    {"gray25",	"#404040",   0xff404040},
    {"gray26",	"#424242",   0xff424242},
    {"gray27",	"#454545",   0xff454545},
    {"gray28",	"#474747",   0xff474747},
    {"gray29",	"#4A4A4A",   0xff4a4a4a},
    {"gray30",	"#4D4D4D",   0xff4d4d4d},
    {"gray31",	"#4F4F4F",   0xff4f4f4f},
    {"gray32",	"#525252",   0xff525252},
    {"gray33",	"#545454",   0xff545454},
    {"gray34",	"#575757",   0xff575757},
    {"gray35",	"#595959",   0xff595959},
    {"gray36",	"#5C5C5C",   0xff5c5c5c},
    {"gray37",	"#5E5E5E",   0xff5e5e5e},
    {"gray38",	"#616161",   0xff616161},
    {"gray39",	"#636363",   0xff636363},
    {"gray40",	"#666666",   0xff666666},
    {"gray41",	"#696969",   0xff696969},
    {"gray42",	"#6B6B6B",   0xff6b6b6b},
    {"gray43",	"#6E6E6E",   0xff6e6e6e},
    {"gray44",	"#707070",   0xff707070},
    {"gray45",	"#737373",   0xff737373},
    {"gray46",	"#757575",   0xff757575},
    {"gray47",	"#787878",   0xff787878},
    {"gray48",	"#7A7A7A",   0xff7a7a7a},
    {"gray49",	"#7D7D7D",   0xff7d7d7d},
    {"gray50",	"#7F7F7F",   0xff7f7f7f},
    {"gray51",	"#828282",   0xff828282},
    {"gray52",	"#858585",   0xff858585},
    {"gray53",	"#878787",   0xff878787},
    {"gray54",	"#8A8A8A",   0xff8a8a8a},
    {"gray55",	"#8C8C8C",   0xff8c8c8c},
    {"gray56",	"#8F8F8F",   0xff8f8f8f},
    {"gray57",	"#919191",   0xff919191},
    {"gray58",	"#949494",   0xff949494},
    {"gray59",	"#969696",   0xff969696},
    {"gray60",	"#999999",   0xff999999},
    {"gray61",	"#9C9C9C",   0xff9c9c9c},
    {"gray62",	"#9E9E9E",   0xff9e9e9e},
    {"gray63",	"#A1A1A1",   0xffa1a1a1},
    {"gray64",	"#A3A3A3",   0xffa3a3a3},
    {"gray65",	"#A6A6A6",   0xffa6a6a6},
    {"gray66",	"#A8A8A8",   0xffa8a8a8},
    {"gray67",	"#ABABAB",   0xffababab},
    {"gray68",	"#ADADAD",   0xffadadad},
    {"gray69",	"#B0B0B0",   0xffb0b0b0},
    {"gray70",	"#B3B3B3",   0xffb3b3b3},
    {"gray71",	"#B5B5B5",   0xffb5b5b5},
    {"gray72",	"#B8B8B8",   0xffb8b8b8},
    {"gray73",	"#BABABA",   0xffbababa},
    {"gray74",	"#BDBDBD",   0xffbdbdbd},
    {"gray75",	"#BFBFBF",   0xffbfbfbf},
    {"gray76",	"#C2C2C2",   0xffc2c2c2},
    {"gray77",	"#C4C4C4",   0xffc4c4c4},
    {"gray78",	"#C7C7C7",   0xffc7c7c7},
    {"gray79",	"#C9C9C9",   0xffc9c9c9},
    {"gray80",	"#CCCCCC",   0xffcccccc},
    {"gray81",	"#CFCFCF",   0xffcfcfcf},
    {"gray82",	"#D1D1D1",   0xffd1d1d1},
    {"gray83",	"#D4D4D4",   0xffd4d4d4},
    {"gray84",	"#D6D6D6",   0xffd6d6d6},
    {"gray85",	"#D9D9D9",   0xffd9d9d9},
    {"gray86",	"#DBDBDB",   0xffdbdbdb},
    {"gray87",	"#DEDEDE",   0xffdedede},
    {"gray88",	"#E0E0E0",   0xffe0e0e0},
    {"gray89",	"#E3E3E3",   0xffe3e3e3},
    {"gray90",	"#E5E5E5",   0xffe5e5e5},
    {"gray91",	"#E8E8E8",   0xffe8e8e8},
    {"gray92",	"#EBEBEB",   0xffebebeb},
    {"gray93",	"#EDEDED",   0xffededed},
    {"gray94",	"#F0F0F0",   0xfff0f0f0},
    {"gray95",	"#F2F2F2",   0xfff2f2f2},
    {"gray96",	"#F5F5F5",   0xfff5f5f5},
    {"gray97",	"#F7F7F7",   0xfff7f7f7},
    {"gray98",	"#FAFAFA",   0xfffafafa},
    {"gray99",	"#FCFCFC",   0xfffcfcfc},
    {"gray100",	"#FFFFFF",   0xffffffff},
    {"green",	"#00FF00",   0xff00ff00},
    {"green1",	"#00FF00",   0xff00ff00},
    {"green2",	"#00EE00",   0xff00ee00},
    {"green3",	"#00CD00",   0xff00cd00},
    {"green4",	"#008B00",   0xff008b00},
    {"greenyellow",	"#ADFF2F",   0xff2fffad},
    {"grey",	"#BEBEBE",   0xffbebebe},
    {"grey0",	"#000000",   0xff000000},
    {"grey1",	"#030303",   0xff030303},
    {"grey2",	"#050505",   0xff050505},
    {"grey3",	"#080808",   0xff080808},
    {"grey4",	"#0A0A0A",   0xff0a0a0a},
    {"grey5",	"#0D0D0D",   0xff0d0d0d},
    {"grey6",	"#0F0F0F",   0xff0f0f0f},
    {"grey7",	"#121212",   0xff121212},
    {"grey8",	"#141414",   0xff141414},
    {"grey9",	"#171717",   0xff171717},
    {"grey10",	"#1A1A1A",   0xff1a1a1a},
    {"grey11",	"#1C1C1C",   0xff1c1c1c},
    {"grey12",	"#1F1F1F",   0xff1f1f1f},
    {"grey13",	"#212121",   0xff212121},
    {"grey14",	"#242424",   0xff242424},
    {"grey15",	"#262626",   0xff262626},
    {"grey16",	"#292929",   0xff292929},
    {"grey17",	"#2B2B2B",   0xff2b2b2b},
    {"grey18",	"#2E2E2E",   0xff2e2e2e},
    {"grey19",	"#303030",   0xff303030},
    {"grey20",	"#333333",   0xff333333},
    {"grey21",	"#363636",   0xff363636},
    {"grey22",	"#383838",   0xff383838},
    {"grey23",	"#3B3B3B",   0xff3b3b3b},
    {"grey24",	"#3D3D3D",   0xff3d3d3d},
    {"grey25",	"#404040",   0xff404040},
    {"grey26",	"#424242",   0xff424242},
    {"grey27",	"#454545",   0xff454545},
    {"grey28",	"#474747",   0xff474747},
    {"grey29",	"#4A4A4A",   0xff4a4a4a},
    {"grey30",	"#4D4D4D",   0xff4d4d4d},
    {"grey31",	"#4F4F4F",   0xff4f4f4f},
    {"grey32",	"#525252",   0xff525252},
    {"grey33",	"#545454",   0xff545454},
    {"grey34",	"#575757",   0xff575757},
    {"grey35",	"#595959",   0xff595959},
    {"grey36",	"#5C5C5C",   0xff5c5c5c},
    {"grey37",	"#5E5E5E",   0xff5e5e5e},
    {"grey38",	"#616161",   0xff616161},
    {"grey39",	"#636363",   0xff636363},
    {"grey40",	"#666666",   0xff666666},
    {"grey41",	"#696969",   0xff696969},
    {"grey42",	"#6B6B6B",   0xff6b6b6b},
    {"grey43",	"#6E6E6E",   0xff6e6e6e},
    {"grey44",	"#707070",   0xff707070},
    {"grey45",	"#737373",   0xff737373},
    {"grey46",	"#757575",   0xff757575},
    {"grey47",	"#787878",   0xff787878},
    {"grey48",	"#7A7A7A",   0xff7a7a7a},
    {"grey49",	"#7D7D7D",   0xff7d7d7d},
    {"grey50",	"#7F7F7F",   0xff7f7f7f},
    {"grey51",	"#828282",   0xff828282},
    {"grey52",	"#858585",   0xff858585},
    {"grey53",	"#878787",   0xff878787},
    {"grey54",	"#8A8A8A",   0xff8a8a8a},
    {"grey55",	"#8C8C8C",   0xff8c8c8c},
    {"grey56",	"#8F8F8F",   0xff8f8f8f},
    {"grey57",	"#919191",   0xff919191},
    {"grey58",	"#949494",   0xff949494},
    {"grey59",	"#969696",   0xff969696},
    {"grey60",	"#999999",   0xff999999},
    {"grey61",	"#9C9C9C",   0xff9c9c9c},
    {"grey62",	"#9E9E9E",   0xff9e9e9e},
    {"grey63",	"#A1A1A1",   0xffa1a1a1},
    {"grey64",	"#A3A3A3",   0xffa3a3a3},
    {"grey65",	"#A6A6A6",   0xffa6a6a6},
    {"grey66",	"#A8A8A8",   0xffa8a8a8},
    {"grey67",	"#ABABAB",   0xffababab},
    {"grey68",	"#ADADAD",   0xffadadad},
    {"grey69",	"#B0B0B0",   0xffb0b0b0},
    {"grey70",	"#B3B3B3",   0xffb3b3b3},
    {"grey71",	"#B5B5B5",   0xffb5b5b5},
    {"grey72",	"#B8B8B8",   0xffb8b8b8},
    {"grey73",	"#BABABA",   0xffbababa},
    {"grey74",	"#BDBDBD",   0xffbdbdbd},
    {"grey75",	"#BFBFBF",   0xffbfbfbf},
    {"grey76",	"#C2C2C2",   0xffc2c2c2},
    {"grey77",	"#C4C4C4",   0xffc4c4c4},
    {"grey78",	"#C7C7C7",   0xffc7c7c7},
    {"grey79",	"#C9C9C9",   0xffc9c9c9},
    {"grey80",	"#CCCCCC",   0xffcccccc},
    {"grey81",	"#CFCFCF",   0xffcfcfcf},
    {"grey82",	"#D1D1D1",   0xffd1d1d1},
    {"grey83",	"#D4D4D4",   0xffd4d4d4},
    {"grey84",	"#D6D6D6",   0xffd6d6d6},
    {"grey85",	"#D9D9D9",   0xffd9d9d9},
    {"grey86",	"#DBDBDB",   0xffdbdbdb},
    {"grey87",	"#DEDEDE",   0xffdedede},
    {"grey88",	"#E0E0E0",   0xffe0e0e0},
    {"grey89",	"#E3E3E3",   0xffe3e3e3},
    {"grey90",	"#E5E5E5",   0xffe5e5e5},
    {"grey91",	"#E8E8E8",   0xffe8e8e8},
    {"grey92",	"#EBEBEB",   0xffebebeb},
    {"grey93",	"#EDEDED",   0xffededed},
    {"grey94",	"#F0F0F0",   0xfff0f0f0},
    {"grey95",	"#F2F2F2",   0xfff2f2f2},
    {"grey96",	"#F5F5F5",   0xfff5f5f5},
    {"grey97",	"#F7F7F7",   0xfff7f7f7},
    {"grey98",	"#FAFAFA",   0xfffafafa},
    {"grey99",	"#FCFCFC",   0xfffcfcfc},
    {"grey100",	"#FFFFFF",   0xffffffff},
    {"honeydew",	"#F0FFF0",   0xfff0fff0},
    {"honeydew1",	"#F0FFF0",   0xfff0fff0},
    {"honeydew2",	"#E0EEE0",   0xffe0eee0},
    {"honeydew3",	"#C1CDC1",   0xffc1cdc1},
    {"honeydew4",	"#838B83",   0xff838b83},
    {"hotpink",	"#FF69B4",   0xffb469ff},
    {"hotpink1",	"#FF6EB4",   0xffb46eff},
    {"hotpink2",	"#EE6AA7",   0xffa76aee},
    {"hotpink3",	"#CD6090",   0xff9060cd},
    {"hotpink4",	"#8B3A62",   0xff623a8b},
    {"indianred",	"#CD5C5C",   0xff5c5ccd},
    {"indianred1",	"#FF6A6A",   0xff6a6aff},
    {"indianred2",	"#EE6363",   0xff6363ee},
    {"indianred3",	"#CD5555",   0xff5555cd},
    {"indianred4",	"#8B3A3A",   0xff3a3a8b},
    {"ivory",	"#FFFFF0",   0xfff0ffff},
    {"ivory1",	"#FFFFF0",   0xfff0ffff},
    {"ivory2",	"#EEEEE0",   0xffe0eeee},
    {"ivory3",	"#CDCDC1",   0xffc1cdcd},
    {"ivory4",	"#8B8B83",   0xff838b8b},
    {"khaki",	"#F0E68C",   0xff8ce6f0},
    {"khaki1",	"#FFF68F",   0xff8ff6ff},
    {"khaki2",	"#EEE685",   0xff85e6ee},
    {"khaki3",	"#CDC673",   0xff73c6cd},
    {"khaki4",	"#8B864E",   0xff4e868b},
    {"lavender",	"#E6E6FA",   0xfffae6e6},
    {"lavenderblush",	"#FFF0F5",   0xfff5f0ff},
    {"lavenderblush1",	"#FFF0F5",   0xfff5f0ff},
    {"lavenderblush2",	"#EEE0E5",   0xffe5e0ee},
    {"lavenderblush3",	"#CDC1C5",   0xffc5c1cd},
    {"lavenderblush4",	"#8B8386",   0xff86838b},
    {"lawngreen",	"#7CFC00",   0xff00fc7c},
    {"lemonchiffon",	"#FFFACD",   0xffcdfaff},
    {"lemonchiffon1",	"#FFFACD",   0xffcdfaff},
    {"lemonchiffon2",	"#EEE9BF",   0xffbfe9ee},
    {"lemonchiffon3",	"#CDC9A5",   0xffa5c9cd},
    {"lemonchiffon4",	"#8B8970",   0xff70898b},
    {"lightblue",	"#ADD8E6",   0xffe6d8ad},
    {"lightblue1",	"#BFEFFF",   0xffffefbf},
    {"lightblue2",	"#B2DFEE",   0xffeedfb2},
    {"lightblue3",	"#9AC0CD",   0xffcdc09a},
    {"lightblue4",	"#68838B",   0xff8b8368},
    {"lightcoral",	"#F08080",   0xff8080f0},
    {"lightcyan",	"#E0FFFF",   0xffffffe0},
    {"lightcyan1",	"#E0FFFF",   0xffffffe0},
    {"lightcyan2",	"#D1EEEE",   0xffeeeed1},
    {"lightcyan3",	"#B4CDCD",   0xffcdcdb4},
    {"lightcyan4",	"#7A8B8B",   0xff8b8b7a},
    {"lightgoldenrod",	"#EEDD82",   0xff82ddee},
    {"lightgoldenrod1",	"#FFEC8B",   0xff8becff},
    {"lightgoldenrod2",	"#EEDC82",   0xff82dcee},
    {"lightgoldenrod3",	"#CDBE70",   0xff70becd},
    {"lightgoldenrod4",	"#8B814C",   0xff4c818b},
    {"lightgoldenrodyellow",	"#FAFAD2",   0xffd2fafa},
    {"lightgray",	"#D3D3D3",   0xffd3d3d3},
    {"lightgreen",	"#90EE90",   0xff90ee90},
    {"lightgrey",	"#D3D3D3",   0xffd3d3d3},
    {"lightpink",	"#FFB6C1",   0xffc1b6ff},
    {"lightpink1",	"#FFAEB9",   0xffb9aeff},
    {"lightpink2",	"#EEA2AD",   0xffada2ee},
    {"lightpink3",	"#CD8C95",   0xff958ccd},
    {"lightpink4",	"#8B5F65",   0xff655f8b},
    {"lightsalmon",	"#FFA07A",   0xff7aa0ff},
    {"lightsalmon1",	"#FFA07A",   0xff7aa0ff},
    {"lightsalmon2",	"#EE9572",   0xff7295ee},
    {"lightsalmon3",	"#CD8162",   0xff6281cd},
    {"lightsalmon4",	"#8B5742",   0xff42578b},
    {"lightseagreen",	"#20B2AA",   0xffaab220},
    {"lightskyblue",	"#87CEFA",   0xffface87},
    {"lightskyblue1",	"#B0E2FF",   0xffffe2b0},
    {"lightskyblue2",	"#A4D3EE",   0xffeed3a4},
    {"lightskyblue3",	"#8DB6CD",   0xffcdb68d},
    {"lightskyblue4",	"#607B8B",   0xff8b7b60},
    {"lightslateblue",	"#8470FF",   0xffff7084},
    {"lightslategray",	"#778899",   0xff998877},
    {"lightslategrey",	"#778899",   0xff998877},
    {"lightsteelblue",	"#B0C4DE",   0xffdec4b0},
    {"lightsteelblue1",	"#CAE1FF",   0xffffe1ca},
    {"lightsteelblue2",	"#BCD2EE",   0xffeed2bc},
    {"lightsteelblue3",	"#A2B5CD",   0xffcdb5a2},
    {"lightsteelblue4",	"#6E7B8B",   0xff8b7b6e},
    {"lightyellow",	"#FFFFE0",   0xffe0ffff},
    {"lightyellow1",	"#FFFFE0",   0xffe0ffff},
    {"lightyellow2",	"#EEEED1",   0xffd1eeee},
    {"lightyellow3",	"#CDCDB4",   0xffb4cdcd},
    {"lightyellow4",	"#8B8B7A",   0xff7a8b8b},
    {"limegreen",	"#32CD32",   0xff32cd32},
    {"linen",	"#FAF0E6",   0xffe6f0fa},
    {"magenta",	"#FF00FF",   0xffff00ff},
    {"magenta1",	"#FF00FF",   0xffff00ff},
    {"magenta2",	"#EE00EE",   0xffee00ee},
    {"magenta3",	"#CD00CD",   0xffcd00cd},
    {"magenta4",	"#8B008B",   0xff8b008b},
    {"maroon",	"#B03060",   0xff6030b0},
    {"maroon1",	"#FF34B3",   0xffb334ff},
    {"maroon2",	"#EE30A7",   0xffa730ee},
    {"maroon3",	"#CD2990",   0xff9029cd},
    {"maroon4",	"#8B1C62",   0xff621c8b},
    {"mediumaquamarine",	"#66CDAA",   0xffaacd66},
    {"mediumblue",	"#0000CD",   0xffcd0000},
    {"mediumorchid",	"#BA55D3",   0xffd355ba},
    {"mediumorchid1",	"#E066FF",   0xffff66e0},
    {"mediumorchid2",	"#D15FEE",   0xffee5fd1},
    {"mediumorchid3",	"#B452CD",   0xffcd52b4},
    {"mediumorchid4",	"#7A378B",   0xff8b377a},
    {"mediumpurple",	"#9370DB",   0xffdb7093},
    {"mediumpurple1",	"#AB82FF",   0xffff82ab},
    {"mediumpurple2",	"#9F79EE",   0xffee799f},
    {"mediumpurple3",	"#8968CD",   0xffcd6889},
    {"mediumpurple4",	"#5D478B",   0xff8b475d},
    {"mediumseagreen",	"#3CB371",   0xff71b33c},
    {"mediumslateblue",	"#7B68EE",   0xffee687b},
    {"mediumspringgreen",	"#00FA9A",   0xff9afa00},
    {"mediumturquoise",	"#48D1CC",   0xffccd148},
    {"mediumvioletred",	"#C71585",   0xff8515c7},
    {"midnightblue",	"#191970",   0xff701919},
    {"mintcream",	"#F5FFFA",   0xfffafff5},
    {"mistyrose",	"#FFE4E1",   0xffe1e4ff},
    {"mistyrose1",	"#FFE4E1",   0xffe1e4ff},
    {"mistyrose2",	"#EED5D2",   0xffd2d5ee},
    {"mistyrose3",	"#CDB7B5",   0xffb5b7cd},
    {"mistyrose4",	"#8B7D7B",   0xff7b7d8b},
    {"moccasin",	"#FFE4B5",   0xffb5e4ff},
    {"navajowhite",	"#FFDEAD",   0xffaddeff},
    {"navajowhite1",	"#FFDEAD",   0xffaddeff},
    {"navajowhite2",	"#EECFA1",   0xffa1cfee},
    {"navajowhite3",	"#CDB38B",   0xff8bb3cd},
    {"navajowhite4",	"#8B795E",   0xff5e798b},
    {"navy",	"#000080",   0xff800000},
    {"navyblue",	"#000080",   0xff800000},
    {"oldlace",	"#FDF5E6",   0xffe6f5fd},
    {"olivedrab",	"#6B8E23",   0xff238e6b},
    {"olivedrab1",	"#C0FF3E",   0xff3effc0},
    {"olivedrab2",	"#B3EE3A",   0xff3aeeb3},
    {"olivedrab3",	"#9ACD32",   0xff32cd9a},
    {"olivedrab4",	"#698B22",   0xff228b69},
    {"orange",	"#FFA500",   0xff00a5ff},
    {"orange1",	"#FFA500",   0xff00a5ff},
    {"orange2",	"#EE9A00",   0xff009aee},
    {"orange3",	"#CD8500",   0xff0085cd},
    {"orange4",	"#8B5A00",   0xff005a8b},
    {"orangered",	"#FF4500",   0xff0045ff},
    {"orangered1",	"#FF4500",   0xff0045ff},
    {"orangered2",	"#EE4000",   0xff0040ee},
    {"orangered3",	"#CD3700",   0xff0037cd},
    {"orangered4",	"#8B2500",   0xff00258b},
    {"orchid",	"#DA70D6",   0xffd670da},
    {"orchid1",	"#FF83FA",   0xfffa83ff},
    {"orchid2",	"#EE7AE9",   0xffe97aee},
    {"orchid3",	"#CD69C9",   0xffc969cd},
    {"orchid4",	"#8B4789",   0xff89478b},
    {"palegoldenrod",	"#EEE8AA",   0xffaae8ee},
    {"palegreen",	"#98FB98",   0xff98fb98},
    {"palegreen1",	"#9AFF9A",   0xff9aff9a},
    {"palegreen2",	"#90EE90",   0xff90ee90},
    {"palegreen3",	"#7CCD7C",   0xff7ccd7c},
    {"palegreen4",	"#548B54",   0xff548b54},
    {"paleturquoise",	"#AFEEEE",   0xffeeeeaf},
    {"paleturquoise1",	"#BBFFFF",   0xffffffbb},
    {"paleturquoise2",	"#AEEEEE",   0xffeeeeae},
    {"paleturquoise3",	"#96CDCD",   0xffcdcd96},
    {"paleturquoise4",	"#668B8B",   0xff8b8b66},
    {"palevioletred",	"#DB7093",   0xff9370db},
    {"palevioletred1",	"#FF82AB",   0xffab82ff},
    {"palevioletred2",	"#EE799F",   0xff9f79ee},
    {"palevioletred3",	"#CD6889",   0xff8968cd},
    {"palevioletred4",	"#8B475D",   0xff5d478b},
    {"papayawhip",	"#FFEFD5",   0xffd5efff},
    {"peachpuff",	"#FFDAB9",   0xffb9daff},
    {"peachpuff1",	"#FFDAB9",   0xffb9daff},
    {"peachpuff2",	"#EECBAD",   0xffadcbee},
    {"peachpuff3",	"#CDAF95",   0xff95afcd},
    {"peachpuff4",	"#8B7765",   0xff65778b},
    {"peru",	"#CD853F",   0xff3f85cd},
    {"pink",	"#FFC0CB",   0xffcbc0ff},
    {"pink1",	"#FFB5C5",   0xffc5b5ff},
    {"pink2",	"#EEA9B8",   0xffb8a9ee},
    {"pink3",	"#CD919E",   0xff9e91cd},
    {"pink4",	"#8B636C",   0xff6c638b},
    {"plum",	"#DDA0DD",   0xffdda0dd},
    {"plum1",	"#FFBBFF",   0xffffbbff},
    {"plum2",	"#EEAEEE",   0xffeeaeee},
    {"plum3",	"#CD96CD",   0xffcd96cd},
    {"plum4",	"#8B668B",   0xff8b668b},
    {"powderblue",	"#B0E0E6",   0xffe6e0b0},
    {"purple",	"#A020F0",   0xfff020a0},
    {"purple1",	"#9B30FF",   0xffff309b},
    {"purple2",	"#912CEE",   0xffee2c91},
    {"purple3",	"#7D26CD",   0xffcd267d},
    {"purple4",	"#551A8B",   0xff8b1a55},
    {"red",	"#FF0000",   0xff0000ff},
    {"red1",	"#FF0000",   0xff0000ff},
    {"red2",	"#EE0000",   0xff0000ee},
    {"red3",	"#CD0000",   0xff0000cd},
    {"red4",	"#8B0000",   0xff00008b},
    {"rosybrown",	"#BC8F8F",   0xff8f8fbc},
    {"rosybrown1",	"#FFC1C1",   0xffc1c1ff},
    {"rosybrown2",	"#EEB4B4",   0xffb4b4ee},
    {"rosybrown3",	"#CD9B9B",   0xff9b9bcd},
    {"rosybrown4",	"#8B6969",   0xff69698b},
    {"royalblue",	"#4169E1",   0xffe16941},
    {"royalblue1",	"#4876FF",   0xffff7648},
    {"royalblue2",	"#436EEE",   0xffee6e43},
    {"royalblue3",	"#3A5FCD",   0xffcd5f3a},
    {"royalblue4",	"#27408B",   0xff8b4027},
    {"saddlebrown",	"#8B4513",   0xff13458b},
    {"salmon",	"#FA8072",   0xff7280fa},
    {"salmon1",	"#FF8C69",   0xff698cff},
    {"salmon2",	"#EE8262",   0xff6282ee},
    {"salmon3",	"#CD7054",   0xff5470cd},
    {"salmon4",	"#8B4C39",   0xff394c8b},
    {"sandybrown",	"#F4A460",   0xff60a4f4},
    {"seagreen",	"#2E8B57",   0xff578b2e},
    {"seagreen1",	"#54FF9F",   0xff9fff54},
    {"seagreen2",	"#4EEE94",   0xff94ee4e},
    {"seagreen3",	"#43CD80",   0xff80cd43},
    {"seagreen4",	"#2E8B57",   0xff578b2e},
    {"seashell",	"#FFF5EE",   0xffeef5ff},
    {"seashell1",	"#FFF5EE",   0xffeef5ff},
    {"seashell2",	"#EEE5DE",   0xffdee5ee},
    {"seashell3",	"#CDC5BF",   0xffbfc5cd},
    {"seashell4",	"#8B8682",   0xff82868b},
    {"sienna",	"#A0522D",   0xff2d52a0},
    {"sienna1",	"#FF8247",   0xff4782ff},
    {"sienna2",	"#EE7942",   0xff4279ee},
    {"sienna3",	"#CD6839",   0xff3968cd},
    {"sienna4",	"#8B4726",   0xff26478b},
    {"skyblue",	"#87CEEB",   0xffebce87},
    {"skyblue1",	"#87CEFF",   0xffffce87},
    {"skyblue2",	"#7EC0EE",   0xffeec07e},
    {"skyblue3",	"#6CA6CD",   0xffcda66c},
    {"skyblue4",	"#4A708B",   0xff8b704a},
    {"slateblue",	"#6A5ACD",   0xffcd5a6a},
    {"slateblue1",	"#836FFF",   0xffff6f83},
    {"slateblue2",	"#7A67EE",   0xffee677a},
    {"slateblue3",	"#6959CD",   0xffcd5969},
    {"slateblue4",	"#473C8B",   0xff8b3c47},
    {"slategray",	"#708090",   0xff908070},
    {"slategray1",	"#C6E2FF",   0xffffe2c6},
    {"slategray2",	"#B9D3EE",   0xffeed3b9},
    {"slategray3",	"#9FB6CD",   0xffcdb69f},
    {"slategray4",	"#6C7B8B",   0xff8b7b6c},
    {"slategrey",	"#708090",   0xff908070},
    {"snow",	"#FFFAFA",   0xfffafaff},
    {"snow1",	"#FFFAFA",   0xfffafaff},
    {"snow2",	"#EEE9E9",   0xffe9e9ee},
    {"snow3",	"#CDC9C9",   0xffc9c9cd},
    {"snow4",	"#8B8989",   0xff89898b},
    {"springgreen",	"#00FF7F",   0xff7fff00},
    {"springgreen1",	"#00FF7F",   0xff7fff00},
    {"springgreen2",	"#00EE76",   0xff76ee00},
    {"springgreen3",	"#00CD66",   0xff66cd00},
    {"springgreen4",	"#008B45",   0xff458b00},
    {"steelblue",	"#4682B4",   0xffb48246},
    {"steelblue1",	"#63B8FF",   0xffffb863},
    {"steelblue2",	"#5CACEE",   0xffeeac5c},
    {"steelblue3",	"#4F94CD",   0xffcd944f},
    {"steelblue4",	"#36648B",   0xff8b6436},
    {"tan",	"#D2B48C",   0xff8cb4d2},
    {"tan1",	"#FFA54F",   0xff4fa5ff},
    {"tan2",	"#EE9A49",   0xff499aee},
    {"tan3",	"#CD853F",   0xff3f85cd},
    {"tan4",	"#8B5A2B",   0xff2b5a8b},
    {"thistle",	"#D8BFD8",   0xffd8bfd8},
    {"thistle1",	"#FFE1FF",   0xffffe1ff},
    {"thistle2",	"#EED2EE",   0xffeed2ee},
    {"thistle3",	"#CDB5CD",   0xffcdb5cd},
    {"thistle4",	"#8B7B8B",   0xff8b7b8b},
    {"tomato",	"#FF6347",   0xff4763ff},
    {"tomato1",	"#FF6347",   0xff4763ff},
    {"tomato2",	"#EE5C42",   0xff425cee},
    {"tomato3",	"#CD4F39",   0xff394fcd},
    {"tomato4",	"#8B3626",   0xff26368b},
    {"turquoise",	"#40E0D0",   0xffd0e040},
    {"turquoise1",	"#00F5FF",   0xfffff500},
    {"turquoise2",	"#00E5EE",   0xffeee500},
    {"turquoise3",	"#00C5CD",   0xffcdc500},
    {"turquoise4",	"#00868B",   0xff8b8600},
    {"violet",	"#EE82EE",   0xffee82ee},
    {"violetred",	"#D02090",   0xff9020d0},
    {"violetred1",	"#FF3E96",   0xff963eff},
    {"violetred2",	"#EE3A8C",   0xff8c3aee},
    {"violetred3",	"#CD3278",   0xff7832cd},
    {"violetred4",	"#8B2252",   0xff52228b},
    {"wheat",	"#F5DEB3",   0xffb3def5},
    {"wheat1",	"#FFE7BA",   0xffbae7ff},
    {"wheat2",	"#EED8AE",   0xffaed8ee},
    {"wheat3",	"#CDBA96",   0xff96bacd},
    {"wheat4",	"#8B7E66",   0xff667e8b},
    {"whitesmoke",	"#F5F5F5",   0xfff5f5f5},
    {"yellow",	"#FFFF00",   0xff00ffff},
    {"yellow1",	"#FFFF00",   0xff00ffff},
    {"yellow2",	"#EEEE00",   0xff00eeee},
    {"yellow3",	"#CDCD00",   0xff00cdcd},
    {"yellow4",	"#8B8B00",   0xff008b8b},
    {"yellowgreen",	"#9ACD32",   0xff32cd9a},
    {NULL,		NULL,		0}
};


/* Hex Digit to Integer Conversion */

static unsigned int hexdigit(int digit)
{
    if('0' <= digit && digit <= '9') return digit - '0';
    if('A' <= digit && digit <= 'F') return 10 + digit - 'A';
    if('a' <= digit && digit <= 'f') return 10 + digit - 'a';
    /*else */ error(_("invalid hex digit in 'color' or 'lty'"));
    return digit; /* never occurs (-Wall) */
}


/* #RRGGBB[AA] String to Internal Color Code */
static rcolor rgb2col(const char *rgb)
{
    unsigned int r = 0, g = 0, b = 0, a = 0; /* -Wall */
    if(rgb[0] != '#')
	error(_("invalid RGB specification"));
    switch (strlen(rgb)) {
    case 9:
	a = 16 * hexdigit(rgb[7]) + hexdigit(rgb[8]);
    case 7:
	r = 16 * hexdigit(rgb[1]) + hexdigit(rgb[2]);
	g = 16 * hexdigit(rgb[3]) + hexdigit(rgb[4]);
	b = 16 * hexdigit(rgb[5]) + hexdigit(rgb[6]);
	break;
    default:
	error(_("invalid RGB specification"));
    }
    if (strlen(rgb) == 7)
	return R_RGB(r, g, b);
    else
	return R_RGBA(r, g, b, a);
}

/* External Color Name to Internal Color Code */

static rcolor name2col(const char *nm)
{
    int i;
    if(strcmp(nm, "NA") == 0 || strcmp(nm, "transparent") == 0)
	/*
	 * Paul 01/07/04 (2004-07-01?)
	 *
	 * Used to be set to NA_INTEGER.
	 *
	 * Now set to fully transparent white.
	 *
	 * In some cases, fully transparent gets caught by
	 * the graphics engine and no drawing occurs, but
	 * in other cases, transparent colours are passed to devices.
	 *
	 * All devices should respond to fully transparent by
	 * not drawing.
	 */
	return R_TRANWHITE;
    for(i = 0; ColorDataBase[i].name ; i++) {
	if(StrMatch(ColorDataBase[i].name, nm))
	    return ColorDataBase[i].code;
    }
    error(_("invalid color name '%s'"), nm);
    return 0U;		/* never occurs but avoid compiler warnings */
}


/* Internal to External Color Representation */
/* Search the color name database first */
/* If this fails, create an #RRGGBB string */

const char *incol2name(rcolor col)
{
    static char ColBuf[10]; // used for return value

    if(R_OPAQUE(col)) {
	for(int i = 0 ; ColorDataBase[i].name ; i++) {
	    if(col == ColorDataBase[i].code)
		return ColorDataBase[i].name;
	}
	ColBuf[0] = '#';
	ColBuf[1] = HexDigits[(col >>  4) & 15];
	ColBuf[2] = HexDigits[(col	    ) & 15];
	ColBuf[3] = HexDigits[(col >> 12) & 15];
	ColBuf[4] = HexDigits[(col >>  8) & 15];
	ColBuf[5] = HexDigits[(col >> 20) & 15];
	ColBuf[6] = HexDigits[(col >> 16) & 15];
	ColBuf[7] = '\0';
	return &ColBuf[0];
    } else if (R_TRANSPARENT(col)) {
	return "transparent";
    } else {
	ColBuf[0] = '#';
	ColBuf[1] = HexDigits[(col >>  4) & 15];
	ColBuf[2] = HexDigits[(col	) & 15];
	ColBuf[3] = HexDigits[(col >> 12) & 15];
	ColBuf[4] = HexDigits[(col >>  8) & 15];
	ColBuf[5] = HexDigits[(col >> 20) & 15];
	ColBuf[6] = HexDigits[(col >> 16) & 15];
	ColBuf[7] = HexDigits[(col >> 28) & 15];
	ColBuf[8] = HexDigits[(col >> 24) & 15];
	ColBuf[9] = '\0';
	return &ColBuf[0];
    }
}

static rcolor str2col(const char *s, rcolor bg)
{
    if(s[0] == '#') return rgb2col(s);
    else if(isdigit((int)s[0])) {
	char *ptr;
	int indx = (int) strtod(s, &ptr);
	if(*ptr) error(_("invalid color specification \"%s\""), s);
	if (indx == 0) return bg;
	return Palette[(indx-1) % PaletteSize];
    } else return name2col(s);
}

rcolor inR_GE_str2col(const char *s)
{
    if (streql(s, "0")) 
	error(_("invalid color specification \"%s\""), s);
    return str2col(s, R_TRANWHITE); // bg is irrelevant
}

/* Convert a sexp element to an R color desc */
/* We Assume that Checks Have Been Done */


rcolor inRGBpar3(SEXP x, int i, rcolor bg)
{
    int indx;
    switch(TYPEOF(x))
    {
    case STRSXP:
	return str2col(CHAR(STRING_ELT(x, i)), bg);
    case LGLSXP:
	indx = LOGICAL(x)[i];
	if (indx == NA_LOGICAL) return R_TRANWHITE;
	break;
    case INTSXP:
	indx = INTEGER(x)[i];
	if (indx == NA_INTEGER) return R_TRANWHITE;
	break;
    case REALSXP:
	if(!R_FINITE(REAL(x)[i])) return R_TRANWHITE;
	indx = (int) REAL(x)[i];
	break;
	   default:
	   warning(_("supplied color is neither numeric nor character"));
	   return bg;
    }
    if (indx < 0) 
	error(_("numerical color values must be >= 0, found %d"), indx);
    if (indx == 0) return bg;
    else return Palette[(indx-1) % PaletteSize];
}

SEXP palette(SEXP val)
{
    SEXP ans;
    rcolor color[MAX_PALETTE_SIZE];
    int i, n;

    if (!isString(val)) error(_("invalid argument type"));
    /* Record the current palette */
    PROTECT(ans = allocVector(STRSXP, PaletteSize));
    for (i = 0; i < PaletteSize; i++)
	SET_STRING_ELT(ans, i, mkChar(incol2name(Palette[i])));
    if ((n = length(val)) == 1) {
	if (StrMatch("default", CHAR(STRING_ELT(val, 0)))) {
	    int i;
	    for (i = 0; (i < MAX_PALETTE_SIZE) && DefaultPalette[i]; i++)
		Palette[i] = name2col(DefaultPalette[i]);
	    PaletteSize = i;
	} else error(_("unknown palette (need >= 2 colors)"));
    }
    else if (n > 1) {
	if (n > MAX_PALETTE_SIZE)
	    error(_("maximum number of colors is %d"), MAX_PALETTE_SIZE);
	for (i = 0; i < n; i++) {
	    const char *s = CHAR(STRING_ELT(val, i));
	    color[i] = (s[0] == '#') ? rgb2col(s) : name2col(s);
	}
	for (i = 0; i < n; i++)
	    Palette[i] = color[i];
	PaletteSize = n;
    }
    UNPROTECT(1);
    return ans;
}

/* A version using 'rcolor' type */
SEXP palette2(SEXP val)
{
    SEXP ans = PROTECT(allocVector(INTSXP, PaletteSize));
    int n = length(val), *ians = INTEGER(ans); 
    for (int i = 0; i < PaletteSize; i++) ians[i] = (int)Palette[i];
    if (n) {
	if (TYPEOF(val) != INTSXP) error("requires INTSXP argment");
	if (n > MAX_PALETTE_SIZE)
	    error(_("maximum number of colors is %d"), MAX_PALETTE_SIZE);
	for (int i = 0; i < n; i++) Palette[i] = (rcolor)INTEGER(val)[i];
	PaletteSize = n;
    }
    UNPROTECT(1);
    return ans;    
}

SEXP colors(void)
{
    int n;

    for (n = 0; ColorDataBase[n].name != NULL; n++) ;
    SEXP ans = PROTECT(allocVector(STRSXP, n));
    for (n = 0; ColorDataBase[n].name != NULL; n++)
	SET_STRING_ELT(ans, n, mkChar(ColorDataBase[n].name));
    UNPROTECT(1);
    return ans;
}

/* Used to push/pop palette when replaying display list */
static void savePalette(Rboolean save)
{
    if (save)
	for (int i = 0; i < PaletteSize; i++)
	    Palette0[i] = Palette[i];
    else
	for (int i = 0; i < PaletteSize; i++)
	    Palette[i] = Palette0[i];
}

/* same as src/main/colors.c */
typedef unsigned int (*F1)(SEXP x, int i, unsigned int bg);
typedef const char * (*F2)(unsigned int col);
typedef unsigned int (*F3)(const char *s);
typedef void (*F4)(Rboolean save);

void Rg_set_col_ptrs(F1 f1, F2 f2, F3 f3, F4 f4);

void initPalette(void)
{
    Rg_set_col_ptrs(&inRGBpar3, &incol2name, &inR_GE_str2col, &savePalette);

    /* Initialize the Color Database: we now pre-compute this
    for(int i = 0 ; ColorDataBase[i].name ; i++)
	ColorDataBase[i].code = rgb2col(ColorDataBase[i].rgb);

    Install Default Palette: precomputed
    int i;
    for(i = 0 ; DefaultPalette[i] ; i++)
	Palette[i] = name2col(DefaultPalette[i]);
    PaletteSize = i;  // 8
    */
}

