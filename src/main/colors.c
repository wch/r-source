/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997-2009  The R Core Team
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

static unsigned int rgb2col(const char *);
static char *RGB2rgb(unsigned int, unsigned int, unsigned int);
static char *RGBA2rgb(unsigned int, unsigned int, unsigned int, unsigned int);
static double str2col(const char *s, double bg);

static int R_ColorTableSize;
static unsigned int R_ColorTable[COLOR_TABLE_SIZE];

/* String Comparison Ignoring Case and Squeezing Out Blanks */
static int StrMatch(const char *s, const char *t)
{
    for(;;) {
	if(*s == '\0' && *t == '\0') {
	    return 1;
	}
	if(*s == ' ') {
	    s++; continue;
	}
	if(*t == ' ') {
	    t++; continue;
	}
	if(tolower(*s++) != tolower(*t++))
	    return 0;
    }
}

static unsigned int char2col(const char *s)
{
    if (s[0] == '#') return rgb2col(s);
    else return name2col(s);
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


SEXP attribute_hidden do_hsv(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP c, h, s, v, a;
    double hh, ss, vv, aa, r=0., g=0., b=0.; /* -Wall */
    int i, max, nh, ns, nv, na;

    checkArity(op, args);

    PROTECT(h = coerceVector(CAR(args),REALSXP)); args = CDR(args);
    PROTECT(s = coerceVector(CAR(args),REALSXP)); args = CDR(args);
    PROTECT(v = coerceVector(CAR(args),REALSXP)); args = CDR(args);
    PROTECT(a = coerceVector(CAR(args),REALSXP)); args = CDR(args);

    nh = LENGTH(h);
    ns = LENGTH(s);
    nv = LENGTH(v);
    na = LENGTH(a);
    if (nh <= 0 || ns <= 0 || nv <= 0 || na <= 0) {
	UNPROTECT(4);
	return(allocVector(STRSXP, 0));
    }
    max = nh;
    if (max < ns) max = ns;
    if (max < nv) max = nv;
    if (max < na) max = na;
    PROTECT(c = allocVector(STRSXP, max));
    if(max == 0) return(c);

    for (i = 0; i < max; i++) {
	hh = REAL(h)[i % nh];
	ss = REAL(s)[i % ns];
	vv = REAL(v)[i % nv];
	aa = REAL(a)[i % na];
	if (hh < 0 || hh > 1 || ss < 0 || ss > 1 || vv < 0 || vv > 1 ||
	    aa < 0 || aa > 1)
	    error(_("invalid hsv color"));
	hsv2rgb(hh, ss, vv, &r, &g, &b);
	SET_STRING_ELT(c, i, mkChar(RGBA2rgb(ScaleColor(r),
					     ScaleColor(g),
					     ScaleColor(b),
					     ScaleAlpha(aa))));
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

SEXP attribute_hidden do_hcl(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP h, c, l, a, ans;
    double H, C, L, A, r, g, b;
    int nh, nc, nl, na, max, i;
    int ir, ig, ib;
    int fixup;

    checkArity(op, args);

    PROTECT(h = coerceVector(CAR(args),REALSXP)); args = CDR(args);
    PROTECT(c = coerceVector(CAR(args),REALSXP)); args = CDR(args);
    PROTECT(l = coerceVector(CAR(args),REALSXP)); args = CDR(args);
    PROTECT(a = coerceVector(CAR(args),REALSXP)); args = CDR(args);
    fixup = asLogical(CAR(args));
    nh = LENGTH(h);
    nc = LENGTH(c);
    nl = LENGTH(l);
    na = LENGTH(a);
    if (nh <= 0 || nc <= 0 || nl <= 0 || na <= 0) {
	UNPROTECT(4);
	return(allocVector(STRSXP, 0));
    }
    max = nh;
    if (max < nc) max = nc;
    if (max < nl) max = nl;
    if (max < na) max = na;
    PROTECT(ans = allocVector(STRSXP, max));
    for (i = 0; i < max; i++) {
	H = REAL(h)[i % nh];
	C = REAL(c)[i % nc];
	L = REAL(l)[i % nl];
	A = REAL(a)[i % na];
	if (!R_FINITE(A)) A = 1;
	if (L < 0 || L > WHITE_Y || C < 0 || A < 0 || A > 1)
	    error(_("invalid hcl color"));
	hcl2rgb(H, C, L, &r, &g, &b);
	ir = 255 * r + .5;
	ig = 255 * g + .5;
	ib = 255 * b + .5;
	if (FixupColor(&ir, &ig, &ib) && !fixup)
	    SET_STRING_ELT(ans, i, NA_STRING);
	else
	    SET_STRING_ELT(ans, i, mkChar(RGBA2rgb(ir, ig, ib,
					  ScaleAlpha(A))));
    }
    UNPROTECT(5);
    return ans;
}

SEXP attribute_hidden do_rgb(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP c, r, g, b, a, nam;
    int OP, i, l_max, nr, ng, nb, na;
    Rboolean max_1 = FALSE;
    double mV = 0.0; /* -Wall */

    checkArity(op, args);
    OP = PRIMVAL(op);
    if(OP) {/* op == 1:  rgb256() :*/
	PROTECT(r = coerceVector(CAR(args), INTSXP)); args = CDR(args);
	PROTECT(g = coerceVector(CAR(args), INTSXP)); args = CDR(args);
	PROTECT(b = coerceVector(CAR(args), INTSXP)); args = CDR(args);
	PROTECT(a = coerceVector(CAR(args), INTSXP)); args = CDR(args);
    }
    else {
	PROTECT(r = coerceVector(CAR(args), REALSXP)); args = CDR(args);
	PROTECT(g = coerceVector(CAR(args), REALSXP)); args = CDR(args);
	PROTECT(b = coerceVector(CAR(args), REALSXP)); args = CDR(args);
	PROTECT(a = coerceVector(CAR(args), REALSXP)); args = CDR(args);
	mV = asReal(CAR(args));			       args = CDR(args);
	max_1 = (mV == 1.);
    }

    nr = LENGTH(r); ng = LENGTH(g); nb = LENGTH(b); na = LENGTH(a);
    if (nr <= 0 || ng <= 0 || nb <= 0 || na <= 0) {
	UNPROTECT(4);
	return(allocVector(STRSXP, 0));
    }
    l_max = nr;
    if (l_max < ng) l_max = ng;
    if (l_max < nb) l_max = nb;
    if (l_max < na) l_max = na;

    PROTECT(nam = coerceVector(CAR(args), STRSXP)); args = CDR(args);
    if (length(nam) != 0 && length(nam) != l_max)
	error(_("invalid 'names' vector"));
    PROTECT(c = allocVector(STRSXP, l_max));

#define _R_set_c_RGBA(_R,_G,_B,_A)				\
    for (i = 0; i < l_max; i++)				\
	SET_STRING_ELT(c, i, mkChar(RGBA2rgb(_R,_G,_B,_A)))

    if(OP) { /* OP == 1:  rgb256() :*/
	_R_set_c_RGBA(CheckColor(INTEGER(r)[i%nr]),
		      CheckColor(INTEGER(g)[i%ng]),
		      CheckColor(INTEGER(b)[i%nb]),
		      CheckAlpha(INTEGER(a)[i%na]));
    }
    else if(max_1) {
	_R_set_c_RGBA(ScaleColor(REAL(r)[i%nr]),
		      ScaleColor(REAL(g)[i%ng]),
		      ScaleColor(REAL(b)[i%nb]),
		      ScaleAlpha(REAL(a)[i%na]));
    }
    else { /* maxColorVal not in {1, 255} */
	_R_set_c_RGBA(ScaleColor(REAL(r)[i%nr] / mV),
		      ScaleColor(REAL(g)[i%ng] / mV),
		      ScaleColor(REAL(b)[i%nb] / mV),
		      ScaleAlpha(REAL(a)[i%na] / mV));
    }
    if (length(nam) != 0)
	setAttrib(c, R_NamesSymbol, nam);
    UNPROTECT(6);
    return c;
}

SEXP attribute_hidden do_gray(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP lev, ans;
    double level;
    int i, ilevel, nlev;

    checkArity(op, args);

    PROTECT(lev = coerceVector(CAR(args),REALSXP));
    nlev = LENGTH(lev);
    PROTECT(ans = allocVector(STRSXP, nlev));
    for (i = 0; i < nlev; i++) {
	level = REAL(lev)[i];
	if (ISNAN(level) || level < 0 || level > 1)
	    error(_("invalid gray level, must be in [0,1]."));
	ilevel = 255 * level + 0.5;
	SET_STRING_ELT(ans, i, mkChar(RGB2rgb(ilevel, ilevel, ilevel)));
    }
    UNPROTECT(2);
    return ans;
}

SEXP attribute_hidden do_col2RGB(SEXP call, SEXP op, SEXP args, SEXP env)
{
/* colorname, "#rrggbb" or "col.number" to (r,g,b) conversion */

    SEXP colors, ans, names, dmns;
    double col, bg;
    unsigned int icol;
    int n, i, i4;

    checkArity(op, args);
    colors = CAR(args);
    if(isString(colors)) PROTECT(colors);
    else {
	PROTECT(colors = coerceVector(colors, INTSXP));
	if (TYPEOF(colors) != INTSXP)
	    error(_("invalid '%s' value"), "col");
    }
    n = LENGTH(colors);

    /* First set up the output matrix */
    PROTECT(ans = allocMatrix(INTSXP, 4, n));
    PROTECT(dmns = allocVector(VECSXP, 2));
    PROTECT(names = allocVector(STRSXP, 4));
    SET_STRING_ELT(names, 0, mkChar("red"));
    SET_STRING_ELT(names, 1, mkChar("green"));
    SET_STRING_ELT(names, 2, mkChar("blue"));
    SET_STRING_ELT(names, 3, mkChar("alpha"));
    SET_VECTOR_ELT(dmns, 0, names);
    UNPROTECT(1); /*names*/
    if ((names = getAttrib(colors, R_NamesSymbol)) != R_NilValue)
	SET_VECTOR_ELT(dmns, 1, names);
    setAttrib(ans, R_DimNamesSymbol, dmns);

    /* avoid looking up the background unless we will need it;
       this may avoid opening a new window.  Unfortunately, there is no
       unavailable colour, so we work with doubles and convert at the 
       last minute */

#define BG_NEEDED -1.0

    bg = BG_NEEDED;

    if(isString(colors)) {
	for(i = i4 = 0; i < n; i++, i4 += 4) {
	    col = str2col(CHAR(STRING_ELT(colors, i)), bg);
	    if (col == BG_NEEDED)
	    	col = bg = dpptr(GEcurrentDevice())->bg;
	    icol = (unsigned int)col;
	    INTEGER(ans)[i4 +0] = R_RED(icol);
	    INTEGER(ans)[i4 +1] = R_GREEN(icol);
	    INTEGER(ans)[i4 +2] = R_BLUE(icol);
	    INTEGER(ans)[i4 +3] = R_ALPHA(icol);
	}
    } else {
	for(i = i4 = 0; i < n; i++, i4 += 4) {
	    col = INTEGER(colors)[i];
	    if      (col == NA_INTEGER) col = R_TRANWHITE;
	    else if (col == 0)          col = bg;
	    else 		        col = R_ColorTable[(unsigned int)(col-1) % R_ColorTableSize];
	    if (col == BG_NEEDED)
	    	col = bg = dpptr(GEcurrentDevice())->bg;
	    icol = (unsigned int)col;
	    INTEGER(ans)[i4 +0] = R_RED(icol);
	    INTEGER(ans)[i4 +1] = R_GREEN(icol);
	    INTEGER(ans)[i4 +2] = R_BLUE(icol);
	    INTEGER(ans)[i4 +3] = R_ALPHA(icol);
	}
    }
    UNPROTECT(3);
    return ans;
}

SEXP attribute_hidden do_RGB2hsv(SEXP call, SEXP op, SEXP args, SEXP env)
{
/* (r,g,b) -> (h,s,v) conversion */
    SEXP rgb, dd, ans, names, dmns;
    int n, i, i3;

    checkArity(op, args);

    PROTECT(rgb = coerceVector(CAR(args),REALSXP)); args = CDR(args);
    if(!isMatrix(rgb))
	error(_("rgb is not a matrix (internally)"));
    dd = getAttrib(rgb, R_DimSymbol);
    if(INTEGER(dd)[0] != 3)
	error(_("rgb must have 3 rows (internally)"));
    n = INTEGER(dd)[1];

    PROTECT(ans = allocMatrix(REALSXP, 3, n));
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

/* From here on down moved from graphics.c in 2.7.0 *//* Colour Code */

/* hsv2rgb -- HSV to RGB conversion  */
/* Based on HSV_TO_RGB from Foley and Van Dam First Ed. Page 616 */
/* See Alvy Ray Smith, Color Gamut Transform Pairs, SIGGRAPH '78 */

static char HexDigits[] = "0123456789ABCDEF";

/* rgb2hsv and hsv2rgb are in Utils.h ! */
void hsv2rgb(double h, double s, double v, double *r, double *g, double *b)
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
void rgb2hsv(double r, double g, double b,
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
/* Note: the color "white" is moved */
/* to the top of the database to avoid */
/* its being known as "gray100" */

typedef
struct colorDataBaseEntry {
	char *name;	/* X11 Color Name */
	char *rgb;	/* #RRGGBB String */
	unsigned int code;  /* Internal R Color Code */
} ColorDataBaseEntry;

static int ColorDataBaseSize;

static ColorDataBaseEntry ColorDataBase[] = {
    /* name		rgb         code -- filled in by InitColors() */
    {"white",		"#FFFFFF",	0},
    {"aliceblue",	"#F0F8FF",	0},
    {"antiquewhite",	"#FAEBD7",	0},
    {"antiquewhite1",	"#FFEFDB",	0},
    {"antiquewhite2",	"#EEDFCC",	0},
    {"antiquewhite3",	"#CDC0B0",	0},
    {"antiquewhite4",	"#8B8378",	0},
    {"aquamarine",	"#7FFFD4",	0},
    {"aquamarine1",	"#7FFFD4",	0},
    {"aquamarine2",	"#76EEC6",	0},
    {"aquamarine3",	"#66CDAA",	0},
    {"aquamarine4",	"#458B74",	0},
    {"azure",		"#F0FFFF",	0},
    {"azure1",		"#F0FFFF",	0},
    {"azure2",		"#E0EEEE",	0},
    {"azure3",		"#C1CDCD",	0},
    {"azure4",		"#838B8B",	0},
    {"beige",		"#F5F5DC",	0},
    {"bisque",		"#FFE4C4",	0},
    {"bisque1",		"#FFE4C4",	0},
    {"bisque2",		"#EED5B7",	0},
    {"bisque3",		"#CDB79E",	0},
    {"bisque4",		"#8B7D6B",	0},
    {"black",		"#000000",	0},
    {"blanchedalmond",	"#FFEBCD",	0},
    {"blue",		"#0000FF",	0},
    {"blue1",		"#0000FF",	0},
    {"blue2",		"#0000EE",	0},
    {"blue3",		"#0000CD",	0},
    {"blue4",		"#00008B",	0},
    {"blueviolet",	"#8A2BE2",	0},
    {"brown",		"#A52A2A",	0},
    {"brown1",		"#FF4040",	0},
    {"brown2",		"#EE3B3B",	0},
    {"brown3",		"#CD3333",	0},
    {"brown4",		"#8B2323",	0},
    {"burlywood",	"#DEB887",	0},
    {"burlywood1",	"#FFD39B",	0},
    {"burlywood2",	"#EEC591",	0},
    {"burlywood3",	"#CDAA7D",	0},
    {"burlywood4",	"#8B7355",	0},
    {"cadetblue",	"#5F9EA0",	0},
    {"cadetblue1",	"#98F5FF",	0},
    {"cadetblue2",	"#8EE5EE",	0},
    {"cadetblue3",	"#7AC5CD",	0},
    {"cadetblue4",	"#53868B",	0},
    {"chartreuse",	"#7FFF00",	0},
    {"chartreuse1",	"#7FFF00",	0},
    {"chartreuse2",	"#76EE00",	0},
    {"chartreuse3",	"#66CD00",	0},
    {"chartreuse4",	"#458B00",	0},
    {"chocolate",	"#D2691E",	0},
    {"chocolate1",	"#FF7F24",	0},
    {"chocolate2",	"#EE7621",	0},
    {"chocolate3",	"#CD661D",	0},
    {"chocolate4",	"#8B4513",	0},
    {"coral",		"#FF7F50",	0},
    {"coral1",		"#FF7256",	0},
    {"coral2",		"#EE6A50",	0},
    {"coral3",		"#CD5B45",	0},
    {"coral4",		"#8B3E2F",	0},
    {"cornflowerblue",	"#6495ED",	0},
    {"cornsilk",	"#FFF8DC",	0},
    {"cornsilk1",	"#FFF8DC",	0},
    {"cornsilk2",	"#EEE8CD",	0},
    {"cornsilk3",	"#CDC8B1",	0},
    {"cornsilk4",	"#8B8878",	0},
    {"cyan",		"#00FFFF",	0},
    {"cyan1",		"#00FFFF",	0},
    {"cyan2",		"#00EEEE",	0},
    {"cyan3",		"#00CDCD",	0},
    {"cyan4",		"#008B8B",	0},
    {"darkblue",	"#00008B",	0},
    {"darkcyan",	"#008B8B",	0},
    {"darkgoldenrod",	"#B8860B",	0},
    {"darkgoldenrod1",	"#FFB90F",	0},
    {"darkgoldenrod2",	"#EEAD0E",	0},
    {"darkgoldenrod3",	"#CD950C",	0},
    {"darkgoldenrod4",	"#8B6508",	0},
    {"darkgray",	"#A9A9A9",	0},
    {"darkgreen",	"#006400",	0},
    {"darkgrey",	"#A9A9A9",	0},
    {"darkkhaki",	"#BDB76B",	0},
    {"darkmagenta",	"#8B008B",	0},
    {"darkolivegreen",	"#556B2F",	0},
    {"darkolivegreen1",	"#CAFF70",	0},
    {"darkolivegreen2",	"#BCEE68",	0},
    {"darkolivegreen3",	"#A2CD5A",	0},
    {"darkolivegreen4",	"#6E8B3D",	0},
    {"darkorange",	"#FF8C00",	0},
    {"darkorange1",	"#FF7F00",	0},
    {"darkorange2",	"#EE7600",	0},
    {"darkorange3",	"#CD6600",	0},
    {"darkorange4",	"#8B4500",	0},
    {"darkorchid",	"#9932CC",	0},
    {"darkorchid1",	"#BF3EFF",	0},
    {"darkorchid2",	"#B23AEE",	0},
    {"darkorchid3",	"#9A32CD",	0},
    {"darkorchid4",	"#68228B",	0},
    {"darkred",		"#8B0000",	0},
    {"darksalmon",	"#E9967A",	0},
    {"darkseagreen",	"#8FBC8F",	0},
    {"darkseagreen1",	"#C1FFC1",	0},
    {"darkseagreen2",	"#B4EEB4",	0},
    {"darkseagreen3",	"#9BCD9B",	0},
    {"darkseagreen4",	"#698B69",	0},
    {"darkslateblue",	"#483D8B",	0},
    {"darkslategray",	"#2F4F4F",	0},
    {"darkslategray1",	"#97FFFF",	0},
    {"darkslategray2",	"#8DEEEE",	0},
    {"darkslategray3",	"#79CDCD",	0},
    {"darkslategray4",	"#528B8B",	0},
    {"darkslategrey",	"#2F4F4F",	0},
    {"darkturquoise",	"#00CED1",	0},
    {"darkviolet",	"#9400D3",	0},
    {"deeppink",	"#FF1493",	0},
    {"deeppink1",	"#FF1493",	0},
    {"deeppink2",	"#EE1289",	0},
    {"deeppink3",	"#CD1076",	0},
    {"deeppink4",	"#8B0A50",	0},
    {"deepskyblue",	"#00BFFF",	0},
    {"deepskyblue1",	"#00BFFF",	0},
    {"deepskyblue2",	"#00B2EE",	0},
    {"deepskyblue3",	"#009ACD",	0},
    {"deepskyblue4",	"#00688B",	0},
    {"dimgray",		"#696969",	0},
    {"dimgrey",		"#696969",	0},
    {"dodgerblue",	"#1E90FF",	0},
    {"dodgerblue1",	"#1E90FF",	0},
    {"dodgerblue2",	"#1C86EE",	0},
    {"dodgerblue3",	"#1874CD",	0},
    {"dodgerblue4",	"#104E8B",	0},
    {"firebrick",	"#B22222",	0},
    {"firebrick1",	"#FF3030",	0},
    {"firebrick2",	"#EE2C2C",	0},
    {"firebrick3",	"#CD2626",	0},
    {"firebrick4",	"#8B1A1A",	0},
    {"floralwhite",	"#FFFAF0",	0},
    {"forestgreen",	"#228B22",	0},
    {"gainsboro",	"#DCDCDC",	0},
    {"ghostwhite",	"#F8F8FF",	0},
    {"gold",		"#FFD700",	0},
    {"gold1",		"#FFD700",	0},
    {"gold2",		"#EEC900",	0},
    {"gold3",		"#CDAD00",	0},
    {"gold4",		"#8B7500",	0},
    {"goldenrod",	"#DAA520",	0},
    {"goldenrod1",	"#FFC125",	0},
    {"goldenrod2",	"#EEB422",	0},
    {"goldenrod3",	"#CD9B1D",	0},
    {"goldenrod4",	"#8B6914",	0},
    {"gray",		"#BEBEBE",	0},
    {"gray0",		"#000000",	0},
    {"gray1",		"#030303",	0},
    {"gray2",		"#050505",	0},
    {"gray3",		"#080808",	0},
    {"gray4",		"#0A0A0A",	0},
    {"gray5",		"#0D0D0D",	0},
    {"gray6",		"#0F0F0F",	0},
    {"gray7",		"#121212",	0},
    {"gray8",		"#141414",	0},
    {"gray9",		"#171717",	0},
    {"gray10",		"#1A1A1A",	0},
    {"gray11",		"#1C1C1C",	0},
    {"gray12",		"#1F1F1F",	0},
    {"gray13",		"#212121",	0},
    {"gray14",		"#242424",	0},
    {"gray15",		"#262626",	0},
    {"gray16",		"#292929",	0},
    {"gray17",		"#2B2B2B",	0},
    {"gray18",		"#2E2E2E",	0},
    {"gray19",		"#303030",	0},
    {"gray20",		"#333333",	0},
    {"gray21",		"#363636",	0},
    {"gray22",		"#383838",	0},
    {"gray23",		"#3B3B3B",	0},
    {"gray24",		"#3D3D3D",	0},
    {"gray25",		"#404040",	0},
    {"gray26",		"#424242",	0},
    {"gray27",		"#454545",	0},
    {"gray28",		"#474747",	0},
    {"gray29",		"#4A4A4A",	0},
    {"gray30",		"#4D4D4D",	0},
    {"gray31",		"#4F4F4F",	0},
    {"gray32",		"#525252",	0},
    {"gray33",		"#545454",	0},
    {"gray34",		"#575757",	0},
    {"gray35",		"#595959",	0},
    {"gray36",		"#5C5C5C",	0},
    {"gray37",		"#5E5E5E",	0},
    {"gray38",		"#616161",	0},
    {"gray39",		"#636363",	0},
    {"gray40",		"#666666",	0},
    {"gray41",		"#696969",	0},
    {"gray42",		"#6B6B6B",	0},
    {"gray43",		"#6E6E6E",	0},
    {"gray44",		"#707070",	0},
    {"gray45",		"#737373",	0},
    {"gray46",		"#757575",	0},
    {"gray47",		"#787878",	0},
    {"gray48",		"#7A7A7A",	0},
    {"gray49",		"#7D7D7D",	0},
    {"gray50",		"#7F7F7F",	0},
    {"gray51",		"#828282",	0},
    {"gray52",		"#858585",	0},
    {"gray53",		"#878787",	0},
    {"gray54",		"#8A8A8A",	0},
    {"gray55",		"#8C8C8C",	0},
    {"gray56",		"#8F8F8F",	0},
    {"gray57",		"#919191",	0},
    {"gray58",		"#949494",	0},
    {"gray59",		"#969696",	0},
    {"gray60",		"#999999",	0},
    {"gray61",		"#9C9C9C",	0},
    {"gray62",		"#9E9E9E",	0},
    {"gray63",		"#A1A1A1",	0},
    {"gray64",		"#A3A3A3",	0},
    {"gray65",		"#A6A6A6",	0},
    {"gray66",		"#A8A8A8",	0},
    {"gray67",		"#ABABAB",	0},
    {"gray68",		"#ADADAD",	0},
    {"gray69",		"#B0B0B0",	0},
    {"gray70",		"#B3B3B3",	0},
    {"gray71",		"#B5B5B5",	0},
    {"gray72",		"#B8B8B8",	0},
    {"gray73",		"#BABABA",	0},
    {"gray74",		"#BDBDBD",	0},
    {"gray75",		"#BFBFBF",	0},
    {"gray76",		"#C2C2C2",	0},
    {"gray77",		"#C4C4C4",	0},
    {"gray78",		"#C7C7C7",	0},
    {"gray79",		"#C9C9C9",	0},
    {"gray80",		"#CCCCCC",	0},
    {"gray81",		"#CFCFCF",	0},
    {"gray82",		"#D1D1D1",	0},
    {"gray83",		"#D4D4D4",	0},
    {"gray84",		"#D6D6D6",	0},
    {"gray85",		"#D9D9D9",	0},
    {"gray86",		"#DBDBDB",	0},
    {"gray87",		"#DEDEDE",	0},
    {"gray88",		"#E0E0E0",	0},
    {"gray89",		"#E3E3E3",	0},
    {"gray90",		"#E5E5E5",	0},
    {"gray91",		"#E8E8E8",	0},
    {"gray92",		"#EBEBEB",	0},
    {"gray93",		"#EDEDED",	0},
    {"gray94",		"#F0F0F0",	0},
    {"gray95",		"#F2F2F2",	0},
    {"gray96",		"#F5F5F5",	0},
    {"gray97",		"#F7F7F7",	0},
    {"gray98",		"#FAFAFA",	0},
    {"gray99",		"#FCFCFC",	0},
    {"gray100",		"#FFFFFF",	0},
    {"green",		"#00FF00",	0},
    {"green1",		"#00FF00",	0},
    {"green2",		"#00EE00",	0},
    {"green3",		"#00CD00",	0},
    {"green4",		"#008B00",	0},
    {"greenyellow",	"#ADFF2F",	0},
    {"grey",		"#BEBEBE",	0},
    {"grey0",		"#000000",	0},
    {"grey1",		"#030303",	0},
    {"grey2",		"#050505",	0},
    {"grey3",		"#080808",	0},
    {"grey4",		"#0A0A0A",	0},
    {"grey5",		"#0D0D0D",	0},
    {"grey6",		"#0F0F0F",	0},
    {"grey7",		"#121212",	0},
    {"grey8",		"#141414",	0},
    {"grey9",		"#171717",	0},
    {"grey10",		"#1A1A1A",	0},
    {"grey11",		"#1C1C1C",	0},
    {"grey12",		"#1F1F1F",	0},
    {"grey13",		"#212121",	0},
    {"grey14",		"#242424",	0},
    {"grey15",		"#262626",	0},
    {"grey16",		"#292929",	0},
    {"grey17",		"#2B2B2B",	0},
    {"grey18",		"#2E2E2E",	0},
    {"grey19",		"#303030",	0},
    {"grey20",		"#333333",	0},
    {"grey21",		"#363636",	0},
    {"grey22",		"#383838",	0},
    {"grey23",		"#3B3B3B",	0},
    {"grey24",		"#3D3D3D",	0},
    {"grey25",		"#404040",	0},
    {"grey26",		"#424242",	0},
    {"grey27",		"#454545",	0},
    {"grey28",		"#474747",	0},
    {"grey29",		"#4A4A4A",	0},
    {"grey30",		"#4D4D4D",	0},
    {"grey31",		"#4F4F4F",	0},
    {"grey32",		"#525252",	0},
    {"grey33",		"#545454",	0},
    {"grey34",		"#575757",	0},
    {"grey35",		"#595959",	0},
    {"grey36",		"#5C5C5C",	0},
    {"grey37",		"#5E5E5E",	0},
    {"grey38",		"#616161",	0},
    {"grey39",		"#636363",	0},
    {"grey40",		"#666666",	0},
    {"grey41",		"#696969",	0},
    {"grey42",		"#6B6B6B",	0},
    {"grey43",		"#6E6E6E",	0},
    {"grey44",		"#707070",	0},
    {"grey45",		"#737373",	0},
    {"grey46",		"#757575",	0},
    {"grey47",		"#787878",	0},
    {"grey48",		"#7A7A7A",	0},
    {"grey49",		"#7D7D7D",	0},
    {"grey50",		"#7F7F7F",	0},
    {"grey51",		"#828282",	0},
    {"grey52",		"#858585",	0},
    {"grey53",		"#878787",	0},
    {"grey54",		"#8A8A8A",	0},
    {"grey55",		"#8C8C8C",	0},
    {"grey56",		"#8F8F8F",	0},
    {"grey57",		"#919191",	0},
    {"grey58",		"#949494",	0},
    {"grey59",		"#969696",	0},
    {"grey60",		"#999999",	0},
    {"grey61",		"#9C9C9C",	0},
    {"grey62",		"#9E9E9E",	0},
    {"grey63",		"#A1A1A1",	0},
    {"grey64",		"#A3A3A3",	0},
    {"grey65",		"#A6A6A6",	0},
    {"grey66",		"#A8A8A8",	0},
    {"grey67",		"#ABABAB",	0},
    {"grey68",		"#ADADAD",	0},
    {"grey69",		"#B0B0B0",	0},
    {"grey70",		"#B3B3B3",	0},
    {"grey71",		"#B5B5B5",	0},
    {"grey72",		"#B8B8B8",	0},
    {"grey73",		"#BABABA",	0},
    {"grey74",		"#BDBDBD",	0},
    {"grey75",		"#BFBFBF",	0},
    {"grey76",		"#C2C2C2",	0},
    {"grey77",		"#C4C4C4",	0},
    {"grey78",		"#C7C7C7",	0},
    {"grey79",		"#C9C9C9",	0},
    {"grey80",		"#CCCCCC",	0},
    {"grey81",		"#CFCFCF",	0},
    {"grey82",		"#D1D1D1",	0},
    {"grey83",		"#D4D4D4",	0},
    {"grey84",		"#D6D6D6",	0},
    {"grey85",		"#D9D9D9",	0},
    {"grey86",		"#DBDBDB",	0},
    {"grey87",		"#DEDEDE",	0},
    {"grey88",		"#E0E0E0",	0},
    {"grey89",		"#E3E3E3",	0},
    {"grey90",		"#E5E5E5",	0},
    {"grey91",		"#E8E8E8",	0},
    {"grey92",		"#EBEBEB",	0},
    {"grey93",		"#EDEDED",	0},
    {"grey94",		"#F0F0F0",	0},
    {"grey95",		"#F2F2F2",	0},
    {"grey96",		"#F5F5F5",	0},
    {"grey97",		"#F7F7F7",	0},
    {"grey98",		"#FAFAFA",	0},
    {"grey99",		"#FCFCFC",	0},
    {"grey100",		"#FFFFFF",	0},
    {"honeydew",	"#F0FFF0",	0},
    {"honeydew1",	"#F0FFF0",	0},
    {"honeydew2",	"#E0EEE0",	0},
    {"honeydew3",	"#C1CDC1",	0},
    {"honeydew4",	"#838B83",	0},
    {"hotpink",		"#FF69B4",	0},
    {"hotpink1",	"#FF6EB4",	0},
    {"hotpink2",	"#EE6AA7",	0},
    {"hotpink3",	"#CD6090",	0},
    {"hotpink4",	"#8B3A62",	0},
    {"indianred",	"#CD5C5C",	0},
    {"indianred1",	"#FF6A6A",	0},
    {"indianred2",	"#EE6363",	0},
    {"indianred3",	"#CD5555",	0},
    {"indianred4",	"#8B3A3A",	0},
    {"ivory",		"#FFFFF0",	0},
    {"ivory1",		"#FFFFF0",	0},
    {"ivory2",		"#EEEEE0",	0},
    {"ivory3",		"#CDCDC1",	0},
    {"ivory4",		"#8B8B83",	0},
    {"khaki",		"#F0E68C",	0},
    {"khaki1",		"#FFF68F",	0},
    {"khaki2",		"#EEE685",	0},
    {"khaki3",		"#CDC673",	0},
    {"khaki4",		"#8B864E",	0},
    {"lavender",	"#E6E6FA",	0},
    {"lavenderblush",	"#FFF0F5",	0},
    {"lavenderblush1",	"#FFF0F5",	0},
    {"lavenderblush2",	"#EEE0E5",	0},
    {"lavenderblush3",	"#CDC1C5",	0},
    {"lavenderblush4",	"#8B8386",	0},
    {"lawngreen",	"#7CFC00",	0},
    {"lemonchiffon",	"#FFFACD",	0},
    {"lemonchiffon1",	"#FFFACD",	0},
    {"lemonchiffon2",	"#EEE9BF",	0},
    {"lemonchiffon3",	"#CDC9A5",	0},
    {"lemonchiffon4",	"#8B8970",	0},
    {"lightblue",	"#ADD8E6",	0},
    {"lightblue1",	"#BFEFFF",	0},
    {"lightblue2",	"#B2DFEE",	0},
    {"lightblue3",	"#9AC0CD",	0},
    {"lightblue4",	"#68838B",	0},
    {"lightcoral",	"#F08080",	0},
    {"lightcyan",	"#E0FFFF",	0},
    {"lightcyan1",	"#E0FFFF",	0},
    {"lightcyan2",	"#D1EEEE",	0},
    {"lightcyan3",	"#B4CDCD",	0},
    {"lightcyan4",	"#7A8B8B",	0},
    {"lightgoldenrod",	"#EEDD82",	0},
    {"lightgoldenrod1",	"#FFEC8B",	0},
    {"lightgoldenrod2",	"#EEDC82",	0},
    {"lightgoldenrod3",	"#CDBE70",	0},
    {"lightgoldenrod4",	"#8B814C",	0},
    {"lightgoldenrodyellow","#FAFAD2",	0},
    {"lightgray",	"#D3D3D3",	0},
    {"lightgreen",	"#90EE90",	0},
    {"lightgrey",	"#D3D3D3",	0},
    {"lightpink",	"#FFB6C1",	0},
    {"lightpink1",	"#FFAEB9",	0},
    {"lightpink2",	"#EEA2AD",	0},
    {"lightpink3",	"#CD8C95",	0},
    {"lightpink4",	"#8B5F65",	0},
    {"lightsalmon",	"#FFA07A",	0},
    {"lightsalmon1",	"#FFA07A",	0},
    {"lightsalmon2",	"#EE9572",	0},
    {"lightsalmon3",	"#CD8162",	0},
    {"lightsalmon4",	"#8B5742",	0},
    {"lightseagreen",	"#20B2AA",	0},
    {"lightskyblue",	"#87CEFA",	0},
    {"lightskyblue1",	"#B0E2FF",	0},
    {"lightskyblue2",	"#A4D3EE",	0},
    {"lightskyblue3",	"#8DB6CD",	0},
    {"lightskyblue4",	"#607B8B",	0},
    {"lightslateblue",	"#8470FF",	0},
    {"lightslategray",	"#778899",	0},
    {"lightslategrey",	"#778899",	0},
    {"lightsteelblue",	"#B0C4DE",	0},
    {"lightsteelblue1",	"#CAE1FF",	0},
    {"lightsteelblue2",	"#BCD2EE",	0},
    {"lightsteelblue3",	"#A2B5CD",	0},
    {"lightsteelblue4",	"#6E7B8B",	0},
    {"lightyellow",	"#FFFFE0",	0},
    {"lightyellow1",	"#FFFFE0",	0},
    {"lightyellow2",	"#EEEED1",	0},
    {"lightyellow3",	"#CDCDB4",	0},
    {"lightyellow4",	"#8B8B7A",	0},
    {"limegreen",	"#32CD32",	0},
    {"linen",		"#FAF0E6",	0},
    {"magenta",		"#FF00FF",	0},
    {"magenta1",	"#FF00FF",	0},
    {"magenta2",	"#EE00EE",	0},
    {"magenta3",	"#CD00CD",	0},
    {"magenta4",	"#8B008B",	0},
    {"maroon",		"#B03060",	0},
    {"maroon1",		"#FF34B3",	0},
    {"maroon2",		"#EE30A7",	0},
    {"maroon3",		"#CD2990",	0},
    {"maroon4",		"#8B1C62",	0},
    {"mediumaquamarine","#66CDAA",	0},
    {"mediumblue",	"#0000CD",	0},
    {"mediumorchid",	"#BA55D3",	0},
    {"mediumorchid1",	"#E066FF",	0},
    {"mediumorchid2",	"#D15FEE",	0},
    {"mediumorchid3",	"#B452CD",	0},
    {"mediumorchid4",	"#7A378B",	0},
    {"mediumpurple",	"#9370DB",	0},
    {"mediumpurple1",	"#AB82FF",	0},
    {"mediumpurple2",	"#9F79EE",	0},
    {"mediumpurple3",	"#8968CD",	0},
    {"mediumpurple4",	"#5D478B",	0},
    {"mediumseagreen",	"#3CB371",	0},
    {"mediumslateblue",	"#7B68EE",	0},
    {"mediumspringgreen","#00FA9A",	0},
    {"mediumturquoise",	"#48D1CC",	0},
    {"mediumvioletred",	"#C71585",	0},
    {"midnightblue",	"#191970",	0},
    {"mintcream",	"#F5FFFA",	0},
    {"mistyrose",	"#FFE4E1",	0},
    {"mistyrose1",	"#FFE4E1",	0},
    {"mistyrose2",	"#EED5D2",	0},
    {"mistyrose3",	"#CDB7B5",	0},
    {"mistyrose4",	"#8B7D7B",	0},
    {"moccasin",	"#FFE4B5",	0},
    {"navajowhite",	"#FFDEAD",	0},
    {"navajowhite1",	"#FFDEAD",	0},
    {"navajowhite2",	"#EECFA1",	0},
    {"navajowhite3",	"#CDB38B",	0},
    {"navajowhite4",	"#8B795E",	0},
    {"navy",		"#000080",	0},
    {"navyblue",	"#000080",	0},
    {"oldlace",		"#FDF5E6",	0},
    {"olivedrab",	"#6B8E23",	0},
    {"olivedrab1",	"#C0FF3E",	0},
    {"olivedrab2",	"#B3EE3A",	0},
    {"olivedrab3",	"#9ACD32",	0},
    {"olivedrab4",	"#698B22",	0},
    {"orange",		"#FFA500",	0},
    {"orange1",		"#FFA500",	0},
    {"orange2",		"#EE9A00",	0},
    {"orange3",		"#CD8500",	0},
    {"orange4",		"#8B5A00",	0},
    {"orangered",	"#FF4500",	0},
    {"orangered1",	"#FF4500",	0},
    {"orangered2",	"#EE4000",	0},
    {"orangered3",	"#CD3700",	0},
    {"orangered4",	"#8B2500",	0},
    {"orchid",		"#DA70D6",	0},
    {"orchid1",		"#FF83FA",	0},
    {"orchid2",		"#EE7AE9",	0},
    {"orchid3",		"#CD69C9",	0},
    {"orchid4",		"#8B4789",	0},
    {"palegoldenrod",	"#EEE8AA",	0},
    {"palegreen",	"#98FB98",	0},
    {"palegreen1",	"#9AFF9A",	0},
    {"palegreen2",	"#90EE90",	0},
    {"palegreen3",	"#7CCD7C",	0},
    {"palegreen4",	"#548B54",	0},
    {"paleturquoise",	"#AFEEEE",	0},
    {"paleturquoise1",	"#BBFFFF",	0},
    {"paleturquoise2",	"#AEEEEE",	0},
    {"paleturquoise3",	"#96CDCD",	0},
    {"paleturquoise4",	"#668B8B",	0},
    {"palevioletred",	"#DB7093",	0},
    {"palevioletred1",	"#FF82AB",	0},
    {"palevioletred2",	"#EE799F",	0},
    {"palevioletred3",	"#CD6889",	0},
    {"palevioletred4",	"#8B475D",	0},
    {"papayawhip",	"#FFEFD5",	0},
    {"peachpuff",	"#FFDAB9",	0},
    {"peachpuff1",	"#FFDAB9",	0},
    {"peachpuff2",	"#EECBAD",	0},
    {"peachpuff3",	"#CDAF95",	0},
    {"peachpuff4",	"#8B7765",	0},
    {"peru",		"#CD853F",	0},
    {"pink",		"#FFC0CB",	0},
    {"pink1",		"#FFB5C5",	0},
    {"pink2",		"#EEA9B8",	0},
    {"pink3",		"#CD919E",	0},
    {"pink4",		"#8B636C",	0},
    {"plum",		"#DDA0DD",	0},
    {"plum1",		"#FFBBFF",	0},
    {"plum2",		"#EEAEEE",	0},
    {"plum3",		"#CD96CD",	0},
    {"plum4",		"#8B668B",	0},
    {"powderblue",	"#B0E0E6",	0},
    {"purple",		"#A020F0",	0},
    {"purple1",		"#9B30FF",	0},
    {"purple2",		"#912CEE",	0},
    {"purple3",		"#7D26CD",	0},
    {"purple4",		"#551A8B",	0},
    {"red",		"#FF0000",	0},
    {"red1",		"#FF0000",	0},
    {"red2",		"#EE0000",	0},
    {"red3",		"#CD0000",	0},
    {"red4",		"#8B0000",	0},
    {"rosybrown",	"#BC8F8F",	0},
    {"rosybrown1",	"#FFC1C1",	0},
    {"rosybrown2",	"#EEB4B4",	0},
    {"rosybrown3",	"#CD9B9B",	0},
    {"rosybrown4",	"#8B6969",	0},
    {"royalblue",	"#4169E1",	0},
    {"royalblue1",	"#4876FF",	0},
    {"royalblue2",	"#436EEE",	0},
    {"royalblue3",	"#3A5FCD",	0},
    {"royalblue4",	"#27408B",	0},
    {"saddlebrown",	"#8B4513",	0},
    {"salmon",		"#FA8072",	0},
    {"salmon1",		"#FF8C69",	0},
    {"salmon2",		"#EE8262",	0},
    {"salmon3",		"#CD7054",	0},
    {"salmon4",		"#8B4C39",	0},
    {"sandybrown",	"#F4A460",	0},
    {"seagreen",	"#2E8B57",	0},
    {"seagreen1",	"#54FF9F",	0},
    {"seagreen2",	"#4EEE94",	0},
    {"seagreen3",	"#43CD80",	0},
    {"seagreen4",	"#2E8B57",	0},
    {"seashell",	"#FFF5EE",	0},
    {"seashell1",	"#FFF5EE",	0},
    {"seashell2",	"#EEE5DE",	0},
    {"seashell3",	"#CDC5BF",	0},
    {"seashell4",	"#8B8682",	0},
    {"sienna",		"#A0522D",	0},
    {"sienna1",		"#FF8247",	0},
    {"sienna2",		"#EE7942",	0},
    {"sienna3",		"#CD6839",	0},
    {"sienna4",		"#8B4726",	0},
    {"skyblue",		"#87CEEB",	0},
    {"skyblue1",	"#87CEFF",	0},
    {"skyblue2",	"#7EC0EE",	0},
    {"skyblue3",	"#6CA6CD",	0},
    {"skyblue4",	"#4A708B",	0},
    {"slateblue",	"#6A5ACD",	0},
    {"slateblue1",	"#836FFF",	0},
    {"slateblue2",	"#7A67EE",	0},
    {"slateblue3",	"#6959CD",	0},
    {"slateblue4",	"#473C8B",	0},
    {"slategray",	"#708090",	0},
    {"slategray1",	"#C6E2FF",	0},
    {"slategray2",	"#B9D3EE",	0},
    {"slategray3",	"#9FB6CD",	0},
    {"slategray4",	"#6C7B8B",	0},
    {"slategrey",	"#708090",	0},
    {"snow",		"#FFFAFA",	0},
    {"snow1",		"#FFFAFA",	0},
    {"snow2",		"#EEE9E9",	0},
    {"snow3",		"#CDC9C9",	0},
    {"snow4",		"#8B8989",	0},
    {"springgreen",	"#00FF7F",	0},
    {"springgreen1",	"#00FF7F",	0},
    {"springgreen2",	"#00EE76",	0},
    {"springgreen3",	"#00CD66",	0},
    {"springgreen4",	"#008B45",	0},
    {"steelblue",	"#4682B4",	0},
    {"steelblue1",	"#63B8FF",	0},
    {"steelblue2",	"#5CACEE",	0},
    {"steelblue3",	"#4F94CD",	0},
    {"steelblue4",	"#36648B",	0},
    {"tan",		"#D2B48C",	0},
    {"tan1",		"#FFA54F",	0},
    {"tan2",		"#EE9A49",	0},
    {"tan3",		"#CD853F",	0},
    {"tan4",		"#8B5A2B",	0},
    {"thistle",		"#D8BFD8",	0},
    {"thistle1",	"#FFE1FF",	0},
    {"thistle2",	"#EED2EE",	0},
    {"thistle3",	"#CDB5CD",	0},
    {"thistle4",	"#8B7B8B",	0},
    {"tomato",		"#FF6347",	0},
    {"tomato1",		"#FF6347",	0},
    {"tomato2",		"#EE5C42",	0},
    {"tomato3",		"#CD4F39",	0},
    {"tomato4",		"#8B3626",	0},
    {"turquoise",	"#40E0D0",	0},
    {"turquoise1",	"#00F5FF",	0},
    {"turquoise2",	"#00E5EE",	0},
    {"turquoise3",	"#00C5CD",	0},
    {"turquoise4",	"#00868B",	0},
    {"violet",		"#EE82EE",	0},
    {"violetred",	"#D02090",	0},
    {"violetred1",	"#FF3E96",	0},
    {"violetred2",	"#EE3A8C",	0},
    {"violetred3",	"#CD3278",	0},
    {"violetred4",	"#8B2252",	0},
    {"wheat",		"#F5DEB3",	0},
    {"wheat1",		"#FFE7BA",	0},
    {"wheat2",		"#EED8AE",	0},
    {"wheat3",		"#CDBA96",	0},
    {"wheat4",		"#8B7E66",	0},
    {"whitesmoke",	"#F5F5F5",	0},
    {"yellow",		"#FFFF00",	0},
    {"yellow1",		"#FFFF00",	0},
    {"yellow2",		"#EEEE00",	0},
    {"yellow3",		"#CDCD00",	0},
    {"yellow4",		"#8B8B00",	0},
    {"yellowgreen",	"#9ACD32",	0},
    {NULL,		NULL,		0}
};


static void setpalette(const char **palette)
{
    int i;
    for (i = 0; (i < COLOR_TABLE_SIZE) && palette[i]; i++)
	R_ColorTable[i] = name2col(palette[i]);
    R_ColorTableSize = i;
}

SEXP attribute_hidden do_palette(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP val, ans;
    unsigned int color[COLOR_TABLE_SIZE];
    int i, n;
    checkArity(op,args);
    /* Record the current palette */
    PROTECT(ans = allocVector(STRSXP, R_ColorTableSize));
    for (i = 0; i < R_ColorTableSize; i++)
	SET_STRING_ELT(ans, i, mkChar(col2name(R_ColorTable[i])));
    val = CAR(args);
    if (!isString(val)) error(_("invalid argument type"));
    if ((n=length(val)) == 1) {
	if (StrMatch("default", CHAR(STRING_ELT(val, 0)))) /* ASCII */
	    setpalette(DefaultPalette);
	else error(_("unknown palette (need >= 2 colors)"));
    }
    else if (n > 1) {
	if (n > COLOR_TABLE_SIZE)
	     error(_("maximum number of colors exceeded"));
	for (i = 0; i < n; i++)
	    color[i] = char2col(CHAR(STRING_ELT(val, i)));
	for (i = 0; i < n; i++)
	    R_ColorTable[i] = color[i];
	R_ColorTableSize = n;
    }
    UNPROTECT(1);
    return ans;
}

SEXP attribute_hidden do_colors(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;
    int n;

    for (n = 0; ColorDataBase[n].name != NULL; n++) ;
    PROTECT(ans = allocVector(STRSXP, n));
    for (n = 0; ColorDataBase[n].name != NULL; n++)
	SET_STRING_ELT(ans, n, mkChar(ColorDataBase[n].name));
    UNPROTECT(1);
    return ans;
}

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
static unsigned int rgb2col(const char *rgb)
{
    unsigned int r=0, g=0, b=0, a=0; /* -Wall */
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

unsigned int attribute_hidden name2col(const char *nm)
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
    return 0;		/* never occurs but avoid compiler warnings */
}

static double number2col(const char *nm, double bg)
{
    int indx;
    char *ptr;
    indx = strtod(nm, &ptr);
    if(*ptr) error(_("invalid color specification '%s'"), nm);
    if(indx == 0) return bg;
    else return R_ColorTable[(indx-1) % R_ColorTableSize];
}


static char ColBuf[10];

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


/* Internal to External Color Representation */
/* Search the color name database first */
/* If this fails, create an #RRGGBB string */

/* used in grid */
const char *col2name(unsigned int col)
{
    int i;

    if(R_OPAQUE(col)) {
	for(i=0 ; ColorDataBase[i].name ; i++) {
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

static double str2col(const char *s, double bg)
{
    if(s[0] == '#') return rgb2col(s);
    /* This seems rather strange,
       and made this depend on base graphics.
       Looks like it was an artefact of conversion in col2rgb().
    */
    else if(isdigit((int)s[0])) return number2col(s, bg);
    else return name2col(s);
}

/* used in grDevices, public */
unsigned int R_GE_str2col(const char *s)
{
    return (unsigned int)str2col(s, R_TRANWHITE);
}

/* Convert a sexp element to an R color desc */
/* We Assume that Checks Have Been Done */

/* used in grid/src/gpar.c */
unsigned int RGBpar3(SEXP x, int i, unsigned int bg)
{
    int indx;
    switch(TYPEOF(x))
    {
    case STRSXP:
	return (unsigned int)str2col(CHAR(STRING_ELT(x, i)), bg);
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
	indx = REAL(x)[i];
	break;
	   default:
	   warning(_("supplied color is not numeric nor character"));
	   return bg;
    }
    if (indx <= 0) return bg;
    else return R_ColorTable[(indx-1) % R_ColorTableSize];
}

unsigned int RGBpar(SEXP x, int i)
{
    return RGBpar3(x, i, R_TRANWHITE);
}


/*
 * Is element i of a colour object NA (or NULL)?
 */
Rboolean attribute_hidden isNAcol(SEXP col, int index, int ncol)
{
    Rboolean result = TRUE; /* -Wall */

    if (isNull(col))
	result = TRUE;
    else {
	if (isLogical(col))
	    result = LOGICAL(col)[index % ncol] == NA_LOGICAL;
	else if (isString(col))
	    result = strcmp(CHAR(STRING_ELT(col, index % ncol)), "NA") == 0;
	else if (isInteger(col))
	    result = INTEGER(col)[index % ncol] == NA_INTEGER;
	else if (isReal(col))
	    result = !R_FINITE(REAL(col)[index % ncol]);
	else
	    error(_("Invalid color specification"));
    }
    return result;
}

/* Initialize the Color Databases */

void attribute_hidden InitColors(void)
{
    int i;

    /* Initialize the Color Database */
    for(i = 0 ; ColorDataBase[i].name ; i++)
	ColorDataBase[i].code = rgb2col(ColorDataBase[i].rgb);
    ColorDataBaseSize = i;

    /* Install Default Palette */
    for(i = 0 ; DefaultPalette[i] ; i++)
	R_ColorTable[i] = name2col(DefaultPalette[i]);
    R_ColorTableSize = i;
}
