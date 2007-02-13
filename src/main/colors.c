/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995-2007  Robert Gentleman, Ross Ihaka and the
 *			     R Development Core Team
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
 *  A copy of the GNU General Public License is available via WWW at
 *  http://www.gnu.org/copyleft/gpl.html.  You can also obtain it by
 *  writing to the Free Software Foundation, Inc., 51 Franklin Street
 *  Fifth Floor, Boston, MA 02110-1301  USA.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Graphics.h>
#include <Rmath.h>

unsigned int char2col(char *s)
{
    if (s[0] == '#') return rgb2col(s);
    else return name2col(s);
}

unsigned int ScaleColor(double x)
{
    if (!R_FINITE(x) || x < 0.0 || x > 1.0)
	error(_("color intensity %g, not in [0,1]"), x);
    return (unsigned int)(255*x + 0.5);
}

unsigned int CheckColor(int x)
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

static void setpalette(char **palette)
{
    int i;
    for (i = 0; (i<COLOR_TABLE_SIZE) && palette[i]; i++)
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
    if (!isString(val)) errorcall(call, _("invalid argument type"));
    if ((n=length(val)) == 1) {
	if (StrMatch("default", CHAR(STRING_ELT(val, 0)))) /* ASCII */
	    setpalette(DefaultPalette);
	else errorcall(call, _("unknown palette (need >= 2 colors)"));
    }
    else if (n > 1) {
	if (n > COLOR_TABLE_SIZE)
	     errorcall(call, _("maximum number of colors exceeded"));
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
    n = 0;
    while (ColorDataBase[n].name!=NULL)
	n++;
    PROTECT(ans = allocVector(STRSXP, n));
    n = 0;
    while (ColorDataBase[n].name!=NULL) {
	SET_STRING_ELT(ans, n, mkChar(ColorDataBase[n].name));
	n++;
    }
    UNPROTECT(1);
    return ans;
}

SEXP attribute_hidden do_hsv(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP c, h, s, v, gm, a;
    double hh, ss, vv, gg, aa, r, g, b;
    int i, max, nh, ns, nv, ng, na;

    checkArity(op, args);

    PROTECT(h = coerceVector(CAR(args),REALSXP)); args = CDR(args);
    PROTECT(s = coerceVector(CAR(args),REALSXP)); args = CDR(args);
    PROTECT(v = coerceVector(CAR(args),REALSXP)); args = CDR(args);
    PROTECT(gm = coerceVector(CAR(args),REALSXP)); args = CDR(args);
    PROTECT(a = coerceVector(CAR(args),REALSXP)); args = CDR(args);

    nh = LENGTH(h);
    ns = LENGTH(s);
    nv = LENGTH(v);
    ng = LENGTH(gm);
    na = LENGTH(a);
    if (nh <= 0 || ns <= 0 || nv <= 0 || ng <= 0 || na <= 0) {
	UNPROTECT(5);
	return(allocVector(STRSXP, 0));
    }
    max = nh;
    if (max < ns) max = ns;
    if (max < nv) max = nv;
    if (max < ng) max = ng;
    if (max < na) max = na;
    PROTECT(c = allocVector(STRSXP, max));
    if(max == 0) return(c);

    for (i = 0; i < max; i++) {
	hh = REAL(h)[i % nh];
	ss = REAL(s)[i % ns];
	vv = REAL(v)[i % nv];
	gg = REAL(gm)[i % ng];
	aa = REAL(a)[i % na];
	if (hh < 0 || hh > 1 || ss < 0 || ss > 1 || vv < 0 || vv > 1 ||
	    aa < 0 || aa > 1)
	    errorcall(call, _("invalid HSV color"));
	hsv2rgb(hh, ss, vv, &r, &g, &b);
	r = pow(r, gg);
	g = pow(g, gg);
	b = pow(b, gg);
	SET_STRING_ELT(c, i, mkChar(RGBA2rgb(ScaleColor(r),
					     ScaleColor(g),
					     ScaleColor(b),
					     ScaleAlpha(aa))));
    }
    UNPROTECT(6);
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
            errorcall(call, _("invalid hcl color"));
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
	errorcall(call, _("invalid names vector"));
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
	    errorcall(call, _("invalid gray level, must be in [0,1]."));
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
    unsigned int col;
    int n, i, i4;

    checkArity(op, args);

    PROTECT(colors = coerceVector(CAR(args),STRSXP));
    n = LENGTH(colors);
    PROTECT(ans = allocMatrix(INTSXP, 4, n));
    PROTECT(dmns = allocVector(VECSXP, 2));

    PROTECT(names = allocVector(STRSXP, 4));
    SET_STRING_ELT(names, 0, mkChar("red"));
    SET_STRING_ELT(names, 1, mkChar("green"));
    SET_STRING_ELT(names, 2, mkChar("blue"));
    SET_STRING_ELT(names, 3, mkChar("alpha"));
    SET_VECTOR_ELT(dmns, 0, names);
    UNPROTECT(1);/*names*/
    if ((names = getAttrib(colors, R_NamesSymbol)) != R_NilValue)
	SET_VECTOR_ELT(dmns, 1, names);
    setAttrib(ans, R_DimNamesSymbol, dmns);
    for(i = i4 = 0; i < n; i++, i4 += 4) {
	col = str2col(CHAR(STRING_ELT(colors, i)));
	INTEGER(ans)[i4 +0] = R_RED(col);
	INTEGER(ans)[i4 +1] = R_GREEN(col);
	INTEGER(ans)[i4 +2] = R_BLUE(col);
	INTEGER(ans)[i4 +3] = R_ALPHA(col);
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
	errorcall(call, _("rgb is not a matrix (internally)"));
    dd = getAttrib(rgb, R_DimSymbol);
    if(INTEGER(dd)[0] != 3)
	errorcall(call, _("rgb must have 3 rows (internally)"));
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
