/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995-2001  Robert Gentleman, Ross Ihaka and the
 *			     R Development Core Team
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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
	error("color intensity %g, not in [0,1]",x);
    return (unsigned int)(255*x + 0.5);
}
unsigned int CheckColor(int x)
{
    if (x == NA_INTEGER || x < 0 || x > 255)
	error("color intensity %d, not in 0:255", x);
    return (unsigned int)x;
}


static void setpalette(char **palette)
{
    int i;
    for (i = 0; (i<COLOR_TABLE_SIZE) && palette[i]; i++)
	R_ColorTable[i] = name2col(palette[i]);
    R_ColorTableSize = i;
}

SEXP do_palette(SEXP call, SEXP op, SEXP args, SEXP rho)
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
    if (!isString(val)) errorcall(call, "invalid argument type");
    if ((n=length(val)) == 1) {
	if (StrMatch("default", CHAR(STRING_ELT(val, 0))))
	    setpalette(DefaultPalette);
	else errorcall(call, "unknown palette (need >= 2 colors)");
    }
    else if (n > 1) {
	if (n > COLOR_TABLE_SIZE)
	     errorcall(call, "maximum number of colors exceeded");
	for (i = 0; i < n; i++)
	    color[i] = char2col(CHAR(STRING_ELT(val, i)));
	for (i = 0; i < n; i++)
	    R_ColorTable[i] = color[i];
	R_ColorTableSize = n;
    }
    UNPROTECT(1);
    return ans;
}

SEXP do_colors(SEXP call, SEXP op, SEXP args, SEXP rho)
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

SEXP do_hsv(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP c, h, s, v, gm;
    double hh, ss, vv, gg, r, g, b;
    int i, max, nh, ns, nv, ng;

    checkArity(op, args);

    PROTECT(h = coerceVector(CAR(args),REALSXP)); args = CDR(args);
    PROTECT(s = coerceVector(CAR(args),REALSXP)); args = CDR(args);
    PROTECT(v = coerceVector(CAR(args),REALSXP)); args = CDR(args);
    PROTECT(gm = coerceVector(CAR(args),REALSXP)); args = CDR(args);

    nh = LENGTH(h);
    ns = LENGTH(s);
    nv = LENGTH(v);
    ng = LENGTH(gm);
    if (nh <= 0 || ns <= 0 || nv <= 0 || ng <= 0) {
	UNPROTECT(4);
	return(allocVector(STRSXP, 0));
    }
    max = nh;
    if (max < ns) max = ns;
    if (max < nv) max = nv;
    if (max < ng) max = ng;
    PROTECT(c = allocVector(STRSXP, max));
    if(max == 0) return(c);

    for (i = 0; i < max; i++) {
	hh = REAL(h)[i % nh];
	ss = REAL(s)[i % ns];
	vv = REAL(v)[i % nv];
	gg = REAL(gm)[i % ng];
	if (hh < 0 || hh > 1 || ss < 0 || ss > 1 || vv < 0 || vv > 1)
	    errorcall(call, "invalid HSV color");
	hsv2rgb(hh, ss, vv, &r, &g, &b);
	r = pow(r, gg);
	g = pow(g, gg);
	b = pow(b, gg);
	SET_STRING_ELT(c, i, mkChar(RGB2rgb(ScaleColor(r),
					    ScaleColor(g),
					    ScaleColor(b))));
    }
    UNPROTECT(5);
    return c;
}


SEXP do_rgb(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP c, r, g, b, nam;
    int OP, i, l_max, nr, ng, nb;
    Rboolean max_1 = FALSE;
    double mV = 0.0; /* -Wall */

    checkArity(op, args);
    OP = PRIMVAL(op);
    if(OP) {/* op == 1:  rgb256() :*/
	PROTECT(r = coerceVector(CAR(args), INTSXP)); args = CDR(args);
	PROTECT(g = coerceVector(CAR(args), INTSXP)); args = CDR(args);
	PROTECT(b = coerceVector(CAR(args), INTSXP)); args = CDR(args);
    }
    else {
	PROTECT(r = coerceVector(CAR(args), REALSXP)); args = CDR(args);
	PROTECT(g = coerceVector(CAR(args), REALSXP)); args = CDR(args);
	PROTECT(b = coerceVector(CAR(args), REALSXP)); args = CDR(args);
	mV = asReal(CAR(args));			       args = CDR(args);
	max_1 = (mV == 1.);
    }

    nr = LENGTH(r); ng = LENGTH(g); nb = LENGTH(b);
    if (nr <= 0 || ng <= 0 || nb <= 0) {
	UNPROTECT(3);
	return(allocVector(STRSXP, 0));
    }
    l_max = nr; if (l_max < ng) l_max = ng; if (l_max < nb) l_max = nb;

    PROTECT(nam = coerceVector(CAR(args), STRSXP)); args = CDR(args);
    if (length(nam) != 0 && length(nam) != l_max)
	errorcall(call, "invalid names vector");
    PROTECT(c = allocVector(STRSXP, l_max));

#define _R_set_c_RGB(_R,_G,_B)				\
    for (i = 0; i < l_max; i++)				\
	SET_STRING_ELT(c, i, mkChar(RGB2rgb(_R,_G,_B)))

    if(OP) { /* OP == 1:  rgb256() :*/
	_R_set_c_RGB(CheckColor(INTEGER(r)[i%nr]),
		     CheckColor(INTEGER(g)[i%ng]),
		     CheckColor(INTEGER(b)[i%nb]));
    }
    else if(max_1) {
	_R_set_c_RGB(ScaleColor(REAL(r)[i%nr]),
		     ScaleColor(REAL(g)[i%ng]),
		     ScaleColor(REAL(b)[i%nb]));
    }
    else { /* maxColorVal not in {1, 255} */
	_R_set_c_RGB(ScaleColor(REAL(r)[i%nr] / mV),
		     ScaleColor(REAL(g)[i%ng] / mV),
		     ScaleColor(REAL(b)[i%nb] / mV));
    }
    if (length(nam) != 0)
	setAttrib(c, R_NamesSymbol, nam);
    UNPROTECT(5);
    return c;
}

SEXP do_gray(SEXP call, SEXP op, SEXP args, SEXP env)
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
	    errorcall(call, "invalid gray level, must be in [0,1].");
	ilevel = 255 * level + 0.5;
	SET_STRING_ELT(ans, i, mkChar(RGB2rgb(ilevel, ilevel, ilevel)));
    }
    UNPROTECT(2);
    return ans;
}

SEXP do_col2RGB(SEXP call, SEXP op, SEXP args, SEXP env)
{
/* colorname, "#rrggbb" or "col.number" to (r,g,b) conversion */
    SEXP colors, ans, names, dmns;
    unsigned int col;
    int n, i, i3;

    checkArity(op, args);

    PROTECT(colors = coerceVector(CAR(args),STRSXP));
    n = LENGTH(colors);
    PROTECT(ans = allocMatrix(INTSXP, 3, n));
    PROTECT(dmns = allocVector(VECSXP, 2));

    PROTECT(names = allocVector(STRSXP, 3));
    SET_STRING_ELT(names, 0, mkChar("red"));
    SET_STRING_ELT(names, 1, mkChar("green"));
    SET_STRING_ELT(names, 2, mkChar("blue"));
    SET_VECTOR_ELT(dmns, 0, names);
    UNPROTECT(1);/*names*/
    if ((names = getAttrib(colors, R_NamesSymbol)) != R_NilValue)
	SET_VECTOR_ELT(dmns, 1, names);
    setAttrib(ans, R_DimNamesSymbol, dmns);
    for(i = i3 = 0; i < n; i++, i3 += 3) {
	col = str2col(CHAR(STRING_ELT(colors, i)));
	INTEGER(ans)[i3 +0] = R_RED(col);
	INTEGER(ans)[i3 +1] = R_GREEN(col);
	INTEGER(ans)[i3 +2] = R_BLUE(col);
    }
    UNPROTECT(3);
    return ans;
}
