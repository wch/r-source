/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995-2000  Robert Gentleman, Ross Ihaka and the
 *                           R Development Core Team
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

#include "Defn.h"
#include "Mathlib.h"
#include "Graphics.h"

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
    unsigned int ncols[COLOR_TABLE_SIZE];
    int i, n;
    checkArity(op,args);
    /* Record the current palette */
    PROTECT(ans = allocVector(STRSXP, R_ColorTableSize));
    for (i = 0; i < R_ColorTableSize; i++)
	STRING(ans)[i] = mkChar(col2name(R_ColorTable[i]));
    val = CAR(args);
    if (!isString(val)) errorcall(call, "invalid argument type");
    if ((n=length(val)) == 1) {
	if (StrMatch("default", CHAR(STRING(val)[0])))
	    setpalette(DefaultPalette);
	else errorcall(call, "unknown palette (need >= 2 colors)");
    }
    else if (n > 1) {
	for (i = 0; i < n; i++)
	    ncols[i] = char2col(CHAR(STRING(val)[i]));
	for (i = 0; i < n; i++)
	    R_ColorTable[i] = ncols[i];
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
    while (ColorDataBase[n].name!=NULL)
	STRING(ans)[n++] = mkChar(ColorDataBase[n].name);
    UNPROTECT(1);
    return ans;
}

SEXP do_hsv(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP c, h, s, v, gm;
    double hh, ss, vv, gg, r, g, b;
    int i, min, max, nh, ns, nv, ng;

    checkArity(op, args);

    PROTECT(h = coerceVector(CAR(args),REALSXP)); args = CDR(args);
    PROTECT(s = coerceVector(CAR(args),REALSXP)); args = CDR(args);
    PROTECT(v = coerceVector(CAR(args),REALSXP)); args = CDR(args);
    PROTECT(gm = coerceVector(CAR(args),REALSXP)); args = CDR(args);

    nh = LENGTH(h);
    ns = LENGTH(s);
    nv = LENGTH(v);
    ng = LENGTH(gm);
    max = nh;
    if (max < ns) max = ns;
    if (max < nv) max = nv;
    if (max < ng) max = ng;
    min = nh;
    if (min > ns) min = ns;
    if (min > nv) min = nv;
    if (min > ng) min = ng;
    if (min <= 0)
	errorcall(call, "invalid argument length");

    PROTECT(c = allocVector(STRSXP, max));
    for (i = 0; i < max; i++) {
	hh = REAL(h)[i % nh];
	ss = REAL(s)[i % ns];
	vv = REAL(v)[i % nv];
	gg = REAL(gm)[i % ng];
	if (hh < 0 || hh > 1 || ss < 0 || ss > 1 || vv < 0 || vv > 1)
	    errorcall(call, "invalid HSV color");
	hsv2rgb(&hh, &ss, &vv, &r, &g, &b);
	r = pow(r, gg);
	g = pow(g, gg);
	b = pow(b, gg);
	STRING(c)[i] = mkChar(RGB2rgb(ScaleColor(r),
				      ScaleColor(g),
				      ScaleColor(b)));
    }
    UNPROTECT(5);
    return c;
}


SEXP do_rgb(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP c, r, g, b, n;
    int i, min, max, nr, ng, nb;
    unsigned int ri, gi, bi;

    checkArity(op, args);

    PROTECT(r = coerceVector(CAR(args), REALSXP)); args = CDR(args);
    PROTECT(g = coerceVector(CAR(args), REALSXP)); args = CDR(args);
    PROTECT(b = coerceVector(CAR(args), REALSXP)); args = CDR(args);
    PROTECT(n = coerceVector(CAR(args), STRSXP)); args = CDR(args);

    nr = LENGTH(r); ng = LENGTH(g); nb = LENGTH(b);
    max = nr; if (max < ng) max = ng; if (max < nb) max = nb;
    min = nr; if (min > ng) min = ng; if (min > nb) min = nb;
    if (min <= 0) errorcall(call, "invalid argument length");

    if (length(n) != 0 && length(n) != max)
	errorcall(call, "invalid names vector");

    PROTECT(c = allocVector(STRSXP, max));
    for (i = 0; i < max; i++) {
	ri = ScaleColor(REAL(r)[i%nr]);
	gi = ScaleColor(REAL(g)[i%ng]);
	bi = ScaleColor(REAL(b)[i%nb]);
	STRING(c)[i] = mkChar(RGB2rgb(ri, gi, bi));
    }
    if (length(n) != 0)
	setAttrib(c, R_NamesSymbol, n);
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
	STRING(ans)[i] = mkChar(RGB2rgb(ilevel, ilevel, ilevel));
    }
    UNPROTECT(2);
    return ans;
}

