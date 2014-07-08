/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2014  The R Core Team
 *  Copyright (C) 2002--2009  The R Foundation
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
# include <config.h>
#endif

#include <Defn.h>
#include <float.h>  /* for DBL_MAX */
#include <Graphics.h>
#include <Print.h>
#include <Rmath.h>  // Rexp10, fmin2, fmax2, imax2

#include "graphics.h"

static R_INLINE void TypeCheck(SEXP s, SEXPTYPE type)
{
    if (TYPEOF(s) != type)
	error("invalid type passed to graphics function");
}


/*
 * Is element i of a colour object NA (or NULL)?
 */
Rboolean isNAcol(SEXP col, int index, int ncol)
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
	    error(_("invalid color specification"));
    }
    return result;
}


/*  P A R A M E T E R	 U T I L I T I E S  */

/*
 * Extract specified par from list of inline pars
 */
static SEXP getInlinePar(SEXP s, char *name)
{
    SEXP result = R_NilValue;
    int found = 0;
    if (isList(s) && !found) {
	while (s != R_NilValue) {
	    if (isList(CAR(s))) {
		result = getInlinePar(CAR(s), name);
		if (result)
		    found = 1;
	    } else
		if (TAG(s) != R_NilValue)
		    if (!strcmp(CHAR(PRINTNAME(TAG(s))), name)) {
			result = CAR(s);
			found = 1;
		    }
	    s = CDR(s);
	}
    }
    return result;
}

/* dflt used to be used for < 0 values in R < 2.7.0,
   now just used for NULL */
static SEXP FixupPch(SEXP pch, int dflt)
{
    int i, n;
    SEXP ans = R_NilValue;/* -Wall*/

    n = length(pch);
    if (n == 0) return ans = ScalarInteger(dflt);

    PROTECT(ans = allocVector(INTSXP, n));
    if (isList(pch)) {
	for (i = 0; pch != R_NilValue;	pch = CDR(pch))
	    INTEGER(ans)[i++] = asInteger(CAR(pch));
    }
    else if (isInteger(pch)) {
	for (i = 0; i < n; i++)
	    INTEGER(ans)[i] = INTEGER(pch)[i];
    }
    else if (isReal(pch)) {
	for (i = 0; i < n; i++)
	    INTEGER(ans)[i] = R_FINITE(REAL(pch)[i]) ?
		(int) REAL(pch)[i] : NA_INTEGER;
    }
    else if (isString(pch)) {
	for (i = 0; i < n; i++) {
	    /* New in 2.7.0: negative values indicate Unicode points. */
	    INTEGER(ans)[i] = GEstring_to_pch(STRING_ELT(pch, i));
	}
    }
    else if (isLogical(pch)) {/* NA, but not TRUE/FALSE */
	for (i = 0; i < n; i++)
	    if(LOGICAL(pch)[i] == NA_LOGICAL) INTEGER(ans)[i] = NA_INTEGER;
	    else error(_("only NA allowed in logical plotting symbol"));
    }
    else error(_("invalid plotting symbol"));
    UNPROTECT(1);
    return ans;
}

SEXP FixupLty(SEXP lty, int dflt)
{
    int i, n;
    SEXP ans;
    n = length(lty);
    if (n == 0) {
	ans = ScalarInteger(dflt);
    }
    else {
	ans = allocVector(INTSXP, n);
	for (i = 0; i < n; i++)
	    INTEGER(ans)[i] = GE_LTYpar(lty, i);
    }
    return ans;
}

SEXP FixupLwd(SEXP lwd, double dflt)
{
    int i, n;
    double w;
    SEXP ans = NULL;

    n = length(lwd);
    if (n == 0)
	ans = ScalarReal(dflt);
    else {
	PROTECT(lwd = coerceVector(lwd, REALSXP));
	n = length(lwd);
	ans = allocVector(REALSXP, n);
	for (i = 0; i < n; i++) {
	    w = REAL(lwd)[i];
	    if (w < 0) w = NA_REAL;
	    REAL(ans)[i] = w;

	}
	UNPROTECT(1);
    }
    return ans;
}

static SEXP FixupFont(SEXP font, int dflt)
{
    int i, k, n;
    SEXP ans = R_NilValue;/* -Wall*/
    n = length(font);
    if (n == 0) {
	ans = ScalarInteger(dflt);
    }
    else if (isLogical(font)) {
	ans = allocVector(INTSXP, n);
	for (i = 0; i < n; i++) {
	    k = LOGICAL(font)[i];
#ifndef Win32
	    if (k < 1 || k > 5) k = NA_INTEGER;
#else
	    if (k < 1 || k > 32) k = NA_INTEGER;
#endif
	    INTEGER(ans)[i] = k;
	}
    }
    else if (isInteger(font)) {
	ans = allocVector(INTSXP, n);
	for (i = 0; i < n; i++) {
	    k = INTEGER(font)[i];
#ifndef Win32
	    if (k < 1 || k > 5) k = NA_INTEGER;
#else
	    if (k < 1 || k > 32) k = NA_INTEGER;
#endif
	    INTEGER(ans)[i] = k;
	}
    }
    else if (isReal(font)) {
	ans = allocVector(INTSXP, n);
	for (i = 0; i < n; i++) {
	    k = (int) REAL(font)[i];
#ifndef Win32
	    if (k < 1 || k > 5) k = NA_INTEGER;
#else
	    if (k < 1 || k > 32) k = NA_INTEGER;
#endif
	    INTEGER(ans)[i] = k;
	}
    }
    else error(_("invalid font specification"));
    return ans;
}

SEXP FixupCol(SEXP col, unsigned int dflt)
{
    int i, n;
    SEXP ans;
    unsigned int bg = dpptr(GEcurrentDevice())->bg; /* col = 0 */

    n = length(col);
    if (n == 0) {
	PROTECT(ans = ScalarInteger(dflt));
    } else {
	ans = PROTECT(allocVector(INTSXP, n));
	if (isList(col))
	    for (i = 0; i < n; i++) {
		INTEGER(ans)[i] = RGBpar3(CAR(col), 0, bg);
		col = CDR(col);
	    }
	else
	    for (i = 0; i < n; i++)
		INTEGER(ans)[i] = RGBpar3(col, i, bg);
    }
    UNPROTECT(1);
    return ans;
}

static SEXP FixupCex(SEXP cex, double dflt)
{
    SEXP ans;
    int i, n;
    n = length(cex);
    if (n == 0) {
	ans = allocVector(REALSXP, 1);
	if (R_FINITE(dflt) && dflt > 0)
	    REAL(ans)[0] = dflt;
	else
	    REAL(ans)[0] = NA_REAL;
    }
    else {
	double c;
	ans = allocVector(REALSXP, n);
	if (isReal(cex))
	    for (i = 0; i < n; i++) {
		c = REAL(cex)[i];
		if (R_FINITE(c) && c > 0)
		    REAL(ans)[i] = c;
		else
		    REAL(ans)[i] = NA_REAL;
	    }
	else if (isInteger(cex) || isLogical(cex))
	    for (i = 0; i < n; i++) {
		c = INTEGER(cex)[i];
		if (c == NA_INTEGER || c <= 0)
		    c = NA_REAL;
		REAL(ans)[i] = c;
	    }
	else
	    error(_("invalid '%s' value"), "cex");
    }
    return ans;
}

SEXP FixupVFont(SEXP vfont) {
    SEXP ans = R_NilValue;
    if (!isNull(vfont)) {
	SEXP vf;
	int typeface, fontindex;
	int minindex, maxindex=0;/* -Wall*/
	int i;
	PROTECT(vf = coerceVector(vfont, INTSXP));
	if (length(vf) != 2)
	    error(_("invalid '%s' value"), "vfont");
	typeface = INTEGER(vf)[0];
	if (typeface < 1 || typeface > 8)
	    error(_("invalid 'vfont' value [typeface %d]"), typeface);
	/* For each of the typefaces {1..8}, there are several fontindices
	   available; how many depends on the typeface.
	   The possible combinations are "given" in ./g_fontdb.c
	   and also listed in help(Hershey).
	 */
	minindex = 1;
	switch (typeface) {
	case 1: /* serif */
	    maxindex = 7;	    break;
	case 2: /* sans serif */
	case 7: /* serif symbol */
	    maxindex = 4;	    break;
	case 3: /* script */
	    maxindex = 3;	    break;
	case 4: /* gothic english */
	case 5: /* gothic german */
	case 6: /* gothic italian */
	    maxindex = 1;	    break;
	case 8: /* sans serif symbol */
	    maxindex = 2;
	}
	fontindex = INTEGER(vf)[1];
	if (fontindex < minindex || fontindex > maxindex)
	    error(_("invalid 'vfont' value [typeface = %d, fontindex = %d]"),
		  typeface, fontindex);
	ans = allocVector(INTSXP, 2);
	for (i = 0; i < 2; i++) INTEGER(ans)[i] = INTEGER(vf)[i];
	UNPROTECT(1);
    }
    return ans;
}


/* GetTextArg() : extract and possibly set text arguments
 *  ("label", col=, cex=, font=)
 *
 * Main purpose: Treat things like  title(main = list("This Title", font= 4))
 *
 * Called from	Title()  [only, currently]
 */
static void
GetTextArg(SEXP spec, SEXP *ptxt, rcolor *pcol, double *pcex, int *pfont)
{
    int i, n, font, colspecd;
    rcolor col;
    double cex;
    SEXP txt, nms;
    PROTECT_INDEX pi;

    txt	  = R_NilValue;
    cex	  = NA_REAL;
    col	  = R_TRANWHITE;
    colspecd = 0;
    font  = NA_INTEGER;
    /* It doesn't look as if this protection is needed */
    PROTECT_WITH_INDEX(txt, &pi);

    switch (TYPEOF(spec)) {
    case LANGSXP:
    case SYMSXP:
	REPROTECT(txt = coerceVector(spec, EXPRSXP), pi);
	break;
    case VECSXP:
	if (length(spec) == 0) {
	    *ptxt = R_NilValue;
	}
	else {
	    nms = getAttrib(spec, R_NamesSymbol);
	    if (nms == R_NilValue){ /* PR#1939 */
	       txt = VECTOR_ELT(spec, 0);
	       if (TYPEOF(txt) == LANGSXP || TYPEOF(txt) == SYMSXP )
		    REPROTECT(txt = coerceVector(txt, EXPRSXP), pi);
	       else if (!isExpression(txt))
		    REPROTECT(txt = coerceVector(txt, STRSXP), pi);
	    } else {
	       n = length(nms);
	       for (i = 0; i < n; i++) {
		if (!strcmp(CHAR(STRING_ELT(nms, i)), "cex")) {
		    cex = asReal(VECTOR_ELT(spec, i));
		}
		else if (!strcmp(CHAR(STRING_ELT(nms, i)), "col")) {
		    SEXP colsxp = VECTOR_ELT(spec, i);
		    if (!isNAcol(colsxp, 0, LENGTH(colsxp))) {
			col = asInteger(FixupCol(colsxp, R_TRANWHITE));
			colspecd = 1;
		    }
		}
		else if (!strcmp(CHAR(STRING_ELT(nms, i)), "font")) {
		    font = asInteger(FixupFont(VECTOR_ELT(spec, i), NA_INTEGER));
		}
		else if (!strcmp(CHAR(STRING_ELT(nms, i)), "")) {
		    txt = VECTOR_ELT(spec, i);
		    if (TYPEOF(txt) == LANGSXP || TYPEOF(txt) == SYMSXP)
			REPROTECT(txt = coerceVector(txt, EXPRSXP), pi);
		    else if (!isExpression(txt))
			REPROTECT(txt = coerceVector(txt, STRSXP), pi);
		}
		else error(_("invalid graphics parameter"));
	       }
	    }
	}
	break;
    case STRSXP:
    case EXPRSXP:
	txt = spec;
	break;
    default:
	REPROTECT(txt = coerceVector(spec, STRSXP), pi);
	break;
    }
    UNPROTECT(1);
    if (txt != R_NilValue) {
	*ptxt = txt;
	if (R_FINITE(cex))	 *pcex	 = cex;
	if (colspecd)	         *pcol	 = col;
	if (font != NA_INTEGER)	 *pfont	 = font;
    }
}/* GetTextArg */


    /* GRAPHICS FUNCTION ENTRY POINTS */

SEXP C_plot_new(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    /* plot.new() - create a new plot "frame" */

    pGEDevDesc dd;

    dd = GEcurrentDevice();
    /*
     * If user is prompted before new page, user has opportunity
     * to kill current device.  GNewPlot returns (potentially new)
     * current device.
     */
    dd = GNewPlot(GRecording(call, dd));

    dpptr(dd)->xlog = gpptr(dd)->xlog = FALSE;
    dpptr(dd)->ylog = gpptr(dd)->ylog = FALSE;

    GScale(0.0, 1.0, 1, dd);
    GScale(0.0, 1.0, 2, dd);
    GMapWin2Fig(dd);
    GSetState(1, dd);

    if (GRecording(call, dd))
	GErecordGraphicOperation(op, args, dd);
    return R_NilValue;
}


/*
 *  SYNOPSIS
 *
 *	plot.window(xlim, ylim, log="", asp=NA)
 *
 *  DESCRIPTION
 *
 *	This function sets up the world coordinates for a graphics
 *	window.	 Note that if asp is a finite positive value then
 *	the window is set up so that one data unit in the y direction
 *	is equal in length to one data unit in the x direction divided
 *	by asp.
 *
 *	The special case asp == 1 produces plots where distances
 *	between points are represented accurately on screen.
 *
 *  NOTE
 *
 *	The use of asp can have weird effects when axis is an
 *	interpreted function.  It has to be internal so that the
 *	full computation is captured in the display list.
 */

SEXP C_plot_window(SEXP args)
{
    SEXP xlim, ylim, logarg;
    double asp, xmin, xmax, ymin, ymax;
    Rboolean logscale;
    const char *p;
    pGEDevDesc dd = GEcurrentDevice();

    args = CDR(args);
    if (length(args) < 3)
	error(_("at least 3 arguments required"));

    xlim = CAR(args);
    if (!isNumeric(xlim) || LENGTH(xlim) != 2)
	error(_("invalid '%s' value"), "xlim");
    args = CDR(args);

    ylim = CAR(args);
    if (!isNumeric(ylim) || LENGTH(ylim) != 2)
	error(_("invalid '%s' value"), "ylim");
    args = CDR(args);

    logscale = FALSE;
    logarg = CAR(args);
    if (!isString(logarg))
	error(_("\"log=\" specification must be character"));
    p = CHAR(STRING_ELT(logarg, 0));
    while (*p) {
	switch (*p) {
	case 'x':
	    dpptr(dd)->xlog = gpptr(dd)->xlog = logscale = TRUE;
	    break;
	case 'y':
	    dpptr(dd)->ylog = gpptr(dd)->ylog = logscale = TRUE;
	    break;
	default:
	    error(_("invalid \"log=%s\" specification"), p);
	}
	p++;
    }
    args = CDR(args);

    asp = (logscale) ? NA_REAL : asReal(CAR(args));;
    args = CDR(args);

    /* This reads [xy]axs and lab, used in GScale */
    GSavePars(dd);
    ProcessInlinePars(args, dd);

    if (isInteger(xlim)) {
	if (INTEGER(xlim)[0] == NA_INTEGER || INTEGER(xlim)[1] == NA_INTEGER)
	    error(_("NAs not allowed in 'xlim'"));
	xmin = INTEGER(xlim)[0];
	xmax = INTEGER(xlim)[1];
    }
    else {
	if (!R_FINITE(REAL(xlim)[0]) || !R_FINITE(REAL(xlim)[1]))
	    error(_("need finite 'xlim' values"));
	xmin = REAL(xlim)[0];
	xmax = REAL(xlim)[1];
    }
    if (isInteger(ylim)) {
	if (INTEGER(ylim)[0] == NA_INTEGER || INTEGER(ylim)[1] == NA_INTEGER)
	    error(_("NAs not allowed in 'ylim'"));
	ymin = INTEGER(ylim)[0];
	ymax = INTEGER(ylim)[1];
    }
    else {
	if (!R_FINITE(REAL(ylim)[0]) || !R_FINITE(REAL(ylim)[1]))
	    error(_("need finite 'ylim' values"));
	ymin = REAL(ylim)[0];
	ymax = REAL(ylim)[1];
    }
    if ((dpptr(dd)->xlog && (xmin < 0 || xmax < 0)) ||
       (dpptr(dd)->ylog && (ymin < 0 || ymax < 0)))
	    error(_("Logarithmic axis must have positive limits"));

    if (R_FINITE(asp) && asp > 0) {
	double pin1, pin2, scale, xdelta, ydelta, xscale, yscale, xadd, yadd;
	pin1 = GConvertXUnits(1.0, NPC, INCHES, dd);
	pin2 = GConvertYUnits(1.0, NPC, INCHES, dd);
	xdelta = fabs(xmax - xmin) / asp;
	ydelta = fabs(ymax - ymin);
	if(xdelta == 0.0 && ydelta == 0.0) {
	    /* We really do mean zero: small non-zero values work.
	       Mimic the behaviour of GScale for the x axis. */
	    xadd = yadd = ((xmin == 0.0) ? 1 : 0.4) * asp;
	    xadd *= asp;
	} else {
	    xscale = pin1 / xdelta;
	    yscale = pin2 / ydelta;
	    scale = (xscale < yscale) ? xscale : yscale;
	    xadd = .5 * (pin1 / scale - xdelta) * asp;
	    yadd = .5 * (pin2 / scale - ydelta);
	}
	if(xmax < xmin) xadd *= -1;
	if(ymax < ymin) yadd *= -1;
	GScale(xmin - xadd, xmax + xadd, 1, dd);
	GScale(ymin - yadd, ymax + yadd, 2, dd);
    }
    else { /* asp <= 0 or not finite -- includes logscale ! */
	GScale(xmin, xmax, 1, dd);
	GScale(ymin, ymax, 2, dd);
    }
    /* GScale set the [xy]axp parameters */
    GMapWin2Fig(dd);
    GRestorePars(dd);
    /* This has now clobbered the Rf_ggptr settings for coord system */
    return R_NilValue;
}

static void GetAxisLimits(double left, double right, Rboolean logflag, double *low, double *high)
{
/*	Called from Axis()	such as
 *	GetAxisLimits(gpptr(dd)->usr[0], gpptr(dd)->usr[1], &low, &high)
 *
 *	Computes  *low < left, right < *high  (even if left=right)
 */
    double eps;
    if (logflag) {
	left = log(left);
	right = log(right);
    }
    if (left > right) {/* swap */
	eps = left; left = right; right = eps;
    }
    eps = right - left;
    if (eps == 0.)
	eps = 0.5 * FLT_EPSILON;
    else
	eps *= FLT_EPSILON;
    *low = left - eps;
    *high = right + eps;
    
    if (logflag) {
	*low = exp(*low);
	*high = exp(*high);
    }
}


/* axis(side, at, labels, ...) */

SEXP labelformat(SEXP labels)
{
    /* format(labels): i.e. from numbers to strings */
    SEXP ans = R_NilValue;/* -Wall*/
    int i, n, w, d, e, wi, di, ei;
    const char *strp;
    n = length(labels);
    R_print.digits = 7;/* maximally 7 digits -- ``burnt in'';
			  S-PLUS <= 5.x has about 6
			  (but really uses single precision..) */
    switch(TYPEOF(labels)) {
    case LGLSXP:
	PROTECT(ans = allocVector(STRSXP, n));
	for (i = 0; i < n; i++) {
	    strp = EncodeLogical(LOGICAL(labels)[i], 0);
	    SET_STRING_ELT(ans, i, mkChar(strp));
	}
	UNPROTECT(1);
	break;
    case INTSXP:
	PROTECT(ans = allocVector(STRSXP, n));
	for (i = 0; i < n; i++) {
	    strp = EncodeInteger(INTEGER(labels)[i], 0);
	    SET_STRING_ELT(ans, i, mkChar(strp));
	}
	UNPROTECT(1);
	break;
    case REALSXP:
	formatReal(REAL(labels), n, &w, &d, &e, 0);
	PROTECT(ans = allocVector(STRSXP, n));
	for (i = 0; i < n; i++) {
	    strp = EncodeReal0(REAL(labels)[i], 0, d, e, OutDec);
	    SET_STRING_ELT(ans, i, mkChar(strp));
	}
	UNPROTECT(1);
	break;
    case CPLXSXP:
	formatComplex(COMPLEX(labels), n, &w, &d, &e, &wi, &di, &ei, 0);
	PROTECT(ans = allocVector(STRSXP, n));
	for (i = 0; i < n; i++) {
	    strp = EncodeComplex(COMPLEX(labels)[i], 0, d, e, 0, di, ei,
				 OutDec);
	    SET_STRING_ELT(ans, i, mkChar(strp));
	}
	UNPROTECT(1);
	break;
    case STRSXP:
	PROTECT(ans = allocVector(STRSXP, n));
	for (i = 0; i < n; i++) {
	    SET_STRING_ELT(ans, i, STRING_ELT(labels, i));
	}
	UNPROTECT(1);
	break;
    default:
	error(_("invalid type for axis labels"));
    }
    return ans;
}


static double ComputePAdjValue(double padj, int side, int las)
{
    if (!R_FINITE(padj)) {
    switch(las) {
    case 0:/* parallel to axis */
	padj = 0.0; break;
    case 1:/* horizontal */
	switch(side) {
	case 1:
	case 3: padj = 0.0; break;
	case 2:
	case 4: padj = 0.5; break;
	}
	break;
    case 2:/* perpendicular to axis */
	padj = 0.5; break;
    case 3:/* vertical */
	switch(side) {
	case 1:
	case 3: padj = 0.5; break;
	case 2:
	case 4: padj = 0.0; break;
	}
	break;
    }
    }
    return padj;
}

static void getxlimits(double *x, pGEDevDesc dd) {
    /*
     * xpd = 0 means clip to current plot region
     * xpd = 1 means clip to current figure region
     * xpd = 2 means clip to device region
     */
    switch (gpptr(dd)->xpd) {
    case 0:
	x[0] = gpptr(dd)->usr[0];
	x[1] = gpptr(dd)->usr[1];
	break;
    case 1:
	x[0] = GConvertX(0, NFC, USER, dd);
	x[1] = GConvertX(1, NFC, USER, dd);
	break;
    case 2:
	x[0] = GConvertX(0, NDC, USER, dd);
	x[1] = GConvertX(1, NDC, USER, dd);
	break;
    }
}

static void getylimits(double *y, pGEDevDesc dd) {
    switch (gpptr(dd)->xpd) {
    case 0:
	y[0] = gpptr(dd)->usr[2];
	y[1] = gpptr(dd)->usr[3];
	break;
    case 1:
	y[0] = GConvertY(0, NFC, USER, dd);
	y[1] = GConvertY(1, NFC, USER, dd);
	break;
    case 2:
	y[0] = GConvertY(0, NDC, USER, dd);
	y[1] = GConvertY(1, NDC, USER, dd);
	break;
    }
}

SEXP C_axis(SEXP args)
{
    /* axis(side, at, labels, tick, line, pos,
     	    outer, font, lty, lwd, lwd.ticks, col, col.ticks,
	    hadj, padj, ...)
    */

    SEXP at, lab, padj, label;
    int font, lty, npadj;
    rcolor col, colticks;
    int i, n, nint = 0, ntmp, side, *ind, outer, lineoff = 0;
    int istart, iend, incr;
    Rboolean dolabels, doticks, logflag = FALSE;
    Rboolean create_at;
    double x, y, temp, tnew, tlast;
    double axp[3], usr[2], limits[2];
    double gap, labw, low, high, line, pos, lwd, lwdticks, hadj;
    double axis_base, axis_tick, axis_lab, axis_low, axis_high;

    pGEDevDesc dd = GEcurrentDevice();

    /* Arity Check */
    /* This is a builtin function, so it should always have */
    /* the correct arity, but it doesn't hurt to be defensive. */

    args = CDR(args);
    if (length(args) < 15)
	error(_("too few arguments"));
    GCheckState(dd);

    PrintDefaults(); /* prepare for labelformat */

    /* Required argument: "side" */
    /* Which side of the plot the axis is to appear on. */
    /* side = 1 | 2 | 3 | 4. */

    side = asInteger(CAR(args));
    if (side < 1 || side > 4)
	error(_("invalid axis number %d"), side);
    args = CDR(args);

    /* Required argument: "at" */
    /* This gives the tick-label locations. */
    /* Note that these are coerced to the correct type below. */

    at = CAR(args); args = CDR(args);

    /* Required argument: "labels" */
    /* Labels can be a logical, indicating whether or not */
    /* to label the axis; or it can be a vector of character */
    /* strings or expressions which give the labels explicitly. */
    /* The expressions are used to set mathematical labelling. */

    dolabels = TRUE;
    lab = CAR(args);
    if (isLogical(lab) && length(lab) > 0) {
	i = asLogical(lab);
	if (i == 0 || i == NA_LOGICAL)
	    dolabels = FALSE;
	PROTECT(lab = R_NilValue);
    } else if (TYPEOF(lab) == LANGSXP || TYPEOF(lab) == SYMSXP) {
	PROTECT(lab = coerceVector(lab, EXPRSXP));
    } else if (isExpression(lab)) {
	PROTECT(lab);
    } else {
	PROTECT(lab = coerceVector(lab, STRSXP));
    }
    args = CDR(args);

    /* Required argument: "tick" */
    /* This indicates whether or not ticks and the axis line */
    /* should be plotted: TRUE => show, FALSE => don't show. */

    doticks = asLogical(CAR(args));
    doticks = (doticks == NA_LOGICAL) ? TRUE : (Rboolean) doticks;
    args = CDR(args);

    /* Optional argument: "line" */

    /* Specifies an offset outward from the plot for the axis.
     * The values in the par value "mgp" are interpreted
     * relative to this value. */
    line = asReal(CAR(args));
    /* defer processing until after in-line pars */
    args = CDR(args);

    /* Optional argument: "pos" */
    /* Specifies a user coordinate at which the axis should be drawn. */
    /* This overrides the value of "line".  Again the "mgp" par values */
    /* are interpreted relative to this value. */

    pos = asReal(CAR(args));
    /* defer processing until after in-line pars */
    args = CDR(args);

    /* Optional argument: "outer" */
    /* Should the axis be drawn in the outer margin. */
    /* This only affects the computation of axis_base. */

    outer = asLogical(CAR(args));
    if (outer == NA_LOGICAL || outer == 0)
	outer = NPC;
    else
	outer = NIC;
    args = CDR(args);

    /* Optional argument: "font" */
    font = asInteger(FixupFont(CAR(args), NA_INTEGER));
    args = CDR(args);

    /* Optional argument: "lty" */
    lty = asInteger(FixupLty(CAR(args), 0));
    args = CDR(args);

    /* Optional argument: "lwd" */
    lwd = asReal(FixupLwd(CAR(args), 1));
    args = CDR(args);
    lwdticks = asReal(FixupLwd(CAR(args), 1));
    args = CDR(args);

    /* Optional argument: "col" */
    col = asInteger(FixupCol(CAR(args), gpptr(dd)->fg));
    args = CDR(args);
    colticks = asInteger(FixupCol(CAR(args), col));
    args = CDR(args);

    /* Optional argument: "hadj" */
    if (length(CAR(args)) != 1)
	error(_("'hadj' must be of length one"));
    hadj = asReal(CAR(args));
    args = CDR(args);

    /* Optional argument: "padj" */
    PROTECT(padj = coerceVector(CAR(args), REALSXP));
    npadj = length(padj);
    if (npadj <= 0) error(_("zero-length '%s' specified"), "padj");

    /* Now we process all the remaining inline par values:
       we need to do it now as x/yaxp are retrieved next.
       That will set gpptr, so we update that first - do_plotwindow
       clobbered the gpptr settings. */
    GSavePars(dd);
    gpptr(dd)->xaxp[0] = dpptr(dd)->xaxp[0];
    gpptr(dd)->xaxp[1] = dpptr(dd)->xaxp[1];
    gpptr(dd)->xaxp[2] = dpptr(dd)->xaxp[2];
    gpptr(dd)->yaxp[0] = dpptr(dd)->yaxp[0];
    gpptr(dd)->yaxp[1] = dpptr(dd)->yaxp[1];
    gpptr(dd)->yaxp[2] = dpptr(dd)->yaxp[2];
    ProcessInlinePars(args, dd);

    /* Retrieve relevant "par" values. */

    switch(side) {
    case 1:
    case 3:
	axp[0] = gpptr(dd)->xaxp[0];
	axp[1] = gpptr(dd)->xaxp[1];
	axp[2] = gpptr(dd)->xaxp[2];
	usr[0] = dpptr(dd)->usr[0];
	usr[1] = dpptr(dd)->usr[1];
	logflag = dpptr(dd)->xlog;
	nint = dpptr(dd)->lab[0];
	break;
    case 2:
    case 4:
	axp[0] = gpptr(dd)->yaxp[0];
	axp[1] = gpptr(dd)->yaxp[1];
	axp[2] = gpptr(dd)->yaxp[2];
	usr[0] = dpptr(dd)->usr[2];
	usr[1] = dpptr(dd)->usr[3];
	logflag = dpptr(dd)->ylog;
	nint = dpptr(dd)->lab[1];
	break;
    }

    /* Deferred processing */
    if (!R_FINITE(line)) {
	/* Except that here mgp values are not relative to themselves */
	line = gpptr(dd)->mgp[2];
	lineoff = (int) line;
    }
    if (!R_FINITE(pos)) pos = NA_REAL; else lineoff = 0;

    /* Determine the tickmark positions.  Note that these may fall */
    /* outside the plot window. We will clip them in the code below. */

    create_at = isNull(at);
    if (create_at) {
	PROTECT(at = CreateAtVector(axp, usr, nint, logflag));
    }
    else {
	if (isReal(at)) PROTECT(at = duplicate(at));
	else PROTECT(at = coerceVector(at, REALSXP));
    }
    n = length(at);

    /* Check/setup the tick labels.  This can mean using user-specified */
    /* labels, or encoding the "at" positions as strings. */

    if (dolabels) {
	if (length(lab) == 0)
	    lab = labelformat(at);
	else {
	    if (create_at)
		error(_("'labels' is supplied and not 'at'"));
	    if (!isExpression(lab)) lab = labelformat(lab);
	}
	if (length(at) != length(lab))
	    error(_("'at' and 'labels' lengths differ, %d != %d"),
		      length(at), length(lab));
    }
    PROTECT(lab);

    /* Check there are no NA, Inf or -Inf values for tick positions. */
    /* The code here is long-winded.  Couldn't we just inline things */
    /* below.  Hmmm - we need the min and max of the finite values ... */

    ind = (int *) R_alloc(n, sizeof(int));
    for(i = 0; i < n; i++) ind[i] = i;
    rsort_with_index(REAL(at), ind, n);
    ntmp = 0;
    for(i = 0; i < n; i++) {
	if(R_FINITE(REAL(at)[i])) ntmp = i+1;
    }
    if (n > 0 && ntmp == 0)
	error(_("no locations are finite"));
    n = ntmp;

    /* Ok, all systems are "GO".  Let's get to it. */

    /* At this point we know the value of "xaxt" and "yaxt",
     * so we test to see whether the relevant one is "n".
     * If it is, we just bail out at this point. */

    if ((n == 0) ||
        ((side == 1 || side == 3) && gpptr(dd)->xaxt == 'n') ||
	((side == 2 || side == 4) && gpptr(dd)->yaxt == 'n')) {
	GRestorePars(dd);
	UNPROTECT(4);
	return R_NilValue;
    }


    gpptr(dd)->lty = lty;
    gpptr(dd)->lwd = lwd;
    gpptr(dd)->adj = R_FINITE(hadj) ? hadj : 0.5;
    gpptr(dd)->font = (font == NA_INTEGER)? gpptr(dd)->fontaxis : font;
    gpptr(dd)->cex = gpptr(dd)->cexbase * gpptr(dd)->cexaxis;

    /* Draw the axis */
    GMode(1, dd);
    switch (side) {
    case 1: /*--- x-axis -- horizontal --- */
    case 3:
        /* First set the clipping limits */
        getxlimits(limits, dd);
        /* Now override par("xpd") and force clipping to device region. */
        gpptr(dd)->xpd = 2;
	GetAxisLimits(limits[0], limits[1], logflag, &low, &high);
	axis_low  = GConvertX(fmin2(high, fmax2(low, REAL(at)[0])), USER, NFC, dd);
	axis_high = GConvertX(fmin2(high, fmax2(low, REAL(at)[n-1])), USER, NFC, dd);
	if (side == 1) {
	    if (R_FINITE(pos))
		axis_base = GConvertY(pos, USER, NFC, dd);
	    else
		axis_base = GConvertY(0.0, outer, NFC, dd)
		    - GConvertYUnits(line, LINES, NFC, dd);
	    if (R_FINITE(gpptr(dd)->tck)) {
		double len, xu, yu;
		if(gpptr(dd)->tck > 0.5)
		    len = GConvertYUnits(gpptr(dd)->tck, NPC, NFC, dd);
		else {
		    xu = GConvertXUnits(gpptr(dd)->tck, NPC, INCHES, dd);
		    yu = GConvertYUnits(gpptr(dd)->tck, NPC, INCHES, dd);
		    xu = (fabs(xu) < fabs(yu)) ? xu : yu;
		    len = GConvertYUnits(xu, INCHES, NFC, dd);
		}
		axis_tick = axis_base + len;

	    } else
		axis_tick = axis_base +
			GConvertYUnits(gpptr(dd)->tcl, LINES, NFC, dd);
	}
	else {
	    if (R_FINITE(pos))
		axis_base = GConvertY(pos, USER, NFC, dd);
	    else
		axis_base =  GConvertY(1.0, outer, NFC, dd)
		    + GConvertYUnits(line, LINES, NFC, dd);
	    if (R_FINITE(gpptr(dd)->tck)) {
		double len, xu, yu;
		if(gpptr(dd)->tck > 0.5)
		    len = GConvertYUnits(gpptr(dd)->tck, NPC, NFC, dd);
		else {
		    xu = GConvertXUnits(gpptr(dd)->tck, NPC, INCHES, dd);
		    yu = GConvertYUnits(gpptr(dd)->tck, NPC, INCHES, dd);
		    xu = (fabs(xu) < fabs(yu)) ? xu : yu;
		    len = GConvertYUnits(xu, INCHES, NFC, dd);
		}
		axis_tick = axis_base - len;
	    } else
		axis_tick = axis_base -
		    GConvertYUnits(gpptr(dd)->tcl, LINES, NFC, dd);
	}
	if (doticks) {
	    gpptr(dd)->col = col;
	    if (lwd > 0.0)
		GLine(axis_low, axis_base, axis_high, axis_base, NFC, dd);
	    gpptr(dd)->col = colticks;
	    gpptr(dd)->lwd = lwdticks;
	    if (lwdticks > 0) {
		for (i = 0; i < n; i++) {
		    x = REAL(at)[i];
		    if (low <= x && x <= high) {
			x = GConvertX(x, USER, NFC, dd);
			GLine(x, axis_base, x, axis_tick, NFC, dd);
		    }
		}
	    }
	}
	/* Tickmark labels. */
	gpptr(dd)->col = gpptr(dd)->colaxis;
	gap = GStrWidth("m", -1, NFC, dd);	/* FIXUP x/y distance */
	tlast = -1.0;
	if (!R_FINITE(hadj)) {
	    if (gpptr(dd)->las == 2 || gpptr(dd)->las == 3) {
		gpptr(dd)->adj = (side == 1) ? 1 : 0;
	    }
	    else gpptr(dd)->adj = 0.5;
	}
	if (side == 1) {
	    axis_lab = - axis_base
		+ GConvertYUnits(gpptr(dd)->mgp[1]-lineoff, LINES, NFC, dd)
		+ GConvertY(0.0, NPC, NFC, dd);
	}
	else { /* side == 3 */
	    axis_lab = axis_base
		+ GConvertYUnits(gpptr(dd)->mgp[1]-lineoff, LINES, NFC, dd)
		- GConvertY(1.0, NPC, NFC, dd);
	}
	axis_lab = GConvertYUnits(axis_lab, NFC, LINES, dd);

	/* The order of processing is important here. */
	/* We must ensure that the labels are drawn left-to-right. */
	/* The logic here is getting way too convoluted. */
	/* This needs a serious rewrite. */

	if (gpptr(dd)->usr[0] > gpptr(dd)->usr[1]) {
	    istart = n - 1;
	    iend = -1;
	    incr = -1;
	}
	else {
	    istart = 0;
	    iend = n;
	    incr = 1;
	}
	for (i = istart; i != iend; i += incr) {
	    double padjval = REAL(padj)[i % npadj];
	    padjval = ComputePAdjValue(padjval, side, gpptr(dd)->las);
	    x = REAL(at)[i];
	    if (!R_FINITE(x)) continue;
	    temp = GConvertX(x, USER, NFC, dd);
	    if (dolabels) {
		/* Clip tick labels to user coordinates. */
		if (x > low && x < high) {
		    if (isExpression(lab)) {
			GMMathText(VECTOR_ELT(lab, ind[i]), side,
				   axis_lab, 0, x, gpptr(dd)->las,
				   padjval, dd);
		    }
		    else {
			label = STRING_ELT(lab, ind[i]);
			if(label != NA_STRING) {
			    const char *ss = CHAR(label);
			    labw = GStrWidth(ss, 0, NFC, dd);
			    tnew = temp - 0.5 * labw;
			    /* Check room for perpendicular labels. */
			    if (gpptr(dd)->las == 2 ||
				gpptr(dd)->las == 3 ||
				tnew - tlast >= gap) {
				GMtext(ss, getCharCE(label),
				       side, axis_lab, 0, x,
				       gpptr(dd)->las, padjval, dd);
				tlast = temp + 0.5 *labw;
			    }
			}
		    }
		}
	    }
	}
	break;

    case 2: /*--- y-axis -- vertical --- */
    case 4:
        /* First set the clipping limits */
        getylimits(limits, dd);
        /* Now override par("xpd") and force clipping to device region. */
        gpptr(dd)->xpd = 2;
	GetAxisLimits(limits[0], limits[1], logflag, &low, &high);
	axis_low = GConvertY(fmin2(high, fmax2(low, REAL(at)[0])), USER, NFC, dd);
	axis_high = GConvertY(fmin2(high, fmax2(low, REAL(at)[n-1])), USER, NFC, dd);
	if (side == 2) {
	    if (R_FINITE(pos))
		axis_base = GConvertX(pos, USER, NFC, dd);
	    else
		axis_base =  GConvertX(0.0, outer, NFC, dd)
		    - GConvertXUnits(line, LINES, NFC, dd);
	    if (R_FINITE(gpptr(dd)->tck)) {
		double len, xu, yu;
		if(gpptr(dd)->tck > 0.5)
		    len = GConvertXUnits(gpptr(dd)->tck, NPC, NFC, dd);
		else {
		    xu = GConvertXUnits(gpptr(dd)->tck, NPC, INCHES, dd);
		    yu = GConvertYUnits(gpptr(dd)->tck, NPC, INCHES, dd);
		    xu = (fabs(xu) < fabs(yu)) ? xu : yu;
		    len = GConvertXUnits(xu, INCHES, NFC, dd);
		}
		axis_tick = axis_base + len;
	    } else
		axis_tick = axis_base +
		    GConvertXUnits(gpptr(dd)->tcl, LINES, NFC, dd);
	}
	else {
	    if (R_FINITE(pos))
		axis_base = GConvertX(pos, USER, NFC, dd);
	    else
		axis_base =  GConvertX(1.0, outer, NFC, dd)
		    + GConvertXUnits(line, LINES, NFC, dd);
	    if (R_FINITE(gpptr(dd)->tck)) {
		double len, xu, yu;
		if(gpptr(dd)->tck > 0.5)
		    len = GConvertXUnits(gpptr(dd)->tck, NPC, NFC, dd);
		else {
		    xu = GConvertXUnits(gpptr(dd)->tck, NPC, INCHES, dd);
		    yu = GConvertYUnits(gpptr(dd)->tck, NPC, INCHES, dd);
		    xu = (fabs(xu) < fabs(yu)) ? xu : yu;
		    len = GConvertXUnits(xu, INCHES, NFC, dd);
		}
		axis_tick = axis_base - len;
	    } else
		axis_tick = axis_base -
		    GConvertXUnits(gpptr(dd)->tcl, LINES, NFC, dd);
	}
	if (doticks) {
	    gpptr(dd)->col = col;
	    if (lwd > 0.0)
		GLine(axis_base, axis_low, axis_base, axis_high, NFC, dd);
	    gpptr(dd)->col = colticks;
	    gpptr(dd)->lwd = lwdticks;
	    if (lwdticks > 0) {
		for (i = 0; i < n; i++) {
		    y = REAL(at)[i];
		    if (low <= y && y <= high) {
			y = GConvertY(y, USER, NFC, dd);
			GLine(axis_base, y, axis_tick, y, NFC, dd);
		    }
		}
	    }
	}
	/* Tickmark labels. */
	gpptr(dd)->col = gpptr(dd)->colaxis;
	gap = GStrWidth("m", CE_ANY, INCHES, dd);
	gap = GConvertYUnits(gap, INCHES, NFC, dd);
	tlast = -1.0;
	if (!R_FINITE(hadj)) {
	    if (gpptr(dd)->las == 1 || gpptr(dd)->las == 2) {
		gpptr(dd)->adj = (side == 2) ? 1 : 0;
	    }
	    else gpptr(dd)->adj = 0.5;
	}
	if (side == 2) {
	    axis_lab = - axis_base
		+ GConvertXUnits(gpptr(dd)->mgp[1]-lineoff, LINES, NFC, dd)
		+ GConvertX(0.0, NPC, NFC, dd);
	}
	else { /* side == 4 */
	    axis_lab = axis_base
		+ GConvertXUnits(gpptr(dd)->mgp[1]-lineoff, LINES, NFC, dd)
		- GConvertX(1.0, NPC, NFC, dd);
	}
	axis_lab = GConvertXUnits(axis_lab, NFC, LINES, dd);

	/* The order of processing is important here. */
	/* We must ensure that the labels are drawn left-to-right. */
	/* The logic here is getting way too convoluted. */
	/* This needs a serious rewrite. */

	if (gpptr(dd)->usr[2] > gpptr(dd)->usr[3]) {
	    istart = n - 1;
	    iend = -1;
	    incr = -1;
	}
	else {
	    istart = 0;
	    iend = n;
	    incr = 1;
	}
	for (i = istart; i != iend; i += incr) {
	    double padjval = REAL(padj)[i % npadj];
	    padjval = ComputePAdjValue(padjval, side, gpptr(dd)->las);
	    y = REAL(at)[i];
	    if (!R_FINITE(y)) continue;
	    temp = GConvertY(y, USER, NFC, dd);
	    if (dolabels) {
		/* Clip tick labels to user coordinates. */
		if (y > low && y < high) {
		    if (isExpression(lab)) {
			GMMathText(VECTOR_ELT(lab, ind[i]), side,
				   axis_lab, 0, y, gpptr(dd)->las,
				   padjval, dd);
		    }
		    else {
			label = STRING_ELT(lab, ind[i]);
			if(label != NA_STRING) {
			    const char *ss = CHAR(label);
			    labw = GStrWidth(ss, getCharCE(label), INCHES, dd);
			    labw = GConvertYUnits(labw, INCHES, NFC, dd);
			    tnew = temp - 0.5 * labw;
			    /* Check room for perpendicular labels. */
			    if (gpptr(dd)->las == 1 ||
				gpptr(dd)->las == 2 ||
				tnew - tlast >= gap) {
				GMtext(ss, getCharCE(label),
				       side, axis_lab, 0, y,
				       gpptr(dd)->las, padjval, dd);
				tlast = temp + 0.5 *labw;
			    }
			}
		    }
		}
	    }
	}
	break;
    } /* end  switch(side, ..) */
    GMode(0, dd);
    GRestorePars(dd);
    UNPROTECT(4); /* lab, at, lab, padj again */
    return at;
} /* Axis */


SEXP C_plotXY(SEXP args)
{
/*	plot.xy(xy, type, pch, lty, col, bg, cex, lwd, ...)

 *	plot points or lines of various types
 */
    SEXP sxy, sx, sy, pch, cex, col, bg, lty, lwd;
    double *x, *y, xold, yold, xx, yy, thiscex, thislwd;
    int i, n, npch, ncex, ncol, nbg, nlwd, type=0, start=0, thispch;
    rcolor thiscol, thisbg;
    const void *vmax = NULL /* -Wall */;

    pGEDevDesc dd = GEcurrentDevice();

    /* Basic Checks */
    GCheckState(dd);
    args = CDR(args);
    if (length(args) < 7)
	error(_("too few arguments"));

    /* Required Arguments */
#define PLOT_XY_DEALING(subname)				\
    sx = R_NilValue;		/* -Wall */			\
    sy = R_NilValue;		/* -Wall */			\
    sxy = CAR(args);						\
    if (isNewList(sxy) && length(sxy) >= 2) {			\
	TypeCheck(sx = VECTOR_ELT(sxy, 0), REALSXP); \
	TypeCheck(sy = VECTOR_ELT(sxy, 1), REALSXP); \
    }								\
    else if (isList(sxy) && length(sxy) >= 2) {			\
	TypeCheck(sx = CAR(sxy), REALSXP);	\
	TypeCheck(sy = CADR(sxy), REALSXP);	\
    }								\
    else							\
	error(_("invalid plotting structure"));	\
    if (LENGTH(sx) != LENGTH(sy))				\
	error(_("'x' and 'y' lengths differ in %s()"), subname);\
    n = LENGTH(sx);						\
    args = CDR(args)

    PLOT_XY_DEALING("plot.xy");

    if (isNull(CAR(args))) type = 'p';
    else {
	if (isString(CAR(args)) && LENGTH(CAR(args)) == 1 &&
	    LENGTH(pch = STRING_ELT(CAR(args), 0)) >= 1) {
	    if(LENGTH(pch) > 1)
		warning(_("plot type '%s' will be truncated to first character"),
			CHAR(pch));
	    type = CHAR(pch)[0];
	}
	else error(_("invalid plot type"));
    }
    args = CDR(args);

    PROTECT(pch = FixupPch(CAR(args), gpptr(dd)->pch));
    npch = length(pch);
    args = CDR(args);

    PROTECT(lty = FixupLty(CAR(args), gpptr(dd)->lty));
    args = CDR(args);

    /* Default col was NA_INTEGER (0x80000000) which was interpreted
       as zero (black) or "don't draw" depending on line/rect/circle
       situation. Now we set the default to zero and don't plot at all
       if col==NA.

       FIXME: bg needs similar change, but that requires changes to
       the specific drivers. */

    PROTECT(col = FixupCol(CAR(args), 0));		args = CDR(args);
    ncol = LENGTH(col);

    PROTECT(bg = FixupCol(CAR(args), R_TRANWHITE));	args = CDR(args);
    nbg = LENGTH(bg);

    PROTECT(cex = FixupCex(CAR(args), 1.0));		args = CDR(args);
    ncex = LENGTH(cex);

    PROTECT(lwd = FixupLwd(CAR(args), gpptr(dd)->lwd)); args = CDR(args);
    nlwd = LENGTH(lwd);

    /* Miscellaneous Graphical Parameters */
    GSavePars(dd);
    ProcessInlinePars(args, dd);

    x = REAL(sx);
    y = REAL(sy);

    if (INTEGER(lty)[0] != NA_INTEGER)
	gpptr(dd)->lty = INTEGER(lty)[0];
    if (R_FINITE( (thislwd = REAL(lwd)[0]) ))
	gpptr(dd)->lwd = thislwd; /* but do recycle for "p" etc */

    GMode(1, dd);

    /* Line drawing :*/
    switch(type) {
    case 'l':
    case 'o':
	/* lines and overplotted lines and points */
	gpptr(dd)->col = INTEGER(col)[0];
	xold = NA_REAL;
	yold = NA_REAL;
	for (i = 0; i < n; i++) {
	    xx = x[i];
	    yy = y[i];
	    /* do the conversion now to check for non-finite */
	    GConvert(&xx, &yy, USER, DEVICE, dd);
	    if ((R_FINITE(xx) && R_FINITE(yy)) &&
		!(R_FINITE(xold) && R_FINITE(yold)))
		start = i;
	    else if ((R_FINITE(xold) && R_FINITE(yold)) &&
		     !(R_FINITE(xx) && R_FINITE(yy))) {
		if (i-start > 1)
		    GPolyline(i-start, x+start, y+start, USER, dd);
	    }
	    else if ((R_FINITE(xold) && R_FINITE(yold)) && (i == n-1))
		GPolyline(n-start, x+start, y+start, USER, dd);
	    xold = xx;
	    yold = yy;
	}
	break;

    case 'b':
    case 'c': /* broken lines (with points in between if 'b') */
    {
	double d, f;
	d = GConvertYUnits(0.5, CHARS, INCHES, dd);
	gpptr(dd)->col = INTEGER(col)[0];
	xold = NA_REAL;
	yold = NA_REAL;
	for (i = 0; i < n; i++) {
	    xx = x[i];
	    yy = y[i];
	    GConvert(&xx, &yy, USER, INCHES, dd);
	    if (R_FINITE(xold) && R_FINITE(yold) &&
		R_FINITE(xx) && R_FINITE(yy)) {
		// might divide by zero
		if (d < 0.5 * hypot(xx-xold, yy-yold)) {
		    f = d/hypot(xx-xold, yy-yold);
		    GLine(xold + f * (xx - xold),
			  yold + f * (yy - yold),
			  xx + f * (xold - xx),
			  yy + f * (yold - yy),
			  INCHES, dd);
		}
	    }
	    xold = xx;
	    yold = yy;
	}
    }
    break;

    case 's': /* step function	I */
    {
	double *xtemp, *ytemp;
	int n0 = 0;
	if(n <= 1000) {
	    R_CheckStack2(4*n*sizeof(double));
	    xtemp = (double *) alloca(2*n*sizeof(double));
	    ytemp = (double *) alloca(2*n*sizeof(double));
	} else {
	    vmax = vmaxget();
	    xtemp = (double *) R_alloc(2*n, sizeof(double));
	    ytemp = (double *) R_alloc(2*n, sizeof(double));
	}
	gpptr(dd)->col = INTEGER(col)[0];
	xold = NA_REAL;
	yold = NA_REAL;
	for (i = 0; i < n; i++) {
	    xx = x[i];
	    yy = y[i];
	    GConvert(&xx, &yy, USER, DEVICE, dd);
	    if ((R_FINITE(xx) && R_FINITE(yy)) &&
		(R_FINITE(xold) && R_FINITE(yold))) {
		if(n0 == 0) { xtemp[n0] = xold; ytemp[n0++] = yold; }
		xtemp[n0] = xx; ytemp[n0++] = yold;/* <-only diff 's' <-> 'S' */
		xtemp[n0] = xx;	ytemp[n0++] = yy;
	    } else if( (R_FINITE(xold) && R_FINITE(yold)) &&
		       !(R_FINITE(xx)  && R_FINITE(yy))	  && n0 > 0) {
		GPolyline(n0, xtemp, ytemp, DEVICE, dd);
		n0 = 0;
	    }
	    xold = xx;
	    yold = yy;
	}
	if(n0 > 0) GPolyline(n0, xtemp, ytemp, DEVICE, dd);
	if(n > 1000) vmaxset(vmax);
    }
    break;

    case 'S': /* step function	II */
    {
	double *xtemp, *ytemp;
	int n0 = 0;
	if(n < 1000) {
	    R_CheckStack2(4*n*sizeof(double));
	    xtemp = (double *) alloca(2*n*sizeof(double));
	    ytemp = (double *) alloca(2*n*sizeof(double));
	} else {
	    vmax = vmaxget();
	    xtemp = (double *) R_alloc(2*n, sizeof(double));
	    ytemp = (double *) R_alloc(2*n, sizeof(double));
	}
	gpptr(dd)->col = INTEGER(col)[0];
	xold = NA_REAL;
	yold = NA_REAL;
	for (i = 0; i < n; i++) {
	    xx = x[i];
	    yy = y[i];
	    GConvert(&xx, &yy, USER, DEVICE, dd);
	    if ((R_FINITE(xx) && R_FINITE(yy)) &&
		(R_FINITE(xold) && R_FINITE(yold))) {
		if(n0 == 0) {xtemp[n0] = xold; ytemp[n0++] = yold;}
		xtemp[n0] = xold; ytemp[n0++] = yy;
		xtemp[n0] = xx;	ytemp[n0++] = yy;
	    } else if( (R_FINITE(xold) && R_FINITE(yold)) &&
		       !(R_FINITE(xx)  && R_FINITE(yy))	  && n0 > 0) {
		GPolyline(n0, xtemp, ytemp, DEVICE, dd);
		n0 = 0;
	    }
	    xold = xx;
	    yold = yy;
	}
	if(n0 > 0) GPolyline(n0, xtemp, ytemp, DEVICE, dd);
	if(n > 1000) vmaxset(vmax);
    }
    break;

    case 'h': /* h[istogram] (bar plot) */
	if (gpptr(dd)->ylog)
	    yold = gpptr(dd)->usr[2];/* DBL_MIN fails.. why ???? */
	else
	    yold = 0.0;
	yold = GConvertY(yold, USER, DEVICE, dd);
	for (i = 0; i < n; i++) {
	    xx = x[i];
	    yy = y[i];
	    GConvert(&xx, &yy, USER, DEVICE, dd);
	    if (R_FINITE(xx) && R_FINITE(yy)
		&& !R_TRANSPARENT(thiscol = INTEGER(col)[i % ncol])) {
		gpptr(dd)->col = thiscol;
		GLine(xx, yold, xx, yy, DEVICE, dd);
	    }
	}
	break;

    case 'p':
    case 'n': /* nothing here */
	break;

    default:/* OTHERWISE */
	error(_("invalid plot type '%c'"), type);

    } /* End {switch(type)} - for lines */

    /* Points : */
    if (type == 'p' || type == 'b' || type == 'o') {
	for (i = 0; i < n; i++) {
	    xx = x[i];
	    yy = y[i];
	    GConvert(&xx, &yy, USER, DEVICE, dd);
	    if (R_FINITE(xx) && R_FINITE(yy)) {
		if (R_FINITE( (thiscex = REAL(cex)[i % ncex]) ) &&
		    (thispch = INTEGER(pch)[i % npch]) != NA_INTEGER) {
		    /* FIXME: should this skip 0-sized symbols? */
		    thiscol = INTEGER(col)[i % ncol];
		    thisbg = INTEGER(bg)[i % nbg];
		    if (!(R_TRANSPARENT(thiscol) &&
			  R_TRANSPARENT(thisbg))) {
			gpptr(dd)->cex = thiscex * gpptr(dd)->cexbase;
			gpptr(dd)->col = thiscol;
			if(nlwd > 1 &&
			   R_FINITE((thislwd = REAL(lwd)[i % nlwd])))
			    gpptr(dd)->lwd = thislwd;
			gpptr(dd)->bg = thisbg;
			GSymbol(xx, yy, DEVICE, thispch, dd);
		    }
		}
	    }
	}
    }
    GMode(0, dd);
    GRestorePars(dd);
    UNPROTECT(6);
    return R_NilValue;
} /* PlotXY */

/* Checks for ... , x0, y0, x1, y1 ... */

static void xypoints(SEXP args, int *n)
{
    int k=0,/* -Wall */ kmin;

    if (!isNumeric(CAR(args)))
	error(_("invalid first argument"));
    SETCAR(args, coerceVector(CAR(args), REALSXP));
    k = LENGTH(CAR(args));
    *n = k; kmin = k;
    args = CDR(args);

    if (!isNumeric(CAR(args)))
	error(_("invalid second argument"));
    k = LENGTH(CAR(args));
    SETCAR(args, coerceVector(CAR(args), REALSXP));
    if (k > *n) *n = k;
    if (k < kmin) kmin = k;
    args = CDR(args);

    if (!isNumeric(CAR(args)))
	error(_("invalid third argument"));
    SETCAR(args, coerceVector(CAR(args), REALSXP));
    k = LENGTH(CAR(args));
    if (k > *n) *n = k;
    if (k < kmin) kmin = k;
    args = CDR(args);

    if (!isNumeric(CAR(args)))
	error(_("invalid fourth argument"));
    SETCAR(args, coerceVector(CAR(args), REALSXP));
    k = LENGTH(CAR(args));
    if (k > *n) *n = k;
    if (k < kmin) kmin = k;
    args = CDR(args);

    if (*n > 0 && kmin == 0)
	error(_("cannot mix zero-length and non-zero-length coordinates"));
}


SEXP C_segments(SEXP args)
{
    /* segments(x0, y0, x1, y1, col, lty, lwd, ...) */
    SEXP sx0, sx1, sy0, sy1, col, lty, lwd;
    double *x0, *x1, *y0, *y1;
    double xx[2], yy[2];
    int nx0, nx1, ny0, ny1, i, n, ncol, nlty, nlwd;
    pGEDevDesc dd = GEcurrentDevice();

    args = CDR(args);
    if (length(args) < 4) error(_("too few arguments"));
    GCheckState(dd);

    xypoints(args, &n);
    if(n == 0) return R_NilValue;

    sx0 = CAR(args); nx0 = length(sx0); args = CDR(args);
    sy0 = CAR(args); ny0 = length(sy0); args = CDR(args);
    sx1 = CAR(args); nx1 = length(sx1); args = CDR(args);
    sy1 = CAR(args); ny1 = length(sy1); args = CDR(args);

    PROTECT(col = FixupCol(CAR(args), R_TRANWHITE));
    ncol = LENGTH(col); args = CDR(args);

    PROTECT(lty = FixupLty(CAR(args), gpptr(dd)->lty));
    nlty = length(lty); args = CDR(args);

    PROTECT(lwd = FixupLwd(CAR(args), gpptr(dd)->lwd));
    nlwd = length(lwd); args = CDR(args);

    GSavePars(dd);
    ProcessInlinePars(args, dd);

    x0 = REAL(sx0);
    y0 = REAL(sy0);
    x1 = REAL(sx1);
    y1 = REAL(sy1);

    GMode(1, dd);
    for (i = 0; i < n; i++) {
	xx[0] = x0[i % nx0];
	yy[0] = y0[i % ny0];
	xx[1] = x1[i % nx1];
	yy[1] = y1[i % ny1];
	GConvert(xx, yy, USER, DEVICE, dd);
	GConvert(xx+1, yy+1, USER, DEVICE, dd);
	if (R_FINITE(xx[0]) && R_FINITE(yy[0]) &&
	    R_FINITE(xx[1]) && R_FINITE(yy[1]))
	{
	    int thiscol = INTEGER(col)[i % ncol];
	    if(!R_TRANSPARENT(thiscol)) {
		gpptr(dd)->col = thiscol;
		gpptr(dd)->lty = INTEGER(lty)[i % nlty];
		gpptr(dd)->lwd = REAL(lwd)[i % nlwd];
		GLine(xx[0], yy[0], xx[1], yy[1], DEVICE, dd);
	    }
	}
    }
    GMode(0, dd);
    GRestorePars(dd);

    UNPROTECT(3);
    return R_NilValue;
}


SEXP C_rect(SEXP args)
{
    /* rect(xl, yb, xr, yt, col, border, lty, ...) */
    SEXP sxl, sxr, syb, syt, col, lty, lwd, border;
    double *xl, *xr, *yb, *yt, x0, y0, x1, y1;
    int i, n, nxl, nxr, nyb, nyt, ncol, nlty, nlwd, nborder;
    pGEDevDesc dd = GEcurrentDevice();

    args = CDR(args);
    if (length(args) < 4) error(_("too few arguments"));
    GCheckState(dd);

    xypoints(args, &n);
    if(n == 0) return R_NilValue;

    sxl = CAR(args); nxl = length(sxl); args = CDR(args);/* x_left */
    syb = CAR(args); nyb = length(syb); args = CDR(args);/* y_bottom */
    sxr = CAR(args); nxr = length(sxr); args = CDR(args);/* x_right */
    syt = CAR(args); nyt = length(syt); args = CDR(args);/* y_top */

    PROTECT(col = FixupCol(CAR(args), R_TRANWHITE));
    ncol = LENGTH(col);
    args = CDR(args);

    PROTECT(border =  FixupCol(CAR(args), gpptr(dd)->fg));
    nborder = LENGTH(border);
    args = CDR(args);

    PROTECT(lty = FixupLty(CAR(args), gpptr(dd)->lty));
    nlty = length(lty);
    args = CDR(args);

    PROTECT(lwd = FixupLwd(CAR(args), gpptr(dd)->lwd));
    nlwd = length(lwd);
    args = CDR(args);

    GSavePars(dd);
    ProcessInlinePars(args, dd);

    xl = REAL(sxl);
    xr = REAL(sxr);
    yb = REAL(syb);
    yt = REAL(syt);

    GMode(1, dd);
    for (i = 0; i < n; i++) {
	if (nlty && INTEGER(lty)[i % nlty] != NA_INTEGER)
	    gpptr(dd)->lty = INTEGER(lty)[i % nlty];
	else
	    gpptr(dd)->lty = dpptr(dd)->lty;
	if (nlwd && REAL(lwd)[i % nlwd] != NA_REAL)
	    gpptr(dd)->lwd = REAL(lwd)[i % nlwd];
	else
	    gpptr(dd)->lwd = dpptr(dd)->lwd;
	x0 = xl[i % nxl];
	y0 = yb[i % nyb];
	x1 = xr[i % nxr];
	y1 = yt[i % nyt];
	GConvert(&x0, &y0, USER, DEVICE, dd);
	GConvert(&x1, &y1, USER, DEVICE, dd);
	if (R_FINITE(x0) && R_FINITE(y0) && R_FINITE(x1) && R_FINITE(y1))
	    GRect(x0, y0, x1, y1, DEVICE, INTEGER(col)[i % ncol],
		  INTEGER(border)[i % nborder], dd);
    }
    GMode(0, dd);

    GRestorePars(dd);
    UNPROTECT(4);
    return R_NilValue;
}

SEXP C_path(SEXP args)
{
    /* path(x, y, col, border, lty, ...) */
    SEXP sx, sy, nper, rule, col, border, lty;
    int i, nx, npoly;
    double *xx, *yy;
    const void *vmax = NULL /* -Wall */;

    pGEDevDesc dd = GEcurrentDevice();

    GCheckState(dd);

    args = CDR(args);
    if (length(args) < 2) error(_("too few arguments"));
    /* (x,y) is checked in R via xy.coords() ; no need here : */
    sx = SETCAR(args, coerceVector(CAR(args), REALSXP));  args = CDR(args);
    sy = SETCAR(args, coerceVector(CAR(args), REALSXP));  args = CDR(args);
    nx = LENGTH(sx);

    PROTECT(nper = CAR(args)); args = CDR(args);
    npoly = LENGTH(nper);

    PROTECT(rule = CAR(args)); args = CDR(args);

    PROTECT(col = FixupCol(CAR(args), R_TRANWHITE));	args = CDR(args);
    PROTECT(border = FixupCol(CAR(args), gpptr(dd)->fg)); args = CDR(args);
    PROTECT(lty = FixupLty(CAR(args), gpptr(dd)->lty)); args = CDR(args);

    GSavePars(dd);
    ProcessInlinePars(args, dd);

    GMode(1, dd);

    vmax = vmaxget();

    /*
     * Work in device coordinates because that is what the
     * graphics engine needs.
     */
    xx = (double*) R_alloc(nx, sizeof(double));
    yy = (double*) R_alloc(nx, sizeof(double));
    if (!xx || !yy)
	error("unable to allocate memory (in GPath)");
    for (i=0; i<nx; i++) {
        xx[i] = REAL(sx)[i];
        yy[i] = REAL(sy)[i];
        GConvert(&(xx[i]), &(yy[i]), USER, DEVICE, dd);
        if (!(R_FINITE(xx[i]) && R_FINITE(yy[i])))
            error("invalid 'x' or 'y' (in 'GPath')");
    }

    if (INTEGER(lty)[0] == NA_INTEGER)
	gpptr(dd)->lty = dpptr(dd)->lty;
    else
	gpptr(dd)->lty = INTEGER(lty)[0];

    GPath(xx, yy, npoly, INTEGER(nper), INTEGER(rule)[0] == 1,
          INTEGER(col)[0], INTEGER(border)[0], dd);

    GMode(0, dd);

    GRestorePars(dd);
    UNPROTECT(5);

    vmaxset(vmax);
    return R_NilValue;
}

SEXP C_raster(SEXP args)
{
    /* raster(image, xl, yb, xr, yt, angle, interpolate, ...) */
    const void *vmax;
    unsigned int *image;
    SEXP raster, dim, sxl, sxr, syb, syt, angle, interpolate;
    double *xl, *xr, *yb, *yt, x0, y0, x1, y1;
    int i, n, nxl, nxr, nyb, nyt;
    pGEDevDesc dd = GEcurrentDevice();

    args = CDR(args);
    if (length(args) < 7) error(_("too few arguments"));
    GCheckState(dd);

    raster = CAR(args); args = CDR(args);
    n = LENGTH(raster);
    if (n <= 0) error(_("Empty raster"));  
    dim = getAttrib(raster, R_DimSymbol);

    vmax = vmaxget();
    /* raster is rather inefficient so allow a native representation as
       an integer array which requires no conversion */
    if (inherits(raster, "nativeRaster") && isInteger(raster))
	image = (unsigned int *) INTEGER(raster);
    else {
	image = (unsigned int *) R_alloc(n, sizeof(unsigned int));
	for (i = 0; i < n; i++)
	    image[i] = RGBpar3(raster, i, R_TRANWHITE);
    }

    xypoints(args, &n);
    if(n == 0) return R_NilValue;

    sxl = CAR(args); nxl = length(sxl); args = CDR(args);/* x_left */
    syb = CAR(args); nyb = length(syb); args = CDR(args);/* y_bottom */
    sxr = CAR(args); nxr = length(sxr); args = CDR(args);/* x_right */
    syt = CAR(args); nyt = length(syt); args = CDR(args);/* y_top */

    angle = CAR(args); args = CDR(args);
    interpolate = CAR(args); args = CDR(args);

    GSavePars(dd);
    ProcessInlinePars(args, dd);

    xl = REAL(sxl);
    xr = REAL(sxr);
    yb = REAL(syb);
    yt = REAL(syt);

    GMode(1, dd);
    for (i = 0; i < n; i++) {
	x0 = xl[i % nxl];
	y0 = yb[i % nyb];
	x1 = xr[i % nxr];
	y1 = yt[i % nyt];
	GConvert(&x0, &y0, USER, DEVICE, dd);
	GConvert(&x1, &y1, USER, DEVICE, dd);
	if (R_FINITE(x0) && R_FINITE(y0) && R_FINITE(x1) && R_FINITE(y1))
           GRaster(image, INTEGER(dim)[1], INTEGER(dim)[0],
                   x0, y0, x1 - x0, y1 - y0,
                   REAL(angle)[i % LENGTH(angle)],
                   LOGICAL(interpolate)[i % LENGTH(interpolate)], dd);
    }
    GMode(0, dd);

    GRestorePars(dd);

    vmaxset(vmax);
    return R_NilValue;
}


SEXP C_arrows(SEXP args)
{
    /* arrows(x0, y0, x1, y1, length, angle, code, col, lty, lwd, ...) */
    SEXP sx0, sx1, sy0, sy1, col, lty, lwd;
    double *x0, *x1, *y0, *y1;
    double xx0, yy0, xx1, yy1;
    double hlength, angle;
    int code;
    int nx0, nx1, ny0, ny1, i, n, ncol, nlty, nlwd;
    rcolor thiscol;
    pGEDevDesc dd = GEcurrentDevice();

    args = CDR(args);
    if (length(args) < 4) error(_("too few arguments"));
    GCheckState(dd);

    xypoints(args, &n);
    if(n == 0) return R_NilValue;

    sx0 = CAR(args); nx0 = length(sx0); args = CDR(args);
    sy0 = CAR(args); ny0 = length(sy0); args = CDR(args);
    sx1 = CAR(args); nx1 = length(sx1); args = CDR(args);
    sy1 = CAR(args); ny1 = length(sy1); args = CDR(args);

    hlength = asReal(CAR(args));
    if (!R_FINITE(hlength) || hlength < 0)
	error(_("invalid arrow head length"));
    args = CDR(args);

    angle = asReal(CAR(args));
    if (!R_FINITE(angle))
	error(_("invalid arrow head angle"));
    args = CDR(args);

    code = asInteger(CAR(args));
    if (code == NA_INTEGER || code < 0 || code > 3)
	error(_("invalid arrow head specification"));
    args = CDR(args);

    PROTECT(col = FixupCol(CAR(args), R_TRANWHITE));
    ncol = LENGTH(col);
    args = CDR(args);

    PROTECT(lty = FixupLty(CAR(args), gpptr(dd)->lty));
    nlty = length(lty);
    args = CDR(args);

    PROTECT(lwd = FixupLwd(CAR(args), gpptr(dd)->lwd));
    nlwd = length(lwd);
    args = CDR(args);

    GSavePars(dd);
    ProcessInlinePars(args, dd);

    x0 = REAL(sx0);
    y0 = REAL(sy0);
    x1 = REAL(sx1);
    y1 = REAL(sy1);

    GMode(1, dd);
    for (i = 0; i < n; i++) {
	xx0 = x0[i % nx0];
	yy0 = y0[i % ny0];
	xx1 = x1[i % nx1];
	yy1 = y1[i % ny1];
	GConvert(&xx0, &yy0, USER, DEVICE, dd);
	GConvert(&xx1, &yy1, USER, DEVICE, dd);
	if (R_FINITE(xx0) && R_FINITE(yy0) && R_FINITE(xx1) && R_FINITE(yy1)
	    && !R_TRANSPARENT(thiscol = INTEGER(col)[i % ncol])) {
	    gpptr(dd)->col = thiscol;
	    gpptr(dd)->lty = INTEGER(lty)[i % nlty];
	    gpptr(dd)->lwd = REAL(lwd)[i % nlwd];
	    GArrow(xx0, yy0, xx1, yy1, DEVICE,
		   hlength, angle, code, dd);
	}
    }
    GMode(0, dd);
    GRestorePars(dd);

    UNPROTECT(3);
    return R_NilValue;
}


static void drawPolygon(int n, double *x, double *y,
			int lty, int fill, int border, pGEDevDesc dd)
{
    if (lty == NA_INTEGER)
	gpptr(dd)->lty = dpptr(dd)->lty;
    else
	gpptr(dd)->lty = lty;
    GPolygon(n, x, y, USER, fill, border, dd);
}

SEXP C_polygon(SEXP args)
{
    /* polygon(x, y, col, border, lty, ...) */
    SEXP sx, sy, col, border, lty;
    int nx;
    int ncol, nborder, nlty, i, start=0;
    int num = 0;
    double *x, *y, xx, yy, xold, yold;

    pGEDevDesc dd = GEcurrentDevice();

    GCheckState(dd);

    args = CDR(args);
    if (length(args) < 2) error(_("too few arguments"));
    /* (x,y) is checked in R via xy.coords() ; no need here : */
    sx = SETCAR(args, coerceVector(CAR(args), REALSXP));  args = CDR(args);
    sy = SETCAR(args, coerceVector(CAR(args), REALSXP));  args = CDR(args);
    nx = LENGTH(sx);

    PROTECT(col = FixupCol(CAR(args), R_TRANWHITE));	args = CDR(args);
    ncol = LENGTH(col);

    PROTECT(border = FixupCol(CAR(args), gpptr(dd)->fg)); args = CDR(args);
    nborder = LENGTH(border);

    PROTECT(lty = FixupLty(CAR(args), gpptr(dd)->lty)); args = CDR(args);
    nlty = length(lty);

    GSavePars(dd);
    ProcessInlinePars(args, dd);

    GMode(1, dd);

    x = REAL(sx);
    y = REAL(sy);
    xold = NA_REAL;
    yold = NA_REAL;
    for (i = 0; i < nx; i++) {
	xx = x[i];
	yy = y[i];
	GConvert(&xx, &yy, USER, DEVICE, dd);
	if ((R_FINITE(xx) && R_FINITE(yy)) &&
	    !(R_FINITE(xold) && R_FINITE(yold)))
	    start = i; /* first point of current segment */
	else if ((R_FINITE(xold) && R_FINITE(yold)) &&
		 !(R_FINITE(xx) && R_FINITE(yy))) {
	    if (i-start > 1) {
		drawPolygon(i-start, x+start, y+start,
			    INTEGER(lty)[num%nlty],
			    INTEGER(col)[num%ncol],
			    INTEGER(border)[num%nborder], dd);
		num++;
	    }
	}
	else if ((R_FINITE(xold) && R_FINITE(yold)) && (i == nx-1)) { /* last */
	    drawPolygon(nx-start, x+start, y+start,
			INTEGER(lty)[num%nlty],
			INTEGER(col)[num%ncol],
			INTEGER(border)[num%nborder], dd);
	    num++;
	}
	xold = xx;
	yold = yy;
    }

    GMode(0, dd);

    GRestorePars(dd);
    UNPROTECT(3);
    return R_NilValue;
}

SEXP C_text(SEXP args)
{
/* text(xy, labels, adj, pos, offset,
 *	vfont, cex, col, font, ...)
 */
    SEXP sx, sy, sxy, txt, adj, pos, cex, col, rawcol, font, vfont;
    int i, n, npos, ncex, ncol, nfont, ntxt;
    double adjx = 0, adjy = 0, offset = 0.5;
    double *x, *y;
    double xx, yy;
    Rboolean vectorFonts = FALSE;
    SEXP string;
    pGEDevDesc dd = GEcurrentDevice();

    GCheckState(dd);

    args = CDR(args);
    if (length(args) < 3) error(_("too few arguments"));

    PLOT_XY_DEALING("text");

    /* labels */
    txt = CAR(args);
    if (isSymbol(txt) || isLanguage(txt))
	txt = coerceVector(txt, EXPRSXP);
    else if (!isExpression(txt))
	txt = coerceVector(txt, STRSXP);
    PROTECT(txt);
    if (length(txt) <= 0)
	error(_("zero-length '%s' specified"), "labels");
    args = CDR(args);

    PROTECT(adj = CAR(args));
    if (isNull(adj) || (isNumeric(adj) && length(adj) == 0)) {
	adjx = gpptr(dd)->adj;
	adjy = NA_REAL;
    }
    else if (isReal(adj)) {
	if (LENGTH(adj) == 1) {
	    adjx = REAL(adj)[0];
	    adjy = NA_REAL;
	}
	else {
	    adjx = REAL(adj)[0];
	    adjy = REAL(adj)[1];
	}
    }
    else if (isInteger(adj)) {
	if (LENGTH(adj) == 1) {
	    adjx = INTEGER(adj)[0];
	    adjy = NA_REAL;
	}
	else {
	    adjx = INTEGER(adj)[0];
	    adjy = INTEGER(adj)[1];
	}
    }
    else error(_("invalid '%s' value"), "adj");
    args = CDR(args);

    PROTECT(pos = coerceVector(CAR(args), INTSXP));
    npos = length(pos);
    for (i = 0; i < npos; i++)
	if (INTEGER(pos)[i] < 1 || INTEGER(pos)[i] > 4)
	    error(_("invalid '%s' value"), "pos");
    args = CDR(args);

    offset = GConvertXUnits(asReal(CAR(args)), CHARS, INCHES, dd);
    args = CDR(args);

    PROTECT(vfont = FixupVFont(CAR(args)));
    args = CDR(args);

    PROTECT(cex = FixupCex(CAR(args), 1.0));
    ncex = LENGTH(cex);
    args = CDR(args);

    rawcol = CAR(args);
    PROTECT(col = FixupCol(rawcol, R_TRANWHITE));
    ncol = LENGTH(col);
    args = CDR(args);

    PROTECT(font = FixupFont(CAR(args), NA_INTEGER));
    nfont = LENGTH(font);
    args = CDR(args);

    x = REAL(sx);
    y = REAL(sy);
    /* n = LENGTH(sx) = LENGTH(sy) */
    ntxt = LENGTH(txt);

    GSavePars(dd);
    ProcessInlinePars(args, dd);

    /* Done here so 'vfont' trumps inline 'family' */
    if (!isNull(vfont) && !isExpression(txt)) {
	strncpy(gpptr(dd)->family, "Her ", 201);
	gpptr(dd)->family[3] = (char) INTEGER(vfont)[0];
	vectorFonts = TRUE;
    }

    GMode(1, dd);
    if (n == 0 && ntxt > 0)
	error(_("no coordinates were supplied"));
    for (i = 0; i < imax2(n,ntxt); i++) {
	xx = x[i % n];
	yy = y[i % n];
	GConvert(&xx, &yy, USER, INCHES, dd);
	if (R_FINITE(xx) && R_FINITE(yy)) {
	    if (ncol && !isNAcol(rawcol, i, ncol))
		gpptr(dd)->col = INTEGER(col)[i % ncol];
	    else
		gpptr(dd)->col = dpptr(dd)->col;
	    if (ncex && R_FINITE(REAL(cex)[i % ncex]))
		gpptr(dd)->cex = gpptr(dd)->cexbase * REAL(cex)[i % ncex];
	    else
		gpptr(dd)->cex = gpptr(dd)->cexbase;

	    if (vectorFonts) gpptr(dd)->font = INTEGER(vfont)[1];
	    else if (nfont && INTEGER(font)[i % nfont] != NA_INTEGER)
		gpptr(dd)->font = INTEGER(font)[i % nfont];
	    else
		gpptr(dd)->font = dpptr(dd)->font;

	    if (npos > 0) {
		switch(INTEGER(pos)[i % npos]) {
		case 1:
		    yy = yy - offset;
		    adjx = 0.5;
		    adjy = 1 - (0.5 - dd->dev->yCharOffset);
		    break;
		case 2:
		    xx = xx - offset;
		    adjx = 1;
		    adjy = dd->dev->yCharOffset;
		    break;
		case 3:
		    yy = yy + offset;
		    adjx = 0.5;
		    adjy = 0;
		    break;
		case 4:
		    xx = xx + offset;
		    adjx = 0;
		    adjy = dd->dev->yCharOffset;
		    break;
		}
	    }
	    if (isExpression(txt)) {
		GMathText(xx, yy, INCHES, VECTOR_ELT(txt, i % ntxt),
			  adjx, adjy, gpptr(dd)->srt, dd);
	    } else {
		string = STRING_ELT(txt, i % ntxt);
		if(string != NA_STRING)
		    GText(xx, yy, INCHES, CHAR(string), getCharCE(string),
			  adjx, adjy, gpptr(dd)->srt, dd);
	    }
	}
    }
    GMode(0, dd);

    GRestorePars(dd);
    UNPROTECT(7);
    return R_NilValue;
}

static double ComputeAdjValue(double adj, int side, int las)
{
    if (!R_FINITE(adj)) {
	switch(las) {
	case 0:/* parallel to axis */
	    adj = 0.5; break;
	case 1:/* horizontal */
	    switch(side) {
	    case 1:
	    case 3: adj = 0.5; break;
	    case 2: adj = 1.0; break;
	    case 4: adj = 0.0; break;
	    }
	    break;
	case 2:/* perpendicular to axis */
	    switch(side) {
	    case 1:
	    case 2: adj = 1.0; break;
	    case 3:
	    case 4: adj = 0.0; break;
	    }
	    break;
	case 3:/* vertical */
	    switch(side) {
	    case 1: adj = 1.0; break;
	    case 3: adj = 0.0; break;
	    case 2:
	    case 4: adj = 0.5; break;
	    }
	    break;
	}
    }
    return adj;
}

static double ComputeAtValueFromAdj(double adj, int side, int outer,
				    pGEDevDesc dd)
{
    double at = 0;		/* -Wall */
    switch(side % 2) {
    case 0:
	at  = outer ? adj : yNPCtoUsr(adj, dd);
	break;
    case 1:
	at = outer ? adj : xNPCtoUsr(adj, dd);
	break;
    }
    return at;
}

static double ComputeAtValue(double at, double adj,
			     int side, int las, int outer,
			     pGEDevDesc dd)
{
    if (!R_FINITE(at)) {
	/* If the text is parallel to the axis, use "adj" for "at"
	 * Otherwise, centre the text
	 */
	switch(las) {
	case 0:/* parallel to axis */
	    at = ComputeAtValueFromAdj(adj, side, outer, dd);
	    break;
	case 1:/* horizontal */
	    switch(side) {
	    case 1:
	    case 3:
		at = ComputeAtValueFromAdj(adj, side, outer, dd);
		break;
	    case 2:
	    case 4:
		at = outer ? 0.5 : yNPCtoUsr(0.5, dd);
		break;
	    }
	    break;
	case 2:/* perpendicular to axis */
	    switch(side) {
	    case 1:
	    case 3:
		at = outer ? 0.5 : xNPCtoUsr(0.5, dd);
		break;
	    case 2:
	    case 4:
		at = outer ? 0.5 : yNPCtoUsr(0.5, dd);
		break;
	    }
	    break;
	case 3:/* vertical */
	    switch(side) {
	    case 1:
	    case 3:
		at = outer ? 0.5 : xNPCtoUsr(0.5, dd);
		break;
	    case 2:
	    case 4:
		at = ComputeAtValueFromAdj(adj, side, outer, dd);
		break;
	    }
	    break;
	}
    }
    return at;
}

/* mtext(text,
	 side = 3,
	 line = 0,
	 outer = TRUE,
	 at = NA,
	 adj = NA,
	 padj = NA,
	 cex = NA,
	 col = NA,
	 font = NA,
	 ...) */

SEXP C_mtext(SEXP args)
{
    SEXP text, side, line, outer, at, adj, padj, cex, col, font, string;
    SEXP rawcol;
    int ntext, nside, nline, nouter, nat, nadj, npadj, ncex, ncol, nfont;
    Rboolean dirtyplot = FALSE, gpnewsave = FALSE, dpnewsave = FALSE;
    int i, n, fontsave, colsave;
    double cexsave;
    pGEDevDesc dd = GEcurrentDevice();

    GCheckState(dd);

    args = CDR(args);
    if (length(args) < 9)
	error(_("too few arguments"));

    /* Arg1 : text= */
    text = CAR(args);
    if (isSymbol(text) || isLanguage(text))
	text = coerceVector(text, EXPRSXP);
    else if (!isExpression(text))
	text = coerceVector(text, STRSXP);
    PROTECT(text);
    n = ntext = length(text);
    if (ntext <= 0)
	error(_("zero-length '%s' specified"), "text");
    args = CDR(args);

    /* Arg2 : side= */
    PROTECT(side = coerceVector(CAR(args), INTSXP));
    nside = length(side);
    if (nside <= 0) error(_("zero-length '%s' specified"), "side");
    if (n < nside) n = nside;
    args = CDR(args);

    /* Arg3 : line= */
    PROTECT(line = coerceVector(CAR(args), REALSXP));
    nline = length(line);
    if (nline <= 0) error(_("zero-length '%s' specified"), "line");
    if (n < nline) n = nline;
    args = CDR(args);

    /* Arg4 : outer= */
    /* outer == NA => outer <- 0 */
    PROTECT(outer = coerceVector(CAR(args), INTSXP));
    nouter = length(outer);
    if (nouter <= 0) error(_("zero-length '%s' specified"), "outer");
    if (n < nouter) n = nouter;
    args = CDR(args);

    /* Arg5 : at= */
    PROTECT(at = coerceVector(CAR(args), REALSXP));
    nat = length(at);
    if (nat <= 0) error(_("zero-length '%s' specified"), "at");
    if (n < nat) n = nat;
    args = CDR(args);

    /* Arg6 : adj= */
    PROTECT(adj = coerceVector(CAR(args), REALSXP));
    nadj = length(adj);
    if (nadj <= 0) error(_("zero-length '%s' specified"), "adj");
    if (n < nadj) n = nadj;
    args = CDR(args);

    /* Arg7 : padj= */
    PROTECT(padj = coerceVector(CAR(args), REALSXP));
    npadj = length(padj);
    if (npadj <= 0) error(_("zero-length '%s' specified"), "padj");
    if (n < npadj) n = npadj;
    args = CDR(args);

    /* Arg8 : cex */
    PROTECT(cex = FixupCex(CAR(args), 1.0));
    ncex = length(cex);
    if (ncex <= 0) error(_("zero-length '%s' specified"), "cex");
    if (n < ncex) n = ncex;
    args = CDR(args);

    /* Arg9 : col */
    rawcol = CAR(args);
    PROTECT(col = FixupCol(rawcol, R_TRANWHITE));
    ncol = length(col);
    if (ncol <= 0) error(_("zero-length '%s' specified"), "col");
    if (n < ncol) n = ncol;
    args = CDR(args);

    /* Arg10 : font */
    PROTECT(font = FixupFont(CAR(args), NA_INTEGER));
    nfont = length(font);
    if (nfont <= 0) error(_("zero-length '%s' specified"), "font");
    if (n < nfont) n = nfont;
    args = CDR(args);

    GSavePars(dd);
    ProcessInlinePars(args, dd);

    /* If we only scribble in the outer margins, */
    /* we don't want to mark the plot as dirty. */

    dirtyplot = FALSE;
    gpnewsave = gpptr(dd)->new;
    dpnewsave = dpptr(dd)->new;
    cexsave = gpptr(dd)->cex;
    fontsave = gpptr(dd)->font;
    colsave = gpptr(dd)->col;

    /* override par("xpd") and force clipping to figure region
       NOTE: don't override to _reduce_ clipping region */
    if (gpptr(dd)->xpd < 1)
	gpptr(dd)->xpd = 1;

    if (outer) {
	gpnewsave = gpptr(dd)->new;
	dpnewsave = dpptr(dd)->new;
	/* override par("xpd") and force clipping to device region */
	gpptr(dd)->xpd = 2;
    }
    GMode(1, dd);

    for (i = 0; i < n; i++) {
	double atval = REAL(at)[i % nat];
	double adjval = REAL(adj)[i % nadj];
	double padjval = REAL(padj)[i % npadj];
	double cexval = REAL(cex)[i % ncex];
	double lineval = REAL(line)[i % nline];
	int outerval = INTEGER(outer)[i % nouter];
	int sideval = INTEGER(side)[i % nside];
	int fontval = INTEGER(font)[i % nfont];
	int colval = INTEGER(col)[i % ncol];

	if (outerval == NA_INTEGER) outerval = 0;
	/* Note : we ignore any shrinking produced */
	/* by mfrow / mfcol specs here.	 I.e. don't */
	/* gpptr(dd)->cexbase. */
	if (R_FINITE(cexval)) gpptr(dd)->cex = cexval;
	else cexval = cexsave;
	gpptr(dd)->font = (fontval == NA_INTEGER) ? fontsave : fontval;
	if (isNAcol(rawcol, i, ncol))
	    gpptr(dd)->col = colsave;
	else
	    gpptr(dd)->col = colval;
	gpptr(dd)->adj = ComputeAdjValue(adjval, sideval, gpptr(dd)->las);
	padjval = ComputePAdjValue(padjval, sideval, gpptr(dd)->las);
	atval = ComputeAtValue(atval, gpptr(dd)->adj, sideval, gpptr(dd)->las,
			       outerval, dd);

	if (isExpression(text))
	    GMMathText(VECTOR_ELT(text, i % ntext),
		       sideval, lineval, outerval, atval, gpptr(dd)->las,
		       padjval, dd);
	else {
	    string = STRING_ELT(text, i % ntext);
	    if(string != NA_STRING)
		GMtext(CHAR(string), getCharCE(string), sideval, lineval,
		       outerval, atval, gpptr(dd)->las, padjval, dd);
	}

	if (outerval == 0) dirtyplot = TRUE;
    }
    GMode(0, dd);

    GRestorePars(dd);
    if (!dirtyplot) {
	gpptr(dd)->new = gpnewsave;
	dpptr(dd)->new = dpnewsave;
    }
    UNPROTECT(10);
    return R_NilValue;
} /* Mtext */


SEXP C_title(SEXP args)
{
/* Annotation for plots :

   title(main, sub, xlab, ylab,
	 line, outer,
	 ...) */

    SEXP Main, xlab, ylab, sub, string;
    double adj, adjy, cex, offset, line, hpos, vpos;
    int i, n, font, outer, where;
    rcolor col;
    pGEDevDesc dd = GEcurrentDevice();

    GCheckState(dd);

    args = CDR(args);
    if (length(args) < 6) error(_("too few arguments"));

    Main = sub = xlab = ylab = R_NilValue;

    if (CAR(args) != R_NilValue && length(CAR(args)) > 0)
	Main = CAR(args);
    args = CDR(args);

    if (CAR(args) != R_NilValue && length(CAR(args)) > 0)
	sub = CAR(args);
    args = CDR(args);

    if (CAR(args) != R_NilValue && length(CAR(args)) > 0)
	xlab = CAR(args);
    args = CDR(args);

    if (CAR(args) != R_NilValue && length(CAR(args)) > 0)
	ylab = CAR(args);
    args = CDR(args);

    line = asReal(CAR(args));
    args = CDR(args);

    outer = asLogical(CAR(args));
    if (outer == NA_LOGICAL) outer = 0;
    args = CDR(args);

    GSavePars(dd);
    ProcessInlinePars(args, dd);

    /* override par("xpd") and force clipping to figure region
       NOTE: don't override to _reduce_ clipping region */
    if (gpptr(dd)->xpd < 1)
	gpptr(dd)->xpd = 1;
    if (outer)
	gpptr(dd)->xpd = 2;
    adj = gpptr(dd)->adj;

    GMode(1, dd);
    if (Main != R_NilValue) {
	cex = gpptr(dd)->cexmain;
	col = gpptr(dd)->colmain;
	font = gpptr(dd)->fontmain;
	/* GetTextArg may coerce, so protect the result */
	GetTextArg(Main, &Main, &col, &cex, &font);
	PROTECT(Main);
	gpptr(dd)->col = col;
	gpptr(dd)->cex = gpptr(dd)->cexbase * cex;
	gpptr(dd)->font = font;
	if (outer) {
	    if (R_FINITE(line)) {
		vpos = line;
		adjy = 0;
	    }
	    else {
		vpos = 0.5 * gpptr(dd)->oma[2];
		adjy = 0.5;
	    }
	    hpos = adj;
	    where = OMA3;
	}
	else {
	    if (R_FINITE(line)) {
		vpos = line;
		adjy = 0;
	    }
	    else {
		vpos = 0.5 * gpptr(dd)->mar[2];
		adjy = 0.5;
	    }
	    hpos = GConvertX(adj, NPC, USER, dd);
	    where = MAR3;
	}
	if (isExpression(Main)) {
	    GMathText(hpos, vpos, where, VECTOR_ELT(Main, 0),
		      adj, 0.5, 0.0, dd);
	}
	else {
	  n = length(Main);
	  offset = 0.5 * (n - 1) + vpos;
	  for (i = 0; i < n; i++) {
		string = STRING_ELT(Main, i);
		if(string != NA_STRING)
		    GText(hpos, offset - i, where, CHAR(string), getCharCE(string),
			  adj, adjy, 0.0, dd);
	  }
	}
	UNPROTECT(1);
    }
    if (sub != R_NilValue) {
	cex = gpptr(dd)->cexsub;
	col = gpptr(dd)->colsub;
	font = gpptr(dd)->fontsub;
	/* GetTextArg may coerce, so protect the result */
	GetTextArg(sub, &sub, &col, &cex, &font);
	PROTECT(sub);
	gpptr(dd)->col = col;
	gpptr(dd)->cex = gpptr(dd)->cexbase * cex;
	gpptr(dd)->font = font;
	if (R_FINITE(line))
	    vpos = line;
	else
	    vpos = gpptr(dd)->mgp[0] + 1;
	if (outer) {
	    hpos = adj;
	    where = 1;
	}
	else {
	    hpos = GConvertX(adj, NPC, USER, dd);
	    where = 0;
	}
	if (isExpression(sub))
	    GMMathText(VECTOR_ELT(sub, 0), 1, vpos, where,
		       hpos, 0, 0.0, dd);
	else {
	    n = length(sub);
	    for (i = 0; i < n; i++) {
		string = STRING_ELT(sub, i);
		if(string != NA_STRING)
		    GMtext(CHAR(string), getCharCE(string), 1, vpos, where,
			   hpos, 0, 0.0, dd);
	    }
	}
	UNPROTECT(1);
    }
    if (xlab != R_NilValue) {
	cex = gpptr(dd)->cexlab;
	col = gpptr(dd)->collab;
	font = gpptr(dd)->fontlab;
	/* GetTextArg may coerce, so protect the result */
	GetTextArg(xlab, &xlab, &col, &cex, &font);
	PROTECT(xlab);
	gpptr(dd)->cex = gpptr(dd)->cexbase * cex;
	gpptr(dd)->col = col;
	gpptr(dd)->font = font;
	if (R_FINITE(line))
	    vpos = line;
	else
	    vpos = gpptr(dd)->mgp[0];
	if (outer) {
	    hpos = adj;
	    where = 1;
	}
	else {
	    hpos = GConvertX(adj, NPC, USER, dd);
	    where = 0;
	}
	if (isExpression(xlab))
	    GMMathText(VECTOR_ELT(xlab, 0), 1, vpos, where,
		       hpos, 0, 0.0, dd);
	else {
	    n = length(xlab);
	    for (i = 0; i < n; i++) {
		string = STRING_ELT(xlab, i);
		if(string != NA_STRING)
		    GMtext(CHAR(string), getCharCE(string), 1, vpos + i,
			   where, hpos, 0, 0.0, dd);
	    }
	}
	UNPROTECT(1);
    }
    if (ylab != R_NilValue) {
	cex = gpptr(dd)->cexlab;
	col = gpptr(dd)->collab;
	font = gpptr(dd)->fontlab;
	/* GetTextArg may coerce, so protect the result */
	GetTextArg(ylab, &ylab, &col, &cex, &font);
	PROTECT(ylab);
	gpptr(dd)->cex = gpptr(dd)->cexbase * cex;
	gpptr(dd)->col = col;
	gpptr(dd)->font = font;
	if (R_FINITE(line))
	    vpos = line;
	else
	    vpos = gpptr(dd)->mgp[0];
	if (outer) {
	    hpos = adj;
	    where = 1;
	}
	else {
	    hpos = GConvertY(adj, NPC, USER, dd);
	    where = 0;
	}
	if (isExpression(ylab))
	    GMMathText(VECTOR_ELT(ylab, 0), 2, vpos, where,
		       hpos, 0, 0.0, dd);
	else {
	    n = length(ylab);
	    for (i = 0; i < n; i++) {
		string = STRING_ELT(ylab, i);
		if(string != NA_STRING)
		    GMtext(CHAR(string), getCharCE(string), 2, vpos - i,
			   where, hpos, 0, 0.0, dd);
	    }
	}
	UNPROTECT(1);
    }
    GMode(0, dd);
    GRestorePars(dd);
    return R_NilValue;
} /* Title */


/*  abline(a, b, h, v, col, lty, lwd, ...)
    draw lines in intercept/slope form.	 */

SEXP C_abline(SEXP args)
{
    SEXP a, b, h, v, untf, col, lty, lwd;
    int i, ncol, nlines, nlty, nlwd, lstart, lstop;
    double aa, bb, x[2], y[2]={0.,0.} /* -Wall */;
    pGEDevDesc dd = GEcurrentDevice();

    GCheckState(dd);

    args = CDR(args);
    if (length(args) < 5) error(_("too few arguments"));

    if ((a = CAR(args)) != R_NilValue)
	SETCAR(args, a = coerceVector(a, REALSXP));
    args = CDR(args);

    if ((b = CAR(args)) != R_NilValue)
	SETCAR(args, b = coerceVector(b, REALSXP));
    args = CDR(args);

    if ((h = CAR(args)) != R_NilValue)
	SETCAR(args, h = coerceVector(h, REALSXP));
    args = CDR(args);

    if ((v = CAR(args)) != R_NilValue)
	SETCAR(args, v = coerceVector(v, REALSXP));
    args = CDR(args);

    if ((untf = CAR(args)) != R_NilValue)
	SETCAR(args, untf = coerceVector(untf, LGLSXP));
    args = CDR(args);


    PROTECT(col = FixupCol(CAR(args), R_TRANWHITE));	args = CDR(args);
    ncol = LENGTH(col);

    PROTECT(lty = FixupLty(CAR(args), gpptr(dd)->lty)); args = CDR(args);
    nlty = length(lty);

    PROTECT(lwd = FixupLwd(CAR(args), gpptr(dd)->lwd)); args = CDR(args);
    nlwd = length(lwd);

    GSavePars(dd);

    ProcessInlinePars(args, dd);

    nlines = 0;

    if (a != R_NilValue) {  /* case where a ans b are supplied */
	if (b == R_NilValue) {
	    if (LENGTH(a) != 2)
		error(_("invalid a=, b= specification"));
	    aa = REAL(a)[0];
	    bb = REAL(a)[1];
	}
	else {
	    aa = asReal(a);
	    bb = asReal(b);
	}
	if (!R_FINITE(aa) || !R_FINITE(bb))
	    error(_("'a' and 'b' must be finite"));
	gpptr(dd)->col = INTEGER(col)[0];
	gpptr(dd)->lwd = REAL(lwd)[0];
	if (nlty && INTEGER(lty)[0] != NA_INTEGER)
	    gpptr(dd)->lty = INTEGER(lty)[0];
	else
	    gpptr(dd)->lty = dpptr(dd)->lty;
	GMode(1, dd);

	/* FIXME?
	 * Seems like the logic here is just draw from xmin to xmax
	 * and you're guaranteed to draw at least from ymin to ymax
	 * This MAY cause a problem at some stage when the line being
	 * drawn is VERY steep -- and the problem is worse now that
	 * abline will potentially draw to the extents of the device
	 * (when xpd = NA).  NOTE that R's internal clipping protects the
	 * device drivers from stupidly large numbers, BUT there is
	 * still a risk that we could produce a number which is too
	 * big for the computer's brain.
	 * Paul.
	 *
	 * The problem is worse -- you could get NaN, which at least the
	 * X11 device coerces to -2^31 <TSL>
	 */
	getxlimits(x, dd);/* -> (x[0], x[1]) */
	if (R_FINITE(gpptr(dd)->lwd)) {
	    Rboolean xlog = gpptr(dd)->xlog, ylog = gpptr(dd)->ylog;
	    if (LOGICAL(untf)[0] && (xlog || ylog)) {
#define NS 100
		/* Plot curve, linear on original scales */
		double xx[NS+1], yy[NS+1];
		if(xlog) {
		    /* x_i should be equidistant in log-scale, i.e., equi-ratio */
		    double x_f = x[1] / DBL_MAX;
		    xx[0] = x[0] = fmax2(x[0], 1.01 *x_f); /* > 0 */
		    x_f = pow(x[1]/x[0], 1./NS);
		    for (i = 1; i < NS; i++)
			xx[i] = xx[i-1] * x_f;
		} else {
		    double xstep = (x[1] - x[0])/NS;
		    for (i = 0; i < NS; i++)
			xx[i] = x[0] + i*xstep;
		}
		xx[NS] = x[1];
		for (i = 0; i <= NS; i++)
		    yy[i] = aa + xx[i] * bb;

		/* now get rid of -ve values */
		lstart = 0;lstop = NS;
		if (xlog) {
		    for(; xx[lstart] <= 0 && lstart < NS+1; lstart++);
		    for(; xx[lstop] <= 0 && lstop > 0; lstop--);
		}
		if (ylog) {
		    for(; yy[lstart] <= 0 && lstart < NS+1; lstart++);
		    for(; yy[lstop] <= 0 && lstop > 0; lstop--);
		}

		GPolyline(lstop-lstart+1, xx+lstart, yy+lstart, USER, dd);
#undef NS
	    } else { /* non-log plots, possibly with log scales */

		y[0] = aa + (xlog ? log10(x[0]) : x[0]) * bb;
		y[1] = aa + (xlog ? log10(x[1]) : x[1]) * bb;
		if (ylog) {
		    y[0] = Rexp10(y[0]);
		    y[1] = Rexp10(y[1]);
		}

		GLine(x[0], y[0], x[1], y[1], USER, dd);
	    }
	}
	GMode(0, dd);
	nlines++;
    }
    if (h != R_NilValue) { /* horizontal liee */
	GMode(1, dd);
	for (i = 0; i < LENGTH(h); i++) {
	    gpptr(dd)->col = INTEGER(col)[nlines % ncol];
	    if (nlty && INTEGER(lty)[nlines % nlty] != NA_INTEGER)
		gpptr(dd)->lty = INTEGER(lty)[nlines % nlty];
	    else
		gpptr(dd)->lty = dpptr(dd)->lty;
	    gpptr(dd)->lwd = REAL(lwd)[nlines % nlwd];
	    aa = REAL(h)[i];
	    if (R_FINITE(aa) && R_FINITE(gpptr(dd)->lwd)) {
		getxlimits(x, dd);
		y[0] = aa;
		y[1] = aa;
		GLine(x[0], y[0], x[1], y[1], USER, dd);
	    }
	    nlines++;
	}
	GMode(0, dd);
    }
    if (v != R_NilValue) { /* vertical line */
	GMode(1, dd);
	for (i = 0; i < LENGTH(v); i++) {
	    gpptr(dd)->col = INTEGER(col)[nlines % ncol];
	    if (nlty && INTEGER(lty)[nlines % nlty] != NA_INTEGER)
		gpptr(dd)->lty = INTEGER(lty)[nlines % nlty];
	    else
		gpptr(dd)->lty = dpptr(dd)->lty;
	    gpptr(dd)->lwd = REAL(lwd)[nlines % nlwd];
	    aa = REAL(v)[i];
	    if (R_FINITE(aa) && R_FINITE(gpptr(dd)->lwd)) {
		getylimits(y, dd);
		x[0] = aa;
		x[1] = aa;
		GLine(x[0], y[0], x[1], y[1], USER, dd);
	    }
	    nlines++;
	}
	GMode(0, dd);
    }
    UNPROTECT(3);
    GRestorePars(dd);
    return R_NilValue;
} /* Abline */


SEXP C_box(SEXP args)
{
/*     box(which="plot", lty="solid", ...)
       --- which is coded, 1 = plot, 2 = figure, 3 = inner, 4 = outer.
*/
    int which, col;
    SEXP colsxp, fgsxp;
    pGEDevDesc dd = GEcurrentDevice();

    GCheckState(dd);
    GSavePars(dd);
    args = CDR(args);
    which = asInteger(CAR(args)); args = CDR(args);
    if (which < 1 || which > 4)
	error(_("invalid '%s' argument"), "which");
    /*
     * If specified non-NA col then use that, else ...
     *
     * if specified non-NA fg then use that, else ...
     *
     * else use par("col")
     */
    col= gpptr(dd)->col;
    ProcessInlinePars(args, dd);
    colsxp = getInlinePar(args, "col");
    if (isNAcol(colsxp, 0, 1)) {
	fgsxp = getInlinePar(args, "fg");
	if (isNAcol(fgsxp, 0, 1))
	    gpptr(dd)->col = col;
	else
	    gpptr(dd)->col = gpptr(dd)->fg;
    }
    /* override par("xpd") and force clipping to device region */
    gpptr(dd)->xpd = 2;
    GMode(1, dd);
    GBox(which, dd);
    GMode(0, dd);
    GRestorePars(dd);
    return R_NilValue;
}

static void drawPointsLines(double xp, double yp, double xold, double yold,
			    char type, int first, pGEDevDesc dd)
{
    if (type == 'p' || type == 'o')
	GSymbol(xp, yp, DEVICE, gpptr(dd)->pch, dd);
    if ((type == 'l' || type == 'o') && !first)
	GLine(xold, yold, xp, yp, DEVICE, dd);
}

SEXP C_locator(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x, y, nobs, ans, saveans, stype = R_NilValue;
    int i, n;
    char type = 'p';
    double xp, yp, xold=0, yold=0;
    pGEDevDesc dd = GEcurrentDevice();
    SEXP name = CAR(args);
    
    args = CDR(args);
    /* If replaying, just draw the points and lines that were recorded */
    if (call == R_NilValue) {
	x = CAR(args); args = CDR(args);
	y = CAR(args); args = CDR(args);
	nobs = CAR(args); args = CDR(args);
	n = INTEGER(nobs)[0];
	stype = CAR(args); args = CDR(args);
	type = CHAR(STRING_ELT(stype, 0))[0];
	if (type != 'n') {
	    GMode(1, dd);
	    for (i = 0; i < n; i++) {
		xp = REAL(x)[i];
		yp = REAL(y)[i];
		GConvert(&xp, &yp, USER, DEVICE, dd);
		drawPointsLines(xp, yp, xold, yold, type, i==0, dd);
		xold = xp;
		yold = yp;
	    }
	    GMode(0, dd);
	}
	return R_NilValue;
    } else {
	GCheckState(dd);

	n = asInteger(CAR(args));
	if (n <= 0 || n == NA_INTEGER)
	    error(_("invalid number of points in %s"), "locator()");
	args = CDR(args);
	if (isString(CAR(args)) && LENGTH(CAR(args)) == 1)
	    stype = CAR(args);
	else
	    error(_("invalid plot type"));
	type = CHAR(STRING_ELT(stype, 0))[0];
	PROTECT(x = allocVector(REALSXP, n));
	PROTECT(y = allocVector(REALSXP, n));
	PROTECT(nobs=allocVector(INTSXP,1));

	GMode(2, dd);
	for (i = 0; i < n; i++) {
	    if (!GLocator(&(REAL(x)[i]), &(REAL(y)[i]), USER, dd)) break;
	    if (type != 'n') {
		GMode(1, dd);
		xp = REAL(x)[i];
		yp = REAL(y)[i];
		GConvert(&xp, &yp, USER, DEVICE, dd);
		drawPointsLines(xp, yp, xold, yold, type, i==0, dd);
		GMode(0, dd);
		GMode(2, dd);
		xold = xp; yold = yp;
	    }
	}
	GMode(0, dd);
	INTEGER(nobs)[0] = i;
	for (; i < n; i++) {
	    REAL(x)[i] = NA_REAL;
	    REAL(y)[i] = NA_REAL;
	}
	PROTECT(ans = allocList(3));
	SETCAR(ans, x);
	SETCADR(ans, y);
	SETCADDR(ans, nobs);
	if (GRecording(call, dd)) {
	    PROTECT(saveans = allocList(5));
	    SETCAR(saveans, name);
	    SETCADR(saveans, x);
	    SETCADDR(saveans, y);
	    SETCADDDR(saveans, nobs);
	    SETCAD4R(saveans, CAR(args));
	    /* Record the points and lines that were drawn in the display list */
	    GErecordGraphicOperation(op, saveans, dd);
	    UNPROTECT(1);
	}
	UNPROTECT(4);
	return ans;
    }
}

static void drawLabel(double xi, double yi, int pos, double offset,
		      const char *l, cetype_t enc, pGEDevDesc dd)
{
    switch (pos) {
    case 4:
	xi = xi+offset;
	GText(xi, yi, INCHES, l, enc, 0.0,
	      dd->dev->yCharOffset, 0.0, dd);
	break;
    case 2:
	xi = xi-offset;
	GText(xi, yi, INCHES, l, enc, 1.0,
	      dd->dev->yCharOffset, 0.0, dd);
	break;
    case 3:
	yi = yi+offset;
	GText(xi, yi, INCHES, l, enc, 0.5,
	      0.0, 0.0, dd);
	break;
    case 1:
	yi = yi-offset;
	GText(xi, yi, INCHES, l, enc, 0.5,
	      1-(0.5-dd->dev->yCharOffset),
	      0.0, dd);
	break;
    case 0:
	GText(xi, yi, INCHES, l, enc, 0.0, 0.0, 0.0, dd);
	break;
    }
}

SEXP C_identify(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, x, y, l, ind, pos, Offset, draw, saveans;
    double xi, yi, xp, yp, d, dmin, offset, tol;
    int atpen, i, imin, k, n, nl, npts, plot, posi, warn;
    pGEDevDesc dd = GEcurrentDevice();
    SEXP name = CAR(args);
    
    args = CDR(args);
    /* If we are replaying the display list, then just redraw the
       labels beside the identified points */
    if (call == R_NilValue) {
	ind = CAR(args); args = CDR(args);
	pos = CAR(args); args = CDR(args);
	x = CAR(args); args = CDR(args);
	y = CAR(args); args = CDR(args);
	Offset = CAR(args); args = CDR(args);
	l = CAR(args); args = CDR(args);
	draw = CAR(args);
	n = LENGTH(x);
	nl = LENGTH(l);
	/*
	 * Most of the appropriate settings have been set up in
	 * R code by par(...)
	 * Hence no GSavePars() or ProcessInlinePars() here
	 * (also because this function is unusual in that it does
	 *  different things when run by a user compared to when
	 *  run from the display list)
	 * BUT par(cex) only sets cexbase, so here we set cex from cexbase
	 */
	gpptr(dd)->cex = gpptr(dd)->cexbase;
	offset = GConvertXUnits(asReal(Offset), CHARS, INCHES, dd);
	for (i = 0; i < n; i++) {
	    plot = LOGICAL(ind)[i];
	    if (LOGICAL(draw)[0] && plot) {
		xi = REAL(x)[i];
		yi = REAL(y)[i];
		GConvert(&xi, &yi, USER, INCHES, dd);
		posi = INTEGER(pos)[i];
		drawLabel(xi, yi, posi, offset,
			  CHAR(STRING_ELT(l, i % nl)),
			  getCharCE(STRING_ELT(l, i % nl)), dd);
	    }
	}
	return R_NilValue;
    }
    else {
	GCheckState(dd);

	x = CAR(args); args = CDR(args);
	y = CAR(args); args = CDR(args);
	l = CAR(args); args = CDR(args);
	npts = asInteger(CAR(args)); args = CDR(args);
	plot = asLogical(CAR(args)); args = CDR(args);
	Offset = CAR(args); args = CDR(args);
	tol = asReal(CAR(args)); args = CDR(args);
	atpen = asLogical(CAR(args));
	if (npts <= 0 || npts == NA_INTEGER)
	    error(_("invalid number of points in %s"), "identify()");
	if (!isReal(x) || !isReal(y) || !isString(l) || !isReal(Offset))
	    error(_("incorrect argument type"));
	if (tol <= 0 || ISNAN(tol))
	    error(_("invalid '%s' value"), "tolerance");
	if (plot == NA_LOGICAL)
	    error(_("invalid '%s' value"), "plot");
	if (atpen == NA_LOGICAL)
	    error(_("invalid '%s' value"), "atpen");
	nl = LENGTH(l);
	if (nl <= 0)
	    error(_("zero-length '%s' specified"), "labels");
	n = LENGTH(x);
	if (n != LENGTH(y))
	    error(_("different argument lengths"));
	if (nl > n)
	    warning(_("more 'labels' than points"));

	/*
	 * Most of the appropriate settings have been set up in
	 * R code by par(...)
	 * Hence no GSavePars() or ProcessInlinePars() here
	 * (also because this function is unusual in that it does
	 *  different things when run by a user compared to when
	 *  run from the display list)
	 * BUT par(cex) only sets cexbase, so here we set cex from cexbase
	 */
	gpptr(dd)->cex = gpptr(dd)->cexbase;
	offset = GConvertXUnits(asReal(Offset), CHARS, INCHES, dd);
	PROTECT(ind = allocVector(LGLSXP, n));
	PROTECT(pos = allocVector(INTSXP, n));
	for (i = 0; i < n; i++) LOGICAL(ind)[i] = 0;

	k = 0;
	GMode(2, dd);
	PROTECT(x = duplicate(x));
	PROTECT(y = duplicate(y));
	while (k < npts) {
	    if (!GLocator(&xp, &yp, INCHES, dd)) break;
	    /*
	     * Repeat cex setting from cexbase within loop
	     * so that if window is redrawn
	     * (e.g., conver/uncover window)
	     * during identifying (i.e., between clicks)
	     * we reset cex properly.
	     */
	    gpptr(dd)->cex = gpptr(dd)->cexbase;
	    dmin = DBL_MAX;
	    imin = -1;
	    for (i = 0; i < n; i++) {
		xi = REAL(x)[i];
		yi = REAL(y)[i];
		GConvert(&xi, &yi, USER, INCHES, dd);
		if (!R_FINITE(xi) || !R_FINITE(yi)) continue;
		d = hypot(xp-xi, yp-yi);
		if (d < dmin) {
		    imin = i;
		    dmin = d;
		}
	    }
	    /* can't use warning because we want to print immediately  */
	    /* might want to handle warn=2? */
	    warn = asInteger(GetOption1(install("warn")));
	    if (dmin > tol) {
		if(warn >= 0) {
		    REprintf(_("warning: no point within %.2f inches\n"), tol);
		    R_FlushConsole();
		}
	    }
	    else if (LOGICAL(ind)[imin]) {
		if(warn >= 0 ) {
		    REprintf(_("warning: nearest point already identified\n"));
		    R_FlushConsole();
		}
	    }
	    else {
		k++;
		LOGICAL(ind)[imin] = 1;

		if (atpen) {
		    xi = xp;
		    yi = yp;
		    INTEGER(pos)[imin] = 0;
		    /* now record where to replot if necessary */
		    GConvert(&xp, &yp, INCHES, USER, dd);
		    REAL(x)[imin] = xp; REAL(y)[imin] = yp;
		} else {
		    xi = REAL(x)[imin];
		    yi = REAL(y)[imin];
		    GConvert(&xi, &yi, USER, INCHES, dd);
		    if (fabs(xp-xi) >= fabs(yp-yi)) {
			if (xp >= xi)
			    INTEGER(pos)[imin] = 4;
			else
			    INTEGER(pos)[imin] = 2;
		    } else {
			if (yp >= yi)
			    INTEGER(pos)[imin] = 3;
			else
			    INTEGER(pos)[imin] = 1;
		    }
		}
		if (plot) {
		    drawLabel(xi, yi, INTEGER(pos)[imin], offset,
			      CHAR(STRING_ELT(l, imin % nl)),
			      getCharCE(STRING_ELT(l, imin % nl)), dd);
		    GMode(0, dd);
		    GMode(2, dd);
		}
	    }
	}
	GMode(0, dd);
	PROTECT(ans = allocList(2));
	SETCAR(ans, ind);
	SETCADR(ans, pos);
	if (GRecording(call, dd)) {
	    /* If we are recording, save enough information to be able to
	       redraw the text labels beside identified points */
	    PROTECT(saveans = allocList(8));
	    SETCAR(saveans, name);
	    SETCADR(saveans, ind);
	    SETCADDR(saveans, pos);
	    SETCADDDR(saveans, x);
	    SETCAD4R(saveans, y);
	    SETCAR(nthcdr(saveans,5), Offset);
	    SETCAR(nthcdr(saveans,6), l);
	    SETCAR(nthcdr(saveans,7), ScalarLogical(plot));

	    GErecordGraphicOperation(op, saveans, dd);
	    UNPROTECT(1);
	}
	UNPROTECT(5);

	return ans;
    }
}

/* strheight(str, units, cex, font, vfont, ...)  ||  strwidth() */
#define DO_STR_DIM(KIND)						\
{									\
    SEXP ans, str, ch, font, vfont;					\
    int i, n, units;							\
    double cex, cexsave;						\
    pGEDevDesc dd = GEcurrentDevice();					\
    args = CDR(args);							\
    if (length(args) < 5) error(_("too few arguments"));		\
									\
    str = CAR(args);							\
    if (isSymbol(str) || isLanguage(str))				\
	str = coerceVector(str, EXPRSXP);				\
    else if (!isExpression(str))					\
	str = coerceVector(str, STRSXP);				\
    PROTECT(str);							\
    args = CDR(args);							\
									\
    if ((units = asInteger(CAR(args))) == NA_INTEGER || units < 0)	\
	error(_("invalid units"));					\
    if(units == 1)  GCheckState(dd); \
    args = CDR(args);							\
									\
    if (isNull(CAR(args)))						\
	cex = gpptr(dd)->cex;						\
    else if (!R_FINITE((cex = asReal(CAR(args)))) || cex <= 0.0)	\
	error(_("invalid '%s' value"), "cex");				\
    args = CDR(args);							\
    PROTECT(font = FixupFont(CAR(args), NA_INTEGER)); args = CDR(args); \
    PROTECT(vfont = FixupVFont(CAR(args))); args = CDR(args);		\
    GSavePars(dd);							\
    ProcessInlinePars(args, dd);					\
									\
    /* 'vfont' trumps inline 'family' */				\
    if (!isNull(vfont) && !isExpression(str)) {				\
	strncpy(gpptr(dd)->family, "Her ", 201);			\
	gpptr(dd)->family[3] = (char)INTEGER(vfont)[0];			\
	gpptr(dd)->font = INTEGER(vfont)[1];				\
    } else gpptr(dd)->font = INTEGER(font)[0];				\
									\
    n = LENGTH(str);							\
    PROTECT(ans = allocVector(REALSXP, n));				\
    cexsave = gpptr(dd)->cex;						\
    gpptr(dd)->cex = cex * gpptr(dd)->cexbase;				\
    for (i = 0; i < n; i++)						\
	if (isExpression(str))						\
	    REAL(ans)[i] = GExpression ## KIND(VECTOR_ELT(str, i),	\
					     GMapUnits(units), dd);	\
	else {								\
	    ch = STRING_ELT(str, i);					\
	    REAL(ans)[i] = (ch == NA_STRING) ? 0.0 :			\
		GStr ## KIND(CHAR(ch), getCharCE(ch), GMapUnits(units), dd);		\
	}								\
    gpptr(dd)->cex = cexsave;						\
    GRestorePars(dd);							\
    UNPROTECT(4);							\
    return ans;								\
}

SEXP C_strHeight(SEXP args)
DO_STR_DIM(Height)

SEXP C_strWidth (SEXP args)
DO_STR_DIM(Width)

#undef DO_STR_DIM


static int *dnd_lptr;
static int *dnd_rptr;
static double *dnd_hght;
static double *dnd_xpos;
static double dnd_hang;
static double dnd_offset;

static void drawdend(int node, double *x, double *y, SEXP dnd_llabels,
		     pGEDevDesc dd)
{
/* Recursive function for 'hclust' dendrogram drawing:
 * Do left + Do right + Do myself
 * "do" : 1) label leafs (if there are) and __
 *        2) find coordinates to draw the  | |
 *        3) return (*x,*y) of "my anchor"
 */
    double xl, xr, yl, yr;
    double xx[4], yy[4];
    int k;

    *y = dnd_hght[node-1];
    /* left part  */
    k = dnd_lptr[node-1];
    if (k > 0) drawdend(k, &xl, &yl, dnd_llabels, dd);
    else {
	xl = dnd_xpos[-k-1];
	yl = (dnd_hang >= 0) ? *y - dnd_hang : 0;
	if(STRING_ELT(dnd_llabels, -k-1) != NA_STRING)
	    GText(xl, yl-dnd_offset, USER,
		  CHAR(STRING_ELT(dnd_llabels, -k-1)),
		  getCharCE(STRING_ELT(dnd_llabels, -k-1)),
		  1.0, 0.3, 90.0, dd);
    }
    /* right part */
    k = dnd_rptr[node-1];
    if (k > 0) drawdend(k, &xr, &yr, dnd_llabels, dd);
    else {
	xr = dnd_xpos[-k-1];
	yr = (dnd_hang >= 0) ? *y - dnd_hang : 0;
	if(STRING_ELT(dnd_llabels, -k-1) != NA_STRING)
	    GText(xr, yr-dnd_offset, USER,
		  CHAR(STRING_ELT(dnd_llabels, -k-1)),
		  getCharCE(STRING_ELT(dnd_llabels, -k-1)),
		  1.0, 0.3, 90.0, dd);
    }
    xx[0] = xl; yy[0] = yl;
    xx[1] = xl; yy[1] = *y;
    xx[2] = xr; yy[2] = *y;
    xx[3] = xr; yy[3] = yr;
    GPolyline(4, xx, yy, USER, dd);
    *x = 0.5 * (xl + xr);
}


SEXP C_dend(SEXP args)
{
    double x, y;
    int n;

    SEXP dnd_llabels, xpos;
    pGEDevDesc dd;

    dd = GEcurrentDevice();
    GCheckState(dd);

    args = CDR(args);
    if (length(args) < 6)
	error(_("too few arguments"));

    /* n */
    n = asInteger(CAR(args));
    if (n == NA_INTEGER || n < 2)
	goto badargs;
    args = CDR(args);

    /* merge */
    if (TYPEOF(CAR(args)) != INTSXP || length(CAR(args)) != 2*n)
	goto badargs;
    dnd_lptr = &(INTEGER(CAR(args))[0]);
    dnd_rptr = &(INTEGER(CAR(args))[n]);
    args = CDR(args);

    /* height */
    if (TYPEOF(CAR(args)) != REALSXP || length(CAR(args)) != n)
	goto badargs;
    dnd_hght = REAL(CAR(args));
    args = CDR(args);

    /* ord = order(x$order) */
    if (length(CAR(args)) != n+1)
	goto badargs;
    PROTECT(xpos = coerceVector(CAR(args), REALSXP));
    dnd_xpos = REAL(xpos);
    args = CDR(args);

    /* hang */
    dnd_hang = asReal(CAR(args));
    if (!R_FINITE(dnd_hang))
	goto badargs;
    dnd_hang = dnd_hang * (dnd_hght[n-1] - dnd_hght[0]);
    args = CDR(args);

    /* labels */
    if (TYPEOF(CAR(args)) != STRSXP || length(CAR(args)) != n+1)
	goto badargs;
    dnd_llabels = CAR(args);
    args = CDR(args);

    GSavePars(dd);
    ProcessInlinePars(args, dd);
    gpptr(dd)->cex = gpptr(dd)->cexbase * gpptr(dd)->cex;
    dnd_offset = GConvertYUnits(GStrWidth("m", CE_ANY, INCHES, dd), INCHES,
				USER, dd);

    /* override par("xpd") and force clipping to figure region
       NOTE: don't override to _reduce_ clipping region */
    if (gpptr(dd)->xpd < 1)
	gpptr(dd)->xpd = 1;

    GMode(1, dd);
    drawdend(n, &x, &y, dnd_llabels, dd);
    GMode(0, dd);
    GRestorePars(dd);
    UNPROTECT(1);
    return R_NilValue;

  badargs:
    error(_("invalid dendrogram input"));
    return R_NilValue;/* never used; to keep -Wall happy */
}

SEXP C_dendwindow(SEXP args)
{
    int i, imax, n;
    double pin, *ll, tmp, yval, *y, ymin, ymax, yrange, m;
    SEXP merge, height, llabels, str;
    const void *vmax;
    pGEDevDesc dd;

    dd = GEcurrentDevice();
    GCheckState(dd);
    args = CDR(args);
    if (length(args) < 5)
	error(_("too few arguments"));
    n = asInteger(CAR(args));
    if (n == NA_INTEGER || n < 2)
	goto badargs;
    args = CDR(args);
    if (TYPEOF(CAR(args)) != INTSXP || length(CAR(args)) != 2 * n)
	goto badargs;
    merge = CAR(args);

    args = CDR(args);
    if (TYPEOF(CAR(args)) != REALSXP || length(CAR(args)) != n)
	goto badargs;
    height = CAR(args);

    args = CDR(args);
    dnd_hang = asReal(CAR(args));
    if (!R_FINITE(dnd_hang))
	goto badargs;

    args = CDR(args);
    if (TYPEOF(CAR(args)) != STRSXP || length(CAR(args)) != n + 1)
	goto badargs;
    llabels = CAR(args);

    args = CDR(args);
    GSavePars(dd);
    ProcessInlinePars(args, dd);
    gpptr(dd)->cex = gpptr(dd)->cexbase * gpptr(dd)->cex;
    dnd_offset = GStrWidth("m", CE_ANY, INCHES, dd);
    vmax = vmaxget();
    /* n is the number of merges, so the points are labelled 1 ... n+1 */
    y =  (double*)R_alloc(n+1, sizeof(double));
    ll = (double*)R_alloc(n+1, sizeof(double));
    dnd_lptr = &(INTEGER(merge)[0]);
    dnd_rptr = &(INTEGER(merge)[n]);
    ymax = ymin = REAL(height)[0];
    for (i = 1; i < n; i++) {
	m = REAL(height)[i];
	if (m > ymax)
	    ymax = m;
	else if (m < ymin)
	    ymin = m;
    }
    pin = gpptr(dd)->pin[1];
    for (i = 0; i <= n; i++) {
	str = STRING_ELT(llabels, i);
	ll[i] = (str == NA_STRING) ? 0.0 :
	    GStrWidth(CHAR(str), getCharCE(str), INCHES, dd) + dnd_offset;
    }

    imax = -1; yval = -DBL_MAX;
    if (dnd_hang >= 0) {
	ymin = ymax - (1 + dnd_hang) * (ymax - ymin);
	yrange = ymax - ymin;
	/* determine leaf heights */
	for (i = 0; i < n; i++) {
	    if (dnd_lptr[i] < 0)
		y[-dnd_lptr[i] - 1] = REAL(height)[i];
	    if (dnd_rptr[i] < 0)
		y[-dnd_rptr[i] - 1] = REAL(height)[i];
	}
	/* determine the most extreme label depth */
	/* assuming that we are using the full plot */
	/* window for the tree itself */
	for (i = 0; i <= n; i++) {
	    tmp = ((ymax - y[i]) / yrange) * pin + ll[i];
	    if (tmp > yval) {
		yval = tmp;
		imax = i;
	    }
	}
    }
    else {
	yrange = ymax;
	for (i = 0; i <= n; i++) {
	    tmp = pin + ll[i];
	    if (tmp > yval) {
		yval = tmp;
		imax = i;
	    }
	}
    }
    /* now determine how much to scale */
    ymin = ymax - (pin/(pin - ll[imax])) * yrange;
    GScale(1.0, n+1.0, 1 /* x */, dd);
    GScale(ymin, ymax, 2 /* y */, dd);
    GMapWin2Fig(dd);
    GRestorePars(dd);
    vmaxset(vmax);
    return R_NilValue;
  badargs:
    error(_("invalid dendrogram input"));
    return R_NilValue;/* never used; to keep -Wall happy */
}


SEXP C_erase(SEXP args)
{
    SEXP col;
    pGEDevDesc dd = GEcurrentDevice();
    args = CDR(args);
    PROTECT(col = FixupCol(CAR(args), R_TRANWHITE));
    GSavePars(dd);
    GMode(1, dd);
    GRect(0.0, 0.0, 1.0, 1.0, NDC, INTEGER(col)[0], R_TRANWHITE, dd);
    GMode(0, dd);
    GRestorePars(dd);
    UNPROTECT(1);
    return R_NilValue;
}

/* symbols(..) in ../library/base/R/symbols.R  : */

/* utility just computing range() */
static Rboolean SymbolRange(double *x, int n, double *xmax, double *xmin)
{
    int i;
    *xmax = -DBL_MAX;
    *xmin =  DBL_MAX;
    for(i = 0; i < n; i++)
	if (R_FINITE(x[i])) {
	    if (*xmax < x[i]) *xmax = x[i];
	    if (*xmin > x[i]) *xmin = x[i];
	}
    return(*xmax >= *xmin && *xmin >= 0);
}

static void CheckSymbolPar(SEXP p, int *nr, int *nc)
{
    SEXP dim = getAttrib(p, R_DimSymbol);
    switch(length(dim)) {
    case 0:
	*nr = LENGTH(p);
	*nc = 1;
	break;
    case 1:
	*nr = INTEGER(dim)[0];
	*nc = 1;
	break;
    case 2:
	*nr = INTEGER(dim)[0];
	*nc = INTEGER(dim)[1];
	break;
    default:
	*nr = 0;
	*nc = 0;
    }
    if (*nr == 0 || *nc == 0)
	error(_("invalid symbol parameter vector"));
}

/* Internal  symbols(x, y, type, data, inches, bg, fg, ...) */
SEXP C_symbols(SEXP args)
{
    SEXP x, y, p, fg, bg;
    int i, j, nr, nc, nbg, nfg, type;
    double pmax, pmin, inches, rx, ry;
    double xx, yy, p0, p1, p2, p3, p4;
    double *pp, *xp, *yp;
    const void *vmax;

    pGEDevDesc dd = GEcurrentDevice();
    GCheckState(dd);
    args = CDR(args);

    if (length(args) < 7)
	error(_("too few arguments"));

    PROTECT(x = coerceVector(CAR(args), REALSXP)); args = CDR(args);
    PROTECT(y = coerceVector(CAR(args), REALSXP)); args = CDR(args);
    if (!isNumeric(x) || !isNumeric(y) || length(x) <= 0 || LENGTH(x) <= 0)
	error(_("invalid symbol coordinates"));

    type = asInteger(CAR(args)); args = CDR(args);

    /* data: */
    p = PROTECT(coerceVector(CAR(args), REALSXP)); args = CDR(args);
    CheckSymbolPar(p, &nr, &nc);
    if (LENGTH(x) != nr || LENGTH(y) != nr)
	error(_("x/y/parameter length mismatch"));

    inches = asReal(CAR(args)); args = CDR(args);
    if (!R_FINITE(inches) || inches < 0)
	inches = 0;

    PROTECT(bg = FixupCol(CAR(args), R_TRANWHITE)); args = CDR(args);
    nbg = LENGTH(bg);

    PROTECT(fg = FixupCol(CAR(args), R_TRANWHITE)); args = CDR(args);
    nfg = LENGTH(fg);

    GSavePars(dd);
    ProcessInlinePars(args, dd);

    GMode(1, dd);
    switch (type) {
    case 1: /* circles */
	if (nc != 1)
	    error(_("invalid circles data"));
	if (!SymbolRange(REAL(p), nr, &pmax, &pmin))
	    error(_("invalid symbol parameter"));
	for (i = 0; i < nr; i++) {
	    if (R_FINITE(REAL(x)[i]) && R_FINITE(REAL(y)[i]) &&
		R_FINITE(REAL(p)[i])) {
		rx = REAL(p)[i];
		/* For GCircle the radius is always in INCHES */
		if (inches > 0)
		    rx *= inches / pmax;
		else
		    rx = GConvertXUnits(rx, USER, INCHES, dd);
		/* GCircle sets radius zero to one pixel, but does
		   not change very small non-zero radii */
		GCircle(REAL(x)[i], REAL(y)[i],	USER, rx,
			INTEGER(bg)[i % nbg], INTEGER(fg)[i % nfg],	dd);
	    }
	}
	break;
    case 2: /* squares */
	if(nc != 1)
	    error(_("invalid squares data"));
	if(!SymbolRange(REAL(p), nr, &pmax, &pmin))
	    error(_("invalid symbol parameter"));
	for (i = 0; i < nr; i++) {
	    if (R_FINITE(REAL(x)[i]) && R_FINITE(REAL(y)[i]) &&
		R_FINITE(REAL(p)[i])) {
		p0 = REAL(p)[i];
		xx = REAL(x)[i];
		yy = REAL(y)[i];
		GConvert(&xx, &yy, USER, DEVICE, dd);
		if (inches > 0) {
		    p0 *= inches / pmax;
		    rx = GConvertXUnits(0.5 * p0, INCHES, DEVICE, dd);
		}
		else {
		    rx = GConvertXUnits(0.5 * p0, USER, DEVICE, dd);
		}
		/* FIXME: should this skip 0-sized symbols? */
		GRect(xx - rx, yy - rx, xx + rx, yy + rx, DEVICE,
		      INTEGER(bg)[i % nbg], INTEGER(fg)[i % nfg], dd);
	    }
	}
	break;
    case 3: /* rectangles */
	if (nc != 2)
	    error(_("invalid rectangles data (need 2 columns)"));
	if (!SymbolRange(REAL(p), 2 * nr, &pmax, &pmin))
	    error(_("invalid symbol parameter"));
	for (i = 0; i < nr; i++) {
	    if (R_FINITE(REAL(x)[i]) && R_FINITE(REAL(y)[i]) &&
		R_FINITE(REAL(p)[i]) && R_FINITE(REAL(p)[i+nr])) {
		xx = REAL(x)[i];
		yy = REAL(y)[i];
		GConvert(&xx, &yy, USER, DEVICE, dd);
		p0 = REAL(p)[i];
		p1 = REAL(p)[i+nr];
		if (inches > 0) {
		    p0 *= inches / pmax;
		    p1 *= inches / pmax;
		    rx = GConvertXUnits(0.5 * p0, INCHES, DEVICE, dd);
		    ry = GConvertYUnits(0.5 * p1, INCHES, DEVICE, dd);
		}
		else {
		    rx = GConvertXUnits(0.5 * p0, USER, DEVICE, dd);
		    ry = GConvertYUnits(0.5 * p1, USER, DEVICE, dd);
		}
		/* FIXME: should this skip 0-sized symbols? */
		GRect(xx - rx, yy - ry, xx + rx, yy + ry, DEVICE,
		      INTEGER(bg)[i % nbg], INTEGER(fg)[i % nfg], dd);

	    }
	}
	break;
    case 4: /* stars */
	if (nc < 3)
	    error(_("invalid stars data"));
	if (!SymbolRange(REAL(p), nc * nr, &pmax, &pmin))
	    error(_("invalid symbol parameter"));
	vmax = vmaxget();
	pp = (double*)R_alloc(nc, sizeof(double));
	xp = (double*)R_alloc(nc, sizeof(double));
	yp = (double*)R_alloc(nc, sizeof(double));
	p1 = 2.0 * M_PI / nc;
	for (i = 0; i < nr; i++) {
	    xx = REAL(x)[i];
	    yy = REAL(y)[i];
	    if (R_FINITE(xx) && R_FINITE(yy)) {
		GConvert(&xx, &yy, USER, NDC, dd);
		if (inches > 0) {
		    for(j = 0; j < nc; j++) {
			p0 = REAL(p)[i + j * nr];
			if (!R_FINITE(p0)) p0 = 0;
			pp[j] = (p0 / pmax) * inches;
		    }
		}
		else {
		    for(j = 0; j < nc; j++) {
			p0 = REAL(p)[i + j * nr];
			if (!R_FINITE(p0)) p0 = 0;
			pp[j] =	 GConvertXUnits(p0, USER, INCHES, dd);
		    }
		}
		/* FIXME: should this skip 0-sized symbols? */
		for(j = 0; j < nc; j++) {
		    xp[j] = GConvertXUnits(pp[j] * cos(j * p1),
					   INCHES, NDC, dd) + xx;
		    yp[j] = GConvertYUnits(pp[j] * sin(j * p1),
					   INCHES, NDC, dd) + yy;
		}
		GPolygon(nc, xp, yp, NDC,
			 INTEGER(bg)[i % nbg], INTEGER(fg)[i % nfg], dd);
	    }
	}
	vmaxset(vmax);
	break;
    case 5: /* thermometers */
	if (nc != 3 && nc != 4)
	    error(_("invalid thermometers data (need 3 or 4 columns)"));
	SymbolRange(REAL(p)+2*nr/* <-- pointer arith*/, nr, &pmax, &pmin);
	if (pmax < pmin)
	    error(_("invalid 'thermometers[, %s]'"),
		      (nc == 4)? "3:4" : "3");
	if (pmin < 0. || pmax > 1.) /* S-PLUS has an error here */
	    warning(_("'thermometers[, %s]' not in [0,1] -- may look funny"),
		    (nc == 4)? "3:4" : "3");
	if (!SymbolRange(REAL(p), 2 * nr, &pmax, &pmin))
	    error(_("invalid 'thermometers[, 1:2]'"));
	for (i = 0; i < nr; i++) {
	    xx = REAL(x)[i];
	    yy = REAL(y)[i];
	    if (R_FINITE(xx) && R_FINITE(yy)) {
		p0 = REAL(p)[i];
		p1 = REAL(p)[i + nr];
		p2 = REAL(p)[i + 2 * nr];
		p3 = (nc == 4)? REAL(p)[i + 3 * nr] : 0.;
		if (R_FINITE(p0) && R_FINITE(p1) &&
		    R_FINITE(p2) && R_FINITE(p3)) {
		    if (p2 < 0) p2 = 0; else if (p2 > 1) p2 = 1;
		    if (p3 < 0) p3 = 0; else if (p3 > 1) p3 = 1;
		    GConvert(&xx, &yy, USER, NDC, dd);
		    if (inches > 0) {
			p0 *= inches / pmax;
			p1 *= inches / pmax;
			rx = GConvertXUnits(0.5 * p0, INCHES, NDC, dd);
			ry = GConvertYUnits(0.5 * p1, INCHES, NDC, dd);
		    }
		    else {
			rx = GConvertXUnits(0.5 * p0, USER, NDC, dd);
			ry = GConvertYUnits(0.5 * p1, USER, NDC, dd);
		    }
		    GRect(xx - rx, yy - ry, xx + rx, yy + ry, NDC,
			  INTEGER(bg)[i % nbg], INTEGER(fg)[i % nfg], dd);
		    GRect(xx - rx,  yy - (1 - 2 * p2) * ry,
			  xx + rx,  yy - (1 - 2 * p3) * ry,
			  NDC,
			  INTEGER(fg)[i % nfg], INTEGER(fg)[i % nfg], dd);
		    GLine(xx - rx, yy, xx - 1.5 * rx, yy, NDC, dd);
		    GLine(xx + rx, yy, xx + 1.5 * rx, yy, NDC, dd);

		}
	    }
	}
	break;
    case 6: /* boxplots (wid, hei, loWhsk, upWhsk, medProp) */
	if (nc != 5)
	    error(_("invalid 'boxplots' data (need 5 columns)"));
	pmax = -DBL_MAX;
	pmin =	DBL_MAX;
	for(i = 0; i < nr; i++) {
	    p4 = REAL(p)[i + 4 * nr];	/* median proport. in [0,1] */
	    if (pmax < p4) pmax = p4;
	    if (pmin > p4) pmin = p4;
	}
	if (pmin < 0. || pmax > 1.) /* S-PLUS has an error here */
	    warning(_("'boxplots[, 5]' outside [0,1] -- may look funny"));
	if (!SymbolRange(REAL(p), 4 * nr, &pmax, &pmin))
	    error(_("invalid 'boxplots[, 1:4]'"));
	for (i = 0; i < nr; i++) {
	    xx = REAL(x)[i];
	    yy = REAL(y)[i];
	    if (R_FINITE(xx) && R_FINITE(yy)) {
		p0 = REAL(p)[i];	/* width */
		p1 = REAL(p)[i + nr];	/* height */
		p2 = REAL(p)[i + 2 * nr];/* lower whisker */
		p3 = REAL(p)[i + 3 * nr];/* upper whisker */
		p4 = REAL(p)[i + 4 * nr];/* median proport. in [0,1] */
		if (R_FINITE(p0) && R_FINITE(p1) &&
		    R_FINITE(p2) && R_FINITE(p3) && R_FINITE(p4)) {
		    GConvert(&xx, &yy, USER, NDC, dd);
		    if (inches > 0) {
			p0 *= inches / pmax;
			p1 *= inches / pmax;
			p2 *= inches / pmax;
			p3 *= inches / pmax;
			p0 = GConvertXUnits(p0, INCHES, NDC, dd);
			p1 = GConvertYUnits(p1, INCHES, NDC, dd);
			p2 = GConvertYUnits(p2, INCHES, NDC, dd);
			p3 = GConvertYUnits(p3, INCHES, NDC, dd);
		    }
		    else {
			p0 = GConvertXUnits(p0, USER, NDC, dd);
			p1 = GConvertYUnits(p1, USER, NDC, dd);
			p2 = GConvertYUnits(p2, USER, NDC, dd);
			p3 = GConvertYUnits(p3, USER, NDC, dd);
		    }
		    rx = 0.5 * p0;
		    ry = 0.5 * p1;
		    p4 = (1 - p4) * (yy - ry) + p4 * (yy + ry);
		    /* Box */
		    GRect(xx - rx, yy - ry, xx + rx, yy + ry, NDC,
			  INTEGER(bg)[i % nbg], INTEGER(fg)[i % nfg], dd);
		    /* Median */
		    GLine(xx - rx, p4, xx + rx, p4, NDC, dd);
		    /* Lower Whisker */
		    GLine(xx, yy - ry, xx, yy - ry - p2, NDC, dd);
		    /* Upper Whisker */
		    GLine(xx, yy + ry, xx, yy + ry + p3, NDC, dd);
		}
	    }
	}
	break;
    default:
	error(_("invalid symbol type"));
    }
    GMode(0, dd);
    GRestorePars(dd);
    UNPROTECT(5);
    return R_NilValue;
}

SEXP C_xspline(SEXP args)
{
    SEXP sx, sy, ss, col, border, res, ans = R_NilValue;
    int i, nx;
    int ncol, nborder;
    double *x, *y;
    Rboolean open, repEnds, draw;
    double *xx;
    double *yy;
    const void *vmaxsave;
    R_GE_gcontext gc;

    pGEDevDesc dd = GEcurrentDevice();

    GCheckState(dd);
    args = CDR(args);

    if (length(args) < 6) error(_("too few arguments"));
    /* (x,y) is checked in R via xy.coords() ; no need here : */
    sx = SETCAR(args, coerceVector(CAR(args), REALSXP));  args = CDR(args);
    sy = SETCAR(args, coerceVector(CAR(args), REALSXP));  args = CDR(args);
    nx = LENGTH(sx);
    ss = SETCAR(args, coerceVector(CAR(args), REALSXP));  args = CDR(args);
    open = asLogical(CAR(args)); args = CDR(args);
    repEnds = asLogical(CAR(args)); args = CDR(args);
    draw = asLogical(CAR(args)); args = CDR(args);

    PROTECT(col = FixupCol(CAR(args), R_TRANWHITE));	args = CDR(args);
    ncol = LENGTH(col);
    if(ncol < 1)
	error(_("incorrect length for '%s' argument"), "col");
    if(ncol > 1)
	warning(_("incorrect length for '%s' argument"), "col");

    PROTECT(border = FixupCol(CAR(args), gpptr(dd)->fg)); args = CDR(args);
    nborder = LENGTH(border);
    if(nborder < 1)
	error(_("incorrect length for '%s' argument"), "border");
    if(nborder > 1)
	warning(_("incorrect length for '%s' argument"), "border");

    GSavePars(dd);
    ProcessInlinePars(args, dd);
    /* Paul 2008-12-05
     * Convert GP to gcontext AFTER ProcessInlinePars
     */
    gcontextFromGP(&gc, dd);

    GMode(1, dd);

    x = REAL(sx);
    y = REAL(sy);
    vmaxsave = vmaxget();
    xx = (double *) R_alloc(nx, sizeof(double));
    yy = (double *) R_alloc(nx, sizeof(double));
    if (!xx || !yy)
	error("unable to allocate memory (in xspline)");
    for (i = 0; i < nx; i++) {
	xx[i] = x[i];
	yy[i] = y[i];
	GConvert(&(xx[i]), &(yy[i]), USER, DEVICE, dd);
    }
    GClip(dd);
    gc.col = INTEGER(border)[0];
    gc.fill = INTEGER(col)[0];
    res = GEXspline(nx, xx, yy, REAL(ss), open, repEnds, draw, &gc, dd);
    vmaxset(vmaxsave);
    UNPROTECT(2);

    if(!draw) {
	SEXP nm, tmpx, tmpy;
	double *xx, *yy, *x0, *y0;
	PROTECT(ans = res);
	PROTECT(nm = allocVector(STRSXP, 2));
	SET_STRING_ELT(nm, 0, mkChar("x"));
	SET_STRING_ELT(nm, 1, mkChar("y"));
	setAttrib(ans, R_NamesSymbol, nm);
	nx = LENGTH(VECTOR_ELT(ans, 0));
	x0 = REAL(VECTOR_ELT(ans, 0));
	y0 = REAL(VECTOR_ELT(ans, 1));
	PROTECT(tmpx = allocVector(REALSXP, nx));
	PROTECT(tmpy = allocVector(REALSXP, nx));
	xx = REAL(tmpx);
	yy = REAL(tmpy);
	for (i = 0; i < nx; i++) {
	    xx[i] = x0[i];
	    yy[i] = y0[i];
	    GConvert(&(xx[i]), &(yy[i]), DEVICE, USER, dd);
	}
	SET_VECTOR_ELT(ans, 0, tmpx);
	SET_VECTOR_ELT(ans, 1, tmpy);
	UNPROTECT(4);
    }

    GMode(0, dd);
    GRestorePars(dd);
    return ans;
}

/* clip(x1, x2, y1, y2) */
SEXP C_clip(SEXP args)
{
    SEXP ans = R_NilValue;
    double x1, x2, y1, y2;
    pGEDevDesc dd = GEcurrentDevice();

    args = CDR(args);
    x1 = asReal(CAR(args));
    if(!R_FINITE(x1)) error("invalid '%s' argument", "x1");
    args = CDR(args);
    x2 = asReal(CAR(args));
    if(!R_FINITE(x2)) error("invalid '%s' argument", "x2");
    args = CDR(args);
    y1 = asReal(CAR(args));
    if(!R_FINITE(y1)) error("invalid '%s' argument", "y1");
    args = CDR(args);
    y2 = asReal(CAR(args));
    if(!R_FINITE(y2)) error("invalid '%s' argument", "y2");

    GConvert(&x1, &y1, USER, DEVICE, dd);
    GConvert(&x2, &y2, USER, DEVICE, dd);
    GESetClip(x1, y1, x2, y2, dd);
    /* avoid GClip resetting this */
    gpptr(dd)->oldxpd = gpptr(dd)->xpd;
    return ans;
}

/* convert[XY](x, from to) */
SEXP C_convertX(SEXP args)
{
    SEXP ans = R_NilValue, x;
    int from, to, i, n;
    double *rx;
    pGEDevDesc gdd = GEcurrentDevice();

    args = CDR(args);
    x = CAR(args);
    if (TYPEOF(x) != REALSXP) error(_("invalid '%s' argument"), "x");
    n = LENGTH(x);
    from = asInteger(CADR(args));
    if (from == NA_INTEGER || from <= 0 || from > 17 )
	error(_("invalid '%s' argument"), "from");
    to = asInteger(CADDR(args));
    if (to == NA_INTEGER || to <= 0 || to > 17 )
	error(_("invalid '%s' argument"), "to");
    from--; to--;

    PROTECT(ans = duplicate(x));
    rx = REAL(ans);
    for (i = 0; i < n; i++) rx[i] = GConvertX(rx[i], from, to, gdd);
    UNPROTECT(1);

    return ans;
}

SEXP C_convertY(SEXP args)
{
    SEXP ans = R_NilValue, x;
    int from, to, i, n;
    double *rx;
    pGEDevDesc gdd = GEcurrentDevice();

    args = CDR(args);
    x = CAR(args);
    if (TYPEOF(x) != REALSXP) error(_("invalid '%s' argument"), "x");
    n = LENGTH(x);
    from = asInteger(CADR(args));
    if (from == NA_INTEGER || from <= 0 || from > 17 )
	error(_("invalid '%s' argument"), "from");
    to = asInteger(CADDR(args));
    if (to == NA_INTEGER || to <= 0 || to > 17 )
	error(_("invalid '%s' argument"), "to");
    from--; to--;

    PROTECT(ans = duplicate(x));
    rx = REAL(ans);
    for (i = 0; i < n; i++) rx[i] = GConvertY(rx[i], from, to, gdd);
    UNPROTECT(1);

    return ans;
}
