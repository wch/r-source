/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2001  Robert Gentleman, Ross Ihaka and the
 *                            R Development Core Team
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
#include <Rmath.h>
#include <Graphics.h>
#include <Rdevices.h>
#include <Print.h>

#ifndef HAVE_HYPOT
# define hypot pythag
#endif


void NewFrameConfirm(void)
{
    unsigned char buf[16];
    R_ReadConsole("Hit <Return> to see next plot: ", buf, 16, 0);
}

	/* Remember: +1 and/or -1 because C arrays are */
	/* zero-based and R-vectors are one-based. */

#define checkArity_length					\
    checkArity(op, args);					\
    if(!LENGTH(CAR(args)))					\
	errorcall(call, "argument must have positive length")


SEXP do_devcontrol(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    inhibitDisplayList(CurrentDevice());
    return R_NilValue;
}

SEXP do_devcopy(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity_length;
    copyDisplayList(INTEGER(CAR(args))[0] - 1);
    return R_NilValue;
}

SEXP do_devcur(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP cd = allocVector(INTSXP, 1);
    checkArity(op, args);
    INTEGER(cd)[0] = curDevice() + 1;
    return cd;
}

SEXP do_devnext(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP nd = allocVector(INTSXP, 1);
    checkArity_length;
    INTEGER(nd)[0] = nextDevice(INTEGER(CAR(args))[0] - 1) + 1;
    return nd;
}

SEXP do_devprev(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pd = allocVector(INTSXP, 1);
    checkArity_length;
    INTEGER(pd)[0] = prevDevice(INTEGER(CAR(args))[0] - 1) + 1;
    return pd;
}

SEXP do_devset(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int devNum = INTEGER(CAR(args))[0] - 1;
    SEXP sd = allocVector(INTSXP, 1);
    checkArity(op, args);
    INTEGER(sd)[0] = selectDevice(devNum) + 1;
    return sd;
}

SEXP do_devoff(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity_length;
    killDevice(INTEGER(CAR(args))[0] - 1);
    return R_NilValue;
}


/*  P A R A M E T E R	 U T I L I T I E S  */


/* ProcessInLinePars handles inline par specifications in graphics functions.
 * It does this by calling Specify2() from ./par.c */

void ProcessInlinePars(SEXP s, DevDesc *dd)
{
    if (isList(s)) {
	while (s != R_NilValue) {
	    if (isList(CAR(s)))
		ProcessInlinePars(CAR(s), dd);
	    else if (TAG(s) != R_NilValue)
		Specify2(CHAR(PRINTNAME(TAG(s))), CAR(s), dd);
	    s = CDR(s);
	}
    }
}

SEXP FixupPch(SEXP pch, int dflt)
{
    int i, n;
    SEXP ans = R_NilValue;/* -Wall*/

    n = length(pch);
    if (n == 0) {
	ans = allocVector(INTSXP, 1);
	INTEGER(ans)[0] = dflt;
    }
    else if (isList(pch)) {
	ans = allocVector(INTSXP, n);
	for (i = 0; pch != R_NilValue;  pch = CDR(pch))
	    INTEGER(ans)[i++] = asInteger(CAR(pch));
    }
    else if (isInteger(pch)) {
	ans = allocVector(INTSXP, n);
	for (i = 0; i < n; i++)
	    INTEGER(ans)[i] = INTEGER(pch)[i];
    }
    else if (isReal(pch)) {
	ans = allocVector(INTSXP, n);
	for (i = 0; i < n; i++)
	    INTEGER(ans)[i] = R_FINITE(REAL(pch)[i]) ?
		REAL(pch)[i] : NA_INTEGER;
    }
    else if (isString(pch)) {
	ans = allocVector(INTSXP, n);
	for (i = 0; i < n; i++)
	    INTEGER(ans)[i] = CHAR(STRING_ELT(pch, i))[0];
    }
    else error("invalid plotting symbol");
    for (i = 0; i < n; i++) {
	if (INTEGER(ans)[i] < 0 && INTEGER(ans)[i] != NA_INTEGER)
	    INTEGER(ans)[i] = dflt;
    }
    return ans;
}

SEXP FixupLty(SEXP lty, int dflt)
{
    int i, n;
    SEXP ans;
    n = length(lty);
    if (n == 0) {
	ans = allocVector(INTSXP, 1);
	INTEGER(ans)[0] = dflt;
    }
    else {
	ans = allocVector(INTSXP, n);
	for (i = 0; i < n; i++)
	    INTEGER(ans)[i] = LTYpar(lty, i);
    }
    return ans;
}

SEXP FixupLwd(SEXP lwd, double dflt)
{
    int i, n;
    double w;
    SEXP ans = NULL;

    n = length(lwd);
    if (n == 0) {
        ans = allocVector(REALSXP, 1);
        REAL(ans)[0] = dflt;
    }
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

SEXP FixupFont(SEXP font, int dflt)
{
    int i, k, n;
    SEXP ans = R_NilValue;/* -Wall*/
    n = length(font);
    if (n == 0) {
	ans = allocVector(INTSXP, 1);
	INTEGER(ans)[0] = dflt;
    }
    else if (isInteger(font) || isLogical(font)) {
	ans = allocVector(INTSXP, n);
	for (i = 0; i < n; i++) {
	    k = INTEGER(font)[i];
#ifndef Win32
	    if (k < 1 || k > 4) k = NA_INTEGER;
#else
	    if (k < 1 || k > 32) k = NA_INTEGER;
#endif
	    INTEGER(ans)[i] = k;
	}
    }
    else if (isReal(font)) {
	ans = allocVector(INTSXP, n);
	for (i = 0; i < n; i++) {
	    k = REAL(font)[i];
#ifndef Win32
	    if (k < 1 || k > 4) k = NA_INTEGER;
#else
	    if (k < 1 || k > 32) k = NA_INTEGER;
#endif
	    INTEGER(ans)[i] = k;
	}
    }
    else error("invalid font specification");
    return ans;
}

SEXP FixupCol(SEXP col, unsigned int dflt)
{
    int i, n;
    SEXP ans;
    n = length(col);
    if (length(col) == 0) {
	ans = allocVector(INTSXP, 1);
	INTEGER(ans)[0] = dflt;
    }
    else if (isList(col)) {
	ans = allocVector(INTSXP, n);
	for (i = 0; i < n; i++) {
	    INTEGER(ans)[i] = RGBpar(CAR(col), 0);
	    col = CDR(col);
	}
    }
    else {
	ans = allocVector(INTSXP, n);
	for (i = 0; i < n; i++)
	    INTEGER(ans)[i] = RGBpar(col, i);
    }
    return ans;
}

SEXP FixupCex(SEXP cex, double dflt)
{
    SEXP ans = R_NilValue;/* -Wall*/
    int i, n;
    double c;
    n = length(cex);
    if (length(cex) == 0) {
	ans = allocVector(REALSXP, 1);
	if (R_FINITE(dflt) && dflt > 0)
	    REAL(ans)[0] = dflt;
	else
	    REAL(ans)[0] = NA_REAL;
    }
    else if (isReal(cex)) {
	ans = allocVector(REALSXP, n);
	for (i = 0; i < n; i++) {
	    c = REAL(cex)[i];
	    if (R_FINITE(c) && c > 0)
		REAL(ans)[i] = c;
	    else
		REAL(ans)[i] = NA_REAL;
	}
    }
    else if (isInteger(cex) || isLogical(cex)) {
	ans = allocVector(REALSXP, n);
	for (i = 0; i < n; i++) {
	    c = INTEGER(cex)[i];
	    if (c == NA_INTEGER || c <= 0)
		c = NA_REAL;
	    REAL(ans)[i] = c;
	}
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
	    error("Invalid vfont value");
	typeface = INTEGER(vf)[0];
	if (typeface < 0 || typeface > 7)
	    error("Invalid vfont value [typeface]");
	/* For each of the typefaces {0..7}, there are several fontindices
	   available; how many depends on the typeface.
	   The possible combinations are "given" in ./g_fontdb.c
	   and also listed in help(Hershey).
	 */
	minindex = 1;
	switch (typeface) {
	case 0: /* serif */
	    maxindex = 7;	    break;
	case 1: /* sans serif */
	case 6: /* serif symbol */
	    maxindex = 4;	    break;
	case 2: /* script */
	    maxindex = 3;	    break;
	case 3: /* gothic english */
	case 4: /* gothic german */
	case 5: /* gothic italian */
	    maxindex = 1;	    break;
	case 7: /* sans serif symbol */
	    maxindex = 2;
	}
	fontindex = INTEGER(vf)[1];
	if (fontindex < minindex || fontindex > maxindex)
	    error("Invalid vfont value [fontindex]");
	ans = allocVector(INTSXP, 2);
	for (i=0; i<2; i++)
	    INTEGER(ans)[i] = INTEGER(vf)[i];
	UNPROTECT(1);
    }
    return ans;
}

/* GetTextArg() : extract from call and possibly set text arguments
 *  ("label", col=, cex=, font=, vfont=)
 *
 * Main purpose: Treat things like  title(main = list("This Title", font= 4))
 *
 * Called from  do_title()  [only, currently]
 */
static void
GetTextArg(SEXP call, SEXP spec, SEXP *ptxt,
	   int *pcol, double *pcex, int *pfont, SEXP *pvfont)
{
    int i, n, col, font;
    double cex;
    SEXP txt, vfont, nms;

    txt   = R_NilValue;
    vfont = R_NilValue;
    cex   = NA_REAL;
    col   = NA_INTEGER;
    font  = NA_INTEGER;
    PROTECT(txt);

    switch (TYPEOF(spec)) {
    case LANGSXP:
    case SYMSXP:
	UNPROTECT(1);
	PROTECT(txt = coerceVector(spec, EXPRSXP));
	break;
    case VECSXP:
	if (length(spec) == 0) {
	    *ptxt = R_NilValue;
	}
	else {
	    nms = getAttrib(spec, R_NamesSymbol);
	    n = length(nms);
	    for (i = 0; i < n; i++) {
		if (!strcmp(CHAR(STRING_ELT(nms, i)), "cex")) {
		    cex = asReal(VECTOR_ELT(spec, i));
		}
		else if (!strcmp(CHAR(STRING_ELT(nms, i)), "col")) {
		    col = asInteger(FixupCol(VECTOR_ELT(spec, i), NA_INTEGER));
		}
		else if (!strcmp(CHAR(STRING_ELT(nms, i)), "font")) {
		    font = asInteger(FixupFont(VECTOR_ELT(spec, i), NA_INTEGER));
		}
		else if (!strcmp(CHAR(STRING_ELT(nms, i)), "vfont")) {
		    vfont = FixupVFont(VECTOR_ELT(spec, i));
		}
		else if (!strcmp(CHAR(STRING_ELT(nms, i)), "")) {
		    txt = VECTOR_ELT(spec, i);
                    if (TYPEOF(txt) == LANGSXP) {
                        UNPROTECT(1);
                        PROTECT(txt = coerceVector(txt, EXPRSXP));
                    }
		    else if (!isExpression(txt)) {
			UNPROTECT(1);
			PROTECT(txt = coerceVector(txt, STRSXP));
		    }
		}
		else errorcall(call, "invalid graphics parameter");
	    }
	}
	break;
    case STRSXP:
    case EXPRSXP:
	txt = spec;
	break;
    default:
	txt = coerceVector(spec, STRSXP);
	break;
    }
    UNPROTECT(1);
    if (txt != R_NilValue) {
	*ptxt = txt;
	if (R_FINITE(cex))       *pcex   = cex;
	if (col != NA_INTEGER)   *pcol   = col;
	if (font != NA_INTEGER)  *pfont  = font;
	if (vfont != R_NilValue) *pvfont = vfont;
    }
}/* GetTextArg */


    /* GRAPHICS FUNCTION ENTRY POINTS */


SEXP do_plot_new(SEXP call, SEXP op, SEXP args, SEXP env)
{
    /* plot.new() - create a new plot "frame" */

    DevDesc *dd;

    checkArity(op, args);

    dd = GNewPlot(GRecording(call));

    dd->dp.xlog = dd->gp.xlog = FALSE;
    dd->dp.ylog = dd->gp.ylog = FALSE;

    GScale(0.0, 1.0, 1, dd);
    GScale(0.0, 1.0, 2, dd);
    GMapWin2Fig(dd);
    GSetState(1, dd);

    if (GRecording(call))
	recordGraphicOperation(op, args, dd);
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

SEXP do_plot_window(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP xlim, ylim, log;
    double asp, xmin, xmax, ymin, ymax;
    Rboolean logscale;
    char *p;
    SEXP originalArgs = args;
    DevDesc *dd = CurrentDevice();

    if (length(args) < 3)
	errorcall(call, "at least 3 arguments required");

    xlim = CAR(args);
    if (!isNumeric(xlim) || LENGTH(xlim) != 2)
	errorcall(call, "invalid xlim");
    args = CDR(args);

    ylim = CAR(args);
    if (!isNumeric(ylim) || LENGTH(ylim) != 2)
	errorcall(call, "invalid ylim");
    args = CDR(args);

    logscale = FALSE;
    log = CAR(args);
    if (!isString(log))
	errorcall(call, "\"log=\" specification must be character");
    p = CHAR(STRING_ELT(log, 0));
    while (*p) {
	switch (*p) {
	case 'x':
	    dd->dp.xlog = dd->gp.xlog = logscale = TRUE;
	    break;
	case 'y':
	    dd->dp.ylog = dd->gp.ylog = logscale = TRUE;
	    break;
	default:
	    errorcall(call,"invalid \"log=%s\" specification",p);
	}
	p++;
    }
    args = CDR(args);

    asp = (logscale) ? NA_REAL : asReal(CAR(args));;
    args = CDR(args);

    GSavePars(dd);
    RecordGraphicsCall(call);
    ProcessInlinePars(args, dd);

    if (isInteger(xlim)) {
	if (INTEGER(xlim)[0] == NA_INTEGER || INTEGER(xlim)[1] == NA_INTEGER)
	    errorcall(call, "NAs not allowed in xlim");
	xmin = INTEGER(xlim)[0];
	xmax = INTEGER(xlim)[1];
    }
    else {
	if (!R_FINITE(REAL(xlim)[0]) || !R_FINITE(REAL(xlim)[1]))
	    errorcall(call, "need finite xlim values");
	xmin = REAL(xlim)[0];
	xmax = REAL(xlim)[1];
    }
    if (isInteger(ylim)) {
	if (INTEGER(ylim)[0] == NA_INTEGER || INTEGER(ylim)[1] == NA_INTEGER)
	    errorcall(call, "NAs not allowed in ylim");
	ymin = INTEGER(ylim)[0];
	ymax = INTEGER(ylim)[1];
    }
    else {
	if (!R_FINITE(REAL(ylim)[0]) || !R_FINITE(REAL(ylim)[1]))
	    errorcall(call, "need finite ylim values");
	ymin = REAL(ylim)[0];
	ymax = REAL(ylim)[1];
    }
    if ((dd->dp.xlog && (xmin < 0 || xmax < 0)) ||
       (dd->dp.ylog && (ymin < 0 || ymax < 0)))
	    errorcall(call, "Logarithmic axis must have positive limits");

    if (R_FINITE(asp) && asp > 0) {
	double pin1, pin2, scale, xdelta, ydelta, xscale, yscale, xadd, yadd;
	pin1 = GConvertXUnits(1.0, NPC, INCHES, dd);
	pin2 = GConvertYUnits(1.0, NPC, INCHES, dd);
	xdelta = fabs(xmax - xmin) / asp;
	ydelta = fabs(ymax - ymin);
	xscale = pin1 / xdelta;
	yscale = pin2 / ydelta;
	scale = (xscale < yscale) ? xscale : yscale;
	xadd = .5 * (pin1 / scale - xdelta) * asp;
	yadd = .5 * (pin2 / scale - ydelta);
	GScale(xmin - xadd, xmax + xadd, 1, dd);
	GScale(ymin - yadd, ymax + yadd, 2, dd);
    }
    else {
	GScale(xmin, xmax, 1, dd);
	GScale(ymin, ymax, 2, dd);
    }
    GMapWin2Fig(dd);
    GRestorePars(dd);
    /* NOTE: the operation is only recorded if there was no "error" */
    if (GRecording(call))
	recordGraphicOperation(op, originalArgs, dd);
    return R_NilValue;
}

void GetAxisLimits(double left, double right, double *low, double *high)
{
/*	Called from do_axis()	such as
 *	GetAxisLimits(dd->gp.usr[0], dd->gp.usr[1], &low, &high)
 *
 *	Computes  *low < left, right < *high  (even if left=right)
 */
    double eps;
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
}


/* axis(side, at, labels, ...) */

SEXP labelformat(SEXP labels)
{
    /* format(labels): i.e. from numbers to strings */
    SEXP ans = R_NilValue;/* -Wall*/
    int i, n, w, d, e, wi, di, ei;
    char *strp;
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
	formatReal(REAL(labels), n, &w, &d, &e);
	PROTECT(ans = allocVector(STRSXP, n));
	for (i = 0; i < n; i++) {
	    strp = EncodeReal(REAL(labels)[i], 0, d, e);
	    SET_STRING_ELT(ans, i, mkChar(strp));
	}
	UNPROTECT(1);
	break;
    case CPLXSXP:
	formatComplex(COMPLEX(labels), n, &w, &d, &e, &wi, &di, &ei);
	PROTECT(ans = allocVector(STRSXP, n));
	for (i = 0; i < n; i++) {
	    strp = EncodeComplex(COMPLEX(labels)[i], 0, d, e, 0, di, ei);
	    SET_STRING_ELT(ans, i, mkChar(strp));
	}
	UNPROTECT(1);
	break;
    case STRSXP:
	formatString(STRING_PTR(labels), n, &w, 0);
	PROTECT(ans = allocVector(STRSXP, n));
	for (i = 0; i < n; i++) {
#ifdef OLD
	    strp = EncodeString(CHAR(STRING_ELT(labels, i)), 0, 0, Rprt_adj_left);
	    SET_STRING_ELT(ans, i, mkChar(strp));
#else
	    SET_STRING_ELT(ans, i, STRING_ELT(labels, i));
#endif
	}
	UNPROTECT(1);
	break;
    default:
	error("invalid type for axis labels");
    }
    return ans;
}

SEXP CreateAtVector(double *axp, double *usr, int nint, Rboolean log)
{
/*	Create an  'at = ...' vector for  axis(.) / do_axis,
 *	i.e., the vector of tick mark locations,
 *	when none has been specified (= default).
 *
 *	axp[0:2] = (x1, x2, nint), where x1..x2 are the extreme tick marks
 *
 *	The resulting REAL vector must have length >= 1, ideally >= 2
 */
    SEXP at = R_NilValue;/* -Wall*/
    double umin, umax, dn, rng, small;
    int i, n, ne;
    if (!log || axp[2] < 0) { /* ---- linear axis ---- Only use	axp[]  arg. */
	n = fabs(axp[2]) + 0.25;/* >= 0 */
	dn = imax2(1, n);
	rng = axp[1] - axp[0];
	small = fabs(rng)/(100.*dn);
	at = allocVector(REALSXP, n + 1);
	for (i = 0; i <= n; i++) {
	    REAL(at)[i] = axp[0] + (i / dn) * rng;
	    if (fabs(REAL(at)[i]) < small)
		REAL(at)[i] = 0;
	}
    }
    else { /* ------ log axis ----- */
	n = (axp[2] + 0.5);
	/* {xy}axp[2] for 'log': GLpretty() [../graphics.c] sets
	   n < 0: very small scale ==> linear axis, above, or
	   n = 1,2,3.  see switch() below */
	umin = usr[0];
	umax = usr[1];
	/* Debugging: When does the following happen... ? */
	if (umin > umax)
	    warning("CreateAtVector \"log\"(from axis()): "
		    "usr[0] = %g > %g = usr[1] !", umin, umax);
	dn = axp[0];
	if (dn < 1e-300)
	    warning("CreateAtVector \"log\"(from axis()): axp[0] = %g !", dn);

	/* You get the 3 cases below by
	 *  for (y in 1e-5*c(1,2,8))  plot(y, log = "y")
	 */
	switch(n) {
	case 1: /* large range:	1	 * 10^k */
	    i = floor(log10(axp[1])) - ceil(log10(axp[0])) + 0.25;
	    ne = i / nint + 1;
	    if (ne < 1)
		error("log - axis(), 'at' creation, _LARGE_ range: "
		      "ne = %d <= 0 !!\n"
		      "\t axp[0:1]=(%g,%g) ==> i = %d;	nint = %d",
		      ne, axp[0],axp[1], i, nint);
	    rng = pow(10., (double)ne);/* >= 10 */
	    n = 0;
	    while (dn < umax) {
		n++;
		dn *= rng;
	    }
	    if (!n)
		error("log - axis(), 'at' creation, _LARGE_ range: "
		      "illegal {xy}axp or par; nint=%d\n"
		      "	 axp[0:1]=(%g,%g), usr[0:1]=(%g,%g); i=%d, ni=%d",
		      nint, axp[0],axp[1], umin,umax, i,ne);
	    at = allocVector(REALSXP, n);
	    dn = axp[0];
	    n = 0;
	    while (dn < umax) {
		REAL(at)[n++] = dn;
		dn *= rng;
	    }
	    break;

	case 2: /* medium range:  1, 5	  * 10^k */
	    n = 0;
	    if (0.5 * dn >= umin) n++;
	    for (;;) {
		if (dn > umax) break;		n++;
		if (5 * dn > umax) break;	n++;
		dn *= 10;
	    }
	    if (!n)
		error("log - axis(), 'at' creation, _MEDIUM_ range: "
		      "illegal {xy}axp or par;\n"
		      "	 axp[0]= %g, usr[0:1]=(%g,%g)",
		      axp[0], umin,umax);

	    at = allocVector(REALSXP, n);
	    dn = axp[0];
	    n = 0;
	    if (0.5 * dn >= umin) REAL(at)[n++] = 0.5 * dn;
	    for (;;) {
		if (dn > umax) break;		REAL(at)[n++] = dn;
		if (5 * dn > umax) break;	REAL(at)[n++] = 5 * dn;
		dn *= 10;
	    }
	    break;

	case 3: /* small range:	 1,2,5,10 * 10^k */
	    n = 0;
	    if (0.2 * dn >= umin) n++;
	    if (0.5 * dn >= umin) n++;
	    for (;;) {
		if (dn > umax) break;		n++;
		if (2 * dn > umax) break;	n++;
		if (5 * dn > umax) break;	n++;
		dn *= 10;
	    }
	    if (!n)
		error("log - axis(), 'at' creation, _SMALL_ range: "
		      "illegal {xy}axp or par;\n"
		      "	 axp[0]= %g, usr[0:1]=(%g,%g)",
		      axp[0], umin,umax);
	    at = allocVector(REALSXP, n);
	    dn = axp[0];
	    n = 0;
	    if (0.2 * dn >= umin) REAL(at)[n++] = 0.2 * dn;
	    if (0.5 * dn >= umin) REAL(at)[n++] = 0.5 * dn;
	    for (;;) {
		if (dn > umax) break;		REAL(at)[n++] = dn;
		if (2 * dn > umax) break;	REAL(at)[n++] = 2 * dn;
		if (5 * dn > umax) break;	REAL(at)[n++] = 5 * dn;
		dn *= 10;
	    }
	    break;
	default:
	    error("log - axis(), 'at' creation: ILLEGAL {xy}axp[3] = %g",
		  axp[2]);
	}
    }
    return at;
}

SEXP do_axis(SEXP call, SEXP op, SEXP args, SEXP env)
{
    /* axis(side, at, labels, tick,
     *       line, pos, outer, font, vfont, ...) */

    SEXP at, lab, vfont;
    int col, fg, font;
    int i, n, nint = 0, ntmp, side, *ind, outer;
    int istart, iend, incr;
    Rboolean dolabels, doticks, logflag = FALSE;
    Rboolean vectorFonts = FALSE;
    double x, y, temp, tnew, tlast;
    double axp[3], usr[2];
    double gap, labw, low, high, line, pos;
    double axis_base, axis_tick, axis_lab, axis_low, axis_high;

    SEXP originalArgs = args;
    DevDesc *dd = CurrentDevice();

    /* Arity Check */
    /* This is a builtin function, so it should always have */
    /* the correct arity, but it doesn't hurt to be defensive. */

    if (length(args) < 9)
	errorcall(call, "too few arguments");
    GCheckState(dd);

    /* Required argument: "side" */
    /* Which side of the plot the axis is to appear on. */
    /* side = 1 | 2 | 3 | 4. */

    side = asInteger(CAR(args));
    if (side < 1 || side > 4)
	errorcall(call, "invalid axis number");
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
    if (isLogical(CAR(args)) && length(CAR(args)) > 0) {
	i = asLogical(CAR(args));
	if (i == 0 || i == NA_LOGICAL)
	    dolabels = FALSE;
	PROTECT(lab = R_NilValue);
    }
    else if (isExpression(CAR(args))) {
	PROTECT(lab = CAR(args));
    }
    else {
	PROTECT(lab = coerceVector(CAR(args), STRSXP));
    }
    args = CDR(args);

    /* Required argument: "ticks" */
    /* This indicates whether or not ticks and the axis line */
    /* should be plotted: TRUE => show, FALSE => don't show. */

    doticks = asLogical(CAR(args));
    doticks = (doticks == NA_LOGICAL) ? TRUE : (Rboolean) doticks;
    args = CDR(args);

    /* Optional argument: "line" */
    /* Specifies an offset outward from the plot for the axis. */
    /* The values in the par value "mgp" are interpreted */
    /* relative to this value. */

    line = asReal(CAR(args));
    if (!R_FINITE(line)) line =  dd->gp.mgp[2];
    args = CDR(args);

    /* Optional argument: "pos" */
    /* Specifies a user coordinate at which the axis should be drawn. */
    /* This overrides the value of "line".  Again the "mgp" par values */
    /* are interpreted relative to this value. */

    pos = asReal(CAR(args));
    if (!R_FINITE(pos)) pos = NA_REAL;
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

    /* Optional argument: "vfont" */
    /* Allows Hershey vector fonts to be used */
    PROTECT(vfont = FixupVFont(CAR(args)));
    if (!isNull(vfont))
	vectorFonts = TRUE;
    args = CDR(args);


    /* Retrieve relevant "par" values. */

    switch(side) {
    case 1:
    case 3:
	axp[0] = dd->dp.xaxp[0];
	axp[1] = dd->dp.xaxp[1];
	axp[2] = dd->dp.xaxp[2];
	usr[0] = dd->dp.usr[0];
	usr[1] = dd->dp.usr[1];
	logflag = dd->dp.xlog;
	nint = dd->dp.lab[0];
	break;
    case 2:
    case 4:
	axp[0] = dd->dp.yaxp[0];
	axp[1] = dd->dp.yaxp[1];
	axp[2] = dd->dp.yaxp[2];
	usr[0] = dd->dp.usr[2];
	usr[1] = dd->dp.usr[3];
	logflag = dd->dp.ylog;
	nint = dd->dp.lab[1];
	break;
    }

    /* Determine the tickmark positions.  Note that these may fall */
    /* outside the plot window. We will clip them in the code below. */

    if (length(at) == 0) {
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
	else if (!isExpression(lab))
	    lab = labelformat(lab);
	if (length(at) != length(lab))
	    errorcall(call, "location and label lengths differ");
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
    n = ntmp;
    if (n == 0)
	errorcall(call, "no locations are finite");

    /* Ok, all systems are "GO".  Let's get to it. */
    /* First we process all the inline par values. */

    GSavePars(dd);
    RecordGraphicsCall(call);
    ProcessInlinePars(args, dd);

    /* At this point we know the value of "xaxt" and "yaxt",
     * so we test to see whether the relevant one is "n".
     * If it is, we just bail out at this point. */

    if (((side == 1 || side == 3) && dd->gp.xaxt == 'n') ||
	((side == 2 || side == 4) && dd->gp.yaxt == 'n')) {
	GRestorePars(dd);
	UNPROTECT(4);
	return R_NilValue;
    }

    /* I can't think of a good reason to allow axes with a non-solid line-type, 
     * so I override any value the user may have specified.  
     * This may need to be revisited, but this should 
     * cover 99.99% of the cases. */

    dd->gp.lty = LTY_SOLID;

    /* Override par("xpd") and force clipping to figure region.
     * NOTE: don't override to _reduce_ clipping region */

    dd->gp.xpd = 2;

    dd->gp.adj = 0.5;
    dd->gp.font = (font == NA_INTEGER)? dd->gp.fontaxis : font;
    dd->gp.cex = dd->gp.cexbase * dd->gp.cexaxis;
    col = dd->gp.col;
    fg = dd->gp.fg;

    /* Draw the axis */
    GMode(1, dd);
    switch (side) {
    case 1: /*--- x-axis -- horizontal --- */
    case 3:
	GetAxisLimits(dd->gp.usr[0], dd->gp.usr[1], &low, &high);
	axis_low  = GConvertX(fmax2(low, REAL(at)[0]), USER, NFC, dd);
	axis_high = GConvertX(fmin2(high, REAL(at)[n-1]), USER, NFC, dd);
	if (side == 1) {
	    if (R_FINITE(pos))
		axis_base = GConvertY(pos, USER, NFC, dd);
	    else
		axis_base = GConvertY(0.0, outer, NFC, dd)
		    - GConvertYUnits(line, LINES, NFC, dd);
	    if (R_FINITE(dd->gp.tck))
		axis_tick = axis_base + GConvertYUnits(dd->gp.tck, NPC, NFC, dd);
	    else
		axis_tick = axis_base + GConvertYUnits(dd->gp.tcl, LINES, NFC, dd);
	}
	else {
	    if (R_FINITE(pos))
		axis_base = GConvertY(pos, USER, NFC, dd);
	    else
		axis_base =  GConvertY(1.0, outer, NFC, dd)
		    + GConvertYUnits(line, LINES, NFC, dd);
	    if (R_FINITE(dd->gp.tck))
		axis_tick = axis_base - GConvertYUnits(dd->gp.tck, NPC, NFC, dd);
	    else
		axis_tick = axis_base - GConvertYUnits(dd->gp.tcl, LINES, NFC, dd);
	}
	if (doticks) {
	    dd->gp.col = fg;
            GLine(axis_low, axis_base, axis_high, axis_base, NFC, dd);
	    for (i = 0; i < n; i++) {
		x = REAL(at)[i];
		if (low <= x && x <= high) {
		    x = GConvertX(x, USER, NFC, dd);
		    GLine(x, axis_base, x, axis_tick, NFC, dd);
		}
	    }
	}
	/* Tickmark labels. */
	dd->gp.col = dd->gp.colaxis;
	gap = GStrWidth("m", NFC, dd);	/* FIXUP x/y distance */
	tlast = -1.0;
	if (dd->gp.las == 2 || dd->gp.las == 3) {
	    dd->gp.adj = (side == 1) ? 1 : 0;
	}
	else dd->gp.adj = 0.5;
	if (side == 1) {
	    axis_lab = - axis_base
		+ GConvertYUnits(dd->gp.mgp[1], LINES, NFC, dd)
		+ GConvertY(0.0, NPC, NFC, dd);
	}
	else {
	    axis_lab = axis_base
		+ GConvertYUnits(dd->gp.mgp[1], LINES, NFC, dd)
		- GConvertY(1.0, NPC, NFC, dd);
	}
	axis_lab = GConvertYUnits(axis_lab, NFC, LINES, dd);

	/* The order of processing is important here. */
	/* We must ensure that the labels are drawn left-to-right. */
	/* The logic here is getting way too convoluted. */
	/* This needs a serious rewrite. */

	if (dd->gp.usr[0] > dd->gp.usr[1]) {
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
	    x = REAL(at)[i];
	    if (!R_FINITE(x)) continue;
	    temp = GConvertX(x, USER, NFC, dd);
	    if (dolabels) {
		/* Clip tick labels to user coordinates. */
		if (x > low && x < high) {
		    if (isExpression(lab)) {
			GMMathText(VECTOR_ELT(lab, ind[i]), side,
				   axis_lab, 0, x, dd->gp.las, dd);
		    }
		    else {
			labw = GStrWidth(CHAR(STRING_ELT(lab, ind[i])), NFC, dd);
			tnew = temp - 0.5 * labw;
			/* Check room for perpendicular labels. */
			if (dd->gp.las == 2 || dd->gp.las == 3 ||
			    tnew - tlast >= gap) {
			    GMtext(CHAR(STRING_ELT(lab, ind[i])), side,
				   axis_lab, 0, x, dd->gp.las, dd);
			    tlast = temp + 0.5 *labw;
			}
		    }
		}
	    }
	}
	break;

    case 2: /*--- y-axis -- vertical --- */
    case 4:
	GetAxisLimits(dd->gp.usr[2], dd->gp.usr[3], &low, &high);
	axis_low = GConvertY(fmax2(low, REAL(at)[0]), USER, NFC, dd);
	axis_high = GConvertY(fmin2(high, REAL(at)[n-1]), USER, NFC, dd);
	if (side == 2) {
	    if (R_FINITE(pos))
		axis_base = GConvertX(pos, USER, NFC, dd);
	    else
		axis_base =  GConvertX(0.0, outer, NFC, dd)
		    - GConvertXUnits(line, LINES, NFC, dd);
	    if (R_FINITE(dd->gp.tck))
		axis_tick = axis_base + GConvertXUnits(dd->gp.tck, NPC, NFC, dd);
	    else
		axis_tick = axis_base + GConvertXUnits(dd->gp.tcl, LINES, NFC, dd);
	}
	else {
	    if (R_FINITE(pos))
		axis_base = GConvertX(pos, USER, NFC, dd);
	    else
		axis_base =  GConvertX(1.0, outer, NFC, dd)
		    + GConvertXUnits(line, LINES, NFC, dd);
	    if (R_FINITE(dd->gp.tck))
		axis_tick = axis_base - GConvertXUnits(dd->gp.tck, NPC, NFC, dd);
	    else
		axis_tick = axis_base - GConvertXUnits(dd->gp.tcl, LINES, NFC, dd);
	}
	if (doticks) {
	    dd->gp.col = fg;
            GLine(axis_base, axis_low, axis_base, axis_high, NFC, dd);
	    for (i = 0; i < n; i++) {
		y = REAL(at)[i];
		if (low <= y && y <= high) {
		    y = GConvertY(y, USER, NFC, dd);
		    GLine(axis_base, y, axis_tick, y, NFC, dd);
		}
	    }
	}
	/* Tickmark labels. */
	dd->gp.col = dd->gp.colaxis;
	gap = GStrWidth("m", INCHES, dd);
	gap = GConvertYUnits(gap, INCHES, NFC, dd);
	tlast = -1.0;
	if (dd->gp.las == 1 || dd->gp.las == 2) {
	    dd->gp.adj = (side == 2) ? 1 : 0;
	}
	else dd->gp.adj = 0.5;
	if (side == 2) {
	    axis_lab = - axis_base
		+ GConvertXUnits(dd->gp.mgp[1], LINES, NFC, dd)
		+ GConvertX(0.0, NPC, NFC, dd);
	}
	else {
	    axis_lab = axis_base
		+ GConvertXUnits(dd->gp.mgp[1], LINES, NFC, dd)
		- GConvertX(1.0, NPC, NFC, dd);
	}
	axis_lab = GConvertXUnits(axis_lab, NFC, LINES, dd);

	/* The order of processing is important here. */
	/* We must ensure that the labels are drawn left-to-right. */
	/* The logic here is getting way too convoluted. */
	/* This needs a serious rewrite. */

	if (dd->gp.usr[2] > dd->gp.usr[3]) {
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
	    y = REAL(at)[i];
	    if (!R_FINITE(y)) continue;
	    temp = GConvertY(y, USER, NFC, dd);
	    if (dolabels) {
		/* Clip tick labels to user coordinates. */
		if (y > low && y < high) {
		    if (isExpression(lab)) {
			GMMathText(VECTOR_ELT(lab, ind[i]), side,
				   axis_lab, 0, y, dd->gp.las, dd);
		    }
		    else {
			labw = GStrWidth(CHAR(STRING_ELT(lab, ind[i])), INCHES, dd);
			labw = GConvertYUnits(labw, INCHES, NFC, dd);
			tnew = temp - 0.5 * labw;
			/* Check room for perpendicular labels. */
			if (dd->gp.las == 1 || dd->gp.las == 2 ||
			    tnew - tlast >= gap) {
			    GMtext(CHAR(STRING_ELT(lab, ind[i])), side,
				   axis_lab, 0, y, dd->gp.las, dd);
			    tlast = temp + 0.5 *labw;
			}
		    }
		}
	    }
	}
	break;
    } /* end  switch(side, ..) */
    UNPROTECT(4); /* lab, vfont, at, lab again */
    GMode(0, dd);
    GRestorePars(dd);
    /* NOTE: only record operation if no "error"  */
    if (GRecording(call))
	recordGraphicOperation(op, originalArgs, dd);
    return R_NilValue;
}/* do_axis */


SEXP do_plot_xy(SEXP call, SEXP op, SEXP args, SEXP env)
{
/*	plot.xy(xy, type, pch, lty, col, cex, ...)

 *	plot points or lines of various types
 */
    SEXP sxy, sx, sy, pch, cex, col, bg, lty;
    double *x, *y, xold, yold, xx, yy, thiscex;
    int i, n, npch, ncex, ncol, nbg, nlty, type=0, start=0, thispch, thiscol;

    SEXP originalArgs = args;
    DevDesc *dd = CurrentDevice();

    /* Basic Checks */
    GCheckState(dd);
    if (length(args) < 6)
	errorcall(call, "too few arguments");

    /* Required Arguments */
#define PLOT_XY_DEALING(subname)				\
    sx = R_NilValue;            /* -Wall */			\
    sy = R_NilValue;            /* -Wall */			\
    sxy = CAR(args);						\
    if (isNewList(sxy) && length(sxy) >= 2) {			\
	internalTypeCheck(call, sx = VECTOR_ELT(sxy, 0), REALSXP);	\
	internalTypeCheck(call, sy = VECTOR_ELT(sxy, 1), REALSXP);	\
    }								\
    else if (isList(sxy) && length(sxy) >= 2) {			\
	internalTypeCheck(call, sx = CAR(sxy), REALSXP);	\
	internalTypeCheck(call, sy = CADR(sxy), REALSXP);	\
    }								\
    else							\
	errorcall(call, "invalid plotting structure");		\
    if (LENGTH(sx) != LENGTH(sy))				\
	error("x and y lengths differ in" subname "().");	\
    n = LENGTH(sx);						\
    args = CDR(args)

    PLOT_XY_DEALING("plot.xy");

    if (isNull(CAR(args))) type = 'p';
    else {
	if (isString(CAR(args)) && LENGTH(CAR(args)) == 1 &&
	    LENGTH(pch = STRING_ELT(CAR(args), 0)) >= 1) {
	    if(LENGTH(pch) > 1)
		warningcall(call, "plot type '%s' truncated to first character",
			    CHAR(pch));
	    type = CHAR(pch)[0];
	}
	else errorcall(call, "invalid plot type");
    }
    args = CDR(args);

    PROTECT(pch = FixupPch(CAR(args), dd->gp.pch));	args = CDR(args);
    npch = length(pch);

    PROTECT(lty = FixupLty(CAR(args), dd->gp.lty));	args = CDR(args);
    nlty = length(lty);

    /* Default col was NA_INTEGER (0x80000000) which was interpreted
       as zero (black) or "don't draw" depending on line/rect/circle
       situation. Now we set the default to zero and don't plot at all
       if col==NA.

       FIXME: bg needs similar change, but that requires changes to
       the specific drivers. */

    PROTECT(col = FixupCol(CAR(args), 0)); args = CDR(args);
    ncol = LENGTH(col);

    PROTECT(bg = FixupCol(CAR(args), NA_INTEGER));	args = CDR(args);
    nbg = LENGTH(bg);

    PROTECT(cex = FixupCex(CAR(args), 1.0));	args = CDR(args);
    ncex = LENGTH(cex);

    /* Miscellaneous Graphical Parameters -- e.g., lwd */
    GSavePars(dd);
    RecordGraphicsCall(call);
    ProcessInlinePars(args, dd);

    x = REAL(sx);
    y = REAL(sy);

    if (nlty && INTEGER(lty)[0] != NA_INTEGER)
	dd->gp.lty = INTEGER(lty)[0];

    GMode(1, dd);
    /* removed by paul 26/5/99 because all clipping now happens in graphics.c
     * GClip(dd);
     */

    switch(type) {
    case 'l':
    case 'o':
	/* lines and overplotted lines and points */
	dd->gp.col = INTEGER(col)[0];
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
		    GPolyline(i-start, x+start, y+start,
			      USER, dd);
	    }
	    else if ((R_FINITE(xold) && R_FINITE(yold)) &&
		     (i == n-1))
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
	dd->gp.col = INTEGER(col)[0];
	xold = NA_REAL;
	yold = NA_REAL;
	for (i = 0; i < n; i++) {
	    xx = x[i];
	    yy = y[i];
	    GConvert(&xx, &yy, USER, INCHES, dd);
	    if (R_FINITE(xold) && R_FINITE(yold) &&
		R_FINITE(xx) && R_FINITE(yy)) {
		if ((f = d/hypot(xx-xold, yy-yold)) < 0.5) {
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

    case 's': /* step function  I */
    {
	double xtemp[3], ytemp[3];
	dd->gp.col = INTEGER(col)[0];
	xold = x[0];
	yold = y[0];
	GConvert(&xold, &yold, USER, DEVICE, dd);
	for (i = 1; i < n; i++) {
	    xx = x[i];
	    yy = y[i];
	    GConvert(&xx, &yy, USER, DEVICE, dd);
	    if (R_FINITE(xold) && R_FINITE(yold) &&
		R_FINITE(xx) && R_FINITE(yy)) {
		xtemp[0] = xold; ytemp[0] = yold;
		xtemp[1] = xx;	ytemp[1] = yold;
		xtemp[2] = xx;	ytemp[2] = yy;
		GPolyline(3, xtemp, ytemp, DEVICE, dd);
	    }
	    xold = xx;
	    yold = yy;
	}
    }
    break;

    case 'S': /* step function  II */
    {
	double xtemp[3], ytemp[3];
	dd->gp.col = INTEGER(col)[0];
	xold = x[0];
	yold = y[0];
	GConvert(&xold, &yold, USER, DEVICE, dd);
	for (i = 1; i < n; i++) {
	    xx = x[i];
	    yy = y[i];
	    GConvert(&xx, &yy, USER, DEVICE, dd);
	    if (R_FINITE(xold) && R_FINITE(yold) &&
		R_FINITE(xx) && R_FINITE(yy)) {
		xtemp[0] = xold; ytemp[0] = yold;
		xtemp[1] = xold; ytemp[1] = yy;
		xtemp[2] = xx; ytemp[2] = yy;
		GPolyline(3, xtemp, ytemp, DEVICE, dd);
	    }
	    xold = xx;
	    yold = yy;
	}
    }
    break;

    case 'h': /* h[istogram] (bar plot) */
	if (dd->gp.ylog)
	    yold = dd->gp.usr[2];/* DBL_MIN fails.. why ???? */
	else
	    yold = 0.0;
	yold = GConvertY(yold, USER, DEVICE, dd);
	for (i = 0; i < n; i++) {
	    xx = x[i];
	    yy = y[i];
	    GConvert(&xx, &yy, USER, DEVICE, dd);
	    if (R_FINITE(xx) && R_FINITE(yy)
		&& (thiscol = INTEGER(col)[i % ncol]) != NA_INTEGER) {
		dd->gp.col = thiscol;
		GLine(xx, yold, xx, yy, DEVICE, dd);
	    }
	}
	break;

    case 'p':
    case 'n': /* nothing here */
	break;

    default:/* OTHERWISE */
	errorcall(call, "invalid plot type '%c'", type);

    } /* End {switch(type)} */

    if (type == 'p' || type == 'b' || type == 'o') {
	for (i = 0; i < n; i++) {
	    xx = x[i];
	    yy = y[i];
	    GConvert(&xx, &yy, USER, DEVICE, dd);
	    if (R_FINITE(xx) && R_FINITE(yy)) {
		if (R_FINITE(thiscex = REAL(cex)[i % ncex])
		    && (thispch = INTEGER(pch)[i % npch]) != NA_INTEGER
		    && (thiscol = INTEGER(col)[i % ncol]) != NA_INTEGER)
		{
		    dd->gp.cex = thiscex * dd->gp.cexbase;
		    dd->gp.col = thiscol;
		    dd->gp.bg = INTEGER(bg)[i % nbg];
		    GSymbol(xx, yy, DEVICE, thispch, dd);
		}
	    }
	}
    }
    GMode(0, dd);
    GRestorePars(dd);
    UNPROTECT(5);
    /* NOTE: only record operation if no "error"  */
    if (GRecording(call))
	recordGraphicOperation(op, originalArgs, dd);
    return R_NilValue;
}/* do_plot_xy */

/* Checks for ... , x0, y0, x1, y1 ... */

static void xypoints(SEXP call, SEXP args, int *n)
{
    int k=0;/* -Wall */

    if (!isNumeric(CAR(args)) || (k = LENGTH(CAR(args))) <= 0)
	errorcall(call, "first argument invalid");
    SETCAR(args, coerceVector(CAR(args), REALSXP));
    *n = k;
    args = CDR(args);

    if (!isNumeric(CAR(args)) || (k = LENGTH(CAR(args))) <= 0)
	errorcall(call, "second argument invalid");
    SETCAR(args, coerceVector(CAR(args), REALSXP));
    if (k > *n) *n = k;
    args = CDR(args);

    if (!isNumeric(CAR(args)) || (k = LENGTH(CAR(args))) <= 0)
	errorcall(call, "third argument invalid");
    SETCAR(args, coerceVector(CAR(args), REALSXP));
    if (k > *n) *n = k;
    args = CDR(args);

    if (!isNumeric(CAR(args)) || (k = LENGTH(CAR(args))) <= 0)
	errorcall(call, "fourth argument invalid");
    SETCAR(args, coerceVector(CAR(args), REALSXP));
    if (k > *n) *n = k;
    args = CDR(args);
}


SEXP do_segments(SEXP call, SEXP op, SEXP args, SEXP env)
{
    /* segments(x0, y0, x1, y1, col, lty, lwd, ...) */
    SEXP sx0, sx1, sy0, sy1, col, lty, lwd;
    double *x0, *x1, *y0, *y1;
    double xx[2], yy[2];
    int nx0, nx1, ny0, ny1, i, n, ncol, nlty, nlwd;
    SEXP originalArgs = args;
    DevDesc *dd = CurrentDevice();

    if (length(args) < 4) errorcall(call, "too few arguments");
    GCheckState(dd);

    xypoints(call, args, &n);

    sx0 = CAR(args); nx0 = length(sx0); args = CDR(args);
    sy0 = CAR(args); ny0 = length(sy0); args = CDR(args);
    sx1 = CAR(args); nx1 = length(sx1); args = CDR(args);
    sy1 = CAR(args); ny1 = length(sy1); args = CDR(args);

    PROTECT(col = FixupCol(CAR(args), NA_INTEGER));
    ncol = LENGTH(col); args = CDR(args);

    PROTECT(lty = FixupLty(CAR(args), dd->gp.lty));
    nlty = length(lty); args = CDR(args);

    PROTECT(lwd = FixupLwd(CAR(args), dd->gp.lwd));
    nlwd = length(lwd); args = CDR(args);

    GSavePars(dd);
    RecordGraphicsCall(call);
    ProcessInlinePars(args, dd);

    x0 = REAL(sx0);
    y0 = REAL(sy0);
    x1 = REAL(sx1);
    y1 = REAL(sy1);

    GMode(1, dd);
    for (i = 0; i < n; i++) {
	xx[0] = x0[i%nx0];
	yy[0] = y0[i%ny0];
	xx[1] = x1[i%nx1];
	yy[1] = y1[i%ny1];
	GConvert(xx, yy, USER, DEVICE, dd);
	GConvert(xx+1, yy+1, USER, DEVICE, dd);
	if (R_FINITE(xx[0]) && R_FINITE(yy[0]) &&
	    R_FINITE(xx[1]) && R_FINITE(yy[1]))
	{
	    dd->gp.col = INTEGER(col)[i % ncol];
	    if (dd->gp.col == NA_INTEGER)
		dd->gp.col = dd->dp.col;
	    dd->gp.lty = INTEGER(lty)[i % nlty];
	    dd->gp.lwd = REAL(lwd)[i % nlwd];
	    GLine(xx[0], yy[0], xx[1], yy[1], DEVICE, dd);
	}
    }
    GMode(0, dd);
    GRestorePars(dd);

    UNPROTECT(3);
    /* NOTE: only record operation if no "error"  */
    if (GRecording(call))
	recordGraphicOperation(op, originalArgs, dd);
    return R_NilValue;
}


SEXP do_rect(SEXP call, SEXP op, SEXP args, SEXP env)
{
    /* rect(xl, yb, xr, yt, col, border, lty, lwd, xpd) */
    SEXP sxl, sxr, syb, syt, col, lty, lwd, border;
    double *xl, *xr, *yb, *yt, x0, y0, x1, y1;
    int i, n, nxl, nxr, nyb, nyt, ncol, nlty, nlwd, nborder, xpd;
    SEXP originalArgs = args;
    DevDesc *dd = CurrentDevice();

    if (length(args) < 4) errorcall(call, "too few arguments");
    GCheckState(dd);

    xypoints(call, args, &n);
    sxl = CAR(args); nxl = length(sxl); args = CDR(args);
    syb = CAR(args); nyb = length(syb); args = CDR(args);
    sxr = CAR(args); nxr = length(sxr); args = CDR(args);
    syt = CAR(args); nyt = length(syt); args = CDR(args);

    PROTECT(col = FixupCol(CAR(args), NA_INTEGER));
    ncol = LENGTH(col);
    args = CDR(args);

    PROTECT(border =  FixupCol(CAR(args), NA_INTEGER));
    nborder = LENGTH(border);
    args = CDR(args);

    PROTECT(lty = FixupLty(CAR(args), dd->gp.lty));
    nlty = length(lty);
    args = CDR(args);

    PROTECT(lwd = FixupLwd(CAR(args), dd->gp.lwd));
    nlwd = length(lwd);
    args = CDR(args);

    xpd = asInteger(CAR(args));
    args = CDR(args);

    GSavePars(dd);

    if (xpd == NA_INTEGER)
	dd->gp.xpd = 2;
    else
	dd->gp.xpd = xpd;

    xl = REAL(sxl);
    xr = REAL(sxr);
    yb = REAL(syb);
    yt = REAL(syt);

    GMode(1, dd);
    for (i = 0; i < n; i++) {
	if (nlty && INTEGER(lty)[i % nlty] != NA_INTEGER)
	    dd->gp.lty = INTEGER(lty)[i % nlty];
	else
	    dd->gp.lty = dd->dp.lty;
	if (nlwd && REAL(lwd)[i % nlwd] != NA_REAL)
	    dd->gp.lwd = REAL(lwd)[i % nlwd];
	else
	    dd->gp.lwd = dd->dp.lwd;
	x0 = xl[i%nxl];
	y0 = yb[i%nyb];
	x1 = xr[i%nxr];
	y1 = yt[i%nyt];
	GConvert(&x0, &y0, USER, DEVICE, dd);
	GConvert(&x1, &y1, USER, DEVICE, dd);
	if (R_FINITE(x0) && R_FINITE(y0) && R_FINITE(x1) && R_FINITE(y1))
	    GRect(x0, y0, x1, y1, DEVICE, INTEGER(col)[i % ncol],
		  INTEGER(border)[i % nborder], dd);
    }
    GMode(0, dd);

    GRestorePars(dd);
    UNPROTECT(4);
    /* NOTE: only record operation if no "error"  */
    if (GRecording(call))
	recordGraphicOperation(op, originalArgs, dd);
    return R_NilValue;
}


SEXP do_arrows(SEXP call, SEXP op, SEXP args, SEXP env)
{
    /* arrows(x0, y0, x1, y1, length, angle, code, col, lty, lwd, xpd) */
    SEXP sx0, sx1, sy0, sy1, col, lty, lwd;
    double *x0, *x1, *y0, *y1;
    double xx0, yy0, xx1, yy1;
    double hlength, angle;
    int code;
    int nx0, nx1, ny0, ny1, i, n, ncol, nlty, nlwd, xpd;
    SEXP originalArgs = args;
    DevDesc *dd = CurrentDevice();

    if (length(args) < 4) errorcall(call, "too few arguments");
    GCheckState(dd);

    xypoints(call, args, &n);

    sx0 = CAR(args); nx0 = length(sx0); args = CDR(args);
    sy0 = CAR(args); ny0 = length(sy0); args = CDR(args);
    sx1 = CAR(args); nx1 = length(sx1); args = CDR(args);
    sy1 = CAR(args); ny1 = length(sy1); args = CDR(args);

    hlength = asReal(CAR(args));
    if (!R_FINITE(hlength) || hlength <= 0)
	errorcall(call, "invalid head length");
    args = CDR(args);

    angle = asReal(CAR(args));
    if (!R_FINITE(angle))
	errorcall(call, "invalid head angle");
    args = CDR(args);

    code = asInteger(CAR(args));
    if (code == NA_INTEGER || code < 0 || code > 3)
	errorcall(call, "invalid arrow head specification");
    args = CDR(args);

    PROTECT(col = FixupCol(CAR(args), NA_INTEGER));
    ncol = LENGTH(col);
    args = CDR(args);

    PROTECT(lty = FixupLty(CAR(args), dd->gp.lty));
    nlty = length(lty);
    args = CDR(args);

    PROTECT(lwd = CAR(args));/*need  FixupLwd ?*/
    nlwd = length(lwd);
    if (nlwd == 0)
	errorcall(call, "'lwd' must be numeric of length >=1");
    args = CDR(args);

    xpd = asInteger(CAR(args));
    args = CDR(args);

    GSavePars(dd);

    if (xpd == NA_INTEGER)
	dd->gp.xpd = 2;
    else
	dd->gp.xpd = xpd;

    x0 = REAL(sx0);
    y0 = REAL(sy0);
    x1 = REAL(sx1);
    y1 = REAL(sy1);

    GMode(1, dd);
    for (i = 0; i < n; i++) {
	xx0 = x0[i%nx0];
	yy0 = y0[i%ny0];
	xx1 = x1[i%nx1];
	yy1 = y1[i%ny1];
	GConvert(&xx0, &yy0, USER, DEVICE, dd);
	GConvert(&xx1, &yy1, USER, DEVICE, dd);
	if (R_FINITE(xx0) && R_FINITE(yy0) && R_FINITE(xx1) && R_FINITE(yy1)) {
	  dd->gp.col = INTEGER(col)[i % ncol];
	    if (dd->gp.col == NA_INTEGER)
		dd->gp.col = dd->dp.col;
	    if (nlty == 0 || INTEGER(lty)[i % nlty] == NA_INTEGER)
		dd->gp.lty = dd->dp.lty;
	    else
		dd->gp.lty = INTEGER(lty)[i % nlty];
	    dd->gp.lwd = REAL(lwd)[i % nlwd];
	    GArrow(xx0, yy0, xx1, yy1, DEVICE,
		   hlength, angle, code, dd);
	}
    }
    GMode(0, dd);
    GRestorePars(dd);

    UNPROTECT(3);
    /* NOTE: only record operation if no "error"  */
    if (GRecording(call))
	recordGraphicOperation(op, originalArgs, dd);
    return R_NilValue;
}


static void drawPolygon(int n, double *x, double *y, 
			int lty, int fill, int border, DevDesc *dd)
{
    if (lty == NA_INTEGER)
	dd->gp.lty = dd->dp.lty;
    else
	dd->gp.lty = lty;
    GPolygon(n, x, y, USER, fill, border, dd);
}

SEXP do_polygon(SEXP call, SEXP op, SEXP args, SEXP env)
{
    /* polygon(x, y, col, border, lty, xpd, ...) */
    SEXP sx, sy, col, border, lty, sxpd;
#ifdef Older
    int nx=1, ny=1;
#else
    int nx;
#endif
    int ncol, nborder, nlty, xpd, i, start=0;
    int num = 0;
    double *x, *y, xx, yy, xold, yold;

    SEXP originalArgs = args;
    DevDesc *dd = CurrentDevice();

    GCheckState(dd);

    if (length(args) < 2) errorcall(call, "too few arguments");
#ifdef Older
    if (!isNumeric(CAR(args)) || (nx = LENGTH(CAR(args))) <= 0)
	errorcall(call, "first argument invalid");
    sx = SETCAR(args, coerceVector(CAR(args), REALSXP));
    args = CDR(args);

    if (!isNumeric(CAR(args)) || (ny = LENGTH(CAR(args))) <= 0)
	errorcall(call, "second argument invalid");
    sy = SETCAR(args, coerceVector(CAR(args), REALSXP));
    args = CDR(args);

    if (ny != nx)
	errorcall(call, "x and y lengths differ in polygon");
#else
    /* (x,y) is checked in R via xy.coords() ; no need here : */
    sx = SETCAR(args, coerceVector(CAR(args), REALSXP));  args = CDR(args);
    sy = SETCAR(args, coerceVector(CAR(args), REALSXP));  args = CDR(args);
    nx = LENGTH(sx);
#endif

    PROTECT(col = FixupCol(CAR(args), NA_INTEGER));	args = CDR(args);
    ncol = LENGTH(col);

    PROTECT(border = FixupCol(CAR(args), dd->gp.fg));	args = CDR(args);
    nborder = LENGTH(border);

    PROTECT(lty = FixupLty(CAR(args), dd->gp.lty));	args = CDR(args);
    nlty = length(lty);

    sxpd = CAR(args);
    if (sxpd != R_NilValue)
	xpd = asInteger(sxpd);
    else
	xpd = dd->gp.xpd;
    args = CDR(args);

    GSavePars(dd);
    RecordGraphicsCall(call);
    ProcessInlinePars(args, dd);

    if (xpd == NA_INTEGER)
	dd->gp.xpd = 2;
    else
	dd->gp.xpd = xpd;

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
    /* NOTE: only record operation if no "error"  */
    if (GRecording(call))
	recordGraphicOperation(op, originalArgs, dd);
    return R_NilValue;
}

SEXP do_text(SEXP call, SEXP op, SEXP args, SEXP env)
{
/* text(xy, labels, adj, pos, offset,
 *	vfont, cex, col, font, xpd, ...)
 */
    SEXP sx, sy, sxy, sxpd, txt, adj, pos, cex, col, font, vfont;
    int i, n, npos, ncex, ncol, nfont, ntxt, xpd;
    double adjx = 0, adjy = 0, offset = 0.5;
    double *x, *y;
    double xx, yy;
    Rboolean vectorFonts = FALSE;
    SEXP originalArgs = args;
    DevDesc *dd = CurrentDevice();

    GCheckState(dd);

    if (length(args) < 3) errorcall(call, "too few arguments");

    PLOT_XY_DEALING("text");

    /* labels */
    txt = CAR(args);
    if (isSymbol(txt) || isLanguage(txt))
	txt = coerceVector(txt, EXPRSXP);
    else if (!isExpression(txt))
	txt = coerceVector(txt, STRSXP);
    PROTECT(txt);
    if (length(txt) <= 0)
	errorcall(call, "zero length \"text\" specified");
    args = CDR(args);

    PROTECT(adj = CAR(args));
    if (isNull(adj) || (isNumeric(adj) && length(adj) == 0)) {
	adjx = dd->gp.adj;
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
    else errorcall(call, "invalid adj value");
    args = CDR(args);

    PROTECT(pos = coerceVector(CAR(args), INTSXP));
    npos = length(pos);
    for (i = 0; i < npos; i++)
        if (INTEGER(pos)[i] < 1 || INTEGER(pos)[i] > 4)
	    errorcall(call, "invalid pos value");
    args = CDR(args);

    offset = GConvertXUnits(asReal(CAR(args)), CHARS, INCHES, dd);
    args = CDR(args);

    PROTECT(vfont = FixupVFont(CAR(args)));
    if (!isNull(vfont))
	vectorFonts = TRUE;
    args = CDR(args);

    PROTECT(cex = FixupCex(CAR(args), 1.0));
    ncex = LENGTH(cex);
    args = CDR(args);

    PROTECT(col = FixupCol(CAR(args), NA_INTEGER));
    ncol = LENGTH(col);
    args = CDR(args);

    PROTECT(font = FixupFont(CAR(args), NA_INTEGER));
    nfont = LENGTH(font);
    args = CDR(args);

    sxpd = CAR(args); /* xpd: NULL -> par("xpd") */
    if (sxpd != R_NilValue)
	xpd = asInteger(sxpd);
    else
	xpd = dd->gp.xpd;
    args = CDR(args);

    x = REAL(sx);
    y = REAL(sy);
    n = LENGTH(sx);
    ntxt = LENGTH(txt);

    GSavePars(dd);
    RecordGraphicsCall(call);
    ProcessInlinePars(args, dd);

    dd->gp.xpd = (xpd == NA_INTEGER)? 2 : xpd;

    GMode(1, dd);
    for (i = 0; i < n; i++) {
	xx = x[i % n];
	yy = y[i % n];
	GConvert(&xx, &yy, USER, INCHES, dd);
	if (R_FINITE(xx) && R_FINITE(yy)) {
	    if (ncol && INTEGER(col)[i % ncol] != NA_INTEGER)
		dd->gp.col = INTEGER(col)[i % ncol];
	    else
		dd->gp.col = dd->dp.col;
	    if (ncex && R_FINITE(REAL(cex)[i%ncex]))
		dd->gp.cex = dd->gp.cexbase * REAL(cex)[i % ncex];
	    else
		dd->gp.cex = dd->gp.cexbase;
	    if (nfont && INTEGER(font)[i % nfont] != NA_INTEGER)
		dd->gp.font = INTEGER(font)[i % nfont];
	    else
		dd->gp.font = dd->dp.font;
	    if (npos > 0) {
		switch(INTEGER(pos)[i % npos]) {
		case 1:
		    yy = yy - offset;
		    adjx = 0.5;
		    adjy = 1 - (0.5 - dd->gp.yCharOffset);
		    break;
		case 2:
		    xx = xx - offset;
		    adjx = 1;
		    adjy = dd->gp.yCharOffset;
		    break;
		case 3:
		    yy = yy + offset;
		    adjx = 0.5;
		    adjy = 0;
		    break;
		case 4:
		    xx = xx + offset;
		    adjx = 0;
		    adjy = dd->gp.yCharOffset;
		    break;
		}
	    }
	    if (vectorFonts)
		GVText(xx, yy, INCHES, CHAR(STRING_ELT(txt, i % ntxt)),
		       INTEGER(vfont)[0], INTEGER(vfont)[1],
		       adjx, adjy, dd->gp.srt, dd);
	    else if (isExpression(txt))
		GMathText(xx, yy, INCHES, VECTOR_ELT(txt, i % ntxt),
			  adjx, adjy, dd->gp.srt, dd);
	    else
		GText(xx, yy, INCHES, CHAR(STRING_ELT(txt, i % ntxt)),
		      adjx, adjy, dd->gp.srt, dd);
	}
    }
    GMode(0, dd);

    GRestorePars(dd);
    UNPROTECT(7);
    /* NOTE: only record operation if no "error"  */
    if (GRecording(call))
	recordGraphicOperation(op, originalArgs, dd);
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
	case 2:/* perpendicular to axis */
	    switch(side) {
	    case 1:
	    case 2: adj = 1.0; break;
	    case 3:
	    case 4: adj = 0.0; break;
	    }
	case 3:/* vertical */
	    switch(side) {
	    case 1: adj = 1.0; break;
	    case 3: adj = 0.0; break;
	    case 2:
	    case 4: adj = 0.5; break;
	    }
	}
    }
    return adj;
}

static double ComputeAtValue(double at, double adj, int side, int outer,
			     DevDesc *dd)
{
    if (!R_FINITE(at)) {
	switch(side % 2) {
	case 0:
	    at  = outer ? adj : yNPCtoUsr(adj, dd);
	    break;
	case 1:
	    at = outer ? adj : xNPCtoUsr(adj, dd);
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
         cex = NA,
         col = NA,
         font = NA,
         vfont = NULL,
         ...) */

SEXP do_mtext(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP text, side, line, outer, at, adj, cex, col, font, vfont;
    int ntext, nside, nline, nouter, nat, nadj, ncex, ncol, nfont;
    Rboolean dirtyplot = FALSE, gpnewsave = FALSE, dpnewsave = FALSE;
    Rboolean vectorFonts = FALSE;
    int i, n, fontsave, colsave;
    double cexsave;
    SEXP originalArgs = args;
    DevDesc *dd = CurrentDevice();

    GCheckState(dd);

    if (length(args) < 9)
	errorcall(call, "too few arguments");

    /* Arg1 : text= */
    text = CAR(args);
    if (isSymbol(text) || isLanguage(text))
	text = coerceVector(text, EXPRSXP);
    else if (!isExpression(text))
	text = coerceVector(text, STRSXP);
    PROTECT(text);
    n = ntext = length(text);
    if (ntext <= 0)
	errorcall(call, "zero length \"text\" specified");
    args = CDR(args);

    /* Arg2 : side= */
    PROTECT(side = coerceVector(CAR(args), INTSXP));
    nside = length(side);
    if (nside <= 0) errorcall(call, "zero length \"side\" specified");
    if (n < nside) n = nside;
    args = CDR(args);

    /* Arg3 : line= */
    PROTECT(line = coerceVector(CAR(args), REALSXP));
    nline = length(line);
    if (nline <= 0) errorcall(call, "zero length \"line\" specified");
    if (n < nline) n = nline;
    args = CDR(args);

    /* Arg4 : outer= */
    /* outer == NA => outer <- 0 */
    PROTECT(outer = coerceVector(CAR(args), INTSXP));
    nouter = length(outer);
    if (nouter <= 0) errorcall(call, "zero length \"outer\" specified");
    if (n < nouter) n = nouter;
    args = CDR(args);

    /* Arg5 : at= */
    PROTECT(at = coerceVector(CAR(args), REALSXP));
    nat = length(at);
    if (nat <= 0) errorcall(call, "zero length \"at\" specified");
    if (n < nat) n = nat;
    args = CDR(args);

    /* Arg6 : adj= */
    PROTECT(adj = coerceVector(CAR(args), REALSXP));
    nadj = length(adj);
    if (nadj <= 0) errorcall(call, "zero length \"adj\" specified");
    if (n < nadj) n = nadj;
    args = CDR(args);

    /* Arg7 : cex */
    PROTECT(cex = FixupCex(CAR(args), 1.0));
    ncex = length(cex);
    if (ncex <= 0) errorcall(call, "zero length \"cex\" specified");
    if (n < ncex) n = ncex;
    args = CDR(args);

    /* Arg8 : col */
    PROTECT(col = FixupCol(CAR(args), NA_INTEGER));
    ncol = length(col);
    if (ncol <= 0) errorcall(call, "zero length \"col\" specified");
    if (n < ncol) n = ncol;
    args = CDR(args);

    /* Arg9 : font */
    PROTECT(font = FixupFont(CAR(args), NA_INTEGER));
    nfont = length(font);
    if (nfont <= 0) errorcall(call, "zero length \"font\" specified");
    if (n < nfont) n = nfont;
    args = CDR(args);

    /* Arg10 : vfont */
    PROTECT(vfont = FixupVFont(CAR(args)));
    if (!isNull(vfont))
	vectorFonts = TRUE;
    args = CDR(args);

    GSavePars(dd);
    RecordGraphicsCall(call);
    ProcessInlinePars(args, dd);

    /* If we only scribble in the outer margins, */
    /* we don't want to mark the plot as dirty. */

    dirtyplot = FALSE;
    gpnewsave = dd->gp.new;
    dpnewsave = dd->dp.new;
    cexsave = dd->gp.cex;
    fontsave = dd->gp.font;
    colsave = dd->gp.col;

    /* override par("xpd") and force clipping to figure region */
    /* NOTE: don't override to _reduce_ clipping region */
    if (dd->gp.xpd < 1)
	dd->gp.xpd = 1;

    if (outer) {
	gpnewsave = dd->gp.new;
	dpnewsave = dd->dp.new;
	/* override par("xpd") and force clipping to device region */
	dd->gp.xpd = 2;
    }
    GMode(1, dd);

    for (i = 0; i < n; i++) {
	double atval = REAL(at)[i%nat];
	double adjval = REAL(adj)[i%nadj];
	double cexval = REAL(cex)[i%ncex];
	double lineval = REAL(line)[i%nline];
	int outerval = INTEGER(outer)[i%nouter];
	int sideval = INTEGER(side)[i%nside];
	int fontval = INTEGER(font)[i%nfont];
	int colval = INTEGER(col)[i%ncol];

	if (outerval == NA_INTEGER) outerval = 0;
	/* Note : we ignore any shrinking produced */
	/* by mfrow / mfcol specs here.  I.e. don't */
	/* dd->gp.cexbase. */
	if (R_FINITE(cexval)) dd->gp.cex = cexval;
	else cexval = cexsave;
	dd->gp.font = (fontval == NA_INTEGER) ? fontsave : fontval;
	dd->gp.col = (colval == NA_INTEGER) ? colsave : colval;
	dd->gp.adj = ComputeAdjValue(adjval, sideval, dd->gp.las);
	atval = ComputeAtValue(atval, dd->gp.adj, sideval, outerval, dd);

	if (vectorFonts) {
#ifdef GMV_implemented
	    GMVText(CHAR(STRING_ELT(text, i%ntext)),
		    INTEGER(vfont)[0], INTEGER(vfont)[1],
		    sideval, lineval, outerval, atval, dd->gp.las, dd);
#else
  	    warningcall(call,"Hershey fonts not yet implemented for mtext()");
	    GMtext(CHAR(STRING_ELT(text, i%ntext)),
		   sideval, lineval, outerval, atval, dd->gp.las, dd);
#endif
	}
	else if (isExpression(text))
	    GMMathText(VECTOR_ELT(text, i%ntext),
		       sideval, lineval, outerval, atval, dd->gp.las, dd);
	else
	    GMtext(CHAR(STRING_ELT(text, i%ntext)),
		   sideval, lineval, outerval, atval, dd->gp.las, dd);

    	if (outerval == 0) dirtyplot = TRUE;
    }
    GMode(0, dd);

    GRestorePars(dd);
    if (!dirtyplot) {
	dd->gp.new = gpnewsave;
	dd->dp.new = dpnewsave;
    }
    UNPROTECT(10);

    /* NOTE: only record operation if no "error"  */
    if (GRecording(call))
	recordGraphicOperation(op, originalArgs, dd);
    return R_NilValue;
}/* do_mtext */


SEXP do_title(SEXP call, SEXP op, SEXP args, SEXP env)
{
/* Annotation for plots :

   title(main, sub, xlab, ylab,
         line, outer,
         ...) */

    SEXP Main, xlab, ylab, sub, vfont;
    double adj, adjy, cex, offset, line, hpos, vpos, where;
    int col, font, outer;
    int i, n;
    SEXP originalArgs = args;
    DevDesc *dd = CurrentDevice();

    GCheckState(dd);

    if (length(args) < 6) errorcall(call, "too few arguments");

    Main = sub = xlab = ylab = R_NilValue;

    if (CAR(args) != R_NilValue && LENGTH(CAR(args)) > 0)
	Main = CAR(args);
    args = CDR(args);

    if (CAR(args) != R_NilValue && LENGTH(CAR(args)) > 0)
	sub = CAR(args);
    args = CDR(args);

    if (CAR(args) != R_NilValue && LENGTH(CAR(args)) > 0)
	xlab = CAR(args);
    args = CDR(args);

    if (CAR(args) != R_NilValue && LENGTH(CAR(args)) > 0)
	ylab = CAR(args);
    args = CDR(args);

    line = asReal(CAR(args));
    args = CDR(args);

    outer = asLogical(CAR(args));
    if (outer == NA_LOGICAL) outer = 0;
    args = CDR(args);

    GSavePars(dd);
    RecordGraphicsCall(call);
    ProcessInlinePars(args, dd);

    /* override par("xpd") and force clipping to figure region */
    /* NOTE: don't override to _reduce_ clipping region */
    if (dd->gp.xpd < 1)
	dd->gp.xpd = 1;
    if (outer)
	dd->gp.xpd = 2;
    adj = dd->gp.adj;

    GMode(1, dd);
    if (Main != R_NilValue) {
	cex = dd->gp.cexmain;
	col = dd->gp.colmain;
	font = dd->gp.fontmain;
	GetTextArg(call, Main, &Main, &col, &cex, &font, &vfont);
	dd->gp.col = col;
	dd->gp.cex = dd->gp.cexbase * cex;
	dd->gp.font = font;
	if (outer) {
	    if (R_FINITE(line)) {
		vpos = line;
		adjy = 0;
	    }
	    else {
		vpos = 0.5 * dd->gp.oma[2];
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
		vpos = 0.5 * dd->gp.mar[2];
		adjy = 0.5;
	    }
	    hpos = GConvertX(adj, NPC, USER, dd);
	    where = MAR3;
	}
	if (isExpression(Main)) {
	    GMathText(hpos, vpos, where,
		      VECTOR_ELT(Main, 0), adj, 0.5, 0.0, dd);
	}
	else {
	  n = length(Main);
	  offset = 0.5 * (n - 1) + vpos;
	  for (i = 0; i < n; i++)
	      GText(hpos, offset - i, where,
		    CHAR(STRING_ELT(Main, i)), adj, adjy, 0.0, dd);
	}
    }
    if (sub != R_NilValue) {
	cex = dd->gp.cexsub;
	col = dd->gp.colsub;
	font = dd->gp.fontsub;
	GetTextArg(call, sub, &sub, &col, &cex, &font, &vfont);
	dd->gp.col = col;
	dd->gp.cex = dd->gp.cexbase * cex;
	dd->gp.font = font;
	if (R_FINITE(line))
	    vpos = line;
	else
	    vpos = dd->gp.mgp[0] + 1;
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
		       hpos, 0, dd);
	else {
	    n = length(sub);
	    for (i = 0; i < n; i++)
		GMtext(CHAR(STRING_ELT(sub, i)), 1, vpos, where,
		       hpos, 0, dd);
	}
    }
    if (xlab != R_NilValue) {
	cex = dd->gp.cexlab;
	col = dd->gp.collab;
	font = dd->gp.fontlab;
	GetTextArg(call, xlab, &xlab, &col, &cex, &font, &vfont);
	dd->gp.cex = dd->gp.cexbase * cex;
	dd->gp.col = col;
	dd->gp.font = font;
	if (R_FINITE(line))
	    vpos = line;
	else
	    vpos = dd->gp.mgp[0];
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
		       hpos, 0, dd);
	else {
	    n = length(xlab);
	    for (i = 0; i < n; i++)
		GMtext(CHAR(STRING_ELT(xlab, i)), 1, vpos + i, where,
		   hpos, 0, dd);
	}
    }
    if (ylab != R_NilValue) {
	cex = dd->gp.cexlab;
	col = dd->gp.collab;
	font = dd->gp.fontlab;
	GetTextArg(call, ylab, &ylab, &col, &cex, &font, &vfont);
	dd->gp.cex = dd->gp.cexbase * cex;
	dd->gp.col = col;
	dd->gp.font = font;
	if (R_FINITE(line))
	    vpos = line;
	else
	    vpos = dd->gp.mgp[0];
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
		       hpos, 0, dd);
	else {
	    n = length(ylab);
	    for (i = 0; i < n; i++)
		GMtext(CHAR(STRING_ELT(ylab, i)), 2, vpos - i, where,
		       hpos, 0, dd);
	}
    }
    GMode(0, dd);
    GRestorePars(dd);
    /* NOTE: only record operation if no "error"  */
    if (GRecording(call))
	recordGraphicOperation(op, originalArgs, dd);
    return R_NilValue;
}


/*  abline(a, b, h, v, col, lty, lwd, ...)
    draw lines in intercept/slope form.  */

SEXP do_abline(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP a, b, h, v, untf, col, lty, lwd;
    int i, ncol, nlines, nlty, nlwd;
    double aa, bb, x[2], y[2];
    SEXP originalArgs = args;
    DevDesc *dd = CurrentDevice();

    GCheckState(dd);

    if (length(args) < 5) errorcall(call, "too few arguments");

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


    PROTECT(col = FixupCol(CAR(args), NA_INTEGER));	args = CDR(args);
    ncol = LENGTH(col);

    PROTECT(lty = FixupLty(CAR(args), dd->gp.lty));	args = CDR(args);
    nlty = length(lty);

    PROTECT(lwd = FixupLwd(CAR(args), dd->gp.lwd));     args = CDR(args);
    nlwd = length(lwd);

    GSavePars(dd);

    nlines = 0;

    if (a != R_NilValue) {
	if (b == R_NilValue) {
	    if (LENGTH(a) != 2)
		errorcall(call, "invalid a=, b= specification");
	    aa = REAL(a)[0];
	    bb = REAL(a)[1];
	}
	else {
	    aa = asReal(a);
	    bb = asReal(b);
	}
	if (!R_FINITE(aa) || !R_FINITE(bb))
	    errorcall(call, "\"a\" and \"b\" must be finite");
	dd->gp.col = INTEGER(col)[0];
	dd->gp.lwd = REAL(lwd)[0];
	if (nlty && INTEGER(lty)[0] != NA_INTEGER)
	    dd->gp.lty = INTEGER(lty)[0];
	else
	    dd->gp.lty = dd->dp.lty;
	GMode(1, dd);
	x[0] = dd->gp.usr[0];
	x[1] = dd->gp.usr[1];
	if (R_FINITE(dd->gp.lwd)) {
	    if (LOGICAL(untf)[0] == 1 && (dd->gp.xlog || dd->gp.ylog)) {
		double xx[101], yy[101];
		double xstep = (x[1] - x[0])/100;
		for (i = 0; i < 100; i++) {
		    xx[i] = x[0] + i*xstep;
		    yy[i] = aa + xx[i] * bb;
		}
		xx[100] = x[1];
		yy[100] = aa + x[1] * bb;
		GPolyline(101, xx, yy, USER, dd);
	    }
	    else {
		double x0, x1;

		x0 = ( dd->gp.xlog ) ?  log10(x[0]) : x[0];
		x1 = ( dd->gp.xlog ) ?  log10(x[1]) : x[1];

		y[0] = aa + x0 * bb;
		y[1] = aa + x1 * bb;

		if ( dd->gp.ylog ){
		    y[0] = pow(10.,y[0]);
		    y[1] = pow(10.,y[1]);
		}

		GLine(x[0], y[0], x[1], y[1], USER, dd);
	    }
	}
	GMode(0, dd);
	nlines++;
    }
    if (h != R_NilValue) {
	GMode(1, dd);
	for (i = 0; i < LENGTH(h); i++) {
	    dd->gp.col = INTEGER(col)[nlines % ncol];
	    if (nlty && INTEGER(lty)[nlines % nlty] != NA_INTEGER)
		dd->gp.lty = INTEGER(lty)[nlines % nlty];
	    else
		dd->gp.lty = dd->dp.lty;
	    dd->gp.lwd = REAL(lwd)[nlines % nlwd];
	    aa = REAL(h)[i];
	    if (R_FINITE(aa) && R_FINITE(dd->gp.lwd)) {
		x[0] = dd->gp.usr[0];
		x[1] = dd->gp.usr[1];
		y[0] = aa;
		y[1] = aa;
		GLine(x[0], y[0], x[1], y[1], USER, dd);
	    }
	    nlines++;
	}
	GMode(0, dd);
    }
    if (v != R_NilValue) {
	GMode(1, dd);
	for (i = 0; i < LENGTH(v); i++) {
	    dd->gp.col = INTEGER(col)[nlines % ncol];
	    if (nlty && INTEGER(lty)[nlines % nlty] != NA_INTEGER)
		dd->gp.lty = INTEGER(lty)[nlines % nlty];
	    else
		dd->gp.lty = dd->dp.lty;
	    dd->gp.lwd = REAL(lwd)[nlines % nlwd];
	    aa = REAL(v)[i];
	    if (R_FINITE(aa) && R_FINITE(dd->gp.lwd)) {
		y[0] = dd->gp.usr[2];
		y[1] = dd->gp.usr[3];
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
    /* NOTE: only record operation if no "error"  */
    if (GRecording(call))
	recordGraphicOperation(op, originalArgs, dd);
    return R_NilValue;
}

SEXP do_box(SEXP call, SEXP op, SEXP args, SEXP env)
{
/*     box(which="plot", lty="solid", ...)
       --- which is coded, 1 = plot, 2 = figure, 3 = inner, 4 = outer.
*/
    int which, col, fg;
    SEXP originalArgs = args;
    DevDesc *dd = CurrentDevice();

    GCheckState(dd);
    GSavePars(dd);
    which = asInteger(CAR(args)); args = CDR(args);
    if (which < 1 || which > 4)
	errorcall(call, "invalid \"which\" specification");
    col= dd->gp.col;	dd->gp.col= NA_INTEGER;
    fg = dd->gp.col;	dd->gp.fg = NA_INTEGER;
    RecordGraphicsCall(call);
    ProcessInlinePars(args, dd);
    if (dd->gp.col == NA_INTEGER) {
	if (dd->gp.fg == NA_INTEGER)
	    dd->gp.col = col;
	else
	    dd->gp.col = dd->gp.fg;
    }
    /* override par("xpd") and force clipping to device region */
    dd->gp.xpd = 2;
    GMode(1, dd);
    GBox(which, dd);
    GMode(0, dd);
    GRestorePars(dd);
    /* NOTE: only record operation if no "error"  */
    if (GRecording(call))
	recordGraphicOperation(op, originalArgs, dd);
    return R_NilValue;
}

static void drawPointsLines(double xp, double yp, double xold, double yold,
			    char type, int first, DevDesc *dd)
{
    if (type == 'p' || type == 'o')
	GSymbol(xp, yp, DEVICE, dd->gp.pch, dd);
    if ((type == 'l' || type == 'o') && !first)
	GLine(xold, yold, xp, yp, DEVICE, dd);
}

SEXP do_locator(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, y, nobs, ans, saveans, stype = R_NilValue;
    int i, n, type='p';
    double xp, yp, xold=0, yold=0;
    DevDesc *dd = CurrentDevice();

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
	    for (i=0; i<n; i++) {
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

	checkArity(op, args);
	n = asInteger(CAR(args));
	if (n <= 0 || n == NA_INTEGER)
	    error("invalid number of points in locator");
	args = CDR(args);
	if (isString(CAR(args)) && LENGTH(CAR(args)) == 1)
	    stype = CAR(args);
	else
	    errorcall(call, "invalid plot type");
	type = CHAR(STRING_ELT(stype, 0))[0];
	PROTECT(x = allocVector(REALSXP, n));
	PROTECT(y = allocVector(REALSXP, n));
	PROTECT(nobs=allocVector(INTSXP,1));
	i = 0;

	GMode(2, dd);
	while (i < n) {
	    if (!GLocator(&(REAL(x)[i]), &(REAL(y)[i]), USER, dd))
		break;
	    if (type != 'n') {
		GMode(1, dd);
		xp = REAL(x)[i];
		yp = REAL(y)[i];
		GConvert(&xp, &yp, USER, DEVICE, dd);
		drawPointsLines(xp, yp, xold, yold, type, i==0, dd);
		GMode(2, dd);
		xold = xp; yold = yp;
	    }
	    i += 1;
	}
	GMode(0, dd);
	INTEGER(nobs)[0] = i;
	while (i < n) {
	    REAL(x)[i] = NA_REAL;
	    REAL(y)[i] = NA_REAL;
	    i += 1;
	}
	PROTECT(ans = allocList(3));
	SETCAR(ans, x);
	SETCADR(ans, y);
	SETCADDR(ans, nobs);
	PROTECT(saveans = allocList(4));
	SETCAR(saveans, x);
	SETCADR(saveans, y);
	SETCADDR(saveans, nobs);
	SETCADDDR(saveans, CAR(args));
	/* Record the points and lines that were drawn in the display list */
	recordGraphicOperation(op, saveans, dd);
	UNPROTECT(5);
	return ans;
    }
}

#define THRESHOLD	0.25

static void drawLabel(double xi, double yi, int pos, double offset, char *l,
		      DevDesc *dd)
{
    switch (pos) {
    case 4:
	xi = xi+offset;
	GText(xi, yi, INCHES, l, 0.0,
	      dd->gp.yCharOffset, 0.0, dd);
	break;
    case 2:
	xi = xi-offset;
	GText(xi, yi, INCHES, l, 1.0,
	      dd->gp.yCharOffset, 0.0, dd);
	break;
    case 3:
	yi = yi+offset;
	GText(xi, yi, INCHES, l, 0.5,
	      0.0, 0.0, dd);
	break;
    case 1:
	yi = yi-offset;
	GText(xi, yi, INCHES, l, 0.5,
	      1-(0.5-dd->gp.yCharOffset),
	      0.0, dd);
    }
}

SEXP do_identify(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, x, y, l, ind, pos, Offset, saveans;
    double xi, yi, xp, yp, d, dmin, offset;
    int i, imin, k, n, npts, plot, posi;
    DevDesc *dd = CurrentDevice();

    /* If we are replaying the display list, then just redraw the
       labels beside the identified points */
    if (call == R_NilValue) {
	ind = CAR(args); args = CDR(args);
	pos = CAR(args); args = CDR(args);
	x = CAR(args); args = CDR(args);
	y = CAR(args); args = CDR(args);
	Offset = CAR(args); args = CDR(args);
	l = CAR(args);
	n = length(x);
	for (i=0; i<n; i++) {
	    plot = LOGICAL(ind)[i];
	    if (plot) {
		xi = REAL(x)[i];
		yi = REAL(y)[i];
		GConvert(&xi, &yi, USER, INCHES, dd);
		posi = INTEGER(pos)[i];
		offset = GConvertXUnits(asReal(Offset), CHARS, INCHES, dd);
		drawLabel(xi, yi, posi, offset, CHAR(STRING_ELT(l, i)), dd);
	    }
	}
	return R_NilValue;
    }
    else {
	GCheckState(dd);

	checkArity(op, args);
	x = CAR(args);
	args = CDR(args); y = CAR(args);
	args = CDR(args); l = CAR(args);
	args = CDR(args); npts = asInteger(CAR(args));
	args = CDR(args); plot = asLogical(CAR(args));
	args = CDR(args); Offset = CAR(args);
	if (npts <= 0 || npts == NA_INTEGER)
	    error("invalid number of points in identify");
	if (!isReal(x) || !isReal(y) || !isString(l) || !isReal(Offset))
	    errorcall(call, "incorrect argument type");
	if (LENGTH(x) != LENGTH(y) || LENGTH(x) != LENGTH(l))
	    errorcall(call, "different argument lengths");
	n = LENGTH(x);
	if (n <= 0) {
	    R_Visible = 0;
	    return NULL;
	}

	offset = GConvertXUnits(asReal(Offset), CHARS, INCHES, dd);
	PROTECT(ind = allocVector(LGLSXP, n));
	PROTECT(pos = allocVector(INTSXP, n));
	for (i = 0; i < n; i++)
	    LOGICAL(ind)[i] = 0;

	k = 0;
	GMode(2, dd);
	while (k < npts) {
	    if (!GLocator(&xp, &yp, INCHES, dd)) break;
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
	    if (dmin > THRESHOLD)
		REprintf("warning: no point with %.2f inches\n", THRESHOLD);
	    else if (LOGICAL(ind)[imin])
		REprintf("warning: nearest point already identified\n");
	    else {
		k++;
		LOGICAL(ind)[imin] = 1;

		xi = REAL(x)[imin];
		yi = REAL(y)[imin];
		GConvert(&xi, &yi, USER, INCHES, dd);
		if (fabs(xp-xi) >= fabs(yp-yi)) {
		    if (xp >= xi) {
			INTEGER(pos)[imin] = 4;
		    }
		    else {
			INTEGER(pos)[imin] = 2;
		    }
		}
		else {
		    if (yp >= yi) {
			INTEGER(pos)[imin] = 3;
		    }
		    else {
			INTEGER(pos)[imin] = 1;
		    }
		}
		if (plot)
		    drawLabel(xi, yi, INTEGER(pos)[imin], offset,
			      CHAR(STRING_ELT(l, imin)), dd);
	    }
	}
	GMode(0, dd);
	PROTECT(ans = allocList(2));
	SETCAR(ans, ind);
	SETCADR(ans, pos);
	PROTECT(saveans = allocList(6));
	SETCAR(saveans, ind);
	SETCADR(saveans, pos);
	SETCADDR(saveans, x);
	SETCADDDR(saveans, y);
	SETCAD4R(saveans, Offset);
	SETCAD4R(CDR(saveans), l);

	/* If we are recording, save enough information to be able to
	   redraw the text labels beside identified points */
	if (GRecording(call))
	    recordGraphicOperation(op, saveans, dd);
	UNPROTECT(4);

	return ans;
    }
}


SEXP do_dotplot(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, labs, offset, saveargs;
    double adj, xpd, wd, ht, lw, gw, xi, xmin, xmax;
#ifdef OLD
    SEXP colors, ltypes;
    double save_mar[4];
#endif
    int save_mUnits, save_defaultPlot;
    int i, n;
    DevDesc *dd;

    /* checkArity(op, args); */

    saveargs = args;
    x = CAR(args); args = CDR(args);		/* real */
    labs = CAR(args); args = CDR(args);		/* character */
    offset = CAR(args); args = CDR(args);	/* logical */
#ifdef NEW
    colors = CAR(args); args = CDR(args);	/* integer */
    ltypes = CAR(args); args = CDR(args);	/* integer */
#endif

    /* checks on lengths/types here */

    n = length(labs);

    /* compute the plot layout */

    dd = GNewPlot(GRecording(call));
    lw = 0;
    gw = 0;
    ht = GStrHeight("M", INCHES, dd);
    xmin = DBL_MAX;
    xmax = DBL_MIN;
    for (i = 0; i < n; i++) {
	wd = GStrWidth(CHAR(STRING_ELT(labs, i)), INCHES, dd) / ht;
	if (wd > 0) {
	    if (INTEGER(offset)[i] == 1) {
		if (wd > gw) gw = wd;
	    }
	    else {
		if (wd > lw) lw = wd;
	    }
	}
	xi = REAL(x)[i];
	if (R_FINITE(xi)) {
	    if (xi < xmin) xmin = xi;
	    if (xi > xmax) xmax = xi;
	}
    }
    if (gw > 0) {
	if (gw < lw + 1)
	    gw = lw + 1;
	lw = lw + 1;
	gw = gw + 1;
    }
    else {
	lw = lw + 1;
	gw = lw;
    }
    save_mUnits = dd->gp.mUnits;
    save_defaultPlot = dd->dp.defaultPlot;
    dd->gp.mar[1] = dd->gp.mar[3] + gw;
    dd->dp.mUnits = dd->gp.mUnits = LINES;
    dd->dp.defaultPlot = dd->gp.defaultPlot = 1;
    GReset(dd);

    /* Set up the plotting window */

    dd->gp.yaxs = 'i';
    GScale(xmin, xmax, 1, dd);
    GScale((double)0.5, (double)(n + 0.5), 2, dd);
    GMapWin2Fig(dd);
    GSetState(1, dd);

    /* Axis labelling must be done here. */
    /* The offsets must be recomputed */
    /* each time the plot is redrawn. */

    adj = dd->gp.adj;
    xpd = dd->gp.xpd;
    dd->gp.adj = 0;
    /* override par("xpd") and force clipping to figure region */
    /* NOTE: don't override to _reduce_ clipping region */
    if (dd->gp.xpd < 1)
	xpd = 1;

    for (i = 0; i < n; i++) {
	if (strlen(CHAR(STRING_ELT(labs, i))) > 0) {
	    if (LOGICAL(offset)[i])
		GMtext(CHAR(STRING_ELT(labs, i)), 2, gw, 0, (double)(i+1), 2, dd);
	    else
		GMtext(CHAR(STRING_ELT(labs, i)), 2, lw, 0, (double)(i+1), 2, dd);
	}
    }
    dd->gp.adj = adj;
    dd->gp.xpd = xpd;

    /* Plotting could be done here */
    /* or later in interpreted code. */

    if (GRecording(call))
	recordGraphicOperation(op, saveargs, dd);
    return R_NilValue;
}


SEXP do_strheight(SEXP call, SEXP op, SEXP args, SEXP env)
{
    /* strheight(str, units) */
    SEXP ans, str;
    int i, n, units;
    double cex, cexsave;
    DevDesc *dd = CurrentDevice();

    checkArity(op,args);
    GCheckState(dd);

    str = CAR(args);
    if ((TYPEOF(str) != STRSXP) && (TYPEOF(str) != EXPRSXP))
	errorcall(call, "character or expression first argument expected");
    args = CDR(args);

    if ((units = asInteger(CAR(args))) == NA_INTEGER || units < 0)
	errorcall(call, "invalid units");
    args = CDR(args);

    if (isNull(CAR(args)))
	cex = dd->gp.cex;
    else if (!R_FINITE(cex = asReal(CAR(args))) || cex <= 0.0)
	errorcall(call, "invalid cex value");

    n = LENGTH(str);
    PROTECT(ans = allocVector(REALSXP, n));
    cexsave = dd->gp.cex;
    dd->gp.cex = cex * dd->gp.cexbase;
    for (i = 0; i < n; i++)
	if (isExpression(str))
	    REAL(ans)[i] = GExpressionHeight(VECTOR_ELT(str, i),
					     GMapUnits(units), dd);
	else
	    REAL(ans)[i] = GStrHeight(CHAR(STRING_ELT(str, i)),
				      GMapUnits(units), dd);
    dd->gp.cex = cexsave;
    UNPROTECT(1);
    return ans;
}


SEXP do_strwidth(SEXP call, SEXP op, SEXP args, SEXP env)
{
    /* strwidth(str, units) */
    SEXP ans, str;
    int i, n, units;
    double cex, cexsave;
    DevDesc *dd = CurrentDevice();

    checkArity(op, args);
    GCheckState(dd);

    str = CAR(args);
    if ((TYPEOF(str) != STRSXP) && (TYPEOF(str) != EXPRSXP))
	errorcall(call, "character or expression first argument expected");
    args = CDR(args);

    if ((units = asInteger(CAR(args))) == NA_INTEGER || units < 0)
	errorcall(call, "invalid units");
    args = CDR(args);

    if (isNull(CAR(args)))
	cex = dd->gp.cex;
    else if (!R_FINITE(cex = asReal(CAR(args))) || cex <= 0.0)
	errorcall(call, "invalid cex value");

    n = LENGTH(str);
    PROTECT(ans = allocVector(REALSXP, n));
    cexsave = dd->gp.cex;
    dd->gp.cex = cex * dd->gp.cexbase;
    for (i = 0; i < n; i++)
	if (isExpression(str))
	    REAL(ans)[i] = GExpressionWidth(VECTOR_ELT(str, i),
					    GMapUnits(units), dd);
	else
	    REAL(ans)[i] = GStrWidth(CHAR(STRING_ELT(str, i)),
				     GMapUnits(units), dd);
    dd->gp.cex = cexsave;
    UNPROTECT(1);
    return ans;
}

static int dnd_n;
static int *dnd_lptr;
static int *dnd_rptr;
static double *dnd_hght;
static double *dnd_xpos;
static double dnd_hang;
static double dnd_offset;
static SEXP *dnd_llabels;


static void drawdend(int node, double *x, double *y, DevDesc *dd)
{
    double xl, xr, yl, yr;
    double xx[4], yy[4];
    int k;
    *y = dnd_hght[node-1];
    k = dnd_lptr[node-1];
    if (k > 0) drawdend(k, &xl, &yl, dd);
    else {
	xl = dnd_xpos[-k-1];
	if (dnd_hang >= 0) yl = *y - dnd_hang;
	else yl = 0;
	GText(xl, yl-dnd_offset, USER, CHAR(dnd_llabels[-k-1]),
	      1.0, 0.3, 90.0, dd);
    }
    k = dnd_rptr[node-1];
    if (k > 0) drawdend(k, &xr, &yr, dd);
    else {
	xr = dnd_xpos[-k-1];
	if (dnd_hang >= 0) yr = *y - dnd_hang;
	else yr = 0;
	GText(xr, yr-dnd_offset, USER, CHAR(dnd_llabels[-k-1]),
	      1.0, 0.3, 90.0, dd);
    }
    xx[0] = xl; yy[0] = yl;
    xx[1] = xl; yy[1] = *y;
    xx[2] = xr; yy[2] = *y;
    xx[3] = xr; yy[3] = yr;
    GPolyline(4, xx, yy, USER, dd);
    *x = 0.5 * (xl + xr);
}


SEXP do_dend(SEXP call, SEXP op, SEXP args, SEXP env)
{
    double x, y;
    SEXP originalArgs;
    DevDesc *dd;

    dd = CurrentDevice();
    GCheckState(dd);

    originalArgs = args;
    if (length(args) < 6)
	errorcall(call, "too few arguments");

    dnd_n = asInteger(CAR(args));
    if (dnd_n == NA_INTEGER || dnd_n < 2)
	goto badargs;
    args = CDR(args);

    if (TYPEOF(CAR(args)) != INTSXP || length(CAR(args)) != 2*dnd_n)
	goto badargs;
    dnd_lptr = &(INTEGER(CAR(args))[0]);
    dnd_rptr = &(INTEGER(CAR(args))[dnd_n]);
    args = CDR(args);

    if (TYPEOF(CAR(args)) != REALSXP || length(CAR(args)) != dnd_n)
	goto badargs;
    dnd_hght = REAL(CAR(args));
    args = CDR(args);

    if (TYPEOF(CAR(args)) != REALSXP || length(CAR(args)) != dnd_n+1)
	goto badargs;
    dnd_xpos = REAL(CAR(args));
    args = CDR(args);

    dnd_hang = asReal(CAR(args));
    if (!R_FINITE(dnd_hang))
	goto badargs;
    dnd_hang = dnd_hang * (dnd_hght[dnd_n-1] - dnd_hght[0]);
    args = CDR(args);

    if (TYPEOF(CAR(args)) != STRSXP || length(CAR(args)) != dnd_n+1)
	goto badargs;
    dnd_llabels = STRING_PTR(CAR(args));
    args = CDR(args);

    GSavePars(dd);
    RecordGraphicsCall(call);
    ProcessInlinePars(args, dd);
    dd->gp.cex = dd->gp.cexbase * dd->gp.cex;
    dnd_offset = GConvertYUnits(GStrWidth("m", INCHES, dd), INCHES, USER, dd);

    /* override par("xpd") and force clipping to figure region */
    /* NOTE: don't override to _reduce_ clipping region */
    if (dd->gp.xpd < 1)
	dd->gp.xpd = 1;

    GMode(1, dd);
    drawdend(dnd_n, &x, &y, dd);
    GMode(0, dd);
    GRestorePars(dd);
    /* NOTE: only record operation if no "error"  */
    if (GRecording(call))
	recordGraphicOperation(op, originalArgs, dd);
    return R_NilValue;

  badargs:
    error("invalid dendrogram input");
    return R_NilValue;/* never used; to keep -Wall happy */
}

SEXP do_dendwindow(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i, imax, n;
    double pin, *ll, tmp, yval, *y, ymin, ymax, yrange;
    SEXP originalArgs, merge, height, llabels;
    char *vmax;
    DevDesc *dd;
    dd = CurrentDevice();
    GCheckState(dd);
    originalArgs = args;
    if (length(args) < 6)
	errorcall(call, "too few arguments");
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
    if (TYPEOF(CAR(args)) != REALSXP || length(CAR(args)) != n + 1)
	goto badargs;
    dnd_xpos = REAL(CAR(args));
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
    RecordGraphicsCall(call);
    ProcessInlinePars(args, dd);
    dd->gp.cex = dd->gp.cexbase * dd->gp.cex;
    dnd_offset = GStrWidth("m", INCHES, dd);
    vmax = vmaxget();
    y =	 (double*)R_alloc(n, sizeof(double));
    ll =  (double*)R_alloc(n, sizeof(double));
    dnd_lptr = &(INTEGER(merge)[0]);
    dnd_rptr = &(INTEGER(merge)[n]);
    ymin = REAL(height)[0];
    ymax = REAL(height)[n - 1];
    pin = dd->gp.pin[1];
    for (i = 0; i < n; i++)
	ll[i] = GStrWidth(CHAR(STRING_ELT(llabels, i)), INCHES, dd)
	    + dnd_offset;
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
	imax = -1;
	yval = -DBL_MAX;
	for (i = 0; i < n; i++) {
	    tmp = ((ymax - y[i]) / yrange) * pin + ll[i];
	    if (tmp > yval) {
		yval = tmp;
		imax = i;
	    }
	}
    }
    else {
	ymin = 0;
	yrange = ymax;
	imax = -1;
	yval = -DBL_MAX;
	for (i = 0; i < n; i++) {
	    tmp = pin + ll[i];
	    if (tmp > yval) {
		yval = tmp;
		imax = i;
	    }
	}
    }
    /* now determine how much to scale */
    ymin = ymax - (pin/(pin - ll[imax])) * (ymax - ymin);
    GScale(1.0, n+1.0, 1, dd);
    GScale(ymin, ymax, 2, dd);
    GMapWin2Fig(dd);
    GRestorePars(dd);
    /* NOTE: only record operation if no "error"  */
    if (GRecording(call))
	recordGraphicOperation(op, originalArgs, dd);
    vmaxset(vmax);
    return R_NilValue;
  badargs:
    error("invalid dendrogram input");
    return R_NilValue;/* never used; to keep -Wall happy */
}


SEXP do_erase(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP col;
    int ncol;
    DevDesc *dd = CurrentDevice();
    checkArity(op, args);
    PROTECT(col = FixupCol(CAR(args), NA_INTEGER));
    ncol = LENGTH(col);
    GSavePars(dd);
    GMode(1, dd);
    GRect(0.0, 0.0, 1.0, 1.0, NDC, INTEGER(col)[0], NA_INTEGER, dd);
    GMode(0, dd);
    GRestorePars(dd);
    UNPROTECT(1);
    return R_NilValue;
}


SEXP do_replay(SEXP call, SEXP op, SEXP args, SEXP env)
{
    DevDesc *dd = CurrentDevice();
    checkArity(op, args);
    dd->dp.resize();
    playDisplayList(dd);
    return R_NilValue;
}

SEXP do_getDL(SEXP call, SEXP op, SEXP args, SEXP env)
{
    DevDesc *dd = CurrentDevice();
    checkArity(op, args);
    return dd->displayList;
}

SEXP do_playDL(SEXP call, SEXP op, SEXP args, SEXP env)
{
    DevDesc *dd = CurrentDevice();
    SEXP theList;
    int ask;

    checkArity(op, args);
    if(!isList(theList = CAR(args)))
       errorcall(call, "invalid argument");
    dd->displayList = theList;
    if (theList != R_NilValue) {
	ask = dd->gp.ask;
	dd->gp.ask = 1;
	GReset(dd);
	while (theList != R_NilValue) {
	    SEXP theOperation = CAR(theList);
	    SEXP l_op = CAR(theOperation);
	    SEXP l_args = CDR(theOperation);
	    PRIMFUN(l_op) (R_NilValue, l_op, l_args, R_NilValue);
            if (!dd->gp.valid) break;
	    theList = CDR(theList);
	}
	dd->gp.ask = ask;
    }
    return R_NilValue;
}

SEXP do_getGPar(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP GP;
    int lGPar = 1 + sizeof(GPar) / sizeof(int);
    DevDesc *dd = CurrentDevice();

    checkArity(op, args);
    GP = allocVector(INTSXP, lGPar);
    copyGPar(&dd->dpSaved, (GPar *) INTEGER(GP));
    return GP;
}

SEXP do_setGPar(SEXP call, SEXP op, SEXP args, SEXP env)
{
    DevDesc *dd = CurrentDevice();
    int lGPar = 1 + sizeof(GPar) / sizeof(int);
    SEXP GP;

    checkArity(op, args);
    GP = CAR(args);
    if (!isInteger(GP) || length(GP) != lGPar)
	errorcall(call, "invalid graphics parameter list");
    copyGPar((GPar *) INTEGER(GP), &dd->dpSaved);
    return R_NilValue;
}

static void SymbolSize(double *x, int n, double *xmax, double *xmin)
{
    int i;
    *xmax = -DBL_MAX;
    *xmin =  DBL_MAX;
    for(i = 0; i < n; i++)
        if (R_FINITE(x[i])) {
	    if (*xmax < x[i]) *xmax = x[i];
	    if (*xmin > x[i]) *xmin = x[i];
        }
}

static void CheckSymbolPar(SEXP call, SEXP p, int *nr, int *nc)
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
	errorcall(call, "invalid symbol parameter vector");
}

SEXP do_symbols(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, y, p, fg, bg;
    double pmax, pmin, inches, rx, ry;
    int i, j, nr, nc, nbg, nfg, type;
    double xx, yy, p0, p1, p2, p3, p4;
    double *pp, *xp, *yp;
    char *vmax;
    int units;

    SEXP originalArgs = args;
    DevDesc *dd = CurrentDevice();
    GCheckState(dd);

    if (length(args) < 7)
	errorcall(call, "insufficient arguments");

    PROTECT(x = coerceVector(CAR(args), REALSXP)); args = CDR(args);
    PROTECT(y = coerceVector(CAR(args), REALSXP)); args = CDR(args);
    if (!isNumeric(x) || !isNumeric(y) || length(x) <= 0 || LENGTH(x) <= 0)
        errorcall(call, "invalid symbol coordinates");

    type = asInteger(CAR(args)); args = CDR(args);

    p = PROTECT(coerceVector(CAR(args), REALSXP)); args = CDR(args);
    CheckSymbolPar(call, p, &nr, &nc);
    if (LENGTH(x) != nr || LENGTH(y) != nr)
	errorcall(call, "x/y/parameter length mismatch");

    inches = asReal(CAR(args)); args = CDR(args);
    if (!R_FINITE(inches) || inches < 0)
	inches = 0;

    PROTECT(bg = FixupCol(CAR(args), NA_INTEGER)); args = CDR(args);
    nbg = LENGTH(bg);

    PROTECT(fg = FixupCol(CAR(args), NA_INTEGER)); args = CDR(args);
    nfg = LENGTH(fg);

    GSavePars(dd);
    ProcessInlinePars(args, dd);

    GMode(1, dd);
    switch (type) {
    case 1: /* circles */
	if (nc != 1)
	    errorcall(call, "invalid circle data");
	SymbolSize(REAL(p), nr, &pmax, &pmin);
	if (pmin > pmin || pmin < 0)
	    errorcall(call, "invalid symbol parameter");
	for (i = 0; i < nr; i++) {
	    if (R_FINITE(REAL(x)[i]) && R_FINITE(REAL(y)[i])
		&& R_FINITE(REAL(p)[i])) {
		rx = REAL(p)[i];
		if (inches > 0)
		    rx = (rx / pmax) * inches;
		else
		    rx = GConvertXUnits(rx, USER, INCHES, dd);
		GCircle(REAL(x)[i], REAL(y)[i],	USER, rx,
			INTEGER(bg)[i%nbg], INTEGER(fg)[i%nfg],	dd);
	    }
	}
	break;
    case 2: /* squares */
	if (nc != 1)
	    errorcall(call, "invalid square data");
	SymbolSize(REAL(p), nr, &pmax, &pmin);
	if (pmin > pmin || pmin < 0)
	    errorcall(call, "invalid symbol parameter");
	units = DEVICE;
	for (i = 0; i < nr; i++) {
	    if (R_FINITE(REAL(x)[i]) && R_FINITE(REAL(y)[i])
		&& R_FINITE(REAL(p)[i])) {
		p0 = REAL(p)[i];
		xx = REAL(x)[i];
		yy = REAL(y)[i];
		GConvert(&xx, &yy, USER, units, dd);
		if (inches > 0) {
		    p0 = p0 / pmax * inches;
		    rx = GConvertXUnits(0.5 * p0, INCHES, units, dd);
		    ry = GConvertYUnits(0.5 * p0, INCHES, units, dd);
		}
		else {
		    rx = GConvertXUnits(0.5 * p0, USER, units, dd);
		    ry = GConvertYUnits(0.5 * p0, USER, units, dd);
		}
		GRect(xx - rx, yy - rx, xx + rx, yy + rx, units,
		      INTEGER(bg)[i%nbg], INTEGER(fg)[i%nfg], dd);

	    }
	}
	break;
    case 3: /* rectangles */
	if (nc != 2)
	    errorcall(call, "invalid square data");
	SymbolSize(REAL(p), 2 * nr, &pmax, &pmin);
	if (pmin > pmax || pmin < 0)
	    errorcall(call, "invalid symbol parameter");
	for (i = 0; i < nr; i++) {
	    if (R_FINITE(REAL(x)[i]) && R_FINITE(REAL(y)[i])
		&& R_FINITE(REAL(p)[i]) && R_FINITE(REAL(p)[i+nr])) {
		xx = REAL(x)[i];
		yy = REAL(y)[i];
		GConvert(&xx, &yy, USER, DEVICE, dd);
		p0 = REAL(p)[i];
		p1 = REAL(p)[i+nr];
		if (inches > 0) {
		    p0 = p0 / pmax * inches;
		    p1 = p1 / pmax * inches;
		    rx = GConvertXUnits(0.5 * p0, INCHES, DEVICE, dd);
		    ry = GConvertYUnits(0.5 * p1, INCHES, DEVICE, dd);
		}
		else {
		    rx = GConvertXUnits(0.5 * p0, USER, DEVICE, dd);
		    ry = GConvertYUnits(0.5 * p1, USER, DEVICE, dd);
		}
		GRect(xx - rx, yy - ry, xx + rx, yy + ry, DEVICE,
		      INTEGER(bg)[i%nbg], INTEGER(fg)[i%nfg], dd);

	    }
	}
	break;
    case 4: /* stars */
	if (nc < 3)
	    errorcall(call, "invalid stars data");
	SymbolSize(REAL(p), nc, &pmax, &pmin);
	if (pmin > pmax || pmin < 0)
	    errorcall(call, "invalid symbol parameter");
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
			pp[j] =  GConvertXUnits(p0, USER, INCHES, dd);
		    }
		}
		for(j = 0; j < nc; j++) {
		    xp[j] = GConvertXUnits(pp[j] * cos(j * p1),
					   INCHES, NDC, dd) + xx;
		    yp[j] = GConvertYUnits(pp[j] * sin(j * p1),
					   INCHES, NDC, dd) + yy;
		}
		GPolygon(nc, xp, yp, NDC,
			 INTEGER(bg)[i%nbg], INTEGER(fg)[i%nfg], dd);
	    }
	}
	vmaxset(vmax);
	break;
    case 5: /* thermometers */
	if (nc != 3 && nc != 4)
	    errorcall(call, "invalid thermometer data");
	SymbolSize(REAL(p), 2 * nr, &pmax, &pmin);
	if (pmin > pmax || pmin < 0)
	    errorcall(call, "invalid symbol parameter");
	for (i = 0; i < nr; i++) {
	    xx = REAL(x)[i];
	    yy = REAL(y)[i];
	    if (R_FINITE(xx) && R_FINITE(yy)) {
		p0 = REAL(p)[i];
		p1 = REAL(p)[i + nr];
		p2 = REAL(p)[i + 2 * nr];
		if (nc == 4)
		    p3 = REAL(p)[i + 3 * nr];
		else
		    p3 = 0;
		if (R_FINITE(p0) && R_FINITE(p0)
		    && R_FINITE(p0) && R_FINITE(p0)) {
		    if (p2 < 0) p2 = 0;
		    if (p2 > 1) p2 = 1;
		    if (p3 < 0) p3 = 0;
		    if (p3 > 1) p3 = 1;
		    GConvert(&xx, &yy, USER, NDC, dd);
		    if (inches > 0) {
			p0 = p0 / pmax * inches;
			p1 = p1 / pmax * inches;
			rx = GConvertXUnits(0.5 * p0, INCHES, NDC, dd);
			ry = GConvertYUnits(0.5 * p1, INCHES, NDC, dd);
		    }
		    else {
			rx = GConvertXUnits(0.5 * p0, USER, NDC, dd);
			ry = GConvertYUnits(0.5 * p1, USER, NDC, dd);
		    }
		    GRect(xx - rx, yy - ry, xx + rx, yy + ry, NDC,
			  INTEGER(bg)[i%nbg], INTEGER(fg)[i%nfg], dd);
		    GRect(xx - rx,  yy - (1 - 2 * p2) * ry,
			  xx + rx,  yy - (1 - 2 * p3) * ry,
			  NDC,
			  INTEGER(fg)[i%nfg], INTEGER(fg)[i%nfg], dd);
		    GLine(xx - rx, yy, xx - 1.5 * rx, yy, NDC, dd);
		    GLine(xx + rx, yy, xx + 1.5 * rx, yy, NDC, dd);

		}
	    }
	}
	break;
    case 6: /* boxplots */
	if (nc != 5)
	    errorcall(call, "invalid boxplot data");
	pmax = -DBL_MAX;
	pmin =  DBL_MAX;
	for(i = 0; i < nr; i++) {
	    p1 = REAL(p)[i + nr];
	    p3 = REAL(p)[i + 3 * nr];
	    p4 = REAL(p)[i + 4 * nr];
	    if (R_FINITE(p1) && R_FINITE(p3) && R_FINITE(p4)) {
		p0 = p1 + p3 + p4;
		if (pmax < p0) pmax = p0;
		if (pmin > p0) pmin = p0;
	    }
	}
	if (pmin > pmax)
	    errorcall(call, "invalid symbol parameter");
	for (i = 0; i < nr; i++) {
	    xx = REAL(x)[i];
	    yy = REAL(y)[i];
	    if (R_FINITE(xx) && R_FINITE(yy)) {
		p0 = REAL(p)[i];
		p1 = REAL(p)[i + nr];
		p2 = REAL(p)[i + 2 * nr];
		p3 = REAL(p)[i + 3 * nr];
		p4 = REAL(p)[i + 4 * nr];
		if (R_FINITE(p0) && R_FINITE(p1)
		        && R_FINITE(p2) && R_FINITE(p3) && R_FINITE(p4)) {
		    GConvert(&xx, &yy, USER, NDC, dd);
		    if (inches > 0) {
			p0 = (p0 / pmax) * inches;
			p1 = (p1 / pmax) * inches;
			p2 = (p2 / pmax) * inches;
			p3 = (p2 / pmax) * inches;
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
			  INTEGER(bg)[i%nbg], INTEGER(fg)[i%nfg], dd);
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
	errorcall(call, "invalid symbol type");
    }
    GMode(0, dd);
    GRestorePars(dd);
    if (GRecording(call))
	recordGraphicOperation(op, originalArgs, dd);
    UNPROTECT(5);
    return R_NilValue;
}
