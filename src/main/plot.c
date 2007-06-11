/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2007  Robert Gentleman, Ross Ihaka and the
 *			      R Development Core Team
 *  Copyright (C) 2002--2004  The R Foundation
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

/* <UTF8> char here is either ASCII or handled as a whole */

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

/* FIXME:  NewFrameConfirm should be a standard device function */
#ifdef Win32
Rboolean winNewFrameConfirm(void);
#endif

void NewFrameConfirm(void)
{
    unsigned char buf[16];
#ifdef Win32
	int i;
	Rboolean haveWindowsDevice;
	SEXP dotDevices = findVar(install(".Devices"), R_BaseEnv); /* This is a pairlist! */
#endif

    if(!R_Interactive) return;
#ifdef Win32
    for(i = 0; i < curDevice(); i++)  /* 0-based */
	dotDevices = CDR(dotDevices);
    haveWindowsDevice =
	strcmp(CHAR(STRING_ELT(CAR(dotDevices), 0)), "windows") == 0;
    
    if (!haveWindowsDevice || !winNewFrameConfirm())
#endif
	R_ReadConsole(_("Hit <Return> to see next plot: "), buf, 16, 0);
}

	/* Remember: +1 and/or -1 because C arrays are */
	/* zero-based and R-vectors are one-based. */

#define checkArity_length					\
    checkArity(op, args);					\
    if(!LENGTH(CAR(args)))					\
	errorcall(call, _("argument must have positive length"))


SEXP attribute_hidden do_devcontrol(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int listFlag;

    checkArity(op, args);
    if(PRIMVAL(op) == 0) { /* dev.control */
	listFlag = asLogical(CAR(args));
	if(listFlag == NA_LOGICAL) errorcall(call, _("invalid argument"));
	if(listFlag)
	    enableDisplayList(CurrentDevice());
	else
	    inhibitDisplayList(CurrentDevice());
    } else { /* dev.displaylist */
	GEDevDesc *dd = (GEDevDesc*)CurrentDevice();
	listFlag = dd->dev->displayListOn;
    }
    return ScalarLogical(listFlag);
}

SEXP attribute_hidden do_devcopy(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity_length;
    GEcopyDisplayList(INTEGER(CAR(args))[0] - 1);
    return R_NilValue;
}

SEXP attribute_hidden do_devcur(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP cd = allocVector(INTSXP, 1);
    checkArity(op, args);
    INTEGER(cd)[0] = curDevice() + 1;
    return cd;
}

SEXP attribute_hidden do_devnext(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP nd = allocVector(INTSXP, 1);
    checkArity_length;
    INTEGER(nd)[0] = nextDevice(INTEGER(CAR(args))[0] - 1) + 1;
    return nd;
}

SEXP attribute_hidden do_devprev(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pd = allocVector(INTSXP, 1);
    checkArity_length;
    INTEGER(pd)[0] = prevDevice(INTEGER(CAR(args))[0] - 1) + 1;
    return pd;
}

SEXP attribute_hidden do_devset(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int devNum = INTEGER(CAR(args))[0] - 1;
    SEXP sd = allocVector(INTSXP, 1);
    checkArity(op, args);
    INTEGER(sd)[0] = selectDevice(devNum) + 1;
    return sd;
}

SEXP attribute_hidden do_devoff(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity_length;
    killDevice(INTEGER(CAR(args))[0] - 1);
    return R_NilValue;
}


/*  P A R A M E T E R	 U T I L I T I E S  */


/* ProcessInLinePars handles inline par specifications in graphics functions.
 * It does this by calling Specify2() from ./par.c */

attribute_hidden
void ProcessInlinePars(SEXP s, DevDesc *dd, SEXP call)
{
    if (isList(s)) {
	while (s != R_NilValue) {
	    if (isList(CAR(s)))
		ProcessInlinePars(CAR(s), dd, call);
	    else if (TAG(s) != R_NilValue)
		Specify2(CHAR(PRINTNAME(TAG(s))), CAR(s), dd, call);
	    s = CDR(s);
	}
    }
}

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

attribute_hidden
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
	for (i = 0; pch != R_NilValue;	pch = CDR(pch))
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
	for (i = 0; i < n; i++) {
	    if(STRING_ELT(pch, i) == NA_STRING ||
	       CHAR(STRING_ELT(pch, i))[0] == '\0') { /* pch = "" */
		INTEGER(ans)[i] = NA_INTEGER;
	    } else {
#ifdef SUPPORT_MBCS
		if(mbcslocale) {
		    wchar_t wc;
		    if(mbrtowc(&wc, translateChar(STRING_ELT(pch, i)),
			       MB_CUR_MAX, NULL) > 0) INTEGER(ans)[i] = wc;
		    else
			error(_("invalid multibyte char in pch=\"c\""));
		} else
#endif
		    INTEGER(ans)[i] = translateChar(STRING_ELT(pch, i))[0];
	    }
	}
    }
    else if (isLogical(pch)) {/* NA, but not TRUE/FALSE */
	ans = allocVector(INTSXP, n);
	for (i = 0; i < n; i++)
	    if(LOGICAL(pch)[i] == NA_LOGICAL)
		INTEGER(ans)[i] = NA_INTEGER;
	    else error(_("only NA allowed in logical plotting symbol"));
    }
    else error(_("invalid plotting symbol"));
    for (i = 0; i < n; i++) {
	if (INTEGER(ans)[i] < 0 && INTEGER(ans)[i] != NA_INTEGER)
	    INTEGER(ans)[i] = dflt;
    }
    return ans;
}

attribute_hidden
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

attribute_hidden
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

attribute_hidden
SEXP FixupFont(SEXP font, int dflt)
{
    int i, k, n;
    SEXP ans = R_NilValue;/* -Wall*/
    n = length(font);
    if (n == 0) {
	ans = allocVector(INTSXP, 1);
	INTEGER(ans)[0] = dflt;
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
	    k = REAL(font)[i];
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

attribute_hidden
SEXP FixupCol(SEXP col, unsigned int dflt)
{
    int i, n;
    SEXP ans;
    n = length(col);
    if (n == 0) {
	PROTECT(ans = allocVector(INTSXP, 1));
	INTEGER(ans)[0] = dflt;
    }
    else {
	ans = PROTECT(allocVector(INTSXP, n));
	if (isList(col))
	    for (i = 0; i < n; i++) {
		INTEGER(ans)[i] = RGBpar(CAR(col), 0);
		col = CDR(col);
	    }
	else
	    for (i = 0; i < n; i++)
		INTEGER(ans)[i] = RGBpar(col, i);
    }
    UNPROTECT(1);
    return ans;
}

attribute_hidden
SEXP FixupCex(SEXP cex, double dflt)
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
    }
    return ans;
}

attribute_hidden
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
	if (typeface < 0 || typeface > 7)
	    error(_("invalid 'vfont' value [typeface]"));
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
	    error(_("invalid 'vfont' value [fontindex]"));
	ans = allocVector(INTSXP, 2);
	for (i=0; i<2; i++)
	    INTEGER(ans)[i] = INTEGER(vf)[i];
	UNPROTECT(1);
    }
    return ans;
}

/* GetTextArg() : extract from call and possibly set text arguments
 *  ("label", col=, cex=, font=)
 *
 * Main purpose: Treat things like  title(main = list("This Title", font= 4))
 *
 * Called from	do_title()  [only, currently]
 */
static void
GetTextArg(SEXP call, SEXP spec, SEXP *ptxt,
	   int *pcol, double *pcex, int *pfont)
{
    int i, n, col, font, colspecd;
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
		else errorcall(call, _("invalid graphics parameter"));
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


SEXP attribute_hidden do_plot_new(SEXP call, SEXP op, SEXP args, SEXP env)
{
    /* plot.new() - create a new plot "frame" */

    DevDesc *dd;

    checkArity(op, args);

    dd = CurrentDevice();
    /*
     * If user is prompted before new page, user has opportunity
     * to kill current device.  GNewPlot returns (potentially new)
     * current device.
     */
    dd = GNewPlot(GRecording(call, dd));

    Rf_dpptr(dd)->xlog = Rf_gpptr(dd)->xlog = FALSE;
    Rf_dpptr(dd)->ylog = Rf_gpptr(dd)->ylog = FALSE;

    GScale(0.0, 1.0, 1, dd);
    GScale(0.0, 1.0, 2, dd);
    GMapWin2Fig(dd);
    GSetState(1, dd);

    if (GRecording(call, dd))
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

SEXP attribute_hidden do_plot_window(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP xlim, ylim, logarg;
    double asp, xmin, xmax, ymin, ymax;
    Rboolean logscale;
    char *p;
    SEXP originalArgs = args;
    DevDesc *dd = CurrentDevice();

    if (length(args) < 3)
	errorcall(call, _("at least 3 arguments required"));

    xlim = CAR(args);
    if (!isNumeric(xlim) || LENGTH(xlim) != 2)
	errorcall(call, _("invalid '%s' value"), "xlim");
    args = CDR(args);

    ylim = CAR(args);
    if (!isNumeric(ylim) || LENGTH(ylim) != 2)
	errorcall(call, _("invalid '%s' value"), "ylim");
    args = CDR(args);

    logscale = FALSE;
    logarg = CAR(args);
    if (!isString(logarg))
	errorcall(call, _("\"log=\" specification must be character"));
    p = CHAR(STRING_ELT(logarg, 0));
    while (*p) {
	switch (*p) {
	case 'x':
	    Rf_dpptr(dd)->xlog = Rf_gpptr(dd)->xlog = logscale = TRUE;
	    break;
	case 'y':
	    Rf_dpptr(dd)->ylog = Rf_gpptr(dd)->ylog = logscale = TRUE;
	    break;
	default:
	    errorcall(call, _("invalid \"log=%s\" specification"), p);
	}
	p++;
    }
    args = CDR(args);

    asp = (logscale) ? NA_REAL : asReal(CAR(args));;
    args = CDR(args);

    /* This reads [xy]axs and lab, used in GScale */
    GSavePars(dd);
    ProcessInlinePars(args, dd, call);

    if (isInteger(xlim)) {
	if (INTEGER(xlim)[0] == NA_INTEGER || INTEGER(xlim)[1] == NA_INTEGER)
	    errorcall(call, _("NAs not allowed in 'xlim'"));
	xmin = INTEGER(xlim)[0];
	xmax = INTEGER(xlim)[1];
    }
    else {
	if (!R_FINITE(REAL(xlim)[0]) || !R_FINITE(REAL(xlim)[1]))
	    errorcall(call, _("need finite 'xlim' values"));
	xmin = REAL(xlim)[0];
	xmax = REAL(xlim)[1];
    }
    if (isInteger(ylim)) {
	if (INTEGER(ylim)[0] == NA_INTEGER || INTEGER(ylim)[1] == NA_INTEGER)
	    errorcall(call, _("NAs not allowed in 'ylim'"));
	ymin = INTEGER(ylim)[0];
	ymax = INTEGER(ylim)[1];
    }
    else {
	if (!R_FINITE(REAL(ylim)[0]) || !R_FINITE(REAL(ylim)[1]))
	    errorcall(call, _("need finite 'ylim' values"));
	ymin = REAL(ylim)[0];
	ymax = REAL(ylim)[1];
    }
    if ((Rf_dpptr(dd)->xlog && (xmin < 0 || xmax < 0)) ||
       (Rf_dpptr(dd)->ylog && (ymin < 0 || ymax < 0)))
	    errorcall(call, _("Logarithmic axis must have positive limits"));

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

    /* NOTE: the operation is only recorded if there was no "error" */
    if (GRecording(call, dd))
	recordGraphicOperation(op, originalArgs, dd);
    return R_NilValue;
}

void GetAxisLimits(double left, double right, double *low, double *high)
{
/*	Called from do_axis()	such as
 *	GetAxisLimits(Rf_gpptr(dd)->usr[0], Rf_gpptr(dd)->usr[1], &low, &high)
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

SEXP attribute_hidden labelformat(SEXP labels)
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
	formatReal(REAL(labels), n, &w, &d, &e, 0);
	PROTECT(ans = allocVector(STRSXP, n));
	for (i = 0; i < n; i++) {
	    strp = EncodeReal(REAL(labels)[i], 0, d, e, OutDec);
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

SEXP CreateAtVector(double *axp, double *usr, int nint, Rboolean logflag)
{
/*	Create an  'at = ...' vector for  axis(.) / do_axis,
 *	i.e., the vector of tick mark locations,
 *	when none has been specified (= default).
 *
 *	axp[0:2] = (x1, x2, nInt), where x1..x2 are the extreme tick marks
 *		   {unless in log case, where nint \in {1,2,3 ; -1,-2,....}
 *		    and the `nint' argument is used.}

 *	The resulting REAL vector must have length >= 1, ideally >= 2
 */
    SEXP at = R_NilValue;/* -Wall*/
    double umin, umax, dn, rng, small;
    int i, n, ne;
    if (!logflag || axp[2] < 0) { /* --- linear axis --- Only use axp[] arg. */
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
	Rboolean reversed = FALSE;

	n = (axp[2] + 0.5);
	/* {xy}axp[2] for 'log': GLpretty() [./graphics.c] sets
	   n < 0: very small scale ==> linear axis, above, or
	   n = 1,2,3.  see switch() below */
	umin = usr[0];
	umax = usr[1];
	if (umin > umax) {
	    reversed = (axp[0] > axp[1]);
	    if (reversed) {
		/* have *reversed* log axis -- whereas
		 * the switch(n) { .. } below assumes *increasing* values
		 * --> reverse axis direction here, and reverse back at end */
		umin = usr[1];
		umax = usr[0];
		dn = axp[0]; axp[0] = axp[1]; axp[1] = dn;
	    }
	    else {
		/* can the following still happen... ? */
		warning("CreateAtVector \"log\"(from axis()): "
			"usr[0] = %g > %g = usr[1] !", umin, umax);
	    }
	}

	dn = axp[0];
	if (dn < DBL_MIN) {/* was 1e-300; now seems too cautious */
	    warning("CreateAtVector \"log\"(from axis()): axp[0] = %g !", dn);
	    if (dn <= 0) /* real trouble (once for Solaris) later on */
		error("CreateAtVector [log-axis()]: axp[0] = %g < 0!", dn);
	}

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
		      "invalid {xy}axp or par; nint=%d\n"
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
		      "invalid {xy}axp or par;\n"
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
		      "invalid {xy}axp or par;\n"
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
	    error("log - axis(), 'at' creation: INVALID {xy}axp[3] = %g",
		  axp[2]);
	}

	if (reversed) {/* reverse back again - last assignment was at[n++]= . */
	    for (i = 0; i < n/2; i++) { /* swap( at[i], at[n-i-1] ) : */
		dn = REAL(at)[i];
		REAL(at)[i] = REAL(at)[n-i-1];
		REAL(at)[n-i-1] = dn;
	    }
	}
    } /* linear / log */
    return at;
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

SEXP attribute_hidden do_axis(SEXP call, SEXP op, SEXP args, SEXP env)
{
    /* axis(side, at, labels, tick, line, pos,
     *	    outer, font, lty, lwd, col, padj, ...) */

    SEXP at, lab, padj;
    int col, font, lty, npadj;
    int i, n, nint = 0, ntmp, side, *ind, outer, lineoff = 0;
    int istart, iend, incr;
    Rboolean dolabels, doticks, logflag = FALSE;
    Rboolean create_at;
    double x, y, temp, tnew, tlast;
    double axp[3], usr[2];
    double gap, labw, low, high, line, pos, lwd, hadj;
    double axis_base, axis_tick, axis_lab, axis_low, axis_high;

    SEXP originalArgs = args, label;
    DevDesc *dd = CurrentDevice();

    /* Arity Check */
    /* This is a builtin function, so it should always have */
    /* the correct arity, but it doesn't hurt to be defensive. */

    if (length(args) < 12)
	errorcall(call, _("too few arguments"));
    GCheckState(dd);

    /* Required argument: "side" */
    /* Which side of the plot the axis is to appear on. */
    /* side = 1 | 2 | 3 | 4. */

    side = asInteger(CAR(args));
    if (side < 1 || side > 4)
	errorcall(call, _("invalid axis number %d"), side);
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
    if (!R_FINITE(line)) {
	/* Except that here mgp values are not relative to themselves */
	line = Rf_gpptr(dd)->mgp[2];
	lineoff = line;
    }
    args = CDR(args);

    /* Optional argument: "pos" */
    /* Specifies a user coordinate at which the axis should be drawn. */
    /* This overrides the value of "line".  Again the "mgp" par values */
    /* are interpreted relative to this value. */

    pos = asReal(CAR(args));
    if (!R_FINITE(pos)) pos = NA_REAL; else lineoff = 0;
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
    lty = asInteger(FixupLty(CAR(args), NA_INTEGER));
    args = CDR(args);

    /* Optional argument: "lwd" */
    lwd = asReal(FixupLwd(CAR(args), 1));
    args = CDR(args);

    /* Optional argument: "col" */
    col = asInteger(FixupCol(CAR(args), Rf_gpptr(dd)->fg));
    args = CDR(args);

    /* Optional argument: "hadj" */
    if (length(CAR(args)) != 1)
	errorcall(call, _("'hadj' must be of length one"));
    hadj = asReal(CAR(args));
    args = CDR(args);

    /* Optional argument: "padj" */
    PROTECT(padj = coerceVector(CAR(args), REALSXP));
    npadj = length(padj);
    if (npadj <= 0) errorcall(call, _("zero length 'padj' specified"));

    /* Now we process all the remaining inline par values:
       we need to do it now as x/yaxp are retrieved next.
       That will set Rf_gpptr, so we update that first - do_plotwindow
       clobbered the Rf_gpptr settings. */
    GSavePars(dd);
    Rf_gpptr(dd)->xaxp[0] = Rf_dpptr(dd)->xaxp[0];
    Rf_gpptr(dd)->xaxp[1] = Rf_dpptr(dd)->xaxp[1];
    Rf_gpptr(dd)->xaxp[2] = Rf_dpptr(dd)->xaxp[2];
    Rf_gpptr(dd)->yaxp[0] = Rf_dpptr(dd)->yaxp[0];
    Rf_gpptr(dd)->yaxp[1] = Rf_dpptr(dd)->yaxp[1];
    Rf_gpptr(dd)->yaxp[2] = Rf_dpptr(dd)->yaxp[2];
    ProcessInlinePars(args, dd, call);

    /* Retrieve relevant "par" values. */

    switch(side) {
    case 1:
    case 3:
	axp[0] = Rf_gpptr(dd)->xaxp[0];
	axp[1] = Rf_gpptr(dd)->xaxp[1];
	axp[2] = Rf_gpptr(dd)->xaxp[2];
	usr[0] = Rf_dpptr(dd)->usr[0];
	usr[1] = Rf_dpptr(dd)->usr[1];
	logflag = Rf_dpptr(dd)->xlog;
	nint = Rf_dpptr(dd)->lab[0];
	break;
    case 2:
    case 4:
	axp[0] = Rf_gpptr(dd)->yaxp[0];
	axp[1] = Rf_gpptr(dd)->yaxp[1];
	axp[2] = Rf_gpptr(dd)->yaxp[2];
	usr[0] = Rf_dpptr(dd)->usr[2];
	usr[1] = Rf_dpptr(dd)->usr[3];
	logflag = Rf_dpptr(dd)->ylog;
	nint = Rf_dpptr(dd)->lab[1];
	break;
    }

    /* Determine the tickmark positions.  Note that these may fall */
    /* outside the plot window. We will clip them in the code below. */

    create_at = (length(at) == 0);
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
		errorcall(call, _("'labels' is supplied and not 'at'"));
	    if (!isExpression(lab)) lab = labelformat(lab);
	}
	if (length(at) != length(lab))
	    errorcall(call, _("'at' and 'labels' lengths differ, %d != %d"),
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
    n = ntmp;
    if (n == 0)
	errorcall(call, _("no locations are finite"));

    /* Ok, all systems are "GO".  Let's get to it. */

    /* At this point we know the value of "xaxt" and "yaxt",
     * so we test to see whether the relevant one is "n".
     * If it is, we just bail out at this point. */

    if (((side == 1 || side == 3) && Rf_gpptr(dd)->xaxt == 'n') ||
	((side == 2 || side == 4) && Rf_gpptr(dd)->yaxt == 'n')) {
	GRestorePars(dd);
	UNPROTECT(4);
	return R_NilValue;
    }


    /* no! we do allow an `lty' argument -- will not be used often though
     *	Rf_gpptr(dd)->lty = LTY_SOLID; */
    Rf_gpptr(dd)->lty = lty;
    Rf_gpptr(dd)->lwd = lwd;

    /* Override par("xpd") and force clipping to device region. */
    Rf_gpptr(dd)->xpd = 2;

    Rf_gpptr(dd)->adj = R_FINITE(hadj) ? hadj : 0.5;
    Rf_gpptr(dd)->font = (font == NA_INTEGER)? Rf_gpptr(dd)->fontaxis : font;
    Rf_gpptr(dd)->cex = Rf_gpptr(dd)->cexbase * Rf_gpptr(dd)->cexaxis;
    /* no!   col = Rf_gpptr(dd)->col; */

    /* Draw the axis */
    GMode(1, dd);
    switch (side) {
    case 1: /*--- x-axis -- horizontal --- */
    case 3:
	GetAxisLimits(Rf_gpptr(dd)->usr[0], Rf_gpptr(dd)->usr[1], &low, &high);
	axis_low  = GConvertX(fmax2(low, REAL(at)[0]), USER, NFC, dd);
	axis_high = GConvertX(fmin2(high, REAL(at)[n-1]), USER, NFC, dd);
	if (side == 1) {
	    if (R_FINITE(pos))
		axis_base = GConvertY(pos, USER, NFC, dd);
	    else
		axis_base = GConvertY(0.0, outer, NFC, dd)
		    - GConvertYUnits(line, LINES, NFC, dd);
	    if (R_FINITE(Rf_gpptr(dd)->tck)) {
		double len, xu, yu;
		if(Rf_gpptr(dd)->tck > 0.5)
		    len = GConvertYUnits(Rf_gpptr(dd)->tck, NPC, NFC, dd);
		else {
		    xu = GConvertXUnits(Rf_gpptr(dd)->tck, NPC, INCHES, dd);
		    yu = GConvertYUnits(Rf_gpptr(dd)->tck, NPC, INCHES, dd);
		    xu = (fabs(xu) < fabs(yu)) ? xu : yu;
		    len = GConvertYUnits(xu, INCHES, NFC, dd);
		}
		axis_tick = axis_base + len;

	    } else
		axis_tick = axis_base +
			GConvertYUnits(Rf_gpptr(dd)->tcl, LINES, NFC, dd);
	}
	else {
	    if (R_FINITE(pos))
		axis_base = GConvertY(pos, USER, NFC, dd);
	    else
		axis_base =  GConvertY(1.0, outer, NFC, dd)
		    + GConvertYUnits(line, LINES, NFC, dd);
	    if (R_FINITE(Rf_gpptr(dd)->tck)) {
		double len, xu, yu;
		if(Rf_gpptr(dd)->tck > 0.5)
		    len = GConvertYUnits(Rf_gpptr(dd)->tck, NPC, NFC, dd);
		else {
		    xu = GConvertXUnits(Rf_gpptr(dd)->tck, NPC, INCHES, dd);
		    yu = GConvertYUnits(Rf_gpptr(dd)->tck, NPC, INCHES, dd);
		    xu = (fabs(xu) < fabs(yu)) ? xu : yu;
		    len = GConvertYUnits(xu, INCHES, NFC, dd);
		}
		axis_tick = axis_base - len;
	    } else
		axis_tick = axis_base -
		    GConvertYUnits(Rf_gpptr(dd)->tcl, LINES, NFC, dd);
	}
	if (doticks) {
	    Rf_gpptr(dd)->col = col;/*was fg */
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
	Rf_gpptr(dd)->col = Rf_gpptr(dd)->colaxis;
	gap = GStrWidth("m", NFC, dd);	/* FIXUP x/y distance */
	tlast = -1.0;
	if (!R_FINITE(hadj)) {
	    if (Rf_gpptr(dd)->las == 2 || Rf_gpptr(dd)->las == 3) {
		Rf_gpptr(dd)->adj = (side == 1) ? 1 : 0;
	    }
	    else Rf_gpptr(dd)->adj = 0.5;
	}
	if (side == 1) {
	    axis_lab = - axis_base
		+ GConvertYUnits(Rf_gpptr(dd)->mgp[1]-lineoff, LINES, NFC, dd)
		+ GConvertY(0.0, NPC, NFC, dd);
	}
	else { /* side == 3 */
	    axis_lab = axis_base
		+ GConvertYUnits(Rf_gpptr(dd)->mgp[1]-lineoff, LINES, NFC, dd)
		- GConvertY(1.0, NPC, NFC, dd);
	}
	axis_lab = GConvertYUnits(axis_lab, NFC, LINES, dd);

	/* The order of processing is important here. */
	/* We must ensure that the labels are drawn left-to-right. */
	/* The logic here is getting way too convoluted. */
	/* This needs a serious rewrite. */

	if (Rf_gpptr(dd)->usr[0] > Rf_gpptr(dd)->usr[1]) {
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
	    double padjval = REAL(padj)[i%npadj];
	    padjval = ComputePAdjValue(padjval, side, Rf_gpptr(dd)->las);
	    x = REAL(at)[i];
	    if (!R_FINITE(x)) continue;
	    temp = GConvertX(x, USER, NFC, dd);
	    if (dolabels) {
		/* Clip tick labels to user coordinates. */
		if (x > low && x < high) {
		    if (isExpression(lab)) {
			GMMathText(VECTOR_ELT(lab, ind[i]), side,
				   axis_lab, 0, x, Rf_gpptr(dd)->las,
				   padjval, dd);
		    }
		    else {
			label = STRING_ELT(lab, ind[i]);
			if(label != NA_STRING) {
			    char *ss = translateChar(label);
			    labw = GStrWidth(ss, NFC, dd);
			    tnew = temp - 0.5 * labw;
			    /* Check room for perpendicular labels. */
			    if (Rf_gpptr(dd)->las == 2 ||
				Rf_gpptr(dd)->las == 3 ||
				tnew - tlast >= gap) {
				GMtext(ss, side, axis_lab, 0, x,
				       Rf_gpptr(dd)->las, padjval, dd);
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
	GetAxisLimits(Rf_gpptr(dd)->usr[2], Rf_gpptr(dd)->usr[3], &low, &high);
	axis_low = GConvertY(fmax2(low, REAL(at)[0]), USER, NFC, dd);
	axis_high = GConvertY(fmin2(high, REAL(at)[n-1]), USER, NFC, dd);
	if (side == 2) {
	    if (R_FINITE(pos))
		axis_base = GConvertX(pos, USER, NFC, dd);
	    else
		axis_base =  GConvertX(0.0, outer, NFC, dd)
		    - GConvertXUnits(line, LINES, NFC, dd);
	    if (R_FINITE(Rf_gpptr(dd)->tck)) {
		double len, xu, yu;
		if(Rf_gpptr(dd)->tck > 0.5)
		    len = GConvertXUnits(Rf_gpptr(dd)->tck, NPC, NFC, dd);
		else {
		    xu = GConvertXUnits(Rf_gpptr(dd)->tck, NPC, INCHES, dd);
		    yu = GConvertYUnits(Rf_gpptr(dd)->tck, NPC, INCHES, dd);
		    xu = (fabs(xu) < fabs(yu)) ? xu : yu;
		    len = GConvertXUnits(xu, INCHES, NFC, dd);
		}
		axis_tick = axis_base + len;
	    } else
		axis_tick = axis_base +
		    GConvertXUnits(Rf_gpptr(dd)->tcl, LINES, NFC, dd);
	}
	else {
	    if (R_FINITE(pos))
		axis_base = GConvertX(pos, USER, NFC, dd);
	    else
		axis_base =  GConvertX(1.0, outer, NFC, dd)
		    + GConvertXUnits(line, LINES, NFC, dd);
	    if (R_FINITE(Rf_gpptr(dd)->tck)) {
		double len, xu, yu;
		if(Rf_gpptr(dd)->tck > 0.5)
		    len = GConvertXUnits(Rf_gpptr(dd)->tck, NPC, NFC, dd);
		else {
		    xu = GConvertXUnits(Rf_gpptr(dd)->tck, NPC, INCHES, dd);
		    yu = GConvertYUnits(Rf_gpptr(dd)->tck, NPC, INCHES, dd);
		    xu = (fabs(xu) < fabs(yu)) ? xu : yu;
		    len = GConvertXUnits(xu, INCHES, NFC, dd);
		}
		axis_tick = axis_base - len;
	    } else
		axis_tick = axis_base -
		    GConvertXUnits(Rf_gpptr(dd)->tcl, LINES, NFC, dd);
	}
	if (doticks) {
	    Rf_gpptr(dd)->col = col;/*was fg */
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
	Rf_gpptr(dd)->col = Rf_gpptr(dd)->colaxis;
	gap = GStrWidth("m", INCHES, dd);
	gap = GConvertYUnits(gap, INCHES, NFC, dd);
	tlast = -1.0;
	if (!R_FINITE(hadj)) {
	    if (Rf_gpptr(dd)->las == 1 || Rf_gpptr(dd)->las == 2) {
		Rf_gpptr(dd)->adj = (side == 2) ? 1 : 0;
	    }
	    else Rf_gpptr(dd)->adj = 0.5;
	}
	if (side == 2) {
	    axis_lab = - axis_base
		+ GConvertXUnits(Rf_gpptr(dd)->mgp[1]-lineoff, LINES, NFC, dd)
		+ GConvertX(0.0, NPC, NFC, dd);
	}
	else { /* side == 4 */
	    axis_lab = axis_base
		+ GConvertXUnits(Rf_gpptr(dd)->mgp[1]-lineoff, LINES, NFC, dd)
		- GConvertX(1.0, NPC, NFC, dd);
	}
	axis_lab = GConvertXUnits(axis_lab, NFC, LINES, dd);

	/* The order of processing is important here. */
	/* We must ensure that the labels are drawn left-to-right. */
	/* The logic here is getting way too convoluted. */
	/* This needs a serious rewrite. */

	if (Rf_gpptr(dd)->usr[2] > Rf_gpptr(dd)->usr[3]) {
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
	    double padjval = REAL(padj)[i%npadj];
	    padjval = ComputePAdjValue(padjval, side, Rf_gpptr(dd)->las);
	    y = REAL(at)[i];
	    if (!R_FINITE(y)) continue;
	    temp = GConvertY(y, USER, NFC, dd);
	    if (dolabels) {
		/* Clip tick labels to user coordinates. */
		if (y > low && y < high) {
		    if (isExpression(lab)) {
			GMMathText(VECTOR_ELT(lab, ind[i]), side,
				   axis_lab, 0, y, Rf_gpptr(dd)->las,
				   padjval, dd);
		    }
		    else {
			label = STRING_ELT(lab, ind[i]);
			if(label != NA_STRING) {
			    char *ss = translateChar(label);
			    labw = GStrWidth(ss, INCHES, dd);
			    labw = GConvertYUnits(labw, INCHES, NFC, dd);
			    tnew = temp - 0.5 * labw;
			    /* Check room for perpendicular labels. */
			    if (Rf_gpptr(dd)->las == 1 ||
				Rf_gpptr(dd)->las == 2 ||
				tnew - tlast >= gap) {
				GMtext(ss, side, axis_lab, 0, y,
				       Rf_gpptr(dd)->las, padjval, dd);
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
    /* NOTE: only record operation if no "error"  */
    if (GRecording(call, dd))
	recordGraphicOperation(op, originalArgs, dd);
    UNPROTECT(4); /* lab, at, lab, padj again */
    return at;
}/* do_axis */


SEXP attribute_hidden do_plot_xy(SEXP call, SEXP op, SEXP args, SEXP env)
{
/*	plot.xy(xy, type, pch, lty, col, bg, cex, lwd, ...)

 *	plot points or lines of various types
 */
    SEXP sxy, sx, sy, pch, cex, col, bg, lty, lwd;
    double *x, *y, xold, yold, xx, yy, thiscex, thislwd;
    int i, n, npch, ncex, ncol, nbg, /*nlty,*/ nlwd,
	type=0, start=0, thispch, thiscol;
    char *vmax = NULL /* -Wall */;

    SEXP originalArgs = args;
    DevDesc *dd = CurrentDevice();

    /* Basic Checks */
    GCheckState(dd);
    if (length(args) < 7)
	errorcall(call, _("too few arguments"));

    /* Required Arguments */
#define PLOT_XY_DEALING(subname)				\
    sx = R_NilValue;		/* -Wall */			\
    sy = R_NilValue;		/* -Wall */			\
    sxy = CAR(args);						\
    if (isNewList(sxy) && length(sxy) >= 2) {			\
	internalTypeCheck(call, sx = VECTOR_ELT(sxy, 0), REALSXP); \
	internalTypeCheck(call, sy = VECTOR_ELT(sxy, 1), REALSXP); \
    }								\
    else if (isList(sxy) && length(sxy) >= 2) {			\
	internalTypeCheck(call, sx = CAR(sxy), REALSXP);	\
	internalTypeCheck(call, sy = CADR(sxy), REALSXP);	\
    }								\
    else							\
	errorcall(call, _("invalid plotting structure"));	\
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
		warningcall(call,
			    _("plot type '%s' will be truncated to first character"),
			    CHAR(pch));
	    type = CHAR(pch)[0];
	}
	else errorcall(call, _("invalid plot type"));
    }
    args = CDR(args);

    PROTECT(pch = FixupPch(CAR(args), Rf_gpptr(dd)->pch));	args = CDR(args);
    npch = length(pch);

    PROTECT(lty = FixupLty(CAR(args), Rf_gpptr(dd)->lty));	args = CDR(args);
    /* nlty = length(lty);*/

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

    PROTECT(lwd = FixupLwd(CAR(args), Rf_gpptr(dd)->lwd)); args = CDR(args);
    nlwd = LENGTH(lwd);

    /* Miscellaneous Graphical Parameters */
    GSavePars(dd);
    ProcessInlinePars(args, dd, call);

    x = REAL(sx);
    y = REAL(sy);

    if (INTEGER(lty)[0] != NA_INTEGER)
	Rf_gpptr(dd)->lty = INTEGER(lty)[0];
    if (R_FINITE( (thislwd = REAL(lwd)[0]) ))
	Rf_gpptr(dd)->lwd = thislwd; /* but do recycle for "p" etc */

    GMode(1, dd);
    /* removed by paul 26/5/99 because all clipping now happens in graphics.c
     * GClip(dd);
     */

    /* Line drawing :*/
    switch(type) {
    case 'l':
    case 'o':
	/* lines and overplotted lines and points */
	Rf_gpptr(dd)->col = INTEGER(col)[0];
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
	Rf_gpptr(dd)->col = INTEGER(col)[0];
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

    case 's': /* step function	I */
    {
	double *xtemp, *ytemp;
	int n0 = 0;
	if(n <= 1000) {
	    xtemp = (double *) alloca(2*n*sizeof(double));
	    ytemp = (double *) alloca(2*n*sizeof(double));
	    R_CheckStack();
	} else {
	    vmax = vmaxget();
	    xtemp = (double *) R_alloc(2*n, sizeof(double));
	    ytemp = (double *) R_alloc(2*n, sizeof(double));
	}
	Rf_gpptr(dd)->col = INTEGER(col)[0];
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
	    xtemp = (double *) alloca(2*n*sizeof(double));
	    ytemp = (double *) alloca(2*n*sizeof(double));
	    R_CheckStack();
	} else {
	    vmax = vmaxget();
	    xtemp = (double *) R_alloc(2*n, sizeof(double));
	    ytemp = (double *) R_alloc(2*n, sizeof(double));
	}
	Rf_gpptr(dd)->col = INTEGER(col)[0];
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
	if (Rf_gpptr(dd)->ylog)
	    yold = Rf_gpptr(dd)->usr[2];/* DBL_MIN fails.. why ???? */
	else
	    yold = 0.0;
	yold = GConvertY(yold, USER, DEVICE, dd);
	for (i = 0; i < n; i++) {
	    xx = x[i];
	    yy = y[i];
	    GConvert(&xx, &yy, USER, DEVICE, dd);
	    if (R_FINITE(xx) && R_FINITE(yy)
		&& !R_TRANSPARENT(thiscol = INTEGER(col)[i % ncol])) {
		Rf_gpptr(dd)->col = thiscol;
		GLine(xx, yold, xx, yy, DEVICE, dd);
	    }
	}
	break;

    case 'p':
    case 'n': /* nothing here */
	break;

    default:/* OTHERWISE */
	errorcall(call, _("invalid plot type '%c'"), type);

    } /* End {switch(type)} - for lines */

    /* Points : */
    if (type == 'p' || type == 'b' || type == 'o') {
	for (i = 0; i < n; i++) {
	    xx = x[i];
	    yy = y[i];
	    GConvert(&xx, &yy, USER, DEVICE, dd);
	    if (R_FINITE(xx) && R_FINITE(yy)) {
		if (R_FINITE( (thiscex = REAL(cex)[i % ncex]) )
		    && (thispch = INTEGER(pch)[i % npch]) != NA_INTEGER
		    && !R_TRANSPARENT(thiscol = INTEGER(col)[i % ncol]))
		{
		    Rf_gpptr(dd)->cex = thiscex * Rf_gpptr(dd)->cexbase;
		    Rf_gpptr(dd)->col = thiscol;
		    if(nlwd > 1 && R_FINITE((thislwd = REAL(lwd)[i % nlwd])))
			Rf_gpptr(dd)->lwd = thislwd;
		    Rf_gpptr(dd)->bg = INTEGER(bg)[i % nbg];
		    GSymbol(xx, yy, DEVICE, thispch, dd);
		}
	    }
	}
    }
    GMode(0, dd);
    GRestorePars(dd);
    UNPROTECT(6);
    /* NOTE: only record operation if no "error"  */
    if (GRecording(call, dd))
	recordGraphicOperation(op, originalArgs, dd);
    return R_NilValue;
}/* do_plot_xy */

/* Checks for ... , x0, y0, x1, y1 ... */

static void xypoints(SEXP call, SEXP args, int *n)
{
    int k=0;/* -Wall */

    if (!isNumeric(CAR(args)) || (k = LENGTH(CAR(args))) <= 0)
	errorcall(call, _("invalid first argument"));
    SETCAR(args, coerceVector(CAR(args), REALSXP));
    *n = k;
    args = CDR(args);

    if (!isNumeric(CAR(args)) || (k = LENGTH(CAR(args))) <= 0)
	errorcall(call, _("invalid second argument"));
    SETCAR(args, coerceVector(CAR(args), REALSXP));
    if (k > *n) *n = k;
    args = CDR(args);

    if (!isNumeric(CAR(args)) || (k = LENGTH(CAR(args))) <= 0)
	errorcall(call, _("invalid third argument"));
    SETCAR(args, coerceVector(CAR(args), REALSXP));
    if (k > *n) *n = k;
    args = CDR(args);

    if (!isNumeric(CAR(args)) || (k = LENGTH(CAR(args))) <= 0)
	errorcall(call, _("invalid fourth argument"));
    SETCAR(args, coerceVector(CAR(args), REALSXP));
    if (k > *n) *n = k;
    args = CDR(args);
}


SEXP attribute_hidden do_segments(SEXP call, SEXP op, SEXP args, SEXP env)
{
    /* segments(x0, y0, x1, y1, col, lty, lwd, ...) */
    SEXP sx0, sx1, sy0, sy1, col, lty, lwd;
    double *x0, *x1, *y0, *y1;
    double xx[2], yy[2];
    int nx0, nx1, ny0, ny1, i, n, ncol, nlty, nlwd;
    SEXP originalArgs = args;
    DevDesc *dd = CurrentDevice();

    if (length(args) < 4) errorcall(call, _("too few arguments"));
    GCheckState(dd);

    xypoints(call, args, &n);

    sx0 = CAR(args); nx0 = length(sx0); args = CDR(args);
    sy0 = CAR(args); ny0 = length(sy0); args = CDR(args);
    sx1 = CAR(args); nx1 = length(sx1); args = CDR(args);
    sy1 = CAR(args); ny1 = length(sy1); args = CDR(args);

    PROTECT(col = FixupCol(CAR(args), R_TRANWHITE));
    ncol = LENGTH(col); args = CDR(args);

    PROTECT(lty = FixupLty(CAR(args), Rf_gpptr(dd)->lty));
    nlty = length(lty); args = CDR(args);

    PROTECT(lwd = FixupLwd(CAR(args), Rf_gpptr(dd)->lwd));
    nlwd = length(lwd); args = CDR(args);

    GSavePars(dd);
    ProcessInlinePars(args, dd, call);

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
	    int thiscol = INTEGER(col)[i % ncol];
	    if(!R_TRANSPARENT(thiscol)) {
		Rf_gpptr(dd)->col = thiscol;
		Rf_gpptr(dd)->lty = INTEGER(lty)[i % nlty];
		Rf_gpptr(dd)->lwd = REAL(lwd)[i % nlwd];
		GLine(xx[0], yy[0], xx[1], yy[1], DEVICE, dd);
	    }
	}
    }
    GMode(0, dd);
    GRestorePars(dd);

    UNPROTECT(3);
    /* NOTE: only record operation if no "error"  */
    if (GRecording(call, dd))
	recordGraphicOperation(op, originalArgs, dd);
    return R_NilValue;
}


SEXP attribute_hidden do_rect(SEXP call, SEXP op, SEXP args, SEXP env)
{
    /* rect(xl, yb, xr, yt, col, border, lty, ...) */
    SEXP sxl, sxr, syb, syt, col, lty, lwd, border;
    double *xl, *xr, *yb, *yt, x0, y0, x1, y1;
    int i, n, nxl, nxr, nyb, nyt, ncol, nlty, nlwd, nborder;
    SEXP originalArgs = args;
    DevDesc *dd = CurrentDevice();

    if (length(args) < 4) errorcall(call, _("too few arguments"));
    GCheckState(dd);

    xypoints(call, args, &n);
    sxl = CAR(args); nxl = length(sxl); args = CDR(args);/* x_left */
    syb = CAR(args); nyb = length(syb); args = CDR(args);/* y_bottom */
    sxr = CAR(args); nxr = length(sxr); args = CDR(args);/* x_right */
    syt = CAR(args); nyt = length(syt); args = CDR(args);/* y_top */

    PROTECT(col = FixupCol(CAR(args), R_TRANWHITE));
    ncol = LENGTH(col);
    args = CDR(args);

    PROTECT(border =  FixupCol(CAR(args), Rf_gpptr(dd)->fg));
    nborder = LENGTH(border);
    args = CDR(args);

    PROTECT(lty = FixupLty(CAR(args), Rf_gpptr(dd)->lty));
    nlty = length(lty);
    args = CDR(args);

    PROTECT(lwd = FixupLwd(CAR(args), Rf_gpptr(dd)->lwd));
    nlwd = length(lwd);
    args = CDR(args);

    GSavePars(dd);
    ProcessInlinePars(args, dd, call);

    xl = REAL(sxl);
    xr = REAL(sxr);
    yb = REAL(syb);
    yt = REAL(syt);

    GMode(1, dd);
    for (i = 0; i < n; i++) {
	if (nlty && INTEGER(lty)[i % nlty] != NA_INTEGER)
	    Rf_gpptr(dd)->lty = INTEGER(lty)[i % nlty];
	else
	    Rf_gpptr(dd)->lty = Rf_dpptr(dd)->lty;
	if (nlwd && REAL(lwd)[i % nlwd] != NA_REAL)
	    Rf_gpptr(dd)->lwd = REAL(lwd)[i % nlwd];
	else
	    Rf_gpptr(dd)->lwd = Rf_dpptr(dd)->lwd;
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
    if (GRecording(call, dd))
	recordGraphicOperation(op, originalArgs, dd);
    return R_NilValue;
}


SEXP attribute_hidden do_arrows(SEXP call, SEXP op, SEXP args, SEXP env)
{
    /* arrows(x0, y0, x1, y1, length, angle, code, col, lty, lwd, ...) */
    SEXP sx0, sx1, sy0, sy1, col, lty, lwd;
    double *x0, *x1, *y0, *y1;
    double xx0, yy0, xx1, yy1;
    double hlength, angle;
    int code;
    int nx0, nx1, ny0, ny1, i, n, ncol, nlty, nlwd, thiscol;
    SEXP originalArgs = args;
    DevDesc *dd = CurrentDevice();

    if (length(args) < 4) errorcall(call, _("too few arguments"));
    GCheckState(dd);

    xypoints(call, args, &n);

    sx0 = CAR(args); nx0 = length(sx0); args = CDR(args);
    sy0 = CAR(args); ny0 = length(sy0); args = CDR(args);
    sx1 = CAR(args); nx1 = length(sx1); args = CDR(args);
    sy1 = CAR(args); ny1 = length(sy1); args = CDR(args);

    hlength = asReal(CAR(args));
    if (!R_FINITE(hlength) || hlength < 0)
	errorcall(call, _("invalid arrow head length"));
    args = CDR(args);

    angle = asReal(CAR(args));
    if (!R_FINITE(angle))
	errorcall(call, _("invalid arrow head angle"));
    args = CDR(args);

    code = asInteger(CAR(args));
    if (code == NA_INTEGER || code < 0 || code > 3)
	errorcall(call, _("invalid arrow head specification"));
    args = CDR(args);

    PROTECT(col = FixupCol(CAR(args), R_TRANWHITE));
    ncol = LENGTH(col);
    args = CDR(args);

    PROTECT(lty = FixupLty(CAR(args), Rf_gpptr(dd)->lty));
    nlty = length(lty);
    args = CDR(args);

    PROTECT(lwd = FixupLwd(CAR(args), Rf_gpptr(dd)->lwd));
    nlwd = length(lwd);
    args = CDR(args);

    GSavePars(dd);
    ProcessInlinePars(args, dd, call);

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
	if (R_FINITE(xx0) && R_FINITE(yy0) && R_FINITE(xx1) && R_FINITE(yy1)
	    && !R_TRANSPARENT(thiscol = INTEGER(col)[i % ncol])) {
	    Rf_gpptr(dd)->col = thiscol;
	    Rf_gpptr(dd)->lty = INTEGER(lty)[i % nlty];
	    Rf_gpptr(dd)->lwd = REAL(lwd)[i % nlwd];
	    GArrow(xx0, yy0, xx1, yy1, DEVICE,
		   hlength, angle, code, dd);
	}
    }
    GMode(0, dd);
    GRestorePars(dd);

    UNPROTECT(3);
    /* NOTE: only record operation if no "error"  */
    if (GRecording(call, dd))
	recordGraphicOperation(op, originalArgs, dd);
    return R_NilValue;
}


static void drawPolygon(int n, double *x, double *y,
			int lty, int fill, int border, DevDesc *dd)
{
    if (lty == NA_INTEGER)
	Rf_gpptr(dd)->lty = Rf_dpptr(dd)->lty;
    else
	Rf_gpptr(dd)->lty = lty;
    GPolygon(n, x, y, USER, fill, border, dd);
}

SEXP attribute_hidden do_polygon(SEXP call, SEXP op, SEXP args, SEXP env)
{
    /* polygon(x, y, col, border, lty, ...) */
    SEXP sx, sy, col, border, lty;
    int nx;
    int ncol, nborder, nlty, i, start=0;
    int num = 0;
    double *x, *y, xx, yy, xold, yold;

    SEXP originalArgs = args;
    DevDesc *dd = CurrentDevice();

    GCheckState(dd);

    if (length(args) < 2) errorcall(call, _("too few arguments"));
    /* (x,y) is checked in R via xy.coords() ; no need here : */
    sx = SETCAR(args, coerceVector(CAR(args), REALSXP));  args = CDR(args);
    sy = SETCAR(args, coerceVector(CAR(args), REALSXP));  args = CDR(args);
    nx = LENGTH(sx);

    PROTECT(col = FixupCol(CAR(args), R_TRANWHITE));	args = CDR(args);
    ncol = LENGTH(col);

    PROTECT(border = FixupCol(CAR(args), Rf_gpptr(dd)->fg)); args = CDR(args);
    nborder = LENGTH(border);

    PROTECT(lty = FixupLty(CAR(args), Rf_gpptr(dd)->lty)); args = CDR(args);
    nlty = length(lty);

    GSavePars(dd);
    ProcessInlinePars(args, dd, call);

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
    if (GRecording(call, dd))
	recordGraphicOperation(op, originalArgs, dd);
    return R_NilValue;
}

SEXP attribute_hidden do_text(SEXP call, SEXP op, SEXP args, SEXP env)
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
    SEXP string, originalArgs = args;
    DevDesc *dd = CurrentDevice();

    GCheckState(dd);

    if (length(args) < 3) errorcall(call, _("too few arguments"));

    PLOT_XY_DEALING("text");

    /* labels */
    txt = CAR(args);
    if (isSymbol(txt) || isLanguage(txt))
	txt = coerceVector(txt, EXPRSXP);
    else if (!isExpression(txt))
	txt = coerceVector(txt, STRSXP);
    PROTECT(txt);
    if (length(txt) <= 0)
	errorcall(call, _("zero length 'labels'"));
    args = CDR(args);

    PROTECT(adj = CAR(args));
    if (isNull(adj) || (isNumeric(adj) && length(adj) == 0)) {
	adjx = Rf_gpptr(dd)->adj;
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
    else errorcall(call, _("invalid '%s' value"), "adj");
    args = CDR(args);

    PROTECT(pos = coerceVector(CAR(args), INTSXP));
    npos = length(pos);
    for (i = 0; i < npos; i++)
	if (INTEGER(pos)[i] < 1 || INTEGER(pos)[i] > 4)
	    errorcall(call, _("invalid '%s' value"), "pos");
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
    ProcessInlinePars(args, dd, call);

    GMode(1, dd);
    if (n == 0 && ntxt > 0)
	errorcall(call, _("no coordinates were supplied"));
    for (i = 0; i < imax2(n,ntxt); i++) {
	xx = x[i % n];
	yy = y[i % n];
	GConvert(&xx, &yy, USER, INCHES, dd);
	if (R_FINITE(xx) && R_FINITE(yy)) {
	    if (ncol && !isNAcol(rawcol, i, ncol))
		Rf_gpptr(dd)->col = INTEGER(col)[i % ncol];
	    else
		Rf_gpptr(dd)->col = Rf_dpptr(dd)->col;
	    if (ncex && R_FINITE(REAL(cex)[i%ncex]))
		Rf_gpptr(dd)->cex = Rf_gpptr(dd)->cexbase * REAL(cex)[i % ncex];
	    else
		Rf_gpptr(dd)->cex = Rf_gpptr(dd)->cexbase;
	    if (nfont && INTEGER(font)[i % nfont] != NA_INTEGER)
		Rf_gpptr(dd)->font = INTEGER(font)[i % nfont];
	    else
		Rf_gpptr(dd)->font = Rf_dpptr(dd)->font;
	    if (npos > 0) {
		switch(INTEGER(pos)[i % npos]) {
		case 1:
		    yy = yy - offset;
		    adjx = 0.5;
		    adjy = 1 - (0.5 - Rf_gpptr(dd)->yCharOffset);
		    break;
		case 2:
		    xx = xx - offset;
		    adjx = 1;
		    adjy = Rf_gpptr(dd)->yCharOffset;
		    break;
		case 3:
		    yy = yy + offset;
		    adjx = 0.5;
		    adjy = 0;
		    break;
		case 4:
		    xx = xx + offset;
		    adjx = 0;
		    adjy = Rf_gpptr(dd)->yCharOffset;
		    break;
		}
	    }
	    if (vectorFonts) {
		string = STRING_ELT(txt, i % ntxt);
		if(string != NA_STRING)
		    GVText(xx, yy, INCHES, translateChar(string),
			   INTEGER(vfont)[0], INTEGER(vfont)[1],
			   adjx, adjy, Rf_gpptr(dd)->srt, dd);
	    } else if (isExpression(txt)) {
		GMathText(xx, yy, INCHES, VECTOR_ELT(txt, i % ntxt),
			  adjx, adjy, Rf_gpptr(dd)->srt, dd);
	    } else {
		string = STRING_ELT(txt, i % ntxt);
		if(string != NA_STRING)
		    GText(xx, yy, INCHES, translateChar(string),
			  adjx, adjy, Rf_gpptr(dd)->srt, dd);
	    }
	}
    }
    GMode(0, dd);

    GRestorePars(dd);
    UNPROTECT(7);
    /* NOTE: only record operation if no "error"  */
    if (GRecording(call, dd))
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
				    DevDesc *dd)
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
			     DevDesc *dd)
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

SEXP attribute_hidden do_mtext(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP text, side, line, outer, at, adj, padj, cex, col, font, string;
    SEXP rawcol;
    int ntext, nside, nline, nouter, nat, nadj, npadj, ncex, ncol, nfont;
    Rboolean dirtyplot = FALSE, gpnewsave = FALSE, dpnewsave = FALSE;
    int i, n, fontsave, colsave;
    double cexsave;
    SEXP originalArgs = args;
    DevDesc *dd = CurrentDevice();

    GCheckState(dd);

    if (length(args) < 9)
	errorcall(call, _("too few arguments"));

    /* Arg1 : text= */
    text = CAR(args);
    if (isSymbol(text) || isLanguage(text))
	text = coerceVector(text, EXPRSXP);
    else if (!isExpression(text))
	text = coerceVector(text, STRSXP);
    PROTECT(text);
    n = ntext = length(text);
    if (ntext <= 0)
	errorcall(call, _("zero length 'text' specified"));
    args = CDR(args);

    /* Arg2 : side= */
    PROTECT(side = coerceVector(CAR(args), INTSXP));
    nside = length(side);
    if (nside <= 0) errorcall(call, _("zero length 'side' specified"));
    if (n < nside) n = nside;
    args = CDR(args);

    /* Arg3 : line= */
    PROTECT(line = coerceVector(CAR(args), REALSXP));
    nline = length(line);
    if (nline <= 0) errorcall(call, _("zero length 'line' specified"));
    if (n < nline) n = nline;
    args = CDR(args);

    /* Arg4 : outer= */
    /* outer == NA => outer <- 0 */
    PROTECT(outer = coerceVector(CAR(args), INTSXP));
    nouter = length(outer);
    if (nouter <= 0) errorcall(call, _("zero length 'outer' specified"));
    if (n < nouter) n = nouter;
    args = CDR(args);

    /* Arg5 : at= */
    PROTECT(at = coerceVector(CAR(args), REALSXP));
    nat = length(at);
    if (nat <= 0) errorcall(call, _("zero length 'at' specified"));
    if (n < nat) n = nat;
    args = CDR(args);

    /* Arg6 : adj= */
    PROTECT(adj = coerceVector(CAR(args), REALSXP));
    nadj = length(adj);
    if (nadj <= 0) errorcall(call, _("zero length 'adj' specified"));
    if (n < nadj) n = nadj;
    args = CDR(args);

    /* Arg7 : padj= */
    PROTECT(padj = coerceVector(CAR(args), REALSXP));
    npadj = length(padj);
    if (npadj <= 0) errorcall(call, _("zero length 'padj' specified"));
    if (n < npadj) n = npadj;
    args = CDR(args);

    /* Arg8 : cex */
    PROTECT(cex = FixupCex(CAR(args), 1.0));
    ncex = length(cex);
    if (ncex <= 0) errorcall(call, _("zero length 'cex' specified"));
    if (n < ncex) n = ncex;
    args = CDR(args);

    /* Arg9 : col */
    rawcol = CAR(args);
    PROTECT(col = FixupCol(rawcol, R_TRANWHITE));
    ncol = length(col);
    if (ncol <= 0) errorcall(call, _("zero length 'col' specified"));
    if (n < ncol) n = ncol;
    args = CDR(args);

    /* Arg10 : font */
    PROTECT(font = FixupFont(CAR(args), NA_INTEGER));
    nfont = length(font);
    if (nfont <= 0) errorcall(call, _("zero length 'font' specified"));
    if (n < nfont) n = nfont;
    args = CDR(args);

    GSavePars(dd);
    ProcessInlinePars(args, dd, call);

    /* If we only scribble in the outer margins, */
    /* we don't want to mark the plot as dirty. */

    dirtyplot = FALSE;
    gpnewsave = Rf_gpptr(dd)->new;
    dpnewsave = Rf_dpptr(dd)->new;
    cexsave = Rf_gpptr(dd)->cex;
    fontsave = Rf_gpptr(dd)->font;
    colsave = Rf_gpptr(dd)->col;

    /* override par("xpd") and force clipping to figure region */
    /* NOTE: don't override to _reduce_ clipping region */
    if (Rf_gpptr(dd)->xpd < 1)
	Rf_gpptr(dd)->xpd = 1;

    if (outer) {
	gpnewsave = Rf_gpptr(dd)->new;
	dpnewsave = Rf_dpptr(dd)->new;
	/* override par("xpd") and force clipping to device region */
	Rf_gpptr(dd)->xpd = 2;
    }
    GMode(1, dd);

    for (i = 0; i < n; i++) {
	double atval = REAL(at)[i%nat];
	double adjval = REAL(adj)[i%nadj];
	double padjval = REAL(padj)[i%npadj];
	double cexval = REAL(cex)[i%ncex];
	double lineval = REAL(line)[i%nline];
	int outerval = INTEGER(outer)[i%nouter];
	int sideval = INTEGER(side)[i%nside];
	int fontval = INTEGER(font)[i%nfont];
	int colval = INTEGER(col)[i%ncol];

	if (outerval == NA_INTEGER) outerval = 0;
	/* Note : we ignore any shrinking produced */
	/* by mfrow / mfcol specs here.	 I.e. don't */
	/* Rf_gpptr(dd)->cexbase. */
	if (R_FINITE(cexval)) Rf_gpptr(dd)->cex = cexval;
	else cexval = cexsave;
	Rf_gpptr(dd)->font = (fontval == NA_INTEGER) ? fontsave : fontval;
	if (isNAcol(rawcol, i, ncol))
	    Rf_gpptr(dd)->col = colsave;
	else
	    Rf_gpptr(dd)->col = colval;
	Rf_gpptr(dd)->adj = ComputeAdjValue(adjval, sideval, Rf_gpptr(dd)->las);
	padjval = ComputePAdjValue(padjval, sideval, Rf_gpptr(dd)->las);
	atval = ComputeAtValue(atval, Rf_gpptr(dd)->adj, sideval, Rf_gpptr(dd)->las,
			       outerval, dd);

	if (isExpression(text))
	    GMMathText(VECTOR_ELT(text, i%ntext),
		       sideval, lineval, outerval, atval, Rf_gpptr(dd)->las,
		       padjval, dd);
	else {
	    string = STRING_ELT(text, i%ntext);
	    if(string != NA_STRING)
		GMtext(translateChar(string), sideval, lineval,
		       outerval, atval, Rf_gpptr(dd)->las, padjval, dd);
	}

	if (outerval == 0) dirtyplot = TRUE;
    }
    GMode(0, dd);

    GRestorePars(dd);
    if (!dirtyplot) {
	Rf_gpptr(dd)->new = gpnewsave;
	Rf_dpptr(dd)->new = dpnewsave;
    }
    UNPROTECT(10);

    /* NOTE: only record operation if no "error"  */
    if (GRecording(call, dd))
	recordGraphicOperation(op, originalArgs, dd);
    return R_NilValue;
}/* do_mtext */


SEXP attribute_hidden do_title(SEXP call, SEXP op, SEXP args, SEXP env)
{
/* Annotation for plots :

   title(main, sub, xlab, ylab,
	 line, outer,
	 ...) */

    SEXP Main, xlab, ylab, sub, string;
    double adj, adjy, cex, offset, line, hpos, vpos, where;
    int col, font, outer;
    int i, n;
    SEXP originalArgs = args;
    DevDesc *dd = CurrentDevice();

    GCheckState(dd);

    if (length(args) < 6) errorcall(call, _("too few arguments"));

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
    ProcessInlinePars(args, dd, call);

    /* override par("xpd") and force clipping to figure region */
    /* NOTE: don't override to _reduce_ clipping region */
    if (Rf_gpptr(dd)->xpd < 1)
	Rf_gpptr(dd)->xpd = 1;
    if (outer)
	Rf_gpptr(dd)->xpd = 2;
    adj = Rf_gpptr(dd)->adj;

    GMode(1, dd);
    if (Main != R_NilValue) {
	cex = Rf_gpptr(dd)->cexmain;
	col = Rf_gpptr(dd)->colmain;
	font = Rf_gpptr(dd)->fontmain;
	/* GetTextArg may coerce, so protect the result */
	GetTextArg(call, Main, &Main, &col, &cex, &font);
	PROTECT(Main);	
	Rf_gpptr(dd)->col = col;
	Rf_gpptr(dd)->cex = Rf_gpptr(dd)->cexbase * cex;
	Rf_gpptr(dd)->font = font;
	if (outer) {
	    if (R_FINITE(line)) {
		vpos = line;
		adjy = 0;
	    }
	    else {
		vpos = 0.5 * Rf_gpptr(dd)->oma[2];
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
		vpos = 0.5 * Rf_gpptr(dd)->mar[2];
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
		    GText(hpos, offset - i, where, translateChar(string),
			  adj, adjy, 0.0, dd);
	  }
	}
	UNPROTECT(1);
    }
    if (sub != R_NilValue) {
	cex = Rf_gpptr(dd)->cexsub;
	col = Rf_gpptr(dd)->colsub;
	font = Rf_gpptr(dd)->fontsub;
	/* GetTextArg may coerce, so protect the result */
	GetTextArg(call, sub, &sub, &col, &cex, &font);
	PROTECT(sub);
	Rf_gpptr(dd)->col = col;
	Rf_gpptr(dd)->cex = Rf_gpptr(dd)->cexbase * cex;
	Rf_gpptr(dd)->font = font;
	if (R_FINITE(line))
	    vpos = line;
	else
	    vpos = Rf_gpptr(dd)->mgp[0] + 1;
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
		    GMtext(translateChar(string), 1, vpos, where, 
			   hpos, 0, 0.0, dd);
	    }
	}
	UNPROTECT(1);
    }
    if (xlab != R_NilValue) {
	cex = Rf_gpptr(dd)->cexlab;
	col = Rf_gpptr(dd)->collab;
	font = Rf_gpptr(dd)->fontlab;
	/* GetTextArg may coerce, so protect the result */
	GetTextArg(call, xlab, &xlab, &col, &cex, &font);
	PROTECT(xlab);
	Rf_gpptr(dd)->cex = Rf_gpptr(dd)->cexbase * cex;
	Rf_gpptr(dd)->col = col;
	Rf_gpptr(dd)->font = font;
	if (R_FINITE(line))
	    vpos = line;
	else
	    vpos = Rf_gpptr(dd)->mgp[0];
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
		    GMtext(translateChar(string), 1, vpos + i, 
			   where, hpos, 0, 0.0, dd);
	    }
	}
	UNPROTECT(1);
    }
    if (ylab != R_NilValue) {
	cex = Rf_gpptr(dd)->cexlab;
	col = Rf_gpptr(dd)->collab;
	font = Rf_gpptr(dd)->fontlab;
	/* GetTextArg may coerce, so protect the result */
	GetTextArg(call, ylab, &ylab, &col, &cex, &font);
	PROTECT(ylab);
	Rf_gpptr(dd)->cex = Rf_gpptr(dd)->cexbase * cex;
	Rf_gpptr(dd)->col = col;
	Rf_gpptr(dd)->font = font;
	if (R_FINITE(line))
	    vpos = line;
	else
	    vpos = Rf_gpptr(dd)->mgp[0];
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
		    GMtext(translateChar(string), 2, vpos - i, 
			   where, hpos, 0, 0.0, dd);
	    }
	}
	UNPROTECT(1);
    }
    GMode(0, dd);
    GRestorePars(dd);
    /* NOTE: only record operation if no "error"  */
    if (GRecording(call, dd))
	recordGraphicOperation(op, originalArgs, dd);
    return R_NilValue;
}/* do_title */


/*  abline(a, b, h, v, col, lty, lwd, ...)
    draw lines in intercept/slope form.	 */

static void getxlimits(double *x, DevDesc *dd) {
    /*
     * xpd = 0 means clip to current plot region
     * xpd = 1 means clip to current figure region
     * xpd = 2 means clip to device region
     */
    switch (Rf_gpptr(dd)->xpd) {
    case 0:
	x[0] = Rf_gpptr(dd)->usr[0];
	x[1] = Rf_gpptr(dd)->usr[1];
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

static void getylimits(double *y, DevDesc *dd) {
    switch (Rf_gpptr(dd)->xpd) {
    case 0:
	y[0] = Rf_gpptr(dd)->usr[2];
	y[1] = Rf_gpptr(dd)->usr[3];
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

SEXP attribute_hidden do_abline(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP a, b, h, v, untf, col, lty, lwd;
    int i, ncol, nlines, nlty, nlwd, lstart, lstop;
    double aa, bb, x[2], y[2]={0.,0.} /* -Wall */;
    SEXP originalArgs = args;
    DevDesc *dd = CurrentDevice();

    GCheckState(dd);

    if (length(args) < 5) errorcall(call, _("too few arguments"));

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

    PROTECT(lty = FixupLty(CAR(args), Rf_gpptr(dd)->lty)); args = CDR(args);
    nlty = length(lty);

    PROTECT(lwd = FixupLwd(CAR(args), Rf_gpptr(dd)->lwd)); args = CDR(args);
    nlwd = length(lwd);

    GSavePars(dd);

    ProcessInlinePars(args, dd, call);

    nlines = 0;

    if (a != R_NilValue) {
	if (b == R_NilValue) {
	    if (LENGTH(a) != 2)
		errorcall(call, _("invalid a=, b= specification"));
	    aa = REAL(a)[0];
	    bb = REAL(a)[1];
	}
	else {
	    aa = asReal(a);
	    bb = asReal(b);
	}
	if (!R_FINITE(aa) || !R_FINITE(bb))
	    errorcall(call, _("'a' and 'b' must be finite"));
	Rf_gpptr(dd)->col = INTEGER(col)[0];
	Rf_gpptr(dd)->lwd = REAL(lwd)[0];
	if (nlty && INTEGER(lty)[0] != NA_INTEGER)
	    Rf_gpptr(dd)->lty = INTEGER(lty)[0];
	else
	    Rf_gpptr(dd)->lty = Rf_dpptr(dd)->lty;
	GMode(1, dd);
	/* FIXME?
	 * Seems like the logic here is just draw from xmin to xmax
	 * and you're guaranteed to draw at least from ymin to ymax
	 * This MAY cause a problem at some stage when the line being
	 * drawn is VERY steep -- and the problem is worse now that
	 * abline will potentially draw to the extents of the device
	 * (when xpd=NA).  NOTE that R's internal clipping protects the
	 * device drivers from stupidly large numbers, BUT there is
	 * still a risk that we could produce a number which is too
	 * big for the computer's brain.
	 * Paul.
	 *
	 * The problem is worse -- you could get NaN, which at least the
	 * X11 device coerces to -2^31 <TSL>
	 */
	getxlimits(x, dd);
	if (R_FINITE(Rf_gpptr(dd)->lwd)) {
	    if (LOGICAL(untf)[0] == 1 && (Rf_gpptr(dd)->xlog || Rf_gpptr(dd)->ylog)) {
		double xx[101], yy[101];
		double xstep = (x[1] - x[0])/100;
		for (i = 0; i < 100; i++) {
		    xx[i] = x[0] + i*xstep;
		    yy[i] = aa + xx[i] * bb;
		}
		xx[100] = x[1];
		yy[100] = aa + x[1] * bb;

		/* now get rid of -ve values */
		lstart=0;lstop=100;
		if (Rf_gpptr(dd)->xlog){
			for(;xx[lstart]<=0 && lstart<101;lstart++);
			for(;xx[lstop]<=0 && lstop>0;lstop--);
		}
		if (Rf_gpptr(dd)->ylog){
			for(;yy[lstart]<=0 && lstart<101;lstart++);
			for(;yy[lstop]<=0 && lstop>0;lstop--);
		}


		GPolyline(lstop-lstart+1, xx+lstart, yy+lstart, USER, dd);
	    }
	    else {
		double x0, x1;

		x0 = ( Rf_gpptr(dd)->xlog ) ?	log10(x[0]) : x[0];
		x1 = ( Rf_gpptr(dd)->xlog ) ?	log10(x[1]) : x[1];

		y[0] = aa + x0 * bb;
		y[1] = aa + x1 * bb;

		if ( Rf_gpptr(dd)->ylog ){
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
	    Rf_gpptr(dd)->col = INTEGER(col)[nlines % ncol];
	    if (nlty && INTEGER(lty)[nlines % nlty] != NA_INTEGER)
		Rf_gpptr(dd)->lty = INTEGER(lty)[nlines % nlty];
	    else
		Rf_gpptr(dd)->lty = Rf_dpptr(dd)->lty;
	    Rf_gpptr(dd)->lwd = REAL(lwd)[nlines % nlwd];
	    aa = REAL(h)[i];
	    if (R_FINITE(aa) && R_FINITE(Rf_gpptr(dd)->lwd)) {
		getxlimits(x, dd);
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
	    Rf_gpptr(dd)->col = INTEGER(col)[nlines % ncol];
	    if (nlty && INTEGER(lty)[nlines % nlty] != NA_INTEGER)
		Rf_gpptr(dd)->lty = INTEGER(lty)[nlines % nlty];
	    else
		Rf_gpptr(dd)->lty = Rf_dpptr(dd)->lty;
	    Rf_gpptr(dd)->lwd = REAL(lwd)[nlines % nlwd];
	    aa = REAL(v)[i];
	    if (R_FINITE(aa) && R_FINITE(Rf_gpptr(dd)->lwd)) {
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
    /* NOTE: only record operation if no "error"  */
    if (GRecording(call, dd))
	recordGraphicOperation(op, originalArgs, dd);
    return R_NilValue;
} /* do_abline */


SEXP attribute_hidden do_box(SEXP call, SEXP op, SEXP args, SEXP env)
{
/*     box(which="plot", lty="solid", ...)
       --- which is coded, 1 = plot, 2 = figure, 3 = inner, 4 = outer.
*/
    int which, col;
    SEXP colsxp, fgsxp;
    SEXP originalArgs = args;
    DevDesc *dd = CurrentDevice();

    GCheckState(dd);
    GSavePars(dd);
    which = asInteger(CAR(args)); args = CDR(args);
    if (which < 1 || which > 4)
	errorcall(call, _("invalid 'which' specification"));
    /*
     * If specified non-NA col then use that, else ...
     *
     * if specified non-NA fg then use that, else ...
     *
     * else use par("col")
     */
    col= Rf_gpptr(dd)->col;
    ProcessInlinePars(args, dd, call);
    colsxp = getInlinePar(args, "col");
    if (isNAcol(colsxp, 0, 1)) {
	fgsxp = getInlinePar(args, "fg");
	if (isNAcol(fgsxp, 0, 1))
	    Rf_gpptr(dd)->col = col;
	else
	    Rf_gpptr(dd)->col = Rf_gpptr(dd)->fg;
    }
    /* override par("xpd") and force clipping to device region */
    Rf_gpptr(dd)->xpd = 2;
    GMode(1, dd);
    GBox(which, dd);
    GMode(0, dd);
    GRestorePars(dd);
    /* NOTE: only record operation if no "error"  */
    if (GRecording(call, dd))
	recordGraphicOperation(op, originalArgs, dd);
    return R_NilValue;
}

static void drawPointsLines(double xp, double yp, double xold, double yold,
			    char type, int first, DevDesc *dd)
{
    if (type == 'p' || type == 'o')
	GSymbol(xp, yp, DEVICE, Rf_gpptr(dd)->pch, dd);
    if ((type == 'l' || type == 'o') && !first)
	GLine(xold, yold, xp, yp, DEVICE, dd);
}

SEXP attribute_hidden do_locator(SEXP call, SEXP op, SEXP args, SEXP env)
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

	checkArity(op, args);
	n = asInteger(CAR(args));
	if (n <= 0 || n == NA_INTEGER)
	    error(_("invalid number of points in locator()"));
	args = CDR(args);
	if (isString(CAR(args)) && LENGTH(CAR(args)) == 1)
	    stype = CAR(args);
	else
	    errorcall(call, _("invalid plot type"));
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

static void drawLabel(double xi, double yi, int pos, double offset, char *l,
		      DevDesc *dd)
{
    switch (pos) {
    case 4:
	xi = xi+offset;
	GText(xi, yi, INCHES, l, 0.0,
	      Rf_gpptr(dd)->yCharOffset, 0.0, dd);
	break;
    case 2:
	xi = xi-offset;
	GText(xi, yi, INCHES, l, 1.0,
	      Rf_gpptr(dd)->yCharOffset, 0.0, dd);
	break;
    case 3:
	yi = yi+offset;
	GText(xi, yi, INCHES, l, 0.5,
	      0.0, 0.0, dd);
	break;
    case 1:
	yi = yi-offset;
	GText(xi, yi, INCHES, l, 0.5,
	      1-(0.5-Rf_gpptr(dd)->yCharOffset),
	      0.0, dd);
	break;
    case 0:
	GText(xi, yi, INCHES, l, 0.0, 0.0, 0.0, dd);
	break;
    }
}

/* This manages R_Visibile */
SEXP attribute_hidden do_identify(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, x, y, l, ind, pos, Offset, draw, saveans;
    double xi, yi, xp, yp, d, dmin, offset, tol;
    int atpen, i, imin, k, n, npts, plot, posi, warn;
    DevDesc *dd = CurrentDevice();

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
	n = length(x);
	/*
	 * Most of the appropriate settings have been set up in
	 * R code by par(...)
	 * Hence no GSavePars() or ProcessInlinePars() here
	 * (also because this function is unusual in that it does
	 *  different things when run by a user compared to when
	 *  run from the display list)
	 * BUT par(cex) only sets cexbase, so here we set cex from cexbase
	 */
	Rf_gpptr(dd)->cex = Rf_gpptr(dd)->cexbase;
	offset = GConvertXUnits(asReal(Offset), CHARS, INCHES, dd);
	for (i = 0; i < n; i++) {
	    plot = LOGICAL(ind)[i];
	    if (LOGICAL(draw)[0] && plot) {
		xi = REAL(x)[i];
		yi = REAL(y)[i];
		GConvert(&xi, &yi, USER, INCHES, dd);
		posi = INTEGER(pos)[i];
		drawLabel(xi, yi, posi, offset,
			  translateChar(STRING_ELT(l, i)), dd);
	    }
	}
	return R_NilValue;
    }
    else {
	GCheckState(dd);

	checkArity(op, args);
	x = CAR(args); args = CDR(args);
	y = CAR(args); args = CDR(args);
	l = CAR(args); args = CDR(args);
	npts = asInteger(CAR(args)); args = CDR(args);
	plot = asLogical(CAR(args)); args = CDR(args);
	Offset = CAR(args); args = CDR(args);
	tol = asReal(CAR(args)); args = CDR(args);
	atpen = asLogical(CAR(args));
	if (npts <= 0 || npts == NA_INTEGER)
	    error(_("invalid number of points in identify()"));
	if (!isReal(x) || !isReal(y) || !isString(l) || !isReal(Offset))
	    errorcall(call, _("incorrect argument type"));
	if (tol <= 0 || ISNAN(tol))
	    errorcall(call, _("invalid '%s' value"), "tolerance");
	if (plot == NA_LOGICAL)
	    errorcall(call, _("invalid '%s' value"), "plot");
	if (atpen == NA_LOGICAL)
	    errorcall(call, _("invalid '%s' value"), "atpen");
	if (LENGTH(x) != LENGTH(y) || LENGTH(x) != LENGTH(l))
	    errorcall(call, _("different argument lengths"));
	n = LENGTH(x);
	if (n <= 0) {
	    R_Visible = FALSE;
	    return NULL;
	}

	/*
	 * Most of the appropriate settings have been set up in
	 * R code by par(...)
	 * Hence no GSavePars() or ProcessInlinePars() here
	 * (also because this function is unusual in that it does
	 *  different things when run by a user compared to when
	 *  run from the display list)
	 * BUT par(cex) only sets cexbase, so here we set cex from cexbase
	 */
	Rf_gpptr(dd)->cex = Rf_gpptr(dd)->cexbase;
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
	    Rf_gpptr(dd)->cex = Rf_gpptr(dd)->cexbase;
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
	    warn = asInteger(GetOption(install("warn"), R_BaseEnv));
	    if (dmin > tol) {
	        if(warn >= 0) {
		    REprintf(_("warning: no point with %.2f inches\n"), tol);
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
		if (plot)
		    drawLabel(xi, yi, INTEGER(pos)[imin], offset,
			      translateChar(STRING_ELT(l, imin)), dd);
	    }
	}
	GMode(0, dd);
	PROTECT(ans = allocList(2));
	SETCAR(ans, ind);
	SETCADR(ans, pos);
	PROTECT(saveans = allocList(7));
	SETCAR(saveans, ind);
	SETCADR(saveans, pos);
	SETCADDR(saveans, x);
	SETCADDDR(saveans, y);
	SETCAD4R(saveans, Offset);
	SETCAD4R(CDR(saveans), l);
	PROTECT(draw = allocVector(LGLSXP, 1));
	LOGICAL(draw)[0] = plot;
	SETCAD4R(CDDR(saveans), draw);

	/* If we are recording, save enough information to be able to
	   redraw the text labels beside identified points */
	if (GRecording(call, dd))
	    recordGraphicOperation(op, saveans, dd);
	UNPROTECT(7);

	R_Visible = TRUE;
	return ans;
    }
}

/* strheight(str, units)  ||  strwidth(str, units) */
#define DO_STR_DIM(KIND) 						\
{									\
    SEXP ans, str, ch;							\
    int i, n, units;							\
    double cex, cexsave;						\
    DevDesc *dd = CurrentDevice();					\
									\
    checkArity(op, args);						\
    /* GCheckState(dd); */						\
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
	errorcall(call, _("invalid units"));				\
    args = CDR(args);							\
									\
    if (isNull(CAR(args)))						\
	cex = Rf_gpptr(dd)->cex;					\
    else if (!R_FINITE((cex = asReal(CAR(args)))) || cex <= 0.0)       	\
	errorcall(call, _("invalid '%s' value"), "cex");	       	\
									\
    n = LENGTH(str);							\
    PROTECT(ans = allocVector(REALSXP, n));				\
    cexsave = Rf_gpptr(dd)->cex;					\
    Rf_gpptr(dd)->cex = cex * Rf_gpptr(dd)->cexbase;			\
    for (i = 0; i < n; i++)						\
	if (isExpression(str))						\
	    REAL(ans)[i] = GExpression ## KIND(VECTOR_ELT(str, i),	\
					     GMapUnits(units), dd);	\
	else {								\
	    ch = STRING_ELT(str, i);					\
	    REAL(ans)[i] = (ch == NA_STRING) ? 0.0 :			\
		GStr ## KIND(CHAR(ch), GMapUnits(units), dd);		\
	}								\
    Rf_gpptr(dd)->cex = cexsave;					\
    UNPROTECT(2);							\
    return ans;								\
}

SEXP attribute_hidden do_strheight(SEXP call, SEXP op, SEXP args, SEXP env)
DO_STR_DIM(Height)

SEXP attribute_hidden do_strwidth (SEXP call, SEXP op, SEXP args, SEXP env)
DO_STR_DIM(Width)

#undef DO_STR_DIM


static int *dnd_lptr;
static int *dnd_rptr;
static double *dnd_hght;
static double *dnd_xpos;
static double dnd_hang;
static double dnd_offset;

static void drawdend(int node, double *x, double *y, SEXP dnd_llabels,
		     DevDesc *dd)
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
		  translateChar(STRING_ELT(dnd_llabels, -k-1)),
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
		  translateChar(STRING_ELT(dnd_llabels, -k-1)),
		  1.0, 0.3, 90.0, dd);
    }
    xx[0] = xl; yy[0] = yl;
    xx[1] = xl; yy[1] = *y;
    xx[2] = xr; yy[2] = *y;
    xx[3] = xr; yy[3] = yr;
    GPolyline(4, xx, yy, USER, dd);
    *x = 0.5 * (xl + xr);
}


SEXP attribute_hidden do_dend(SEXP call, SEXP op, SEXP args, SEXP env)
{
    double x, y;
    int n;

    SEXP originalArgs, dnd_llabels;
    DevDesc *dd;

    dd = CurrentDevice();
    GCheckState(dd);

    originalArgs = args;
    if (length(args) < 6)
	errorcall(call, _("too few arguments"));

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
    dnd_xpos = REAL(coerceVector(CAR(args),REALSXP));
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
    ProcessInlinePars(args, dd, call);
    Rf_gpptr(dd)->cex = Rf_gpptr(dd)->cexbase * Rf_gpptr(dd)->cex;
    dnd_offset = GConvertYUnits(GStrWidth("m", INCHES, dd), INCHES, USER, dd);

    /* override par("xpd") and force clipping to figure region */
    /* NOTE: don't override to _reduce_ clipping region */
    if (Rf_gpptr(dd)->xpd < 1)
	Rf_gpptr(dd)->xpd = 1;

    GMode(1, dd);
    drawdend(n, &x, &y, dnd_llabels, dd);
    GMode(0, dd);
    GRestorePars(dd);
    /* NOTE: only record operation if no "error"  */
    if (GRecording(call, dd))
	recordGraphicOperation(op, originalArgs, dd);
    return R_NilValue;

  badargs:
    error(_("invalid dendrogram input"));
    return R_NilValue;/* never used; to keep -Wall happy */
}

SEXP attribute_hidden do_dendwindow(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i, imax, n;
    double pin, *ll, tmp, yval, *y, ymin, ymax, yrange, m;
    SEXP originalArgs, merge, height, llabels, str;
    char *vmax;
    DevDesc *dd;

    dd = CurrentDevice();
    GCheckState(dd);
    originalArgs = args;
    if (length(args) < 5)
	errorcall(call, _("too few arguments"));
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
    ProcessInlinePars(args, dd, call);
    Rf_gpptr(dd)->cex = Rf_gpptr(dd)->cexbase * Rf_gpptr(dd)->cex;
    dnd_offset = GStrWidth("m", INCHES, dd);
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
    pin = Rf_gpptr(dd)->pin[1];
    for (i = 0; i <= n; i++) {
	str = STRING_ELT(llabels, i);
	ll[i] = (str == NA_STRING) ? 0.0 :
	    GStrWidth(translateChar(str), INCHES, dd) + dnd_offset;
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
    /* NOTE: only record operation if no "error"  */
    if (GRecording(call, dd))
	recordGraphicOperation(op, originalArgs, dd);
    vmaxset(vmax);
    return R_NilValue;
  badargs:
    error(_("invalid dendrogram input"));
    return R_NilValue;/* never used; to keep -Wall happy */
}


SEXP attribute_hidden do_erase(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP col;
    int ncol;
    DevDesc *dd = CurrentDevice();
    checkArity(op, args);
    PROTECT(col = FixupCol(CAR(args), R_TRANWHITE));
    ncol = LENGTH(col);
    GSavePars(dd);
    GMode(1, dd);
    GRect(0.0, 0.0, 1.0, 1.0, NDC, INTEGER(col)[0], R_TRANWHITE, dd);
    GMode(0, dd);
    GRestorePars(dd);
    UNPROTECT(1);
    return R_NilValue;
}


SEXP attribute_hidden do_getSnapshot(SEXP call, SEXP op, SEXP args, SEXP env)
{
    DevDesc *dd = CurrentDevice();

    checkArity(op, args);
    if (dd->newDevStruct) {
	return GEcreateSnapshot((GEDevDesc*) dd);
    } else {
	errorcall(call, _("cannot take snapshot of old-style device"));
	return R_NilValue;
    }
}

SEXP attribute_hidden do_playSnapshot(SEXP call, SEXP op, SEXP args, SEXP env)
{
    DevDesc *dd = CurrentDevice();

    checkArity(op, args);
    if (dd->newDevStruct)
	GEplaySnapshot(CAR(args), (GEDevDesc*) dd);
    else
	errorcall(call, _("cannot play snapshot on old-style device"));
    return R_NilValue;
}

/* I don't think this gets called in any base R code
 */
SEXP attribute_hidden do_replay(SEXP call, SEXP op, SEXP args, SEXP env)
{
    if (!NoDevices()) {
	GEDevDesc *dd = GEcurrentDevice();
	checkArity(op, args);
	/*     Rf_dpptr(dd)->resize(); */
	GEplayDisplayList(dd);
    }
    return R_NilValue;
}

SEXP attribute_hidden do_playDL(SEXP call, SEXP op, SEXP args, SEXP env)
{
    DevDesc *dd = CurrentDevice();
    SEXP theList;
    int ask;

    checkArity(op, args);
    if(!isList(theList = CAR(args)))
       errorcall(call, _("invalid argument"));
    if (dd->newDevStruct)
	((GEDevDesc*) dd)->dev->displayList = theList;
    else
	dd->displayList = theList;
    if (theList != R_NilValue) {
	ask = Rf_gpptr(dd)->ask;
	Rf_gpptr(dd)->ask = 1;
	GReset(dd);
	while (theList != R_NilValue) {
	    SEXP theOperation = CAR(theList);
	    SEXP l_op = CAR(theOperation);
	    SEXP l_args = CDR(theOperation);
	    PRIMFUN(l_op) (R_NilValue, l_op, l_args, R_NilValue);
	    if (!Rf_gpptr(dd)->valid) break;
	    theList = CDR(theList);
	}
	Rf_gpptr(dd)->ask = ask;
    }
    return R_NilValue;
}

SEXP attribute_hidden do_setGPar(SEXP call, SEXP op, SEXP args, SEXP env)
{
    DevDesc *dd = CurrentDevice();
    int lGPar = 1 + sizeof(GPar) / sizeof(int);
    SEXP GP;

    checkArity(op, args);
    GP = CAR(args);
    if (!isInteger(GP) || length(GP) != lGPar)
	errorcall(call, _("invalid graphics parameter list"));
    copyGPar((GPar *) INTEGER(GP), Rf_dpSavedptr(dd)); /* &dd->Rf_dpSaved); */
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
	errorcall(call, _("invalid symbol parameter vector"));
}

/* Internal  symbols(x, y, type, data, inches, bg, fg, ...) */
SEXP attribute_hidden do_symbols(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, y, p, fg, bg;
    int i, j, nr, nc, nbg, nfg, type;
    double pmax, pmin, inches, rx, ry;
    double xx, yy, p0, p1, p2, p3, p4;
    double *pp, *xp, *yp;
    char *vmax;

    SEXP originalArgs = args;
    DevDesc *dd = CurrentDevice();
    GCheckState(dd);

    if (length(args) < 7)
	errorcall(call, _("too few arguments"));

    PROTECT(x = coerceVector(CAR(args), REALSXP)); args = CDR(args);
    PROTECT(y = coerceVector(CAR(args), REALSXP)); args = CDR(args);
    if (!isNumeric(x) || !isNumeric(y) || length(x) <= 0 || LENGTH(x) <= 0)
	errorcall(call, _("invalid symbol coordinates"));

    type = asInteger(CAR(args)); args = CDR(args);

    /* data: */
    p = PROTECT(coerceVector(CAR(args), REALSXP)); args = CDR(args);
    CheckSymbolPar(call, p, &nr, &nc);
    if (LENGTH(x) != nr || LENGTH(y) != nr)
	errorcall(call, _("x/y/parameter length mismatch"));

    inches = asReal(CAR(args)); args = CDR(args);
    if (!R_FINITE(inches) || inches < 0)
	inches = 0;

    PROTECT(bg = FixupCol(CAR(args), R_TRANWHITE)); args = CDR(args);
    nbg = LENGTH(bg);

    PROTECT(fg = FixupCol(CAR(args), R_TRANWHITE)); args = CDR(args);
    nfg = LENGTH(fg);

    GSavePars(dd);
    ProcessInlinePars(args, dd, call);

    GMode(1, dd);
    switch (type) {
    case 1: /* circles */
	if (nc != 1)
	    errorcall(call, _("invalid circles data"));
	if (!SymbolRange(REAL(p), nr, &pmax, &pmin))
	    errorcall(call, _("invalid symbol parameter"));
	for (i = 0; i < nr; i++) {
	    if (R_FINITE(REAL(x)[i]) && R_FINITE(REAL(y)[i]) &&
		R_FINITE(REAL(p)[i])) {
		rx = REAL(p)[i];
		/* For GCircle the radius is always in INCHES */
		if (inches > 0)
		    rx *= inches / pmax;
		else
		    rx = GConvertXUnits(rx, USER, INCHES, dd);
		GCircle(REAL(x)[i], REAL(y)[i],	USER, rx,
			INTEGER(bg)[i%nbg], INTEGER(fg)[i%nfg],	dd);
	    }
	}
	break;
    case 2: /* squares */
	if(nc != 1)
	    errorcall(call, _("invalid squares data"));
	if(!SymbolRange(REAL(p), nr, &pmax, &pmin))
	    errorcall(call, _("invalid symbol parameter"));
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
		GRect(xx - rx, yy - rx, xx + rx, yy + rx, DEVICE,
		      INTEGER(bg)[i%nbg], INTEGER(fg)[i%nfg], dd);
	    }
	}
	break;
    case 3: /* rectangles */
	if (nc != 2)
	    errorcall(call, _("invalid rectangles data (need 2 columns)"));
	if (!SymbolRange(REAL(p), 2 * nr, &pmax, &pmin))
	    errorcall(call, _("invalid symbol parameter"));
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
		GRect(xx - rx, yy - ry, xx + rx, yy + ry, DEVICE,
		      INTEGER(bg)[i%nbg], INTEGER(fg)[i%nfg], dd);

	    }
	}
	break;
    case 4: /* stars */
	if (nc < 3)
	    errorcall(call, _("invalid stars data"));
	if (!SymbolRange(REAL(p), nc * nr, &pmax, &pmin))
	    errorcall(call, _("invalid symbol parameter"));
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
	    errorcall(call,
		      _("invalid thermometers data (need 3 or 4 columns)"));
	SymbolRange(REAL(p)+2*nr/* <-- pointer arith*/, nr, &pmax, &pmin);
	if (pmax < pmin)
	    errorcall(call, _("invalid thermometers[,%s]"),
		      (nc == 4)? "3:4" : "3");
	if (pmin < 0. || pmax > 1.) /* S-PLUS has an error here */
	    warningcall(call,
			_("thermometers[,%s] not in [0,1] -- may look funny"),
			(nc == 4)? "3:4" : "3");
	if (!SymbolRange(REAL(p), 2 * nr, &pmax, &pmin))
	    errorcall(call, _("invalid thermometers[,1:2]"));
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
    case 6: /* boxplots (wid, hei, loWhsk, upWhsk, medProp) */
	if (nc != 5)
	    errorcall(call, _("invalid boxplots data (need 5 columns)"));
	pmax = -DBL_MAX;
	pmin =	DBL_MAX;
	for(i = 0; i < nr; i++) {
	    p4 = REAL(p)[i + 4 * nr];	/* median proport. in [0,1] */
	    if (pmax < p4) pmax = p4;
	    if (pmin > p4) pmin = p4;
	}
	if (pmin < 0. || pmax > 1.) /* S-PLUS has an error here */
	    warningcall(call,
			_("boxplots[,5] outside [0,1] -- may look funny"));
	if (!SymbolRange(REAL(p), 4 * nr, &pmax, &pmin))
	    errorcall(call, _("invalid boxplots[, 1:4]"));
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
	errorcall(call, _("invalid symbol type"));
    }
    GMode(0, dd);
    GRestorePars(dd);
    if (GRecording(call, dd))
	recordGraphicOperation(op, originalArgs, dd);
    UNPROTECT(5);
    return R_NilValue;
}
