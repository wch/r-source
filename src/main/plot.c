/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2012  The R Core Team
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
#include <Colors.h> /* for isNAcol */
#include <Print.h>

#define imax2(x, y) ((x < y) ? y : x)

/*  P A R A M E T E R	 U T I L I T I E S  */

/* used on do_contour, graphics/src */
attribute_hidden
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

attribute_hidden
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
	if (typeface < 1 || typeface > 8)
	    error(_("invalid 'vfont' value [typeface]"));
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

/* used in graphics and grid */
SEXP CreateAtVector(double *axp, double *usr, int nint, Rboolean logflag)
{
/*	Create an  'at = ...' vector for  axis(.)
 *	i.e., the vector of tick mark locations,
 *	when none has been specified (= default).
 *
 *	axp[0:2] = (x1, x2, nInt), where x1..x2 are the extreme tick marks
 *		   {unless in log case, where nInt \in {1,2,3 ; -1,-2,....}
 *		    and the `nint' argument is used *instead*.}

 *	The resulting REAL vector must have length >= 1, ideally >= 2
 */
    SEXP at = R_NilValue;/* -Wall*/
    double umin, umax, dn, rng, small;
    int i, n, ne;
    if (!logflag || axp[2] < 0) { /* --- linear axis --- Only use axp[] arg. */
	n = (int)(fabs(axp[2]) + 0.25);/* >= 0 */
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

	n = (int)(axp[2] + 0.5);
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
	/* allow a fuzz since we will do things like 0.2*dn >= umin */
	umin *= 1 - 1e-12;
	umax *= 1 + 1e-12;

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
	    i = (int)(floor(log10(axp[1])) - ceil(log10(axp[0])) + 0.25);
	    ne = i / nint + 1;
#ifdef DEBUG_axis
	    REprintf("CreateAtVector [log-axis(), case 1]: (nint, ne) = (%d,%d)\n",
		     nint, ne);
#endif
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

/* in Graphics.h, used in contour and in graphics/src */
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


    /* GRAPHICS FUNCTION ENTRY POINTS */


SEXP attribute_hidden do_plot_new(SEXP call, SEXP op, SEXP args, SEXP env)
{
    /* plot.new() - create a new plot "frame" */

    pGEDevDesc dd;

    checkArity(op, args);

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

static void drawPointsLines(double xp, double yp, double xold, double yold,
			    char type, int first, pGEDevDesc dd)
{
    if (type == 'p' || type == 'o')
	GSymbol(xp, yp, DEVICE, gpptr(dd)->pch, dd);
    if ((type == 'l' || type == 'o') && !first)
	GLine(xold, yold, xp, yp, DEVICE, dd);
}


SEXP attribute_hidden do_locator(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, y, nobs, ans, saveans, stype = R_NilValue;
    int i, n;
    char type = 'p';
    double xp, yp, xold=0, yold=0;
    pGEDevDesc dd = GEcurrentDevice();

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
	PROTECT(saveans = allocList(4));
	SETCAR(saveans, x);
	SETCADR(saveans, y);
	SETCADDR(saveans, nobs);
	SETCADDDR(saveans, CAR(args));
	/* Record the points and lines that were drawn in the display list */
	GErecordGraphicOperation(op, saveans, dd);
	UNPROTECT(5);
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

/* This manages R_Visible */
SEXP attribute_hidden do_identify(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, x, y, l, ind, pos, Offset, draw, saveans;
    double xi, yi, xp, yp, d, dmin, offset, tol;
    int atpen, i, imin, k, n, nl, npts, plot, posi, warn;
    pGEDevDesc dd = GEcurrentDevice();

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
	    error(_("incorrect argument type"));
	if (tol <= 0 || ISNAN(tol))
	    error(_("invalid '%s' value"), "tolerance");
	if (plot == NA_LOGICAL)
	    error(_("invalid '%s' value"), "plot");
	if (atpen == NA_LOGICAL)
	    error(_("invalid '%s' value"), "atpen");
	nl = LENGTH(l);
	if (nl <= 0)
	    error(_("zero length 'labels'"));
	n = LENGTH(x);
	if (n != LENGTH(y))
	    error(_("different argument lengths"));
	if (nl > n)
	    warning(_("more 'labels' than points"));
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
	PROTECT(saveans = allocList(7));
	SETCAR(saveans, ind);
	SETCADR(saveans, pos);
	SETCADDR(saveans, x);
	SETCADDDR(saveans, y);
	SETCAD4R(saveans, Offset);
	SETCAD4R(CDR(saveans), l);
	SETCAD4R(CDDR(saveans), ScalarLogical(plot));

	/* If we are recording, save enough information to be able to
	   redraw the text labels beside identified points */
	if (GRecording(call, dd))
	    GErecordGraphicOperation(op, saveans, dd);
	UNPROTECT(6);

	R_Visible = TRUE;
	return ans;
    }
}
