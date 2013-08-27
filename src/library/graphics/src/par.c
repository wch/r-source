/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2013  The R Core Team.
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
 *
 *
 *
 *  GRZ-like state information.
 *
 *  This is a quick knock-off of the GRZ library to provide a basic
 *  S-like graphics capability for R.  Basically this bit of code
 *  provides the functionality of the "par" function in S.
 *
 *  "The horror, the horror ..."
 *	Marlon Brando in Apocalypse Now.
 *
 *  Main functions:
 *	do_par(.)	and
 *	do_layout(.)	implement R's  par(.), layout()rely on
 *
 *	Specify(.)	[ par(what = value) ]
 *	Specify2(.)	[ <highlevelplot>(what = value) ]
 *	Query(.)	[ par(what) ]
 */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Rmath.h>
#include <Graphics.h>		/* "GPar" structure + COMMENTS */

#include "graphics.h"

typedef struct {
    char *name;
    int code; /* 0 normal, 1 not inline, 2 read-only
		 -1 unknown, -2 obselete, -3 graphical args
	       */
} ParTab;

static const ParTab
ParTable  [] = {
    { "adj",		 0 },
    { "ann",		 0 },
    { "ask",		 1 },
    { "bg",		 0 },
    { "bty",		 0 },
    { "cex",		 0 },
    { "cex.axis",	 0 },
    { "cex.lab",	 0 },
    { "cex.main",	 0 },
    { "cex.sub",	 0 },
    { "cin",		 2 },
    { "col",		 0 },
    { "col.axis",	 0 },
    { "col.lab",	 0 },
    { "col.main",	 0 },
    { "col.sub",	 0 },
    { "cra",		 2 },
    { "crt",		 0 },
    { "csi",		 2 },
    { "csy",		 0 },
    { "cxy",		 2 },
    { "din",		 2 },
    { "err",		 0 },
    { "family",		 0 },
    { "fg",		 0 },
    { "fig",		 1 },
    { "fin",		 1 },
    { "font",		 0 },
    { "font.axis",	 0 },
    { "font.lab",	 0 },
    { "font.main",	 0 },
    { "font.sub",	 0 },
    { "lab",		 0 },
    { "las",		 0 },
    { "lend",		 0 },
    { "lheight",	 1 },
    { "ljoin",		 0 },
    { "lmitre",		 0 },
    { "lty",		 0 },
    { "lwd",		 0 },
    { "mai",		 1 },
    { "mar",		 1 },
    { "mex",		 1 },
    { "mfcol",		 1 },
    { "mfg",		 1 },
    { "mfrow",		 1 },
    { "mgp",		 0 },
    { "mkh",		 0 },
    { "new",		 1 },
    { "oma",		 1 },
    { "omd",		 1 },
    { "omi",		 1 },
    { "page",            2 },
    { "pch",		 0 },
    { "pin",		 1 },
    { "plt",		 1 },
    { "ps",		 1 },
    { "pty",		 1 },
    { "smo",		 0 },
    { "srt",		 0 },
    { "tck",		 0 },
    { "tcl",		 0 },
    { "usr",		 1 },
    { "xaxp",		 0 },
    { "xaxs",		 0 },
    { "xaxt",		 0 },
    { "xlog",		 1 },
    { "xpd",		 0 },
    { "yaxp",		 0 },
    { "yaxs",		 0 },
    { "yaxt",		 0 },
    { "ylbias",		 1 },
    { "ylog",		 1 },
    /* Obsolete pars */
    { "gamma",		-2},
    { "type",		-2},
    { "tmag",           -2},
    /* Non-pars that might get passed to Specify2 */
    { "asp",		-3},
    { "main",		-3},
    { "sub",		-3},
    { "xlab",		-3},
    { "ylab",		-3},
    { "xlim",		-3},
    { "ylim",		-3},
    { NULL,		-1}
};


static int ParCode(const char *what)
{
    int i;
    for (i = 0; ParTable[i].name; i++)
	if (!strcmp(what, ParTable[i].name)) return ParTable[i].code;
    return -1;
}


static void par_error(const char *what)
{
    error(_("invalid value specified for graphical parameter \"%s\""),  what);
}


static void lengthCheck(const char *what, SEXP v, int n)
{
    if (length(v) != n)
	error(_("graphical parameter \"%s\" has the wrong length"), what);
}


static void nonnegIntCheck(int x, const char *s)
{
    if (x == NA_INTEGER || x < 0)
	par_error(s);
}

static void posIntCheck(int x, const char *s)
{
    if (x == NA_INTEGER || x <= 0)
	par_error(s);
}

static void posRealCheck(double x, const char *s)
{
    if (!R_FINITE(x) || x <= 0)
	par_error(s);
}

static void nonnegRealCheck(double x, const char *s)
{
    if (!R_FINITE(x) || x < 0)
	par_error(s);
}

static void naRealCheck(double x, const char *s)
{
    if (!R_FINITE(x))
	par_error(s);
}

static void logAxpCheck(int x, const char *s)
{
    if (x == NA_INTEGER || x == 0 || x > 4)
	par_error(s);
}


static void BoundsCheck(double x, double a, double b, const char *s)
{
/* Check if   a <= x <= b */
    if (!R_FINITE(x) || (R_FINITE(a) && x < a) || (R_FINITE(b) && x > b))
	par_error(s);
}


/* When any one of the layout parameters (which can only be set via */
/* par(...)) is modified, must call GReset() to update the layout and */
/* the transformations between coordinate systems */

/* These will be defined differently for Specify() and Specify2() : */
/* <FIXME>  do not need separate macros for a = b = c and b = a = c */
#define R_DEV__(_P_) dpptr(dd)->_P_ = gpptr(dd)->_P_
#define R_DEV_2(_P_) gpptr(dd)->_P_ = dpptr(dd)->_P_
/* In Emacs : -- only inside Specify() :
 *  (query-replace-regexp
     "dpptr(dd)->\\([][A-Za-z0-9]+\\) = gpptr(dd)->\\(\\1\\)"
     "R_DEV__(\\1)" nil nil nil)

   (query-replace-regexp
     "gpptr(dd)->\\([][A-Za-z0-9]+\\) = dpptr(dd)->\\(\\1\\)"
     "R_DEV_2(\\1)" nil nil nil)
*/

static void Specify(const char *what, SEXP value, pGEDevDesc dd)
{
/* If you ADD a NEW par, then do NOT forget to update the code in
 *			 ../library/base/R/par.R

 * Parameters in Specify(),
 * which can*not* be specified in high-level functions,
 * i.e., by Specify2() [below]:
 *	this list is in \details{.} of ../library/base/man/par.Rd
 *	------------------------
 *	"ask",
 *	"family", "fig", "fin",
 *      "lheight",
 *	"mai", "mar", "mex", "mfrow", "mfcol", "mfg",
 *	"new",
 *	"oma", "omd", "omi",
 *	"pin", "plt", "ps", "pty"
 *	"usr",
 *	"xlog", "ylog"
 *	"ylbias",
 */
    double x;
    int ix = 0;
    char cx = '\0';

    /* If we get here, Query has already checked that 'what' is valid */

    if (ParCode(what) == 2) {
	warning(_("graphical parameter \"%s\" cannot be set"), what);
	return;
    }
#define FOR_PAR
#include "par-common.c"
#undef FOR_PAR
/*	  ------------ */
    else if (streql(what, "bg")) {
	lengthCheck(what, value, 1);
	ix = RGBpar3(value, 0, dpptr(dd)->bg);
	/*	naIntCheck(ix, what); */
	R_DEV__(bg) = ix;
	R_DEV__(new) = FALSE;
    }
/*--- and these are "Specify() only" {i.e. par(nam = val)} : */
    else if (streql(what, "ask")) {
	lengthCheck(what, value, 1);	ix = asLogical(value);
	dd->ask = (ix == 1);/* NA |-> FALSE */
    }
    else if (streql(what, "fig")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 4);
	if (0.0 <= REAL(value)[0] && REAL(value)[0] < REAL(value)[1] &&
	    REAL(value)[1] <= 1.0 &&
	    0.0 <= REAL(value)[2] && REAL(value)[2] < REAL(value)[3] &&
	    REAL(value)[3] <= 1.0) {
	    R_DEV_2(defaultFigure) = 0;
	    R_DEV_2(fUnits) = NIC;
	    R_DEV_2(numrows) = 1;
	    R_DEV_2(numcols) = 1;
	    R_DEV_2(heights[0]) = 1;
	    R_DEV_2(widths[0]) = 1;
	    R_DEV_2(cmHeights[0]) = 0;
	    R_DEV_2(cmWidths[0]) = 0;
	    R_DEV_2(order[0]) = 1;
	    R_DEV_2(currentFigure) = 1;
	    R_DEV_2(lastFigure) = 1;
	    R_DEV__(rspct) = 0;

	    R_DEV_2(fig[0]) = REAL(value)[0];
	    R_DEV_2(fig[1]) = REAL(value)[1];
	    R_DEV_2(fig[2]) = REAL(value)[2];
	    R_DEV_2(fig[3]) = REAL(value)[3];
	    GReset(dd);
	}
	else par_error(what);
    }
    else if (streql(what, "fin")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 2);
	R_DEV_2(defaultFigure) = 0;
	R_DEV_2(fUnits) = INCHES;
	R_DEV_2(numrows) = 1;
	R_DEV_2(numcols) = 1;
	R_DEV_2(heights[0]) = 1;
	R_DEV_2(widths[0]) = 1;
	R_DEV_2(cmHeights[0]) = 0;
	R_DEV_2(cmWidths[0]) = 0;
	R_DEV_2(order[0]) = 1;
	R_DEV_2(currentFigure) = 1;
	R_DEV_2(lastFigure) = 1;
	R_DEV__(rspct) = 0;
	R_DEV_2(fin[0]) = REAL(value)[0];
	R_DEV_2(fin[1]) = REAL(value)[1];
	GReset(dd);
    }
    /* -- */
    else if (streql(what, "lheight")) {
	lengthCheck(what, value, 1);
	x = asReal(value);
	posRealCheck(x, what);
	R_DEV__(lheight) = x;
    }
    else if (streql(what, "mai")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 4);
	nonnegRealCheck(REAL(value)[0], what);
	nonnegRealCheck(REAL(value)[1], what);
	nonnegRealCheck(REAL(value)[2], what);
	nonnegRealCheck(REAL(value)[3], what);
	R_DEV__(mai[0]) = REAL(value)[0];
	R_DEV__(mai[1]) = REAL(value)[1];
	R_DEV__(mai[2]) = REAL(value)[2];
	R_DEV__(mai[3]) = REAL(value)[3];
	R_DEV__(mUnits) = INCHES;
	R_DEV__(defaultPlot) = TRUE;
	GReset(dd);
    }
    else if (streql(what, "mar")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 4);
	nonnegRealCheck(REAL(value)[0], what);
	nonnegRealCheck(REAL(value)[1], what);
	nonnegRealCheck(REAL(value)[2], what);
	nonnegRealCheck(REAL(value)[3], what);
	R_DEV__(mar[0]) = REAL(value)[0];
	R_DEV__(mar[1]) = REAL(value)[1];
	R_DEV__(mar[2]) = REAL(value)[2];
	R_DEV__(mar[3]) = REAL(value)[3];
	R_DEV__(mUnits) = LINES;
	R_DEV__(defaultPlot) = TRUE;
	GReset(dd);
    }
    else if (streql(what, "mex")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	posRealCheck(x, what);
	R_DEV__(mex) = x;
	GReset(dd);
    }
    else if (streql(what, "mfrow")) {
	int nrow, ncol;
	value = coerceVector(value, INTSXP);
	lengthCheck(what, value, 2);
	posIntCheck(INTEGER(value)[0], what);
	posIntCheck(INTEGER(value)[1], what);
	nrow = INTEGER(value)[0];
	ncol = INTEGER(value)[1];
	R_DEV_2(numrows) = nrow;
	R_DEV_2(numcols) = ncol;
	R_DEV_2(currentFigure) = nrow*ncol;
	R_DEV_2(lastFigure) = nrow*ncol;
	R_DEV_2(defaultFigure) = TRUE;
	R_DEV_2(layout) = FALSE;
	if (nrow > 2 || ncol > 2) {
	    R_DEV_2(cexbase) = 0.66;
	    R_DEV_2(mex) = 1.0;
	}
	else if (nrow == 2 && ncol == 2) {
	    R_DEV_2(cexbase) = 0.83;
	    R_DEV_2(mex) = 1.0;
	}
	else {
	    R_DEV_2(cexbase) = 1.0;
	    R_DEV_2(mex) = 1.0;
	}
	R_DEV__(mfind) = 0;
	GReset(dd);
    }
    else if (streql(what, "mfcol")) {
	int nrow, ncol;
	value = coerceVector(value, INTSXP);
	lengthCheck(what, value, 2);
	posIntCheck(INTEGER(value)[0], what);
	posIntCheck(INTEGER(value)[1], what);
	nrow = INTEGER(value)[0];
	ncol = INTEGER(value)[1];
	R_DEV_2(numrows) = nrow;
	R_DEV_2(numcols) = ncol;
	R_DEV_2(currentFigure) = nrow*ncol;
	R_DEV_2(lastFigure) = nrow*ncol;
	R_DEV_2(defaultFigure) = TRUE;
	R_DEV_2(layout) = FALSE;
	if (nrow > 2 || ncol > 2) {
	    R_DEV_2(cexbase) = 0.66;
	    R_DEV_2(mex) = 1.0;
	}
	else if (nrow == 2 && ncol == 2) {
	    R_DEV_2(cexbase) = 0.83;
	    R_DEV_2(mex) = 1.0;
	}
	else {
	    R_DEV__(cexbase) = 1.0;
	    R_DEV__(mex) = 1.0;
	}
	R_DEV__(mfind) = 1;
	GReset(dd);
    }
    else if (streql(what, "mfg")) {
	int row, col, nrow, ncol, np;
	value = coerceVector(value, INTSXP);
	np = length(value);
	if(np != 2 && np != 4)
	    error(_("parameter \"mfg\" has the wrong length"));
	posIntCheck(INTEGER(value)[0], what);
	posIntCheck(INTEGER(value)[1], what);
	row = INTEGER(value)[0];
	col = INTEGER(value)[1];
	nrow = dpptr(dd)->numrows;
	ncol = dpptr(dd)->numcols;
	if(row <= 0 || row > nrow)
	    error(_("parameter \"i\" in \"mfg\" is out of range"));
	if(col <= 0 || col > ncol)
	    error(_("parameter \"j\" in \"mfg\" is out of range"));
	if(np == 4) {
	    posIntCheck(INTEGER(value)[2], what);
	    posIntCheck(INTEGER(value)[3], what);
	    if(nrow != INTEGER(value)[2])
		warning(_("value of 'nr' in \"mfg\" is wrong and will be ignored"));
	    if(ncol != INTEGER(value)[3])
		warning(_("value of 'nc' in \"mfg\" is wrong and will be ignored"));
	}
	R_DEV_2(lastFigure) = nrow*ncol;
	/*R_DEV__(mfind) = 1;*/
	/* currentFigure is 1-based */
	if(gpptr(dd)->mfind)
	    dpptr(dd)->currentFigure = (col-1)*nrow + row;
	else dpptr(dd)->currentFigure = (row-1)*ncol + col;
	/*
	  if (dpptr(dd)->currentFigure == 0)
	  dpptr(dd)->currentFigure = dpptr(dd)->lastFigure;
	*/
	R_DEV_2(currentFigure);
	/* R_DEV_2(defaultFigure) = TRUE;
	   R_DEV_2(layout) = FALSE; */
	R_DEV_2(new) = TRUE;
	GReset(dd);
	/* Force a device clip */
	if (dd->dev->canClip) GForceClip(dd);
    } /* mfg */

    else if (streql(what, "new")) {
	lengthCheck(what, value, 1);
	ix = asLogical(value);
	if(!gpptr(dd)->state) {
	    /* no need to warn with new=FALSE and no plot */
	    if(ix != 0) warning(_("calling par(new=TRUE) with no plot"));
	} else R_DEV__(new) = (ix != 0);
    }
    /* -- */

    else if (streql(what, "oma")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 4);
	nonnegRealCheck(REAL(value)[0], what);
	nonnegRealCheck(REAL(value)[1], what);
	nonnegRealCheck(REAL(value)[2], what);
	nonnegRealCheck(REAL(value)[3], what);
	R_DEV__(oma[0]) = REAL(value)[0];
	R_DEV__(oma[1]) = REAL(value)[1];
	R_DEV__(oma[2]) = REAL(value)[2];
	R_DEV__(oma[3]) = REAL(value)[3];
	R_DEV__(oUnits) = LINES;
	/* !!! Force eject of multiple figures !!! */
	R_DEV__(currentFigure) = gpptr(dd)->lastFigure;
	GReset(dd);
    }
    else if (streql(what, "omd")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 4);
	BoundsCheck(REAL(value)[0], 0.0, 1.0, what);
	BoundsCheck(REAL(value)[1], 0.0, 1.0, what);
	BoundsCheck(REAL(value)[2], 0.0, 1.0, what);
	BoundsCheck(REAL(value)[3], 0.0, 1.0, what);
	R_DEV__(omd[0]) = REAL(value)[0];
	R_DEV__(omd[1]) = REAL(value)[1];
	R_DEV__(omd[2]) = REAL(value)[2];
	R_DEV__(omd[3]) = REAL(value)[3];
	R_DEV__(oUnits) = NDC;
	/* Force eject of multiple figures */
	R_DEV__(currentFigure) = gpptr(dd)->lastFigure;
	GReset(dd);
    }
    else if (streql(what, "omi")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 4);
	nonnegRealCheck(REAL(value)[0], what);
	nonnegRealCheck(REAL(value)[1], what);
	nonnegRealCheck(REAL(value)[2], what);
	nonnegRealCheck(REAL(value)[3], what);
	R_DEV__(omi[0]) = REAL(value)[0];
	R_DEV__(omi[1]) = REAL(value)[1];
	R_DEV__(omi[2]) = REAL(value)[2];
	R_DEV__(omi[3]) = REAL(value)[3];
	R_DEV__(oUnits) = INCHES;
	/* Force eject of multiple figures */
	R_DEV__(currentFigure) = gpptr(dd)->lastFigure;
	GReset(dd);
    }
    /* -- */

    else if (streql(what, "pin")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 2);
	nonnegRealCheck(REAL(value)[0], what);
	nonnegRealCheck(REAL(value)[1], what);
	R_DEV__(pin[0]) = REAL(value)[0];
	R_DEV__(pin[1]) = REAL(value)[1];
	R_DEV__(pUnits) = INCHES;
	R_DEV__(defaultPlot) = FALSE;
	GReset(dd);
    }
    else if (streql(what, "plt")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 4);
	nonnegRealCheck(REAL(value)[0], what);
	nonnegRealCheck(REAL(value)[1], what);
	nonnegRealCheck(REAL(value)[2], what);
	nonnegRealCheck(REAL(value)[3], what);
	R_DEV__(plt[0]) = REAL(value)[0];
	R_DEV__(plt[1]) = REAL(value)[1];
	R_DEV__(plt[2]) = REAL(value)[2];
	R_DEV__(plt[3]) = REAL(value)[3];
	R_DEV__(pUnits) = NFC;
	R_DEV__(defaultPlot) = FALSE;
	GReset(dd);
    }
    else if (streql(what, "ps")) {
	lengthCheck(what, value, 1);	ix = asInteger(value);
	nonnegIntCheck(ix, what);
	R_DEV__(ps) = ix;
    }
    else if (streql(what, "pty")) {
	if (!isString(value) || LENGTH(value) < 1)
	    par_error(what);
	cx = CHAR(STRING_ELT(value, 0))[0];
	if (cx == 'm' || cx == 's') {
	    R_DEV__(pty) = cx;
	    R_DEV__(defaultPlot) = TRUE;
	}
	else par_error(what);
    }
    /* -- */
    else if (streql(what, "usr")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 4);
	naRealCheck(REAL(value)[0], what);
	naRealCheck(REAL(value)[1], what);
	naRealCheck(REAL(value)[2], what);
	naRealCheck(REAL(value)[3], what);
	if (REAL(value)[0] == REAL(value)[1] ||
	    REAL(value)[2] == REAL(value)[3])
	    par_error(what);
	if (gpptr(dd)->xlog) {
	    R_DEV_2(logusr[0]) = REAL(value)[0];
	    R_DEV_2(logusr[1]) = REAL(value)[1];
	    R_DEV_2(usr[0]) = pow(10., REAL(value)[0]);
	    R_DEV_2(usr[1]) = pow(10., REAL(value)[1]);
	}
	else {
	    R_DEV_2(usr[0]) = REAL(value)[0];
	    R_DEV_2(usr[1]) = REAL(value)[1];
	    R_DEV_2(logusr[0]) = R_Log10(REAL(value)[0]);
	    R_DEV_2(logusr[1]) = R_Log10(REAL(value)[1]);
	}
	if (gpptr(dd)->ylog) {
	    R_DEV_2(logusr[2]) = REAL(value)[2];
	    R_DEV_2(logusr[3]) = REAL(value)[3];
	    R_DEV_2(usr[2]) = pow(10., REAL(value)[2]);
	    R_DEV_2(usr[3]) = pow(10., REAL(value)[3]);
	}
	else {
	    R_DEV_2(usr[2]) = REAL(value)[2];
	    R_DEV_2(usr[3]) = REAL(value)[3];
	    R_DEV_2(logusr[2]) = R_Log10(REAL(value)[2]);
	    R_DEV_2(logusr[3]) = R_Log10(REAL(value)[3]);
	}
	/* Reset Mapping and Axis Parameters */
	GMapWin2Fig(dd);
	GSetupAxis(1, dd);
	GSetupAxis(2, dd);
    }/* usr */

    else if (streql(what, "xlog")) {
	lengthCheck(what, value, 1);	ix = asLogical(value);
	if (ix == NA_LOGICAL)
	    par_error(what);
	R_DEV__(xlog) = (ix != 0);
    }
    else if (streql(what, "ylog")) {
	lengthCheck(what, value, 1);	ix = asLogical(value);
	if (ix == NA_LOGICAL)
	    par_error(what);
	R_DEV__(ylog) = (ix != 0);
    }
    else if (streql(what, "ylbias")) {
	lengthCheck(what, value, 1);
	dd->dev->yLineBias = asReal(value);
    }
    /* We do not need these as Query will already have warned.
    else if (streql(what, "type")) {
	warning(_("graphical parameter \"%s\" is obsolete"), what);
    }
    else warning(_("unknown graphical parameter \"%s\""), what);
    */

    return;
} /* Specify */


/* Specify2 -- parameters as arguments from higher-level graphics functions
 * --------
 * Many things in PARALLEL to Specify(.)
 * for par()s not valid here, see comment there.
 */
#undef R_DEV_2
#undef R_DEV__
/* Now defined differently in Specify2() : */
#define R_DEV__(_P_) gpptr(dd)->_P_

static void Specify2(const char *what, SEXP value, pGEDevDesc dd)
{
    double x;
    int ix = 0, ptype = ParCode(what);
    char cx = '\0';

    if (ptype == 1 || ptype == -3) {
	/* 1: these are valid, but not settable inline
	   3: arguments, not pars
	*/
	return;
    }
    if (ptype == -2) {
	warning(_("graphical parameter \"%s\" is obsolete"), what);
	return;
    }
    if (ptype < 0) {
	warning(_("\"%s\" is not a graphical parameter"), what);
	return;
    }
    if (ptype == 2) {
	warning(_("graphical parameter \"%s\" cannot be set"), what);
	return;
    }

#include "par-common.c"
} /* Specify2 */


/* Do NOT forget to update  ../library/base/R/par.R */
/* if you  ADD a NEW  par !! */

static SEXP Query(const char *what, pGEDevDesc dd)
{
    SEXP value;

    if (streql(what, "adj")) {
	value = allocVector(REALSXP, 1);
	REAL(value)[0] = dpptr(dd)->adj;
    }
    else if (streql(what, "ann")) {
	value = allocVector(LGLSXP, 1);
	LOGICAL(value)[0] = (dpptr(dd)->ann != 0);
    }
    else if (streql(what, "ask")) {
	value = allocVector(LGLSXP, 1);
	LOGICAL(value)[0] = dd->ask;
    }
    else if (streql(what, "bg")) {
	value = mkString(col2name(dpptr(dd)->bg));
    }
    else if (streql(what, "bty")) {
	char buf[2];
	buf[0] = dpptr(dd)->bty;
	buf[1] = '\0';
	value = mkString(buf);
    }
    else if (streql(what, "cex")) {
	value = allocVector(REALSXP, 1);
	REAL(value)[0] = dpptr(dd)->cexbase;
    }
    else if (streql(what, "cex.main")) {
	value = allocVector(REALSXP, 1);
	REAL(value)[0] = dpptr(dd)->cexmain;
    }
    else if (streql(what, "cex.lab")) {
	value = allocVector(REALSXP, 1);
	REAL(value)[0] = dpptr(dd)->cexlab;
    }
    else if (streql(what, "cex.sub")) {
	value = allocVector(REALSXP, 1);
	REAL(value)[0] = dpptr(dd)->cexsub;
    }
    else if (streql(what, "cex.axis")) {
	value = allocVector(REALSXP, 1);
	REAL(value)[0] = dpptr(dd)->cexaxis;
    }
    else if (streql(what, "cin")) {
	value = allocVector(REALSXP, 2);
	REAL(value)[0] = dpptr(dd)->scale * dd->dev->cra[0] * dd->dev->ipr[0];
	REAL(value)[1] = dpptr(dd)->scale * dd->dev->cra[1] * dd->dev->ipr[1];
    }
    else if (streql(what, "col")) {
	value = mkString(col2name(dpptr(dd)->col));
    }
    else if (streql(what, "col.main")) {
	value = mkString(col2name(dpptr(dd)->colmain));
    }
    else if (streql(what, "col.lab")) {
	value = mkString(col2name(dpptr(dd)->collab));
    }
    else if (streql(what, "col.sub")) {
	value = mkString(col2name(dpptr(dd)->colsub));
    }
    else if (streql(what, "col.axis")) {
	value = mkString(col2name(dpptr(dd)->colaxis));
    }
    else if (streql(what, "cra")) {
	value = allocVector(REALSXP, 2);
	REAL(value)[0] = dpptr(dd)->scale * dd->dev->cra[0];
	REAL(value)[1] = dpptr(dd)->scale * dd->dev->cra[1];
    }
    else if (streql(what, "crt")) {
	value = allocVector(REALSXP, 1);
	REAL(value)[0] = dpptr(dd)->crt;
    }
    else if (streql(what, "csi")) {
	value = allocVector(REALSXP, 1);
	REAL(value)[0] = GConvertYUnits(1.0, CHARS, INCHES, dd);
    }
    else if (streql(what, "cxy")) {
	value = allocVector(REALSXP, 2);
	/* == par("cin") / par("pin") : */
	REAL(value)[0] = dpptr(dd)->scale * dd->dev->cra[0]
	    * dd->dev->ipr[0] / dpptr(dd)->pin[0]
	    * (dpptr(dd)->usr[1] - dpptr(dd)->usr[0]);
	REAL(value)[1] = dpptr(dd)->scale * dd->dev->cra[1]
	    * dd->dev->ipr[1] / dpptr(dd)->pin[1]
	    * (dpptr(dd)->usr[3] - dpptr(dd)->usr[2]);
    }
    else if (streql(what, "din")) {
	value = allocVector(REALSXP, 2);
	REAL(value)[0] = GConvertXUnits(1.0, NDC, INCHES, dd);
	REAL(value)[1] = GConvertYUnits(1.0, NDC, INCHES, dd);
    }
    else if (streql(what, "err")) {
	value = allocVector(INTSXP, 1);
	INTEGER(value)[0] = dpptr(dd)->err;
    }
    else if (streql(what, "family")) {
	value = mkString(dpptr(dd)->family);
    }
    else if (streql(what, "fg")) {
	value = mkString(col2name(dpptr(dd)->fg));
    }
    else if (streql(what, "fig")) {
	value = allocVector(REALSXP, 4);
	REAL(value)[0] = dpptr(dd)->fig[0];
	REAL(value)[1] = dpptr(dd)->fig[1];
	REAL(value)[2] = dpptr(dd)->fig[2];
	REAL(value)[3] = dpptr(dd)->fig[3];
    }
    else if (streql(what, "fin")) {
	value = allocVector(REALSXP, 2);
	REAL(value)[0] = dpptr(dd)->fin[0];
	REAL(value)[1] = dpptr(dd)->fin[1];
    }
    else if (streql(what, "font")) {
	value = allocVector(INTSXP, 1);
	INTEGER(value)[0] = dpptr(dd)->font;
    }
    else if (streql(what, "font.main")) {
	value = allocVector(INTSXP, 1);
	INTEGER(value)[0] = dpptr(dd)->fontmain;
    }
    else if (streql(what, "font.lab")) {
	value = allocVector(INTSXP, 1);
	INTEGER(value)[0] = dpptr(dd)->fontlab;
    }
    else if (streql(what, "font.sub")) {
	value = allocVector(INTSXP, 1);
	INTEGER(value)[0] = dpptr(dd)->fontsub;
    }
    else if (streql(what, "font.axis")) {
	value = allocVector(INTSXP, 1);
	INTEGER(value)[0] = dpptr(dd)->fontaxis;
    }
    else if (streql(what, "lab")) {
	value = allocVector(INTSXP, 3);
	INTEGER(value)[0] = dpptr(dd)->lab[0];
	INTEGER(value)[1] = dpptr(dd)->lab[1];
	INTEGER(value)[2] = dpptr(dd)->lab[2];
    }
    else if (streql(what, "las")) {
	value = allocVector(INTSXP, 1);
	INTEGER(value)[0] = dpptr(dd)->las;
    }
    else if (streql(what, "lend")) {
	value = GE_LENDget(dpptr(dd)->lend);
    }
    else if (streql(what, "lheight")) {
	value = allocVector(REALSXP, 1);
	REAL(value)[0] = dpptr(dd)->lheight;
    }
    else if (streql(what, "ljoin")) {
	value = GE_LJOINget(dpptr(dd)->ljoin);
    }
    else if (streql(what, "lmitre")) {
	value = allocVector(REALSXP, 1);
	REAL(value)[0] = dpptr(dd)->lmitre;
    }
    else if (streql(what, "lty")) {
	value = GE_LTYget(dpptr(dd)->lty);
    }
    else if (streql(what, "lwd")) {
	value =	 allocVector(REALSXP, 1);
	REAL(value)[0] = dpptr(dd)->lwd;
    }
    else if (streql(what, "mai")) {
	value = allocVector(REALSXP, 4);
	REAL(value)[0] = dpptr(dd)->mai[0];
	REAL(value)[1] = dpptr(dd)->mai[1];
	REAL(value)[2] = dpptr(dd)->mai[2];
	REAL(value)[3] = dpptr(dd)->mai[3];
    }
    else if (streql(what, "mar")) {
	value = allocVector(REALSXP, 4);
	REAL(value)[0] = dpptr(dd)->mar[0];
	REAL(value)[1] = dpptr(dd)->mar[1];
	REAL(value)[2] = dpptr(dd)->mar[2];
	REAL(value)[3] = dpptr(dd)->mar[3];
    }
    else if (streql(what, "mex")) {
	value = allocVector(REALSXP, 1);
	REAL(value)[0] = dpptr(dd)->mex;
    }
    /* NOTE that if a complex layout has been specified */
    /* then this simple information may not be very useful. */
    else if (streql(what, "mfrow") || streql(what, "mfcol")) {
	value = allocVector(INTSXP, 2);
	INTEGER(value)[0] = dpptr(dd)->numrows;
	INTEGER(value)[1] = dpptr(dd)->numcols;
    }
    else if (streql(what, "mfg")) {
	int row, col;
	value = allocVector(INTSXP, 4);
	currentFigureLocation(&row, &col, dd);
	INTEGER(value)[0] = row+1;
	INTEGER(value)[1] = col+1;
	INTEGER(value)[2] = dpptr(dd)->numrows;
	INTEGER(value)[3] = dpptr(dd)->numcols;
    }
    else if (streql(what, "mgp")) {
	value = allocVector(REALSXP, 3);
	REAL(value)[0] = dpptr(dd)->mgp[0];
	REAL(value)[1] = dpptr(dd)->mgp[1];
	REAL(value)[2] = dpptr(dd)->mgp[2];
    }
    else if (streql(what, "mkh")) {
	/* Unused in R, but settable */
	value = allocVector(REALSXP, 1);
	REAL(value)[0] = dpptr(dd)->mkh;
    }
    else if (streql(what, "new")) {
	value = allocVector(LGLSXP, 1);
	LOGICAL(value)[0] = dpptr(dd)->new;
    }
    else if (streql(what, "oma")) {
	value = allocVector(REALSXP, 4);
	REAL(value)[0] = dpptr(dd)->oma[0];
	REAL(value)[1] = dpptr(dd)->oma[1];
	REAL(value)[2] = dpptr(dd)->oma[2];
	REAL(value)[3] = dpptr(dd)->oma[3];
    }
    else if (streql(what, "omd")) {
	value = allocVector(REALSXP, 4);
	REAL(value)[0] = dpptr(dd)->omd[0];
	REAL(value)[1] = dpptr(dd)->omd[1];
	REAL(value)[2] = dpptr(dd)->omd[2];
	REAL(value)[3] = dpptr(dd)->omd[3];
    }
    else if (streql(what, "omi")) {
	value = allocVector(REALSXP, 4);
	REAL(value)[0] = dpptr(dd)->omi[0];
	REAL(value)[1] = dpptr(dd)->omi[1];
	REAL(value)[2] = dpptr(dd)->omi[2];
	REAL(value)[3] = dpptr(dd)->omi[3];
    }
    else if (streql(what, "page")) {
        /* This calculation mimics the decision-making in GNewPlot()
         * in graphics.c SO it MUST be kept in synch with the logic there
         */
        value = allocVector(LGLSXP, 1);
        LOGICAL(value)[0] = 0;
        if (dpptr(dd)->new) {
            if (!dpptr(dd)->state) 
                LOGICAL(value)[0] = 1;
        } else {
            if (dpptr(dd)->currentFigure + 1 > dpptr(dd)->lastFigure) 
                LOGICAL(value)[0] = 1;
        }
    }
    else if (streql(what, "pch")) {
	int val = dpptr(dd)->pch;
	/* we need to be careful that par("pch") is converted back
	   to the same value */
	if (known_to_be_latin1 && val <= -32 && val >= -255) val = -val;
	if(val >= ' ' && val <= (mbcslocale ? 127 : 255)) {
	    char buf[2];
	    buf[0] = (char) val;
	    buf[1] = '\0';
	    value = mkString(buf);
	} else {
	    /* Could return as UTF-8 string */
	    value = ScalarInteger(val);
	}
    }
    else if (streql(what, "pin")) {
	value = allocVector(REALSXP, 2);
	REAL(value)[0] = dpptr(dd)->pin[0];
	REAL(value)[1] = dpptr(dd)->pin[1];
    }
    else if (streql(what, "plt")) {
	value = allocVector(REALSXP, 4);
	REAL(value)[0] = dpptr(dd)->plt[0];
	REAL(value)[1] = dpptr(dd)->plt[1];
	REAL(value)[2] = dpptr(dd)->plt[2];
	REAL(value)[3] = dpptr(dd)->plt[3];
    }
    else if (streql(what, "ps")) {
	value = allocVector(INTSXP, 1);
	/* was reporting unscaled prior to 2.7.0 */
	INTEGER(value)[0] = (int)(dpptr(dd)->ps * dpptr(dd)->scale);
    }
    else if (streql(what, "pty")) {
	char buf[2];
	buf[0] = dpptr(dd)->pty;
	buf[1] = '\0';
	value = mkString(buf);
    }
    else if (streql(what, "smo")) {
	value = allocVector(REALSXP, 1);
	REAL(value)[0] = dpptr(dd)->smo;
    }
    else if (streql(what, "srt")) {
	value = allocVector(REALSXP, 1);
	REAL(value)[0] = dpptr(dd)->srt;
    }
    else if (streql(what, "tck")) {
	value = allocVector(REALSXP, 1);
	REAL(value)[0] = dpptr(dd)->tck;
    }
    else if (streql(what, "tcl")) {
	value = allocVector(REALSXP, 1);
	REAL(value)[0] = dpptr(dd)->tcl;
    }
    else if (streql(what, "usr")) {
	value = allocVector(REALSXP, 4);
	if (gpptr(dd)->xlog) {
	    REAL(value)[0] = gpptr(dd)->logusr[0];
	    REAL(value)[1] = gpptr(dd)->logusr[1];
	}
	else {
	    REAL(value)[0] = dpptr(dd)->usr[0];
	    REAL(value)[1] = dpptr(dd)->usr[1];
	}
	if (gpptr(dd)->ylog) {
	    REAL(value)[2] = gpptr(dd)->logusr[2];
	    REAL(value)[3] = gpptr(dd)->logusr[3];
	}
	else {
	    REAL(value)[2] = dpptr(dd)->usr[2];
	    REAL(value)[3] = dpptr(dd)->usr[3];
	}
    }
    else if (streql(what, "xaxp")) {
	value = allocVector(REALSXP, 3);
	REAL(value)[0] = dpptr(dd)->xaxp[0];
	REAL(value)[1] = dpptr(dd)->xaxp[1];
	REAL(value)[2] = dpptr(dd)->xaxp[2];
    }
    else if (streql(what, "xaxs")) {
	char buf[2];
	buf[0] = dpptr(dd)->xaxs;
	buf[1] = '\0';
	value = mkString(buf);
    }
    else if (streql(what, "xaxt")) {
	char buf[2];
	buf[0] = dpptr(dd)->xaxt;
	buf[1] = '\0';
	value = mkString(buf);
    }
    else if (streql(what, "xlog")) {
	value = allocVector(LGLSXP, 1);
	LOGICAL(value)[0] = dpptr(dd)->xlog;
    }
    else if (streql(what, "xpd")) {
	value = allocVector(LGLSXP, 1);
	if (dpptr(dd)->xpd == 2)
	    LOGICAL(value)[0] = NA_LOGICAL;
	else
	    LOGICAL(value)[0] = dpptr(dd)->xpd;
    }
    else if (streql(what, "yaxp")) {
	value = allocVector(REALSXP, 3);
	REAL(value)[0] = dpptr(dd)->yaxp[0];
	REAL(value)[1] = dpptr(dd)->yaxp[1];
	REAL(value)[2] = dpptr(dd)->yaxp[2];
    }
    else if (streql(what, "yaxs")) {
	char buf[2];
	buf[0] = dpptr(dd)->yaxs;
	buf[1] = '\0';
	value = mkString(buf);
    }
    else if (streql(what, "yaxt")) {
	char buf[2];
	buf[0] = dpptr(dd)->yaxt;
	buf[1] = '\0';
	value = mkString(buf);
    }
    else if (streql(what, "ylbias")) {
	value = allocVector(REALSXP, 1);
	REAL(value)[0] = dd->dev->yLineBias;
    }
    else if (streql(what, "ylog")) {
	value = allocVector(LGLSXP, 1);
	LOGICAL(value)[0] = dpptr(dd)->ylog;
    }
    else if (ParCode(what) == -2) {
	warning(_("graphical parameter \"%s\" is obsolete"), what);
	value = R_NilValue;
    }
    else {
	warning(_("\"%s\" is not a graphical parameter"), what);
	value = R_NilValue;
    }
    return value;
}

SEXP C_par(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP value;
    SEXP originalArgs = args;
    pGEDevDesc dd;
    int new_spec, nargs;

    args = CDR(args);

    dd = GEcurrentDevice();
    new_spec = 0;
    args = CAR(args);
    nargs = length(args);
    if (isNewList(args)) {
	SEXP oldnames, newnames, tag, val;
	int i;
	PROTECT(newnames = allocVector(STRSXP, nargs));
	PROTECT(value = allocVector(VECSXP, nargs));
	oldnames = getAttrib(args, R_NamesSymbol);
	for (i = 0 ; i < nargs ; i++) {
	    if (oldnames != R_NilValue)
		tag = STRING_ELT(oldnames, i);
	    else
		tag = R_NilValue;
	    val = VECTOR_ELT(args, i);
	    /* tags are all ASCII */
	    if (tag != R_NilValue && CHAR(tag)[0]) {
		new_spec = 1;
		SET_VECTOR_ELT(value, i, Query(CHAR(tag), dd));
		SET_STRING_ELT(newnames, i, tag);
		Specify(CHAR(tag), val, dd);
	    }
	    else if (isString(val) && length(val) > 0) {
		tag = STRING_ELT(val, 0);
		if (tag != R_NilValue && CHAR(tag)[0]) {
		    SET_VECTOR_ELT(value, i, Query(CHAR(tag), dd));
		    SET_STRING_ELT(newnames, i, tag);
		}
	    }
	    else {
		SET_VECTOR_ELT(value, i, R_NilValue);
		SET_STRING_ELT(newnames, i, R_BlankString);
	    }
	}
	setAttrib(value, R_NamesSymbol, newnames);
    }
    else {
	error(_("invalid argument passed to par()"));
	return R_NilValue/* -Wall */;
    }
    /* should really only do this if specifying new pars ?  yes! [MM] */
    
    if (new_spec && GRecording(call, dd))
	GErecordGraphicOperation(op, originalArgs, dd);
    
    UNPROTECT(2);
    return value;
}

/*
 *  Layout was written by Paul Murrell during 1997-1998 as a partial
 *  implementation of ideas in his PhD thesis.	The orginal was
 *  written in common lisp provides rather more general capabilities.
 *
 *  layout(
 *	num.rows,
 *	num.cols,
 *	mat,
 *	num.figures,
 *	col.widths,
 *	row.heights,
 *	cm.widths,
 *	cm.heights,
 *	respect,
 *	respect.mat
 *  )
 */

SEXP C_layout(SEXP args)
{
    int i, j, nrow, ncol, ncmrow, ncmcol;
    pGEDevDesc dd;

    args = CDR(args);

    dd = GEcurrentDevice();

    /* num.rows: */
    nrow = dpptr(dd)->numrows = gpptr(dd)->numrows =
	INTEGER(CAR(args))[0];
    if (nrow > MAX_LAYOUT_ROWS)
	error(_("too many rows in layout, limit %d"), MAX_LAYOUT_ROWS);
    args = CDR(args);
    /* num.cols: */
    ncol = dpptr(dd)->numcols = gpptr(dd)->numcols =
	INTEGER(CAR(args))[0];
    if (ncol > MAX_LAYOUT_COLS)
	error(_("too many columns in layout, limit %d"), MAX_LAYOUT_COLS);
    if (nrow * ncol > MAX_LAYOUT_CELLS)
	error(_("too many cells in layout, limit %d"), MAX_LAYOUT_CELLS);
    args = CDR(args);
    /* mat[i,j] == order[i+j*nrow] : */
    for (i = 0; i < nrow * ncol; i++)
	dpptr(dd)->order[i] = gpptr(dd)->order[i] =
	    (unsigned short) INTEGER(CAR(args))[i];
    args = CDR(args);

    /* num.figures: */
    dpptr(dd)->currentFigure = gpptr(dd)->currentFigure =
	dpptr(dd)->lastFigure = gpptr(dd)->lastFigure =
	INTEGER(CAR(args))[0];
    args = CDR(args);
    /* col.widths: */
    for (j = 0; j < ncol; j++)
	dpptr(dd)->widths[j] = gpptr(dd)->widths[j] =
	    REAL(CAR(args))[j];
    args = CDR(args);
    /* row.heights: */
    for (i = 0; i < nrow; i++)
	dpptr(dd)->heights[i] = gpptr(dd)->heights[i] =
	    REAL(CAR(args))[i];
    args = CDR(args);
    /* cm.widths: */
    ncmcol = length(CAR(args));
    for (j = 0; j < ncol; j++)
	dpptr(dd)->cmWidths[j] = gpptr(dd)->cmWidths[j] = 0;
    for (j = 0; j < ncmcol; j++) {
	dpptr(dd)->cmWidths[INTEGER(CAR(args))[j] - 1]
	    = gpptr(dd)->cmWidths[INTEGER(CAR(args))[j] - 1]
	    = 1;
    }
    args = CDR(args);
    /* cm.heights: */
    ncmrow = length(CAR(args));
    for (i = 0; i < nrow; i++)
	dpptr(dd)->cmHeights[i] = gpptr(dd)->cmHeights[i] = 0;
    for (i = 0; i < ncmrow; i++) {
	dpptr(dd)->cmHeights[INTEGER(CAR(args))[i] - 1]
	    = gpptr(dd)->cmHeights[INTEGER(CAR(args))[i]-1]
	    = 1;
    }
    args = CDR(args);
    /* respect =  0 (FALSE), 1 (TRUE), or 2 (matrix) : */
    dpptr(dd)->rspct = gpptr(dd)->rspct = INTEGER(CAR(args))[0];
    args = CDR(args);
    /* respect.mat */
    for (i = 0; i < nrow * ncol; i++)
	dpptr(dd)->respect[i] = gpptr(dd)->respect[i]
	    = (unsigned char)INTEGER(CAR(args))[i];

    /*------------------------------------------------------*/

    if (nrow > 2 || ncol > 2) {
	gpptr(dd)->cexbase = dpptr(dd)->cexbase = 0.66;
	gpptr(dd)->mex = dpptr(dd)->mex = 1.0;
    }
    else if (nrow == 2 && ncol == 2) {
	gpptr(dd)->cexbase = dpptr(dd)->cexbase = 0.83;
	gpptr(dd)->mex = dpptr(dd)->mex = 1.0;
    }
    else {
	gpptr(dd)->cexbase = dpptr(dd)->cexbase = 1.0;
	gpptr(dd)->mex = dpptr(dd)->mex = 1.0;
    }

    dpptr(dd)->defaultFigure = gpptr(dd)->defaultFigure = TRUE;
    dpptr(dd)->layout = gpptr(dd)->layout = TRUE;

    GReset(dd);

    return R_NilValue;
}


/* ProcessInLinePars handles inline par specifications
   in graphics functions. */

void ProcessInlinePars(SEXP s, pGEDevDesc dd)
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



/*= Local Variables: **/
/*= mode: C **/
/*= kept-old-versions: 12 **/
/*= kept-new-versions: 30 **/
/*= End: **/
