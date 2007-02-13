/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997-2007 Robert Gentleman, Ross Ihaka and the R core team.
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
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301  USA
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

/* <UTF8> char here is either ASCII or handled as a whole */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Rmath.h>
#include <Graphics.h>		/* "GPar" structure + COMMENTS */
#include <Rdevices.h>

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
    { "gamma",		 0 },
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
    { "ylog",		 1 },
    /* Obsolete pars */
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


static int ParCode(char *what)
{
    int i;
    for (i = 0; ParTable[i].name; i++)
	if (!strcmp(what, ParTable[i].name)) return ParTable[i].code;
    return -1;
}


#ifdef UNUSED
/* par(.)'s call */

static SEXP gcall;

void RecordGraphicsCall(SEXP call)
{
    gcall = call;
}
#endif

static void par_error(char *what)
{
    error(_("invalid value specified for graphical parameter \"%s\""),  what);
}


static void lengthCheck(char *what, SEXP v, int n, SEXP call)
{
    if (length(v) != n)
	errorcall(call, _("graphical parameter \"%s\" has the wrong length"),
		  what);
}


static void nonnegIntCheck(int x, char *s)
{
    if (x == NA_INTEGER || x < 0)
	par_error(s);
}

static void posIntCheck(int x, char *s)
{
    if (x == NA_INTEGER || x <= 0)
	par_error(s);
}

#ifdef UNUSED
static void naIntCheck(int x, char *s)
{
    if (x == NA_INTEGER)
	par_error(s);
}
#endif

static void posRealCheck(double x, char *s)
{
    if (!R_FINITE(x) || x <= 0)
	par_error(s);
}

static void nonnegRealCheck(double x, char *s)
{
    if (!R_FINITE(x) || x < 0)
	par_error(s);
}

static void naRealCheck(double x, char *s)
{
    if (!R_FINITE(x))
	par_error(s);
}

static void logAxpCheck(int x, char *s)
{
    if (x == NA_INTEGER || x == 0 || x > 4)
	par_error(s);
}


static void BoundsCheck(double x, double a, double b, char *s)
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
#define R_DEV__(_P_) Rf_dpptr(dd)->_P_ = Rf_gpptr(dd)->_P_
#define R_DEV_2(_P_) Rf_gpptr(dd)->_P_ = Rf_dpptr(dd)->_P_
/* In Emacs : -- only inside Specify() :
 *  (query-replace-regexp
     "Rf_dpptr(dd)->\\([][A-Za-z0-9]+\\) = Rf_gpptr(dd)->\\(\\1\\)"
     "R_DEV__(\\1)" nil nil nil)

   (query-replace-regexp
     "Rf_gpptr(dd)->\\([][A-Za-z0-9]+\\) = Rf_dpptr(dd)->\\(\\1\\)"
     "R_DEV_2(\\1)" nil nil nil)
*/

static void Specify(char *what, SEXP value, DevDesc *dd, SEXP call)
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
 *      "lend", lheight", "ljoin", "lmitre",
 *	"mai", "mar", "mex",
 *	"mfrow", "mfcol", "mfg",
 *	"new",
 *	"oma", "omd", "omi",
 *	"pin", "plt", "ps", "pty"
 *	"usr",
 *	"xlog", "ylog"
 */
    double x;
    int ix = 0;

    /* If we get here, Query has already checked that 'what' is valid */

    if (ParCode(what) == 2) {
	warning(_("graphical parameter \"%s\" cannot be set"), what);
	return;
    }
#include "par-common.c"
/*	  ------------
 *--- now, these are *different* from  "Specify2() use" : */
    else if (streql(what, "bg")) {
	lengthCheck(what, value, 1, call);	ix = RGBpar(value, 0);
	/*	naIntCheck(ix, what); */
	R_DEV__(bg) = ix;
	R_DEV__(new) = FALSE;
    }
    else if (streql(what, "cex")) {
	lengthCheck(what, value, 1, call);	x = asReal(value);
	posRealCheck(x, what);
	R_DEV__(cex) = 1.0; /* ! (highlevel par, i.e.  Specify2(), set x ! */
	R_DEV__(cexbase) = x;
    }

    else if (streql(what, "fg")) {
	/* par(fg=) sets BOTH "fg" and "col" */
	lengthCheck(what, value, 1, call);	ix = RGBpar(value, 0);
	/*	naIntCheck(ix, what); */
	R_DEV__(col) = R_DEV__(fg) = ix;
    }


/*--- and these are "Specify() only" {i.e. par(nam = val)} : */
    else if (streql(what, "ask")) {
	lengthCheck(what, value, 1, call);	ix = asLogical(value);
	R_DEV__(ask) = (ix == 1);/* NA |-> FALSE */
    }
    else if (streql(what, "fig")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 4, call);
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
    else if (streql(what, "family")) {
	char *ss;
	value = coerceVector(value, STRSXP);
	lengthCheck(what, value, 1, call);
	ss = translateChar(STRING_ELT(value, 0));
	if(strlen(ss) > 200)
	    error(_("graphical parameter 'family' has a maximum length of 200 bytes"));
	strncpy(Rf_dpptr(dd)->family, ss, 201);
	strncpy(Rf_gpptr(dd)->family, ss, 201);
    }
    else if (streql(what, "fin")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 2, call);
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
	lengthCheck(what, value, 1, call);
	x = asReal(value);
	posRealCheck(x, what);
	R_DEV__(lheight) = x;
    }
    else if (streql(what, "mai")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 4, call);
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
	lengthCheck(what, value, 4, call);
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
	lengthCheck(what, value, 1, call);	x = asReal(value);
	posRealCheck(x, what);
	R_DEV__(mex) = x;
	GReset(dd);
    }
    else if (streql(what, "mfrow")) {
	int nrow, ncol;
	value = coerceVector(value, INTSXP);
	lengthCheck(what, value, 2, call);
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
	lengthCheck(what, value, 2, call);
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
	    errorcall(call, _("parameter \"mfg\" has the wrong length"));
	posIntCheck(INTEGER(value)[0], what);
	posIntCheck(INTEGER(value)[1], what);
	row = INTEGER(value)[0];
	col = INTEGER(value)[1];
	nrow = Rf_dpptr(dd)->numrows;
	ncol = Rf_dpptr(dd)->numcols;
	if(row <= 0 || row > nrow)
	    errorcall(call, _("parameter \"i\" in \"mfg\" is out of range"));
	if(col <= 0 || col > ncol)
	    errorcall(call, _("parameter \"j\" in \"mfg\" is out of range"));
	if(np == 4) {
	    posIntCheck(INTEGER(value)[2], what);
	    posIntCheck(INTEGER(value)[3], what);
	    if(nrow != INTEGER(value)[2])
		warning(_("value of nr in \"mfg\" is wrong and will be ignored"));
	    if(ncol != INTEGER(value)[3])
		warning(_("value of nc in \"mfg\" is wrong and will be ignored"));
	}
	R_DEV_2(lastFigure) = nrow*ncol;
	/*R_DEV__(mfind) = 1;*/
	/* currentFigure is 1-based */
	if(Rf_gpptr(dd)->mfind)
	    Rf_dpptr(dd)->currentFigure = (col-1)*nrow + row;
	else Rf_dpptr(dd)->currentFigure = (row-1)*ncol + col;
	/*
	  if (Rf_dpptr(dd)->currentFigure == 0)
	  Rf_dpptr(dd)->currentFigure = Rf_dpptr(dd)->lastFigure;
	*/
	R_DEV_2(currentFigure);
	/* R_DEV_2(defaultFigure) = TRUE;
	   R_DEV_2(layout) = FALSE; */
	R_DEV_2(new) = TRUE;
	GReset(dd);
	/* Force a device clip */
	if (Rf_dpptr(dd)->canClip)
	    GForceClip(dd);
    } /* mfg */

    else if (streql(what, "new")) {
	lengthCheck(what, value, 1, call);	ix = asLogical(value);
	if(!Rf_gpptr(dd)->state) warning(_("calling par(new=) with no plot"));
	else R_DEV__(new) = (ix != 0);
    }
    /* -- */

    else if (streql(what, "oma")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 4, call);
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
	R_DEV__(currentFigure) = Rf_gpptr(dd)->lastFigure;
	GReset(dd);
    }
    else if (streql(what, "omd")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 4, call);
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
	R_DEV__(currentFigure) = Rf_gpptr(dd)->lastFigure;
	GReset(dd);
    }
    else if (streql(what, "omi")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 4, call);
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
	R_DEV__(currentFigure) = Rf_gpptr(dd)->lastFigure;
	GReset(dd);
    }
    /* -- */

    else if (streql(what, "pin")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 2, call);
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
	lengthCheck(what, value, 4, call);
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
	lengthCheck(what, value, 1, call);	ix = asInteger(value);
	nonnegIntCheck(ix, what);
	R_DEV__(ps) = ix;
    }
    else if (streql(what, "pty")) {
	if (!isString(value) || LENGTH(value) < 1)
	    par_error(what);
	ix = CHAR(STRING_ELT(value, 0))[0];
	if (ix == 'm' || ix == 's') {
	    R_DEV__(pty) = ix;
	    R_DEV__(defaultPlot) = TRUE;
	}
	else par_error(what);
    }
    /* -- */
    else if (streql(what, "usr")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 4, call);
	naRealCheck(REAL(value)[0], what);
	naRealCheck(REAL(value)[1], what);
	naRealCheck(REAL(value)[2], what);
	naRealCheck(REAL(value)[3], what);
	if (REAL(value)[0] == REAL(value)[1] ||
	    REAL(value)[2] == REAL(value)[3])
	    par_error(what);
	if (Rf_gpptr(dd)->xlog) {
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
	if (Rf_gpptr(dd)->ylog) {
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
	lengthCheck(what, value, 1, call);	ix = asLogical(value);
	if (ix == NA_LOGICAL)
	    par_error(what);
	R_DEV__(xlog) = (ix != 0);
    }
    else if (streql(what, "ylog")) {
	lengthCheck(what, value, 1, call);	ix = asLogical(value);
	if (ix == NA_LOGICAL)
	    par_error(what);
	R_DEV__(ylog) = (ix != 0);
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
#define R_DEV__(_P_) Rf_gpptr(dd)->_P_

void attribute_hidden Specify2(char *what, SEXP value, DevDesc *dd, SEXP call)
{
    double x;
    int ix = 0, ptype = ParCode(what);

    if (ptype == 1 || ptype == -3) {
	/* 1: these are valid, but not settable inline
	   3: arguments, not pars
	*/
	return;
    }
    if (ptype == -2) {
	warningcall(call, _("graphical parameter \"%s\" is obsolete"), what);
	return;
    }
    if (ptype < 0) {
	warningcall(call, _("\"%s\" is not a graphical parameter"), what);
	return;
    }
    if (ptype == 2) {
	warningcall(call, _("graphical parameter \"%s\" cannot be set"), what);
	return;
    }

#include "par-common.c"
/*	  ------------
 *  these are *different* from Specify() , i.e., par(<NAM> = .) use : */
    else if (streql(what, "bg")) {
	/* bg can be a vector of length > 1, so pick off first value
	   (as e.g. pch always did) */
	if (!isVector(value) || LENGTH(value) < 1)
	    par_error(what);
	R_DEV__(bg) = RGBpar(value, 0);
    }
    else if (streql(what, "cex")) {
	/* cex can be a vector of length > 1, so pick off first value
	   (as e.g. pch always did) */
	x = asReal(value);
	posRealCheck(x, what);
	R_DEV__(cex) = x;
	/* not setting cexbase here (but in Specify()) */
    }
    else if (streql(what, "family")) {
	char *ss;
	value = coerceVector(value, STRSXP);
	lengthCheck(what, value, 1, call);
	ss = translateChar(STRING_ELT(value, 0));
	if(strlen(ss) > 200)
	    error(_("graphical parameter 'family' has a maximum length of 200 bytes"));
	strncpy(Rf_gpptr(dd)->family, ss, 201);
    }
    else if (streql(what, "fg")) {
	/* highlevel arg `fg = ' does *not* set `col' (as par(fg=.) does!*/
	lengthCheck(what, value, 1, call);	ix = RGBpar(value, 0);
	/*	naIntCheck(ix, what); */
	R_DEV__(fg) = ix;
    }
} /* Specify2 */


/* Do NOT forget to update  ../library/base/R/par.R */
/* if you  ADD a NEW  par !! */

static SEXP Query(char *what, DevDesc *dd)
{
    SEXP value;

    if (streql(what, "adj")) {
	value = allocVector(REALSXP, 1);
	REAL(value)[0] = Rf_dpptr(dd)->adj;
    }
    else if (streql(what, "ann")) {
	value = allocVector(LGLSXP, 1);
	LOGICAL(value)[0] = (Rf_dpptr(dd)->ann != 0);
    }
    else if (streql(what, "ask")) {
	value = allocVector(LGLSXP, 1);
	LOGICAL(value)[0] = Rf_dpptr(dd)->ask;
    }
    else if (streql(what, "bg")) {
	PROTECT(value = allocVector(STRSXP, 1));
	SET_STRING_ELT(value, 0, mkChar(col2name(Rf_dpptr(dd)->bg)));
	UNPROTECT(1);
    }
    else if (streql(what, "bty")) {
	char buf[2];
	PROTECT(value = allocVector(STRSXP, 1));
	buf[0] = Rf_dpptr(dd)->bty;
	buf[1] = '\0';
	SET_STRING_ELT(value, 0, mkChar(buf));
	UNPROTECT(1);
    }
    else if (streql(what, "cex")) {
	value = allocVector(REALSXP, 1);
	REAL(value)[0] = Rf_dpptr(dd)->cexbase;
    }
    else if (streql(what, "cex.main")) {
	value = allocVector(REALSXP, 1);
	REAL(value)[0] = Rf_dpptr(dd)->cexmain;
    }
    else if (streql(what, "cex.lab")) {
	value = allocVector(REALSXP, 1);
	REAL(value)[0] = Rf_dpptr(dd)->cexlab;
    }
    else if (streql(what, "cex.sub")) {
	value = allocVector(REALSXP, 1);
	REAL(value)[0] = Rf_dpptr(dd)->cexsub;
    }
    else if (streql(what, "cex.axis")) {
	value = allocVector(REALSXP, 1);
	REAL(value)[0] = Rf_dpptr(dd)->cexaxis;
    }
    else if (streql(what, "cin")) {
	value = allocVector(REALSXP, 2);
	REAL(value)[0] = Rf_dpptr(dd)->cra[0]*Rf_dpptr(dd)->ipr[0];
	REAL(value)[1] = Rf_dpptr(dd)->cra[1]*Rf_dpptr(dd)->ipr[1];
    }
    else if (streql(what, "col")) {
	PROTECT(value = allocVector(STRSXP, 1));
	SET_STRING_ELT(value, 0, mkChar(col2name(Rf_dpptr(dd)->col)));
	UNPROTECT(1);
    }
    else if (streql(what, "col.main")) {
	PROTECT(value = allocVector(STRSXP, 1));
	SET_STRING_ELT(value, 0, mkChar(col2name(Rf_dpptr(dd)->colmain)));
	UNPROTECT(1);
    }
    else if (streql(what, "col.lab")) {
	PROTECT(value = allocVector(STRSXP, 1));
	SET_STRING_ELT(value, 0, mkChar(col2name(Rf_dpptr(dd)->collab)));
	UNPROTECT(1);
    }
    else if (streql(what, "col.sub")) {
	PROTECT(value = allocVector(STRSXP, 1));
	SET_STRING_ELT(value, 0, mkChar(col2name(Rf_dpptr(dd)->colsub)));
	UNPROTECT(1);
    }
    else if (streql(what, "col.axis")) {
	PROTECT(value = allocVector(STRSXP, 1));
	SET_STRING_ELT(value, 0, mkChar(col2name(Rf_dpptr(dd)->colaxis)));
	UNPROTECT(1);
    }
    else if (streql(what, "cra")) {
	value = allocVector(REALSXP, 2);
	REAL(value)[0] = Rf_dpptr(dd)->cra[0];
	REAL(value)[1] = Rf_dpptr(dd)->cra[1];
    }
    else if (streql(what, "crt")) {
	value = allocVector(REALSXP, 1);
	REAL(value)[0] = Rf_dpptr(dd)->crt;
    }
    else if (streql(what, "csi")) {
	value = allocVector(REALSXP, 1);
	REAL(value)[0] = GConvertYUnits(1.0, CHARS, INCHES, dd);
    }
    else if (streql(what, "cxy")) {
	value = allocVector(REALSXP, 2);
	/* == par("cin") / par("pin") : */
	REAL(value)[0] = Rf_dpptr(dd)->cra[0]*Rf_dpptr(dd)->ipr[0] / Rf_dpptr(dd)->pin[0]
	    * (Rf_dpptr(dd)->usr[1] - Rf_dpptr(dd)->usr[0]);
	REAL(value)[1] = Rf_dpptr(dd)->cra[1]*Rf_dpptr(dd)->ipr[1] / Rf_dpptr(dd)->pin[1]
	    * (Rf_dpptr(dd)->usr[3] - Rf_dpptr(dd)->usr[2]);
    }
    else if (streql(what, "din")) {
	value = allocVector(REALSXP, 2);
	REAL(value)[0] = GConvertXUnits(1.0, NDC, INCHES, dd);
	REAL(value)[1] = GConvertYUnits(1.0, NDC, INCHES, dd);
    }
    else if (streql(what, "err")) {
	value = allocVector(INTSXP, 1);
	INTEGER(value)[0] = Rf_dpptr(dd)->err;
    }
    else if (streql(what, "family")) {
	PROTECT(value = allocVector(STRSXP, 1));
	SET_STRING_ELT(value, 0, mkChar(Rf_dpptr(dd)->family));
	UNPROTECT(1);
    }
    else if (streql(what, "fg")) {
	PROTECT(value = allocVector(STRSXP, 1));
	SET_STRING_ELT(value, 0, mkChar(col2name(Rf_dpptr(dd)->fg)));
	UNPROTECT(1);
    }
    else if (streql(what, "fig")) {
	value = allocVector(REALSXP, 4);
	REAL(value)[0] = Rf_dpptr(dd)->fig[0];
	REAL(value)[1] = Rf_dpptr(dd)->fig[1];
	REAL(value)[2] = Rf_dpptr(dd)->fig[2];
	REAL(value)[3] = Rf_dpptr(dd)->fig[3];
    }
    else if (streql(what, "fin")) {
	value = allocVector(REALSXP, 2);
	REAL(value)[0] = Rf_dpptr(dd)->fin[0];
	REAL(value)[1] = Rf_dpptr(dd)->fin[1];
    }
    else if (streql(what, "font")) {
	value = allocVector(INTSXP, 1);
	INTEGER(value)[0] = Rf_dpptr(dd)->font;
    }
    else if (streql(what, "font.main")) {
	value = allocVector(INTSXP, 1);
	INTEGER(value)[0] = Rf_dpptr(dd)->fontmain;
    }
    else if (streql(what, "font.lab")) {
	value = allocVector(INTSXP, 1);
	INTEGER(value)[0] = Rf_dpptr(dd)->fontlab;
    }
    else if (streql(what, "font.sub")) {
	value = allocVector(INTSXP, 1);
	INTEGER(value)[0] = Rf_dpptr(dd)->fontsub;
    }
    else if (streql(what, "font.axis")) {
	value = allocVector(INTSXP, 1);
	INTEGER(value)[0] = Rf_dpptr(dd)->fontaxis;
    }
    else if (streql(what, "gamma")) {
	value = allocVector(REALSXP, 1);
	REAL(value)[0] = Rf_dpptr(dd)->gamma;
    }
    else if (streql(what, "lab")) {
	value = allocVector(INTSXP, 3);
	INTEGER(value)[0] = Rf_dpptr(dd)->lab[0];
	INTEGER(value)[1] = Rf_dpptr(dd)->lab[1];
	INTEGER(value)[2] = Rf_dpptr(dd)->lab[2];
    }
    else if (streql(what, "las")) {
	value = allocVector(INTSXP, 1);
	INTEGER(value)[0] = Rf_dpptr(dd)->las;
    }
    else if (streql(what, "lend")) {
	value = LENDget(Rf_dpptr(dd)->lend);
    }
    else if (streql(what, "lheight")) {
	value = allocVector(REALSXP, 1);
	REAL(value)[0] = Rf_dpptr(dd)->lheight;
    }
    else if (streql(what, "ljoin")) {
	value = LJOINget(Rf_dpptr(dd)->ljoin);
    }
    else if (streql(what, "lmitre")) {
	value = allocVector(REALSXP, 1);
	REAL(value)[0] = Rf_dpptr(dd)->lmitre;
    }
    else if (streql(what, "lty")) {
	value = LTYget(Rf_dpptr(dd)->lty);
    }
    else if (streql(what, "lwd")) {
	value =	 allocVector(REALSXP, 1);
	REAL(value)[0] = Rf_dpptr(dd)->lwd;
    }
    else if (streql(what, "mai")) {
	value = allocVector(REALSXP, 4);
	REAL(value)[0] = Rf_dpptr(dd)->mai[0];
	REAL(value)[1] = Rf_dpptr(dd)->mai[1];
	REAL(value)[2] = Rf_dpptr(dd)->mai[2];
	REAL(value)[3] = Rf_dpptr(dd)->mai[3];
    }
    else if (streql(what, "mar")) {
	value = allocVector(REALSXP, 4);
	REAL(value)[0] = Rf_dpptr(dd)->mar[0];
	REAL(value)[1] = Rf_dpptr(dd)->mar[1];
	REAL(value)[2] = Rf_dpptr(dd)->mar[2];
	REAL(value)[3] = Rf_dpptr(dd)->mar[3];
    }
    else if (streql(what, "mex")) {
	value = allocVector(REALSXP, 1);
	REAL(value)[0] = Rf_dpptr(dd)->mex;
    }
    /* NOTE that if a complex layout has been specified */
    /* then this simple information may not be very useful. */
    else if (streql(what, "mfrow") || streql(what, "mfcol")) {
	value = allocVector(INTSXP, 2);
	INTEGER(value)[0] = Rf_dpptr(dd)->numrows;
	INTEGER(value)[1] = Rf_dpptr(dd)->numcols;
    }
    else if (streql(what, "mfg")) {
	int row, col;
	value = allocVector(INTSXP, 4);
	currentFigureLocation(&row, &col, dd);
	INTEGER(value)[0] = row+1;
	INTEGER(value)[1] = col+1;
	INTEGER(value)[2] = Rf_dpptr(dd)->numrows;
	INTEGER(value)[3] = Rf_dpptr(dd)->numcols;
    }
    else if (streql(what, "mgp")) {
	value = allocVector(REALSXP, 3);
	REAL(value)[0] = Rf_dpptr(dd)->mgp[0];
	REAL(value)[1] = Rf_dpptr(dd)->mgp[1];
	REAL(value)[2] = Rf_dpptr(dd)->mgp[2];
    }
    else if (streql(what, "mkh")) {
	value = allocVector(REALSXP, 1);
	REAL(value)[0] = Rf_dpptr(dd)->mkh;
    }
    else if (streql(what, "new")) {
	value = allocVector(LGLSXP, 1);
	LOGICAL(value)[0] = Rf_dpptr(dd)->new;
    }
    else if (streql(what, "oma")) {
	value = allocVector(REALSXP, 4);
	REAL(value)[0] = Rf_dpptr(dd)->oma[0];
	REAL(value)[1] = Rf_dpptr(dd)->oma[1];
	REAL(value)[2] = Rf_dpptr(dd)->oma[2];
	REAL(value)[3] = Rf_dpptr(dd)->oma[3];
    }
    else if (streql(what, "omd")) {
	value = allocVector(REALSXP, 4);
	REAL(value)[0] = Rf_dpptr(dd)->omd[0];
	REAL(value)[1] = Rf_dpptr(dd)->omd[1];
	REAL(value)[2] = Rf_dpptr(dd)->omd[2];
	REAL(value)[3] = Rf_dpptr(dd)->omd[3];
    }
    else if (streql(what, "omi")) {
	value = allocVector(REALSXP, 4);
	REAL(value)[0] = Rf_dpptr(dd)->omi[0];
	REAL(value)[1] = Rf_dpptr(dd)->omi[1];
	REAL(value)[2] = Rf_dpptr(dd)->omi[2];
	REAL(value)[3] = Rf_dpptr(dd)->omi[3];
    }
    else if (streql(what, "pch")) {
	char buf[2];
	if(Rf_dpptr(dd)->pch < ' ' || Rf_dpptr(dd)->pch > 255) {
	    PROTECT(value = allocVector(INTSXP, 1));
	    INTEGER(value)[0] = Rf_dpptr(dd)->pch;
	}
	else {
	    PROTECT(value = allocVector(STRSXP, 1));
	    buf[0] = Rf_dpptr(dd)->pch;
	    buf[1] = '\0';
	    SET_STRING_ELT(value, 0, mkChar(buf));
	}
	UNPROTECT(1);
    }
    else if (streql(what, "pin")) {
	value = allocVector(REALSXP, 2);
	REAL(value)[0] = Rf_dpptr(dd)->pin[0];
	REAL(value)[1] = Rf_dpptr(dd)->pin[1];
    }
    else if (streql(what, "plt")) {
	value = allocVector(REALSXP, 4);
	REAL(value)[0] = Rf_dpptr(dd)->plt[0];
	REAL(value)[1] = Rf_dpptr(dd)->plt[1];
	REAL(value)[2] = Rf_dpptr(dd)->plt[2];
	REAL(value)[3] = Rf_dpptr(dd)->plt[3];
    }
    else if (streql(what, "ps")) {
	value = allocVector(INTSXP, 1);
	INTEGER(value)[0] = Rf_dpptr(dd)->ps;
    }
    else if (streql(what, "pty")) {
	char buf[2];
	PROTECT(value = allocVector(STRSXP, 1));
	buf[0] = Rf_dpptr(dd)->pty;
	buf[1] = '\0';
	SET_STRING_ELT(value, 0, mkChar(buf));
	UNPROTECT(1);
    }
    else if (streql(what, "smo")) {
	value = allocVector(REALSXP, 1);
	REAL(value)[0] = Rf_dpptr(dd)->smo;
    }
    else if (streql(what, "srt")) {
	value = allocVector(REALSXP, 1);
	REAL(value)[0] = Rf_dpptr(dd)->srt;
    }
    else if (streql(what, "tck")) {
	value = allocVector(REALSXP, 1);
	REAL(value)[0] = Rf_dpptr(dd)->tck;
    }
    else if (streql(what, "tcl")) {
	value = allocVector(REALSXP, 1);
	REAL(value)[0] = Rf_dpptr(dd)->tcl;
    }
    else if (streql(what, "usr")) {
	value = allocVector(REALSXP, 4);
	if (Rf_gpptr(dd)->xlog) {
	    REAL(value)[0] = Rf_gpptr(dd)->logusr[0];
	    REAL(value)[1] = Rf_gpptr(dd)->logusr[1];
	}
	else {
	    REAL(value)[0] = Rf_dpptr(dd)->usr[0];
	    REAL(value)[1] = Rf_dpptr(dd)->usr[1];
	}
	if (Rf_gpptr(dd)->ylog) {
	    REAL(value)[2] = Rf_gpptr(dd)->logusr[2];
	    REAL(value)[3] = Rf_gpptr(dd)->logusr[3];
	}
	else {
	    REAL(value)[2] = Rf_dpptr(dd)->usr[2];
	    REAL(value)[3] = Rf_dpptr(dd)->usr[3];
	}
    }
    else if (streql(what, "xaxp")) {
	value = allocVector(REALSXP, 3);
	REAL(value)[0] = Rf_dpptr(dd)->xaxp[0];
	REAL(value)[1] = Rf_dpptr(dd)->xaxp[1];
	REAL(value)[2] = Rf_dpptr(dd)->xaxp[2];
    }
    else if (streql(what, "xaxs")) {
	char buf[2];
	PROTECT(value = allocVector(STRSXP, 1));
	buf[0] = Rf_dpptr(dd)->xaxs;
	buf[1] = '\0';
	SET_STRING_ELT(value, 0, mkChar(buf));
	UNPROTECT(1);
    }
    else if (streql(what, "xaxt")) {
	char buf[2];
	PROTECT(value = allocVector(STRSXP, 1));
	buf[0] = Rf_dpptr(dd)->xaxt;
	buf[1] = '\0';
	SET_STRING_ELT(value, 0, mkChar(buf));
	UNPROTECT(1);
    }
    else if (streql(what, "xlog")) {
	value = allocVector(LGLSXP, 1);
	LOGICAL(value)[0] = Rf_dpptr(dd)->xlog;
    }
    else if (streql(what, "xpd")) {
	value = allocVector(LGLSXP, 1);
	if (Rf_dpptr(dd)->xpd == 2)
	    LOGICAL(value)[0] = NA_LOGICAL;
	else
	    LOGICAL(value)[0] = Rf_dpptr(dd)->xpd;
    }
    else if (streql(what, "yaxp")) {
	value = allocVector(REALSXP, 3);
	REAL(value)[0] = Rf_dpptr(dd)->yaxp[0];
	REAL(value)[1] = Rf_dpptr(dd)->yaxp[1];
	REAL(value)[2] = Rf_dpptr(dd)->yaxp[2];
    }
    else if (streql(what, "yaxs")) {
	char buf[2];
	PROTECT(value = allocVector(STRSXP, 1));
	buf[0] = Rf_dpptr(dd)->yaxs;
	buf[1] = '\0';
	SET_STRING_ELT(value, 0, mkChar(buf));
	UNPROTECT(1);
    }
    else if (streql(what, "yaxt")) {
	char buf[2];
	PROTECT(value = allocVector(STRSXP, 1));
	buf[0] = Rf_dpptr(dd)->yaxt;
	buf[1] = '\0';
	SET_STRING_ELT(value, 0, mkChar(buf));
	UNPROTECT(1);
    }
    else if (streql(what, "ylog")) {
	value = allocVector(LGLSXP, 1);
	LOGICAL(value)[0] = Rf_dpptr(dd)->ylog;
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

SEXP attribute_hidden do_par(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP value;
    SEXP originalArgs = args;
    DevDesc *dd;
    int new_spec, nargs;

    checkArity(op, args);

    dd = CurrentDevice();
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
		Specify(CHAR(tag), val, dd, call);
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
		SET_STRING_ELT(newnames, i, R_NilValue);
	    }
	}
	setAttrib(value, R_NamesSymbol, newnames);
	UNPROTECT(2);
    }
    else {
	error(_("invalid argument passed to par()"));
	return R_NilValue/* -Wall */;
    }
    /* should really only do this if specifying new pars ?  yes! [MM] */
    if (new_spec && GRecording(call, dd))
	recordGraphicOperation(op, originalArgs, dd);
    return value;
}

SEXP attribute_hidden do_readonlypars(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP result;
    GEDevDesc *dd;
    Rboolean canChangeGamma;
    int nreadonly;

    checkArity(op, args);

    dd = GEcurrentDevice();
    canChangeGamma = dd->dev->canChangeGamma;

    if (canChangeGamma)
	nreadonly = 5;
    else
	nreadonly = 6;
    PROTECT(result = allocVector(STRSXP, nreadonly));
    SET_STRING_ELT(result, 0, mkChar("cin"));
    SET_STRING_ELT(result, 1, mkChar("cra"));
    SET_STRING_ELT(result, 2, mkChar("csi"));
    SET_STRING_ELT(result, 3, mkChar("cxy"));
    SET_STRING_ELT(result, 4, mkChar("din"));
    if (!canChangeGamma)
	SET_STRING_ELT(result, 5, mkChar("gamma"));
    UNPROTECT(1);
    return result;
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

SEXP attribute_hidden do_layout(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i, j, nrow, ncol, ncmrow, ncmcol;
    SEXP originalArgs = args;
    DevDesc *dd;

    checkArity(op, args);

    dd = CurrentDevice();

    /* num.rows: */
    nrow = Rf_dpptr(dd)->numrows = Rf_gpptr(dd)->numrows =
	INTEGER(CAR(args))[0];
    if (nrow > MAX_LAYOUT_ROWS)
	error(_("too many rows in layout, limit %d"), MAX_LAYOUT_ROWS);
    args = CDR(args);
    /* num.cols: */
    ncol = Rf_dpptr(dd)->numcols = Rf_gpptr(dd)->numcols =
	INTEGER(CAR(args))[0];
    if (ncol > MAX_LAYOUT_COLS)
	error(_("too many columns in layout, limit %d"), MAX_LAYOUT_COLS);
    if (nrow * ncol > MAX_LAYOUT_CELLS)
	error(_("too many cells in layout, limit %d"), MAX_LAYOUT_CELLS);
    args = CDR(args);
    /* mat[i,j] == order[i+j*nrow] : */
    for (i = 0; i < nrow * ncol; i++)
	Rf_dpptr(dd)->order[i] = Rf_gpptr(dd)->order[i] =
	    INTEGER(CAR(args))[i];
    args = CDR(args);

    /* num.figures: */
    Rf_dpptr(dd)->currentFigure = Rf_gpptr(dd)->currentFigure =
	Rf_dpptr(dd)->lastFigure = Rf_gpptr(dd)->lastFigure =
	INTEGER(CAR(args))[0];
    args = CDR(args);
    /* col.widths: */
    for (j = 0; j < ncol; j++)
	Rf_dpptr(dd)->widths[j] = Rf_gpptr(dd)->widths[j] =
	    REAL(CAR(args))[j];
    args = CDR(args);
    /* row.heights: */
    for (i = 0; i < nrow; i++)
	Rf_dpptr(dd)->heights[i] = Rf_gpptr(dd)->heights[i] =
	    REAL(CAR(args))[i];
    args = CDR(args);
    /* cm.widths: */
    ncmcol = length(CAR(args));
    for (j = 0; j < ncol; j++)
	Rf_dpptr(dd)->cmWidths[j] = Rf_gpptr(dd)->cmWidths[j] = 0;
    for (j = 0; j < ncmcol; j++) {
	Rf_dpptr(dd)->cmWidths[INTEGER(CAR(args))[j] - 1]
	    = Rf_gpptr(dd)->cmWidths[INTEGER(CAR(args))[j] - 1]
	    = 1;
    }
    args = CDR(args);
    /* cm.heights: */
    ncmrow = length(CAR(args));
    for (i = 0; i < nrow; i++)
	Rf_dpptr(dd)->cmHeights[i] = Rf_gpptr(dd)->cmHeights[i] = 0;
    for (i = 0; i < ncmrow; i++) {
	Rf_dpptr(dd)->cmHeights[INTEGER(CAR(args))[i] - 1]
	    = Rf_gpptr(dd)->cmHeights[INTEGER(CAR(args))[i]-1]
	    = 1;
    }
    args = CDR(args);
    /* respect =  0 (FALSE), 1 (TRUE), or 2 (matrix) : */
    Rf_dpptr(dd)->rspct = Rf_gpptr(dd)->rspct = INTEGER(CAR(args))[0];
    args = CDR(args);
    /* respect.mat */
    for (i = 0; i < nrow * ncol; i++)
	Rf_dpptr(dd)->respect[i] = Rf_gpptr(dd)->respect[i]
		= INTEGER(CAR(args))[i];

    /*------------------------------------------------------*/

    if (nrow > 2 || ncol > 2) {
	Rf_gpptr(dd)->cexbase = Rf_dpptr(dd)->cexbase = 0.66;
	Rf_gpptr(dd)->mex = Rf_dpptr(dd)->mex = 1.0;
    }
    else if (nrow == 2 && ncol == 2) {
	Rf_gpptr(dd)->cexbase = Rf_dpptr(dd)->cexbase = 0.83;
	Rf_gpptr(dd)->mex = Rf_dpptr(dd)->mex = 1.0;
    }
    else {
	Rf_gpptr(dd)->cexbase = Rf_dpptr(dd)->cexbase = 1.0;
	Rf_gpptr(dd)->mex = Rf_dpptr(dd)->mex = 1.0;
    }

    Rf_dpptr(dd)->defaultFigure = Rf_gpptr(dd)->defaultFigure = TRUE;
    Rf_dpptr(dd)->layout = Rf_gpptr(dd)->layout = TRUE;

    GReset(dd);

    if (GRecording(call, dd))
	recordGraphicOperation(op, originalArgs, dd);
    return R_NilValue;
}

/*= Local Variables: **/
/*= mode: C **/
/*= kept-old-versions: 12 **/
/*= kept-new-versions: 30 **/
/*= End: **/

