/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997-2001 Robert Gentleman, Ross Ihaka and the R core team.
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
#include <Rdevices.h>


/* par(.)'s call */

static SEXP gcall;

void RecordGraphicsCall(SEXP call)
{
    gcall = call;
}

static void par_error(char *what)
{
    error("invalid value specified for graphics parameter \"%s\".",  what);
}


static void lengthCheck(char *what, SEXP v, int n)
{
    if (length(v) != n)
	errorcall(gcall, "parameter \"%s\" has the wrong length", what);
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


static void BoundsCheck(double x, double a, double b, char *s)
{
/* Check if   a <= x <= b */
    if (!R_FINITE(x) || (R_FINITE(a) && x < a) || (R_FINITE(b) && x > b))
	par_error(s);
}


/* When any one of the layout parameters (which can only be set via */
/* par(...)) is modified, must call GReset() to update the layout and */
/* the transformations between coordinate systems */

/* If you ADD a NEW par, then do NOT forget to update the code in
 *			 ../library/base/R/par.R

 * Parameters in Specify(),
 * which can*not* be specified in high-level functions,
 * i.e., by Specify2() [below]:
 *	this list is in \details{.} of ../library/base/man/par.Rd
 *	------------------------
 *	"ask",
 *	"fig", "fin",
 *	"mai", "mar", "mex",
 *	"mfrow", "mfcol", "mfg",
 *	"new",
 *	"oma", "omd", "omi",
 *	"pin", "plt", "ps", "pty"
 *	"usr",
 *	"xlog", "ylog"
 */
static void Specify(char *what, SEXP value, DevDesc *dd)
{
    double x;
    int ix = 0;

    if (streql(what, "adj")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	BoundsCheck(x, 0.0, 1.0, what);
	Rf_dpptr(dd)->adj = Rf_gpptr(dd)->adj = x;
    }
    else if (streql(what, "ann")) {
	lengthCheck(what, value, 1);	ix = asLogical(value);
	Rf_dpptr(dd)->ann = Rf_gpptr(dd)->ann = (ix != 0);/* NA |-> TRUE */
    }
    else if (streql(what, "ask")) {
	lengthCheck(what, value, 1);	ix = asLogical(value);
	Rf_dpptr(dd)->ask = Rf_gpptr(dd)->ask = (ix == 1);/* NA |-> FALSE */
    }
    else if (streql(what, "bg")) {
	lengthCheck(what, value, 1);	ix = RGBpar(value, 0);
/*	naIntCheck(ix, what); */
	Rf_dpptr(dd)->bg = Rf_gpptr(dd)->bg = ix;
	Rf_dpptr(dd)->new = Rf_gpptr(dd)->new = FALSE;
    }
    else if (streql(what, "bty")) {
	lengthCheck(what, value, 1);
	if (!isString(value))
	    par_error(what);
	ix = CHAR(STRING_ELT(value, 0))[0];
	switch (ix) {
	case 'o': case 'O':
	case 'l': case 'L':
	case '7':
	case 'c': case 'C': case '[':
	case ']':
	case 'u': case 'U':
	case 'n':
	    Rf_dpptr(dd)->bty = Rf_gpptr(dd)->bty = ix;
	    break;
	default:
	    par_error(what);
	}
    }
    else if (streql(what, "cex")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	posRealCheck(x, what);
	Rf_dpptr(dd)->cex = Rf_gpptr(dd)->cex = 1.0;
	Rf_dpptr(dd)->cexbase = Rf_gpptr(dd)->cexbase = x;
    }
    else if (streql(what, "cex.main")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	posRealCheck(x, what);
	Rf_dpptr(dd)->cexmain = Rf_gpptr(dd)->cexmain = x;
    }
    else if (streql(what, "cex.lab")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	posRealCheck(x, what);
	Rf_dpptr(dd)->cexlab = Rf_gpptr(dd)->cexlab = x;
    }
    else if (streql(what, "cex.sub")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	posRealCheck(x, what);
	Rf_dpptr(dd)->cexsub = Rf_gpptr(dd)->cexsub = x;
    }
    else if (streql(what, "cex.axis")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	posRealCheck(x, what);
	Rf_dpptr(dd)->cexaxis = Rf_gpptr(dd)->cexaxis = x;
    }
    else if (streql(what, "col")) {
	lengthCheck(what, value, 1);
	ix = RGBpar(value, 0);
/*	naIntCheck(ix, what); */
	Rf_dpptr(dd)->col = Rf_gpptr(dd)->col = ix;
    }
    else if (streql(what, "col.main")) {
	lengthCheck(what, value, 1);
	ix = RGBpar(value, 0);
/*	naIntCheck(ix, what); */
	Rf_dpptr(dd)->colmain = Rf_gpptr(dd)->colmain = ix;
    }
    else if (streql(what, "col.lab")) {
	lengthCheck(what, value, 1);
	ix = RGBpar(value, 0);
/*	naIntCheck(ix, what); */
	Rf_dpptr(dd)->collab = Rf_gpptr(dd)->collab = ix;
    }
    else if (streql(what, "col.sub")) {
	lengthCheck(what, value, 1);
	ix = RGBpar(value, 0);
/*	naIntCheck(ix, what); */
	Rf_dpptr(dd)->colsub = Rf_gpptr(dd)->colsub = ix;
    }
    else if (streql(what, "col.axis")) {
	lengthCheck(what, value, 1);
/*	naIntCheck(ix = RGBpar(value, 0), what); */
	ix = RGBpar(value, 0);
	Rf_dpptr(dd)->colaxis = Rf_gpptr(dd)->colaxis = ix;
    }
    else if (streql(what, "crt")) {
	lengthCheck(what, value, 1);
	x = asReal(value);
	naRealCheck(x, what);
	Rf_dpptr(dd)->crt = Rf_gpptr(dd)->crt = x;
    }
    else if (streql(what, "err")) {
	lengthCheck(what, value, 1);
	ix = asInteger(value);
	if (ix == 0 || ix == -1)
	    Rf_dpptr(dd)->err = Rf_gpptr(dd)->err = ix;
	else par_error(what);
    }
    else if (streql(what, "fg")) {
	lengthCheck(what, value, 1);
	ix = RGBpar(value, 0);
/*	naIntCheck(ix, what); */
	Rf_dpptr(dd)->col = Rf_gpptr(dd)->col = Rf_dpptr(dd)->fg = Rf_gpptr(dd)->fg = ix;
    }
    else if (streql(what, "fig")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 4);
	if (0.0 <= REAL(value)[0] && REAL(value)[0] < REAL(value)[1] &&
	    REAL(value)[1] <= 1.0 &&
	    0.0 <= REAL(value)[2] && REAL(value)[2] < REAL(value)[3] &&
	    REAL(value)[3] <= 1.0) {
	    Rf_gpptr(dd)->defaultFigure = Rf_dpptr(dd)->defaultFigure = 0;
	    Rf_gpptr(dd)->fUnits = Rf_dpptr(dd)->fUnits = NIC;
	    Rf_gpptr(dd)->numrows = Rf_dpptr(dd)->numrows = 1;
	    Rf_gpptr(dd)->numcols = Rf_dpptr(dd)->numcols = 1;
	    Rf_gpptr(dd)->heights[0] = Rf_dpptr(dd)->heights[0] = 1;
	    Rf_gpptr(dd)->widths[0] = Rf_dpptr(dd)->widths[0] = 1;
	    Rf_gpptr(dd)->cmHeights[0] = Rf_dpptr(dd)->cmHeights[0] = 0;
	    Rf_gpptr(dd)->cmWidths[0] = Rf_dpptr(dd)->cmWidths[0] = 0;
	    Rf_gpptr(dd)->order[0][0] = Rf_dpptr(dd)->order[0][0] = 1;
	    Rf_gpptr(dd)->currentFigure = Rf_dpptr(dd)->currentFigure = 1;
	    Rf_gpptr(dd)->lastFigure = Rf_dpptr(dd)->lastFigure = 1;
	    Rf_dpptr(dd)->rspct = Rf_gpptr(dd)->rspct = 0;
	    Rf_gpptr(dd)->fig[0] = Rf_dpptr(dd)->fig[0] = REAL(value)[0];
	    Rf_gpptr(dd)->fig[1] = Rf_dpptr(dd)->fig[1] = REAL(value)[1];
	    Rf_gpptr(dd)->fig[2] = Rf_dpptr(dd)->fig[2] = REAL(value)[2];
	    Rf_gpptr(dd)->fig[3] = Rf_dpptr(dd)->fig[3] = REAL(value)[3];
	    GReset(dd);
	}
	else par_error(what);
    }
    else if (streql(what, "fin")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 2);
	Rf_gpptr(dd)->defaultFigure = Rf_dpptr(dd)->defaultFigure = 0;
	Rf_gpptr(dd)->fUnits = Rf_dpptr(dd)->fUnits = INCHES;
	Rf_gpptr(dd)->numrows = Rf_dpptr(dd)->numrows = 1;
	Rf_gpptr(dd)->numcols = Rf_dpptr(dd)->numcols = 1;
	Rf_gpptr(dd)->heights[0] = Rf_dpptr(dd)->heights[0] = 1;
	Rf_gpptr(dd)->widths[0] = Rf_dpptr(dd)->widths[0] = 1;
	Rf_gpptr(dd)->cmHeights[0] = Rf_dpptr(dd)->cmHeights[0] = 0;
	Rf_gpptr(dd)->cmWidths[0] = Rf_dpptr(dd)->cmWidths[0] = 0;
	Rf_gpptr(dd)->order[0][0] = Rf_dpptr(dd)->order[0][0] = 1;
	Rf_gpptr(dd)->currentFigure = Rf_dpptr(dd)->currentFigure = 1;
	Rf_gpptr(dd)->lastFigure = Rf_dpptr(dd)->lastFigure = 1;
	Rf_dpptr(dd)->rspct = Rf_gpptr(dd)->rspct = 0;
	Rf_gpptr(dd)->fin[0] = Rf_dpptr(dd)->fin[0] = REAL(value)[0];
	Rf_gpptr(dd)->fin[1] = Rf_dpptr(dd)->fin[1] = REAL(value)[1];
	GReset(dd);
    }
    else if (streql(what, "font")) {
	lengthCheck(what, value, 1);
	ix = asInteger(value);
	posIntCheck(ix, what);
	Rf_dpptr(dd)->font = Rf_gpptr(dd)->font = ix;
    }
    else if (streql(what, "font.main")) {
	lengthCheck(what, value, 1);
	ix = asInteger(value);
	posIntCheck(ix, what);
	Rf_dpptr(dd)->fontmain = Rf_gpptr(dd)->fontmain = ix;
    }
    else if (streql(what, "font.lab")) {
	lengthCheck(what, value, 1);
	ix = asInteger(value);
	posIntCheck(ix, what);
	Rf_dpptr(dd)->fontlab = Rf_gpptr(dd)->fontlab = ix;
    }
    else if (streql(what, "font.sub")) {
	lengthCheck(what, value, 1);
	ix = asInteger(value);
	posIntCheck(ix, what);
	Rf_dpptr(dd)->fontsub = Rf_gpptr(dd)->fontsub = ix;
    }
    else if (streql(what, "font.axis")) {
	lengthCheck(what, value, 1);
	ix = asInteger(value);
	posIntCheck(ix, what);
	Rf_dpptr(dd)->fontaxis = Rf_gpptr(dd)->fontaxis = ix;
    }
    else if(streql(what, "gamma")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	posRealCheck(x, what);
	if (((GEDevDesc*) dd)->dev->canChangeGamma)
	    Rf_dpptr(dd)->gamma = Rf_gpptr(dd)->gamma = x;
	else
	    warningcall(gcall, "gamma cannot be modified on this device");
    }
    else if (streql(what, "lab")) {
	value = coerceVector(value, INTSXP);
	lengthCheck(what, value, 3);
	posIntCheck   (INTEGER(value)[0], what);
	posIntCheck   (INTEGER(value)[1], what);
	nonnegIntCheck(INTEGER(value)[2], what);
	Rf_dpptr(dd)->lab[0] = Rf_gpptr(dd)->lab[0] = INTEGER(value)[0];
	Rf_dpptr(dd)->lab[1] = Rf_gpptr(dd)->lab[1] = INTEGER(value)[1];
	Rf_dpptr(dd)->lab[2] = Rf_gpptr(dd)->lab[2] = INTEGER(value)[2];
    }
    else if (streql(what, "las")) {
	lengthCheck(what, value, 1);	ix = asInteger(value);
	if (0 <= ix && ix <= 3)
	    Rf_dpptr(dd)->las = Rf_gpptr(dd)->las = ix;
	else par_error(what);
    }
    else if (streql(what, "lty")) {
	lengthCheck(what, value, 1);
	Rf_dpptr(dd)->lty = Rf_gpptr(dd)->lty = LTYpar(value, 0);
    }
    else if (streql(what, "lwd")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	posRealCheck(x, what);
	Rf_dpptr(dd)->lwd = Rf_gpptr(dd)->lwd = x;
    }
    else if (streql(what, "mai")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 4);
	nonnegRealCheck(REAL(value)[0], what);
	nonnegRealCheck(REAL(value)[1], what);
	nonnegRealCheck(REAL(value)[2], what);
	nonnegRealCheck(REAL(value)[3], what);
	Rf_dpptr(dd)->mai[0] = Rf_gpptr(dd)->mai[0] = REAL(value)[0];
	Rf_dpptr(dd)->mai[1] = Rf_gpptr(dd)->mai[1] = REAL(value)[1];
	Rf_dpptr(dd)->mai[2] = Rf_gpptr(dd)->mai[2] = REAL(value)[2];
	Rf_dpptr(dd)->mai[3] = Rf_gpptr(dd)->mai[3] = REAL(value)[3];
	Rf_dpptr(dd)->mUnits = Rf_gpptr(dd)->mUnits = INCHES;
	Rf_dpptr(dd)->defaultPlot = Rf_gpptr(dd)->defaultPlot = TRUE;
	GReset(dd);
    }
    else if (streql(what, "mar")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 4);
	nonnegRealCheck(REAL(value)[0], what);
	nonnegRealCheck(REAL(value)[1], what);
	nonnegRealCheck(REAL(value)[2], what);
	nonnegRealCheck(REAL(value)[3], what);
	Rf_dpptr(dd)->mar[0] = Rf_gpptr(dd)->mar[0] = REAL(value)[0];
	Rf_dpptr(dd)->mar[1] = Rf_gpptr(dd)->mar[1] = REAL(value)[1];
	Rf_dpptr(dd)->mar[2] = Rf_gpptr(dd)->mar[2] = REAL(value)[2];
	Rf_dpptr(dd)->mar[3] = Rf_gpptr(dd)->mar[3] = REAL(value)[3];
	Rf_dpptr(dd)->mUnits = Rf_gpptr(dd)->mUnits = LINES;
	Rf_dpptr(dd)->defaultPlot = Rf_gpptr(dd)->defaultPlot = TRUE;
	GReset(dd);
    }
    else if (streql(what, "mex")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	posRealCheck(x, what);
	Rf_dpptr(dd)->mex = Rf_gpptr(dd)->mex = x;
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
	Rf_gpptr(dd)->numrows = Rf_dpptr(dd)->numrows = nrow;
	Rf_gpptr(dd)->numcols = Rf_dpptr(dd)->numcols = ncol;
	Rf_gpptr(dd)->currentFigure = Rf_dpptr(dd)->currentFigure = nrow*ncol;
	Rf_gpptr(dd)->lastFigure = Rf_dpptr(dd)->lastFigure = nrow*ncol;
	Rf_gpptr(dd)->defaultFigure = Rf_dpptr(dd)->defaultFigure = TRUE;
	Rf_gpptr(dd)->layout = Rf_dpptr(dd)->layout = FALSE;
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
	Rf_dpptr(dd)->mfind = Rf_gpptr(dd)->mfind = 0;
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
	Rf_gpptr(dd)->numrows = Rf_dpptr(dd)->numrows = nrow;
	Rf_gpptr(dd)->numcols = Rf_dpptr(dd)->numcols = ncol;
	Rf_gpptr(dd)->currentFigure = Rf_dpptr(dd)->currentFigure = nrow*ncol;
	Rf_gpptr(dd)->lastFigure = Rf_dpptr(dd)->lastFigure = nrow*ncol;
	Rf_gpptr(dd)->defaultFigure = Rf_dpptr(dd)->defaultFigure = TRUE;
	Rf_gpptr(dd)->layout = Rf_dpptr(dd)->layout = FALSE;
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
	Rf_dpptr(dd)->mfind = Rf_gpptr(dd)->mfind = 1;
	GReset(dd);
    }
    else if (streql(what, "mfg")) {
	int row, col, nrow, ncol, np;
	value = coerceVector(value, INTSXP);
	np = length(value);
	if(np != 2 && np != 4)
	    errorcall(gcall, "parameter \"mfg\" has the wrong length");
	posIntCheck(INTEGER(value)[0], what);
	posIntCheck(INTEGER(value)[1], what);
	row = INTEGER(value)[0];
	col = INTEGER(value)[1];
	nrow = Rf_dpptr(dd)->numrows;
	ncol = Rf_dpptr(dd)->numcols;
	if(row <= 0 || row > nrow)
	    errorcall(gcall, "parameter \"i\" in \"mfg\" is out of range");
	if(col <= 0 || col > ncol)
	    errorcall(gcall, "parameter \"j\" in \"mfg\" is out of range");
	if(np == 4) {
	    posIntCheck(INTEGER(value)[2], what);
	    posIntCheck(INTEGER(value)[3], what);
	    if(nrow != INTEGER(value)[2])
		warningcall(gcall, "value of nr in \"mfg\" is wrong and will be ignored");
	    if(ncol != INTEGER(value)[3])
		warningcall(gcall, "value of nc in \"mfg\" is wrong and will be ignored");
	}
	Rf_gpptr(dd)->lastFigure = Rf_dpptr(dd)->lastFigure = nrow*ncol;
	/*Rf_dpptr(dd)->mfind = Rf_gpptr(dd)->mfind = 1;*/
	/* currentFigure is 1-based */
	if(Rf_gpptr(dd)->mfind)
	    Rf_dpptr(dd)->currentFigure = (col-1)*nrow + row;
	else Rf_dpptr(dd)->currentFigure = (row-1)*ncol + col;
	/*
	  if (Rf_dpptr(dd)->currentFigure == 0)
	  Rf_dpptr(dd)->currentFigure = Rf_dpptr(dd)->lastFigure;
	*/
	Rf_gpptr(dd)->currentFigure = Rf_dpptr(dd)->currentFigure;
	/* Rf_gpptr(dd)->defaultFigure = Rf_dpptr(dd)->defaultFigure = TRUE;
	Rf_gpptr(dd)->layout = Rf_dpptr(dd)->layout = FALSE; */
	Rf_gpptr(dd)->new = Rf_dpptr(dd)->new = 1;
	/*
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
	*/
	GReset(dd);
	/* Force a device clip */
	if (Rf_dpptr(dd)->canClip)
	    GForceClip(dd);
    }
    else if (streql(what, "mgp")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 3);
	nonnegRealCheck(REAL(value)[0], what);
	nonnegRealCheck(REAL(value)[1], what);
	nonnegRealCheck(REAL(value)[2], what);
	Rf_dpptr(dd)->mgp[0] = Rf_gpptr(dd)->mgp[0] = REAL(value)[0];
	Rf_dpptr(dd)->mgp[1] = Rf_gpptr(dd)->mgp[1] = REAL(value)[1];
	Rf_dpptr(dd)->mgp[2] = Rf_gpptr(dd)->mgp[2] = REAL(value)[2];
    }
    else if (streql(what, "mkh")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	posRealCheck(x, what);
	Rf_dpptr(dd)->mkh = Rf_gpptr(dd)->mkh = x;
    }
    else if (streql(what, "new")) {
	lengthCheck(what, value, 1);	ix = asLogical(value);
	Rf_dpptr(dd)->new = Rf_gpptr(dd)->new = (ix != 0);
    }
    else if (streql(what, "oma")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 4);
	nonnegRealCheck(REAL(value)[0], what);
	nonnegRealCheck(REAL(value)[1], what);
	nonnegRealCheck(REAL(value)[2], what);
	nonnegRealCheck(REAL(value)[3], what);
	Rf_dpptr(dd)->oma[0] = Rf_gpptr(dd)->oma[0] = REAL(value)[0];
	Rf_dpptr(dd)->oma[1] = Rf_gpptr(dd)->oma[1] = REAL(value)[1];
	Rf_dpptr(dd)->oma[2] = Rf_gpptr(dd)->oma[2] = REAL(value)[2];
	Rf_dpptr(dd)->oma[3] = Rf_gpptr(dd)->oma[3] = REAL(value)[3];
	Rf_dpptr(dd)->oUnits = Rf_gpptr(dd)->oUnits = LINES;
	/* !!! Force eject of multiple figures !!! */
	Rf_dpptr(dd)->currentFigure = Rf_gpptr(dd)->currentFigure = Rf_gpptr(dd)->lastFigure;
	GReset(dd);
    }
    else if (streql(what, "omd")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 4);
	BoundsCheck(REAL(value)[0], 0.0, 1.0, what);
	BoundsCheck(REAL(value)[1], 0.0, 1.0, what);
	BoundsCheck(REAL(value)[2], 0.0, 1.0, what);
	BoundsCheck(REAL(value)[3], 0.0, 1.0, what);
	Rf_dpptr(dd)->omd[0] = Rf_gpptr(dd)->omd[0] = REAL(value)[0];
	Rf_dpptr(dd)->omd[1] = Rf_gpptr(dd)->omd[1] = REAL(value)[1];
	Rf_dpptr(dd)->omd[2] = Rf_gpptr(dd)->omd[2] = REAL(value)[2];
	Rf_dpptr(dd)->omd[3] = Rf_gpptr(dd)->omd[3] = REAL(value)[3];
	Rf_dpptr(dd)->oUnits = Rf_gpptr(dd)->oUnits = NDC;
	/* Force eject of multiple figures */
	Rf_dpptr(dd)->currentFigure = Rf_gpptr(dd)->currentFigure = Rf_gpptr(dd)->lastFigure;
	GReset(dd);
    }
    else if (streql(what, "omi")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 4);
	nonnegRealCheck(REAL(value)[0], what);
	nonnegRealCheck(REAL(value)[1], what);
	nonnegRealCheck(REAL(value)[2], what);
	nonnegRealCheck(REAL(value)[3], what);
	Rf_dpptr(dd)->omi[0] = Rf_gpptr(dd)->omi[0] = REAL(value)[0];
	Rf_dpptr(dd)->omi[1] = Rf_gpptr(dd)->omi[1] = REAL(value)[1];
	Rf_dpptr(dd)->omi[2] = Rf_gpptr(dd)->omi[2] = REAL(value)[2];
	Rf_dpptr(dd)->omi[3] = Rf_gpptr(dd)->omi[3] = REAL(value)[3];
	Rf_dpptr(dd)->oUnits = Rf_gpptr(dd)->oUnits = INCHES;
	/* Force eject of multiple figures */
	Rf_dpptr(dd)->currentFigure = Rf_gpptr(dd)->currentFigure = Rf_gpptr(dd)->lastFigure;
	GReset(dd);
    }
    else if (streql(what, "pch")) {
	if (!isVector(value) || LENGTH(value) < 1)
	    par_error(what);
	if (isString(value)) {
	    ix = CHAR(STRING_ELT(value, 0))[0];
	}
	else if (isNumeric(value)) {
	    ix = asInteger(value);
	    nonnegIntCheck(ix, what);
	}
	else par_error(what);
	Rf_dpptr(dd)->pch = Rf_gpptr(dd)->pch = ix;
    }
    else if (streql(what, "pin")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 2);
	nonnegRealCheck(REAL(value)[0], what);
	nonnegRealCheck(REAL(value)[1], what);
	Rf_dpptr(dd)->pin[0] = Rf_gpptr(dd)->pin[0] = REAL(value)[0];
	Rf_dpptr(dd)->pin[1] = Rf_gpptr(dd)->pin[1] = REAL(value)[1];
	Rf_dpptr(dd)->pUnits = Rf_gpptr(dd)->pUnits = INCHES;
	Rf_dpptr(dd)->defaultPlot = Rf_gpptr(dd)->defaultPlot = FALSE;
	GReset(dd);
    }
    else if (streql(what, "plt")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 4);
	nonnegRealCheck(REAL(value)[0], what);
	nonnegRealCheck(REAL(value)[1], what);
	nonnegRealCheck(REAL(value)[2], what);
	nonnegRealCheck(REAL(value)[3], what);
	Rf_dpptr(dd)->plt[0] = Rf_gpptr(dd)->plt[0] = REAL(value)[0];
	Rf_dpptr(dd)->plt[1] = Rf_gpptr(dd)->plt[1] = REAL(value)[1];
	Rf_dpptr(dd)->plt[2] = Rf_gpptr(dd)->plt[2] = REAL(value)[2];
	Rf_dpptr(dd)->plt[3] = Rf_gpptr(dd)->plt[3] = REAL(value)[3];
	Rf_dpptr(dd)->pUnits = Rf_gpptr(dd)->pUnits = NFC;
	Rf_dpptr(dd)->defaultPlot = Rf_gpptr(dd)->defaultPlot = FALSE;
	GReset(dd);
    }
    else if (streql(what, "ps")) {
	lengthCheck(what, value, 1);	ix = asInteger(value);
	nonnegIntCheck(ix, what);
	Rf_dpptr(dd)->ps = Rf_gpptr(dd)->ps = ix;
    }
    else if (streql(what, "pty")) {
	if (!isString(value) || LENGTH(value) < 1)
	    par_error(what);
	ix = CHAR(STRING_ELT(value, 0))[0];
	if (ix == 'm' || ix == 's') {
	    Rf_dpptr(dd)->pty = Rf_gpptr(dd)->pty = ix;
	    Rf_dpptr(dd)->defaultPlot = Rf_gpptr(dd)->defaultPlot = TRUE;
	}
	else par_error(what);
    }
    else if (streql(what, "smo")) {
	lengthCheck(what, value, 1);	ix = asInteger(value);
	nonnegIntCheck(ix, what);
	Rf_dpptr(dd)->smo = Rf_gpptr(dd)->smo = ix;
    }
    else if (streql(what, "srt")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	naRealCheck(x, what);
	Rf_dpptr(dd)->srt = Rf_gpptr(dd)->srt = x;
    }
    /* NOTE: tck and tcl must be treated in parallel. */
    /* If one is NA, the other must be non NA.	If tcl */
    /* is NA then setting tck to NA will reset tck to its */
    /* initial default value.  See also graphics.c. */
    else if (streql(what, "tck")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	Rf_dpptr(dd)->tck = Rf_gpptr(dd)->tck = x;
	if (R_FINITE(x))
	    Rf_dpptr(dd)->tcl = Rf_gpptr(dd)->tcl = NA_REAL;
	else if(!R_FINITE(Rf_dpptr(dd)->tcl))
	    Rf_dpptr(dd)->tcl = Rf_gpptr(dd)->tcl = -0.5;
    }
    else if (streql(what, "tcl")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	Rf_dpptr(dd)->tcl = Rf_gpptr(dd)->tcl = x;
	if (R_FINITE(x))
	    Rf_dpptr(dd)->tck = Rf_gpptr(dd)->tck = NA_REAL;
	else if (!R_FINITE(Rf_dpptr(dd)->tck))
	    Rf_dpptr(dd)->tck = Rf_gpptr(dd)->tck = 0.02;	/* S Default */
    }
    else if (streql(what, "tmag")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	posRealCheck(x, what);
	Rf_dpptr(dd)->tmag = Rf_gpptr(dd)->tmag = x;
    }
    else if (streql(what, "type")) {
	if (!isString(value) || LENGTH(value) < 1)
	    par_error(what);
	ix = CHAR(STRING_ELT(value, 0))[0];
	switch (ix) {
	case 'p':
	case 'l':
	case 'b':
	case 'o':
	case 'c':
	case 's':
	case 'S':
	case 'h':
	case 'n':
	    Rf_dpptr(dd)->type = Rf_gpptr(dd)->type = ix;
	    break;
	default:
	    par_error(what);
	}
    }
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
	if (Rf_gpptr(dd)->xlog) {
	    Rf_gpptr(dd)->logusr[0] = Rf_dpptr(dd)->logusr[0] = REAL(value)[0];
	    Rf_gpptr(dd)->logusr[1] = Rf_dpptr(dd)->logusr[1] = REAL(value)[1];
	    Rf_gpptr(dd)->usr[0] = Rf_dpptr(dd)->usr[0] = pow(10., REAL(value)[0]);
	    Rf_gpptr(dd)->usr[1] = Rf_dpptr(dd)->usr[1] = pow(10., REAL(value)[1]);
	}
	else {
	    Rf_gpptr(dd)->usr[0] = Rf_dpptr(dd)->usr[0] = REAL(value)[0];
	    Rf_gpptr(dd)->usr[1] = Rf_dpptr(dd)->usr[1] = REAL(value)[1];
	    Rf_gpptr(dd)->logusr[0] = Rf_dpptr(dd)->logusr[0] =
		R_Log10(REAL(value)[0]);
	    Rf_gpptr(dd)->logusr[1] = Rf_dpptr(dd)->logusr[1] =
		R_Log10(REAL(value)[1]);
	}
	if (Rf_gpptr(dd)->ylog) {
	    Rf_gpptr(dd)->logusr[2] = Rf_dpptr(dd)->logusr[2] = REAL(value)[2];
	    Rf_gpptr(dd)->logusr[3] = Rf_dpptr(dd)->logusr[3] = REAL(value)[3];
	    Rf_gpptr(dd)->usr[2] = Rf_dpptr(dd)->usr[2] = pow(10., REAL(value)[2]);
	    Rf_gpptr(dd)->usr[3] = Rf_dpptr(dd)->usr[3] = pow(10., REAL(value)[3]);
	}
	else {
	    Rf_gpptr(dd)->usr[2] = Rf_dpptr(dd)->usr[2] = REAL(value)[2];
	    Rf_gpptr(dd)->usr[3] = Rf_dpptr(dd)->usr[3] = REAL(value)[3];
	    Rf_gpptr(dd)->logusr[2] = Rf_dpptr(dd)->logusr[2] =
		R_Log10(REAL(value)[2]);
	    Rf_gpptr(dd)->logusr[3] = Rf_dpptr(dd)->logusr[3] =
		R_Log10(REAL(value)[3]);
	}
	/* Reset Mapping and Axis Parameters */
	GMapWin2Fig(dd);
	GSetupAxis(1, dd);
	GSetupAxis(2, dd);
    }
    else if (streql(what, "xaxp")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 3);
	naRealCheck(REAL(value)[0], what);
	naRealCheck(REAL(value)[1], what);
	posIntCheck((int) (REAL(value)[2]), what);
	Rf_dpptr(dd)->xaxp[0] = Rf_gpptr(dd)->xaxp[0] = REAL(value)[0];
	Rf_dpptr(dd)->xaxp[1] = Rf_gpptr(dd)->xaxp[1] = REAL(value)[1];
	Rf_dpptr(dd)->xaxp[2] = Rf_gpptr(dd)->xaxp[2] = (int)(REAL(value)[2]);
    }
    else if (streql(what, "xaxs")) {
	if (!isString(value) || LENGTH(value) < 1)
	    par_error(what);
	ix = CHAR(STRING_ELT(value, 0))[0];
	if (ix == 's' || ix == 'e' || ix == 'i' || ix == 'r' || ix == 'd')
	    Rf_dpptr(dd)->xaxs = Rf_gpptr(dd)->xaxs = ix;
	else par_error(what);
    }
    else if (streql(what, "xaxt")) {
	if (!isString(value) || LENGTH(value) < 1)
	    par_error(what);
	ix = CHAR(STRING_ELT(value, 0))[0];
	if (ix == 's' || ix == 'l' || ix == 't' || ix == 'n')
	    Rf_dpptr(dd)->xaxt = Rf_gpptr(dd)->xaxt = ix;
	else par_error(what);
    }
    else if (streql(what, "xlog")) {
	lengthCheck(what, value, 1);	ix = asLogical(value);
	if (ix == NA_LOGICAL)
	    par_error(what);
	Rf_dpptr(dd)->xlog = Rf_gpptr(dd)->xlog = (ix != 0);
    }
    else if (streql(what, "xpd")) {
	lengthCheck(what, value, 1);
	ix = asInteger(value);
	if (ix == NA_INTEGER)
	    Rf_dpptr(dd)->xpd = Rf_gpptr(dd)->xpd = 2;
	else
	    Rf_dpptr(dd)->xpd = Rf_gpptr(dd)->xpd = (ix != 0);
    }
    else if (streql(what, "yaxp")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 3);
	naRealCheck(REAL(value)[0], what);
	naRealCheck(REAL(value)[1], what);
	posIntCheck((int) (REAL(value)[2]), what);
	Rf_dpptr(dd)->yaxp[0] = Rf_gpptr(dd)->yaxp[0] = REAL(value)[0];
	Rf_dpptr(dd)->yaxp[1] = Rf_gpptr(dd)->yaxp[1] = REAL(value)[1];
	Rf_dpptr(dd)->yaxp[2] = Rf_gpptr(dd)->yaxp[2] = (int) (REAL(value)[2]);
    }
    else if (streql(what, "yaxs")) {
	if (!isString(value) || LENGTH(value) < 1)
	    par_error(what);
	ix = CHAR(STRING_ELT(value, 0))[0];
	if (ix == 's' || ix == 'e' || ix == 'i' || ix == 'r' || ix == 'd')
	    Rf_dpptr(dd)->yaxs = Rf_gpptr(dd)->yaxs = ix;
	else par_error(what);
    }
    else if (streql(what, "yaxt")) {
	if (!isString(value) || LENGTH(value) < 1)
	    par_error(what);
	ix = CHAR(STRING_ELT(value, 0))[0];
	if (ix == 's' || ix == 'l' || ix == 't' || ix == 'n')
	    Rf_dpptr(dd)->yaxt = Rf_gpptr(dd)->yaxt = ix;
	else par_error(what);
    }
    else if (streql(what, "ylog")) {
	lengthCheck(what, value, 1);	ix = asLogical(value);
	if (ix == NA_LOGICAL)
	    par_error(what);
	Rf_dpptr(dd)->ylog = Rf_gpptr(dd)->ylog = (ix != 0);
    }
    else warningcall(gcall, "parameter \"%s\" can't be set", what);
    return;
}


/* Specify2 -- parameters as arguments from higher-level graphics functions
 * --------
 * Many things in PARALLEL to Specify(.)
 * for par()s not valid here, see comment there.
 */
void Specify2(char *what, SEXP value, DevDesc *dd)
{
    double x;
    int ix = 0;

    if (streql(what, "adj")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	BoundsCheck(x, 0.0, 1.0, what);
	Rf_gpptr(dd)->adj = x;
    }
    else if (streql(what, "ann")) {
	lengthCheck(what, value, 1);	ix = asInteger(value);
	Rf_gpptr(dd)->ann = (ix != 0);
    }
    else if (streql(what, "bg")) {
	lengthCheck(what, value, 1);	ix = RGBpar(value, 0);
/*	naIntCheck(ix, what); */
	Rf_gpptr(dd)->bg = ix;
    }
    else if (streql(what, "bty")) {
	lengthCheck(what, value, 1);
	if (!isString(value))
	    par_error(what);
	ix = CHAR(STRING_ELT(value, 0))[0];
	switch (ix) {
	case 'o': case 'O':
	case 'l': case 'L':
	case '7':
	case 'c': case 'C': case '[':
	case ']':
	case 'u': case 'U':
	case 'n':
	    Rf_gpptr(dd)->bty = ix;
	    break;
	default:
	    par_error(what);
	}
    }
    else if (streql(what, "cex")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	posRealCheck(x, what);
	Rf_gpptr(dd)->cex = x;
	/* Rf_gpptr(dd)->cexbase = x; */
    }
    else if (streql(what, "cex.main")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	posRealCheck(x, what);
	Rf_gpptr(dd)->cexmain = x;
    }
    else if (streql(what, "cex.lab")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	posRealCheck(x, what);
	Rf_gpptr(dd)->cexlab = x;
    }
    else if (streql(what, "cex.sub")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	posRealCheck(x, what);
	Rf_gpptr(dd)->cexsub = x;
    }
    else if (streql(what, "cex.axis")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	posRealCheck(x, what);
	Rf_gpptr(dd)->cexaxis = x;
    }
    else if (streql(what, "col")) {
	lengthCheck(what, value, 1);	ix = RGBpar(value, 0);
/*	naIntCheck(ix, what); */
	Rf_gpptr(dd)->col = ix;
    }
    else if (streql(what, "col.main")) {
	lengthCheck(what, value, 1);	ix = RGBpar(value, 0);
/*	naIntCheck(ix, what); */
	Rf_gpptr(dd)->colmain = ix;
    }
    else if (streql(what, "col.lab")) {
	lengthCheck(what, value, 1);	ix = RGBpar(value, 0);
/*	naIntCheck(ix, what); */
	Rf_gpptr(dd)->collab = ix;
    }
    else if (streql(what, "col.sub")) {
	lengthCheck(what, value, 1);	ix = RGBpar(value, 0);
/*	naIntCheck(ix, what); */
	Rf_gpptr(dd)->colsub = ix;
    }
    else if (streql(what, "col.axis")) {
	lengthCheck(what, value, 1);	ix = RGBpar(value, 0);
/*	naIntCheck(ix, what); */
	Rf_gpptr(dd)->colaxis = ix;
    }
    else if (streql(what, "crt")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	naRealCheck(x, what);
	Rf_gpptr(dd)->crt = x;
    }
    else if (streql(what, "err")) {
	lengthCheck(what, value, 1);	ix = asInteger(value);
	if (ix == 0 || ix == -1)
	    Rf_gpptr(dd)->err = ix;
	else par_error(what);
    }
    else if (streql(what, "fg")) {
	lengthCheck(what, value, 1);	ix = RGBpar(value, 0);
/*	naIntCheck(ix, what); */
	Rf_gpptr(dd)->fg = ix;
    }
    else if (streql(what, "font")) {
	lengthCheck(what, value, 1);	ix = asInteger(value);
	posIntCheck(ix, what);
	Rf_gpptr(dd)->font = ix;
    }
    else if (streql(what, "font.main")) {
	lengthCheck(what, value, 1);	ix = asInteger(value);
	posIntCheck(ix, what);
	Rf_gpptr(dd)->fontmain = ix;
    }
    else if (streql(what, "font.lab")) {
	lengthCheck(what, value, 1);	ix = asInteger(value);
	posIntCheck(ix, what);
	Rf_gpptr(dd)->fontlab = ix;
    }
    else if (streql(what, "font.sub")) {
	lengthCheck(what, value, 1);	ix = asInteger(value);
	posIntCheck(ix, what);
	Rf_gpptr(dd)->fontsub = ix;
    }
    else if (streql(what, "font.axis")) {
	lengthCheck(what, value, 1);	ix = asInteger(value);
	posIntCheck(ix, what);
	Rf_gpptr(dd)->fontaxis = ix;
    }
    else if(streql(what, "gamma")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	posRealCheck(x, what);
	if (((GEDevDesc*) dd)->dev->canChangeGamma)
	    Rf_gpptr(dd)->gamma = x;
	else
	    warningcall(gcall, "gamma cannot be modified on this device");
    }
    else if (streql(what, "lab")) {
	value = coerceVector(value, INTSXP);
	lengthCheck(what, value, 3);
	posIntCheck   (INTEGER(value)[0], what);
	posIntCheck   (INTEGER(value)[1], what);
	nonnegIntCheck(INTEGER(value)[2], what);
	Rf_gpptr(dd)->lab[0] = INTEGER(value)[0];
	Rf_gpptr(dd)->lab[1] = INTEGER(value)[1];
	Rf_gpptr(dd)->lab[2] = INTEGER(value)[2];
    }
    else if (streql(what, "las")) {
	lengthCheck(what, value, 1);	ix = asInteger(value);
	if (0 <= ix && ix <= 3)
	    Rf_gpptr(dd)->las = ix;
	else par_error(what);
    }
    else if (streql(what, "lty")) {
	lengthCheck(what, value, 1);
	Rf_gpptr(dd)->lty = LTYpar(value, 0);
    }
    else if (streql(what, "lwd")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	posRealCheck(x, what);
	Rf_gpptr(dd)->lwd = x;
    }
    else if (streql(what, "mgp")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 3);
	nonnegRealCheck(REAL(value)[0], what);
	nonnegRealCheck(REAL(value)[1], what);
	nonnegRealCheck(REAL(value)[2], what);
	Rf_gpptr(dd)->mgp[0] = REAL(value)[0];
	Rf_gpptr(dd)->mgp[1] = REAL(value)[1];
	Rf_gpptr(dd)->mgp[2] = REAL(value)[2];
    }
    else if (streql(what, "mkh")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	posRealCheck(x, what);
	Rf_gpptr(dd)->mkh = x;
    }
    else if (streql(what, "pch")) {
	if (!isVector(value) || LENGTH(value) < 1)
	    par_error(what);
	if (isString(value)) {
	    ix = CHAR(STRING_ELT(value, 0))[0];
	}
	else if (isNumeric(value)) {
	    ix = asInteger(value);
	    nonnegIntCheck(ix, what);
	}
	else par_error(what);
	Rf_gpptr(dd)->pch = ix;
    }
    else if (streql(what, "smo")) {
	lengthCheck(what, value, 1);	ix = asInteger(value);
	posIntCheck(ix, what);
	Rf_gpptr(dd)->smo = ix;
    }
    else if (streql(what, "srt")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	naRealCheck(x, what);
	Rf_gpptr(dd)->srt = x;
    }
    else if (streql(what, "tck")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	naRealCheck(x, what);
	Rf_gpptr(dd)->tck = x;
    }
    else if (streql(what, "tcl")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	naRealCheck(x, what);
	Rf_gpptr(dd)->tcl = x;
    }
    else if (streql(what, "tmag")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	posRealCheck(x, what);
	Rf_gpptr(dd)->tmag = x;
    }
    else if (streql(what, "type")) {
	if (!isString(value) || LENGTH(value) < 1)
	    par_error(what);
	ix = CHAR(STRING_ELT(value, 0))[0];
	switch (ix) {
	case 'p':
	case 'l':
	case 'b':
	case 'o':
	case 'c':
	case 's':
	case 'S':
	case 'h':
	case 'n':
	    Rf_gpptr(dd)->type = ix;
	    break;
	default:
	    par_error(what);
	}
    }
    else if (streql(what, "xaxp")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 3);
	naRealCheck(REAL(value)[0], what);
	naRealCheck(REAL(value)[1], what);
	posIntCheck((int) (REAL(value)[2]), what);
	Rf_gpptr(dd)->xaxp[0] = REAL(value)[0];
	Rf_gpptr(dd)->xaxp[1] = REAL(value)[1];
	Rf_gpptr(dd)->xaxp[2] = (int)(REAL(value)[2]);
    }
    else if (streql(what, "xaxs")) {
	if (!isString(value) || LENGTH(value) < 1)
	    par_error(what);
	ix = CHAR(STRING_ELT(value, 0))[0];
	if (ix == 's' || ix == 'e' || ix == 'i' || ix == 'r' || ix == 'd')
	    Rf_gpptr(dd)->xaxs = ix;
	else par_error(what);
    }
    else if (streql(what, "xaxt")) {
	if (!isString(value) || LENGTH(value) < 1)
	    par_error(what);
	ix = CHAR(STRING_ELT(value, 0))[0];
	if (ix == 's' || ix == 'l' || ix == 't' || ix == 'n')
	    Rf_gpptr(dd)->xaxt = ix;
	else par_error(what);
    }
    else if (streql(what, "xpd")) {
	lengthCheck(what, value, 1);
	ix = asInteger(value);
	if (ix==NA_INTEGER)
	    Rf_gpptr(dd)->xpd = 2;
	else
	    Rf_gpptr(dd)->xpd = (ix != 0);
    }
    else if (streql(what, "yaxp")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 3);
	naRealCheck(REAL(value)[0], what);
	naRealCheck(REAL(value)[1], what);
	posIntCheck((int) (REAL(value)[2]), what);
	Rf_gpptr(dd)->yaxp[0] = REAL(value)[0];
	Rf_gpptr(dd)->yaxp[1] = REAL(value)[1];
	Rf_gpptr(dd)->yaxp[2] = (int) (REAL(value)[2]);
    }
    else if (streql(what, "yaxs")) {
	if (!isString(value) || LENGTH(value) < 1)
	    par_error(what);
	ix = CHAR(STRING_ELT(value, 0))[0];
	if (ix == 's' || ix == 'e' || ix == 'i' || ix == 'r' || ix == 'd')
	    Rf_gpptr(dd)->yaxs = ix;
	else par_error(what);
    }
    else if (streql(what, "yaxt")) {
	if (!isString(value) || LENGTH(value) < 1)
	    par_error(what);
	ix = CHAR(STRING_ELT(value, 0))[0];
	if (ix == 's' || ix == 'l' || ix == 't' || ix == 'n')
	    Rf_gpptr(dd)->yaxt = ix;
	else par_error(what);
    }
    else warning("parameter \"%s\" couldn't be set in high-level plot() function", what);
}


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
	INTEGER(value)[0] = Rf_dpptr(dd)->ask;
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
	INTEGER(value)[0] = Rf_dpptr(dd)->new;
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
	value = allocVector(INTSXP, 1);
	INTEGER(value)[0] = Rf_dpptr(dd)->smo;
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
    else if (streql(what, "tmag")) {
	value = allocVector(REALSXP, 1);
	REAL(value)[0] = Rf_dpptr(dd)->tmag;
    }
    else if (streql(what, "type")) {
	char buf[2];
	PROTECT(value = allocVector(STRSXP, 1));
	buf[0] = Rf_dpptr(dd)->type;
	buf[1] = '\0';
	SET_STRING_ELT(value, 0, mkChar(buf));
	UNPROTECT(1);
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
	INTEGER(value)[0] = Rf_dpptr(dd)->xlog;
    }
    else if (streql(what, "xpd")) {
	value = allocVector(LGLSXP, 1);
	if (Rf_dpptr(dd)->xpd == 2)
	    INTEGER(value)[0] = NA_INTEGER;
	else
	    INTEGER(value)[0] = Rf_dpptr(dd)->xpd;
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
	INTEGER(value)[0] = Rf_dpptr(dd)->ylog;
    }
    else
	value = R_NilValue;
    return value;
}

SEXP do_par(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP value;
    SEXP originalArgs = args;
    DevDesc *dd;
    int new_spec, nargs;

    checkArity(op, args);
    gcall = call;
    if (NoDevices()) {
	SEXP defdev = GetOption(install("device"), R_NilValue);
	if (isString(defdev) && length(defdev) > 0) {
	    PROTECT(defdev = lang1(install(CHAR(STRING_ELT(defdev, 0)))));
	}
	else errorcall(call, "No active or default device");
	eval(defdev, R_GlobalEnv);
	UNPROTECT(1);
    }
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
		SET_STRING_ELT(newnames, i, R_NilValue);
	    }
	}
	setAttrib(value, R_NamesSymbol, newnames);
	UNPROTECT(2);
    }
    else {
	errorcall(call, "invalid parameter passed to \"par\"");
	return R_NilValue/* -Wall */;
    }
    /* should really only do this if specifying new pars ?  yes! [MM] */
    if (new_spec && call != R_NilValue)
	recordGraphicOperation(op, originalArgs, dd);
    return value;
}

SEXP do_readonlypars(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP result;
    GEDevDesc *dd;
    Rboolean canChangeGamma;
    int nreadonly;

    checkArity(op, args);
    /* need a device open: called from par() which would open a 
       device later, so do it now */
    if (NoDevices()) {
	SEXP defdev = GetOption(install("device"), R_NilValue);
	if (isString(defdev) && length(defdev) > 0) {
	    PROTECT(defdev = lang1(install(CHAR(STRING_ELT(defdev, 0)))));
	}
	else errorcall(call, "No active or default device");
	eval(defdev, R_GlobalEnv);
	UNPROTECT(1);
    }
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

SEXP do_layout(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i, j, nrow, ncol, ncmrow, ncmcol;
    SEXP originalArgs = args;
    DevDesc *dd;

    if (NoDevices()) {
	SEXP defdev = GetOption(install("device"), R_NilValue);
	if (isString(defdev) && length(defdev) > 0) {
	    PROTECT(defdev = lang1(install(CHAR(STRING_ELT(defdev, 0)))));
	}
	else errorcall(call, "No active or default device");
	eval(defdev, R_GlobalEnv);
	UNPROTECT(1);
    }

    checkArity(op, args);
    dd = CurrentDevice();

    /* num.rows: */
    nrow = Rf_dpptr(dd)->numrows = Rf_gpptr(dd)->numrows = INTEGER(CAR(args))[0];
    args = CDR(args);
    /* num.cols: */
    ncol = Rf_dpptr(dd)->numcols = Rf_gpptr(dd)->numcols = INTEGER(CAR(args))[0];
    args = CDR(args);
    /* mat[i,j] == order[i][j] : */
    for (i = 0; i < nrow; i++)
	for (j = 0; j < ncol; j++)
	    Rf_dpptr(dd)->order[i][j] = Rf_gpptr(dd)->order[i][j] =
		INTEGER(CAR(args))[i + j*nrow];
    args = CDR(args);

    /* num.figures: */
    Rf_dpptr(dd)->currentFigure = Rf_gpptr(dd)->currentFigure =
	Rf_dpptr(dd)->lastFigure = Rf_gpptr(dd)->lastFigure = INTEGER(CAR(args))[0];
    args = CDR(args);
    /* col.widths: */
    for (j = 0; j < ncol; j++)
	Rf_dpptr(dd)->widths[j] = Rf_gpptr(dd)->widths[j] = REAL(CAR(args))[j];
    args = CDR(args);
    /* row.heights: */
    for (i = 0; i < nrow; i++)
	Rf_dpptr(dd)->heights[i] = Rf_gpptr(dd)->heights[i] = REAL(CAR(args))[i];
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
    for (i = 0; i < nrow; i++)
	for (j = 0; j < ncol; j++)
	    Rf_dpptr(dd)->respect[i][j] = Rf_gpptr(dd)->respect[i][j]
		= INTEGER(CAR(args))[i + j * nrow];

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

    if (call != R_NilValue)
	recordGraphicOperation(op, originalArgs, dd);
    return R_NilValue;
}
