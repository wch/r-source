/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997, 1998  Robert Gentleman, Ross Ihaka and the R core team.
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
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/*
 *	GRZ-like state information.
 *
 *	This is a quick knock-off of the GRZ library to provide a basic
 *	S-like graphics capability for R.  Basically this bit of code
 *	provides the functionality of the "par" function in S.
 *
 *	"The horror, the horror ..."
 *		Marlon Brando in Apocalypse Now.
 *
 * Main functions:
 *	do_par(.)	and
 *	do_layout(.)	implement R's  par(.), layout()rely on
 *
 *	Specify(.)	[ par(what = value) ]
 *	Specify2(.)	[ <highlevelplot>(what = value) ]
 *	Query(.)	[ par(what) ]
 */

#include "Defn.h"
#include "Mathlib.h"
#include "Graphics.h"/* ../include/Graphics.h :	 "GPar" structure + COMMENTS */


static SEXP gcall;/* par(.)'s call */

SEXP LTYget(int);
char *col2name(unsigned int);

static void par_error(char *what)
{
 error("attempt to set invalid value for graphics parameter \"%s\".\n", what);
}

static void lengthCheck(char *what, SEXP v, int n)
{
 if (length(v) != n)
	errorcall(gcall, "parameter \"%s\" has the wrong length\n", what);
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

static void posRealCheck(double x, char *s)
{
 if (!FINITE(x) || x <= 0)
	par_error(s);
}

static void nonnegRealCheck(double x, char *s)
{
 if (!FINITE(x) || x < 0)
	par_error(s);
}

static void naRealCheck(double x, char *s)
{
 if (!FINITE(x))
	par_error(s);
}

static void BoundsCheck(double x, double a, double b, char *s)
{
 if (!FINITE(x) || (FINITE(a) && x < a) || (FINITE(b) && x > b))
	par_error(s);
}


/* when any one of the layout parameters (which can only be set via par(...))
 * is modified, must call GReset()
 * to update the layout and the transformations between coordinate systems
 */

/* If you ADD a NEW par then do NOT forget to update
 * the code in ../library/base/R/par.R
 */

static int Specify(char *what, SEXP value, DevDesc *dd)
{
    double x;
    int ix=0;

    if (streql(what, "adj")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	if (0.0 <= x && x <= 1.0)
	    dd->dp.adj = dd->gp.adj = x;
	else par_error(what);
    }
    else if (streql(what, "ann")) {
	lengthCheck(what, value, 1);	ix = asInteger(value);
	dd->dp.ann = dd->gp.ann = (ix != 0);
    }
    else if (streql(what, "ask")) {
	lengthCheck(what, value, 1);	ix = asLogical(value);
	dd->dp.ask = dd->gp.ask = (ix != 0);
    }
    else if (streql(what, "bg")) {
	lengthCheck(what, value, 1);	ix = RGBpar(value, 0, dd);
	if (ix != NA_INTEGER) {
	    dd->dp.bg = dd->gp.bg = ix;
	    dd->dp.new = dd->gp.new = 0;
	}
	else par_error(what);
    }
    else if (streql(what, "bty")) {
	if (!isString(value) || LENGTH(value) < 1)
	    par_error(what);
	ix = CHAR(STRING(value)[0])[0];
	if (ix == 'o' || ix == 'l' || ix == '7' || ix == 'c' || ix == 'n') {
	    dd->dp.bty = dd->gp.bty = ix;
	}
    }
    else if (streql(what, "cex")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	if (FINITE(x) && 0.0 < x) {
	    dd->dp.cex = dd->gp.cex = 1.0;
	    dd->dp.cexbase = dd->gp.cexbase = x;
	}
	else par_error(what);
    }
    else if (streql(what, "cex.main")) {
	lengthCheck(what, value, 1);
	x = asReal(value);
	if (FINITE(x) && 0.0 < x)
	    dd->dp.cexmain = dd->gp.cexmain = x;
	else par_error(what);
    }
    else if (streql(what, "cex.lab")) {
	lengthCheck(what, value, 1);
	x = asReal(value);
	if (FINITE(x) && 0.0 < x)
	    dd->dp.cexlab = dd->gp.cexlab = x;
	else par_error(what);
    }
    else if (streql(what, "cex.sub")) {
	lengthCheck(what, value, 1);
	x = asReal(value);
	if (FINITE(x) && 0.0 < x)
	    dd->dp.cexsub = dd->gp.cexsub = x;
	else par_error(what);
    }
    else if (streql(what, "cex.axis")) {
	lengthCheck(what, value, 1);
	x = asReal(value);
	if (x != NA_INTEGER && 0.0 < x)
	    dd->dp.cexaxis = dd->gp.cexaxis = x;
	else par_error(what);
    }
    else if (streql(what, "col")) {
	lengthCheck(what, value, 1);
	ix = RGBpar(value, 0, dd);
	if (ix != NA_INTEGER)
	    dd->dp.col = dd->gp.col = ix;
	else par_error(what);
    }
    else if (streql(what, "col.main")) {
	lengthCheck(what, value, 1);
	ix = RGBpar(value, 0, dd);
	if (ix != NA_INTEGER)
	    dd->dp.colmain = dd->gp.colmain = ix;
	else par_error(what);
    }
    else if (streql(what, "col.lab")) {
	lengthCheck(what, value, 1);
	ix = RGBpar(value, 0, dd);
	if (ix != NA_INTEGER)
	    dd->dp.collab = dd->gp.collab = ix;
	else par_error(what);
    }
    else if (streql(what, "col.sub")) {
	lengthCheck(what, value, 1);
	ix = RGBpar(value, 0, dd);
	if (ix != NA_INTEGER)
	    dd->dp.colsub = dd->gp.colsub = ix;
	else par_error(what);
    }
    else if (streql(what, "col.axis")) {
	lengthCheck(what, value, 1);
	ix = RGBpar(value, 0, dd);
	if (ix != NA_INTEGER)
	    dd->dp.colaxis = dd->gp.colaxis = ix;
	else par_error(what);
    }
    else if (streql(what, "crt")) {
	lengthCheck(what, value, 1);
	x = asReal(value);
	if (FINITE(x))
	    dd->dp.crt = dd->gp.crt = x;
	else par_error(what);
    }
    else if (streql(what, "err")) {
	lengthCheck(what, value, 1);
	ix = asInteger(value);
	if (ix == 0 || ix == -1)
	    dd->dp.err = dd->gp.err = ix;
	else par_error(what);
    }
    else if (streql(what, "fg")) {
	lengthCheck(what, value, 1);
	ix = RGBpar(value, 0, dd);
	if (ix != NA_INTEGER)
	    dd->dp.col = dd->gp.col = dd->dp.fg = dd->gp.fg = ix;
	else par_error(what);
    }
    else if (streql(what, "fig")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 4);
	if (0.0 <= REAL(value)[0] && REAL(value)[0] < REAL(value)[1] &&
	    REAL(value)[1] <= 1.0 &&
	    0.0 <= REAL(value)[2] && REAL(value)[2] < REAL(value)[3] &&
	    REAL(value)[3] <= 1.0) {
	    dd->gp.defaultFigure = dd->dp.defaultFigure = 0;
	    dd->gp.fUnits = dd->dp.fUnits = NIC;
	    dd->gp.numrows = dd->dp.numrows = 1;
	    dd->gp.numcols = dd->dp.numcols = 1;
	    dd->gp.heights[0] = dd->dp.heights[0] = 1;
	    dd->gp.widths[0] = dd->dp.widths[0] = 1;
	    dd->gp.cmHeights[0] = dd->dp.cmHeights[0] = 0;
	    dd->gp.cmWidths[0] = dd->dp.cmWidths[0] = 0;
	    dd->gp.order[0][0] = dd->dp.order[0][0] = 1;
	    dd->gp.currentFigure = dd->dp.currentFigure = 1;
	    dd->gp.lastFigure = dd->dp.lastFigure = 1;
	    dd->dp.rspct = dd->gp.rspct = 0;
	    dd->gp.fig[0] = dd->dp.fig[0] = REAL(value)[0];
	    dd->gp.fig[1] = dd->dp.fig[1] = REAL(value)[1];
	    dd->gp.fig[2] = dd->dp.fig[2] = REAL(value)[2];
	    dd->gp.fig[3] = dd->dp.fig[3] = REAL(value)[3];
	    GReset(dd);
	}
	else par_error(what);
    }
    else if (streql(what, "fin")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 2);
	dd->gp.defaultFigure = dd->dp.defaultFigure = 0;
	dd->gp.fUnits = dd->dp.fUnits = INCHES;
	dd->gp.numrows = dd->dp.numrows = 1;
	dd->gp.numcols = dd->dp.numcols = 1;
	dd->gp.heights[0] = dd->dp.heights[0] = 1;
	dd->gp.widths[0] = dd->dp.widths[0] = 1;
	dd->gp.cmHeights[0] = dd->dp.cmHeights[0] = 0;
	dd->gp.cmWidths[0] = dd->dp.cmWidths[0] = 0;
	dd->gp.order[0][0] = dd->dp.order[0][0] = 1;
	dd->gp.currentFigure = dd->dp.currentFigure = 1;
	dd->gp.lastFigure = dd->dp.lastFigure = 1;
	dd->dp.rspct = dd->gp.rspct = 0;
	dd->gp.fin[0] = dd->dp.fin[0] = REAL(value)[0];
	dd->gp.fin[1] = dd->dp.fin[1] = REAL(value)[1];
	GReset(dd);
    }
    else if (streql(what, "font")) {
	lengthCheck(what, value, 1);
	ix = asInteger(value);
	if (ix != NA_INTEGER && ix > 0)
	    dd->dp.font = dd->gp.font = ix;
	else par_error(what);
    }
    else if (streql(what, "font.main")) {
	lengthCheck(what, value, 1);
	ix = asInteger(value);
	if (ix != NA_INTEGER && ix > 0)
	    dd->dp.fontmain = dd->gp.fontmain = ix;
	else par_error(what);
    }
    else if (streql(what, "font.lab")) {
	lengthCheck(what, value, 1);
	ix = asInteger(value);
	if (ix != NA_INTEGER && ix > 0)
	    dd->dp.fontlab = dd->gp.fontlab = ix;
	else par_error(what);
    }
    else if (streql(what, "font.sub")) {
	lengthCheck(what, value, 1);
	ix = asInteger(value);
	if (ix != NA_INTEGER && ix > 0)
	    dd->dp.fontsub = dd->gp.fontsub = ix;
	else par_error(what);
    }
    else if (streql(what, "font.axis")) {
	lengthCheck(what, value, 1);
	ix = asInteger(value);
	if (ix != NA_INTEGER && ix > 0)
	    dd->dp.fontaxis = dd->gp.fontaxis = ix;
	else par_error(what);
    }
    else if(streql(what, "gamma")) {
	lengthCheck(what, value, 1);
	x = asReal(value);
	if (FINITE(x) && x > 0)
	    dd->dp.gamma = dd->gp.gamma = x;
	else par_error(what);
    }
    else if (streql(what, "lab")) {
	value = coerceVector(value, INTSXP);
	lengthCheck(what, value, 3);
	nonnegIntCheck(INTEGER(value)[0], what);
	nonnegIntCheck(INTEGER(value)[0], what);
	nonnegIntCheck(INTEGER(value)[0], what);
	dd->dp.lab[0] = dd->gp.lab[0] = INTEGER(value)[0];
	dd->dp.lab[1] = dd->gp.lab[1] = INTEGER(value)[1];
	dd->dp.lab[2] = dd->gp.lab[2] = INTEGER(value)[2];
    }
    else if (streql(what, "las")) {
	lengthCheck(what, value, 1);
	ix = asInteger(value);
	if (0 <= ix && ix <= 2)
	    dd->dp.las = dd->gp.las = ix;
	else par_error(what);
    }
    else if (streql(what, "lty")) {
	lengthCheck(what, value, 1);
	dd->dp.lty = dd->gp.lty = LTYpar(value, 0);
    }
    else if (streql(what, "lwd")) {
	lengthCheck(what, value, 1);
	x = asReal(value);
	if (FINITE(x) && 0.0 < x)
	    dd->dp.lwd = dd->gp.lwd = x;
	else par_error(what);
    }
    else if (streql(what, "mai")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 4);
	nonnegRealCheck(REAL(value)[0], what);
	nonnegRealCheck(REAL(value)[1], what);
	nonnegRealCheck(REAL(value)[2], what);
	nonnegRealCheck(REAL(value)[3], what);
	dd->dp.mai[0] = dd->gp.mai[0] = REAL(value)[0];
	dd->dp.mai[1] = dd->gp.mai[1] = REAL(value)[1];
	dd->dp.mai[2] = dd->gp.mai[2] = REAL(value)[2];
	dd->dp.mai[3] = dd->gp.mai[3] = REAL(value)[3];
	dd->dp.mUnits = dd->gp.mUnits = INCHES;
	dd->dp.defaultPlot = dd->gp.defaultPlot = 1;
	GReset(dd);
    }
    else if (streql(what, "mar")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 4);
	nonnegRealCheck(REAL(value)[0], what);
	nonnegRealCheck(REAL(value)[1], what);
	nonnegRealCheck(REAL(value)[2], what);
	nonnegRealCheck(REAL(value)[3], what);
	dd->dp.mar[0] = dd->gp.mar[0] = REAL(value)[0];
	dd->dp.mar[1] = dd->gp.mar[1] = REAL(value)[1];
	dd->dp.mar[2] = dd->gp.mar[2] = REAL(value)[2];
	dd->dp.mar[3] = dd->gp.mar[3] = REAL(value)[3];
	dd->dp.mUnits = dd->gp.mUnits = LINES;
	dd->dp.defaultPlot = dd->gp.defaultPlot = 1;
	GReset(dd);
    }
    else if (streql(what, "mex")) {
	lengthCheck(what, value, 1);
	x = asReal(value);
	posRealCheck(x, what);
	dd->dp.mex = dd->gp.mex = x;
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
	dd->gp.numrows = dd->dp.numrows = nrow;
	dd->gp.numcols = dd->dp.numcols = ncol;
	dd->gp.currentFigure = dd->dp.currentFigure = nrow*ncol;
	dd->gp.lastFigure = dd->dp.lastFigure = nrow*ncol;
	dd->gp.defaultFigure = dd->dp.defaultFigure = 1;
	dd->gp.layout = dd->dp.layout = 0;
	if (nrow > 2 || ncol > 2) {
	    dd->gp.cexbase = dd->dp.cexbase = 0.5;
	    dd->gp.mex = dd->dp.mex = 1.0;
	}
	else if (nrow == 2 && ncol == 2) {
	    dd->gp.cexbase = dd->dp.cexbase = 0.8;
	    dd->gp.mex = dd->dp.mex = 1.0;
	}
	else {
	    dd->gp.cexbase = dd->dp.cexbase = 1.0;
	    dd->gp.mex = dd->dp.mex = 1.0;
	}
	dd->dp.mfind = dd->gp.mfind = 0;
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
	dd->gp.numrows = dd->dp.numrows = nrow;
	dd->gp.numcols = dd->dp.numcols = ncol;
	dd->gp.currentFigure = dd->dp.currentFigure = nrow*ncol;
	dd->gp.lastFigure = dd->dp.lastFigure = nrow*ncol;
	dd->gp.defaultFigure = dd->dp.defaultFigure = 1;
	dd->gp.layout = dd->dp.layout = 0;
	if (nrow > 2 || ncol > 2) {
	    dd->gp.cexbase = dd->dp.cexbase = 0.5;
	    dd->gp.mex = dd->dp.mex = 1.0;
	}
	else if (nrow == 2 && ncol == 2) {
	    dd->gp.cexbase = dd->dp.cexbase = 0.8;
	    dd->gp.mex = dd->dp.mex = 1.0;
	}
	else {
	    dd->gp.cexbase = dd->dp.cexbase = 1.0;
	    dd->gp.mex = dd->dp.mex = 1.0;
	}
	dd->dp.mfind = dd->gp.mfind = 1;
	GReset(dd);
    }
    else if (streql(what, "mfg")) {
	int row, col, nrow, ncol;
	value = coerceVector(value, INTSXP);
	lengthCheck(what, value, 4);
	posIntCheck(INTEGER(value)[0], what);
	posIntCheck(INTEGER(value)[1], what);
	posIntCheck(INTEGER(value)[2], what);
	posIntCheck(INTEGER(value)[3], what);
	row = INTEGER(value)[0];
	col = INTEGER(value)[1];
	nrow = INTEGER(value)[2];
	ncol = INTEGER(value)[3];
	dd->gp.numrows = dd->dp.numrows = nrow;
	dd->gp.numcols = dd->dp.numcols = ncol;
	dd->gp.lastFigure = dd->dp.lastFigure = nrow*ncol;
	dd->dp.mfind = dd->gp.mfind = 1;
		/* currentFigure is 1-based */
	dd->dp.currentFigure = (col-1)*nrow + row;
	/*
	if (dd->dp.currentFigure == 0)
	    dd->dp.currentFigure = dd->dp.lastFigure;
	*/
	dd->gp.currentFigure = dd->dp.currentFigure;
	dd->gp.defaultFigure = dd->dp.defaultFigure = 1;
	dd->gp.layout = dd->dp.layout = 0;
	dd->gp.new = dd->dp.new = 1;
	if (nrow > 2 || ncol > 2) {
	    dd->gp.cexbase = dd->dp.cexbase = 0.5;
	    dd->gp.mex = dd->dp.mex = 1.0;
	}
	else if (nrow == 2 && ncol == 2) {
	    dd->gp.cexbase = dd->dp.cexbase = 0.8;
	    dd->gp.mex = dd->dp.mex = 1.0;
	}
	else {
	    dd->gp.cexbase = dd->dp.cexbase = 1.0;
	    dd->gp.mex = dd->dp.mex = 1.0;
	}
	GReset(dd);
    }
    else if (streql(what, "mgp")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 3);
	nonnegRealCheck(REAL(value)[0], what);
	nonnegRealCheck(REAL(value)[1], what);
	nonnegRealCheck(REAL(value)[2], what);
	dd->dp.mgp[0] = dd->gp.mgp[0] = REAL(value)[0];
	dd->dp.mgp[1] = dd->gp.mgp[1] = REAL(value)[1];
	dd->dp.mgp[2] = dd->gp.mgp[2] = REAL(value)[2];
    }
    else if (streql(what, "mkh")) {
	lengthCheck(what, value, 1);
	x = asReal(value);
	posRealCheck(x, what);
	dd->dp.mkh = dd->gp.mkh = x;
    }
    else if (streql(what, "new")) {
	lengthCheck(what, value, 1);
	ix = asLogical(value);
	dd->dp.new = dd->gp.new = (ix != 0);
    }
    else if (streql(what, "oma")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 4);
	nonnegRealCheck(REAL(value)[0], what);
	nonnegRealCheck(REAL(value)[1], what);
	nonnegRealCheck(REAL(value)[2], what);
	nonnegRealCheck(REAL(value)[3], what);
	dd->dp.oma[0] = dd->gp.oma[0] = REAL(value)[0];
	dd->dp.oma[1] = dd->gp.oma[1] = REAL(value)[1];
	dd->dp.oma[2] = dd->gp.oma[2] = REAL(value)[2];
	dd->dp.oma[3] = dd->gp.oma[3] = REAL(value)[3];
	dd->dp.oUnits = dd->gp.oUnits = LINES;
	/* !!! Force eject of multiple figures !!! */
	dd->dp.currentFigure = dd->gp.currentFigure = dd->gp.lastFigure;
	GReset(dd);
    }
    else if (streql(what, "omd")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 4);
	BoundsCheck(REAL(value)[0], 0.0, 1.0, what);
	BoundsCheck(REAL(value)[1], 0.0, 1.0, what);
	BoundsCheck(REAL(value)[2], 0.0, 1.0, what);
	BoundsCheck(REAL(value)[3], 0.0, 1.0, what);
	dd->dp.omd[0] = dd->gp.omd[0] = REAL(value)[0];
	dd->dp.omd[1] = dd->gp.omd[1] = REAL(value)[1];
	dd->dp.omd[2] = dd->gp.omd[2] = REAL(value)[2];
	dd->dp.omd[3] = dd->gp.omd[3] = REAL(value)[3];
	dd->dp.oUnits = dd->gp.oUnits = NDC;
	/* Force eject of multiple figures */
	dd->dp.currentFigure = dd->gp.currentFigure = dd->gp.lastFigure;
	GReset(dd);
    }
    else if (streql(what, "omi")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 4);
	nonnegRealCheck(REAL(value)[0], what);
	nonnegRealCheck(REAL(value)[1], what);
	nonnegRealCheck(REAL(value)[2], what);
	nonnegRealCheck(REAL(value)[3], what);
	dd->dp.omi[0] = dd->gp.omi[0] = REAL(value)[0];
	dd->dp.omi[1] = dd->gp.omi[1] = REAL(value)[1];
	dd->dp.omi[2] = dd->gp.omi[2] = REAL(value)[2];
	dd->dp.omi[3] = dd->gp.omi[3] = REAL(value)[3];
	dd->dp.oUnits = dd->gp.oUnits = INCHES;
	/* Force eject of multiple figures */
	dd->dp.currentFigure = dd->gp.currentFigure = dd->gp.lastFigure;
	GReset(dd);
    }
    else if (streql(what, "pch")) {
	if (!isVector(value) || LENGTH(value) < 1)
	    par_error(what);
	if (isString(value)) {
	    ix = CHAR(STRING(value)[0])[0];
	}
	else if (isNumeric(value)) {
	    ix = asInteger(value);
	    if (ix == NA_INTEGER || ix < 0)
		par_error(what);
	}
	else par_error(what);
	dd->dp.pch = dd->gp.pch = ix;
    }
    else if (streql(what, "pin")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 2);
	nonnegRealCheck(REAL(value)[0], what);
	nonnegRealCheck(REAL(value)[1], what);
	dd->dp.pin[0] = dd->gp.pin[0] = REAL(value)[0];
	dd->dp.pin[1] = dd->gp.pin[1] = REAL(value)[0];
	dd->dp.pUnits = dd->gp.pUnits = INCHES;
	dd->dp.defaultPlot = dd->gp.defaultPlot = 0;
	GReset(dd);
    }
    else if (streql(what, "plt")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 4);
	nonnegRealCheck(REAL(value)[0], what);
	nonnegRealCheck(REAL(value)[1], what);
	nonnegRealCheck(REAL(value)[2], what);
	nonnegRealCheck(REAL(value)[3], what);
	dd->dp.plt[0] = dd->gp.plt[0] = REAL(value)[0];
	dd->dp.plt[1] = dd->gp.plt[1] = REAL(value)[1];
	dd->dp.plt[2] = dd->gp.plt[2] = REAL(value)[2];
	dd->dp.plt[3] = dd->gp.plt[3] = REAL(value)[3];
	dd->dp.pUnits = dd->gp.pUnits = NFC;
	dd->dp.defaultPlot = dd->gp.defaultPlot = 0;
	GReset(dd);
    }
    else if (streql(what, "ps")) {
	lengthCheck(what, value, 1);	ix = asInteger(value);
	if (ix != NA_INTEGER && ix >= 0)
	    dd->dp.ps = dd->gp.ps = ix;
	else par_error(what);
    }
    else if (streql(what, "pty")) {
	if (!isString(value) || LENGTH(value) < 1)
	    par_error(what);
	ix = CHAR(STRING(value)[0])[0];
	if (ix == 'm' || ix == 's')
	    dd->dp.pty = dd->gp.pty = ix;
	else par_error(what);
    }
    else if (streql(what, "smo")) {
	lengthCheck(what, value, 1);	ix = asInteger(value);
	if (ix == NA_INTEGER || ix <= 0)
	    par_error(what);
	dd->dp.smo = dd->gp.smo = ix;
    }
    else if (streql(what, "srt")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	if (FINITE(x))
	    dd->dp.srt = dd->gp.srt = x;
	else par_error(what);
    }
	/* NOTE: tck and tcl must be treated in parallel.
	 * If one is NA, the other must be non NA.
	 * If tcl is NA then setting tck to NA will reset tck to
	 * its initial default value.
	 * See also graphics.c
	 */
    else if (streql(what, "tck")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	dd->dp.tck = dd->gp.tck = x;
	if (FINITE(x))
	    dd->dp.tcl = dd->gp.tcl = NA_REAL;
	else if(!FINITE(dd->dp.tcl))
	    dd->dp.tcl = dd->gp.tcl = -0.5;
    }
    else if (streql(what, "tcl")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	dd->dp.tcl = dd->gp.tcl = x;
	if (FINITE(x))
	    dd->dp.tck = dd->gp.tck = NA_REAL;
	else if (!FINITE(dd->dp.tck))
	    dd->dp.tck = dd->gp.tck = 0.02;	/* S Default */
    }
    else if (streql(what, "tmag")) {
	lengthCheck(what, value, 1);	x = asReal(value);
	posRealCheck(x, what);
	dd->dp.tmag = dd->gp.tmag = x;
    }
    else if (streql(what, "type")) {
	if (!isString(value) || LENGTH(value) < 1)
	    par_error(what);
	ix = CHAR(STRING(value)[0])[0];
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
	    dd->dp.type = dd->gp.type = ix;
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
	if (REAL(value)[0] == REAL(value)[1] || REAL(value)[2] == REAL(value)[3])
	    par_error(what);
	if (dd->gp.xlog) {
	    dd->gp.logusr[0] = dd->dp.logusr[0] = REAL(value)[0];
	    dd->gp.logusr[1] = dd->dp.logusr[1] = REAL(value)[1];
	    dd->gp.usr[0] = dd->dp.usr[0] = pow(10, REAL(value)[0]);
	    dd->gp.usr[1] = dd->dp.usr[1] = pow(10, REAL(value)[1]);
	}
	else {
	    dd->gp.usr[0] = dd->dp.usr[0] = REAL(value)[0];
	    dd->gp.usr[1] = dd->dp.usr[1] = REAL(value)[1];
	    dd->gp.logusr[0] = dd->dp.logusr[0] =
		Log10(REAL(value)[0]);
	    dd->gp.logusr[1] = dd->dp.logusr[1] =
		Log10(REAL(value)[1]);
	}
	if (dd->gp.ylog) {
	    dd->gp.logusr[2] = dd->dp.logusr[2] = REAL(value)[2];
	    dd->gp.logusr[3] = dd->dp.logusr[3] = REAL(value)[3];
	    dd->gp.usr[2] = dd->dp.usr[2] = pow(10, REAL(value)[2]);
	    dd->gp.usr[3] = dd->dp.usr[3] = pow(10, REAL(value)[3]);
	}
	else {
	    dd->gp.usr[2] = dd->dp.usr[2] = REAL(value)[2];
	    dd->gp.usr[3] = dd->dp.usr[3] = REAL(value)[3];
	    dd->gp.logusr[2] = dd->dp.logusr[2] =
		Log10(REAL(value)[2]);
	    dd->gp.logusr[3] = dd->dp.logusr[3] =
		Log10(REAL(value)[3]);
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
	dd->dp.xaxp[0] = dd->gp.xaxp[0] = REAL(value)[0];
	dd->dp.xaxp[1] = dd->gp.xaxp[1] = REAL(value)[1];
	dd->dp.xaxp[2] = dd->gp.xaxp[2] = (int)(REAL(value)[2]);
    }
    else if (streql(what, "xaxs")) {
	if (!isString(value) || LENGTH(value) < 1)
	    par_error(what);
	ix = CHAR(STRING(value)[0])[0];
	if (ix == 's' || ix == 'e' || ix == 'i' || ix == 'r' || ix == 'd')
	    dd->dp.xaxs = dd->gp.xaxs = ix;
	else par_error(what);
    }
    else if (streql(what, "xaxt")) {
	if (!isString(value) || LENGTH(value) < 1)
	    par_error(what);
	ix = CHAR(STRING(value)[0])[0];
	if (ix == 's' || ix == 'l' || ix == 't' || ix == 'n')
	    dd->dp.xaxt = dd->gp.xaxt = ix;
	else par_error(what);
    }
    else if (streql(what, "xlog")) {
	lengthCheck(what, value, 1);	ix = asLogical(value);
	if (ix == NA_LOGICAL)
	    par_error(what);
	dd->dp.xlog = dd->gp.xlog = (ix != 0);
    }
    else if (streql(what, "xpd")) {
	lengthCheck(what, value, 1);	ix = asInteger(value);
	if (ix == NA_INTEGER)
	    par_error(what);
	dd->dp.xpd = dd->gp.xpd = (ix != 0);
    }
    else if (streql(what, "yaxp")) {
	value = coerceVector(value, REALSXP);
	lengthCheck(what, value, 3);
	naRealCheck(REAL(value)[0], what);
	naRealCheck(REAL(value)[1], what);
	posIntCheck((int) (REAL(value)[2]), what);
	dd->dp.yaxp[0] = dd->gp.yaxp[0] = REAL(value)[0];
	dd->dp.yaxp[1] = dd->gp.yaxp[1] = REAL(value)[1];
	dd->dp.yaxp[2] = dd->gp.yaxp[2] = (int) (REAL(value)[2]);
    }
    else if (streql(what, "yaxs")) {
	if (!isString(value) || LENGTH(value) < 1)
	    par_error(what);
	ix = CHAR(STRING(value)[0])[0];
	if (ix == 's' || ix == 'e' || ix == 'i' || ix == 'r' || ix == 'd')
	    dd->dp.yaxs = dd->gp.yaxs = ix;
	else par_error(what);
    }
    else if (streql(what, "yaxt")) {
	if (!isString(value) || LENGTH(value) < 1)
	    par_error(what);
	ix = CHAR(STRING(value)[0])[0];
	if (ix == 's' || ix == 'l' || ix == 'n')
	    dd->dp.yaxt = dd->gp.yaxt = ix;
	else par_error(what);
    }
    else if (streql(what, "ylog")) {
	lengthCheck(what, value, 1);	ix = asLogical(value);
	if (ix == NA_LOGICAL)
	    par_error(what);
	dd->dp.ylog = dd->gp.ylog = (ix != 0);
    }
    else warningcall(gcall, "parameter \"%s\" can't be set\n", what);
    return 0;/* never used; to keep -Wall happy */
}

/* Specify2 -- parameters as arguments from higher-level graphics functions */
void Specify2(char *what, SEXP value, DevDesc *dd)
{
	double x;
	int ix=0;

	if (streql(what, "adj")) {
		lengthCheck(what, value, 1);	x = asReal(value);
		if (0.0 <= x && x <= 1.0)
			dd->gp.adj = x;
		else par_error(what);
	}
	else if (streql(what, "ann")) {
		lengthCheck(what, value, 1);	ix = asInteger(value);
		dd->gp.ann = (ix != 0);
	}
	else if (streql(what, "bg")) {
		lengthCheck(what, value, 1);	ix = RGBpar(value, 0, dd);
		if (ix != NA_INTEGER)
			dd->gp.bg = ix;
		else par_error(what);
	}
	else if (streql(what, "bty")) {
		if (!isString(value) || LENGTH(value) < 1)
			par_error(what);
		ix = CHAR(STRING(value)[0])[0];
		if (ix == 'o' || ix == 'l' || ix == '7' || ix == 'c' || ix == 'n') {
			dd->gp.bty = ix;
		}
	}
	else if (streql(what, "cex")) {
		lengthCheck(what, value, 1);	x = asReal(value);
		if (FINITE(x) && 0.0 < x) {
			dd->gp.cex = x;
			/* dd->gp.cexbase = x; */
		}
		else par_error(what);
	}
	else if (streql(what, "cex.main")) {
		lengthCheck(what, value, 1);	x = asReal(value);
		if (FINITE(x) && 0.0 < x)
			dd->gp.cexmain = x;
		else par_error(what);
	}
	else if (streql(what, "cex.lab")) {
		lengthCheck(what, value, 1);	x = asReal(value);
		if (FINITE(x) && 0.0 < x)
			dd->gp.cexlab = x;
		else par_error(what);
	}
	else if (streql(what, "cex.sub")) {
		lengthCheck(what, value, 1);	x = asReal(value);
		if (FINITE(x) && 0.0 < x)
			dd->gp.cexsub = x;
		else par_error(what);
	}
	else if (streql(what, "cex.axis")) {
		lengthCheck(what, value, 1);	x = asReal(value);
		if (x != NA_INTEGER && 0.0 < x)
			dd->gp.cexaxis = x;
		else par_error(what);
	}
	else if (streql(what, "col")) {
		lengthCheck(what, value, 1);	ix = RGBpar(value, 0, dd);
		if (ix != NA_INTEGER)
			dd->gp.col = ix;
		else par_error(what);
	}
	else if (streql(what, "col.main")) {
		lengthCheck(what, value, 1);	ix = RGBpar(value, 0, dd);
		if (ix != NA_INTEGER)
			dd->gp.colmain = ix;
		else par_error(what);
	}
	else if (streql(what, "col.lab")) {
		lengthCheck(what, value, 1);	ix = RGBpar(value, 0, dd);
		if (ix != NA_INTEGER)
			dd->gp.collab = ix;
		else par_error(what);
	}
	else if (streql(what, "col.sub")) {
		lengthCheck(what, value, 1);	ix = RGBpar(value, 0, dd);
		if (ix != NA_INTEGER)
			dd->gp.colsub = ix;
		else par_error(what);
	}
	else if (streql(what, "col.axis")) {
		lengthCheck(what, value, 1);	ix = RGBpar(value, 0, dd);
		if (ix != NA_INTEGER)
			dd->gp.colaxis = ix;
		else par_error(what);
	}
	else if (streql(what, "crt")) {
		lengthCheck(what, value, 1);	x = asReal(value);
		if (FINITE(x))
			dd->gp.crt = x;
		else par_error(what);
	}
	else if (streql(what, "err")) {
		lengthCheck(what, value, 1);	ix = asInteger(value);
		if (ix == 0 || ix == -1)
			dd->gp.err = ix;
		else par_error(what);
	}
	else if (streql(what, "fg")) {
		lengthCheck(what, value, 1);	ix = RGBpar(value, 0, dd);
		if (ix != NA_INTEGER)
			dd->gp.fg = ix;
		else par_error(what);
	}
	else if (streql(what, "font")) {
		lengthCheck(what, value, 1);	ix = asInteger(value);
		if (ix != NA_INTEGER && ix > 0)
			dd->gp.font = ix;
		else par_error(what);
	}
	else if (streql(what, "font.main")) {
		lengthCheck(what, value, 1);	ix = asInteger(value);
		if (ix != NA_INTEGER && ix > 0)
			dd->gp.fontmain = ix;
		else par_error(what);
	}
	else if (streql(what, "font.lab")) {
		lengthCheck(what, value, 1);	ix = asInteger(value);
		if (ix != NA_INTEGER && ix > 0)
			dd->gp.fontlab = ix;
		else par_error(what);
	}
	else if (streql(what, "font.sub")) {
		lengthCheck(what, value, 1);	ix = asInteger(value);
		if (ix != NA_INTEGER && ix > 0)
			dd->gp.fontsub = ix;
		else par_error(what);
	}
	else if (streql(what, "font.axis")) {
		lengthCheck(what, value, 1);	ix = asInteger(value);
		if (ix != NA_INTEGER && ix > 0)
			dd->gp.fontaxis = ix;
		else par_error(what);
	}
	else if (streql(what, "lab")) {
		value = coerceVector(value, INTSXP);
		lengthCheck(what, value, 3);
		nonnegIntCheck(INTEGER(value)[0], what);
		nonnegIntCheck(INTEGER(value)[0], what);
		nonnegIntCheck(INTEGER(value)[0], what);
		dd->gp.lab[0] = INTEGER(value)[0];
		dd->gp.lab[1] = INTEGER(value)[1];
		dd->gp.lab[2] = INTEGER(value)[2];
	}
	else if (streql(what, "las")) {
		lengthCheck(what, value, 1);	ix = asInteger(value);
		if (0 <= ix && ix <= 2)
			dd->gp.las = ix;
		else par_error(what);
	}
	else if (streql(what, "lty")) {
		lengthCheck(what, value, 1);	dd->gp.lty = LTYpar(value, 0);
	}
	else if (streql(what, "lwd")) {
		lengthCheck(what, value, 1);	x = asReal(value);
		if (FINITE(x) && 0.0 < x)
			dd->gp.lwd = x;
		else par_error(what);
	}
	else if (streql(what, "mgp")) {
		value = coerceVector(value, REALSXP);
		lengthCheck(what, value, 3);
		nonnegRealCheck(REAL(value)[0], what);
		nonnegRealCheck(REAL(value)[1], what);
		nonnegRealCheck(REAL(value)[2], what);
		dd->gp.mgp[0] = REAL(value)[0];
		dd->gp.mgp[1] = REAL(value)[1];
		dd->gp.mgp[2] = REAL(value)[2];
	}
	else if (streql(what, "mkh")) {
		lengthCheck(what, value, 1);	x = asReal(value);
		posRealCheck(x, what);
		dd->gp.mkh = x;
	}
	else if (streql(what, "pch")) {
		if (!isVector(value) || LENGTH(value) < 1)
			par_error(what);
		if (isString(value)) {
			ix = CHAR(STRING(value)[0])[0];
		}
		else if (isNumeric(value)) {
			ix = asInteger(value);
			if (ix == NA_INTEGER || ix < 0)
				par_error(what);
		}
		else par_error(what);
		dd->gp.pch = ix;
	}
	else if (streql(what, "smo")) {
		lengthCheck(what, value, 1);	ix = asInteger(value);
		if (ix == NA_INTEGER || ix <= 0)
			par_error(what);
		dd->gp.smo = ix;
	}
	else if (streql(what, "srt")) {
		lengthCheck(what, value, 1);	x = asReal(value);
		if (FINITE(x))
			dd->gp.srt = x;
		else par_error(what);
	}
	else if (streql(what, "tck")) {
		lengthCheck(what, value, 1);	x = asReal(value);
		if (FINITE(x))
			dd->gp.tck = x;
		else par_error(what);
	}
	else if (streql(what, "tcl")) {
		lengthCheck(what, value, 1);	x = asReal(value);
		if (FINITE(x))
			dd->gp.tcl = x;
		else par_error(what);
	}
	else if (streql(what, "tmag")) {
		lengthCheck(what, value, 1);	x = asReal(value);
		posRealCheck(x, what);
		dd->gp.tmag = x;
	}
	else if (streql(what, "type")) {
		if (!isString(value) || LENGTH(value) < 1)
			par_error(what);
		ix = CHAR(STRING(value)[0])[0];
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
			dd->gp.type = ix;
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
		dd->gp.xaxp[0] = REAL(value)[0];
		dd->gp.xaxp[1] = REAL(value)[1];
		dd->gp.xaxp[2] = (int)(REAL(value)[2]);
	}
	else if (streql(what, "xaxs")) {
		if (!isString(value) || LENGTH(value) < 1)
			par_error(what);
		ix = CHAR(STRING(value)[0])[0];
		if (ix == 's' || ix == 'e' || ix == 'i' || ix == 'r' || ix == 'd')
			dd->gp.xaxs = ix;
		else par_error(what);
	}
	else if (streql(what, "xaxt")) {
		if (!isString(value) || LENGTH(value) < 1)
			par_error(what);
		ix = CHAR(STRING(value)[0])[0];
		if (ix == 's' || ix == 'l' || ix == 't' || ix == 'n')
			dd->gp.xaxt = ix;
		else par_error(what);
	}
	else if (streql(what, "xpd")) {
		lengthCheck(what, value, 1);	ix = asInteger(value);
		if (ix == NA_INTEGER)
			par_error(what);
		dd->gp.xpd = (ix != 0);
	}
	else if (streql(what, "yaxp")) {
		value = coerceVector(value, REALSXP);
		lengthCheck(what, value, 3);
		naRealCheck(REAL(value)[0], what);
		naRealCheck(REAL(value)[1], what);
		posIntCheck((int) (REAL(value)[2]), what);
		dd->gp.yaxp[0] = REAL(value)[0];
		dd->gp.yaxp[1] = REAL(value)[1];
		dd->gp.yaxp[2] = (int) (REAL(value)[2]);
	}
	else if (streql(what, "yaxs")) {
		if (!isString(value) || LENGTH(value) < 1)
			par_error(what);
		ix = CHAR(STRING(value)[0])[0];
		if (ix == 's' || ix == 'e' || ix == 'i' || ix == 'r' || ix == 'd')
			dd->gp.yaxs = ix;
		else par_error(what);
	}
	else if (streql(what, "yaxt")) {
		if (!isString(value) || LENGTH(value) < 1)
			par_error(what);
		ix = CHAR(STRING(value)[0])[0];
		if (ix == 's' || ix == 'l' || ix == 't' || ix == 'n')
			dd->gp.yaxt = ix;
		else par_error(what);
	}
	else warning("parameter \"%s\" couldn't be set in high-level plot() function\n",
		     what);
}/* end Specify2(.) */

/* Do NOT forget to update  ../library/base/R/par.R if you  ADD a NEW  par !! */

static SEXP Query(char *what, DevDesc *dd)
{
	SEXP value;

	if (streql(what, "adj")) {
		value = allocVector(REALSXP, 1);
		REAL(value)[0] = dd->dp.adj;
	}
	else if (streql(what, "ann")) {
		value = allocVector(INTSXP, 1);
		INTEGER(value)[0] = dd->dp.ann;
	}
	else if (streql(what, "ask")) {
		value = allocVector(LGLSXP, 1);
		INTEGER(value)[0] = dd->dp.ask;
	}
	else if (streql(what, "bg")) {
		value = allocVector(STRSXP, 1);
		STRING(value)[0] = mkChar(col2name(dd->dp.bg));
	}
	else if (streql(what, "bty")) {
		char buf[2];
		PROTECT(value = allocVector(STRSXP, 1));
		buf[0] = dd->dp.bty;
		buf[1] = '\0';
		STRING(value)[0] = mkChar(buf);
		UNPROTECT(1);
	}
	else if (streql(what, "cex")) {
		value = allocVector(REALSXP, 1);
		REAL(value)[0] = dd->dp.cexbase;
	}
	else if (streql(what, "cex.main")) {
		value = allocVector(REALSXP, 1);
		REAL(value)[0] = dd->dp.cexmain;
	}
	else if (streql(what, "cex.lab")) {
		value = allocVector(REALSXP, 1);
		REAL(value)[0] = dd->dp.cexlab;
	}
	else if (streql(what, "cex.sub")) {
		value = allocVector(REALSXP, 1);
		REAL(value)[0] = dd->dp.cexsub;
	}
	else if (streql(what, "cex.axis")) {
		value = allocVector(REALSXP, 1);
		REAL(value)[0] = dd->dp.cexaxis;
	}
	else if (streql(what, "cin")) {
		value = allocVector(REALSXP, 2);
		REAL(value)[0] = dd->dp.cra[0]*dd->dp.ipr[0];
		REAL(value)[1] = dd->dp.cra[1]*dd->dp.ipr[1];
	}
	else if (streql(what, "col")) {
		value = allocVector(STRSXP, 1);
		STRING(value)[0] = mkChar(col2name(dd->dp.col));
	}
	else if (streql(what, "col.main")) {
		value = allocVector(STRSXP, 1);
		STRING(value)[0] = mkChar(col2name(dd->dp.colmain));
	}
	else if (streql(what, "col.lab")) {
		value = allocVector(STRSXP, 1);
		STRING(value)[0] = mkChar(col2name(dd->dp.collab));
	}
	else if (streql(what, "col.sub")) {
		value = allocVector(STRSXP, 1);
		STRING(value)[0] = mkChar(col2name(dd->dp.colsub));
	}
	else if (streql(what, "col.axis")) {
		value = allocVector(STRSXP, 1);
		STRING(value)[0] = mkChar(col2name(dd->dp.colaxis));
	}
	else if (streql(what, "cra")) {
		value = allocVector(REALSXP, 2);
		REAL(value)[0] = dd->dp.cra[0];
		REAL(value)[1] = dd->dp.cra[1];
	}
	else if (streql(what, "crt")) {
		value = allocVector(REALSXP, 1);
		REAL(value)[0] = dd->dp.crt;
	}
	else if (streql(what, "csi")) {
		value = allocVector(REALSXP, 1);
		REAL(value)[0] = GConvertYUnits(1.0, CHARS, INCHES, dd);
	}
	else if (streql(what, "din")) {
		value = allocVector(REALSXP, 2);
		REAL(value)[0] = GConvertXUnits(1.0, NDC, INCHES, dd);
		REAL(value)[1] = GConvertYUnits(1.0, NDC, INCHES, dd);
	}
	else if (streql(what, "err")) {
		value = allocVector(INTSXP, 1);
		INTEGER(value)[0] = dd->dp.err;
	}
	else if (streql(what, "fg")) {
		value = allocVector(STRSXP, 1);
		STRING(value)[0] = mkChar(col2name(dd->dp.fg));
	}
	else if (streql(what, "fig")) {
		value = allocVector(REALSXP, 4);
		REAL(value)[0] = dd->dp.fig[0];
		REAL(value)[1] = dd->dp.fig[1];
		REAL(value)[2] = dd->dp.fig[2];
		REAL(value)[3] = dd->dp.fig[3];
	}
	else if (streql(what, "fin")) {
		value = allocVector(REALSXP, 2);
		REAL(value)[0] = dd->dp.fin[0];
		REAL(value)[1] = dd->dp.fin[1];
	}
	else if (streql(what, "font")) {
		value = allocVector(INTSXP, 1);
		INTEGER(value)[0] = dd->dp.font;
	}
	else if (streql(what, "font.main")) {
		value = allocVector(INTSXP, 1);
		INTEGER(value)[0] = dd->dp.fontmain;
	}
	else if (streql(what, "font.lab")) {
		value = allocVector(INTSXP, 1);
		INTEGER(value)[0] = dd->dp.fontlab;
	}
	else if (streql(what, "font.sub")) {
		value = allocVector(INTSXP, 1);
		INTEGER(value)[0] = dd->dp.fontsub;
	}
	else if (streql(what, "font.axis")) {
		value = allocVector(INTSXP, 1);
		INTEGER(value)[0] = dd->dp.fontaxis;
	}
	else if (streql(what, "gamma")) {
		value = allocVector(REALSXP, 1);
		REAL(value)[0] = dd->dp.gamma;
	}
	else if (streql(what, "lab")) {
		value = allocVector(INTSXP, 3);
		INTEGER(value)[0] = dd->dp.lab[0];
		INTEGER(value)[1] = dd->dp.lab[1];
		INTEGER(value)[2] = dd->dp.lab[2];
	}
	else if (streql(what, "las")) {
		value = allocVector(INTSXP, 1);
		INTEGER(value)[0] = dd->dp.las;
	}
	else if (streql(what, "lty")) {
		value = LTYget(dd->dp.lty);
	}
	else if (streql(what, "lwd")) {
		value =	 allocVector(REALSXP, 1);
		REAL(value)[0] = dd->dp.lwd;
	}
	else if (streql(what, "mai")) {
		value = allocVector(REALSXP, 4);
		REAL(value)[0] = dd->dp.mai[0];
		REAL(value)[1] = dd->dp.mai[1];
		REAL(value)[2] = dd->dp.mai[2];
		REAL(value)[3] = dd->dp.mai[3];
	}
	else if (streql(what, "mar")) {
		value = allocVector(REALSXP, 4);
		REAL(value)[0] = dd->dp.mar[0];
		REAL(value)[1] = dd->dp.mar[1];
		REAL(value)[2] = dd->dp.mar[2];
		REAL(value)[3] = dd->dp.mar[3];
	}
	else if (streql(what, "mex")) {
		value = allocVector(REALSXP, 1);
		REAL(value)[0] = dd->dp.mex;
	}
		/* NOTE that if a complex layout has been specified */
		/* then this simple information may not be very useful */

	else if (streql(what, "mfrow") || streql(what, "mfcol")) {
		value = allocVector(INTSXP, 2);
		INTEGER(value)[0] = dd->dp.numrows;
		INTEGER(value)[1] = dd->dp.numcols;
	}
	else if (streql(what, "mfg")) {
		int row, col;
		value = allocVector(INTSXP, 4);
		currentFigureLocation(&row, &col, dd);
		INTEGER(value)[0] = row+1;
		INTEGER(value)[1] = col+1;
		INTEGER(value)[2] = dd->dp.numrows;
		INTEGER(value)[3] = dd->dp.numcols;
	}
	else if (streql(what, "mgp")) {
		value = allocVector(REALSXP, 3);
		REAL(value)[0] = dd->dp.mgp[0];
		REAL(value)[1] = dd->dp.mgp[1];
		REAL(value)[2] = dd->dp.mgp[2];
	}
	else if (streql(what, "mkh")) {
		value = allocVector(REALSXP, 1);
		REAL(value)[0] = dd->dp.mkh;
	}
	else if (streql(what, "new")) {
		value = allocVector(LGLSXP, 1);
		INTEGER(value)[0] = dd->dp.new;
	}
	else if (streql(what, "oma")) {
		value = allocVector(REALSXP, 4);
		REAL(value)[0] = dd->dp.oma[0];
		REAL(value)[1] = dd->dp.oma[1];
		REAL(value)[2] = dd->dp.oma[2];
		REAL(value)[3] = dd->dp.oma[3];
	}
	else if (streql(what, "omd")) {
		value = allocVector(REALSXP, 4);
		REAL(value)[0] = dd->dp.omd[0];
		REAL(value)[1] = dd->dp.omd[1];
		REAL(value)[2] = dd->dp.omd[2];
		REAL(value)[3] = dd->dp.omd[3];
	}
	else if (streql(what, "omi")) {
		value = allocVector(REALSXP, 4);
		REAL(value)[0] = dd->dp.omi[0];
		REAL(value)[1] = dd->dp.omi[1];
		REAL(value)[2] = dd->dp.omi[2];
		REAL(value)[3] = dd->dp.omi[3];
	}
	else if (streql(what, "pch")) {
		char buf[2];
		if(dd->dp.pch < ' ' || dd->dp.pch > 255) {
			PROTECT(value = allocVector(INTSXP, 1));
			INTEGER(value)[0] = dd->dp.pch;
		}
		else {
			PROTECT(value = allocVector(STRSXP, 1));
			buf[0] = dd->dp.pch;
			buf[1] = '\0';
			STRING(value)[0] = mkChar(buf);
		}
		UNPROTECT(1);
	}
	else if (streql(what, "pin")) {
		value = allocVector(REALSXP, 2);
		REAL(value)[0] = dd->dp.pin[0];
		REAL(value)[1] = dd->dp.pin[1];
	}
	else if (streql(what, "plt")) {
		value = allocVector(REALSXP, 4);
		REAL(value)[0] = dd->dp.plt[0];
		REAL(value)[1] = dd->dp.plt[1];
		REAL(value)[2] = dd->dp.plt[2];
		REAL(value)[3] = dd->dp.plt[3];
	}
	else if (streql(what, "ps")) {
		value = allocVector(INTSXP, 1);
		INTEGER(value)[0] = dd->dp.ps;
	}
	else if (streql(what, "pty")) {
		char buf[2];
		PROTECT(value = allocVector(STRSXP, 1));
		buf[0] = dd->dp.pty;
		buf[1] = '\0';
		STRING(value)[0] = mkChar(buf);
		UNPROTECT(1);
	}
	else if (streql(what, "smo")) {
		value = allocVector(INTSXP, 1);
		INTEGER(value)[0] = dd->dp.smo;
	}
	else if (streql(what, "srt")) {
		value = allocVector(REALSXP, 1);
		REAL(value)[0] = dd->dp.srt;
	}
	else if (streql(what, "tck")) {
		value = allocVector(REALSXP, 1);
		REAL(value)[0] = dd->dp.tck;
	}
	else if (streql(what, "tcl")) {
		value = allocVector(REALSXP, 1);
		REAL(value)[0] = dd->dp.tcl;
	}
	else if (streql(what, "tmag")) {
		value = allocVector(REALSXP, 1);
		REAL(value)[0] = dd->dp.tmag;
	}
	else if (streql(what, "type")) {
		char buf[2];
		PROTECT(value = allocVector(STRSXP, 1));
		buf[0] = dd->dp.type;
		buf[1] = '\0';
		STRING(value)[0] = mkChar(buf);
		UNPROTECT(1);
	}
	else if (streql(what, "usr")) {
		value = allocVector(REALSXP, 4);
		if (dd->gp.xlog) {
			REAL(value)[0] = dd->gp.logusr[0];
			REAL(value)[1] = dd->gp.logusr[1];
		}
		else {
			REAL(value)[0] = dd->dp.usr[0];
			REAL(value)[1] = dd->dp.usr[1];
		}
		if (dd->gp.ylog) {
			REAL(value)[2] = dd->gp.logusr[2];
			REAL(value)[3] = dd->gp.logusr[3];
		}
		else
		{
			REAL(value)[2] = dd->dp.usr[2];
			REAL(value)[3] = dd->dp.usr[3];
		}
	}
	else if (streql(what, "xaxp")) {
		value = allocVector(REALSXP, 3);
		REAL(value)[0] = dd->dp.xaxp[0];
		REAL(value)[1] = dd->dp.xaxp[1];
		REAL(value)[2] = dd->dp.xaxp[2];
	}
	else if (streql(what, "xaxs")) {
		char buf[2];
		PROTECT(value = allocVector(STRSXP, 1));
		buf[0] = dd->dp.xaxs;
		buf[1] = '\0';
		STRING(value)[0] = mkChar(buf);
		UNPROTECT(1);
	}
	else if (streql(what, "xaxt")) {
		char buf[2];
		PROTECT(value = allocVector(STRSXP, 1));
		buf[0] = dd->dp.xaxt;
		buf[1] = '\0';
		STRING(value)[0] = mkChar(buf);
		UNPROTECT(1);
	}
	else if (streql(what, "xlog")) {
		value = allocVector(LGLSXP, 1);
		INTEGER(value)[0] = dd->dp.xlog;
	}
	else if (streql(what, "xpd")) {
		value = allocVector(LGLSXP, 1);
		INTEGER(value)[0] = dd->dp.xpd;
	}
	else if (streql(what, "yaxp")) {
		value = allocVector(REALSXP, 3);
		REAL(value)[0] = dd->dp.yaxp[0];
		REAL(value)[1] = dd->dp.yaxp[1];
		REAL(value)[2] = dd->dp.yaxp[2];
	}
	else if (streql(what, "yaxs")) {
		char buf[2];
		PROTECT(value = allocVector(STRSXP, 1));
		buf[0] = dd->dp.yaxs;
		buf[1] = '\0';
		STRING(value)[0] = mkChar(buf);
		UNPROTECT(1);
	}
	else if (streql(what, "yaxt")) {
		char buf[2];
		PROTECT(value = allocVector(STRSXP, 1));
		buf[0] = dd->dp.xaxt;
		buf[1] = '\0';
		STRING(value)[0] = mkChar(buf);
		UNPROTECT(1);
	}
	else if (streql(what, "ylog")) {
		value = allocVector(LGLSXP, 1);
		INTEGER(value)[0] = dd->dp.ylog;
	}
	else
		value = R_NilValue;
	return value;
}

SEXP do_par(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP ap, vp, value;
	SEXP originalArgs = args;
	DevDesc *dd;
	int new_spec;


	if (NoDevices()) {
	  SEXP defdev = GetOption(install("device"), R_NilValue);
	  if (isString(defdev) && length(defdev) > 0) {
	    PROTECT(defdev = lang1(install(CHAR(STRING(defdev)[0]))));
	  }
	  else errorcall(call, "No active or default device\n");
	  eval(defdev, R_GlobalEnv);
	  UNPROTECT(1);
	}

	gcall = call;
	checkArity(op, args);

	if (!isList(CAR(args)))
		errorcall(call, "invalid parameter passed to \"par\"\n");

	dd = CurrentDevice();
	args = CAR(args);
	new_spec = 0;
	PROTECT(value = allocList(length(args)));
	for (vp = value, ap = args; ap != R_NilValue; vp = CDR(vp), ap = CDR(ap)) {
		if (TAG(ap) != R_NilValue) {
			new_spec = 1;
			CAR(vp) = Query(CHAR(PRINTNAME(TAG(ap))), dd);
			TAG(vp) = TAG(ap);
			Specify(CHAR(PRINTNAME(TAG(ap))), CAR(ap), dd);
		}
		else if (TYPEOF(CAR(ap)) == STRSXP) {
			CAR(vp) = Query(CHAR(STRING(CAR(ap))[0]), dd);
			TAG(vp) = install(CHAR(STRING(CAR(ap))[0]));
		}
	}
	UNPROTECT(1);
		/* should really only do this if specifying new pars ?
		 * yes! [MM] */
	if (new_spec && call != R_NilValue)
		recordGraphicOperation(op, originalArgs, dd);
	return value;
}

SEXP do_layout(SEXP call, SEXP op, SEXP args, SEXP env)
{
  /* layout(num.rows, num.cols, mat,
	    num.figures,
	    col.widths,	row.heights,
	    cm.widths,	 cm.heights,
	    respect,	respect.mat)
   */

	int i, j, nrow, ncol, ncmrow, ncmcol;
	SEXP originalArgs = args;
	DevDesc *dd;

	if (NoDevices()) {
	  SEXP defdev = GetOption(install("device"), R_NilValue);
	  if (isString(defdev) && length(defdev) > 0) {
	    PROTECT(defdev = lang1(install(CHAR(STRING(defdev)[0]))));
	  }
	  else errorcall(call, "No active or default device\n");
	  eval(defdev, R_GlobalEnv);
	  UNPROTECT(1);
	}

	checkArity(op, args);
	dd = CurrentDevice();

	/* num.rows: */
	nrow = dd->dp.numrows = dd->gp.numrows = INTEGER(CAR(args))[0];
	args = CDR(args);
	/* num.cols: */
	ncol = dd->dp.numcols = dd->gp.numcols = INTEGER(CAR(args))[0];
	args = CDR(args);
	/* mat[i,j] == order[i][j] : */
	for (i=0; i<nrow; i++)
		for (j=0; j<ncol; j++)
			dd->dp.order[i][j] = dd->gp.order[i][j] =
				INTEGER(CAR(args))[i + j*nrow];
	args = CDR(args);

	/* num.figures: */
	dd->dp.currentFigure = dd->gp.currentFigure =
	dd->dp.lastFigure = dd->gp.lastFigure = INTEGER(CAR(args))[0];
	args = CDR(args);
	/* col.widths: */
	for (j=0; j<ncol; j++)
		dd->dp.widths[j] = dd->gp.widths[j] = REAL(CAR(args))[j];
	args = CDR(args);
	/* row.heights: */
	for (i=0; i<nrow; i++)
		dd->dp.heights[i] = dd->gp.heights[i] = REAL(CAR(args))[i];
	args = CDR(args);
	/* cm.widths: */
	ncmcol = length(CAR(args));
	for (j=0; j<ncol; j++)
		dd->dp.cmWidths[j] = dd->gp.cmWidths[j] = 0;
	for (j=0; j<ncmcol; j++)
		dd->dp.cmWidths[INTEGER(CAR(args))[j]-1] =
		dd->gp.cmWidths[INTEGER(CAR(args))[j]-1] = 1;
	args = CDR(args);
	/* cm.heights: */
	ncmrow = length(CAR(args));
	for (i=0; i<nrow; i++)
		dd->dp.cmHeights[i] = dd->gp.cmHeights[i] = 0;
	for (i=0; i<ncmrow; i++)
		dd->dp.cmHeights[INTEGER(CAR(args))[i]-1] =
		dd->gp.cmHeights[INTEGER(CAR(args))[i]-1] = 1;
	args = CDR(args);
	/* respect =  0 (FALSE), 1 (TRUE), or 2 (matrix) : */
	dd->dp.rspct = dd->gp.rspct = INTEGER(CAR(args))[0];
	args = CDR(args);
	/* respect.mat */
	for (i=0; i<nrow; i++)
		for (j=0; j<ncol; j++)
			dd->dp.respect[i][j] = dd->gp.respect[i][j] =
				INTEGER(CAR(args))[i + j*nrow];
	/*------------------------------------------------------*/

	if (nrow > 2 || ncol > 2) {
		dd->gp.cexbase = dd->dp.cexbase = 0.5;
		dd->gp.mex = dd->dp.mex = 1.0;
	} else if (nrow == 2 && ncol == 2) {
		dd->gp.cexbase = dd->dp.cexbase = 0.8;
		dd->gp.mex = dd->dp.mex = 1.0;
	} else {
		dd->gp.cexbase = dd->dp.cexbase = 1.0;
		dd->gp.mex = dd->dp.mex = 1.0;
	}

	dd->dp.defaultFigure = dd->gp.defaultFigure = 1;
	dd->dp.layout = dd->gp.layout = 1;

	GReset(dd);

	if (call != R_NilValue)
		recordGraphicOperation(op, originalArgs, dd);
	return R_NilValue;
}




