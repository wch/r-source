/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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
 */

#include "Defn.h"
#include "Mathlib.h"
#include "Graphics.h"

static SEXP gcall;

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

static int Specify(char *what, SEXP value)
{
	double x;
	int ix;

	if (streql(what, "adj")) {
		lengthCheck(what, value, 1);
		x = asReal(value);
		if (0.0 <= x && x <= 1.0)
			DP->adj = GP->adj = x;
		else par_error(what);
	}
	else if (streql(what, "ann")) {
		lengthCheck(what, value, 1);
		ix = asInteger(value);
		DP->ann = GP->ann = (ix != 0);
	}
	else if (streql(what, "ask")) {
		lengthCheck(what, value, 1);
		ix = asLogical(value);
		DP->ask = GP->ask = (ix != 0);
	}
	else if (streql(what, "bg")) {
		lengthCheck(what, value, 1);
		ix = RGBpar(value, 0);
		if (ix != NA_INTEGER) {
			DP->bg = GP->bg = ix;
			DP->new = GP->new = 0;
		}
		else par_error(what);
	}
	else if (streql(what, "bty")) {
		if (!isString(value) || LENGTH(value) < 1)
			par_error(what);
		ix = CHAR(STRING(value)[0])[0];
		if (ix == 'o' || ix == 'l' || ix == '7' || ix == 'c' || ix == 'n') {
			DP->bty = GP->bty = ix;
		}
	}
	else if (streql(what, "cex")) {
		lengthCheck(what, value, 1);
		x = asReal(value);
		if (FINITE(x) && 0.0 < x) {
			DP->cex = GP->cex = 1.0;
			DP->cexbase = GP->cexbase = x;
		}
		else par_error(what);
	}
	else if (streql(what, "cex.main")) {
		lengthCheck(what, value, 1);
		x = asReal(value);
		if (FINITE(x) && 0.0 < x)
			DP->cexmain = GP->cexmain = x;
		else par_error(what);
	}
	else if (streql(what, "cex.lab")) {
		lengthCheck(what, value, 1);
		x = asReal(value);
		if (FINITE(x) && 0.0 < x)
			DP->cexlab = GP->cexlab = x;
		else par_error(what);
	}
	else if (streql(what, "cex.sub")) {
		lengthCheck(what, value, 1);
		x = asReal(value);
		if (FINITE(x) && 0.0 < x)
			DP->cexsub = GP->cexsub = x;
		else par_error(what);
	}
	else if (streql(what, "cex.axis")) {
		lengthCheck(what, value, 1);
		x = asReal(value);
		if (x != NA_INTEGER && 0.0 < x)
			DP->cexaxis = GP->cexaxis = x;
		else par_error(what);
	}
	else if (streql(what, "col")) {
		lengthCheck(what, value, 1);
		ix = RGBpar(value, 0);
		if (ix != NA_INTEGER)
			DP->col = GP->col = ix;
		else par_error(what);
	}
	else if (streql(what, "col.main")) {
		lengthCheck(what, value, 1);
		ix = RGBpar(value, 0);
		if (ix != NA_INTEGER)
			DP->colmain = GP->colmain = ix;
		else par_error(what);
	}
	else if (streql(what, "col.lab")) {
		lengthCheck(what, value, 1);
		ix = RGBpar(value, 0);
		if (ix != NA_INTEGER)
			DP->collab = GP->collab = ix;
		else par_error(what);
	}
	else if (streql(what, "col.sub")) {
		lengthCheck(what, value, 1);
		ix = RGBpar(value, 0);
		if (ix != NA_INTEGER)
			DP->colsub = GP->colsub = ix;
		else par_error(what);
	}
	else if (streql(what, "col.axis")) {
		lengthCheck(what, value, 1);
		ix = RGBpar(value, 0);
		if (ix != NA_INTEGER)
			DP->colaxis = GP->colaxis = ix;
		else par_error(what);
	}
	else if (streql(what, "crt")) {
		lengthCheck(what, value, 1);
		x = asReal(value);
		if (FINITE(x))
			DP->crt = GP->crt = x;
		else par_error(what);
	}
	else if (streql(what, "err")) {
		lengthCheck(what, value, 1);
		ix = asInteger(value);
		if (ix == 0 || ix == -1)
			DP->err = GP->err = ix;
		else par_error(what);
	}
	else if (streql(what, "fg")) {
		lengthCheck(what, value, 1);
		ix = RGBpar(value, 0);
		if (ix != NA_INTEGER)
			DP->col = GP->col = DP->fg = GP->fg = ix;
		else par_error(what);
	}
	else if (streql(what, "fig")) {
		value = coerceVector(value, REALSXP);
		lengthCheck(what, value, 4);
		if (0.0 <= REAL(value)[0] && REAL(value)[0] < REAL(value)[1] && REAL(value)[1] <= 1.0 &&
		    0.0 <= REAL(value)[2] && REAL(value)[2] < REAL(value)[3] && REAL(value)[3] <= 1.0) {
			/* Note the special setting of -1 below. */
			/* This is an indication that the usual computations */
			/* for setting up a plot should be skipped. */
			/* The effect of this is to lock in this */
			/* figure region. */
			GP->mfg[0] = DP->mfg[0] = 1;
			GP->mfg[2] = DP->mfg[2] = 1;
			GP->mfg[1] = DP->mfg[1] = -1;
			GP->mfg[3] = DP->mfg[3] = 1;
			GP->fig[0] = DP->fig[0] = REAL(value)[0];
			GP->fig[1] = DP->fig[1] = REAL(value)[1];
			GP->fig[2] = DP->fig[2] = REAL(value)[2];
			GP->fig[3] = DP->fig[3] = REAL(value)[3];
			GReset();
		}
		else par_error(what);
	}
	else if (streql(what, "fin")) {
		double x, y;
		value = coerceVector(value, REALSXP);
		lengthCheck(what, value, 2);
		x = xInchtoNDC(REAL(value)[0]);
		y = yInchtoNDC(REAL(value)[1]);
		if (0.0 <= x && x <= 1.0 && 0.0 <= y && y <= 1.0) {
			/* Force single figure mode */
			GP->mfg[0] = DP->mfg[0] = 1;
			GP->mfg[2] = DP->mfg[2] = 1;
			GP->mfg[1] = DP->mfg[1] = 1;
			GP->mfg[3] = DP->mfg[3] = 1;
			/* Equal margin space left/right and top/bottom */
			x = xNDCtoChar(0.5 * (1.0 - x)) / DP->mex;
			y = yNDCtoChar(0.5 * (1.0 - y)) / DP->mex;
			GP->oma[0] = DP->oma[0] = y;
			GP->oma[1] = DP->oma[1] = x;
			GP->oma[2] = DP->oma[2] = y;
			GP->oma[3] = DP->oma[3] = x;
			GReset();
		}
		else par_error(what);
	}
	else if (streql(what, "font")) {
		lengthCheck(what, value, 1);
		ix = asInteger(value);
		if (ix != NA_INTEGER && ix > 0)
			DP->font = GP->font = ix;
		else par_error(what);
	}
	else if (streql(what, "font.main")) {
		lengthCheck(what, value, 1);
		ix = asInteger(value);
		if (ix != NA_INTEGER && ix > 0)
			DP->fontmain = GP->fontmain = ix;
		else par_error(what);
	}
	else if (streql(what, "font.lab")) {
		lengthCheck(what, value, 1);
		ix = asInteger(value);
		if (ix != NA_INTEGER && ix > 0)
			DP->fontlab = GP->fontlab = ix;
		else par_error(what);
	}
	else if (streql(what, "font.sub")) {
		lengthCheck(what, value, 1);
		ix = asInteger(value);
		if (ix != NA_INTEGER && ix > 0)
			DP->fontsub = GP->fontsub = ix;
		else par_error(what);
	}
	else if (streql(what, "font.axis")) {
		lengthCheck(what, value, 1);
		ix = asInteger(value);
		if (ix != NA_INTEGER && ix > 0)
			DP->fontaxis = GP->fontaxis = ix;
		else par_error(what);
	}
	else if(streql(what, "gamma")) {
		lengthCheck(what, value, 1);
		x = asReal(value);
		if (FINITE(x) && x > 0)
			DP->gamma = GP->gamma = x;
		else par_error(what);
        }
	else if (streql(what, "lab")) {
		value = coerceVector(value, INTSXP);
		lengthCheck(what, value, 3);
		nonnegIntCheck(INTEGER(value)[0], what);
		nonnegIntCheck(INTEGER(value)[0], what);
		nonnegIntCheck(INTEGER(value)[0], what);
		DP->lab[0] = GP->lab[0] = INTEGER(value)[0];
		DP->lab[1] = GP->lab[1] = INTEGER(value)[1];
		DP->lab[2] = GP->lab[2] = INTEGER(value)[1];
	}
	else if (streql(what, "las")) {
		lengthCheck(what, value, 1);
		ix = asInteger(value);
		if (0 <= ix && ix <= 2)
			DP->las = GP->las = ix;
		else par_error(what);
	}
	else if (streql(what, "lty")) {
		lengthCheck(what, value, 1);
		DP->lty = GP->lty = LTYpar(value, 0);
	}
	else if (streql(what, "lwd")) {
		lengthCheck(what, value, 1);
		x = asReal(value);
		if (FINITE(x) && 0.0 < x)
			DP->lwd = GP->lwd = x;
		else par_error(what);
	}
	else if (streql(what, "mai")) {
		value = coerceVector(value, REALSXP);
		lengthCheck(what, value, 4);
		nonnegRealCheck(REAL(value)[0], what);
		nonnegRealCheck(REAL(value)[1], what);
		nonnegRealCheck(REAL(value)[2], what);
		nonnegRealCheck(REAL(value)[3], what);
		/* convert to "mar" values */
		DP->mar[0] = GP->mar[0] = xInchtoChar(REAL(value)[0]) / (DP->cexbase * DP->mex);
		DP->mar[1] = GP->mar[1] = yInchtoChar(REAL(value)[1]) / (DP->cexbase * DP->mex);
		DP->mar[2] = GP->mar[2] = xInchtoChar(REAL(value)[2]) / (DP->cexbase * DP->mex);
		DP->mar[3] = GP->mar[3] = yInchtoChar(REAL(value)[3]) / (DP->cexbase * DP->mex);
	}
	else if (streql(what, "mar")) {
		value = coerceVector(value, REALSXP);
		lengthCheck(what, value, 4);
		nonnegRealCheck(REAL(value)[0], what);
		nonnegRealCheck(REAL(value)[1], what);
		nonnegRealCheck(REAL(value)[2], what);
		nonnegRealCheck(REAL(value)[3], what);
		DP->mar[0] = GP->mar[0] = REAL(value)[0];
		DP->mar[1] = GP->mar[1] = REAL(value)[1];
		DP->mar[2] = GP->mar[2] = REAL(value)[2];
		DP->mar[3] = GP->mar[3] = REAL(value)[3];
	}
	else if (streql(what, "mex")) {
		lengthCheck(what, value, 1);
		x = asReal(value);
		posRealCheck(x, what);
		DP->mex = GP->mex = x;
	}
	else if (streql(what, "mfrow")) {
		value = coerceVector(value, INTSXP);
		lengthCheck(what, value, 2);
		posIntCheck(INTEGER(value)[0], what);
		posIntCheck(INTEGER(value)[1], what);
		GP->mfind = DP->mfind = 1;
		GP->mfg[0] = DP->mfg[0] = INTEGER(value)[0];
		GP->mfg[2] = DP->mfg[2] = INTEGER(value)[0];
		GP->mfg[1] = DP->mfg[1] = INTEGER(value)[1];
		GP->mfg[3] = DP->mfg[3] = INTEGER(value)[1];
		if (GP->mfg[2] > 2 || GP->mfg[3] > 2) {
			GP->cexbase = DP->cexbase = 0.5;
			GP->mex = DP->mex = 1.0;
		}
		else if (GP->mfg[2] == 2 && GP->mfg[3] == 2) {
			GP->cexbase = DP->cexbase = 0.8;
			GP->mex = DP->mex = 1.0;
		}
		else {
			GP->cexbase = DP->cexbase = 1.0;
			GP->mex = DP->mex = 1.0;
		}
		GReset();
	}
	else if (streql(what, "mfcol")) {
		value = coerceVector(value, INTSXP);
		lengthCheck(what, value, 2);
		posIntCheck(INTEGER(value)[0], what);
		posIntCheck(INTEGER(value)[1], what);
		GP->mfind = DP->mfind = 0;
		GP->mfg[0] = DP->mfg[0] = INTEGER(value)[0];
		GP->mfg[2] = DP->mfg[2] = INTEGER(value)[0];
		GP->mfg[1] = DP->mfg[1] = INTEGER(value)[1];
		GP->mfg[3] = DP->mfg[3] = INTEGER(value)[1];
		if (GP->mfg[2] > 2 || GP->mfg[3] > 2) {
			GP->cexbase = DP->cexbase = 0.5;
			GP->mex = DP->mex = 1.0;
		}
		else if (GP->mfg[2] == 2 && GP->mfg[3] == 2) {
			GP->cexbase = DP->cexbase = 0.8;
			GP->mex = DP->mex = 1.0;
		}
		else {
			GP->cexbase = DP->cexbase = 1.0;
			GP->mex = DP->mex = 1.0;
		}
		GReset();
	}
	else if (streql(what, "mfg")) {
		int ind;
		value = coerceVector(value, INTSXP);
		lengthCheck(what, value, 4);
		posIntCheck(INTEGER(value)[0], what);
		posIntCheck(INTEGER(value)[1], what);
		posIntCheck(INTEGER(value)[2], what);
		posIntCheck(INTEGER(value)[3], what);
		ind = (DP->mfg[2] * DP->mfg[3] == 1);
		GP->mfg[0] = DP->mfg[0] = INTEGER(value)[0];
		GP->mfg[1] = DP->mfg[1] = INTEGER(value)[1];
		GP->mfg[2] = DP->mfg[2] = INTEGER(value)[2];
		GP->mfg[3] = DP->mfg[3] = INTEGER(value)[3];
		if (ind) {
			GP->oma[0] = DP->oma[0] = 0.0;
			GP->oma[2] = DP->oma[2] = 0.0;
			GP->oma[1] = DP->oma[1] = 0.0;
			GP->oma[3] = DP->oma[3] = 0.0;
		}
		if (GP->mfg[2] > 2 || GP->mfg[3] > 2) {
			GP->cexbase = DP->cexbase = 0.5;
			GP->mex = DP->mex = 1.0;
		}
		else if (GP->mfg[2] == 2 && GP->mfg[3] == 2) {
			GP->cexbase = DP->cexbase = 0.8;
			GP->mex = DP->mex = 1.0;
		}
		else {
			GP->cexbase = DP->cexbase = 1.0;
			GP->mex = DP->mex = 1.0;
		}
		GReset();
	}
	else if (streql(what, "mgp")) {
		value = coerceVector(value, REALSXP);
		lengthCheck(what, value, 3);
		nonnegRealCheck(REAL(value)[0], what);
		nonnegRealCheck(REAL(value)[1], what);
		nonnegRealCheck(REAL(value)[2], what);
		DP->mgp[0] = GP->mgp[0] = REAL(value)[0];
		DP->mgp[1] = GP->mgp[1] = REAL(value)[1];
		DP->mgp[2] = GP->mgp[2] = REAL(value)[2];
	}
	else if (streql(what, "mkh")) {
		lengthCheck(what, value, 1);
		x = asReal(value);
		posRealCheck(x, what);
		DP->mkh = GP->mkh = x;
	}
	else if (streql(what, "new")) {
		lengthCheck(what, value, 1);
		ix = asLogical(value);
		DP->new = GP->new = (ix != 0);
	}
	else if (streql(what, "oma")) {
		value = coerceVector(value, REALSXP);
		lengthCheck(what, value, 4);
		nonnegRealCheck(REAL(value)[0], what);
		nonnegRealCheck(REAL(value)[1], what);
		nonnegRealCheck(REAL(value)[2], what);
		nonnegRealCheck(REAL(value)[3], what);
		GP->oma[0] = DP->oma[0] = REAL(value)[0];
		GP->oma[1] = DP->oma[1] = REAL(value)[1];
		GP->oma[2] = DP->oma[2] = REAL(value)[2];
		GP->oma[3] = DP->oma[3] = REAL(value)[3];
		/* !!! Force eject of multiple figures !!! */
		GP->mfg[0] = DP->mfg[0] = DP->mfg[2];
		GP->mfg[1] = DP->mfg[1] = DP->mfg[3];
		GReset();
	}
	else if (streql(what, "omd")) {
		value = coerceVector(value, REALSXP);
		lengthCheck(what, value, 4);
		BoundsCheck(REAL(value)[0], 0.0, 1.0, what);
		BoundsCheck(REAL(value)[1], 0.0, 1.0, what);
		BoundsCheck(REAL(value)[2], 0.0, 1.0, what);
		BoundsCheck(REAL(value)[3], 0.0, 1.0, what);
		GP->oma[0] = DP->oma[0] = xNDCtoChar(REAL(value)[2]) / DP->mex;
		GP->oma[1] = DP->oma[1] = yNDCtoChar(REAL(value)[0]) / DP->mex;
		GP->oma[2] = DP->oma[2] = xNDCtoChar(1.0 - REAL(value)[3]) / DP->mex;
		GP->oma[3] = DP->oma[3] = yNDCtoChar(1.0 - REAL(value)[1]) / DP->mex;
		/* Force eject of multiple figures */
		GP->mfg[0] = DP->mfg[0] = DP->mfg[2];
		GP->mfg[1] = DP->mfg[1] = DP->mfg[3];
		GReset();
	}
	else if (streql(what, "omi")) {
		value = coerceVector(value, REALSXP);
		lengthCheck(what, value, 4);
		nonnegRealCheck(REAL(value)[0], what);
		nonnegRealCheck(REAL(value)[1], what);
		nonnegRealCheck(REAL(value)[2], what);
		nonnegRealCheck(REAL(value)[3], what);
		GP->oma[0] = DP->oma[0] = xInchtoNDC(REAL(value)[0]) / DP->mex;
		GP->oma[1] = DP->oma[1] = yInchtoNDC(REAL(value)[1]) / DP->mex;
		GP->oma[2] = DP->oma[2] = xInchtoNDC(REAL(value)[2]) / DP->mex;
		GP->oma[3] = DP->oma[3] = yInchtoNDC(REAL(value)[3]) / DP->mex;
		/* Force eject of multiple figures */
		GP->mfg[0] = DP->mfg[0] = DP->mfg[2];
		GP->mfg[1] = DP->mfg[1] = DP->mfg[3];
		GReset();
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
		DP->pch = GP->pch = ix;
	}
	else if (streql(what, "pin")) {
		double width, height;
		width = xNDCtoInch(GP->fig2dev.bx / GP->ndc2dev.bx);
		height = yNDCtoInch(GP->fig2dev.by / GP->ndc2dev.by);
		if (0.0 < REAL(value)[0] && REAL(value)[0] < width &&
		    0.0 < REAL(value)[1] && REAL(value)[1] < height) {
			GP->mfg[0] = DP->mfg[0] = 1;
			GP->mfg[1] = DP->mfg[1] = 1;
			GP->mfg[2] = DP->mfg[2] = 1;
			GP->mfg[3] = DP->mfg[3] = 1;
			/* Get the border size in inches */
			width = 0.5 * (width - REAL(value)[0]);
			height = 0.5 * (height - REAL(value)[1]);
			/* Convert to margin lines */
			GP->mar[0] = DP->mar[0] = yInchtoChar(height) / DP->mex;
			GP->mar[1] = DP->mar[1] = xInchtoChar(width) / DP->mex;
			GP->mar[2] = DP->mar[2] = yInchtoChar(height) / DP->mex;
			GP->mar[3] = DP->mar[3] = xInchtoChar(width) / DP->mex;
			GReset();
		}
		else par_error(what);
		GReset();
	}
	else if (streql(what, "plt")) {
		double width, height;
		value = coerceVector(value, REALSXP);
		lengthCheck(what, value, 4);
		nonnegRealCheck(REAL(value)[0], what);
		nonnegRealCheck(REAL(value)[1], what);
		nonnegRealCheck(REAL(value)[2], what);
		nonnegRealCheck(REAL(value)[3], what);
		/* Get the figure size in inches */
		width = xNDCtoInch(GP->fig2dev.bx / GP->ndc2dev.bx);
		height = yNDCtoInch(GP->fig2dev.by / GP->ndc2dev.by);
		GP->mar[0] = DP->mar[0] = REAL(value)[2] * yInchtoChar(height) / DP->mex;
		GP->mar[1] = DP->mar[1] = REAL(value)[0] * xInchtoChar(width) / DP->mex;
		GP->mar[2] = DP->mar[2] = (1.0 - REAL(value)[3]) * yInchtoChar(height) / DP->mex;
		GP->mar[3] = DP->mar[3] = (1.0 - REAL(value)[1]) * xInchtoChar(width) / DP->mex;
		GReset();
	}
	else if (streql(what, "ps")) {
		lengthCheck(what, value, 1);
		ix = asInteger(value);
		if (ix != NA_INTEGER && ix >= 0)
			DP->ps = GP->ps = ix;
		else par_error(what);
	}
	else if (streql(what, "pty")) {
		if (!isString(value) || LENGTH(value) < 1)
			par_error(what);
		ix = CHAR(STRING(value)[0])[0];
		if (ix == 'm' || ix == 's')
			DP->pty = GP->pty = ix;
		else par_error(what);
	}
	else if (streql(what, "smo")) {
		lengthCheck(what, value, 1);
		ix = asInteger(value);
		if (ix == NA_INTEGER || ix <= 0)
			par_error(what);
		DP->smo = GP->smo = ix;
	}
	else if (streql(what, "srt")) {
		lengthCheck(what, value, 1);
		x = asReal(value);
		if (FINITE(x))
			DP->srt = GP->srt = x;
		else par_error(what);
	}
	else if (streql(what, "tck")) {
		lengthCheck(what, value, 1);
		x = asReal(value);
		if (FINITE(x))
			DP->tck = GP->tck = x;
		else par_error(what);
	}
	else if (streql(what, "tmag")) {
		lengthCheck(what, value, 1);
		x = asReal(value);
		posRealCheck(x, what);
		DP->tmag = GP->tmag = x;
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
			DP->type = GP->type = ix;
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
		GP->usr[0] = DP->usr[0] = REAL(value)[0];
		GP->usr[1] = DP->usr[1] = REAL(value)[1];
		GP->usr[2] = DP->usr[2] = REAL(value)[2];
		GP->usr[3] = DP->usr[3] = REAL(value)[3];
		/* Reset Mapping and Axis Parameters */
		GMapWin2Fig();
		GSetupAxis(1);
		GSetupAxis(2);
	}
	else if (streql(what, "xaxp")) {
		value = coerceVector(value, REALSXP);
		lengthCheck(what, value, 3);
		naRealCheck(REAL(value)[0], what);
		naRealCheck(REAL(value)[1], what);
		posIntCheck((int) (REAL(value)[2]), what);
		DP->xaxp[0] = GP->xaxp[0] = REAL(value)[0];
		DP->xaxp[1] = GP->xaxp[1] = REAL(value)[1];
		DP->xaxp[2] = GP->xaxp[2] = (int)(REAL(value)[2]);
	}
	else if (streql(what, "xaxs")) {
		if (!isString(value) || LENGTH(value) < 1)
			par_error(what);
		ix = CHAR(STRING(value)[0])[0];
		if (ix == 's' || ix == 'e' || ix == 'i' || ix == 'r' || ix == 'd')
			DP->xaxs = GP->xaxs = ix;
		else par_error(what);
	}
	else if (streql(what, "xaxt")) {
		if (!isString(value) || LENGTH(value) < 1)
			par_error(what);
		ix = CHAR(STRING(value)[0])[0];
		if (ix == 's' || ix == 'l' || ix == 't' || ix == 'n')
			DP->xaxt = GP->xaxt = ix;
		else par_error(what);
	}
	else if (streql(what, "xlog")) {
		lengthCheck(what, value, 1);
		ix = asLogical(value);
		if (ix == NA_LOGICAL)
			par_error(what);
		DP->xlog = GP->xlog = (ix != 0);
	}
	else if (streql(what, "xpd")) {
		lengthCheck(what, value, 1);
		ix = asInteger(value);
		if (ix == NA_INTEGER)
			par_error(what);
		DP->xpd = GP->xpd = (ix != 0);
	}
	else if (streql(what, "yaxp")) {
		value = coerceVector(value, REALSXP);
		lengthCheck(what, value, 3);
		naRealCheck(REAL(value)[0], what);
		naRealCheck(REAL(value)[1], what);
		posIntCheck((int) (REAL(value)[2]), what);
		DP->yaxp[0] = GP->yaxp[0] = REAL(value)[0];
		DP->yaxp[1] = GP->yaxp[1] = REAL(value)[1];
		DP->yaxp[2] = GP->yaxp[2] = (int) (REAL(value)[2]);
	}
	else if (streql(what, "yaxs")) {
		if (!isString(value) || LENGTH(value) < 1)
			par_error(what);
		ix = CHAR(STRING(value)[0])[0];
		if (ix == 's' || ix == 'e' || ix == 'i' || ix == 'r' || ix == 'd')
			DP->yaxs = GP->yaxs = ix;
		else par_error(what);
	}
	else if (streql(what, "yaxt")) {
		if (!isString(value) || LENGTH(value) < 1)
			par_error(what);
		ix = CHAR(STRING(value)[0])[0];
		if (ix == 's' || ix == 'l' || ix == 'n')
			DP->yaxt = GP->yaxt = ix;
		else par_error(what);
	}
	else if (streql(what, "ylog")) {
		lengthCheck(what, value, 1);
		ix = asLogical(value);
		if (ix == NA_LOGICAL)
			par_error(what);
		DP->ylog = GP->ylog = (ix != 0);
	}
	/* else errorcall(gcall, "parameter \"%s\" is not setable\n", what); */
}

static SEXP Query(char *what, GPar *gp)
{
	SEXP value;

	if (streql(what, "adj")) {
		value = allocVector(REALSXP, 1);
		REAL(value)[0] = gp->adj;
	}
	else if (streql(what, "ann")) {
		value = allocVector(INTSXP, 1);
		INTEGER(value)[0] = gp->ann;
	}
	else if (streql(what, "ask")) {
		value = allocVector(LGLSXP, 1);
		INTEGER(value)[0] = gp->ask;
	}
	else if (streql(what, "bg")) {
		value = allocVector(STRSXP, 1);
		STRING(value)[0] = mkChar(col2name(gp->bg));
	}
	else if (streql(what, "bty")) {
		char buf[2];
		PROTECT(value = allocVector(STRSXP, 1));
		buf[0] = gp->bty;
		buf[1] = '\0';
		STRING(value)[0] = mkChar(buf);
		UNPROTECT(1);
	}
	else if (streql(what, "cex")) {
		value = allocVector(REALSXP, 1);
		REAL(value)[0] = gp->cexbase;
	}
	else if (streql(what, "cex.main")) {
		value = allocVector(REALSXP, 1);
		REAL(value)[0] = gp->cexmain;
	}
	else if (streql(what, "cex.lab")) {
		value = allocVector(REALSXP, 1);
		REAL(value)[0] = gp->cexlab;
	}
	else if (streql(what, "cex.sub")) {
		value = allocVector(REALSXP, 1);
		REAL(value)[0] = gp->cexsub;
	}
	else if (streql(what, "cex.axis")) {
		value = allocVector(REALSXP, 1);
		REAL(value)[0] = gp->cexaxis;
	}
	else if (streql(what, "cin")) {
		value = allocVector(REALSXP, 2);
		REAL(value)[0] = gp->cra[0]*DP->ipr[0];
		REAL(value)[1] = gp->cra[1]*DP->ipr[1];
	}
	else if (streql(what, "col")) {
		value = allocVector(STRSXP, 1);
		STRING(value)[0] = mkChar(col2name(gp->col));
	}
	else if (streql(what, "col.main")) {
		value = allocVector(STRSXP, 1);
		STRING(value)[0] = mkChar(col2name(gp->colmain));
	}
	else if (streql(what, "col.lab")) {
		value = allocVector(STRSXP, 1);
		STRING(value)[0] = mkChar(col2name(gp->collab));
	}
	else if (streql(what, "col.sub")) {
		value = allocVector(STRSXP, 1);
		STRING(value)[0] = mkChar(col2name(gp->colsub));
	}
	else if (streql(what, "col.axis")) {
		value = allocVector(STRSXP, 1);
		STRING(value)[0] = mkChar(col2name(gp->colaxis));
	}
	else if (streql(what, "cra")) {
		value = allocVector(REALSXP, 2);
		REAL(value)[0] = gp->cra[0];
		REAL(value)[1] = gp->cra[1];
	}
	else if (streql(what, "crt")) {
		value = allocVector(REALSXP, 1);
		REAL(value)[0] = gp->crt;
	}
	else if (streql(what, "csi")) {
		value = allocVector(REALSXP, 1);
		REAL(value)[0] = yChartoInch(1.0);
	}
	else if (streql(what, "err")) {
		value = allocVector(INTSXP, 1);
		INTEGER(value)[0] = gp->err;
	}
	else if (streql(what, "fg")) {
		value = allocVector(STRSXP, 1);
		STRING(value)[0] = mkChar(col2name(gp->fg));
	}
	else if (streql(what, "fig")) {
		value = allocVector(REALSXP, 4);
		REAL(value)[0] = xChartoNDC(gp->oma[1] * DP->mex);
		REAL(value)[1] = 1.0 - xChartoNDC(gp->oma[3] * DP->mex);
		REAL(value)[2] = yChartoNDC(gp->oma[0] * DP->mex);
		REAL(value)[3] = 1.0 - yChartoNDC(gp->oma[2] * DP->mex);
	}
	else if (streql(what, "fin")) {
		value = allocVector(REALSXP, 2);
		REAL(value)[0] = xNDCtoInch(gp->fig2dev.bx / DP->ndc2dev.bx);
		REAL(value)[1] = yNDCtoInch(gp->fig2dev.by / DP->ndc2dev.by);
	}
	else if (streql(what, "font")) {
		value = allocVector(INTSXP, 1);
		INTEGER(value)[0] = gp->font;
	}
	else if (streql(what, "font.main")) {
		value = allocVector(INTSXP, 1);
		INTEGER(value)[0] = gp->fontmain;
	}
	else if (streql(what, "font.lab")) {
		value = allocVector(INTSXP, 1);
		INTEGER(value)[0] = gp->fontlab;
	}
	else if (streql(what, "font.sub")) {
		value = allocVector(INTSXP, 1);
		INTEGER(value)[0] = gp->fontsub;
	}
	else if (streql(what, "font.axis")) {
		value = allocVector(INTSXP, 1);
		INTEGER(value)[0] = gp->fontaxis;
	}
	else if (streql(what, "gamma")) {
		value = allocVector(REALSXP, 1);
		REAL(value)[0] = gp->gamma;
	}
	else if (streql(what, "lab")) {
		value = allocVector(INTSXP, 3);
		INTEGER(value)[0] = gp->lab[0];
		INTEGER(value)[1] = gp->lab[1];
		INTEGER(value)[2] = gp->lab[2];
	}
	else if (streql(what, "las")) {
		value = allocVector(INTSXP, 1);
		INTEGER(value)[0] = gp->las;
	}
	else if (streql(what, "lty")) {
		value = LTYget(gp->lty);
	}
	else if (streql(what, "lwd")) {
		value =  allocVector(REALSXP, 1);
		REAL(value)[0] = gp->lwd;
	}
	else if (streql(what, "mai")) {
		value = allocVector(REALSXP, 4);
		REAL(value)[0] = yChartoInch(gp->mar[0] * DP->cexbase * DP->mex);
		REAL(value)[1] = xChartoInch(gp->mar[1] * DP->cexbase * DP->mex);
		REAL(value)[2] = yChartoInch(gp->mar[2] * DP->cexbase * DP->mex);
		REAL(value)[3] = xChartoInch(gp->mar[3] * DP->cexbase * DP->mex);
	}
	else if (streql(what, "mar")) {
		value = allocVector(REALSXP, 4);
		REAL(value)[0] = gp->mar[0];
		REAL(value)[1] = gp->mar[1];
		REAL(value)[2] = gp->mar[2];
		REAL(value)[3] = gp->mar[3];
	}
	else if (streql(what, "mex")) {
		value = allocVector(REALSXP, 1);
		REAL(value)[0] = gp->mex;
	}
	else if (streql(what, "mfrow") || streql(what, "mfcol")) {
		value = allocVector(INTSXP, 2);
		INTEGER(value)[0] = gp->mfg[2];
		INTEGER(value)[1] = gp->mfg[3];
	}
	else if (streql(what, "mfg")) {
		value = allocVector(INTSXP, 4);
		INTEGER(value)[0] = gp->mfg[0];
		INTEGER(value)[1] = gp->mfg[1];
		INTEGER(value)[2] = gp->mfg[2];
		INTEGER(value)[3] = gp->mfg[3];
	}
	else if (streql(what, "mgp")) {
		value = allocVector(REALSXP, 3);
		REAL(value)[0] = gp->mgp[0];
		REAL(value)[1] = gp->mgp[1];
		REAL(value)[2] = gp->mgp[2];
	}
	else if (streql(what, "mkh")) {
		value = allocVector(REALSXP, 1);
		REAL(value)[0] = gp->mkh;
	}
	else if (streql(what, "new")) {
		value = allocVector(LGLSXP, 1);
		INTEGER(value)[0] = gp->new;
	}
	else if (streql(what, "oma")) {
		value = allocVector(REALSXP, 4);
		REAL(value)[0] = gp->oma[0];
		REAL(value)[1] = gp->oma[1];
		REAL(value)[2] = gp->oma[2];
		REAL(value)[3] = gp->oma[3];
	}
	else if (streql(what, "omd")) {
		value = allocVector(REALSXP, 4);
		REAL(value)[0] = xChartoNDC(gp->oma[1] * DP->mex);
		REAL(value)[1] = 1.0 - xChartoNDC(gp->oma[3] * DP->mex);
		REAL(value)[2] = yChartoNDC(gp->oma[0] * DP->mex);
		REAL(value)[3] = 1.0 - yChartoNDC(gp->oma[2] * DP->mex);
	}
	else if (streql(what, "omi")) {
		value = allocVector(REALSXP, 4);
		REAL(value)[0] = xChartoInch(gp->oma[0] * DP->mex);
		REAL(value)[1] = yChartoInch(gp->oma[1] * DP->mex);
		REAL(value)[2] = xChartoInch(gp->oma[2] * DP->mex);
		REAL(value)[3] = yChartoInch(gp->oma[3] * DP->mex);
	}
	else if (streql(what, "pch")) {
		char buf[2];
		if(gp->pch < ' ' || DP->pch > 255) {
			PROTECT(value = allocVector(INTSXP, 1));
			INTEGER(value)[0] = gp->pch;
		}
		else {
			PROTECT(value = allocVector(STRSXP, 1));
			buf[0] = gp->pch;
			buf[1] = '\0';
			STRING(value)[0] = mkChar(buf);
		}
		UNPROTECT(1);
	}
	else if (streql(what, "pin")) {
		value = allocVector(REALSXP, 2);
		REAL(value)[0] = xNDCtoInch(gp->fig2dev.bx / DP->ndc2dev.bx) * (DP->plt[1] - DP->plt[0]);
		REAL(value)[1] = yNDCtoInch(gp->fig2dev.by / DP->ndc2dev.by) * (DP->plt[3] - DP->plt[2]);
	}
	else if (streql(what, "plt")) {
		double width, height;
		value = allocVector(REALSXP, 4);
		/* Size of the figure region in NDC */
		width = gp->fig2dev.bx / DP->ndc2dev.bx;
		height = gp->fig2dev.by / DP->ndc2dev.by;
		REAL(value)[0] = xChartoNDC(gp->mar[1] * DP->mex) / width;
		REAL(value)[1] = 1.0 - xChartoNDC(gp->mar[3] * DP->mex) / width;
		REAL(value)[2] = yChartoNDC(gp->mar[0] * DP->mex) / height;
		REAL(value)[3] = 1.0 - yChartoNDC(gp->mar[2] * DP->mex) / height;
	}
	else if (streql(what, "ps")) {
		value = allocVector(INTSXP, 1);
		INTEGER(value)[0] = gp->ps;
	}
	else if (streql(what, "pty")) {
		char buf[2];
		PROTECT(value = allocVector(STRSXP, 1));
		buf[0] = gp->pty;
		buf[1] = '\0';
		STRING(value)[0] = mkChar(buf);
		UNPROTECT(1);
	}
	else if (streql(what, "smo")) {
		value = allocVector(INTSXP, 1);
		INTEGER(value)[0] = gp->smo;
	}
	else if (streql(what, "srt")) {
		value = allocVector(REALSXP, 1);
		REAL(value)[0] = gp->srt;
	}
	else if (streql(what, "tck")) {
		value = allocVector(REALSXP, 1);
		REAL(value)[0] = gp->tck;
	}
	else if (streql(what, "tmag")) {
		value = allocVector(REALSXP, 1);
		REAL(value)[0] = gp->tmag;
	}
	else if (streql(what, "type")) {
		char buf[2];
		PROTECT(value = allocVector(STRSXP, 1));
		buf[0] = gp->type;
		buf[1] = '\0';
		STRING(value)[0] = mkChar(buf);
		UNPROTECT(1);
	}
	else if (streql(what, "usr")) {
		value = allocVector(REALSXP, 4);
		REAL(value)[0] = gp->usr[0];
		REAL(value)[1] = gp->usr[1];
		REAL(value)[2] = gp->usr[2];
		REAL(value)[3] = gp->usr[3];
	}
	else if (streql(what, "xaxp")) {
		value = allocVector(REALSXP, 3);
		REAL(value)[0] = gp->xaxp[0];
		REAL(value)[1] = gp->xaxp[1];
		REAL(value)[2] = gp->xaxp[2];
	}
	else if (streql(what, "xaxs")) {
		char buf[2];
		PROTECT(value = allocVector(STRSXP, 1));
		buf[0] = gp->xaxs;
		buf[1] = '\0';
		STRING(value)[0] = mkChar(buf);
		UNPROTECT(1);
	}
	else if (streql(what, "xaxt")) {
		char buf[2];
		PROTECT(value = allocVector(STRSXP, 1));
		buf[0] = gp->xaxt;
		buf[1] = '\0';
		STRING(value)[0] = mkChar(buf);
		UNPROTECT(1);
	}
	else if (streql(what, "xlog")) {
		value = allocVector(LGLSXP, 1);
		INTEGER(value)[0] = gp->xlog;
	}
	else if (streql(what, "xpd")) {
		value = allocVector(LGLSXP, 1);
		INTEGER(value)[0] = gp->xpd;
	}
	else if (streql(what, "yaxp")) {
		value = allocVector(REALSXP, 3);
		REAL(value)[0] = gp->yaxp[0];
		REAL(value)[1] = gp->yaxp[1];
		REAL(value)[2] = gp->yaxp[2];
	}
	else if (streql(what, "yaxs")) {
		char buf[2];
		PROTECT(value = allocVector(STRSXP, 1));
		buf[0] = gp->yaxs;
		buf[1] = '\0';
		STRING(value)[0] = mkChar(buf);
		UNPROTECT(1);
	}
	else if (streql(what, "yaxt")) {
		char buf[2];
		PROTECT(value = allocVector(STRSXP, 1));
		buf[0] = gp->xaxt;
		buf[1] = '\0';
		STRING(value)[0] = mkChar(buf);
		UNPROTECT(1);
	}
	else if (streql(what, "ylog")) {
		value = allocVector(LGLSXP, 1);
		INTEGER(value)[0] = gp->ylog;
	}
	else
		value = R_NilValue;
	return value;
}

SEXP do_par(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP ap, vp, value;

	if (!DevInit)
		errorcall(call, "No device is active\n");

	gcall = call;
	checkArity(op, args);

	if (!isList(CAR(args)))
		errorcall(call, "invalid parameter passed to \"par\"\n");

	args = CAR(args);
	PROTECT(value = allocList(length(args)));
	for (vp = value, ap = args; ap != R_NilValue; vp = CDR(vp), ap = CDR(ap)) {
		if (TAG(ap) != R_NilValue) {
			CAR(vp) = Query(CHAR(PRINTNAME(TAG(ap))), DP);
			TAG(vp) = TAG(ap);
			Specify(CHAR(PRINTNAME(TAG(ap))), CAR(ap));
		}
		else if (TYPEOF(CAR(ap)) == STRSXP) {
			CAR(vp) = Query(CHAR(STRING(CAR(ap))[0]), DP);
			TAG(vp) = install(CHAR(STRING(CAR(ap))[0]));
		}
	}
	UNPROTECT(1);
	return value;
}

void Specify2(char *what, SEXP value)
{
	double x;
	int ix;

	if (streql(what, "adj")) {
		lengthCheck(what, value, 1);
		x = asReal(value);
		if (0.0 <= x && x <= 1.0)
			GP->adj = x;
		else par_error(what);
	}
	else if (streql(what, "ann")) {
		lengthCheck(what, value, 1);
		ix = asInteger(value);
		GP->ann = (ix != 0);
	}
	else if (streql(what, "bg")) {
		lengthCheck(what, value, 1);
		ix = RGBpar(value, 0);
		if (ix != NA_INTEGER)
			GP->bg = ix;
		else par_error(what);
	}
	else if (streql(what, "bty")) {
		if (!isString(value) || LENGTH(value) < 1)
			par_error(what);
		ix = CHAR(STRING(value)[0])[0];
		if (ix == 'o' || ix == 'l' || ix == '7' || ix == 'c' || ix == 'n') {
			GP->bty = ix;
		}
	}
	else if (streql(what, "cex")) {
		lengthCheck(what, value, 1);
		x = asReal(value);
		if (FINITE(x) && 0.0 < x) {
			GP->cex = 1.0;
			GP->cexbase = x;
		}
		else par_error(what);
	}
	else if (streql(what, "cex.main")) {
		lengthCheck(what, value, 1);
		x = asReal(value);
		if (FINITE(x) && 0.0 < x)
			GP->cexmain = x;
		else par_error(what);
	}
	else if (streql(what, "cex.lab")) {
		lengthCheck(what, value, 1);
		x = asReal(value);
		if (FINITE(x) && 0.0 < x)
			GP->cexlab = x;
		else par_error(what);
	}
	else if (streql(what, "cex.sub")) {
		lengthCheck(what, value, 1);
		x = asReal(value);
		if (FINITE(x) && 0.0 < x)
			GP->cexsub = x;
		else par_error(what);
	}
	else if (streql(what, "cex.axis")) {
		lengthCheck(what, value, 1);
		x = asReal(value);
		if (x != NA_INTEGER && 0.0 < x)
			GP->cexaxis = x;
		else par_error(what);
	}
	else if (streql(what, "col")) {
		lengthCheck(what, value, 1);
		ix = RGBpar(value, 0);
		if (ix != NA_INTEGER)
			GP->col = ix;
		else par_error(what);
	}
	else if (streql(what, "col.main")) {
		lengthCheck(what, value, 1);
		ix = RGBpar(value, 0);
		if (ix != NA_INTEGER)
			GP->colmain = ix;
		else par_error(what);
	}
	else if (streql(what, "col.lab")) {
		lengthCheck(what, value, 1);
		ix = RGBpar(value, 0);
		if (ix != NA_INTEGER)
			GP->collab = ix;
		else par_error(what);
	}
	else if (streql(what, "col.sub")) {
		lengthCheck(what, value, 1);
		ix = RGBpar(value, 0);
		if (ix != NA_INTEGER)
			GP->colsub = ix;
		else par_error(what);
	}
	else if (streql(what, "col.axis")) {
		lengthCheck(what, value, 1);
		ix = RGBpar(value, 0);
		if (ix != NA_INTEGER)
			GP->colaxis = ix;
		else par_error(what);
	}
	else if (streql(what, "crt")) {
		lengthCheck(what, value, 1);
		x = asReal(value);
		if (FINITE(x))
			GP->crt = x;
		else par_error(what);
	}
	else if (streql(what, "err")) {
		lengthCheck(what, value, 1);
		ix = asInteger(value);
		if (ix == 0 || ix == -1)
			GP->err = ix;
		else par_error(what);
	}
	else if (streql(what, "fg")) {
		lengthCheck(what, value, 1);
		ix = RGBpar(value, 0);
		if (ix != NA_INTEGER)
			GP->fg = ix;
		else par_error(what);
	}
	else if (streql(what, "font")) {
		lengthCheck(what, value, 1);
		ix = asInteger(value);
		if (ix != NA_INTEGER && ix > 0)
			GP->font = ix;
		else par_error(what);
	}
	else if (streql(what, "font.main")) {
		lengthCheck(what, value, 1);
		ix = asInteger(value);
		if (ix != NA_INTEGER && ix > 0)
			GP->fontmain = ix;
		else par_error(what);
	}
	else if (streql(what, "font.lab")) {
		lengthCheck(what, value, 1);
		ix = asInteger(value);
		if (ix != NA_INTEGER && ix > 0)
			GP->fontlab = ix;
		else par_error(what);
	}
	else if (streql(what, "font.sub")) {
		lengthCheck(what, value, 1);
		ix = asInteger(value);
		if (ix != NA_INTEGER && ix > 0)
			GP->fontsub = ix;
		else par_error(what);
	}
	else if (streql(what, "font.axis")) {
		lengthCheck(what, value, 1);
		ix = asInteger(value);
		if (ix != NA_INTEGER && ix > 0)
			GP->fontaxis = ix;
		else par_error(what);
	}
	else if (streql(what, "lab")) {
		value = coerceVector(value, INTSXP);
		lengthCheck(what, value, 3);
		nonnegIntCheck(INTEGER(value)[0], what);
		nonnegIntCheck(INTEGER(value)[0], what);
		nonnegIntCheck(INTEGER(value)[0], what);
		GP->lab[0] = INTEGER(value)[0];
		GP->lab[1] = INTEGER(value)[1];
		GP->lab[2] = INTEGER(value)[1];
	}
	else if (streql(what, "las")) {
		lengthCheck(what, value, 1);
		ix = asInteger(value);
		if (0 <= ix && ix <= 2)
			GP->las = ix;
		else par_error(what);
	}
	else if (streql(what, "lty")) {
		lengthCheck(what, value, 1);
		GP->lty = LTYpar(value, 0);
	}
	else if (streql(what, "lwd")) {
		lengthCheck(what, value, 1);
		x = asReal(value);
		if (FINITE(x) && 0.0 < x)
			GP->lwd = x;
		else par_error(what);
	}
	else if (streql(what, "mgp")) {
		value = coerceVector(value, REALSXP);
		lengthCheck(what, value, 3);
		nonnegRealCheck(REAL(value)[0], what);
		nonnegRealCheck(REAL(value)[1], what);
		nonnegRealCheck(REAL(value)[2], what);
		GP->mgp[0] = REAL(value)[0];
		GP->mgp[1] = REAL(value)[1];
		GP->mgp[2] = REAL(value)[2];
	}
	else if (streql(what, "mkh")) {
		lengthCheck(what, value, 1);
		x = asReal(value);
		posRealCheck(x, what);
		GP->mkh = x;
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
		GP->pch = ix;
	}
	else if (streql(what, "smo")) {
		lengthCheck(what, value, 1);
		ix = asInteger(value);
		if (ix == NA_INTEGER || ix <= 0)
			par_error(what);
		GP->smo = ix;
	}
	else if (streql(what, "srt")) {
		lengthCheck(what, value, 1);
		x = asReal(value);
		if (FINITE(x))
			GP->srt = x;
		else par_error(what);
	}
	else if (streql(what, "tck")) {
		lengthCheck(what, value, 1);
		x = asReal(value);
		if (FINITE(x))
			GP->tck = x;
		else par_error(what);
	}
	else if (streql(what, "tmag")) {
		lengthCheck(what, value, 1);
		x = asReal(value);
		posRealCheck(x, what);
		GP->tmag = x;
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
			GP->type = ix;
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
		GP->xaxp[0] = REAL(value)[0];
		GP->xaxp[1] = REAL(value)[1];
		GP->xaxp[2] = (int)(REAL(value)[2]);
	}
	else if (streql(what, "xaxs")) {
		if (!isString(value) || LENGTH(value) < 1)
			par_error(what);
		ix = CHAR(STRING(value)[0])[0];
		if (ix == 's' || ix == 'e' || ix == 'i' || ix == 'r' || ix == 'd')
			GP->xaxs = ix;
		else par_error(what);
	}
	else if (streql(what, "xaxt")) {
		if (!isString(value) || LENGTH(value) < 1)
			par_error(what);
		ix = CHAR(STRING(value)[0])[0];
		if (ix == 's' || ix == 'l' || ix == 't' || ix == 'n')
			GP->xaxt = ix;
		else par_error(what);
	}
	else if (streql(what, "xpd")) {
		lengthCheck(what, value, 1);
		ix = asInteger(value);
		if (ix == NA_INTEGER)
			par_error(what);
		GP->xpd = (ix != 0);
	}
	else if (streql(what, "yaxp")) {
		value = coerceVector(value, REALSXP);
		lengthCheck(what, value, 3);
		naRealCheck(REAL(value)[0], what);
		naRealCheck(REAL(value)[1], what);
		posIntCheck((int) (REAL(value)[2]), what);
		GP->yaxp[0] = REAL(value)[0];
		GP->yaxp[1] = REAL(value)[1];
		GP->yaxp[2] = (int) (REAL(value)[2]);
	}
	else if (streql(what, "yaxs")) {
		if (!isString(value) || LENGTH(value) < 1)
			par_error(what);
		ix = CHAR(STRING(value)[0])[0];
		if (ix == 's' || ix == 'e' || ix == 'i' || ix == 'r' || ix == 'd')
			GP->yaxs = ix;
		else par_error(what);
	}
	else if (streql(what, "yaxt")) {
		if (!isString(value) || LENGTH(value) < 1)
			par_error(what);
		ix = CHAR(STRING(value)[0])[0];
		if (ix == 's' || ix == 'l' || ix == 't' || ix == 'n')
			GP->yaxt = ix;
		else par_error(what);
	}
	/* else errorcall(gcall, "parameter \"%s\" is not setable\n", what); */
}

SEXP do_par2(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP ap, vp, value;

	if(!DevInit) errorcall(call, "No device is active\n");

	gcall = call;
	checkArity(op, args);

	if (!isList(CAR(args)))
		errorcall(call, "invalid parameter passed to \"par\"\n");

	args = CAR(args);
	PROTECT(value = allocList(length(args)));
	for (vp = value, ap = args; ap != R_NilValue; vp = CDR(vp), ap = CDR(ap)) {
		if (TAG(ap) != R_NilValue) {
			CAR(vp) = Query(CHAR(PRINTNAME(TAG(ap))), GP);
			TAG(vp) = TAG(ap);
			Specify2(CHAR(PRINTNAME(TAG(ap))), CAR(ap));
		}
		else if (TYPEOF(CAR(ap)) == STRSXP) {
			CAR(vp) = Query(CHAR(STRING(CAR(ap))[0]), GP);
			TAG(vp) = install(CHAR(STRING(CAR(ap))[0]));
		}
	}
	UNPROTECT(1);
	return value;
}
