/*
 *  R : A Computer Language for Statistical Data Analysis
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

#include "Defn.h"
#include "Mathlib.h"
#include "Graphics.h"

void NewFrameConfirm()
{
	char buf[16];
	R_ReadConsole("Hit <Return> to see next plot: ", buf, 16, 0);
}

	/* Device Startup */

SEXP do_device(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP s, name, cpars, npars;
	int i, ncpars, nnpars;

	checkArity(op, args);

	name = CAR(args);
	if (!isString(name) || length(name) <= 0)
		errorcall(call, "device name must be a character string\n");

	cpars = CADR(args);
	if (!isString(cpars) || length(cpars) > 20)
		errorcall(call, "invalid device driver parameters\n");
	ncpars = LENGTH(cpars);

	npars = CADDR(args);
	if (!isReal(CADDR(args)))
		errorcall(call, "width and height must be numeric\n");
	nnpars = LENGTH(npars);

	if( !strcmp(CHAR(STRING(name)[0]),"X11") )
		for(i=0 ; i<nnpars ; i++ ) 
			if(!FINITE(REAL(npars)[i]) || REAL(npars)[i] <= 0)
				errorcall(call, "invalid device driver parameter\n");

	if (!SetDevice(name, cpars, ncpars, npars, nnpars))
		errorcall(call, "unable to start device %s\n", 
			  CHAR(STRING(name)[0]));

	return CAR(args);
}

	/* +1 and/or -1 because C arrays are zero-based and R-vectors */
	/* are one-based */

SEXP do_devcontrol(SEXP call, SEXP op, SEXP args, SEXP env)
{
	checkArity(op, args);
	inhibitDisplayList(CurrentDevice());
	return R_NilValue;
}

SEXP do_devcopy(SEXP call, SEXP op, SEXP args, SEXP env)
{
	int devNum = INTEGER(CAR(args))[0] - 1;
	checkArity(op, args);
	copyDisplayList(devNum);
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
	int fd = INTEGER(CAR(args))[0] - 1;
	SEXP nd = allocVector(INTSXP, 1);
	checkArity(op, args);
	INTEGER(nd)[0] = nextDevice(fd) + 1;
	return nd;
}

SEXP do_devprev(SEXP call, SEXP op, SEXP args, SEXP env)
{
	int fd = INTEGER(CAR(args))[0] - 1;
	SEXP pd = allocVector(INTSXP, 1);
	checkArity(op, args);
	INTEGER(pd)[0] = prevDevice(fd) + 1;
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
	checkArity(op, args);
	killDevice(INTEGER(CAR(args))[0] - 1);
	return R_NilValue;
}

	/* ProcessInLinePars handles inline par specifications */
	/* in graphics functions.  It does this by calling */
	/* Specify2 which is in par.c */

int Specify2(char*, SEXP, DevDesc*);

void ProcessInlinePars(SEXP s, DevDesc *dd)
{
	if(isList(s)) {
		while(s != R_NilValue) {
			if(isList(CAR(s)))
				ProcessInlinePars(CAR(s), dd);
			else if(TAG(s) != R_NilValue)
				Specify2(CHAR(PRINTNAME(TAG(s))), CAR(s), dd);
			s = CDR(s);
		}
	}
}

	/* GetPar is intended for looking through a list */
	/* typically that bound to ... for a particular */
	/* parameter value.  This is easier than trying */
	/* to match every graphics parameter in argument */
	/* lists and passing them explicitly. */

SEXP GetPar(char *which, SEXP parlist)
{
	SEXP w, p;
	w = install(which);
	for(p=parlist ; p!=R_NilValue ; p=CDR(p)) {
		if(TAG(p) == w)
			return CAR(p);
	}
	return R_NilValue;
}

SEXP FixupPch(SEXP pch, DevDesc *dd)
{
	int i, n;
	SEXP ans;

	if(length(pch) == 0) {
		ans = allocVector(INTSXP, n=1);
		INTEGER(ans)[0] = dd->gp.pch;
	}
	else if(isList(pch)) {
		ans = allocVector(INTSXP, n=length(pch));
		for(i=0 ; pch != R_NilValue ;  pch = CDR(pch))
			INTEGER(ans)[i++] = asInteger(CAR(pch));
	}
	else if(isInteger(pch)) {
		ans = allocVector(INTSXP, n=length(pch));
		for(i=0 ; i<n ; i++)
			INTEGER(ans)[i] = INTEGER(pch)[i];
	}
	else if(isReal(pch)) {
		ans = allocVector(INTSXP, n=length(pch));
		for(i=0 ; i<n ; i++)
			INTEGER(ans)[i] = FINITE(REAL(pch)[i]) ?
						REAL(pch)[i] : NA_INTEGER;
	}
	else if(isString(pch)) {
		ans = allocVector(INTSXP, n=length(pch));
		for(i=0 ; i<n ; i++)
			INTEGER(ans)[i] = CHAR(STRING(pch)[i])[0];
	}
	else error("invalid plotting symbol\n");
	for(i=0 ; i<n ; i++) {
		if(INTEGER(ans)[i] < 0)
			INTEGER(ans)[i] = dd->gp.pch;
	}
	return ans;
}

SEXP FixupLty(SEXP lty, DevDesc *dd)
{
	int i, n;
	SEXP ans;
	if(length(lty) == 0) {
		ans = allocVector(INTSXP, 1);
		INTEGER(ans)[0] = dd->gp.lty;
	}
	else {
		ans = allocVector(INTSXP, n=length(lty));
		for(i=0 ; i<n; i++)
			INTEGER(ans)[i] = LTYpar(lty, i);
	}
	return ans;
}

SEXP FixupFont(SEXP font)
{
	int i, k, n;
	SEXP ans;
	if(length(font) == 0) {
		ans = allocVector(INTSXP, 1);
		INTEGER(ans)[0] = NA_INTEGER;
	}
	else if(isInteger(font)) {
		ans = allocVector(INTSXP, n=length(font));
		for(i=0 ; i<n; i++) {
			k = INTEGER(font)[i];
			if(k < 1 || k > 4) k = NA_INTEGER;
			INTEGER(ans)[i] = k;
		}
	}
	else if(isReal(font)) {
		ans = allocVector(INTSXP, n=length(font));
		for(i=0 ; i<n; i++) {
			k = REAL(font)[i];
			if(k < 1 || k > 4) k = NA_INTEGER;
			INTEGER(ans)[i] = k;
		}
	}
	else error("invalid font specification\n");
	return ans;
}

SEXP FixupCol(SEXP col, DevDesc *dd)
{
	int i, n;
	SEXP ans;

	if(length(col) == 0) {
		ans = allocVector(INTSXP, 1);
		INTEGER(ans)[0] = NA_INTEGER;
	}
	else if(isList(col)) {
		ans = allocVector(INTSXP, n=length(col));
		for(i=0 ; i<n; i++) {
			INTEGER(ans)[i] = RGBpar(CAR(col), 0, dd);
			col = CDR(col);
		}
	}
	else {
		ans = allocVector(INTSXP, n=length(col));
		for(i=0 ; i<n; i++)
			INTEGER(ans)[i] = RGBpar(col, i, dd);
	}
	return ans;
}

SEXP FixupCex(SEXP cex)
{
	SEXP ans;
	int i, n;
	double c;

	if(length(cex) == 0) {
		ans = allocVector(REALSXP, 1);
		REAL(ans)[0] = NA_REAL;
	}
	else if(isReal(cex)) {
		ans = allocVector(REALSXP, n=length(cex));
		for(i=0 ; i<n; i++) {
			c = REAL(cex)[i];
			if(FINITE(c) && c > 0)
				REAL(ans)[i] = c;
			else
				REAL(ans)[i] = NA_REAL;
		}
	}
	else if(isInteger(cex)) {
		ans = allocVector(REALSXP, n=length(cex));
		for(i=0 ; i<n; i++) {
			c = INTEGER(cex)[i];
			if(c == NA_INTEGER || c <= 0)
				c = NA_REAL;
			REAL(ans)[i] = c;
		}
	}
	return ans;
}

	/*  plot.new(ask)  --  create a new plot  */

SEXP do_plot_new(SEXP call, SEXP op, SEXP args, SEXP env)
{
	int ask, asksave;
	DevDesc *dd = CurrentDevice();

	checkArity(op, args);

	ask = asLogical(CAR(args));
	if (ask == NA_LOGICAL) 
		ask = dd->dp.ask;
	asksave = dd->gp.ask;
	dd->gp.ask = ask;

	GNewPlot(dd, call != R_NilValue);

	dd->dp.xlog = dd->gp.xlog = 0;
	dd->dp.ylog = dd->gp.ylog = 0;

	GScale(0.0, 1.0, 1, dd);
	GScale(0.0, 1.0, 2, dd);
	GMapWin2Fig(dd);
	GSetState(1, dd); 

	dd->gp.ask = asksave;
		/* NOTE that if we're replaying then call == R_NilValue */
	if (call != R_NilValue) 
		recordGraphicOperation(op, args, dd);
	return R_NilValue;
}


	/*  plot.window(xlim, ylim, log, ...) --  define world coordinates  */

SEXP do_plot_window(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP xlim, ylim, log;
	double xmin, xmax, ymin, ymax;
	char *p;
	SEXP originalArgs = args;
	DevDesc *dd = CurrentDevice();

	/* checkArity(op, args); */
	if(length(args) < 3)
		errorcall(call, "at least 3 arguments required\n");

	xlim = CAR(args);
	if(!isNumeric(xlim) || LENGTH(xlim) != 2)
		errorcall(call, "invalid xlim\n");
	args = CDR(args);

	ylim = CAR(args);
	if(!isNumeric(ylim) || LENGTH(ylim) != 2)
		errorcall(call, "invalid ylim\n");
	args = CDR(args);

	log = CAR(args);
	if (!isString(log))
		error("invalid \"log=\" specification\n");
	p = CHAR(STRING(log)[0]);
	while (*p) {
		switch (*p) {
		case 'x':
			dd->dp.xlog = dd->gp.xlog = 1;
			break;
		case 'y':
			dd->dp.ylog = dd->gp.ylog = 1;
			break;
		default:
			error("invalid \"log=\" specification\n");
		}
		p++;
	}
	args = CDR(args);

	GSavePars(dd);
	ProcessInlinePars(args, dd);

	if(isInteger(xlim)) {
		if(INTEGER(xlim)[0] == NA_INTEGER || INTEGER(xlim)[1] == NA_INTEGER)
			errorcall(call, "NAs not allowed in xlim\n");
		xmin = INTEGER(xlim)[0];
		xmax = INTEGER(xlim)[1];
	}
	else {
		if(!FINITE(REAL(xlim)[0]) || !FINITE(REAL(xlim)[1]))
			errorcall(call, "NAs not allowed in xlim\n");
		xmin = REAL(xlim)[0];
		xmax = REAL(xlim)[1];
	}
	if(isInteger(ylim)) {
		if(INTEGER(ylim)[0] == NA_INTEGER || INTEGER(ylim)[1] == NA_INTEGER)
			errorcall(call, "NAs not allowed in ylim\n");
		ymin = INTEGER(ylim)[0];
		ymax = INTEGER(ylim)[1];
	}
	else {
		if(!FINITE(REAL(ylim)[0]) || !FINITE(REAL(ylim)[1]))
			errorcall(call, "NAs not allowed in ylim\n");
		ymin = REAL(ylim)[0];
		ymax = REAL(ylim)[1];
	}
	GScale(xmin, xmax, 1, dd);
	GScale(ymin, ymax, 2, dd);
	GMapWin2Fig(dd);
	GRestorePars(dd);
		/* NOTE that i only record operation if no "error"  */
		/* NOTE that if we're replaying then call == R_NilValue */
	if (call != R_NilValue)
		recordGraphicOperation(op, originalArgs, dd);
	return R_NilValue;
}

static void GetAxisLimits(double left, double right, double *low, double *high)
{
	double eps;
	if(left <= right) {
		eps = FLT_EPSILON * (right - left);
		if(eps == 0) eps = 0.5 * FLT_EPSILON;
		*low = left - eps;
		*high = right + eps;
	}
	else {
		eps = FLT_EPSILON * (left - right);
		if(eps == 0) eps = 0.5 * FLT_EPSILON;
		*low = right - eps;
		*high = left + eps;
	}
}

	/* axis(which, at, labels, ...) -- draw an axis */

SEXP do_axis(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP at, lab;
	int col, fg, i, n, which, xtckCoords, ytckCoords;
	double x, y, tempx, tempy, tnew, tlast;
	double gap, labw, low, high;
	double xx[2], yy[2];
	SEXP originalArgs = args;
	DevDesc *dd = CurrentDevice();

		/*  Initial checks  */

	GCheckState(dd);
	if(length(args) < 3)
		errorcall(call, "too few arguments\n");

		/*  Required arguments	*/

	which = asInteger(CAR(args));
	if (which < 1 || which > 4)
		errorcall(call, "invalid axis number\n");
	args = CDR(args);

	internalTypeCheck(call, at = CAR(args), REALSXP);
	args = CDR(args);

	/* internalTypeCheck(call, lab = CAR(args), STRSXP); */
	lab = CAR(args);
	if (LENGTH(at) != LENGTH(lab))
		errorcall(call, "location and label lengths differ\n");
	n = LENGTH(at);
	args = CDR(args);

	R_Visible = 0;
	GSavePars(dd);

	dd->gp.xpd = 1;
	dd->gp.adj = 0.5;
	dd->gp.font = dd->gp.fontaxis;
	dd->gp.cex = dd->gp.cex * dd->gp.cexbase;
	col = dd->gp.col;
	fg = dd->gp.fg;

		/*  Check the axis type parameter  */
		/*  If it is 'n', there is nothing to do  */

	if(which == 1 || which == 3) {
		if(dd->gp.xaxt == 'n') {
			GRestorePars(dd);
			return R_NilValue;
		}
	}
	else if(which == 2 || which == 4) {
		if(dd->gp.yaxt == 'n') {
			GRestorePars(dd);
			return R_NilValue;
		}
	}
	else errorcall(call, "invalid \"which\" value\n");

	x = dd->gp.usr[0];
	y = dd->gp.usr[2];
	xtckCoords = MAR1;
	ytckCoords = MAR2;
		
		/*  Draw the axis  */

	GMode(dd, 1);
	switch (which) {
	case 1:
	case 3:
		GetAxisLimits(dd->gp.usr[0], dd->gp.usr[1], &low, &high);
		if (which == 3) {
			y = dd->gp.usr[3];
			xtckCoords = MAR3;
		}
		dd->gp.col = fg;
		/* axis line */
		GLine(REAL(at)[0], y, REAL(at)[n - 1], y, USER, dd);
		/* ticks */
		for (i = 0; i < n; i++) {
			x = REAL(at)[i];
			if (low <= x && x <= high) {
				GLine(x, 0, x, 0.5, xtckCoords, dd);
			}
		}
		dd->gp.col = dd->gp.colaxis;
		/* labels */
		tlast = -1.0;
		gap = GStrWidth("m", NFC, dd);	/* FIXUP x/y distance */
		for (i = 0; i < n; i++) {
			x = REAL(at)[i];
			tempx = x; tempy = y;
			GConvert(&tempx, &tempy, USER, NFC, dd);
			if(isExpression(lab)) {
				GMMathText(VECTOR(lab)[i], which, 
					   dd->gp.mgp[1], 0, x, dd->gp.las, dd);
			}
			else {
				labw = GStrWidth(CHAR(STRING(lab)[i]), NFC, dd);
				tnew = tempx - 0.5 * labw;
				/* check that there's room */
				/* for labels */
				if (dd->gp.las == 2 || tnew - tlast >= gap) {
					GMtext(CHAR(STRING(lab)[i]), which, 
					       dd->gp.mgp[1], 0, x, 
					       dd->gp.las, dd);
					tlast = tempx + 0.5 *labw;
				}
			}
		}
		break;
	case 2:
	case 4:
		GetAxisLimits(dd->gp.usr[2], dd->gp.usr[3], &low, &high);
		if (which == 4) {
			x = dd->gp.usr[1];
			ytckCoords = MAR4;
		}
		dd->gp.col = fg;
		GLine(x, REAL(at)[0], x, REAL(at)[n - 1], USER, dd);
		for (i = 0; i < n; i++) {
			y = REAL(at)[i];
			if (low <= y && y <= high) {
				GLine(y, 0, y, 0.5, ytckCoords, dd);
			}
		}
		dd->gp.col = dd->gp.colaxis;
		gap = GStrWidth("m", INCHES, dd);
		gap = GConvertYUnits(gap, INCHES, NFC, dd);
		tlast = -1.0;
		for (i = 0; i < n; i++) {
			y = REAL(at)[i];
			tempx = x; tempy = y;
			GConvert(&tempx, &tempy, USER, NFC, dd);
			if(isExpression(lab)) {
				GMMathText(VECTOR(lab)[i], which, 
					   dd->gp.mgp[1], 0, y, dd->gp.las, dd);
			}
			else {
				labw = GStrWidth(CHAR(STRING(lab)[i]), 
						 INCHES, dd);
				labw = GConvertYUnits(labw, INCHES, NFC, dd);
				tnew = tempy - 0.5 * labw;
				if (dd->gp.las > 0 || tnew - tlast >= gap) {
					GMtext(CHAR(STRING(lab)[i]), which, 
					       dd->gp.mgp[1], 0, y,
					       dd->gp.las, dd);
					tlast = tempy + 0.5 *labw;
				}
			}
		}
		break;
	}

	GMode(dd, 0);
	GRestorePars(dd);
		/* NOTE that i only record operation if no "error"  */
		/* NOTE that if we're replaying then call == R_NilValue */
	if (call != R_NilValue)
		recordGraphicOperation(op, originalArgs, dd);
	return R_NilValue;
}

	/*  plot.xy(xy, type, pch, lty, col, cex, ...)	*/
	/*  plot points or lines of various types  */

SEXP do_plot_xy(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP sxy, sx, sy, pch, cex, col, bg, lty;
	double *x, *y, xold, yold, xx, yy, xc, yc;
	int i, n, npch, ncex, ncol, nbg, nlty, type, start;
	SEXP originalArgs = args;
	DevDesc *dd = CurrentDevice();

		/* Basic Checks */

	GCheckState(dd);
	if(length(args) < 6)
		errorcall(call, "too few arguments\n");

		/* Required Arguments */

	sxy = CAR(args);
	if (!isList(sxy) || length(sxy) < 2)
		errorcall(call, "invalid plotting structure\n");
	internalTypeCheck(call, sx = CAR(sxy), REALSXP);
	internalTypeCheck(call, sy = CADR(sxy), REALSXP);
	if (LENGTH(sx) != LENGTH(sy))
		error("x and y lengths differ for plot\n");
	n = LENGTH(sx);
	args = CDR(args);

	if(isNull(CAR(args))) type = 'p';
	else {
		if(isString(CAR(args)) && LENGTH(CAR(args)) == 1)
			type = CHAR(STRING(CAR(args))[0])[0];
		else errorcall(call, "invalid plot type\n");
	}
	args = CDR(args);

	PROTECT(pch = FixupPch(CAR(args), dd));
	npch = length(pch);
	args = CDR(args);

	PROTECT(lty = FixupLty(CAR(args), dd));
	nlty = length(lty);
	args = CDR(args);

	PROTECT(col = FixupCol(CAR(args), dd));
	ncol = LENGTH(col);
	args = CDR(args);

	PROTECT(bg = FixupCol(CAR(args), dd));
	nbg = LENGTH(bg);
	args = CDR(args);

	PROTECT(cex = FixupCex(CAR(args)));
	ncex = LENGTH(cex);
	args = CDR(args);

		/* Miscellaneous Graphical Parameters */

	GSavePars(dd);

	x = REAL(sx);
	y = REAL(sy);

	if(nlty && INTEGER(lty)[0] != NA_INTEGER)
		dd->gp.lty = INTEGER(lty)[0];

	if(ncex && FINITE(REAL(cex)[0]))
		dd->gp.cex = dd->gp.cexbase * REAL(cex)[0];
	else
		dd->gp.cex = dd->gp.cexbase;

	GMode(dd, 1);
	GClip(dd);

		/* lines and overplotted lines and points */

	if (type == 'l' || type == 'o') {
		dd->gp.col = INTEGER(col)[0];
		xold = NA_REAL;
		yold = NA_REAL;
		for (i = 0; i < n; i++) {
			xx = x[i];
			yy = y[i];
			/* do the conversion now to check for non-finite */
			GConvert(&xx, &yy, USER, DEVICE, dd);
			if ((FINITE(xx) && FINITE(yy)) &&
			    !(FINITE(xold) && FINITE(yold))) 
				start = i;
			else if ((FINITE(xold) && FINITE(yold)) &&
				 !(FINITE(xx) && FINITE(yy))) {
				if (i-start > 1)
					GPolyline(i-start, x+start, y+start,
						  USER, dd);
			}
			else if ((FINITE(xold) && FINITE(yold)) &&
				 (i == n-1))
				GPolyline(n-start, x+start, y+start, USER, dd);
			xold = xx;
			yold = yy;
		}
	}

		/* points connected with broken lines */

	if(type == 'b' || type == 'c') {
		double d, f;
		d = GConvertYUnits(0.5, CHARS, INCHES, dd);
		dd->gp.col = INTEGER(col)[0];
		xold = NA_REAL;
		yold = NA_REAL;
		for (i = 0; i < n; i++) {
			xx = x[i];
			yy = y[i];
			GConvert(&xx, &yy, USER, INCHES, dd);
			if (FINITE(xold) && FINITE(yold) && 
			    FINITE(xx) && FINITE(yy)) {
				if((f = d/hypot(xx-xold, yy-yold)) < 0.5) {
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

	if (type == 's') {
		double xtemp[3], ytemp[3];
		dd->gp.col = INTEGER(col)[0];
		xold = x[0];
		yold = y[0];
		GConvert(&xold, &yold, USER, DEVICE, dd);
		for (i = 1; i < n; i++) {
			xx = x[i];
			yy = y[i];
			GConvert(&xx, &yy, USER, DEVICE, dd);
			if (FINITE(xold) && FINITE(yold) && 
			    FINITE(xx) && FINITE(yy)) {
				xtemp[0] = xold; ytemp[0] = yold;
				xtemp[1] = xx;	ytemp[1] = yold;
				xtemp[2] = xx;  ytemp[2] = yy;
				GPolyline(3, xtemp, ytemp, DEVICE, dd);
			}
			xold = xx;
			yold = yy;
		}
	}

	if (type == 'S') {
		double xtemp[3], ytemp[3];
		dd->gp.col = INTEGER(col)[0];
		xold = x[0];
		yold = y[0];
		GConvert(&xold, &yold, USER, DEVICE, dd);
		for (i = 1; i < n; i++) {
			xx = x[i];
			yy = y[i];
			GConvert(&xx, &yy, USER, DEVICE, dd);
			if (FINITE(xold) && FINITE(yold) && 
			    FINITE(xx) && FINITE(yy)) {
				xtemp[0] = xold; ytemp[0] = yold;
				xtemp[1] = xold; ytemp[1] = yy;
				xtemp[2] = xx; ytemp[2] = yy;
				GPolyline(3, xtemp, ytemp, DEVICE, dd);
			}
			xold = xx;
			yold = yy;
		}
	}

	if (type == 'h') {
		dd->gp.col = INTEGER(col)[0];
		for (i = 0; i < n; i++) {
			xx = x[i];
			yy = y[i];
			xold = xx;
			yold = 0.0;
			GConvert(&xx, &yy, USER, DEVICE, dd);
			GConvert(&xold, &yold, USER, DEVICE, dd);
			if (FINITE(xx) && FINITE(yy)) {
				GLine(xold, yold, xx, yy, DEVICE, dd);
			}
		}
	}

	if (type == 'p' || type == 'b' || type == 'o') {
		for (i = 0; i < n; i++) {
			xx = x[i];
			yy = y[i];
			GConvert(&xx, &yy, USER, DEVICE, dd);
			if (FINITE(xx) && FINITE(yy)) {
				dd->gp.col = INTEGER(col)[i % ncol];
				dd->gp.bg = INTEGER(bg)[i % nbg];
				GSymbol(xx, yy, DEVICE, 
					INTEGER(pch)[i % npch], dd);
			}
		}
	}
	GMode(dd, 0);
	GRestorePars(dd);
	UNPROTECT(5);
		/* NOTE that i only record operation if no "error"  */
		/* NOTE that if we're replaying then call == R_NilValue */
	if (call != R_NilValue)
		recordGraphicOperation(op, originalArgs, dd);
	return R_NilValue;
}

SEXP do_saveplot(SEXP call, SEXP op, SEXP args, SEXP env)
{
}

SEXP do_printplot(SEXP call, SEXP op, SEXP args, SEXP env)
{
}

static void xypoints(SEXP call, SEXP args, int *n)
{
	int k;

	if (!isNumeric(CAR(args)) || (k = LENGTH(CAR(args))) <= 0)
		errorcall(call, "first argument invalid\n");
	CAR(args) = coerceVector(CAR(args), REALSXP);
	*n = k;
	args = CDR(args);

	if (!isNumeric(CAR(args)) || (k = LENGTH(CAR(args))) <= 0)
		errorcall(call, "second argument invalid\n");
	CAR(args) = coerceVector(CAR(args), REALSXP);
	if (k > *n) *n = k;
	args = CDR(args);

	if (!isNumeric(CAR(args)) || (k = LENGTH(CAR(args))) <= 0)
		errorcall(call, "third argument invalid\n");
	CAR(args) = coerceVector(CAR(args), REALSXP);
	if (k > *n) *n = k;
	args = CDR(args);

	if (!isNumeric(CAR(args)) || (k = LENGTH(CAR(args))) <= 0)
		errorcall(call, "fourth argument invalid\n");
	CAR(args) = coerceVector(CAR(args), REALSXP);
	if (k > *n) *n = k;
	args = CDR(args);
}

	/* segments(x0, y0, x1, y1, col, lty) */

SEXP do_segments(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP sx0, sy0, sx1, sy1, col, lty;
	double *x0, *x1, *y0, *y1;
	double xx[2], yy[2];
	int nx0, nx1, ny0, ny1;
	int i, n, ncol, nlty;
	SEXP originalArgs = args;
	DevDesc *dd = CurrentDevice();

	GCheckState(dd);

	if(length(args) < 4) errorcall(call, "too few arguments\n");

	xypoints(call, args, &n);

	sx0 = CAR(args); nx0 = length(sx0); args = CDR(args);
	sy0 = CAR(args); ny0 = length(sy0); args = CDR(args);
	sx1 = CAR(args); nx1 = length(sx1); args = CDR(args);
	sy1 = CAR(args); ny1 = length(sy1); args = CDR(args);

	PROTECT(lty = FixupLty(GetPar("lty", args), dd));
	nlty = length(lty);

	PROTECT(col = FixupCol(GetPar("col", args), dd));
	ncol = LENGTH(col);

	GSavePars(dd);

	x0 = REAL(sx0);
	y0 = REAL(sy0);
	x1 = REAL(sx1);
	y1 = REAL(sy1);

	GMode(dd, 1);
	for (i = 0; i < n; i++) {
		xx[0] = x0[i%nx0];
		yy[0] = y0[i%ny0];
		xx[1] = x1[i%nx1];
		yy[1] = y1[i%ny1];
		GConvert(xx, yy, USER, DEVICE, dd);
		GConvert(xx+1, yy+1, USER, DEVICE, dd);
		if (FINITE(xx[0]) && FINITE(yy[0]) && 
		    FINITE(xx[1]) && FINITE(yy[1])) {
			dd->gp.col = INTEGER(col)[i % ncol];
			if (dd->gp.col == NA_INTEGER) 
				dd->gp.col = dd->dp.col;
			dd->gp.lty = INTEGER(lty)[i % nlty];
			GLine(xx[0], yy[0], xx[1], yy[1], DEVICE, dd);
		}
	}
	GMode(dd, 0);
	GRestorePars(dd);

	UNPROTECT(2);
		/* NOTE that i only record operation if no "error"  */
		/* NOTE that if we're replaying then call == R_NilValue */
	if (call != R_NilValue)
		recordGraphicOperation(op, originalArgs, dd);
	return R_NilValue;
}

	/* rect(xl, yb, xr, yt, col, border) */

SEXP do_rect(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP sxl, sxr, syb, syt, col, lty, border;
	double *xl, *xr, *yb, *yt, x0, y0, x1, y1;
	int i, n, nxl, nxr, nyb, nyt;
	int ncol, nlty, nborder;
	SEXP originalArgs = args;
	DevDesc *dd = CurrentDevice();

	if(length(args) < 4) errorcall(call, "too few arguments\n");
	GCheckState(dd);

	xypoints(call, args, &n);

	sxl = CAR(args); nxl = length(sxl); args = CDR(args);
	syb = CAR(args); nyb = length(syb); args = CDR(args);
	sxr = CAR(args); nxr = length(sxr); args = CDR(args);
	syt = CAR(args); nyt = length(syt); args = CDR(args);

	PROTECT(col = FixupCol(GetPar("col", args), dd));
	ncol = LENGTH(col);

	PROTECT(border =  FixupCol(GetPar("border", args), dd));
	nborder = LENGTH(border);

	PROTECT(lty = FixupLty(GetPar("lty", args), dd));
	nlty = length(lty);

	GSavePars(dd);

	if (nlty && INTEGER(lty)[i % nlty] != NA_INTEGER) 
		dd->gp.lty = INTEGER(lty)[i % nlty];
	else 
		dd->gp.lty = dd->dp.lty;

	xl = REAL(sxl);
	xr = REAL(sxr);
	yb = REAL(syb);
	yt = REAL(syt);

	GMode(dd, 1);
	for (i = 0; i < n; i++) {
		x0 = xl[i%nxl];
		y0 = yb[i%nyb];
		x1 = xr[i%nxr];
		y1 = yt[i%nyt];
		GConvert(&x0, &y0, USER, DEVICE, dd);
		GConvert(&x1, &y1, USER, DEVICE, dd);
		if (FINITE(x0) && FINITE(y0) && FINITE(x1) && FINITE(y1))
			GRect(x0, y0, x1, y1, DEVICE, INTEGER(col)[i % ncol],
			      INTEGER(border)[i % nborder], dd);
	}
	GMode(dd, 0);

	GRestorePars(dd);
	UNPROTECT(3);
		/* NOTE that i only record operation if no "error"  */
		/* NOTE that if we're replaying then call == R_NilValue */
	if (call != R_NilValue)
		recordGraphicOperation(op, originalArgs, dd);
	return R_NilValue;
}

	/* do_arrows(x0, y0, x1, y1, length, angle, code, col) */

SEXP do_arrows(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP sx0, sx1, sy0, sy1, col, lty;
	double *x0, *x1, *y0, *y1;
	double xx0, yy0, xx1, yy1, hlength, angle;
	int code, i, n, nx0, nx1, ny0, ny1;
	int ncol, nlty, xpd;
	SEXP originalArgs = args;
	DevDesc *dd = CurrentDevice();

	if(length(args) < 4) errorcall(call, "too few arguments\n");
	GCheckState(dd);

	xypoints(call, args, &n);

	sx0 = CAR(args); nx0 = length(sx0); args = CDR(args);
	sy0 = CAR(args); ny0 = length(sy0); args = CDR(args);
	sx1 = CAR(args); nx1 = length(sx1); args = CDR(args);
	sy1 = CAR(args); ny1 = length(sy1); args = CDR(args);

	hlength = asReal(GetPar("length", args));
	if (!FINITE(hlength) || hlength <= 0)
		errorcall(call, "invalid head length\n");

	angle = asReal(GetPar("angle", args));
	if (!FINITE(angle))
		errorcall(call, "invalid head angle\n");

	code = asInteger(GetPar("code", args));
	if (code == NA_INTEGER || code < 0 || code > 3)
		errorcall(call, "invalid arrow head specification\n");

	PROTECT(col = FixupCol(GetPar("col", args), dd));
	ncol = LENGTH(col);

	PROTECT(lty = FixupLty(GetPar("lty", args), dd));
	nlty = length(lty);

	xpd = asLogical(GetPar("xpd", args));
	if (xpd == NA_LOGICAL) 
		xpd = dd->gp.xpd;

	GSavePars(dd);

	x0 = REAL(sx0);
	y0 = REAL(sy0);
	x1 = REAL(sx1);
	y1 = REAL(sy1);

	GMode(dd, 1);
	for (i = 0; i < n; i++) {
		xx0 = x0[i%nx0];
		yy0 = y0[i%ny0];
		xx1 = x1[i%nx1];
		yy1 = y1[i%ny1];
		GConvert(&xx0, &yy0, USER, DEVICE, dd);
		GConvert(&xx1, &yy1, USER, DEVICE, dd);
		if (FINITE(xx0) && FINITE(yy0) && FINITE(xx1) && FINITE(yy1)) {
			dd->gp.col = INTEGER(col)[i % ncol];
			if (dd->gp.col == NA_INTEGER) 
				dd->gp.col = dd->dp.col;
			if(nlty == 0 || INTEGER(lty)[i % nlty] == NA_INTEGER)
				dd->gp.lty = dd->dp.lty;
			else
				dd->gp.lty = INTEGER(lty)[i % nlty];
			GArrow(xx0, yy0, xx1, yy1, DEVICE,
			       hlength, angle, code, dd);
		}
	}
	GMode(dd, 0);

	GRestorePars(dd);
	UNPROTECT(2);
		/* NOTE that i only record operation if no "error"  */
		/* NOTE that if we're replaying then call == R_NilValue */
	if (call != R_NilValue)
		recordGraphicOperation(op, originalArgs, dd);
	return R_NilValue;
}

	/* polygon(x, y, col, border) */

SEXP do_polygon(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP sx, sy, col, border, lty;
	int nx, ny, ncol, nborder, nlty, xpd;
	double *work;
	char *vmax;
	SEXP originalArgs = args;
	DevDesc *dd = CurrentDevice();

	GCheckState(dd);

	if(length(args) < 2) errorcall(call, "too few arguments\n");

	if (!isNumeric(CAR(args)) || (nx = LENGTH(CAR(args))) <= 0)
		errorcall(call, "first argument invalid\n");
	sx = CAR(args) = coerceVector(CAR(args), REALSXP);
	args = CDR(args);

	if (!isNumeric(CAR(args)) || (ny = LENGTH(CAR(args))) <= 0)
		errorcall(call, "second argument invalid\n");
	sy = CAR(args) = coerceVector(CAR(args), REALSXP);
	args = CDR(args);

	if (ny != nx)
		errorcall(call, "x and y lengths differ in polygon");

	PROTECT(col = FixupCol(GetPar("col", args), dd));
	ncol = LENGTH(col);

	PROTECT(border = FixupCol(GetPar("border", args), dd));
	nborder = LENGTH(border);

	PROTECT(lty = FixupLty(GetPar("lty", args), dd));
	nlty = length(lty);

	xpd = asLogical(GetPar("xpd", args));
	if (xpd == NA_LOGICAL) 
		xpd = dd->gp.xpd;

	GSavePars(dd);

	GMode(dd, 1);

	if (INTEGER(lty)[0] == NA_INTEGER) 
		dd->gp.lty = dd->dp.lty;
	else 
		dd->gp.lty = INTEGER(lty)[0];
	GPolygon(nx, REAL(sx), REAL(sy), USER,
		 INTEGER(col)[0], INTEGER(border)[0], dd);

	GMode(dd, 0);

	GRestorePars(dd);
	UNPROTECT(3);
		/* NOTE that i only record operation if no "error"  */
		/* NOTE that if we're replaying then call == R_NilValue */
	if (call != R_NilValue)
		recordGraphicOperation(op, originalArgs, dd);
	return R_NilValue;
}

SEXP do_text(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP sx, sy, sxy, txt, adj, cex, col, font;
	int i, n, ncex, ncol, nfont, ntxt, xpd;
	double adjx, adjy;
	double *x, *y;
	double xx, yy;
	SEXP originalArgs = args;
	DevDesc *dd = CurrentDevice();

	GCheckState(dd);

	if(length(args) < 2) errorcall(call, "too few arguments\n");

	sxy = CAR(args);
	if (!isList(sxy) || length(sxy) < 2)
		errorcall(call, "invalid plotting structure\n");
	internalTypeCheck(call, sx = CAR(sxy), REALSXP);
	internalTypeCheck(call, sy = CADR(sxy), REALSXP);
	if (LENGTH(sx) != LENGTH(sy))
		error("x and y lengths differ for plot\n");
	n = LENGTH(sx);
	args = CDR(args);

#ifdef OLD
	internalTypeCheck(call, txt = CAR(args), STRSXP);
#else
	txt = CAR(args);
#endif
	if (LENGTH(txt) <= 0)
		errorcall(call, "zero length \"text\" specified\n");
	args = CDR(args);

	PROTECT(cex = FixupCex(GetPar("cex", args)));
	ncex = LENGTH(cex);

	PROTECT(col = FixupCol(GetPar("col", args), dd));
	ncol = LENGTH(col);

	PROTECT(font = FixupFont(GetPar("font", args)));
	nfont = LENGTH(font);

	PROTECT(adj = GetPar("adj", args));
	if(isNull(adj) || (isNumeric(adj) && length(adj) == 0)) {
		adjx = dd->gp.adj;
		adjy = dd->gp.yCharOffset;
	}
	else if(isReal(adj)) {
		if(LENGTH(adj) == 1) {
			adjx = REAL(adj)[0];
			adjy = dd->gp.yCharOffset;
		}
		else {
			adjx = REAL(adj)[0];
			adjy = REAL(adj)[1];
		}
	}
	else errorcall(call, "invalid adj value\n");

	xpd = asLogical(GetPar("xpd", args));
	if(xpd == NA_LOGICAL) xpd = 0;

	x = REAL(sx);
	y = REAL(sy);
	n = LENGTH(sx);
	ntxt = LENGTH(txt);

	GSavePars(dd);

	dd->gp.xpd = xpd;

	GMode(dd, 1);
	for (i = 0; i < n; i++) {
		xx = x[i % n];
		yy = y[i % n];
		GConvert(&xx, &yy, USER, DEVICE, dd);
		if (FINITE(xx) && FINITE(yy)) {
			if (ncol && INTEGER(col)[i % ncol] != NA_INTEGER)
				dd->gp.col = INTEGER(col)[i % ncol];
			else 
				dd->gp.col = dd->dp.col;
			if(ncex && FINITE(REAL(cex)[i%ncex]))
				dd->gp.cex = dd->gp.cexbase * 
					     REAL(cex)[i % ncex];
			else 
				dd->gp.cex = dd->gp.cexbase;
			if (nfont && INTEGER(font)[i % nfont] != NA_INTEGER)
				dd->gp.font = INTEGER(font)[i % nfont];
			else 
				dd->gp.font = dd->dp.font;
			if(isExpression(txt))
				GMathText(xx, yy, DEVICE, 
					  VECTOR(txt)[i % ntxt],
					  adjx, adjy, 0.0, dd);
			else
				GText(xx, yy, DEVICE, 
				      CHAR(STRING(txt)[i % ntxt]), 
				      adjx, adjy, 0.0, dd);
		}
	}
	GMode(dd, 0);

	GRestorePars(dd);
	UNPROTECT(4);
		/* NOTE that i only record operation if no "error"  */
		/* NOTE that if we're replaying then call == R_NilValue */
	if (call != R_NilValue)
		recordGraphicOperation(op, originalArgs, dd);
	return R_NilValue;
}

	/* mtext(text, side, line, outer, at = NULL, ...)
	 *
	 * where ... supports  adj, cex, col, font  */

SEXP do_mtext(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP adj, cex, col, font, text;
	double line, at, adjx, adjy;
	int side, outer;
	int newsave;
	SEXP originalArgs = args;
	DevDesc *dd = CurrentDevice();

	GCheckState(dd);

	if(length(args) < 5) errorcall(call, "too few arguments\n");

	/* internalTypeCheck(call, text = CAR(args), STRSXP); */
	text = CAR(args);
	if (LENGTH(text) <= 0)
		errorcall(call, "zero length \"text\" specified\n");
	args = CDR(args);

	side = asInteger(CAR(args));
	if(side < 1 || side > 4) errorcall(call, "invalid side value\n");
	args = CDR(args);

	line = asReal(CAR(args));
	if(!FINITE(line)) /* || line < 0.0 -- negative values make sense ! */
	  errorcall(call, "invalid line value\n");
	args = CDR(args);

	outer = asInteger(CAR(args));
	if(outer == NA_INTEGER) outer = 0;
	args = CDR(args);

	if (CAR(args) != R_NilValue || LENGTH(CAR(args)) == 0) {
		at = asReal(CAR(args));
		if(!FINITE(at)) errorcall(call, "invalid at value\n");
		args = CDR(args);
	}
	else at = NA_REAL;

	GSavePars(dd);

	PROTECT(adj = GetPar("adj", args));
	if(isNull(adj) || (isNumeric(adj) && length(adj) == 0)) {
		adjx = dd->gp.adj;
		adjy = dd->gp.yCharOffset;
	}
	else if(isReal(adj)) {
		if(LENGTH(adj) == 1) {
			adjx = REAL(adj)[0];
			adjy = dd->gp.yCharOffset;
		}
		else {
			adjx = REAL(adj)[0];
			adjy = REAL(adj)[1];
		}
	}
	else errorcall(call, "invalid adj value\n");

	if(!FINITE(at)) {
		switch(side % 2) {
		case 0:
			at = yNPCtoUsr(adjx, dd);
			break;
		case 1:
			at = xNPCtoUsr(adjx, dd);
			break;
		}
	}

	PROTECT(cex = FixupCex(GetPar("cex", args)));
	if(FINITE(REAL(cex)[0])) dd->gp.cex = dd->gp.cexbase * REAL(cex)[0];
	else dd->gp.cex = dd->gp.cexbase;

	PROTECT(col = FixupCol(GetPar("col", args), dd));
	if(INTEGER(col)[0] != NA_INTEGER) dd->gp.col = INTEGER(col)[0];

	PROTECT(font = FixupFont(GetPar("font", args)));
	if(INTEGER(font)[0] != NA_INTEGER) dd->gp.font = INTEGER(font)[0];

	dd->gp.adj = adjx;
	dd->gp.xpd = 1;
	if (outer) 
		newsave = dd->gp.new;
	GMode(dd, 1);
	if(isExpression(text))
		GMMathText(VECTOR(text)[0], side, line, outer, at, 0, dd);
	else
		GMtext(CHAR(STRING(text)[0]), side, line, outer, at, 0, dd);
	GMode(dd, 0);

	GRestorePars(dd);
	if (outer) 
		dd->gp.new = dd->dp.new = newsave;
	UNPROTECT(4);
		/* NOTE that i only record operation if no "error"  */
		/* NOTE that if we're replaying then call == R_NilValue */
	if (call != R_NilValue)
		recordGraphicOperation(op, originalArgs, dd);
	return R_NilValue;
}

	/* Title(main=NULL, sub=NULL, xlab=NULL, ylab=NULL, ...) */

SEXP do_title(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP main, xlab, ylab, sub;
	double adj;
	SEXP originalArgs = args;
	DevDesc *dd = CurrentDevice();

	GCheckState(dd);

	if(length(args) < 4) errorcall(call, "too few arguments\n");

	main = sub = xlab = ylab = R_NilValue;

	if (CAR(args) != R_NilValue && LENGTH(CAR(args)) > 0)
		main = CAR(args);
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

	GSavePars(dd);

	/* Always work in expanded mode */
	dd->gp.xpd = 1;

	adj = dd->gp.adj;

	GMode(dd, 1);
	if(main != R_NilValue) {
		dd->gp.cex = dd->gp.cexbase * dd->gp.cexmain;
		dd->gp.col = dd->gp.colmain;
		dd->gp.font = dd->gp.fontmain;
		if(isExpression(main))
			GMathText(xNPCtoUsr(adj, dd), 0.5*dd->gp.mar[2], MAR3,
				  VECTOR(main)[0], 0.5, 0.5, 0.0, dd);
		else
			GText(xNPCtoUsr(adj, dd), 0.5*dd->gp.mar[2], MAR3,
			      CHAR(STRING(main)[0]), adj, 0.5, 0.0, dd);
	}
	if(sub != R_NilValue) {
		dd->gp.cex = dd->gp.cexbase * dd->gp.cexsub;
		dd->gp.col = dd->gp.colsub;
		dd->gp.font = dd->gp.fontsub;
		if(isExpression(sub))
			GMMathText(VECTOR(sub)[0], 1, dd->gp.mgp[0]+1.0, 0,
				   xNPCtoUsr(adj, dd), 0, dd);
		else
			GMtext(CHAR(STRING(sub)[0]), 1, dd->gp.mgp[0]+1.0, 0, 
			       xNPCtoUsr(adj, dd), 0, dd);
	}
	if(xlab != R_NilValue) {
		dd->gp.cex = dd->gp.cexbase * dd->gp.cexlab;
		dd->gp.col = dd->gp.collab;
		dd->gp.font = dd->gp.fontlab;
		if(isExpression(xlab))
			GMMathText(VECTOR(xlab)[0], 1, dd->gp.mgp[0], 0,
				   xNPCtoUsr(adj, dd), 0, dd);
		else
			GMtext(CHAR(STRING(xlab)[0]), 1, dd->gp.mgp[0], 0,
			       xNPCtoUsr(adj, dd), 0, dd);
	}
	if(ylab != R_NilValue) {
		dd->gp.cex = dd->gp.cexbase * dd->gp.cexlab;
		dd->gp.col = dd->gp.collab;
		dd->gp.font = dd->gp.fontlab;
		if(isExpression(ylab))
			GMMathText(VECTOR(ylab)[0], 2, dd->gp.mgp[0], 0,
				   yNPCtoUsr(adj, dd), 0, dd);
		else
			GMtext(CHAR(STRING(ylab)[0]), 2, dd->gp.mgp[0], 0,
			       yNPCtoUsr(adj, dd), 0, dd);
	}
	GMode(dd, 0);
	GRestorePars(dd);
		/* NOTE that i only record operation if no "error"  */
		/* NOTE that if we're replaying then call == R_NilValue */
	if (call != R_NilValue)
		recordGraphicOperation(op, originalArgs, dd);
	return R_NilValue;
}

SEXP do_abline(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP a, b, h, v, col, lty;
	int i, ncol, nlines, nlty;
	double aa, bb, x[2], y[2];
	SEXP originalArgs = args;
	DevDesc *dd = CurrentDevice();

	GCheckState(dd);

	if(length(args) < 4) errorcall(call, "too few arguments\n");

	if((a = CAR(args)) != R_NilValue)
		CAR(args) = a = coerceVector(a, REALSXP);
	args = CDR(args);

	if((b = CAR(args)) != R_NilValue)
		CAR(args) = b = coerceVector(b, REALSXP);
	args = CDR(args);

	if((h = CAR(args)) != R_NilValue)
		CAR(args) = h = coerceVector(h, REALSXP);
	args = CDR(args);

	if((v = CAR(args)) != R_NilValue)
		CAR(args) = v = coerceVector(v, REALSXP);
	args = CDR(args);

	PROTECT(col = FixupCol(CAR(args), dd));
	ncol = LENGTH(col);
	args = CDR(args);

	PROTECT(lty = FixupLty(CAR(args), dd));
	nlty = length(lty);
	args = CDR(args);

	GSavePars(dd);

	nlines = 0;

	if (a != R_NilValue) {
		if (b == R_NilValue) {
			if (LENGTH(a) != 2)
				errorcall(call, "invalid a=, b= specification in \"abline\"\n");
			aa = REAL(a)[0];
			bb = REAL(a)[1];
		}
		else {
			aa = asReal(a);
			bb = asReal(b);
		}
		if (!FINITE(aa) || !FINITE(bb))
			errorcall(call, "\"a\" and \"b\" must be non-missing\n");
		dd->gp.col = INTEGER(col)[i % ncol];
		if (nlty && INTEGER(lty)[i % nlty] != NA_INTEGER) 
			dd->gp.lty = INTEGER(lty)[i % nlty];
		else 
			dd->gp.lty = dd->dp.lty;
		GMode(dd, 1);
		if (dd->gp.xlog) {
			x[0] = dd->gp.logusr[0]; 
			x[1] = dd->gp.logusr[1]; 
		}
		else {
			x[0] = dd->gp.usr[0]; 
			x[1] = dd->gp.usr[1];
		}
		if (dd->gp.ylog) {
			y[0] = aa + dd->gp.logusr[0] * bb;
			y[1] = aa + dd->gp.logusr[1] * bb;
		}
		else {
			y[0] = aa + dd->gp.usr[0] * bb;
			y[1] = aa + dd->gp.usr[1] * bb;
		}
		GLine(x[0], y[0], x[1], y[1], USER, dd);
		GMode(dd, 0);
		nlines++;
	}
	if (h != R_NilValue) {
		GMode(dd, 1);
		for (i = 0; i < LENGTH(h); i++) {
			dd->gp.col = INTEGER(col)[i % ncol];
			if (nlty && INTEGER(lty)[i % nlty] != NA_INTEGER) 
				dd->gp.lty = INTEGER(lty)[i % nlty];
			else 
				dd->gp.lty = dd->dp.lty;
			aa = REAL(h)[i];
			if (FINITE(aa)) {
				if (dd->gp.xlog) {
					x[0] = dd->gp.logusr[0];
					x[1] = dd->gp.logusr[1];
				}
				else {
					x[0] = dd->gp.usr[0]; 
					x[1] = dd->gp.usr[1];
				}
				y[0] = aa;
				y[1] = aa;
				GLine(x[0], y[0], x[1], y[1], USER, dd);
			}
			nlines++;
		}
		GMode(dd, 0);
	}
	if (v != R_NilValue) {
		GMode(dd, 1);
		for (i = 0; i < LENGTH(v); i++) {
			dd->gp.col = INTEGER(col)[i % ncol];
			if (nlty && INTEGER(lty)[i % nlty] != NA_INTEGER) 
				dd->gp.lty = INTEGER(lty)[i % nlty];
			else 
				dd->gp.lty = dd->dp.lty;
			aa = REAL(v)[i];
			if (FINITE(aa)) {
				if (dd->gp.ylog) {
					y[0] = dd->gp.logusr[2];
					y[1] = dd->gp.logusr[3];
				}
				else {
					y[0] = dd->gp.usr[2];
					y[1] = dd->gp.usr[3];
				}
				x[0] = aa; 
				x[1] = aa;
				GLine(x[0], y[0], x[1], y[1], USER, dd);
			}
			nlines++;
		}
		GMode(dd, 0);
	}
	UNPROTECT(2);
	GRestorePars(dd);
		/* NOTE that i only record operation if no "error"  */
		/* NOTE that if we're replaying then call == R_NilValue */
	if (call != R_NilValue)
		recordGraphicOperation(op, originalArgs, dd);
	return R_NilValue;
}

SEXP do_box(SEXP call, SEXP op, SEXP args, SEXP env)
{
	int which, col, fg;
	SEXP originalArgs = args;
	DevDesc *dd = CurrentDevice();

	GCheckState(dd);
	GSavePars(dd);
	which = asInteger(CAR(args));
	if(which < 1 || which > 4)
		errorcall(call, "invalid \"which\" specification\n");
	col = dd->gp.col;
	fg = dd->gp.fg;
	dd->gp.col = NA_INTEGER;
	dd->gp.fg = NA_INTEGER;
	dd->gp.xpd = 1;
	if(dd->gp.col == NA_REAL) fg = dd->gp.col;
	if(dd->gp.fg == NA_REAL) fg = dd->gp.fg;
	dd->gp.col = fg;
	GMode(dd, 1);
	GBox(which, dd);
	GMode(dd, 0);
	GRestorePars(dd);
		/* NOTE that i only record operation if no "error"  */
		/* NOTE that if we're replaying then call == R_NilValue */
	if (call != R_NilValue)
		recordGraphicOperation(op, originalArgs, dd);
	return R_NilValue;
}

SEXP do_locator(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP x, y, nobs, ans;
	int i, n;
	DevDesc *dd = CurrentDevice();

	GCheckState(dd);

	checkArity(op, args);
	n = asInteger(CAR(args));
	if(n <= 0 || n == NA_INTEGER)
		error("invalid number of points in locator\n");
	PROTECT(x = allocVector(REALSXP, n));
	PROTECT(y = allocVector(REALSXP, n));
	PROTECT(nobs=allocVector(INTSXP,1));
	i = 0;

	GMode(dd, 2);
	while(i < n) {
		if(!GLocator(&(REAL(x)[i]), &(REAL(y)[i]), USER, dd))
			break;
		i += 1;
	}
	GMode(dd, 0);
	INTEGER(nobs)[0] = i;
	while(i < n) {
		REAL(x)[i] = NA_REAL;
		REAL(y)[i] = NA_REAL;
		i += 1;
	}
	ans = allocList(3);
	UNPROTECT(3);
	CAR(ans) = x;
	CADR(ans) = y;
	CADDR(ans) = nobs;
	return ans;
}

#define THRESHOLD	0.25

#ifdef Macintosh
double hypot(double x, double y)
{
	return sqrt(x*x+y*y);
}
#endif

SEXP do_identify(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP ans, x, y, l, ind, pos;
	double xi, yi, xp, yp, d, dmin, offset;
	int i, imin, k, n;
	DevDesc *dd = CurrentDevice();

	GCheckState(dd);

	checkArity(op, args);
	x = CAR(args);
	y = CADR(args);
	l = CADDR(args);
	if(!isReal(x) || !isReal(y) || !isString(l))
		errorcall(call, "incorrect argument type\n");
	if(LENGTH(x) != LENGTH(y) || LENGTH(x) != LENGTH(l))
		errorcall(call, "different argument lengths\n");
	n = LENGTH(x);
	if(n <= 0) {
		R_Visible = 0;
		return NULL;
	}

	offset = GConvertXUnits(0.5, CHARS, INCHES, dd);
	PROTECT(ind = allocVector(LGLSXP, n));
	PROTECT(pos = allocVector(INTSXP, n));
	for(i=0 ; i<n ; i++)
		LOGICAL(ind)[i] = 0;

	k = 0;
	GMode(dd, 2);
	while(k < n) {
		if(!GLocator(&xp, &yp, INCHES, dd)) break;
		dmin = DBL_MAX;
		imin = -1;
		for(i=0 ; i<n ; i++) {
			xi = REAL(x)[i];
			yi = REAL(y)[i];
			GConvert(&xi, &yi, USER, INCHES, dd);
			if(!FINITE(xi) || !FINITE(yi)) continue;
			d = hypot(xp-xi, yp-yi);
			if(d < dmin) {
				imin = i;
				dmin = d;
			}
		}
		if(dmin > THRESHOLD)
			REprintf("warning: no point with %.2f inches\n", THRESHOLD);
		else if(LOGICAL(ind)[imin])
			REprintf("warning: nearest point already identified\n");
		else {
			LOGICAL(ind)[imin] = 1;
			xi = REAL(x)[imin];
			yi = REAL(y)[imin];
			GConvert(&xi, &yi, USER, INCHES, dd);
			if(fabs(xp-xi) >= fabs(yp-yi)) {
				if(xp >= xi) {
					INTEGER(pos)[imin] = 4;
					xi = xi+offset;
					GText(xi, yi, INCHES,
					      CHAR(STRING(l)[imin]), 0.0, 
					      dd->gp.yCharOffset, 0.0, dd);
				}
				else {
					INTEGER(pos)[imin] = 2;
					xi = xi-offset;
					GText(xi, yi, INCHES,
					      CHAR(STRING(l)[imin]), 1.0, 
					      dd->gp.yCharOffset, 0.0, dd);
				}
			}
			else {
				if(yp >= yi) {
					INTEGER(pos)[imin] = 3;
					yi = yi+offset;
					GText(xi, yi, INCHES,
					      CHAR(STRING(l)[imin]), 0.5, 
					      0.0, 0.0, dd);
				}
				else {
					INTEGER(pos)[imin] = 1;
					yi = yi-offset;
					GText(xi, yi, INCHES,
					      CHAR(STRING(l)[imin]), 0.5, 
					      1-(0.5-dd->gp.yCharOffset), 
					      0.0, dd);
				}
			}
		}
	}
	GMode(dd, 0);
	ans = allocList(2);
	CAR(ans) = ind;
	CADR(ans) = pos;
	UNPROTECT(2);
	return ans;
}

	/* strwidth(str, units) */

SEXP do_strheight(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP ans, str;
	int i, n, units;
	double cex, cexsave;
	DevDesc *dd = CurrentDevice();

	checkArity(op,args);
	str = CAR(args);
	if((TYPEOF(str) != STRSXP) && (TYPEOF(str) != EXPRSXP))
		errorcall(call, "character or expression first argument expected\n");
	args = CDR(args);

	if((units = asInteger(CAR(args))) == NA_INTEGER || units < 0)
		errorcall(call, "invalid units\n");
	args = CDR(args);

	if(isNull(CAR(args)))
		cex = dd->gp.cex;
	else if(!FINITE(cex = asReal(CAR(args))) || cex <= 0.0)
		errorcall(call, "invalid cex value\n");

	n = LENGTH(str);
	ans = allocVector(REALSXP, n);
	cexsave = dd->gp.cex;
	dd->gp.cex = cex * dd->gp.cexbase;
	for(i=0 ; i<n ; i++)
		if (isExpression(str))
			REAL(ans)[i] = GExpressionHeight(VECTOR(str)[i], 
							 GMapUnits(units), dd);
		else
			REAL(ans)[i] = GStrHeight(CHAR(STRING(str)[i]), 
						  GMapUnits(units), dd);
	dd->gp.cex = cexsave;
	return ans;
}

SEXP do_strwidth(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP ans, str;
	int i, n, units;
	double cex, cexsave;
	DevDesc *dd = CurrentDevice();

	checkArity(op, args);
	str = CAR(args);
	if((TYPEOF(str) != STRSXP) && (TYPEOF(str) != EXPRSXP))
		errorcall(call, "character or expression first argument expected\n");
	args = CDR(args);

	if((units = asInteger(CAR(args))) == NA_INTEGER || units < 0)
		errorcall(call, "invalid units\n");
	args = CDR(args);

	if(isNull(CAR(args)))
		cex = dd->gp.cex;
	else if(!FINITE(cex = asReal(CAR(args))) || cex <= 0.0)
		errorcall(call, "invalid cex value\n");

	n = LENGTH(str);
	ans = allocVector(REALSXP, n);
	cexsave = dd->gp.cex;
	dd->gp.cex = cex * dd->gp.cexbase;
	for(i=0 ; i<n ; i++)
		if (isExpression(str))
			REAL(ans)[i] = GExpressionWidth(VECTOR(str)[i], 
							GMapUnits(units), dd);
		else
			REAL(ans)[i] = GStrWidth(CHAR(STRING(str)[i]), 
						 GMapUnits(units), dd);
	dd->gp.cex = cexsave;
	return ans;
}

static int n;
static int *lptr;
static int *rptr;
static double *hght;
static double *xpos;
static double hang;
static double offset;
static SEXP *llabels;

static void drawdend(int node, double *x, double *y, DevDesc *dd)
{
	double xl, xr, yl, yr;
	double xx[4], yy[4];
	int k;

	*y = hght[node-1];

	k = lptr[node-1];
	if(k > 0) drawdend(k, &xl, &yl, dd);
	else {
		xl = xpos[-k-1];
		if(hang >= 0) yl = *y - hang;
		else yl = 0;
		GText(xl, yl-offset, USER, CHAR(llabels[-k-1]), 
		      1.0, 0.3, 90.0, dd);
	}
	k = rptr[node-1];
	if(k > 0) drawdend(k, &xr, &yr, dd);
	else {
		xr = xpos[-k-1];
		if(hang >= 0) yr = *y - hang;
		else yr = 0;
		GText(xr, yr-offset, USER, CHAR(llabels[-k-1]), 
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
	int xpdsave;
	double x, y, ypin;
	SEXP originalArgs = args;
	DevDesc *dd = CurrentDevice();

	checkArity(op, args);

	GCheckState(dd);

	n = asInteger(CAR(args));
	if(n == NA_INTEGER || n < 2)
		goto badargs;
	args = CDR(args);

	if(TYPEOF(CAR(args)) != INTSXP || length(CAR(args)) != 2*n)
		goto badargs;
	lptr = &(INTEGER(CAR(args))[0]);
	rptr = &(INTEGER(CAR(args))[n]);
	args = CDR(args);

	if(TYPEOF(CAR(args)) != REALSXP || length(CAR(args)) != n)
		goto badargs;
	hght = REAL(CAR(args));
	args = CDR(args);

	if(TYPEOF(CAR(args)) != REALSXP || length(CAR(args)) != n+1)
		goto badargs;
	xpos = REAL(CAR(args));
	args = CDR(args);

	hang = asReal(CAR(args));
	if(!FINITE(hang))
		goto badargs;
	args = CDR(args);

	if(TYPEOF(CAR(args)) != STRSXP || length(CAR(args)) != n+1)
		goto badargs;
	llabels = STRING(CAR(args));

	offset = GConvertYUnits(GStrWidth("m", INCHES, dd), INCHES, USER, dd);

	GSavePars(dd);

	xpdsave = dd->gp.xpd;
	dd->gp.xpd = 1;
	GMode(dd, 1);
	drawdend(n, &x, &y, dd);
	GMode(dd, 0);
	dd->gp.xpd = xpdsave;

	GRestorePars(dd);

		/* NOTE that i only record operation if no "error"  */
		/* NOTE that if we're replaying then call == R_NilValue */
	if (call != R_NilValue)
		recordGraphicOperation(op, originalArgs, dd);

	return R_NilValue;

badargs:
	error("invalid dendrogram input\n");
}
 
SEXP do_erase(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP col;
	int ncol;
	DevDesc *dd = CurrentDevice();

	checkArity(op, args);

	PROTECT(col = FixupCol(CAR(args), dd));
	ncol = LENGTH(col);

	GSavePars(dd);

	GMode(dd, 1);
	GRect(0.0, 0.0, 1.0, 1.0, NDC, INTEGER(col)[0], NA_INTEGER, dd);
	GMode(dd, 0);

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

	/* CONTOUR PLOTTING */
	
	/**** stuff for contour plots ****/

typedef struct SEG {
	struct SEG *next;
	double x0;
	double y0;
	double x1;
	double y1;
} SEG, *SEGP;

static SEGP *SegDB;

static int intersect(double z0, double z1, double zc, double *f)
{
	if((z0 - zc) * (z1 - zc) < 0.0) {
		*f = (zc - z0) / (z1 -  z0);
		return 1;
	}
	return 0;
}

static SEGP NewSeg(double x0, double y0, double x1, double y1, SEGP prev)
{
	SEGP seg = (SEGP)R_alloc(1, sizeof(SEG));
	seg->x0 = x0;
	seg->y0 = y0;
	seg->x1 = x1;
	seg->y1 = y1;
	seg->next = prev;
	return seg;
}

static void SwapSeg(SEGP seg)
{
	double x, y;
	x = seg->x0;
	y = seg->y0;
	seg->x0 = seg->x1;
	seg->y0 = seg->y1;
	seg->x1 = x;
	seg->y1 = y;
}

	/* Determine the entry direction to the next cell */
	/* and update the cell indices */

#ifdef OLD
#define XMATCH(x0,x1) (fabs(x0-x1)<xtol)
#define YMATCH(y0,y1) (fabs(y0-y1)<ytol)
#else
#define XMATCH(x0,x1) (fabs(x0-x1)==0)
#define YMATCH(y0,y1) (fabs(y0-y1)==0)
#endif

static double xtol;
static double ytol;

static int SegDir(double xend, double yend, double *x, double *y, int *i, int *j, int nx, int ny)
{
	if(YMATCH(yend, y[*j])) {
		if(*j == 0) return 0;
		*j = *j - 1;
		return 3;
	}
	if(XMATCH(xend, x[*i])) {
		if(*i == 0) return 0;
		*i = *i - 1;
		return 4;
	}
	if(YMATCH(yend, y[*j+1])) {
		if(*j >= ny - 1) return 0;
		*j = *j + 1;
		return 1;
	}
	if(XMATCH(xend, x[*i+1])) {
		if(*i >= nx - 1) return 0;
		*i = *i + 1;
		return 2;
	}
	return 0;
}

	/* Search seglist for a segment with endpoint (xend, yend). */
	/* The cell entry direction is dir, and if tail=1/0 we are */
	/* building the tail/head of a contour.  The matching segment */
	/* is pointed to by seg and the updated segment list (with
	/* the matched segment stripped is returned by the funtion. */

static SEGP SegUpdate(double xend, double yend, int dir, int tail, SEGP seglist, SEGP* seg)
{
	if(seglist == NULL) {
		*seg = NULL;
		return NULL;
	}
	switch(dir) {
	case 1:
	case 3:
		if(YMATCH(yend,seglist->y0)) {
			if(!tail) SwapSeg(seglist);
			*seg = seglist;
			return seglist->next;
		}
		if(YMATCH(yend,seglist->y1)) {
			if(tail) SwapSeg(seglist);
			*seg = seglist;
			return seglist->next;
		}
		break;
	case 2:
	case 4:
		if(XMATCH(xend,seglist->x0)) {
			if(!tail) SwapSeg(seglist);
			*seg = seglist;
			return seglist->next;
		}
		if(XMATCH(xend,seglist->x1)) {
			if(tail) SwapSeg(seglist);
			*seg = seglist;
			return seglist->next;
		}
		break;
	}
	seglist->next = SegUpdate(xend, yend, dir, tail, seglist->next, seg);
	return seglist;
}

static void contour(SEXP x, int nx, SEXP y, int ny, SEXP z, double zc, 
		    double atom, DevDesc *dd)
{
	double f, xl, xh, yl, yh, zll, zhl, zlh, zhh, xx[4], yy[4];
	double xend, yend;
	int i, ii, j, jj, k, l, m, nacode, ns, ns2, dir;
	SEGP seglist, seg, s, start, end;
	double *xxx, *yyy;

	for(i=0 ; i<nx-1 ; i++) {
		xl = REAL(x)[i];
		xh = REAL(x)[i+1];
		for(j=0 ; j<ny-1 ; j++) {
			yl = REAL(y)[j];
			yh = REAL(y)[j+1];
			k = i+j*nx;
			zll = REAL(z)[k];
			zhl = REAL(z)[k+1];
			zlh = REAL(z)[k+nx];
			zhh = REAL(z)[k+nx+1];
			k = 0;

				/* If the value at a corner is */
				/* exactly equal to a contour */
				/* level, change the value at */
				/* corner by a tiny amount. */

			if(zll == zc) zll = zll + atom;
			if(zhl == zc) zhl = zhl + atom;
			if(zlh == zc) zlh = zlh + atom;
			if(zhh == zc) zhh = zhh + atom;

				/* Check for intersections with sides */

			nacode = 0;
			if(FINITE(zll)) nacode += 1;
			if(FINITE(zhl)) nacode += 2;
			if(FINITE(zlh)) nacode += 4;
			if(FINITE(zhh)) nacode += 8;

			switch(nacode) {
			case 15:
				if(intersect(zll, zhl, zc, &f)) {
					xx[k] = xl + f * (xh - xl);
					yy[k] = yl; k++;
				}
				if(intersect(zll, zlh, zc, &f)) {
					yy[k] = yl + f * (yh - yl);
					xx[k] = xl; k++;
				}
				if(intersect(zhl, zhh, zc, &f)) {
					yy[k] = yl + f * (yh - yl);
					xx[k] = xh; k++;
				}
				if(intersect(zlh, zhh, zc, &f)) {
					xx[k] = xl + f * (xh - xl);
					yy[k] = yh; k++;
				}
				break;
			case 14:
				if(intersect(zhl, zhh, zc, &f)) {
					yy[k] = yl + f * (yh - yl);
					xx[k] = xh; k++;
				}
				if(intersect(zlh, zhh, zc, &f)) {
					xx[k] = xl + f * (xh - xl);
					yy[k] = yh; k++;
				}
				if(intersect(zlh, zhl, zc, &f)) {
					xx[k] = xl + f * (xh - xl);
					yy[k] = yh + f * (yl - yh);
					k++;
				}
				break;
			case 13:
				if(intersect(zll, zlh, zc, &f)) {
					yy[k] = yl + f * (yh - yl);
					xx[k] = xl; k++;
				}
				if(intersect(zlh, zhh, zc, &f)) {
					xx[k] = xl + f * (xh - xl);
					yy[k] = yh; k++;
				}
				if(intersect(zll, zhh, zc, &f)) {
					xx[k] = xl + f * (xh - xl);
					yy[k] = yl + f * (yh - yl);
					k++;
				}
				break;
			case 11:
				if(intersect(zhl, zhh, zc, &f)) {
					yy[k] = yl + f * (yh - yl);
					xx[k] = xh; k++;
				}
				if(intersect(zll, zhl, zc, &f)) {
					xx[k] = xl + f * (xh - xl);
					yy[k] = yl; k++;
				}
				if(intersect(zll, zhh, zc, &f)) {
					xx[k] = xl + f * (xh - xl);
					yy[k] = yl + f * (yh - yl);
					k++;
				}
				break;
			case 7:
				if(intersect(zll, zlh, zc, &f)) {
					yy[k] = yl + f * (yh - yl);
					xx[k] = xl; k++;
				}
				if(intersect(zll, zhl, zc, &f)) {
					xx[k] = xl + f * (xh - xl);
					yy[k] = yl; k++;
				}
				if(intersect(zlh, zhl, zc, &f)) {
					xx[k] = xl + f * (xh - xl);
					yy[k] = yh + f * (yl - yh);
					k++;
				}
				break;
			}

				/* We now have k(=2,4) endpoints */
				/* Decide which to join */

			seglist = NULL;

			if(k > 0) {
				if(k == 2) {
					seglist = NewSeg(xx[0], yy[0], xx[1], yy[1], seglist);
				}
				else if(k == 4) {
					for(k=3 ; k>=1 ; k--) {
						m = k;
						xl = xx[k];
						for(l=0 ; l<k ; l++) {
							if(xx[l] > xl) {
								xl = xx[l];
								m = l;
							}
						}
						if(m != k) {
							xl = xx[k];
							yl = yy[k];
							xx[k] = xx[m];
							yy[k] = yy[m];
							xx[m] = xl;
							yy[m] = yl;
						}
					}
					seglist = NewSeg(xx[0], yy[0], xx[1], yy[1], seglist);
					seglist = NewSeg(xx[2], yy[2], xx[3], yy[3], seglist);
				}
				else error("k != 2 or 4\n");
			}
			SegDB[i+j*nx] = seglist;
		}
	}

		/* The segment database is now assembled. */
		/* Begin following contours. */
		/* 1. Grab a segment */
		/* 2. Follow its tail */
		/* 3. Follow its head */
		/* 4. Draw the contour */

	for(i=0 ; i<nx-1 ; i++)
		for(j=0 ; j<ny-1 ; j++) {
			while(seglist = SegDB[i+j*nx]) {
				ii = i; jj = j;
				start = end = seglist;
				SegDB[i+j*nx] = seglist->next;
				xend = seglist->x1;
				yend = seglist->y1;
				while(dir=SegDir(xend, yend, REAL(x), REAL(y), &ii, &jj, nx, ny)) {
					SegDB[ii+jj*nx] = SegUpdate(xend, yend, dir, 1, SegDB[ii+jj*nx], &seg);
					if(!seg) break;
					end->next = seg;
					end = seg;
					xend = end->x1;
					yend = end->y1;
				}
				ii = i; jj = j;
				xend = seglist->x0;
				yend = seglist->y0;
				while(dir=SegDir(xend, yend, REAL(x), REAL(y), &ii, &jj, nx, ny)) {
					SegDB[ii+jj*nx] = SegUpdate(xend, yend, dir, 0, SegDB[ii+jj*nx], &seg);
					if(!seg) break;
					seg->next = start;
					start = seg;
					xend = start->x0;
					yend = start->y0;
				}
				s = start;
				ns = 0;
				while(s) {
					ns++;
					s = s->next;
				}

				/* countour midpoint */
				/* use for labelling sometime */

				if(ns > 3) ns2 = ns/2;
				else ns2 = -1;

				s = start;
				xxx = (double *) C_alloc(ns+1, sizeof(double));
				yyy = (double *) C_alloc(ns+1, sizeof(double));
				ns = 0;
				xxx[ns] = s->x0;
				yyy[ns++] = s->y0;
				while(s->next) {
					s = s->next;
					xxx[ns] = s->x0;
					yyy[ns++] = s->y0;
				}
				xxx[ns] = s->x1;
				yyy[ns++] = s->y1;
				GMode(dd, 1);
				GPolyline(ns, xxx, yyy, USER, dd);
				GMode(dd, 0);
				C_free((char *) xxx);
				C_free((char *) yyy);
			}
		}

}


SEXP do_contour(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP oargs, c, x, y, z, col, lty;
	int i, j, nx, ny, nc, ncol, nlty;
	int ltysave, colsave;
	double atom, zmin, zmax;
	char *vmax, *vmax0;
	DevDesc *dd = CurrentDevice();

	GCheckState(dd);

	if(length(args) < 4) errorcall(call, "too few arguments\n");

	oargs = args;

	x = CAR(args);
	internalTypeCheck(call, x, REALSXP);
	nx = LENGTH(x);
	args = CDR(args);

	y = CAR(args);
	internalTypeCheck(call, y, REALSXP);
	ny = LENGTH(y);
	args = CDR(args);

	z = CAR(args);
	internalTypeCheck(call, z, REALSXP);
	args = CDR(args);

	c = CAR(args);
	internalTypeCheck(call, c, REALSXP);
	nc = LENGTH(c);
	args = CDR(args);

	PROTECT(col = FixupCol(GetPar("col", args), dd));
	ncol = length(col);

	PROTECT(lty = FixupLty(GetPar("lty", args), dd));
	nlty = length(lty);

		/* col, lwd and lty vectors here */

	if(nx < 2 || ny < 2)
		errorcall(call, "insufficient x or y values\n");

	if(nrows(z) != nx || ncols(z) != ny)
		errorcall(call, "dimension mismatch\n");


	if(nc < 1)
		errorcall(call, "no contour values\n");

	for(i=0 ; i<nx ; i++) {
		if(!FINITE(REAL(x)[i]))
			errorcall(call, "missing x values\n");
		if(i > 0 && REAL(x)[i] < REAL(x)[i-1])
			errorcall(call, "increasing x values expected\n");
	}

	for(i=0 ; i<ny ; i++) {
		if(!FINITE(REAL(y)[i]))
			errorcall(call, "missing y values\n");
		if(i > 0 && REAL(y)[i] < REAL(y)[i-1])
			errorcall(call, "increasing y values expected\n");
	}

	xtol = 1e-3 * fabs(REAL(x)[nx-1]-REAL(x)[0]);
	ytol = 1e-3 * fabs(REAL(y)[ny-1]-REAL(y)[0]);

	for(i=0 ; i<nc ; i++)
		if(!FINITE(REAL(c)[i]))
			errorcall(call, "illegal NA contour values\n");

	zmin = DBL_MAX;
	zmax = DBL_MIN;
	for(i=0 ; i<nx*ny ; i++)
		if(FINITE(REAL(z)[i])) {
			if(zmax < REAL(z)[i]) zmax =  REAL(z)[i];
			if(zmin > REAL(z)[i]) zmin =  REAL(z)[i];
		}

	if(zmin >= zmax) {
		if(zmin == zmax)
			warning("all z values are equal\n");
		else
			warning("all z values are NA\n");
		return R_NilValue;
	}

	/*
	atom = DBL_EPSILON * (zmax - zmin);
	*/
	atom = 1e-3 * (zmax - zmin);

		/* Initialize the segment data base */
		/* Note we must be careful about resetting */
		/* the top of the stack, otherwise we run out of */
		/* memory after a sequence of displaylist replays */

	vmax0 = vmaxget();
	SegDB = (SEGP*)R_alloc(nx*ny, sizeof(SEGP));

	for(i=0 ; i<nx ; i++)
		for(j=0 ; j<ny ; j++)
			SegDB[i+j*nx] = NULL;

		/* Draw the contours -- note the heap release */

	ltysave = dd->gp.lty;
	colsave = dd->gp.col;
	for(i=0 ; i<nc ; i++) {
		vmax = vmaxget();
		dd->gp.lty = INTEGER(lty)[i%nlty];
		if (dd->gp.lty == NA_INTEGER) 
			dd->gp.lty = ltysave;
		dd->gp.col = INTEGER(col)[i%ncol];
		if (dd->gp.col == NA_INTEGER) 
			dd->gp.col = colsave;
		contour(x, nx, y, ny, z, REAL(c)[i], atom, dd);
		vmaxset(vmax);
	}
	vmaxset(vmax0);
	dd->gp.lty = ltysave;
	dd->gp.col = colsave;
	UNPROTECT(2);
                /* NOTE that i only record operation if no "error"  */
                /* NOTE that if we're replaying then call == R_NilValue */
        if (call != R_NilValue)
                recordGraphicOperation(op, oargs, dd);
	return R_NilValue;
}


SEXP do_image(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP oargs, sx, sy, sz, szlim, sc;
	double *x, *y, *z;
	unsigned *c;
	double xlow, xhigh, ylow, yhigh, zmin, zmax;
	int i, j, nx, ny, nz, ic, nc, colsave, xpdsave;
	DevDesc *dd = CurrentDevice();

	GCheckState(dd);

	checkArity(op,args);
	oargs = args;

	sx = CAR(args);
	internalTypeCheck(call, sx, REALSXP);
	nx = LENGTH(sx);
	args = CDR(args);

	sy = CAR(args);
	internalTypeCheck(call, sy, REALSXP);
	ny = LENGTH(sy);
	args = CDR(args);

	sz = CAR(args);
	internalTypeCheck(call, sz, REALSXP);
	nz = length(sz);
	args = CDR(args);

	szlim = CAR(args);
	internalTypeCheck(call, szlim, REALSXP);
	if(length(szlim) != 2 ||
		!FINITE(REAL(szlim)[0]) ||
		!FINITE(REAL(szlim)[1]) ||
		REAL(szlim)[0] >= REAL(szlim)[1])
			errorcall(call, "invalid z limits\n");
	zmin = REAL(szlim)[0];
	zmax = REAL(szlim)[1];
	args = CDR(args);

	PROTECT(sc = FixupCol(CAR(args), dd));
	nc = length(sc);

		/* Shorthand Pointers */

	x = REAL(sx);
	y = REAL(sy);
	z = REAL(sz);
	c = (unsigned*)INTEGER(sc);

		/* Check of grid coordinates */
		/* We want them to all be finite and */
		/* in strictly ascending order */

	if(nx < 2 || ny < 2) goto badxy;
	if(!FINITE(x[0])) goto badxy;
	if(!FINITE(y[0])) goto badxy;
	for(i=1 ; i<nx ; i++)
		if(!FINITE(x[i]) || x[i] <= x[i-1]) goto badxy;
	for(j=1 ; j<ny ; j++)
		if(!FINITE(y[j]) || y[j] <= y[j-1]) goto badxy;

	colsave = dd->gp.col;
	xpdsave = dd->gp.xpd;
	dd->gp.xpd = 0;

	GMode(dd, 1);

	for(i=0 ; i<nx ; i++) {
		if(i == 0)
			xlow = x[0];
		else
			xlow = 0.5 * (x[i] + x[i-1]);
		if(i == nx-1)
			xhigh = x[nx-1];
		else
			xhigh = 0.5 * (x[i] + x[i+1]);

		for(j=0 ; j<ny ; j++) {
			if(FINITE(z[i+j*nx])) {
				ic = floor((nc - 1) * (z[i+j*nx]-zmin)/(zmax - zmin) + 0.5);
				if(ic >= 0 && ic < nc) {
					if(j == 0)
						ylow = y[0];
					else
						ylow = 0.5 * (y[j] + y[j-1]);
					if(j == ny-1)
						yhigh = y[ny-1];
					else
						yhigh = 0.5 * (y[j] + y[j+1]);
					GRect(xlow, ylow, xhigh, yhigh, 
					      USER, c[ic], NA_INTEGER, dd);
				}
			}
		}
	}
	GMode(dd, 0);
	dd->gp.col = colsave;
	dd->gp.xpd = xpdsave;
	R_Visible = 0;
	UNPROTECT(1);
	if (call != R_NilValue)
		recordGraphicOperation(op, oargs, dd);
	return R_NilValue;

badxy:
	errorcall(call, "invalid x / y limits\n");
}

