/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998  Robert Gentleman and Ross Ihaka
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

static SEXP gcall;

static char *SaveString(SEXP sxp, int offset)
{
	/* Return a non-relocatable copy of a string */

	char *s;
	if(!isString(sxp) || length(sxp) <= offset)
		errorcall(gcall, "invalid string argument\n");
	s = R_alloc(strlen(CHAR(STRING(sxp)[offset]))+1, sizeof(char));
	strcpy(s, CHAR(STRING(sxp)[offset]));
	return s;
}

#ifdef NotUsed
static void DeviceUnavailable(char *dev)
{
	errorcall(gcall, "%s device is unavailable.\n", dev);
}
#endif

#ifdef Unix
#include "../unix/devX11.h"

SEXP do_X11(SEXP call, SEXP op, SEXP args, SEXP env)
{
	DevDesc *dd;
	char *display, *vmax;
	double height, width, ps;

	gcall = call;
	vmax = vmaxget();
	display = SaveString(CAR(args), 0);
	args = CDR(args);
	width = asReal(CAR(args));
	args = CDR(args);
	height = asReal(CAR(args));
	if (width <= 0 || height <= 0)
		errorcall(call, "invalid width or height");
	args = CDR(args);
	ps = asReal(CAR(args));

		/* Allocate and initialize the device driver data */

	if (!(dd = (DevDesc *) malloc(sizeof(DevDesc))))
		return 0;
	/* Do this for early redraw attempts */
	dd->displayList = R_NilValue;
        GInit(&dd->dp);
	if(!X11DeviceDriver(dd, display, width, height, ps)) {
		free(dd);
		errorcall(call, "unable to start device X11\n");
	}
        gsetVar(install(".Device"), mkString("X11"), R_NilValue);
        addDevice(dd);
        initDisplayList(dd);
	vmaxset(vmax);
	return R_NilValue;
}
#else
SEXP do_x11(SEXP call, SEXP op, SEXP args, SEXP env)
{
	DeviceUnavailable("X11");
}
#endif

        /*  PostScript Device Driver Parameters  */
        /*  file      = output filename          */
        /*  paper     = paper type               */
        /*  face      = typeface                 */
        /*  bg        = background color         */
        /*  fg        = foreground color         */
        /*  width     = width in inches          */
        /*  height    = height in inches	 */
        /*  landscape = landscape		 */
        /*  ps        = pointsize		 */

int PSDeviceDriver(DevDesc*, char*, char *, char*,
                char*, char*, double, double, double, double);

SEXP do_PS(SEXP call, SEXP op, SEXP args, SEXP env)
{
        DevDesc *dd;
	char *vmax;
        char *file, *paper, *face, *bg, *fg;
	int horizontal;
        double height, width, ps;

	gcall = call;
	vmax = vmaxget();

	file = SaveString(CAR(args), 0);
	args = CDR(args);
	paper = SaveString(CAR(args), 0);
	args = CDR(args);
	face = SaveString(CAR(args), 0);
	args = CDR(args);
	bg = SaveString(CAR(args), 0);
	args = CDR(args);
	fg = SaveString(CAR(args), 0);
	args = CDR(args);
	width = asReal(CAR(args));
	args = CDR(args);
	height = asReal(CAR(args));
	args = CDR(args);
	horizontal = asLogical(CAR(args));
	if(horizontal == NA_LOGICAL)
		horizontal = 1;
	args = CDR(args);
	ps = asReal(CAR(args));

	if (!(dd = (DevDesc *) malloc(sizeof(DevDesc))))
		return 0;
	/* Do this for early redraw attempts */
	dd->displayList = R_NilValue;
        GInit(&dd->dp);
	if(!PSDeviceDriver(dd, file, paper, face, bg, fg, width, height, (double)horizontal, ps)) {
		free(dd);
		errorcall(call, "unable to start device PostScript\n");
	}
        gsetVar(install(".Device"), mkString("postscript"), R_NilValue);
        addDevice(dd);
        initDisplayList(dd);
	vmaxset(vmax);
	return R_NilValue;
}

        /*  PicTeX Device Driver Parameters  */
        /*  file      = output filename          */
        /*  bg        = background color         */
        /*  fg        = foreground color         */
        /*  width     = width in inches          */
        /*  height    = height in inches	 */

int PicTeXDeviceDriver(DevDesc*, char*, char *, char*,
                       double, double, int);

SEXP do_PicTeX(SEXP call, SEXP op, SEXP args, SEXP env)
{
        DevDesc *dd;
	char *vmax;
        char *file, *bg, *fg;
        double height, width;
	int debug;

	gcall = call;
	vmax = vmaxget();

	file = SaveString(CAR(args), 0);
	args = CDR(args);
	bg = SaveString(CAR(args), 0);
	args = CDR(args);
	fg = SaveString(CAR(args), 0);
	args = CDR(args);
	width = asReal(CAR(args));
	args = CDR(args);
	height = asReal(CAR(args));
	args = CDR(args);
	debug = asInteger(CAR(args));
	args = CDR(args);

	if (!(dd = (DevDesc *) malloc(sizeof(DevDesc))))
		return 0;
	/* Do this for early redraw attempts */
	dd->displayList = R_NilValue;
        GInit(&dd->dp);
	if(!PicTeXDeviceDriver(dd, file, bg, fg, width, height, debug)) {
		free(dd);
		errorcall(call, "unable to start device PicTeX\n");
	}
        gsetVar(install(".Device"), mkString("pictex"), R_NilValue);
        addDevice(dd);
        initDisplayList(dd);
	vmaxset(vmax);
	return R_NilValue;
}

