/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998	Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2003   The R Development Core Team.
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

#include "../modules/X11/devX11.h"

/* Return a non-relocatable copy of a string */

static SEXP gcall;

static char *SaveString(SEXP sxp, int offset)
{
    char *s;
    if(!isString(sxp) || length(sxp) <= offset)
	errorcall(gcall, "invalid string argument");
    s = R_alloc(strlen(CHAR(STRING_ELT(sxp, offset)))+1, sizeof(char));
    strcpy(s, CHAR(STRING_ELT(sxp, offset)));
    return s;
}

#include "devUI.h"

/*  X11 Device Driver Parameters:
 *  -----------------		--> X11/devX11.c
 *  display	= display
 *  width	= width in inches
 *  height	= height in inches
 *  ps		= pointsize
 *  gamma       = gamma correction
 *  colormodel  = color model
 */


SEXP do_GTK(SEXP call, SEXP op, SEXP args, SEXP env)
{
    NewDevDesc *dev;
    GEDevDesc *dd;
    char *display, *vmax;
    double height, width, ps;
    gcall = call;
    vmax = vmaxget();
    display = SaveString(CAR(args), 0); args = CDR(args);
    width = asReal(CAR(args));	args = CDR(args);
    height = asReal(CAR(args)); args = CDR(args);
    if (width <= 0 || height <= 0)
	errorcall(call, "invalid width or height");
    ps = asReal(CAR(args));

    R_CheckDeviceAvailable();
    BEGIN_SUSPEND_INTERRUPTS {
	/* Allocate and initialize the device driver data */
	if (!(dev = (NewDevDesc *) calloc(1, sizeof(NewDevDesc))))
	    return 0;
	/* Do this for early redraw attempts */
	dev->displayList = R_NilValue;
	/* Make sure that this is initialised before a GC can occur.
	 * This (and displayList) get protected during GC
	 */
	dev->savedSnapshot = R_NilValue;
	if (!ptr_GTKDeviceDriver ((DevDesc*)dev, display, width, height, ps)) {
	    free(dev);
	    errorcall(call, "unable to start device gtk");
	}
	gsetVar(install(".Device"), mkString("GTK"), R_NilValue);
	dd = GEcreateDevDesc(dev);
        dd->newDevStruct = 1;
	addDevice((DevDesc*) dd);
	GEinitDisplayList(dd);
    } END_SUSPEND_INTERRUPTS;
    vmaxset(vmax);
    return R_NilValue;
}

SEXP do_Gnome(SEXP call, SEXP op, SEXP args, SEXP env)
{
    NewDevDesc *dev;
    GEDevDesc *dd;
    char *display, *vmax;
    double height, width, ps;
    gcall = call;
    vmax = vmaxget();
    display = SaveString(CAR(args), 0); args = CDR(args);
    width = asReal(CAR(args));	args = CDR(args);
    height = asReal(CAR(args)); args = CDR(args);
    if (width <= 0 || height <= 0)
	errorcall(call, "invalid width or height");
    ps = asReal(CAR(args));

    R_CheckDeviceAvailable();
    BEGIN_SUSPEND_INTERRUPTS {
	/* Allocate and initialize the device driver data */
	if (!(dev = (NewDevDesc *) calloc(1, sizeof(NewDevDesc))))
	    return 0;
	/* Do this for early redraw attempts */
	dev->displayList = R_NilValue;
	/* Make sure that this is initialised before a GC can occur.
	 * This (and displayList) get protected during GC
	 */
	dev->savedSnapshot = R_NilValue;
	if (!ptr_GnomeDeviceDriver((DevDesc*)dev, display, width, height, ps)){
	    free(dev);
	    errorcall(call, "unable to start device gtk");
	}
	gsetVar(install(".Device"), mkString("gnome"), R_NilValue);
	dd = GEcreateDevDesc(dev);
        dd->newDevStruct = 1;
	addDevice((DevDesc*) dd);
	GEinitDisplayList(dd);
    } END_SUSPEND_INTERRUPTS;
    vmaxset(vmax);
    return R_NilValue;
}
