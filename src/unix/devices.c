/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998	Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2000   The R Development Core Team.
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

#ifdef OLD
SEXP do_X11(SEXP call, SEXP op, SEXP args, SEXP env)
{
    char *display, *vmax, *cname, *devname;
    double height, width, ps, gamma;
    int colormodel, maxcubesize, canvascolor;
    SEXP sc;

    gcall = call;
    vmax = vmaxget();

    /* Decode the arguments */
    display = SaveString(CAR(args), 0); args = CDR(args);
    width = asReal(CAR(args));	args = CDR(args);
    height = asReal(CAR(args)); args = CDR(args);
    if (width <= 0 || height <= 0)
	errorcall(call, "invalid width or height");
    ps = asReal(CAR(args)); args = CDR(args);
    gamma = asReal(CAR(args)); args = CDR(args);
    if (gamma < 0 || gamma > 100)
	errorcall(call, "invalid gamma value");

    if (!isValidString(CAR(args)))
	error("invalid colortype passed to X11 driver");
    cname = CHAR(STRING_ELT(CAR(args), 0));
    if (strcmp(cname, "mono") == 0)
	colormodel = 0;
    else if (strcmp(cname, "gray") == 0 || strcmp(cname, "grey") == 0)
	colormodel = 1;
    else if (strcmp(cname, "pseudo.cube") == 0)
	colormodel = 2;
    else if (strcmp(cname, "pseudo") == 0)
	colormodel = 3;
    else if (strcmp(cname, "true") == 0)
	colormodel = 4;
    else {
	warningcall(call, 
		    "unknown X11 color/colour model -- using monochrome");
	colormodel = 0;
    }
    args = CDR(args);
    maxcubesize = asInteger(CAR(args));
    if (maxcubesize < 1 || maxcubesize > 256)
        maxcubesize = 256;
    args = CDR(args);
    sc = CAR(args);
    if (!isString(sc) && !isInteger(sc) && !isLogical(sc) && !isReal(sc))
	errorcall(call, "invalid value of `canvas'");
    canvascolor = RGBpar(sc, 0);

    devname = "X11";
    if (!strncmp(display, "png::", 5)) devname = "PNG";
    else if (!strncmp(display, "jpeg::", 6)) devname = "JPEG";
    else if (!strcmp(display, "XImage")) devname = "XImage";

    Rf_addX11Device(display, width, height, ps, gamma, colormodel, 
		    maxcubesize, canvascolor, devname, ptr_X11DeviceDriver);
    vmaxset(vmax);
    return R_NilValue;
}


DevDesc*
Rf_addX11Device(char *display, double width, double height, double ps, 
		double gamma, int colormodel, int maxcubesize,
		int canvascolor,
		char *devname, X11DeviceDriverRoutine deviceDriverRoutine)
{
    DevDesc *dd = NULL;
    R_CheckDeviceAvailable();
    BEGIN_SUSPEND_INTERRUPTS {
	/* Allocate and initialize the device driver data */
	if (!(dd = (DevDesc*)calloc(1, sizeof(DevDesc))))
	    return 0;
	/* Do this for early redraw attempts */
	dd->displayList = R_NilValue;
	/* Make sure that this is initialised before a GC can occur.
	 * This (and displayList) get protected during GC
	 */
	dev->savedSnapshot = R_NilValue;
	GInit(&dd->dp);
	if (!deviceDriverRoutine || 
	    !(deviceDriverRoutine)(dd, display, width, height, ps, gamma, 
				   colormodel, maxcubesize, canvascolor)) {
	    free(dd);
	    errorcall(gcall, "unable to start device %s", devname);
       	}
	gsetVar(install(".Device"), mkString(devname), R_NilValue);
	addDevice(dd);
	initDisplayList(dd);
    } END_SUSPEND_INTERRUPTS;

    return(dd);
}
#endif


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

SEXP do_X11(SEXP call, SEXP op, SEXP args, SEXP env)
{
    char *display, *vmax, *cname, *devname;
    double height, width, ps, gamma;
    int colormodel, maxcubesize, canvascolor;
    SEXP sc;

    gcall = call;
    vmax = vmaxget();

    /* Decode the arguments */
    display = SaveString(CAR(args), 0); args = CDR(args);
    width = asReal(CAR(args));	args = CDR(args);
    height = asReal(CAR(args)); args = CDR(args);
    if (width <= 0 || height <= 0)
	errorcall(call, "invalid width or height");
    ps = asReal(CAR(args)); args = CDR(args);
    gamma = asReal(CAR(args)); args = CDR(args);
    if (gamma < 0 || gamma > 100)
	errorcall(call, "invalid gamma value");

    if (!isValidString(CAR(args)))
	error("invalid colortype passed to X11 driver");
    cname = CHAR(STRING_ELT(CAR(args), 0));
    if (strcmp(cname, "mono") == 0)
	colormodel = 0;
    else if (strcmp(cname, "gray") == 0 || strcmp(cname, "grey") == 0)
	colormodel = 1;
    else if (strcmp(cname, "pseudo.cube") == 0)
	colormodel = 2;
    else if (strcmp(cname, "pseudo") == 0)
	colormodel = 3;
    else if (strcmp(cname, "true") == 0)
	colormodel = 4;
    else {
	warningcall(call, 
		    "unknown X11 color/colour model -- using monochrome");
	colormodel = 0;
    }
    args = CDR(args);
    maxcubesize = asInteger(CAR(args));
    if (maxcubesize < 1 || maxcubesize > 256)
        maxcubesize = 256;
    args = CDR(args);
    sc = CAR(args);
    if (!isString(sc) && !isInteger(sc) && !isLogical(sc) && !isReal(sc))
	errorcall(call, "invalid value of `canvas'");
    canvascolor = RGBpar(sc, 0);

    devname = "X11";
    if (!strncmp(display, "png::", 5)) devname = "PNG";
    else if (!strncmp(display, "jpeg::", 6)) devname = "JPEG";
    else if (!strcmp(display, "XImage")) devname = "XImage";

    Rf_addX11Device(display, width, height, ps, gamma, colormodel, 
		    maxcubesize, canvascolor, devname, ptr_X11DeviceDriver);
    vmaxset(vmax);
    return R_NilValue;
}

DevDesc* 
Rf_addX11Device(char *display, double width, double height, double ps, 
		double gamma, int colormodel, int maxcubesize,
		int canvascolor,
		char *devname, X11DeviceDriverRoutine deviceDriverRoutine)
{
    NewDevDesc *dev = NULL;
    GEDevDesc *dd;
    R_CheckDeviceAvailable();
    BEGIN_SUSPEND_INTERRUPTS {
	/* Allocate and initialize the device driver data */
	if (!(dev = (NewDevDesc*)calloc(1, sizeof(NewDevDesc))))
	    return 0;
	/* Do this for early redraw attempts */
	dev->newDevStruct = 1;
	dev->displayList = R_NilValue;
	/* Make sure that this is initialised before a GC can occur.
	 * This (and displayList) get protected during GC
	 */
	dev->savedSnapshot = R_NilValue;
	/* Took out the GInit because MOST of it is setting up
	 * R base graphics parameters.  
	 * This is supposed to happen via addDevice now.
	 */
	if (!(ptr_X11DeviceDriver)((DevDesc*)(dev), display, width, height, ps, gamma, 
				      colormodel, maxcubesize, canvascolor)) {
	    free(dev);
	    errorcall(gcall, "unable to start device %s", devname);
       	}
	gsetVar(install(".Device"), mkString(devname), R_NilValue);
	dd = GEcreateDevDesc(dev);
	addDevice((DevDesc*) dd);
	GEinitDisplayList(dd);
    } END_SUSPEND_INTERRUPTS;
    
    return((DevDesc*) dd);
}

