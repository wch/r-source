/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-12   The R Core Team.
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
#include <config.h>
#endif

#include <Defn.h>
#include <Internal.h>
#include <R_ext/GraphicsEngine.h>
#include <R_ext/Applic.h>	/* pretty() */
#include <Rmath.h>

# include <rlocale.h>

int R_GE_getVersion()
{
    return R_GE_version;
}

void R_GE_checkVersionOrDie(int version)
{
    if (version != R_GE_version)
    error(_("Graphics API version mismatch"));
}

/* A note on memory management ...
 * Here (with GEDevDesc's) I have continued the deplorable tradition of
 * malloc'ing device structures and maintaining global variables to
 * record the device structures.  I believe that what I should
 * be doing is recording the device structures in R-level objects
 * (i.e., SEXP's) using Luke's reference pointers to make sure that
 * nasty things like duplicate copies of device structures do not
 * occur.  The thing stopping me doing "the right thing" right now
 * is time.  Hopefully, I will get time later to come back and do
 * it properly -- in the meantime I'll just have to burn in hell.
 * Paul.
 */

static int numGraphicsSystems = 0;

static GESystemDesc* registeredSystems[MAX_GRAPHICS_SYSTEMS];


/****************************************************************
 * GEdestroyDevDesc
 ****************************************************************
 */

static void unregisterOne(pGEDevDesc dd, int systemNumber) {
    if (dd->gesd[systemNumber] != NULL) {
	(dd->gesd[systemNumber]->callback)(GE_FinaliseState, dd, R_NilValue);
	free(dd->gesd[systemNumber]);
	dd->gesd[systemNumber] = NULL;
    }
}

/* NOTE that dd->dev has been shut down by a call
 * to dev->close within devices.c
 */
void GEdestroyDevDesc(pGEDevDesc dd)
{
    int i;
    if (dd != NULL) {
	for (i = 0; i < MAX_GRAPHICS_SYSTEMS; i++) unregisterOne(dd, i);
	free(dd->dev);
	dd->dev = NULL;
	free(dd);
    }
}

/****************************************************************
 * GEsystemState
 ****************************************************************

 Currently unused, but future systems might need it.
 */

void* GEsystemState(pGEDevDesc dd, int index)
{
    return dd->gesd[index]->systemSpecific;
}

/****************************************************************
 * GEregisterWithDevice
 ****************************************************************
 */

/* The guts of adding information about a specific graphics
 * system to a specific device.
 */
static void registerOne(pGEDevDesc dd, int systemNumber, GEcallback cb) {
    SEXP result;
    dd->gesd[systemNumber] =
	(GESystemDesc*) calloc(1, sizeof(GESystemDesc));
    if (dd->gesd[systemNumber] == NULL)
	error(_("unable to allocate memory (in GEregister)"));
    result = cb(GE_InitState, dd, R_NilValue);
    if (isNull(result)) {
        /* tidy up */
        free(dd->gesd[systemNumber]);
	error(_("unable to allocate memory (in GEregister)"));
    } else {
        dd->gesd[systemNumber]->callback = cb;
    }
}

/* Store the graphics system state and callback information
 * for a specified device.
 * This is called when a new device is created.
 */
void GEregisterWithDevice(pGEDevDesc dd) {
    int i;
    for (i = 0; i < MAX_GRAPHICS_SYSTEMS; i++)
	/* If a graphics system has unregistered, there might be
	 * "holes" in the array of registeredSystems.
	 */
	if (registeredSystems[i] != NULL)
	    registerOne(dd, i, registeredSystems[i]->callback);
}

/****************************************************************
 * GEregisterSystem
 ****************************************************************
 */

/* Record the state and callback information for a new graphics
 * system.
 * This is called when a graphics system is loaded.
 * Return the index of the system's information in the graphic
 * engine's register.
 */
void GEregisterSystem(GEcallback cb, int *systemRegisterIndex) {
    int i, devNum;
    pGEDevDesc gdd;
    if (numGraphicsSystems + 1 == MAX_GRAPHICS_SYSTEMS)
	error(_("too many graphics systems registered"));
    /* Set the system register index so that, if there are existing
     * devices, it will know where to put the system-specific
     * information in those devices
     * If a graphics system has been unregistered, there might
     * be "holes" in the list of graphics systems, so start
     * from zero and look for the first NULL 
     */
    *systemRegisterIndex = 0;
    while (registeredSystems[*systemRegisterIndex] != NULL) {
        (*systemRegisterIndex)++;
    }
    /* Run through the existing devices and add the new information
     * to any GEDevDesc's
     */
    i = 1;
    if (!NoDevices()) {
	devNum = curDevice();
	while (i++ < NumDevices()) {
	    gdd = GEgetDevice(devNum);
	    registerOne(gdd, *systemRegisterIndex, cb);
	    devNum = nextDevice(devNum);
	}
    }
    /* Store the information for adding to any new devices
     */
    registeredSystems[*systemRegisterIndex] =
	(GESystemDesc*) calloc(1, sizeof(GESystemDesc));
    if (registeredSystems[*systemRegisterIndex] == NULL)
	error(_("unable to allocate memory (in GEregister)"));
    registeredSystems[*systemRegisterIndex]->callback = cb;
    numGraphicsSystems += 1;
}

/****************************************************************
 * GEunregisterSystem
 ****************************************************************
 */

void GEunregisterSystem(int registerIndex)
{
    int i, devNum;
    pGEDevDesc gdd;

    /* safety check if called before Ginit() */
    if(registerIndex < 0) return;
    if (numGraphicsSystems == 0) {
	/* This gets called from KillAllDevices, which is called
	   during shutdown.  Prior to 2.14.0 it gave an error, which
	   would inhibit shutdown.  This should not happen, but
	   apparently it did after a segfault:
	   https://stat.ethz.ch/pipermail/r-devel/2011-June/061153.html
	*/
	warning(_("no graphics system to unregister"));
	return;
    }
    /* Run through the existing devices and remove the information
     * from any GEDevDesc's
     */
    i = 1;
    if (!NoDevices()) {
	devNum = curDevice();
	while (i++ < NumDevices()) {
	    gdd = GEgetDevice(devNum);
	    unregisterOne(gdd, registerIndex);
	    devNum = nextDevice(devNum);
	}
    }
    /* Remove the information from the global record
     * NOTE that there is no systemSpecific information stored
     * in the global record -- just the system callback pointer.
     */
    if (registeredSystems[registerIndex] != NULL) {
	free(registeredSystems[registerIndex]);
	registeredSystems[registerIndex] = NULL;
    }
    numGraphicsSystems -= 1;
}

/****************************************************************
 * GEhandleEvent
 ****************************************************************
 */

/* This guy can be called by device drivers.
 * It calls back to registered graphics systems and passes on the event
 * so that the graphics systems can respond however they want to.
 *
 * Currently only used for GE_ScalePS in devWindows.c
 */
SEXP GEhandleEvent(GEevent event, pDevDesc dev, SEXP data)
{
    int i;
    pGEDevDesc gdd = desc2GEDesc(dev);
    for (i = 0; i < MAX_GRAPHICS_SYSTEMS; i++)
	if (registeredSystems[i] != NULL)
	    (registeredSystems[i]->callback)(event, gdd, data);
    return R_NilValue;
}

/****************************************************************
 * Some graphics engine transformations
 ****************************************************************
 */

double fromDeviceX(double value, GEUnit to, pGEDevDesc dd)
{
    double result = value;
    switch (to) {
    case GE_DEVICE:
	break;
    case GE_NDC:
	result = (result - dd->dev->left) / (dd->dev->right - dd->dev->left);
	break;
    case GE_INCHES:
	result = (result - dd->dev->left) / (dd->dev->right - dd->dev->left) *
	    fabs(dd->dev->right - dd->dev->left) * dd->dev->ipr[0];
	break;
    case GE_CM:
	result = (result - dd->dev->left) / (dd->dev->right - dd->dev->left) *
	    fabs(dd->dev->right - dd->dev->left) * dd->dev->ipr[0] * 2.54;
    }
    return result;
}

double toDeviceX(double value, GEUnit from, pGEDevDesc dd)
{
    double result = value;
    switch (from) {
    case GE_CM:
	/* Convert GE_CM to GE_INCHES */
	result = result / 2.54;
    case GE_INCHES:
	/* Convert GE_INCHES to GE_NDC */
	result = (result / dd->dev->ipr[0]) / fabs(dd->dev->right - dd->dev->left);
    case GE_NDC:
	/* Convert GE_NDC to Dev */
	result = dd->dev->left + result*(dd->dev->right - dd->dev->left);
    case GE_DEVICE:
	/* Do nothing */
	break;
    }
    return result;
}

double fromDeviceY(double value, GEUnit to, pGEDevDesc dd)
{
    double result = value;
    switch (to) {
    case GE_DEVICE:
	break;
    case GE_NDC:
	result = (result - dd->dev->bottom) / (dd->dev->top - dd->dev->bottom);
	break;
    case GE_INCHES:
	result = (result - dd->dev->bottom) / (dd->dev->top - dd->dev->bottom) *
	    fabs(dd->dev->top - dd->dev->bottom) * dd->dev->ipr[1];
	break;
    case GE_CM:
	result = (result - dd->dev->bottom) / (dd->dev->top - dd->dev->bottom) *
	    fabs(dd->dev->top - dd->dev->bottom) * dd->dev->ipr[1] * 2.54;
    }
    return result;
}

double toDeviceY(double value, GEUnit from, pGEDevDesc dd)
{
    double result = value;
    switch (from) {
    case GE_CM:
	/* Convert GE_CM to GE_INCHES */
	result = result / 2.54;
    case GE_INCHES:
	/* Convert GE_INCHES to GE_NDC */
	result = (result / dd->dev->ipr[1]) / fabs(dd->dev->top - dd->dev->bottom);
    case GE_NDC:
	/* Convert GE_NDC to Dev */
	result = dd->dev->bottom + result*(dd->dev->top - dd->dev->bottom);
    case GE_DEVICE:
	/* Do nothing */
	break;
    }
    return result;
}

double fromDeviceWidth(double value, GEUnit to, pGEDevDesc dd)
{
    double result = value;
    switch (to) {
    case GE_DEVICE:
	break;
    case GE_NDC:
	result = result / (dd->dev->right - dd->dev->left);
	break;
    case GE_INCHES:
	result = result * dd->dev->ipr[0];
	break;
    case GE_CM:
	result = result * dd->dev->ipr[0] * 2.54;
    }
    return result;
}

double toDeviceWidth(double value, GEUnit from, pGEDevDesc dd)
{
    double result = value;
    switch (from) {
    case GE_CM:
	/* Convert GE_CM to GE_INCHES */
	result = result / 2.54;
    case GE_INCHES:
	/* Convert GE_INCHES to GE_NDC */
	result = (result / dd->dev->ipr[0]) / fabs(dd->dev->right - dd->dev->left);
    case GE_NDC:
	/* Convert GE_NDC to Dev */
	result = result*(dd->dev->right - dd->dev->left);
    case GE_DEVICE:
	/* Do nothing */
	break;
    }
    return result;
}

double fromDeviceHeight(double value, GEUnit to, pGEDevDesc dd)
{
    double result = value;
    switch (to) {
    case GE_DEVICE:
	break;
    case GE_NDC:
	result = result / (dd->dev->top - dd->dev->bottom);
	break;
    case GE_INCHES:
	result = result * dd->dev->ipr[1];
	break;
    case GE_CM:
	result = result * dd->dev->ipr[1] * 2.54;
    }
    return result;
}

double toDeviceHeight(double value, GEUnit from, pGEDevDesc dd)
{
    double result = value;
    switch (from) {
    case GE_CM:
	/* Convert GE_CM to GE_INCHES */
	result = result / 2.54;
    case GE_INCHES:
	/* Convert GE_INCHES to GE_NDC */
	result = (result / dd->dev->ipr[1]) / fabs(dd->dev->top - dd->dev->bottom);
    case GE_NDC:
	/* Convert GE_NDC to Dev */
	result = result*(dd->dev->top - dd->dev->bottom);
    case GE_DEVICE:
	/* Do nothing */
	break;
    }
    return result;
}

/****************************************************************
 * Code for converting line ends and joins from SEXP to internal
 * representation
 ****************************************************************
 */
typedef struct {
    char *name;
    R_GE_lineend end;
} LineEND;

static LineEND lineend[] = {
    { "round",   GE_ROUND_CAP  },
    { "butt",	 GE_BUTT_CAP   },
    { "square",	 GE_SQUARE_CAP },
    { NULL,	 0	     }
};

static int nlineend = (sizeof(lineend)/sizeof(LineEND)-2);

R_GE_lineend GE_LENDpar(SEXP value, int ind)
{
    int i, code;
    double rcode;

    if(isString(value)) {
	for(i = 0; lineend[i].name; i++) { /* is it the i-th name ? */
	    if(!strcmp(CHAR(STRING_ELT(value, ind)), lineend[i].name)) /*ASCII */
		return lineend[i].end;
	}
	error(_("invalid line end")); /*NOTREACHED, for -Wall : */ return 0;
    }
    else if(isInteger(value)) {
	code = INTEGER(value)[ind];
	if(code == NA_INTEGER || code < 0)
	    error(_("invalid line end"));
	if (code > 0)
	    code = (code-1) % nlineend + 1;
	return lineend[code].end;
    }
    else if(isReal(value)) {
	rcode = REAL(value)[ind];
	if(!R_FINITE(rcode) || rcode < 0)
	    error(_("invalid line end"));
	code = (int) rcode;
	if (code > 0)
	    code = (code-1) % nlineend + 1;
	return lineend[code].end;
    }
    else {
	error(_("invalid line end")); /*NOTREACHED, for -Wall : */ return 0;
    }
}

SEXP GE_LENDget(R_GE_lineend lend)
{
    SEXP ans = R_NilValue;
    int i;

    for (i = 0; lineend[i].name; i++) {
	if(lineend[i].end == lend)
	    return mkString(lineend[i].name);
    }

    error(_("invalid line end"));
    /*
     * Should never get here
     */
    return ans;
}

typedef struct {
    char *name;
    R_GE_linejoin join;
} LineJOIN;

static LineJOIN linejoin[] = {
    { "round",   GE_ROUND_JOIN },
    { "mitre",	 GE_MITRE_JOIN },
    { "bevel",	 GE_BEVEL_JOIN},
    { NULL,	 0	     }
};

static int nlinejoin = (sizeof(linejoin)/sizeof(LineJOIN)-2);

R_GE_linejoin GE_LJOINpar(SEXP value, int ind)
{
    int i, code;
    double rcode;

    if(isString(value)) {
	for(i = 0; linejoin[i].name; i++) { /* is it the i-th name ? */
	    if(!strcmp(CHAR(STRING_ELT(value, ind)), linejoin[i].name)) /* ASCII */
		return linejoin[i].join;
	}
	error(_("invalid line join")); /*NOTREACHED, for -Wall : */ return 0;
    }
    else if(isInteger(value)) {
	code = INTEGER(value)[ind];
	if(code == NA_INTEGER || code < 0)
	    error(_("invalid line join"));
	if (code > 0)
	    code = (code-1) % nlinejoin + 1;
	return linejoin[code].join;
    }
    else if(isReal(value)) {
	rcode = REAL(value)[ind];
	if(!R_FINITE(rcode) || rcode < 0)
	    error(_("invalid line join"));
	code = (int) rcode;
	if (code > 0)
	    code = (code-1) % nlinejoin + 1;
	return linejoin[code].join;
    }
    else {
	error(_("invalid line join")); /*NOTREACHED, for -Wall : */ return 0;
    }
}

SEXP GE_LJOINget(R_GE_linejoin ljoin)
{
    SEXP ans = R_NilValue;
    int i;

    for (i = 0; linejoin[i].name; i++) {
	if(linejoin[i].join == ljoin)
	    return mkString(linejoin[i].name);
    }

    error(_("invalid line join"));
    /*
     * Should never get here
     */
    return ans;
}

/****************************************************************
 * Code to retrieve current clipping rect from device
 ****************************************************************
 */

static void getClipRect(double *x1, double *y1, double *x2, double *y2,
			pGEDevDesc dd)
{
    /* Since these are only set by GESetClip they should be in order */
    if (dd->dev->clipLeft < dd->dev->clipRight) {
	*x1 = dd->dev->clipLeft;
	*x2 = dd->dev->clipRight;
    } else {
	*x2 = dd->dev->clipLeft;
	*x1 = dd->dev->clipRight;
    }
    if (dd->dev->clipBottom < dd->dev->clipTop) {
	*y1 = dd->dev->clipBottom;
	*y2 = dd->dev->clipTop;
    } else {
	*y2 = dd->dev->clipBottom;
	*y1 = dd->dev->clipTop;
    }
}

static void getClipRectToDevice(double *x1, double *y1, double *x2, double *y2,
				pGEDevDesc dd)
{
    /* Devices can have flipped coord systems */
    if (dd->dev->left < dd->dev->right) {
	*x1 = dd->dev->left;
	*x2 = dd->dev->right;
    } else {
	*x2 = dd->dev->left;
	*x1 = dd->dev->right;
    }
    if (dd->dev->bottom < dd->dev->top) {
	*y1 = dd->dev->bottom;
	*y2 = dd->dev->top;
    } else {
	*y2 = dd->dev->bottom;
	*y1 = dd->dev->top;
    }
}

/****************************************************************
 * GESetClip
 ****************************************************************
 */
void GESetClip(double x1, double y1, double x2, double y2, pGEDevDesc dd)
{
    pDevDesc d = dd->dev;
    double dx1 = d->left, dx2 = d->right, dy1 = d->bottom, dy2 = d->top;

    /* clip to device region */
    if (dx1 <= dx2) {
	x1 = fmax2(x1, dx1);
	x2 = fmin2(x2, dx2);
    } else {
	x1 = fmin2(x1, dx1);
	x2 = fmax2(x2, dx2);
    }
    if (dy1 <= dy2) {
	y1 = fmax2(y1, dy1);
	y2 = fmin2(y2, dy2);
    } else {
	y1 = fmin2(y1, dy1);
	y2 = fmax2(y2, dy2);
    }
    d->clip(x1, x2, y1, y2, dd->dev);
    /*
     * Record the current clip rect settings so that calls to
     * getClipRect get the up-to-date values.
     */
    d->clipLeft = fmin2(x1, x2);
    d->clipRight = fmax2(x1, x2);
    d->clipTop = fmax2(y1, y2);
    d->clipBottom = fmin2(y1, y2);
}

/****************************************************************
 * R code for clipping lines
 ****************************************************************
 */

/* Draw Line Segments, Clipping to the Viewport */
/* Cohen-Sutherland Algorithm */
/* Unneeded if the device can do the clipping */


#define	CS_BOTTOM	001
#define	CS_LEFT		002
#define	CS_TOP		004
#define	CS_RIGHT	010

typedef struct {
    double xl;
    double xr;
    double yb;
    double yt;
} cliprect;


static int clipcode(double x, double y, cliprect *cr)
{
    int c = 0;
    if(x < cr->xl)
	c |= CS_LEFT;
    else if(x > cr->xr)
	c |= CS_RIGHT;
    if(y < cr->yb)
	c |= CS_BOTTOM;
    else if(y > cr->yt)
	c |= CS_TOP;
    return c;
}

static Rboolean
CSclipline(double *x1, double *y1, double *x2, double *y2,
	   cliprect *cr, int *clipped1, int *clipped2,
	   pGEDevDesc dd)
{
    int c, c1, c2;
    double x, y, xl, xr, yb, yt;

    *clipped1 = 0;
    *clipped2 = 0;
    c1 = clipcode(*x1, *y1, cr);
    c2 = clipcode(*x2, *y2, cr);
    if ( !c1 && !c2 )
	return TRUE;

    xl = cr->xl;
    xr = cr->xr;
    yb = cr->yb;
    yt = cr->yt;
    /* Paul took out the code for (dd->dev->gp.xlog || dd->dev->gp.ylog)
     * (i) because device holds no state on whether scales are logged
     * (ii) it appears to be identical to the code for non-log scales !?
     */
    x = xl;		/* keep -Wall happy */
    y = yb;		/* keep -Wall happy */
    while( c1 || c2 ) {
	if(c1 & c2)
	    return FALSE;
	if( c1 )
	    c = c1;
	else
	    c = c2;
	if( c & CS_LEFT ) {
	    y = *y1 + (*y2 - *y1) * (xl - *x1) / (*x2 - *x1);
	    x = xl;
	}
	else if( c & CS_RIGHT ) {
	    y = *y1 + (*y2 - *y1) * (xr - *x1) / (*x2 -  *x1);
	    x = xr;
	}
	else if( c & CS_BOTTOM ) {
	    x = *x1 + (*x2 - *x1) * (yb - *y1) / (*y2 - *y1);
	    y = yb;
	}
	else if( c & CS_TOP ) {
	    x = *x1 + (*x2 - *x1) * (yt - *y1)/(*y2 - *y1);
	    y = yt;
	}

	if( c==c1 ) {
	    *x1 = x;
	    *y1 = y;
	    *clipped1 = 1;
	    c1 = clipcode(x, y, cr);
	}
	else {
	    *x2 = x;
	    *y2 = y;
	    *clipped2 = 1;
	    c2 = clipcode(x, y, cr);
	}
    }
    return TRUE;
}


/* Clip the line
   If toDevice = 1, clip to the device extent (i.e., temporarily ignore
   dd->dev->gp.xpd) */
static Rboolean
clipLine(double *x1, double *y1, double *x2, double *y2,
	 int toDevice, pGEDevDesc dd)
{
    int dummy1, dummy2;
    cliprect cr;

    if (toDevice)
	getClipRectToDevice(&cr.xl, &cr.yb, &cr.xr, &cr.yt, dd);
    else
	getClipRect(&cr.xl, &cr.yb, &cr.xr, &cr.yt, dd);

    return CSclipline(x1, y1, x2, y2, &cr, &dummy1, &dummy2, dd);
}

/****************************************************************
 * GELine
 ****************************************************************
 */
/* If the device canClip, R clips line to device extent and
   device does all other clipping. */
void GELine(double x1, double y1, double x2, double y2,
	    const pGEcontext gc, pGEDevDesc dd)
{
    Rboolean clip_ok;
    if (gc->lwd == R_PosInf || gc->lwd < 0.0)
	error(_("'lwd' must be non-negative and finite"));
    if (ISNAN(gc->lwd) || gc->lty == LTY_BLANK) return;
    if (dd->dev->canClip) {
	clip_ok = clipLine(&x1, &y1, &x2, &y2, 1, dd);
    }
    else {
	clip_ok = clipLine(&x1, &y1, &x2, &y2, 0, dd);
    }
    if (clip_ok)
	dd->dev->line(x1, y1, x2, y2, gc, dd->dev);
}

/****************************************************************
 * R code for clipping polylines
 ****************************************************************
 */

static void CScliplines(int n, double *x, double *y,
			const pGEcontext gc, int toDevice, pGEDevDesc dd)
{
    int ind1, ind2;
    /*int firstPoint = 1;*/
    int count = 0;
    int i = 0;
    double *xx, *yy;
    double x1, y1, x2, y2;
    cliprect cr;
    const void *vmax = vmaxget();

    if (toDevice)
	getClipRectToDevice(&cr.xl, &cr.yb, &cr.xr, &cr.yt, dd);
    else
	getClipRect(&cr.xl, &cr.yb, &cr.xr, &cr.yt, dd);

    xx = (double *) R_alloc(n, sizeof(double));
    yy = (double *) R_alloc(n, sizeof(double));
    if (xx == NULL || yy == NULL)
	error(_("out of memory while clipping polyline"));

    xx[0] = x1 = x[0];
    yy[0] = y1 = y[0];
    count = 1;

    for (i = 1; i < n; i++) {
	x2 = x[i];
	y2 = y[i];
	if (CSclipline(&x1, &y1, &x2, &y2, &cr, &ind1, &ind2, dd)) {
	    if (ind1 && ind2) {
		xx[0] = x1;
		yy[0] = y1;
		xx[1] = x2;
		yy[1] = y2;
		dd->dev->polyline(2, xx, yy, gc, dd->dev);
	    }
	    else if (ind1) {
		xx[0] = x1;
		yy[0] = y1;
		xx[1] = x2;
		yy[1] = y2;
		count = 2;
		if (i == n - 1)
		    dd->dev->polyline(count, xx, yy, gc, dd->dev);
	    }
	    else if (ind2) {
		xx[count] = x2;
		yy[count] = y2;
		count++;
		if (count > 1)
		    dd->dev->polyline(count, xx, yy, gc, dd->dev);
	    }
	    else {
		xx[count] = x2;
		yy[count] = y2;
		count++;
		if (i == n - 1 && count > 1)
		    dd->dev->polyline(count, xx, yy, gc, dd->dev);
	    }
	}
	x1 = x[i];
	y1 = y[i];
    }

    vmaxset(vmax);
}

/****************************************************************
 * GEPolyline
 ****************************************************************
 */
/* Clip and draw the polyline.
   If clipToDevice = 0, clip according to dd->dev->gp.xpd
   If clipToDevice = 1, clip to the device extent */
static void clipPolyline(int n, double *x, double *y,
			 const pGEcontext gc, int clipToDevice, pGEDevDesc dd)
{
    CScliplines(n, x, y, gc, clipToDevice, dd);
}

/* Draw a series of line segments. */
/* If the device canClip, R clips to the device extent and the device
   does all other clipping */
void GEPolyline(int n, double *x, double *y, const pGEcontext gc, pGEDevDesc dd)
{
    if (gc->lwd == R_PosInf || gc->lwd < 0.0)
	error(_("'lwd' must be non-negative and finite"));
    if (ISNAN(gc->lwd) || gc->lty == LTY_BLANK) return;
    if (dd->dev->canClip) {
	clipPolyline(n, x, y, gc, 1, dd);  /* clips to device extent
						  then draws */
    }
    else
	clipPolyline(n, x, y, gc, 0, dd);
}

/****************************************************************
 * R code for clipping polygons
 ****************************************************************
 */

typedef enum {
    Left = 0,
    Right = 1,
    Bottom = 2,
    Top = 3
} Edge;

/* Clipper State Variables */
typedef struct {
    int first;    /* true if we have seen the first point */
    double fx;    /* x coord of the first point */
    double fy;    /* y coord of the first point */
    double sx;    /* x coord of the most recent point */
    double sy;    /* y coord of the most recent point */
}
GClipState;

/* The Clipping Rectangle */
typedef struct {
    double xmin;
    double xmax;
    double ymin;
    double ymax;
}
GClipRect;

static
int inside (Edge b, double px, double py, GClipRect *clip)
{
    switch (b) {
    case Left:   if (px < clip->xmin) return 0; break;
    case Right:  if (px > clip->xmax) return 0; break;
    case Bottom: if (py < clip->ymin) return 0; break;
    case Top:    if (py > clip->ymax) return 0; break;
    }
    return 1;
}

static
int cross (Edge b, double x1, double y1, double x2, double y2,
	   GClipRect *clip)
{
    if (inside (b, x1, y1, clip) == inside (b, x2, y2, clip))
	return 0;
    else return 1;
}

static
void intersect (Edge b, double x1, double y1, double x2, double y2,
		double *ix, double *iy, GClipRect *clip)
{
    double m = 0;

    if (x1 != x2) m = (y1 - y2) / (x1 - x2);
    switch (b) {
    case Left:
	*ix = clip->xmin;
	*iy = y2 + (clip->xmin - x2) * m;
	break;
    case Right:
	*ix = clip->xmax;
	*iy = y2 + (clip->xmax - x2) * m;
	break;
    case Bottom:
	*iy = clip->ymin;
	if (x1 != x2) *ix = x2 + (clip->ymin - y2) / m;
	else *ix = x2;
	break;
    case Top:
	*iy = clip->ymax;
	if (x1 != x2) *ix = x2 + (clip->ymax - y2) / m;
	else *ix = x2;
	break;
    }
}

static
void clipPoint (Edge b, double x, double y,
		double *xout, double *yout, int *cnt, int store,
		GClipRect *clip, GClipState *cs)
{
    double ix = 0.0, iy = 0.0 /* -Wall */;

    if (!cs[b].first) {
	/* No previous point exists for this edge. */
	/* Save this point. */
	cs[b].first = 1;
	cs[b].fx = x;
	cs[b].fy = y;
    }
    else
	/* A previous point exists.  */
	/* If 'p' and previous point cross edge, find intersection.  */
	/* Clip against next boundary, if any.  */
	/* If no more edges, add intersection to output list. */
	if (cross (b, x, y, cs[b].sx, cs[b].sy, clip)) {
	    intersect (b, x, y, cs[b].sx, cs[b].sy, &ix, &iy, clip);
	    if (b < Top)
		clipPoint (b + 1, ix, iy, xout, yout, cnt, store,
			   clip, cs);
	    else {
		if (store) {
		    xout[*cnt] = ix;
		    yout[*cnt] = iy;
		}
		(*cnt)++;
	    }
	}

    /* Save as most recent point for this edge */
    cs[b].sx = x;
    cs[b].sy = y;

    /* For all, if point is 'inside' */
    /* proceed to next clip edge, if any */
    if (inside (b, x, y, clip)) {
	if (b < Top)
	    clipPoint (b + 1, x, y, xout, yout, cnt, store, clip, cs);
	else {
	    if (store) {
		xout[*cnt] = x;
		yout[*cnt] = y;
	    }
	    (*cnt)++;
	}
    }
}

static
void closeClip (double *xout, double *yout, int *cnt, int store,
		GClipRect *clip, GClipState *cs)
{
    double ix = 0.0, iy = 0.0 /* -Wall */;
    Edge b;

    for (b = Left; b <= Top; b++) {
	if (cross (b, cs[b].sx, cs[b].sy, cs[b].fx, cs[b].fy, clip)) {
	    intersect (b, cs[b].sx, cs[b].sy,
		       cs[b].fx, cs[b].fy, &ix, &iy, clip);
	    if (b < Top)
		clipPoint (b + 1, ix, iy, xout, yout, cnt, store, clip, cs);
	    else {
		if (store) {
		    xout[*cnt] = ix;
		    yout[*cnt] = iy;
		}
		(*cnt)++;
	    }
	}
    }
}

static int clipPoly(double *x, double *y, int n, int store, int toDevice,
		    double *xout, double *yout, pGEDevDesc dd)
{
    int i, cnt = 0;
    GClipState cs[4];
    GClipRect clip;
    for (i = 0; i < 4; i++)
	cs[i].first = 0;
    if (toDevice)
	getClipRectToDevice(&clip.xmin, &clip.ymin, &clip.xmax, &clip.ymax,
			    dd);
    else
	getClipRect(&clip.xmin, &clip.ymin, &clip.xmax, &clip.ymax, dd);
    for (i = 0; i < n; i++)
	clipPoint (Left, x[i], y[i], xout, yout, &cnt, store, &clip, cs);
    closeClip (xout, yout, &cnt, store, &clip, cs);
    return (cnt);
}

static void clipPolygon(int n, double *x, double *y,
			const pGEcontext gc, int toDevice, pGEDevDesc dd)
{
    double *xc = NULL, *yc = NULL;
    const void *vmax = vmaxget();

    /* if bg not specified then draw as polyline rather than polygon
     * to avoid drawing line along border of clipping region
     * If bg was NA then it has been converted to fully transparent */
    if (R_TRANSPARENT(gc->fill)) {
	int i;
	xc = (double*) R_alloc(n + 1, sizeof(double));
	yc = (double*) R_alloc(n + 1, sizeof(double));
	for (i=0; i<n; i++) {
	    xc[i] = x[i];
	    yc[i] = y[i];
	}
	xc[n] = x[0];
	yc[n] = y[0];
	GEPolyline(n+1, xc, yc, gc, dd);
    }
    else {
	int npts;
	xc = yc = 0;		/* -Wall */
	npts = clipPoly(x, y, n, 0, toDevice, xc, yc, dd);
	if (npts > 1) {
	    xc = (double*) R_alloc(npts, sizeof(double));
	    yc = (double*) R_alloc(npts, sizeof(double));
	    npts = clipPoly(x, y, n, 1, toDevice, xc, yc, dd);
	    dd->dev->polygon(npts, xc, yc, gc, dd->dev);
	}
    }
    vmaxset(vmax);
}

/****************************************************************
 * GEPolygon
 ****************************************************************
 */
void GEPolygon(int n, double *x, double *y, const pGEcontext gc, pGEDevDesc dd)
{
    /*
     * Save (and reset below) the heap pointer to clean up
     * after any R_alloc's done by functions I call.
     */
    const void *vmaxsave = vmaxget();
    if (gc->lwd == R_PosInf || gc->lwd < 0.0)
	error(_("'lwd' must be non-negative and finite"));
    if (ISNAN(gc->lwd) || gc->lty == LTY_BLANK)
	/* "transparent" border */
	gc->col = R_TRANWHITE;
    if (dd->dev->canClip) {
	/*
	 * If the device can clip, then we just clip to the device
	 * boundary and let the device do clipping within that.
	 * We do this to avoid problems where writing WAY off the
	 * device can cause problems for, e.g., ghostview
	 */
	clipPolygon(n, x, y, gc, 1, dd);
    }
    else
	/*
	 * If the device can't clip, we have to do all the clipping
	 * ourselves.
	 */
	clipPolygon(n, x, y, gc, 0, dd);
    vmaxset(vmaxsave);
}


/****************************************************************
 * R code for clipping circles
 ****************************************************************
 */
/* Convert a circle into a polygon with specified number of vertices */
static void convertCircle(double x, double y, double r,
			  int numVertices, double *xc, double *yc)
{
    int i;
    double theta = 2*M_PI/numVertices;
    for (i=0; i<numVertices; i++) {
	xc[i] = x + r*sin(theta*i);
	yc[i] = y + r*cos(theta*i);
    }
    xc[numVertices] = x;
    yc[numVertices] = y+r;
}

/* Takes a specification of a circle as input and returns a code indicating
   how the circle should be clipped.
   The return value will be -1 if the circle is to
   be totally clipped out of existence, -2 if the circle is to be
   totally left alone, 0 and above if the circle has been converted
   into a polygon (in which case, the return value indicates the
   number of vertices of the polygon and the function convertCircle()
   should be called to obtain the vertices of the polygon). */
static int clipCircleCode(double x, double y, double r,
			  int toDevice, pGEDevDesc dd)
{
    int result;
    /* determine clipping region */
    double xmin, xmax, ymin, ymax;
    if (toDevice)
	getClipRectToDevice(&xmin, &ymin, &xmax, &ymax, dd);
    else
	getClipRect(&xmin, &ymin, &xmax, &ymax, dd);

    /* if circle is all within clipping rect */
    if (x-r > xmin && x+r < xmax && y-r > ymin && y+r < ymax) {
	result = -2;
    }
    /* if circle is all outside clipping rect */
    else {
	double distance = r*r;
	if (x-r > xmax || x+r < xmin || y-r > ymax || y+r < ymin ||
	    (x < xmin && y < ymin &&
	     ((x-xmin)*(x-xmin)+(y-ymin)*(y-ymin) > distance)) ||
	    (x > xmax && y < ymin &&
	     ((x-xmax)*(x-xmax)+(y-ymin)*(y-ymin) > distance)) ||
	    (x < xmin && y > ymax &&
	     ((x-xmin)*(x-xmin)+(y-ymax)*(y-ymax) > distance)) ||
	    (x > xmax && y > ymax &&
	     ((x-xmax)*(x-xmax)+(y-ymax)*(y-ymax) > distance))) {
	    result = -1;
	}
	/* otherwise, convert circle to polygon */
	else {
	    /* Replace circle with polygon.

	       Heuristic for number of vertices is to use theta so
	       that cos(theta)*r ~ r - 1 in device units. This is
	       roughly const * sqrt(r) so there'd be little point in
	       enforcing an upper limit. */

	    result = (r <= 6) ? 10 : (int)(2 * M_PI/acos(1 - 1/r));
	}
    }
    return result;
}

/****************************************************************
 * GECircle
 ****************************************************************
 */
void GECircle(double x, double y, double radius, const pGEcontext gc, pGEDevDesc dd)
{
    const void *vmax;
    double *xc, *yc;
    int result;

    /* There is no point in trying to plot a circle of zero radius */
    if (radius <= 0.0) return;

    if (gc->lwd == R_PosInf || gc->lwd < 0.0)
	error(_("'lwd' must be non-negative and finite"));
    if (ISNAN(gc->lwd) || gc->lty == LTY_BLANK)
	/* "transparent" border */
	gc->col = R_TRANWHITE;
    /*
     * If the device can clip, then we just clip to the device
     * boundary and let the device do clipping within that.
     * We do this to avoid problems where writing WAY off the
     * device can cause problems for, e.g., ghostview
     *
     * If the device can't clip, we have to do all the clipping
     * ourselves.
     */
    result = clipCircleCode(x, y, radius, dd->dev->canClip, dd);

    switch (result) {
    case -2: /* No clipping;  draw all of circle */
	/*
	 * If we did the clipping, then the circle is entirely
	 * within the current clipping rect.
	 *
	 * If the device can clip then we just clipped to the device
	 * boundary so the circle is entirely within the device; the
	 * device will perform the clipping to the current clipping rect.
	 */
	dd->dev->circle(x, y, radius, gc, dd->dev);
	break;
    case -1: /* Total clipping; draw nothing */
	/*
	 * If we did the clipping, then the circle is entirely outside
	 * the current clipping rect, so there is nothing to draw.
	 *
	 * If the device can clip then we just determined that the
	 * circle is entirely outside the device, so again there is
	 * nothing to draw
	 */
	break;
    default: /* Partial clipping; draw poly[line|gon] */
	/*
	 * If we did the clipping this means that the circle
	 * intersects the current clipping rect and we need to
	 * convert to a poly[line|gon] and draw that.
	 *
	 * If the device can clip then we just determined that the
	 * circle intersects the device boundary.  We assume that the
	 * circle is not so big that other parts may be WAY off the
	 * device and just draw a circle.
	 */
	if (dd->dev->canClip) {
	    dd->dev->circle(x, y, radius, gc, dd->dev);
	}
	else {
	    vmax = vmaxget();
	    xc = (double*)R_alloc(result+1, sizeof(double));
	    yc = (double*)R_alloc(result+1, sizeof(double));
	    convertCircle(x, y, radius, result, xc, yc);
	    if (R_TRANSPARENT(gc->fill)) {
		GEPolyline(result+1, xc, yc, gc, dd);
	    }
	    else {
		int npts;
		double *xcc, *ycc;
		xcc = ycc = 0;	/* -Wall */
		npts = clipPoly(xc, yc, result, 0, !dd->dev->canClip,
				    xcc, ycc, dd);
		if (npts > 1) {
		    xcc = (double*)R_alloc(npts, sizeof(double));
		    ycc = (double*)R_alloc(npts, sizeof(double));
		    npts = clipPoly(xc, yc, result, 1, !dd->dev->canClip,
					xcc, ycc, dd);
		    dd->dev->polygon(npts, xcc, ycc, gc, dd->dev);
		}
	    }
	    vmaxset(vmax);
	}
    }
}

/****************************************************************
 * R code for clipping rectangles
 ****************************************************************
 */
/* Return a code indicating how the rectangle should be clipped.
   0 means the rectangle is totally outside the clip region
   1 means the rectangle is totally inside the clip region
   2 means the rectangle intersects the clip region */
static int clipRectCode(double x0, double y0, double x1, double y1,
			int toDevice, pGEDevDesc dd)
{
    int result;
    /* determine clipping region */
    double xmin, xmax, ymin, ymax;
    if (toDevice)
	getClipRectToDevice(&xmin, &ymin, &xmax, &ymax, dd);
    else
	getClipRect(&xmin, &ymin, &xmax, &ymax, dd);

    if ((x0 < xmin && x1 < xmin) || (x0 > xmax && x1 > xmax) ||
	(y0 < ymin && y1 < ymin) || (y0 > ymax && y1 > ymax))
	result = 0;
    else if ((x0 > xmin && x0 < xmax) && (x1 > xmin && x1 < xmax) &&
	     (y0 > ymin && y0 < ymax) && (y1 > ymin && y1 < ymax))
	result = 1;
    else
	result = 2;

    return result;
}

/****************************************************************
 * GERect
 ****************************************************************
 */
/* Filled with color fill and outlined with color col  */
/* These may both be fully transparent */
void GERect(double x0, double y0, double x1, double y1,
	    const pGEcontext gc, pGEDevDesc dd)
{
    const void *vmax;
    double *xc, *yc;
    int result;

    if (gc->lwd == R_PosInf || gc->lwd < 0.0)
	error(_("'lwd' must be non-negative and finite"));
    if (ISNAN(gc->lwd) || gc->lty == LTY_BLANK)
	/* "transparent" border */
	gc->col = R_TRANWHITE;
    /*
     * For clipping logic, see comments in GECircle
     */
    result = clipRectCode(x0, y0, x1, y1, dd->dev->canClip, dd);
    switch (result) {
    case 0:  /* rectangle totally clipped; draw nothing */
	break;
    case 1:  /* rectangle totally inside;  draw all */
	dd->dev->rect(x0, y0, x1, y1, gc, dd->dev);
	break;
    case 2:  /* rectangle intersects clip region;  use polygon clipping */
	if (dd->dev->canClip)
	    dd->dev->rect(x0, y0, x1, y1, gc, dd->dev);
	else {
	    vmax = vmaxget();
	    xc = (double*)R_alloc(5, sizeof(double));
	    yc = (double*)R_alloc(5, sizeof(double));
	    xc[0] = x0; yc[0] = y0;
	    xc[1] = x0; yc[1] = y1;
	    xc[2] = x1; yc[2] = y1;
	    xc[3] = x1; yc[3] = y0;
	    xc[4] = x0; yc[4] = y0;
	    if (R_TRANSPARENT(gc->fill)) {
		GEPolyline(5, xc, yc, gc, dd);
	    }
	    else { /* filled rectangle */
		int npts;
		double *xcc, *ycc;
		xcc = ycc = 0;		/* -Wall */
		npts = clipPoly(xc, yc, 4, 0, !dd->dev->canClip, xcc, ycc, dd);
		if (npts > 1) {
		    xcc = (double*)R_alloc(npts, sizeof(double));
		    ycc = (double*)R_alloc(npts, sizeof(double));
		    npts = clipPoly(xc, yc, 4, 1, !dd->dev->canClip, xcc, ycc, dd);
		    dd->dev->polygon(npts, xcc, ycc, gc, dd->dev);
		}
	    }
	    vmaxset(vmax);
	}
    }
}

/****************************************************************
 * GEPath
 ****************************************************************
 */

void GEPath(double *x, double *y, 
            int npoly, int *nper,
            Rboolean winding,
            const pGEcontext gc, pGEDevDesc dd)
{
    /* safety check: this will be NULL if the device did not set it. */
    if (!dd->dev->path) {
	warning(_("path rendering is not implemented for this device"));
	return;
    }
    /* FIXME: what about clipping? (if the device can't) 
    */
    if (gc->lwd == R_PosInf || gc->lwd < 0.0)
	error(_("'lwd' must be non-negative and finite"));
    if (ISNAN(gc->lwd) || gc->lty == LTY_BLANK)
	gc->col = R_TRANWHITE;
    if (npoly > 0) {
        int i;
        int draw = 1;
        for (i=0; i < npoly; i++) {
            if (nper[i] < 2) {
                draw = 0;
            }
        }
        if (draw) {
            dd->dev->path(x, y, npoly, nper, winding, gc, dd->dev);
        } else {
	    error(_("Invalid graphics path"));
        }
    }
}

/****************************************************************
 * GERaster
 ****************************************************************
 */

void GERaster(unsigned int *raster, int w, int h,
              double x, double y, 
              double width, double height,
              double angle, 
              Rboolean interpolate,
              const pGEcontext gc, pGEDevDesc dd)
{
    /* safety check: this will be NULL if the device did not set it. */
    if (!dd->dev->raster) {
	warning(_("raster rendering is not implemented for this device"));
	return;
    }

    /* FIXME: what about clipping? (if the device can't) 
     * Maybe not too bad because it is just a matter of shaving off
     * some rows and columns from the image? (because R only does
     * rectangular clipping regions) */
    
    if (width != 0 && height != 0) {
        dd->dev->raster(raster, w, h, x, y, width, height,
                        angle, interpolate, gc, dd->dev);
    }
}

/****************************************************************
 * GERaster
 ****************************************************************
 */

SEXP GECap(pGEDevDesc dd)
{
    /* safety check: this will be NULL if the device did not set it. */
    if (!dd->dev->cap) {
	warning(_("raster capture is not available for this device"));
	return R_NilValue;
    }
    return dd->dev->cap(dd->dev);
}

/****************************************************************
 * R code for clipping text
 ****************************************************************
 */

/* Return a code indicating how the text should be clipped
   NOTE that x, y indicate the bottom-left of the text
   NOTE also also that this is a bit crude because it actually uses
   a bounding box for the entire text to determine the clipping code.
   This will mean that in certain (very rare ?) cases, a piece of
   text will be characterised as intersecting with the clipping region
   when in fact it lies totally outside the clipping region.  But
   this is not a problem because the final output will still be correct.
   0 means totally outside clip region
   1 means totally inside clip region
   2 means intersects clip region */
static int clipTextCode(double x, double y, const char *str, cetype_t enc,
			double width, double height, double rot, double hadj,
			const pGEcontext gc, int toDevice, pGEDevDesc dd)
{
    double x0, x1, x2, x3, y0, y1, y2, y3, left, right, bottom, top;
    double length, theta2;
    double angle = DEG2RAD * rot;
    double theta1 = M_PI/2 - angle;
    double widthInches, heightInches, xInches, yInches;

    if (!R_FINITE(width)) width = GEStrWidth(str, enc, gc, dd);
    if (!R_FINITE(height)) height = GEStrHeight(str, enc, gc, dd);

    /* Work in inches */
    widthInches = fromDeviceWidth(width, GE_INCHES, dd);
    heightInches = fromDeviceHeight(height, GE_INCHES, dd);
    xInches = fromDeviceX(x, GE_INCHES, dd);
    yInches = fromDeviceY(y, GE_INCHES, dd);

    length = hypot(widthInches, heightInches);
    theta2 = angle + atan2(heightInches, widthInches);

    x  = xInches - hadj*widthInches*cos(angle);
    y  = yInches - hadj*widthInches*sin(angle);
    x0 = x + heightInches*cos(theta1);
    x1 = x;
    x2 = x + length*cos(theta2);
    x3 = x + widthInches*cos(angle);
    y0 = y + heightInches*sin(theta1);
    y1 = y;
    y2 = y + length*sin(theta2);
    y3 = y + widthInches*sin(angle);
    left = fmin2(fmin2(x0, x1), fmin2(x2, x3));
    right = fmax2(fmax2(x0, x1), fmax2(x2, x3));
    bottom = fmin2(fmin2(y0, y1), fmin2(y2, y3));
    top = fmax2(fmax2(y0, y1), fmax2(y2, y3));
    return clipRectCode(toDeviceX(left, GE_INCHES, dd),
                        toDeviceY(bottom, GE_INCHES, dd),
                        toDeviceX(right, GE_INCHES, dd),
                        toDeviceY(top, GE_INCHES, dd), 
                        toDevice, dd);
}

static void clipText(double x, double y, const char *str, cetype_t enc,
		     double width, double height, double rot, double hadj,
		     const pGEcontext gc, int toDevice, pGEDevDesc dd)
{
    int result = clipTextCode(x, y, str, enc, width, height, rot, hadj,
			      gc, toDevice, dd);
    void (*textfn)(double x, double y, const char *str, double rot,
		   double hadj, const pGEcontext gc, pDevDesc dd);
    /* This guards against uninitialized values, e.g. devices installed
       in earlier versions of R */
    textfn = (dd->dev->hasTextUTF8 ==TRUE) && enc == CE_UTF8 ?
	dd->dev->textUTF8 : dd->dev->text;

    switch (result) {
    case 0:  /* text totally clipped; draw nothing */
	break;
    case 1:  /* text totally inside;  draw all */
	textfn(x, y, str, rot, hadj, gc, dd->dev);
	break;
    case 2:  /* text intersects clip region
		act according to value of clipToDevice */
	if (toDevice) /* Device will do clipping */
	    textfn(x, y, str, rot, hadj, gc, dd->dev);
	else /* don't draw anything; this could be made less crude :) */
	    ;
    }
}

/****************************************************************
 * Code for determining when to branch to vfont code from GEText
 ****************************************************************
 */

typedef struct {
    char *name;
    int minface;
    int maxface;
} VFontTab;

static VFontTab
VFontTable[] = {
    { "HersheySerif",	          1, 7 },
    /*
       HersheySerif
       HersheySerif-Italic
       HersheySerif-Bold
       HersheySerif-BoldItalic
       HersheyCyrillic
       HersheyCyrillic-Oblique
       HersheyEUC
    */
    { "HersheySans",	          1, 4 },
    /*
       HersheySans
       HersheySans-Oblique
       HersheySans-Bold
       HersheySans-BoldOblique
    */
    { "HersheyScript",	          1, 4 },
    /*
      HersheyScript
      HersheyScript
      HersheyScript-Bold
      HersheyScript-Bold
    */
    { "HersheyGothicEnglish",	  1, 1 },
    { "HersheyGothicGerman",	  1, 1 },
    { "HersheyGothicItalian",	  1, 1 },
    { "HersheySymbol",	          1, 4 },
    /*
      HersheySerifSymbol
      HersheySerifSymbol-Oblique
      HersheySerifSymbol-Bold
      HersheySerifSymbol-BoldOblique
    */
    { "HersheySansSymbol",        1, 2 },
    /*
      HersheySansSymbol
      HersheySansSymbol-Oblique
    */

    { NULL,		          0, 0 },
};

static int VFontFamilyCode(char *fontfamily)
{
    int i, j = fontfamily[3];

    /* Inline vfont is passed down as familycode in fourth byte */
    if (!strncmp(fontfamily, "Her", 3) && j < 9) return 100 + j;
    for (i = 0; VFontTable[i].minface; i++)
	if (!strcmp(fontfamily, VFontTable[i].name)) {
	    return i+1;
	}
    return -1;
}

static int VFontFaceCode(int familycode, int fontface) {
    int face = fontface;
    familycode--;  /* Table is 0-based, coding is 1-based */
    /*
     * R's "font" par has historically made 2=bold and 3=italic
     * These must be switched to correspond to Hershey fontfaces
     */
    if (fontface == 2)
	face = 3;
    else if (fontface == 3)
	face = 2;
    /*
     * If font face is outside supported set of faces for font
     * family, either convert or throw and error
     */
    if (!(face >= VFontTable[familycode].minface &&
	  face <= VFontTable[familycode].maxface)) {
	/*
	 * Silently convert standard faces to closest match
	 */
	switch (face) {
	    /*
	     * italic becomes plain (gothic only)
	     */
	case 2:
	    /*
	     * bold becomes plain
	     */
	case 3:
	    face = 1;
	    break;
	    /*
	     * bold-italic becomes italic for gothic fonts
	     * and bold for sans symbol font
	     */
	case 4:
	    if (familycode == 7)
		face = 2;
	    else
		face = 1;
	    break;
	default:
	    /*
	     * Other font faces just too wacky so throw an error
	     */
	    error(_("font face %d not supported for font family '%s'"),
		  fontface, VFontTable[familycode].name);
	}
    }
    return face;
}

/****************************************************************
 * GEText
 ****************************************************************
 */
/* If you want EXACT centering of text (e.g., like in GSymbol) */
/* then pass NA_REAL for xc and yc */
void GEText(double x, double y, const char * const str, cetype_t enc,
	    double xc, double yc, double rot,
	    const pGEcontext gc, pGEDevDesc dd)
{
    /*
     * If the fontfamily is a Hershey font family, call R_GE_VText
     */
    int vfontcode = VFontFamilyCode(gc->fontfamily);
    if (vfontcode >= 100) {
	R_GE_VText(x, y, str, enc, xc, yc, rot, gc, dd);
    } else if (vfontcode >= 0) {
	gc->fontfamily[3] = (char) vfontcode;
	gc->fontface = VFontFaceCode(vfontcode, gc->fontface);
	R_GE_VText(x, y, str, enc, xc, yc, rot, gc, dd);
    } else {
	/* PR#7397: this seemed to reset R_Visible */
	Rboolean savevis = R_Visible;
	int noMetricInfo = -1;
	char *sbuf = NULL;
	if(str && *str) {
	    const char *s;
	    char *sb;
	    int i, n;
	    cetype_t enc2;
	    double xoff, yoff, hadj;
	    double sin_rot, cos_rot;/* sin() & cos() of rot{ation} in radians */
	    double xleft, ybottom;
	    const void *vmax = vmaxget();

	    enc2 = (gc->fontface == 5) ? CE_SYMBOL : enc;
	    if(enc2 != CE_SYMBOL)
		enc2 = (dd->dev->hasTextUTF8 == TRUE) ? CE_UTF8 : CE_NATIVE;
	    else if(dd->dev->wantSymbolUTF8 == TRUE) enc2 = CE_UTF8;
	    else if(dd->dev->wantSymbolUTF8 == NA_LOGICAL) {
		enc = CE_LATIN1;
		enc2 = CE_UTF8;
	    }

#ifdef DEBUG_MI
	    printf("string %s, enc %d, %d\n", str, enc, enc2);
#endif

	    /* We work in GE_INCHES */
	    x = fromDeviceX(x, GE_INCHES, dd);
	    y = fromDeviceY(y, GE_INCHES, dd);
	    /* Count the lines of text */
	    n = 1;
	    for(s = str; *s ; s++)
		if (*s == '\n') n++;
	    /* Allocate a temporary buffer */
	    sb = sbuf = (char*) R_alloc(strlen(str) + 1, sizeof(char));
	    i = 0;
	    sin_rot = DEG2RAD * rot;
	    cos_rot = cos(sin_rot);
	    sin_rot = sin(sin_rot);
	    for(s = str; ; s++) {
		if (*s == '\n' || *s == '\0') {
		    double w = NA_REAL, h = NA_REAL;
		    const char *str;
		    *sb = '\0';
		    /* This may R_alloc, but let's assume that
		       there are not many lines of text per string */
		    str = reEnc(sbuf, enc, enc2, 2);
		    if (n > 1) {
			/* first determine location of THIS line */
			if (!R_FINITE(xc))
			    xc = 0.5;
			if (!R_FINITE(yc))
			    yc = 0.5;
			yoff = (1 - yc)*(n - 1) - i;
			/* cra is based on the font pointsize at the
			 * time the device was created.
			 * Adjust for potentially different current pointsize.
			 * This is a crude calculation that might be better
			 * performed using a device call that responds with
			 * the current font pointsize in device coordinates.
			 */
			yoff = fromDeviceHeight(yoff * gc->lineheight *
						gc->cex * dd->dev->cra[1] *
						gc->ps/dd->dev->startps,
						GE_INCHES, dd);
			xoff = - yoff*sin_rot;
			yoff = yoff*cos_rot;
			xoff = x + xoff;
			yoff = y + yoff;
		    } else {
			xoff = x;
			yoff = y;
		    }
		    /* now determine bottom-left for THIS line */
		    if(xc != 0.0 || yc != 0.0) {
			double width, height = 0.0 /* -Wall */;
			w  = GEStrWidth(str, enc2, gc, dd);
			width = fromDeviceWidth(w, GE_INCHES, dd);
			if (!R_FINITE(xc))
			    xc = 0.5;
			if (!R_FINITE(yc)) {
			    /* "exact" vertical centering */
			    /* If font metric info is available AND */
			    /* there is only one line, use GMetricInfo & yc=0.5 */
			    /* Otherwise use GEStrHeight and fiddle yc */
			    double h, d, w;
			    if (noMetricInfo < 0) {
				GEMetricInfo('M', gc, &h, &d, &w, dd);
				noMetricInfo = (h == 0 && d == 0 && w == 0) ? 1 : 0;
			    }
			    if (n > 1 || noMetricInfo) {
				h = GEStrHeight(str, enc2, gc, dd);
				height = fromDeviceHeight(h, GE_INCHES, dd);
				yc = dd->dev->yCharOffset;
			    } else {
				double maxHeight = 0.0;
				double maxDepth = 0.0;
				const char *ss = str;
				int charNum = 0;
				Rboolean done = FALSE;
				/* Symbol fonts are not encoded in MBCS ever */
				if(enc2 != CE_SYMBOL && !strIsASCII(ss)) {
				    if(mbcslocale && enc2 == CE_NATIVE) {
					/* FIXME: This assumes that wchar_t is UCS-2/4,
					   since that is what GEMetricInfo expects */
					size_t n = strlen(ss), used;
					wchar_t wc;
					mbstate_t mb_st;
					mbs_init(&mb_st);
					while ((used = mbrtowc(&wc, ss, n, &mb_st)) > 0) {
#ifdef DEBUG_MI
					    printf(" centring %s aka %d in MBCS\n", ss, wc);
#endif
					    GEMetricInfo((int) wc, gc, &h, &d, &w, dd);
					    h = fromDeviceHeight(h, GE_INCHES, dd);
					    d = fromDeviceHeight(d, GE_INCHES, dd);
					    if (charNum++ == 0) {
						maxHeight = h;
						maxDepth = d;
					    } else {
						if (h > maxHeight) maxHeight = h;
						if (d > maxDepth) maxDepth = d;
					    }
					    ss += used; n -=used;
					}
					done = TRUE;
				    } else if (enc2 == CE_UTF8) {
					size_t used;
					wchar_t wc;
					while ((used = utf8toucs(&wc, ss)) > 0) {
					    GEMetricInfo(-(int) wc, gc, &h, &d, &w, dd);
					    h = fromDeviceHeight(h, GE_INCHES, dd);
					    d = fromDeviceHeight(d, GE_INCHES, dd);
#ifdef DEBUG_MI
					    printf(" centring %s aka %d in UTF-8, %f %f\n", ss, wc, h, d);
#endif
					    if (charNum++ == 0) {
						maxHeight = h;
						maxDepth = d;
					    } else {
						if (h > maxHeight) maxHeight = h;
						if (d > maxDepth) maxDepth = d;
					    }
					    ss += used;
					}
					done = TRUE;
				    }
				}
				if(!done) {
				    for (ss = str; *ss; ss++) {
					GEMetricInfo((unsigned char) *ss, gc,
						     &h, &d, &w, dd);
					h = fromDeviceHeight(h, GE_INCHES, dd);
					d = fromDeviceHeight(d, GE_INCHES, dd);
#ifdef DEBUG_MI
					printf("metric info for %d, %f %f\n",
					       (unsigned char) *ss, h, d);
#endif
					/* Set maxHeight and maxDepth from height
					   and depth of first char.
					   Must NOT set to 0 in case there is
					   only 1 char and it has negative
					   height or depth
					*/
					if (charNum++ == 0) {
					    maxHeight = h;
					    maxDepth = d;
					} else {
					    if (h > maxHeight) maxHeight = h;
					    if (d > maxDepth) maxDepth = d;
					}
				    }
				}
				height = maxHeight - maxDepth;
				yc = 0.5;
			    }
			} else {
			    h = GEStrHeight(str, CE_NATIVE, gc, dd);
			    height = fromDeviceHeight(h, GE_INCHES, dd);
			}
			if (dd->dev->canHAdj == 2) hadj = xc;
			else if (dd->dev->canHAdj == 1) {
			    hadj = 0.5 * floor(2*xc + 0.5);
			    /* limit to 0, 0.5, 1 */
			    hadj = (hadj > 1.0) ? 1.0 :((hadj < 0.0) ? 0.0 : hadj);
			} else hadj = 0.0;
			xleft = xoff - (xc-hadj)*width*cos_rot + yc*height*sin_rot;
			ybottom = yoff - (xc-hadj)*width*sin_rot -
			    yc*height*cos_rot;
		    } else { /* xc = yc = 0.0 */
			xleft = xoff;
			ybottom = yoff;
			hadj = 0.0;
		    }
		    /* Convert GE_INCHES back to device.
		     */
		    xleft = toDeviceX(xleft, GE_INCHES, dd);
		    ybottom = toDeviceY(ybottom, GE_INCHES, dd);
		    clipText(xleft, ybottom, str, enc2, w, h, rot, hadj,
			     gc, dd->dev->canClip, dd);
		    sb = sbuf;
		    i++;
		}
		else *sb++ = *s;
		if (!*s) break;
	    }
	    vmaxset(vmax);
	}
	R_Visible = savevis;
    }
}

/****************************************************************
 * GEXspline
 ****************************************************************
 */

#include "xspline.c"

/*
 * Draws a "curve" through the specified control points.
 * Return the vertices of the line that gets drawn.

 * NB: this works in device coordinates.  To make it work correctly
 * with non-square 'pixels' we use the x-dimensions only.
 */
SEXP GEXspline(int n, double *x, double *y, double *s, Rboolean open,
	       Rboolean repEnds,
	       Rboolean draw, /* May be called just to get points */
	       const pGEcontext gc, pGEDevDesc dd)
{
    /*
     * Use xspline.c code to generate points to draw
     * Draw polygon or polyline from points
     */
    SEXP result = R_NilValue;
    int i;
    double *ipr = dd->dev->ipr, asp = ipr[0]/ipr[1], *ys;
    /*
     * Save (and reset below) the heap pointer to clean up
     * after any R_alloc's done by functions I call.
     */
    const void *vmaxsave = vmaxget();
    ys = (double *) R_alloc(n, sizeof(double));
    for (i = 0; i < n; i++) ys[i] = y[i]*asp;
    if (open) {
      compute_open_spline(n, x, ys, s, repEnds, LOW_PRECISION, dd);
      if (draw) {
	  GEPolyline(npoints, xpoints, ypoints, gc, dd);
      }
    } else {
      compute_closed_spline(n, x, ys, s, LOW_PRECISION, dd);
      if (draw)
	  GEPolygon(npoints, xpoints, ypoints, gc, dd);
    }
    if (npoints > 1) {
	SEXP xpts, ypts;
	int i;
	PROTECT(xpts = allocVector(REALSXP, npoints));
	PROTECT(ypts = allocVector(REALSXP, npoints));
	for (i = 0; i < npoints; i++) {
	    REAL(xpts)[i] = xpoints[i];
	    REAL(ypts)[i] = ypoints[i]/asp;
	}
	PROTECT(result = allocVector(VECSXP, 2));
	SET_VECTOR_ELT(result, 0, xpts);
	SET_VECTOR_ELT(result, 1, ypts);
	UNPROTECT(3);
    }
    vmaxset(vmaxsave);
    return result;
}


/****************************************************************
 * GEMode
 ****************************************************************
 */
/* Check that everything is initialized :
	Interpretation :
	mode = 0, graphics off
	mode = 1, graphics on
	mode = 2, graphical input on (ignored by most drivers)
*/
void GEMode(int mode, pGEDevDesc dd)
{
    if (NoDevices())
	error(_("no graphics device is active"));
    if(dd->dev->mode) dd->dev->mode(mode, dd->dev);
}

/****************************************************************
 * GESymbol
 ****************************************************************
 */
#define SMALL	0.25
#define RADIUS	0.375
#define SQRC	0.88622692545275801364		/* sqrt(pi / 4) */
#define DMDC	1.25331413731550025119		/* sqrt(pi / 4) * sqrt(2) */
#define TRC0	1.55512030155621416073		/* sqrt(4 * pi/(3 * sqrt(3))) */
#define TRC1	1.34677368708859836060		/* TRC0 * sqrt(3) / 2 */
#define TRC2	0.77756015077810708036		/* TRC0 / 2 */
/* Draw one of the R special symbols. */
/* "size" is in device coordinates and is assumed to be a width
 * rather than a height.
 * This could cause a problem for devices which have ipr[0] != ipr[1]
 * The problem would be evident where calculations are done on
 * angles -- in those cases, a conversion to and from GE_INCHES is done
 * to preserve angles.
 */
void GESymbol(double x, double y, int pch, double size,
	      const pGEcontext gc, pGEDevDesc dd)
{
    double r, xc, yc;
    double xx[4], yy[4];
    unsigned int maxchar;

    maxchar = (mbcslocale && gc->fontface != 5) ? 127 : 255;
    /* Special cases for plotting pch="." or pch=<character>
     */
    if(pch == NA_INTEGER) /* do nothing */;
    else if(pch < 0) {
	size_t res;
	char str[16];
	if(gc->fontface == 5)
	    error("use of negative pch with symbol font is invalid");
	res = ucstoutf8(str, -pch);
	if(res == (size_t)-1 || res == (size_t)-2) 
	    error("invalid Unicode pch '%d'", pch);
	str[res] = '\0';
	GEText(x, y, str, CE_UTF8, NA_REAL, NA_REAL, 0., gc, dd);
    } else if(' ' <= pch && pch <= maxchar) {
	if (pch == '.') {
	    /*
	     * NOTE:  we are *filling* a rect with the current
	     * colour (we are not drawing the border AND we are
	     * not using the current fill colour)
	     */
	    gc->fill = gc->col;
	    gc->col = R_TRANWHITE;
	    /*
	       The idea here is to use a 0.01" square, but to be of
	       at least one device unit in each direction,
	       assuming that corresponds to pixels. That may be odd if
	       pixels are not square, but only on low resolution
	       devices where we can do nothing better.

	       For this symbol only, size is cex (see engine.c).

	       Prior to 2.1.0 the offsets were always 0.5.
	    */
	    xc = size * fabs(toDeviceWidth(0.005, GE_INCHES, dd));
	    yc = size * fabs(toDeviceHeight(0.005, GE_INCHES, dd));
	    if(size > 0 && xc < 0.5) xc = 0.5;
	    if(size > 0 && yc < 0.5) yc = 0.5;
	    GERect(x-xc, y-yc, x+xc, y+yc, gc, dd);
	} else {
	    char str[2];
	    str[0] = (char) pch;
	    str[1] = '\0';
	    GEText(x, y, str,
		   (gc->fontface == 5) ? CE_SYMBOL : CE_NATIVE,
		   NA_REAL, NA_REAL, 0., gc, dd);
	}
    }
    else if(pch > maxchar)
	    warning(_("pch value '%d' is invalid in this locale"), pch);
    else {
	double GSTR_0 = fromDeviceWidth(size, GE_INCHES, dd);

	switch(pch) {

	case 0: /* S square */
	    xc = toDeviceWidth(RADIUS * GSTR_0, GE_INCHES, dd);
	    yc = toDeviceHeight(RADIUS * GSTR_0, GE_INCHES, dd);
	    gc->fill = R_TRANWHITE;
	    GERect(x-xc, y-yc, x+xc, y+yc, gc, dd);
	    break;

	case 1: /* S octahedron ( circle) */
	    xc = RADIUS * size; /* NB: could be zero */
	    gc->fill = R_TRANWHITE;
	    GECircle(x, y, xc, gc, dd);
	    break;

	case 2:	/* S triangle - point up */
	    xc = RADIUS * GSTR_0;
	    r = toDeviceHeight(TRC0 * xc, GE_INCHES, dd);
	    yc = toDeviceHeight(TRC2 * xc, GE_INCHES, dd);
	    xc = toDeviceWidth(TRC1 * xc, GE_INCHES, dd);
	    xx[0] = x; yy[0] = y+r;
	    xx[1] = x+xc; yy[1] = y-yc;
	    xx[2] = x-xc; yy[2] = y-yc;
	    gc->fill = R_TRANWHITE;
	    GEPolygon(3, xx, yy, gc, dd);
	    break;

	case 3: /* S plus */
	    xc = toDeviceWidth(M_SQRT2*RADIUS*GSTR_0, GE_INCHES, dd);
	    yc = toDeviceHeight(M_SQRT2*RADIUS*GSTR_0, GE_INCHES, dd);
	    GELine(x-xc, y, x+xc, y, gc, dd);
	    GELine(x, y-yc, x, y+yc, gc, dd);
	    break;

	case 4: /* S times */
	    xc = toDeviceWidth(RADIUS * GSTR_0, GE_INCHES, dd);
	    yc = toDeviceHeight(RADIUS * GSTR_0, GE_INCHES, dd);
	    GELine(x-xc, y-yc, x+xc, y+yc, gc, dd);
	    GELine(x-xc, y+yc, x+xc, y-yc, gc, dd);
	    break;

	case 5: /* S diamond */
	    xc = toDeviceWidth(M_SQRT2 * RADIUS * GSTR_0, GE_INCHES, dd);
	    yc = toDeviceHeight(M_SQRT2 * RADIUS * GSTR_0, GE_INCHES, dd);
	    xx[0] = x-xc; yy[0] = y;
	    xx[1] = x; yy[1] = y+yc;
	    xx[2] = x+xc; yy[2] = y;
	    xx[3] = x; yy[3] = y-yc;
	    gc->fill = R_TRANWHITE;
	    GEPolygon(4, xx, yy, gc, dd);
	    break;

	case 6: /* S triangle - point down */
	    xc = RADIUS * GSTR_0;
	    r = toDeviceHeight(TRC0 * xc, GE_INCHES, dd);
	    yc = toDeviceHeight(TRC2 * xc, GE_INCHES, dd);
	    xc = toDeviceWidth(TRC1 * xc, GE_INCHES, dd);
	    xx[0] = x; yy[0] = y-r;
	    xx[1] = x+xc; yy[1] = y+yc;
	    xx[2] = x-xc; yy[2] = y+yc;
	    gc->fill = R_TRANWHITE;
	    GEPolygon(3, xx, yy, gc, dd);
	    break;

	case 7:	/* S square and times superimposed */
	    xc = toDeviceWidth(RADIUS * GSTR_0, GE_INCHES, dd);
	    yc = toDeviceHeight(RADIUS * GSTR_0, GE_INCHES, dd);
	    gc->fill = R_TRANWHITE;
	    GERect(x-xc, y-yc, x+xc, y+yc, gc, dd);
	    GELine(x-xc, y-yc, x+xc, y+yc, gc, dd);
	    GELine(x-xc, y+yc, x+xc, y-yc, gc, dd);
	    break;

	case 8: /* S plus and times superimposed */
	    xc = toDeviceWidth(RADIUS * GSTR_0, GE_INCHES, dd);
	    yc = toDeviceHeight(RADIUS * GSTR_0, GE_INCHES, dd);
	    GELine(x-xc, y-yc, x+xc, y+yc, gc, dd);
	    GELine(x-xc, y+yc, x+xc, y-yc, gc, dd);
	    xc = toDeviceWidth(M_SQRT2*RADIUS*GSTR_0, GE_INCHES, dd);
	    yc = toDeviceHeight(M_SQRT2*RADIUS*GSTR_0, GE_INCHES, dd);
	    GELine(x-xc, y, x+xc, y, gc, dd);
	    GELine(x, y-yc, x, y+yc, gc, dd);
	    break;

	case 9: /* S diamond and plus superimposed */
	    xc = toDeviceWidth(M_SQRT2 * RADIUS * GSTR_0, GE_INCHES, dd);
	    yc = toDeviceHeight(M_SQRT2 * RADIUS * GSTR_0, GE_INCHES, dd);
	    GELine(x-xc, y, x+xc, y, gc, dd);
	    GELine(x, y-yc, x, y+yc, gc, dd);
	    xx[0] = x-xc; yy[0] = y;
	    xx[1] = x; yy[1] = y+yc;
	    xx[2] = x+xc; yy[2] = y;
	    xx[3] = x; yy[3] = y-yc;
	    gc->fill = R_TRANWHITE;
	    GEPolygon(4, xx, yy, gc, dd);
	    break;

	case 10: /* S hexagon (circle) and plus superimposed */
	    xc = toDeviceWidth(RADIUS * GSTR_0, GE_INCHES, dd);
	    yc = toDeviceHeight(RADIUS * GSTR_0, GE_INCHES, dd);
	    gc->fill = R_TRANWHITE;
	    GECircle(x, y, xc, gc, dd);
	    GELine(x-xc, y, x+xc, y, gc, dd);
	    GELine(x, y-yc, x, y+yc, gc, dd);
	    break;

	case 11: /* S superimposed triangles */
	    xc = RADIUS * GSTR_0;
	    r = toDeviceHeight(TRC0 * xc, GE_INCHES, dd);
	    yc = toDeviceHeight(TRC2 * xc, GE_INCHES, dd);
	    yc = 0.5 * (yc + r);
	    xc = toDeviceWidth(TRC1 * xc, GE_INCHES, dd);
	    xx[0] = x; yy[0] = y-r;
	    xx[1] = x+xc; yy[1] = y+yc;
	    xx[2] = x-xc; yy[2] = y+yc;
	    gc->fill = R_TRANWHITE;
	    GEPolygon(3, xx, yy, gc, dd);
	    xx[0] = x; yy[0] = y+r;
	    xx[1] = x+xc; yy[1] = y-yc;
	    xx[2] = x-xc; yy[2] = y-yc;
	    GEPolygon(3, xx, yy, gc, dd);
	    break;

	case 12: /* S square and plus superimposed */
	    xc = toDeviceWidth(RADIUS * GSTR_0, GE_INCHES, dd);
	    yc = toDeviceHeight(RADIUS * GSTR_0, GE_INCHES, dd);
	    GELine(x-xc, y, x+xc, y, gc, dd);
	    GELine(x, y-yc, x, y+yc, gc, dd);
	    gc->fill = R_TRANWHITE;
	    GERect(x-xc, y-yc, x+xc, y+yc, gc, dd);
	    break;

	case 13: /* S octagon (circle) and times superimposed */
	    xc = RADIUS * size;
	    gc->fill = R_TRANWHITE;
	    GECircle(x, y, xc, gc, dd);
	    xc = toDeviceWidth(RADIUS * GSTR_0, GE_INCHES, dd);
	    yc = toDeviceHeight(RADIUS * GSTR_0, GE_INCHES, dd);
	    GELine(x-xc, y-yc, x+xc, y+yc, gc, dd);
	    GELine(x-xc, y+yc, x+xc, y-yc, gc, dd);
	    break;

	case 14: /* S square and point-up triangle superimposed */
	    xc = toDeviceWidth(RADIUS * GSTR_0, GE_INCHES, dd);
	    xx[0] = x; yy[0] = y+xc;
	    xx[1] = x+xc; yy[1] = y-xc;
	    xx[2] = x-xc; yy[2] = y-xc;
	    gc->fill = R_TRANWHITE;
	    GEPolygon(3, xx, yy, gc, dd);
	    GERect(x-xc, y-xc, x+xc, y+xc, gc, dd);
	    break;

	case 15: /* S filled square */
	    xc = toDeviceWidth(RADIUS * GSTR_0, GE_INCHES, dd);
	    yc = toDeviceHeight(RADIUS * GSTR_0, GE_INCHES, dd);
	    xx[0] = x-xc; yy[0] = y-yc;
	    xx[1] = x+xc; yy[1] = y-yc;
	    xx[2] = x+xc; yy[2] = y+yc;
	    xx[3] = x-xc; yy[3] = y+yc;
	    gc->fill = gc->col;
	    gc->col = R_TRANWHITE;
	    GEPolygon(4, xx, yy, gc, dd);
	    break;

	case 16: /* S filled octagon (circle) */
	    xc = RADIUS * size;
	    gc->fill = gc->col;
	    gc->col = R_TRANWHITE;
	    GECircle(x, y, xc, gc, dd);
	    break;

	case 17: /* S filled point-up triangle */
	    xc = RADIUS * GSTR_0;
	    r = toDeviceHeight(TRC0 * xc, GE_INCHES, dd);
	    yc = toDeviceHeight(TRC2 * xc, GE_INCHES, dd);
	    xc = toDeviceWidth(TRC1 * xc, GE_INCHES, dd);
	    xx[0] = x; yy[0] = y+r;
	    xx[1] = x+xc; yy[1] = y-yc;
	    xx[2] = x-xc; yy[2] = y-yc;
	    gc->fill = gc->col;
	    gc->col = R_TRANWHITE;
	    GEPolygon(3, xx, yy, gc, dd);
	    break;

	case 18: /* S filled diamond */
	    xc = toDeviceWidth(RADIUS * GSTR_0, GE_INCHES, dd);
	    yc = toDeviceHeight(RADIUS * GSTR_0, GE_INCHES, dd);
	    xx[0] = x-xc; yy[0] = y;
	    xx[1] = x; yy[1] = y+yc;
	    xx[2] = x+xc; yy[2] = y;
	    xx[3] = x; yy[3] = y-yc;
	    gc->fill = gc->col;
	    gc->col = R_TRANWHITE;
	    GEPolygon(4, xx, yy, gc, dd);
	    break;

	case 19: /* R filled circle */
	    xc = RADIUS * size;
	    gc->fill = gc->col;
	    GECircle(x, y, xc, gc, dd);
	    break;


	case 20: /* R `Dot' (small circle) */
	    xc = SMALL * size;
	    gc->fill = gc->col;
	    GECircle(x, y, xc, gc, dd);
	    break;


	case 21: /* circles */
	    xc = RADIUS * size;
	    GECircle(x, y, xc, gc, dd);
	    break;

	case  22: /* squares */
	    xc = toDeviceWidth(RADIUS * SQRC * GSTR_0, GE_INCHES, dd);
	    yc = toDeviceHeight(RADIUS * SQRC * GSTR_0, GE_INCHES, dd);
	    GERect(x-xc, y-yc, x+xc, y+yc, gc, dd);
	    break;

	case 23: /* diamonds */
	    xc = toDeviceWidth(RADIUS * DMDC * GSTR_0, GE_INCHES, dd);
	    yc = toDeviceHeight(RADIUS * DMDC * GSTR_0, GE_INCHES, dd);
	    xx[0] = x	  ; yy[0] = y-yc;
	    xx[1] = x+xc; yy[1] = y;
	    xx[2] = x	  ; yy[2] = y+yc;
	    xx[3] = x-xc; yy[3] = y;
	    GEPolygon(4, xx, yy, gc, dd);
	    break;

	case 24: /* triangle (point up) */
	    xc = RADIUS * GSTR_0;
	    r = toDeviceHeight(TRC0 * xc, GE_INCHES, dd);
	    yc = toDeviceHeight(TRC2 * xc, GE_INCHES, dd);
	    xc = toDeviceWidth(TRC1 * xc, GE_INCHES, dd);
	    xx[0] = x; yy[0] = y+r;
	    xx[1] = x+xc; yy[1] = y-yc;
	    xx[2] = x-xc; yy[2] = y-yc;
	    GEPolygon(3, xx, yy, gc, dd);
	    break;

	case 25: /* triangle (point down) */
	    xc = RADIUS * GSTR_0;
	    r = toDeviceHeight(TRC0 * xc, GE_INCHES, dd);
	    yc = toDeviceHeight(TRC2 * xc, GE_INCHES, dd);
	    xc = toDeviceWidth(TRC1 * xc, GE_INCHES, dd);
	    xx[0] = x; yy[0] = y-r;
	    xx[1] = x+xc; yy[1] = y+yc;
	    xx[2] = x-xc; yy[2] = y+yc;
	    GEPolygon(3, xx, yy, gc, dd);
	    break;
	default:
	    warning(_("unimplemented pch value '%d'"), pch);
	}
    }
}

/****************************************************************
 * GEPretty
 ****************************************************************
 */
void GEPretty(double *lo, double *up, int *ndiv)
{
/*	Set scale and ticks for linear scales.
 *
 *	Pre:	    x1 == lo < up == x2      ;  ndiv >= 1
 *	Post: x1 <= y1 := lo < up =: y2 <= x2;	ndiv >= 1
 */
    double unit, ns, nu;
    double high_u_fact[2] = { .8, 1.7 };
#ifdef DEBUG_PLOT
    double x1,x2;
#endif

    if(*ndiv <= 0)
	error(_("invalid axis extents [GEPretty(.,.,n=%d)"), *ndiv);
    if(*lo == R_PosInf || *up == R_PosInf ||
       *lo == R_NegInf || *up == R_NegInf ||
       !R_FINITE(*up - *lo)) {
	error(_("infinite axis extents [GEPretty(%g,%g,%d)]"), *lo, *up, *ndiv);
	return;/*-Wall*/
    }

    ns = *lo; nu = *up;
#ifdef DEBUG_PLOT
    x1 = ns; x2 = nu;
#endif
    unit = R_pretty(&ns, &nu, ndiv, /* min_n = */ 1,
		    /* shrink_sml = */ 0.25,
		    high_u_fact,
		    2, /* do eps_correction in any case */
		    0 /* return (ns,nu) in  (lo,up) */);

    /* The following is ugly since it kind of happens already in Rpretty0(..):
     */
#define rounding_eps 1e-7
    if(nu >= ns + 1) {
	if(               ns * unit < *lo - rounding_eps*unit)
	    ns++;
	if(nu > ns + 1 && nu * unit > *up + rounding_eps*unit)
	    nu--;
	*ndiv = (int)(nu - ns);
    }
    *lo = ns * unit;
    *up = nu * unit;
#ifdef non_working_ALTERNATIVE
    if(ns * unit > *lo)
	*lo = ns * unit;
    if(nu * unit < *up)
	*up = nu * unit;
    if(nu - ns >= 1)
	*ndiv = nu - ns;
#endif

#ifdef DEBUG_PLOT
    if(*lo < x1)
	warning(_(" .. GEPretty(.): new *lo = %g < %g = x1"), *lo, x1);
    if(*up > x2)
	warning(_(" .. GEPretty(.): new *up = %g > %g = x2"), *up, x2);
#endif
}

/****************************************************************
 * GEMetricInfo
 ****************************************************************
 */
/*
  If c is negative, -c is a Unicode point.
  In a MBCS locale, values > 127 are Unicode points (and so really are
  values 32 ... 126, 127 being unused).
  In a SBCS locale, values 32 ... 255 are the characters in the encoding.
 */
void GEMetricInfo(int c, const pGEcontext gc,
		  double *ascent, double *descent, double *width,
		  pGEDevDesc dd)
{
    /*
     * If the fontfamily is a Hershey font family, call R_GE_VText
     */
    int vfontcode = VFontFamilyCode(gc->fontfamily);
    if (vfontcode >= 0) {
	/*
	 * It should be straightforward to figure this out, but
	 * just haven't got around to it yet
	 */
	*ascent = 0.0;
	*descent = 0.0;
	*width = 0.0;
    } else {
	/* c = 'M' gets called very often, usually to see if there are
	   any char metrics available but also in plotmath.  So we
	   cache that value.  Depends on the context through cex, ps,
	   fontface, family, and also on the device.

           PAUL 2008-11-27
           The point of checking dd == last_dd is to check for
           a different TYPE of device (e.g., PDF vs. PNG).
           Checking just the pGEDevDesc pointer is not a good enough 
           test;  it is possible for that to be the same when one
           device is closed and a new one is opened (I have seen 
           it happen!). 
           So, ALSO compare dd->dev->close function pointer
           which really should be different for different devices.
	*/
	static pGEDevDesc last_dd= NULL;
#if R_USE_PROTOTYPES
        static void (*last_close)(pDevDesc dd);
#else
        static void (*last_close)();
#endif
	static int last_face = 1;
	static double last_cex = 0.0, last_ps = 0.0,
	    a = 0.0 , d = 0.0, w = 0.0;
	static char last_family[201];
	if (dd == last_dd && dd->dev->close == last_close && abs(c) == 77
	    && gc->cex == last_cex && gc->ps == last_ps
	    && gc->fontface == last_face
	    && streql(gc->fontfamily, last_family)) {
	    *ascent = a; *descent = d; *width = w; return;
	}
	dd->dev->metricInfo(c, gc, ascent, descent, width, dd->dev);
	if(abs(c) == 77) {
	    last_dd = dd;  last_close = dd->dev->close;
            last_cex = gc->cex; last_ps = gc->ps;
	    last_face = gc->fontface;
	    strcpy(last_family, gc->fontfamily);
	    a = *ascent; d = *descent; w = *width;
	}
    }
}

/****************************************************************
 * GEStrWidth
 ****************************************************************
 */
double GEStrWidth(const char *str, cetype_t enc, const pGEcontext gc, pGEDevDesc dd)
{
    /*
     * If the fontfamily is a Hershey font family, call R_GE_VStrWidth
     */
    int vfontcode = VFontFamilyCode(gc->fontfamily);
    if (vfontcode >= 100)
	return R_GE_VStrWidth(str, enc, gc, dd);
    else if (vfontcode >= 0) {
	gc->fontfamily[3] = (char) vfontcode;
	gc->fontface = VFontFaceCode(vfontcode, gc->fontface);
	return R_GE_VStrWidth(str, enc, gc, dd);
    } else {
	double w;
	char *sbuf = NULL;
	w = 0;
	if(str && *str) {
	    const char *s;
	    char *sb;
	    double wdash;
	    cetype_t enc2;
	    const void *vmax = vmaxget();

	    enc2 = (gc->fontface == 5) ? CE_SYMBOL : enc;
	    if(enc2 != CE_SYMBOL)
		enc2 = (dd->dev->hasTextUTF8 == TRUE) ? CE_UTF8 : CE_NATIVE;
	    else if(dd->dev->wantSymbolUTF8 == TRUE) enc2 = CE_UTF8;

	    sb = sbuf = (char*) R_alloc(strlen(str) + 1, sizeof(char));
	    for(s = str; ; s++) {
		if (*s == '\n' || *s == '\0') {
		    const char *str;
		    *sb = '\0';
		    /* This may R_alloc, but let's assume that
		       there are not many lines of text per string */
		    str = reEnc(sbuf, enc, enc2, 2);
		    if(dd->dev->hasTextUTF8 == TRUE && enc2 == CE_UTF8)
			wdash = dd->dev->strWidthUTF8(str, gc, dd->dev);
		    else
			wdash = dd->dev->strWidth(str, gc, dd->dev);
		    if (wdash > w) w = wdash;
		    sb = sbuf;
		}
		else *sb++ = *s;
		if (!*s) break;
	    }
	    vmaxset(vmax);
	}
	return w;
    }
}

/****************************************************************
 * GEStrHeight
 ****************************************************************

 * This does not (currently) depend on the encoding.  It depends on
 * the string only through the number of lines of text (via embedded
 * \n) and we assume they are never part of an mbc.
 */
double GEStrHeight(const char *str, cetype_t enc, const pGEcontext gc, pGEDevDesc dd)
{
    /*
     * If the fontfamily is a Hershey font family, call R_GE_VStrHeight
     */
    int vfontcode = VFontFamilyCode(gc->fontfamily);
    if (vfontcode >= 100)
	return R_GE_VStrHeight(str, enc, gc, dd);
    else if (vfontcode >= 0) {
	gc->fontfamily[3] = (char) vfontcode;
	gc->fontface = VFontFaceCode(vfontcode, gc->fontface);
	return R_GE_VStrHeight(str, enc, gc, dd);
    } else {
	double h;
	const char *s;
	double asc, dsc, wid;
	int n;
	/* Count the lines of text minus one */
	n = 0;
	for(s = str; *s ; s++)
	    if (*s == '\n')
		n++;
	/* cra is based on the font pointsize at the
	 * time the device was created.
	 * Adjust for potentially different current pointsize
	 * This is a crude calculation that might be better
	 * performed using a device call that responds with
	 * the current font pointsize in device coordinates.
	 */
	h = n * gc->lineheight * gc->cex * dd->dev->cra[1] *
	    gc->ps/dd->dev->startps;
	/* Add in the ascent of the font, if available */
	GEMetricInfo('M', gc, &asc, &dsc, &wid, dd);
	if ((asc == 0.0) && (dsc == 0.0) && (wid == 0.0))
	    asc = gc->lineheight * gc->cex * dd->dev->cra[1] *
		gc->ps/dd->dev->startps;
	h += asc;
	return h;
    }
}

/****************************************************************
 * GEStrMetric
 ****************************************************************

 * This does not (currently) depend on the encoding.  It depends on
 * the string only through the number of lines of text (via embedded
 * \n) and we assume they are never part of an mbc.
 */
void GEStrMetric(const char *str, cetype_t enc, const pGEcontext gc, 
                 double *ascent, double *descent, double *width,
                 pGEDevDesc dd)
{
    /*
     * If the fontfamily is a Hershey font family, call R_GE_VStrHeight
     */
    int vfontcode = VFontFamilyCode(gc->fontfamily);
    *ascent = 0.0;
    *descent = 0.0;
    *width = 0.0;
    if (vfontcode >= 0) {
	/*
	 * It should be straightforward to figure this out, but
	 * just haven't got around to it yet
	 */
    } else {
	double h;
	const char *s;
	double asc, dsc, wid;
	/* cra is based on the font pointsize at the
	 * time the device was created.
	 * Adjust for potentially different current pointsize
	 * This is a crude calculation that might be better
	 * performed using a device call that responds with
	 * the current font pointsize in device coordinates.
	 */
        double lineheight = gc->lineheight * gc->cex * dd->dev->cra[1] *
                            gc->ps/dd->dev->startps;
	int n;
	/* Count the lines of text minus one */
	n = 0;
	for(s = str; *s ; s++)
	    if (*s == '\n')
		n++;
        /* Where is the start of the last line? */
        if (n > 0) {
            while (*s != '\n') 
                s--;
            s++;
        } else {
            s = str;
        }
	h = n * lineheight;
        /* Find the largest ascent and descent for the last line of text
         */
        while (*s) {
            GEMetricInfo(*s, gc, &asc, &dsc, &wid, dd);
            if (asc > *ascent)
                *ascent = asc;
            if (dsc > *descent)
                *descent = dsc;
            s++;
        }
        *ascent = *ascent + h;
        *width = GEStrWidth(str, enc, gc ,dd);
    }
}

/****************************************************************
 * GENewPage
 ****************************************************************
 */

void GENewPage(const pGEcontext gc, pGEDevDesc dd)
{
    dd->dev->newPage(gc, dd->dev);
}

/****************************************************************
 * GEdeviceDirty
 ****************************************************************
 *
 * Has the device received output from any graphics system?
 */

Rboolean GEdeviceDirty(pGEDevDesc dd)
{
    return dd->dirty;
}

/****************************************************************
 * GEdirtyDevice
 ****************************************************************
 *
 * Indicate that the device has received output from at least one
 * graphics system.
 */

void GEdirtyDevice(pGEDevDesc dd)
{
    dd->dirty = TRUE;
}

/****************************************************************
 * GEcheckState
 ****************************************************************
 *
 * Check whether all registered graphics systems are in a
 * "valid" state.
 */

Rboolean GEcheckState(pGEDevDesc dd)
{
    int i;
    Rboolean result = TRUE;
    for (i=0; i < MAX_GRAPHICS_SYSTEMS; i++)
	if (dd->gesd[i] != NULL)
	    if (!LOGICAL((dd->gesd[i]->callback)(GE_CheckPlot, dd,
						 R_NilValue))[0])
		result = FALSE;
    return result;
}

/****************************************************************
 * GErecording
 ****************************************************************
 */

Rboolean GErecording(SEXP call, pGEDevDesc dd)
{
    return (call != R_NilValue && dd->recordGraphics);
}

/****************************************************************
 * GErecordGraphicOperation
 ****************************************************************
 */

void GErecordGraphicOperation(SEXP op, SEXP args, pGEDevDesc dd)
{
    SEXP lastOperation = dd->DLlastElt;
    if (dd->displayListOn) {
	SEXP newOperation = list2(op, args);
	if (lastOperation == R_NilValue) {
	    dd->displayList = CONS(newOperation, R_NilValue);
	    dd->DLlastElt = dd->displayList;
	} else {
	    SETCDR(lastOperation, CONS(newOperation, R_NilValue));
	    dd->DLlastElt = CDR(lastOperation);
	}
    }
}

/****************************************************************
 * GEinitDisplayList
 ****************************************************************
 */

void GEinitDisplayList(pGEDevDesc dd)
{
    int i;
    /* Save the current displayList so that, for example, a device
     * can maintain a plot history
     */
    dd->savedSnapshot = GEcreateSnapshot(dd);
    /* Get each graphics system to save state required for
     * replaying the display list
     */
    for (i = 0; i < MAX_GRAPHICS_SYSTEMS; i++)
	if (dd->gesd[i] != NULL)
	    (dd->gesd[i]->callback)(GE_SaveState, dd, R_NilValue);
    dd->displayList = dd->DLlastElt = R_NilValue;
}

/****************************************************************
 * GEplayDisplayList
 ****************************************************************
 */

/* from colors.c */
void savePalette(Rboolean save);

void GEplayDisplayList(pGEDevDesc dd)
{
    int i, this, savedDevice, plotok;
    SEXP theList;

    /* If the device is not registered with the engine (which might
       happen in a device callback before it has been registered or
       while it is being killed) we might get the null device and
       should do nothing.

       Also do nothing if displayList is empty (which should be the
       case for the null device).
    */
    this = GEdeviceNumber(dd);
    if (this == 0) return;
    theList = dd->displayList;
    if (theList == R_NilValue) return;

    /* Get each graphics system to restore state required for
     * replaying the display list
     */
    for (i = 0; i < MAX_GRAPHICS_SYSTEMS; i++)
	if (dd->gesd[i] != NULL)
	    (dd->gesd[i]->callback)(GE_RestoreState, dd, R_NilValue);
    /* Play the display list
     */
    PROTECT(theList);
    plotok = 1;
    if (theList != R_NilValue) {
	savePalette(TRUE);
	savedDevice = curDevice();
	selectDevice(this);
	while (theList != R_NilValue && plotok) {
	    SEXP theOperation = CAR(theList);
	    SEXP op = CAR(theOperation);
	    SEXP args = CADR(theOperation);
	    if (TYPEOF(op) == BUILTINSXP || TYPEOF(op) == SPECIALSXP) {
	    	PRIMFUN(op) (R_NilValue, op, args, R_NilValue);
		/* Check with each graphics system that the plotting went ok
		 */
		if (!GEcheckState(dd)) {
		    warning(_("display list redraw incomplete"));
		    plotok = 0;
		}
	    } else {
	    	warning(_("invalid display list"));
	    	plotok = 0;
	    }
	    theList = CDR(theList);
	}
	selectDevice(savedDevice);
	savePalette(FALSE);
    }
    UNPROTECT(1);
}


/****************************************************************
 * GEcopyDisplayList
 ****************************************************************
 */

/* We assume that the device being copied TO is the "current" device
 */
void GEcopyDisplayList(int fromDevice)
{
    SEXP tmp;
    pGEDevDesc dd = GEcurrentDevice(), gd = GEgetDevice(fromDevice);
    int i;

    tmp = gd->displayList;
    if(!isNull(tmp)) tmp = duplicate(tmp);
    dd->displayList = tmp;
    dd->DLlastElt = lastElt(dd->displayList);
    /* Get each registered graphics system to copy system state
     * information from the "from" device to the current device
     */
    for (i=0; i < MAX_GRAPHICS_SYSTEMS; i++)
	if (dd->gesd[i] != NULL)
	    (dd->gesd[i]->callback)(GE_CopyState, gd, R_NilValue);
    GEplayDisplayList(dd);
    if (!dd->displayListOn) GEinitDisplayList(dd);
}

/****************************************************************
 * GEcreateSnapshot
 ****************************************************************
 */

/* Create a recording of the current display,
 * including enough information from each registered
 * graphics system to be able to recreate the display
 * The structure created is an SEXP which nicely hides the
 * internals, because noone should be looking in there anyway
 * The product of this call can be stored, but should only
 * be used in a call to GEplaySnapshot.
 */

SEXP GEcreateSnapshot(pGEDevDesc dd)
{
    int i;
    SEXP snapshot, tmp;
    SEXP state;
    /* Create a list with one spot for the display list
     * and one spot each for the registered graphics systems
     * to put their graphics state
     */
    PROTECT(snapshot = allocVector(VECSXP, 1 + numGraphicsSystems));
    /* The first element of the snapshot is the display list.
     */
    if(!isNull(dd->displayList)) {
	PROTECT(tmp = duplicate(dd->displayList));
	SET_VECTOR_ELT(snapshot, 0, tmp);
	UNPROTECT(1);
    }
    /* For each registered system, obtain state information,
     * and store that in the snapshot.
     */
    for (i = 0; i < MAX_GRAPHICS_SYSTEMS; i++)
	if (dd->gesd[i] != NULL) {
	    PROTECT(state = (dd->gesd[i]->callback)(GE_SaveSnapshotState, dd,
						    R_NilValue));
	    SET_VECTOR_ELT(snapshot, i + 1, state);
	    UNPROTECT(1);
	}
    UNPROTECT(1);
    return snapshot;
}

/****************************************************************
 * GEplaySnapshot
 ****************************************************************
 */

/* Recreate a saved display using the information in a structure
 * created by GEcreateSnapshot.
 *
 * The graphics engine assumes that it is getting a snapshot
 * that was created in THE CURRENT R SESSION
 * (Thus, it can assume that registered graphics systems are
 *  in the same order as they were when the snapshot was
 *  created -- in patricular, state information will be sent
 *  to the appropriate graphics system.)
 * [With only two systems and base registered on each device at
 * creation, that has to be true: and grid does not save any state.]
 *
 *  It also assumes that the system that created the snapshot is
 *  still loaded (e.g. the grid namespace has not been unloaded).
 *
 * It is possible to save a snapshot to an R variable
 * (and therefore save and reload it between sessions and
 *  even possibly into a different R version),
 * BUT this is strongly discouraged
 * (in the documentation for recordPlot() and replayPlot()
 *  and in the documentation for the Rgui interface on Windows)
 */

void GEplaySnapshot(SEXP snapshot, pGEDevDesc dd)
{
    /* Only have to set up information for as many graphics systems
     * as were registered when the snapshot was taken.
     */
    int i, numSystems = LENGTH(snapshot) - 1;
    /* Reset the snapshot state information in each registered
     * graphics system
     */
    for (i = 0; i < numSystems; i++)
	if (dd->gesd[i] != NULL)
	    (dd->gesd[i]->callback)(GE_RestoreSnapshotState, dd,
				    VECTOR_ELT(snapshot, i + 1));
    /* Replay the display list
     */
    dd->displayList = duplicate(VECTOR_ELT(snapshot, 0));
    dd->DLlastElt = lastElt(dd->displayList);
    GEplayDisplayList(dd);
    if (!dd->displayListOn) GEinitDisplayList(dd);
}

/* recordPlot() */
SEXP do_getSnapshot(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    return GEcreateSnapshot(GEcurrentDevice());
}

/* replayPlot() */
SEXP do_playSnapshot(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    GEplaySnapshot(CAR(args), GEcurrentDevice());
    return R_NilValue;
}

/****************************************************************
 * do_recordGraphics
 *
 * A ".Internal" R function
 *
 ****************************************************************
 */

SEXP attribute_hidden do_recordGraphics(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, xptr, evalenv, retval;
    pGEDevDesc dd = GEcurrentDevice();
    Rboolean record = dd->recordGraphics;
    /*
     * This function can be run under three conditions:
     *
     *   (i) a top-level call to do_recordGraphics.
     *       In this case, call != R_NilValue and
     *       dd->recordGraphics = TRUE
     *       [so GErecording() returns TRUE]
     *
     *   (ii) a nested call to do_recordGraphics.
     *        In this case, call != R_NilValue but
     *        dd->recordGraphics = FALSE
     *        [so GErecording() returns FALSE]
     *
     *   (iii) a replay of the display list
     *         In this case, call == R_NilValue and
     *         dd->recordGraphics = FALSE
     *         [so GErecording() returns FALSE]
     */
    /*
     * First arg is an expression, second arg is a list, third arg is an env
     */
    SEXP code = CAR(args);
    SEXP list = CADR(args);
    SEXP parentenv = CADDR(args);
    if (!isLanguage(code))
	error(_("'expr' argument must be an expression"));
    if (TYPEOF(list) != VECSXP)
	error(_("'list' argument must be a list"));
    if (isNull(parentenv)) {
	error(_("use of NULL environment is defunct"));
	parentenv = R_BaseEnv;
    } else
    if (!isEnvironment(parentenv))
	error(_("'env' argument must be an environment"));
    /*
     * This conversion of list to env taken from do_eval
     */
    PROTECT(x = VectorToPairList(list));
    for (xptr = x ; xptr != R_NilValue ; xptr = CDR(xptr))
	SET_NAMED(CAR(xptr) , 2);
    /*
     * The environment passed in as the third arg is used as
     * the parent of the new evaluation environment.
     */
    PROTECT(evalenv = NewEnvironment(R_NilValue, x, parentenv));
    dd->recordGraphics = FALSE;
    PROTECT(retval = eval(code, evalenv));
    /*
     * If there is an error or user-interrupt in the above
     * evaluation, dd->recordGraphics is set to TRUE
     * on all graphics devices (see GEonExit(); called in errors.c)
     */
    dd->recordGraphics = record;
    if (GErecording(call, dd)) {
	if (!GEcheckState(dd))
	    error(_("invalid graphics state"));
	GErecordGraphicOperation(op, args, dd);
    }
    UNPROTECT(3);
    return retval;
}

/****************************************************************
 * GEonExit
 *
 * Reset some important graphics state on an error/interrupt
 ****************************************************************
 */

void GEonExit()
{
  /*
   * Run through all devices and turn graphics recording back on
   * in case an error occurred in the middle of a do_recordGraphics
   * call.
   * Awkward cos device code still in graphics.c
   * Can be cleaned up when device code moved here.
   */
    int i, devNum;
    pGEDevDesc gd;
    pDevDesc dd;
    i = 1;
    if (!NoDevices()) {
	devNum = curDevice();
	while (i++ < NumDevices()) {
	    gd = GEgetDevice(devNum);
	    gd->recordGraphics = TRUE;
	    dd = gd->dev;
	    if (dd->onExit) dd->onExit(dd);
	    devNum = nextDevice(devNum);
	}
    }
}

/* This is also used in grid. It may be used millions of times on the
 * same character */
/* FIXME: should we warn on more than one character here? */
int GEstring_to_pch(SEXP pch)
{
    int ipch = NA_INTEGER;
    static SEXP last_pch = NULL;
    static int last_ipch = 0;

    if (pch == NA_STRING) return NA_INTEGER;
    if (CHAR(pch)[0] == 0) return NA_INTEGER;  /* pch = "" */
    if (pch == last_pch) return last_ipch;/* take advantage of CHARSXP cache */
    ipch = (unsigned char) CHAR(pch)[0];
    if (IS_LATIN1(pch)) {
	if (ipch > 127) ipch = -ipch;  /* record as Unicode */
    } else if (IS_UTF8(pch) || utf8locale) {
	wchar_t wc = 0;
	if (ipch > 127) {
	    if ( (int) utf8toucs(&wc, CHAR(pch)) > 0) ipch = -wc;
	    else error(_("invalid multibyte char in pch=\"c\""));
	}
    } else if(mbcslocale) {
	/* Could we safely assume that 7-bit first byte means ASCII?
	   On Windows this only covers CJK locales, so we could.
	 */
	unsigned int ucs = 0;
	if ( (int) mbtoucs(&ucs, CHAR(pch), MB_CUR_MAX) > 0) ipch = ucs;
	else error(_("invalid multibyte char in pch=\"c\""));
	if (ipch > 127) ipch = -ipch;
    }

    last_ipch = ipch; last_pch = pch;
    return ipch;
}

/* moved from graphics.c as used by grid */
/*  LINE TEXTURE CODE */

/*
 *  LINE TEXTURE SPECIFICATION
 *
 *  Linetypes are stored internally in integers.  An integer
 *  is interpreted as containing a sequence of 8 4-bit integers
 *  which give the lengths of up to 8 on-off line segments.
 *  The lengths are typically interpreted as pixels on a screen
 *  and as "points" in postscript.
 *
 *  more comments (and LTY_* def.s) in	../include/Rgraphics.h
 *					----------------------
 */

typedef struct {
    char *name;
    int pattern;
} LineTYPE;

static LineTYPE linetype[] = {
    { "blank",   LTY_BLANK   },/* -1 */
    { "solid",	 LTY_SOLID   },/* 1 */
    { "dashed",	 LTY_DASHED  },/* 2 */
    { "dotted",	 LTY_DOTTED  },/* 3 */
    { "dotdash", LTY_DOTDASH },/* 4 */
    { "longdash",LTY_LONGDASH},/* 5 */
    { "twodash", LTY_TWODASH },/* 6 */
    { NULL,	 0	     },
};

/* Duplicated from graphics.c */
static char HexDigits[] = "0123456789ABCDEF";
static unsigned int hexdigit(int digit)
{
    if('0' <= digit && digit <= '9') return digit - '0';
    if('A' <= digit && digit <= 'F') return 10 + digit - 'A';
    if('a' <= digit && digit <= 'f') return 10 + digit - 'a';
    /*else */ error(_("invalid hex digit in 'color' or 'lty'"));
    return digit; /* never occurs (-Wall) */
}

static int nlinetype = (sizeof(linetype)/sizeof(LineTYPE)-2);

unsigned int GE_LTYpar(SEXP value, int ind)
{
    const char *p;
    int i, code, shift, digit;
    double rcode;

    if(isString(value)) {
	for(i = 0; linetype[i].name; i++) { /* is it the i-th name ? */
	    if(!strcmp(CHAR(STRING_ELT(value, ind)), linetype[i].name))
		return linetype[i].pattern;
	}
	/* otherwise, a string of hex digits: */
	code = 0;
	shift = 0;
	p = CHAR(STRING_ELT(value, ind));
	size_t len = strlen(p);
	if(len < 2 || len > 8 || len % 2 == 1)
	    error(_("invalid line type: must be length 2, 4, 6 or 8"));
	for(; *p; p++) {
	    digit = hexdigit(*p);
	    if(digit == 0)
		error(_("invalid line type: zeroes are not allowed"));
	    code  |= (digit<<shift);
	    shift += 4;
	}
	return code;
    }
    else if(isInteger(value)) {
	code = INTEGER(value)[ind];
	if(code == NA_INTEGER || code < 0)
	    error(_("invalid line type"));
	if (code > 0)
	    code = (code-1) % nlinetype + 1;
	return linetype[code].pattern;
    }
    else if(isReal(value)) {
	rcode = REAL(value)[ind];
	if(!R_FINITE(rcode) || rcode < 0)
	    error(_("invalid line type"));
	code = (int) rcode;
	if (code > 0)
	    code = (code-1) % nlinetype + 1;
	return linetype[code].pattern;
    }
    else {
	error(_("invalid line type")); /*NOTREACHED, for -Wall : */ return 0;
    }
}

SEXP GE_LTYget(unsigned int lty)
{
    int i, ndash;
    unsigned char dash[8];
    unsigned int l;
    char cbuf[17]; /* 8 hex digits plus nul */

    for (i = 0; linetype[i].name; i++)
	if(linetype[i].pattern == lty) return mkString(linetype[i].name);

    l = lty; ndash = 0;
    for (i = 0; i < 8 && l & 15; i++) {
	dash[ndash++] = l & 15;
	l = l >> 4;
    }
    for(i = 0 ; i < ndash ; i++) cbuf[i] = HexDigits[dash[i]];
    return mkString(cbuf);
}

/****************************************************************
 * 
 * Some functions for operations on raster images
 * (for those devices that cannot do these themselves)
 ****************************************************************
 */

/* Some of this code is based on code from the leptonica library
 * hence the following notice 
 */

/*====================================================================*
-  Copyright (C) 2001 Leptonica.  All rights reserved.
-  This software is distributed in the hope that it will be
-  useful, but with NO WARRANTY OF ANY KIND.
-  No author or distributor accepts responsibility to anyone for the
-  consequences of using this software, or for whether it serves any
-  particular purpose or works at all, unless he or she says so in
-  writing.  Everyone is granted permission to copy, modify and
-  redistribute this source code, for commercial or non-commercial
-  purposes, with the following restrictions: (1) the origin of this
-  source code must not be misrepresented; (2) modified versions must
-  be plainly marked as such; and (3) this notice may not be removed
-  or altered from any source or modified source distribution.
*====================================================================*/

/* 
 * Scale a raster image to a desired size using 
 * nearest-neighbour interpolation

 * draster must be pre-allocated.
 */
void R_GE_rasterScale(unsigned int *sraster, int sw, int sh,
                      unsigned int *draster, int dw, int dh) {
    int i, j;
    int sx, sy;
    unsigned int pixel;

    /* Iterate over the destination pixels */
    for (i = 0; i < dh; i++) {
        for (j = 0; j < dw; j++) {
            sy = i * sh / dh;
            sx = j * sw / dw;
            if ((sx >= 0) && (sx < sw) && (sy >= 0) && sy < sh) {
                pixel = sraster[sy * sw + sx];
            } else {
                pixel = 0;
            }
            draster[i * dw + j] = pixel;
        }
    }
}

/* 
 * Scale a raster image to a desired size using 
 * bilinear interpolation
 * Code based on scaleColorLILow() from leptonica library

 *  Divide each destination pixel into 16 x 16 sub-pixels.
 *  Linear interpolation is equivalent to finding the 
 *  fractional area (i.e., number of sub-pixels divided
 *  by 256) associated with each of the four nearest src pixels,
 *  and weighting each pixel value by this fractional area.

 * draster must be pre-allocated.
 */
void R_GE_rasterInterpolate(unsigned int *sraster, int sw, int sh,
                            unsigned int *draster, int dw, int dh) {
    int i, j;
    double scx, scy;
    int wm2, hm2;
    int xpm, ypm;  /* location in src image, to 1/16 of a pixel */
    int xp, yp, xf, yf;  /* src pixel and pixel fraction coordinates */
    int v00r, v01r, v10r, v11r, v00g, v01g, v10g, v11g;
    int v00b, v01b, v10b, v11b, v00a, v01a, v10a, v11a;
    int area00, area01, area10, area11;
    unsigned int pixels1, pixels2, pixels3, pixels4, pixel;
    unsigned int *sline, *dline;

    /* (scx, scy) are scaling factors that are applied to the
     * dest coords to get the corresponding src coords.
     * We need them because we iterate over dest pixels
     * and must find the corresponding set of src pixels. */
    scx = (16. * sw) / dw;
    scy = (16. * sh) / dh;

    wm2 = sw - 2;
    hm2 = sh - 2;

    /* Iterate over the destination pixels */
    for (i = 0; i < dh; i++) {
        ypm = (int) fmax2(scy * i - 8, 0);
        yp = ypm >> 4;
        yf = ypm & 0x0f;
        dline = draster + i * dw;
        sline = sraster + yp * sw;
        for (j = 0; j < dw; j++) {
            xpm = (int) fmax2(scx * j - 8, 0);
            xp = xpm >> 4;
            xf = xpm & 0x0f;

            pixels1 = *(sline + xp);

            if (xp > wm2 || yp > hm2) {
                if (yp > hm2 && xp <= wm2) {  /* pixels near bottom */
                    pixels2 = *(sline + xp + 1);
                    pixels3 = pixels1;
                    pixels4 = pixels2;
                }
                else if (xp > wm2 && yp <= hm2) {  /* pixels near right side */
                    pixels2 = pixels1;
                    pixels3 = *(sline + sw + xp);
                    pixels4 = pixels3;
                }
                else {  /* pixels at LR corner */
                    pixels4 = pixels3 = pixels2 = pixels1;
                }
            }
            else {
                pixels2 = *(sline + xp + 1);
                pixels3 = *(sline + sw + xp);
                pixels4 = *(sline + sw + xp + 1);
            }

            area00 = (16 - xf) * (16 - yf);
            area10 = xf * (16 - yf);
            area01 = (16 - xf) * yf;
            area11 = xf * yf;
            v00r = area00 * R_RED(pixels1);
            v00g = area00 * R_GREEN(pixels1);
            v00b = area00 * R_BLUE(pixels1);
            v00a = area00 * R_ALPHA(pixels1);
            v10r = area10 * R_RED(pixels2);
            v10g = area10 * R_GREEN(pixels2);
            v10b = area10 * R_BLUE(pixels2);
            v10a = area10 * R_ALPHA(pixels2);
            v01r = area01 * R_RED(pixels3);
            v01g = area01 * R_GREEN(pixels3);
            v01b = area01 * R_BLUE(pixels3);
            v01a = area01 * R_ALPHA(pixels3);
            v11r = area11 * R_RED(pixels4);
            v11g = area11 * R_GREEN(pixels4);
            v11b = area11 * R_BLUE(pixels4);
            v11a = area11 * R_ALPHA(pixels4);
            pixel = (((v00r + v10r + v01r + v11r + 128) >>  8) & 0x000000ff) |
                    (((v00g + v10g + v01g + v11g + 128)      ) & 0x0000ff00) |
                    (((v00b + v10b + v01b + v11b + 128) <<  8) & 0x00ff0000) |
                    (((v00a + v10a + v01a + v11a + 128) << 16) & 0xff000000);
            *(dline + j) = pixel;
        }
    }
}

/*
 * Calculate the size needed for rotated image
 *
 * Rotate top-right and bottom-right corners
 * New width/height based on max of rotated corners
 */
void R_GE_rasterRotatedSize(int w, int h, double angle,
                            int *wnew, int *hnew) {
    double diag = sqrt(w*w + h*h);
    double theta = atan2((double) h, (double) w);
    double trx1 = diag*cos(theta + angle);
    double trx2 = diag*cos(theta - angle);
    double try1 = diag*sin(theta + angle);
    double try2 = diag*sin(angle - theta);
    *wnew = (int) (fmax2(fabs(trx1), fabs(trx2)) + 0.5);
    *hnew = (int) (fmax2(fabs(try1), fabs(try2)) + 0.5);
    /* 
     * Rotated image may be shorter or thinner than original
     */
    *wnew = imax2(w, *wnew);
    *hnew = imax2(h, *hnew);
}

/*
 * Calculate offset for (left, bottom) or 
 * (left, top) of image 
 * to account for image rotation
 */
void R_GE_rasterRotatedOffset(int w, int h, double angle,
                              int botleft,
                              double *xoff, double *yoff) {
    double hypot = .5*sqrt(w*w + h*h);
    double theta, dw, dh;
    if (botleft) {
        theta = M_PI + atan2(h, w);
        dw = hypot*cos(theta + angle);
        dh = hypot*sin(theta + angle);
        *xoff = dw + w/2;
        *yoff = dh + h/2;
    } else {
        theta = -M_PI - atan2(h, w);
        dw = hypot*cos(theta + angle);
        dh = hypot*sin(theta + angle);
        *xoff = dw + w/2;
        *yoff = dh - h/2;
    }
}

/* 
 * Copy a raster image into the middle of a larger 
 * raster image (ready for rotation)

 * newRaster must be pre-allocated.
 */
void R_GE_rasterResizeForRotation(unsigned int *sraster, 
                                  int w, int h, 
                                  unsigned int *newRaster,
                                  int wnew, int hnew,
                                  const pGEcontext gc)
{
    int i, j, inew, jnew;
    int xoff = (wnew - w)/2;
    int yoff = (hnew - h)/2;

    for (i=0; i<hnew; i++) {
        for (j=0; j<wnew; j++) {
            newRaster[i*wnew + j] = gc->fill;
        }
    }
    for (i=0; i<h; i++) {
        for (j=0; j<w; j++) {
            inew = i+yoff;
            jnew = j+xoff;
            newRaster[inew*wnew + jnew] = sraster[i*w + j];
        }

    }
}

/* 
 * Rotate a raster image 
 * Code based on rotateAMColorLow() from leptonica library

 * draster must be pre-allocated.
 
 * smoothAlpha allows alpha channel to vary smoothly based on 
 * interpolation.  If this is FALSE, then alpha values are 
 * taken from MAX(alpha) of relevant pixels.  This means that
 * areas of full transparency remain fully transparent, 
 * areas of opacity remain opaque, edges between anything less than opacity
 * and opacity are opaque, and edges between full transparency
 * and semitransparency become semitransparent.
 */
void R_GE_rasterRotate(unsigned int *sraster, int w, int h, double angle,
                       unsigned int *draster, const pGEcontext gc,
                       Rboolean smoothAlpha) {
    int i, j;
    int xcen, ycen, wm2, hm2;
    int xdif, ydif, xpm, ypm, xp, yp, xf, yf;
    int rval, gval, bval, aval;
    unsigned int word00, word01, word10, word11;
    unsigned int *sline, *dline;
    double sina, cosa;

    /* 'angle' in leptonica is clockwise */
    angle = -angle;

    xcen = w / 2;
    wm2 = w - 2;
    ycen = h / 2;
    hm2 = h - 2;
    sina = 16. * sin(angle);
    cosa = 16. * cos(angle);

    for (i = 0; i < h; i++) {
        ydif = ycen - i;
        dline = draster + i * w;
        for (j = 0; j < w; j++) {
            xdif = xcen - j;
            xpm = (int) (-xdif * cosa - ydif * sina);
            ypm = (int) (-ydif * cosa + xdif * sina);
            xp = xcen + (xpm >> 4);
            yp = ycen + (ypm >> 4);
            xf = xpm & 0x0f;
            yf = ypm & 0x0f;

                /* if off the edge, use transparent */
            if (xp < 0 || yp < 0 || xp > wm2 || yp > hm2) {
                *(dline + j) = gc->fill;
                continue;
            }

            sline = sraster + yp * w;

            word00 = *(sline + xp);
            word10 = *(sline + xp + 1);
            word01 = *(sline + w + xp);
            word11 = *(sline + w + xp + 1);
            rval = ((16 - xf) * (16 - yf) * R_RED(word00) +
                    xf * (16 - yf) * R_RED(word10) +
                    (16 - xf) * yf * R_RED(word01) +
                    xf * yf * R_RED(word11) + 128) / 256;
            gval = ((16 - xf) * (16 - yf) * R_GREEN(word00) +
                    xf * (16 - yf) * R_GREEN(word10) +
                    (16 - xf) * yf * R_GREEN(word01) +
                    xf * yf * R_GREEN(word11) + 128) / 256;
            bval = ((16 - xf) * (16 - yf) * R_BLUE(word00) +
                    xf * (16 - yf) * R_BLUE(word10) +
                    (16 - xf) * yf * R_BLUE(word01) +
                    xf * yf * R_BLUE(word11) + 128) / 256;
            if (smoothAlpha) {
                aval = ((16 - xf) * (16 - yf) * R_ALPHA(word00) +
                        xf * (16 - yf) * R_ALPHA(word10) +
                        (16 - xf) * yf * R_ALPHA(word01) +
                        xf * yf * R_ALPHA(word11) + 128) / 256;
            } else {
                aval = (int)fmax2(fmax2(R_ALPHA(word00), R_ALPHA(word10)),
				  fmax2(R_ALPHA(word01), R_ALPHA(word11)));
            }
            *(dline + j) = R_RGBA(rval, gval, bval, aval);
        }
    }
}
