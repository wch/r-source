/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001   The R Development Core Team.
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
/* REALLY shouldn't have to include Graphics.h once engine.h is
 * properly set up
 */
#include <Graphics.h>
#include <Rdevices.h>
#include <R_ext/Applic.h>	/* pretty0() */
#include <Rmath.h>

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
 * GEcreateDevDesc
 ****************************************************************
 */

/* Create a GEDevDesc, given a NewDevDesc*
 */
GEDevDesc* GEcreateDevDesc(NewDevDesc* dev)
{
    /* Wrap the device description within a graphics engine
     * device description (add graphics engine information
     * to the device description).
     */
    GEDevDesc *dd = (GEDevDesc*) calloc(1, sizeof(GEDevDesc));
    /* NULL the gesd array
     */
    int i;
    for (i=0; i<MAX_GRAPHICS_SYSTEMS; i++)
	dd->gesd[i] = NULL;
    if (!dd)
	error("Not enough memory to allocate device (in addDevice)");
    dd->newDevStruct = 1;
    dd->dev = dev;
    return dd;
}

/****************************************************************
 * GEdestroyDevDesc
 ****************************************************************
 */

static void unregisterOne(GEDevDesc *dd, int systemNumber) {
    if (dd->gesd[systemNumber] != NULL) {
	(dd->gesd[systemNumber]->callback)(GE_FinaliseState, dd, 
					   R_NilValue);
	free(dd->gesd[systemNumber]);
	dd->gesd[systemNumber] = NULL;
    }
}

/* NOTE that the NewDevDesc* dev has been shut down by a call
 * to dev->close within graphics.c
 */
void GEdestroyDevDesc(GEDevDesc* dd)
{
    int i;
    if (dd != NULL) {
	for (i=0; i<numGraphicsSystems; i++)
	    unregisterOne(dd, i);
	free(dd->dev);
	dd->dev = NULL;
	free(dd);
    }
}

/****************************************************************
 * GEsystemState
 ****************************************************************
 */

void* GEsystemState(GEDevDesc *dd, int index)
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
static void registerOne(GEDevDesc *dd, int systemNumber, GEcallback cb) {
    dd->gesd[systemNumber] =
	(GESystemDesc*) calloc(1, sizeof(GESystemDesc));
    if (dd->gesd[systemNumber] == NULL)
	error("unable to allocate memory (in GEregister)");
    cb(GE_InitState, dd, R_NilValue);
    dd->gesd[systemNumber]->callback = cb;
}

/* Store the graphics system state and callback information
 * for a specified device.
 * This is called when a new device is created.
 */
void GEregisterWithDevice(GEDevDesc *dd) {
    int i;
    for (i=0; i<numGraphicsSystems; i++)
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
    /* Bit awkward, but I leave GetDevice to return DevDesc for now
     * Can be fixed once device handling code is in here rather than
     * in graphics.c
     */
    DevDesc *dd;
    if (numGraphicsSystems + 1 == MAX_GRAPHICS_SYSTEMS)
	error("Too many graphics systems registered");
    /* Set the system register index so that, if there are existing
     * devices, it will know where to put the system-specific 
     * information in those devices
     */
    *systemRegisterIndex = numGraphicsSystems;
    /* Run through the existing devices and add the new information
     * to any GEDevDesc's
     */
    i = 1;
    if (!NoDevices()) {
	devNum = curDevice();
	while (i++ < NumDevices()) {
	    dd = GetDevice(devNum);
	    /* FIXME:  won't need this check once engine.c has
	     * replaced graphics.c
	     */
	    if (dd->newDevStruct) {
		registerOne((GEDevDesc*) dd, numGraphicsSystems, cb);
	    }
	    devNum = nextDevice(devNum);
	}
    }
    /* Store the information for adding to any new devices
     */
    registeredSystems[numGraphicsSystems] =
	(GESystemDesc*) calloc(1, sizeof(GESystemDesc));
    if (registeredSystems[numGraphicsSystems] == NULL)
	error("unable to allocate memory (in GEregister)");
    registeredSystems[numGraphicsSystems]->callback = cb;
    numGraphicsSystems += 1;
}

/****************************************************************
 * GEunregisterSystem
 ****************************************************************
 */

void GEunregisterSystem(int registerIndex)
{
    int i, devNum;
    /* Bit awkward, but I leave GetDevice to return DevDesc for now
     * Can be fixed once device handling code is in here rather than
     * in graphics.c
     */
    DevDesc *dd;

    /* safety check if called before Ginit() */
    if(registerIndex < 0) return;
    if (numGraphicsSystems == 0)
	error("No graphics system to unregister");
    /* Run through the existing devices and remove the information
     * from any GEDevDesc's
     */
    i = 1;
    if (!NoDevices()) {
	devNum = curDevice();
	while (i++ < NumDevices()) {
	    dd = GetDevice(devNum);
	    /* FIXME:  won't need this check once engine.c has
	     * replaced graphics.c
	     */
	    if (dd->newDevStruct) {
		unregisterOne((GEDevDesc*) dd, registerIndex);
	    }
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
    /* NOTE that I deliberately do not decrease the number of
     * registered graphics systems.  This means that unloading
     * a graphics system will create a "hole" in the global
     * record, but otherwise I have to assume that graphics
     * systems are unloaded in the reverse order from that which
     * they were loaded, which may be unreasonable.
     * The downside to this approach is that if you unload and
     * reload graphics systems you will run out of room in the
     * global record -- I'm assuming that unloading and reloading
     * of graphics systems is something that won't happen that
     * many times in a session.
     * Hopefully, all of these problems will go away when I get
     * around to storing the device structures in SEXP's
     */
}

/****************************************************************
 * GEHandleEvent
 ****************************************************************
 */

/* This guy can be called by device drivers.
 * It calls back to registered graphics systems and passes on the event
 * so that the graphics systems can respond however they want to.
 */
SEXP GEHandleEvent(GEevent event, NewDevDesc *dev, SEXP data)
{
    int i;
    DevDesc* dd = GetDevice(devNumber((DevDesc*) dev));
    for (i=0; i<numGraphicsSystems; i++)
	if (registeredSystems[i] != NULL)
	    (registeredSystems[i]->callback)(event, (GEDevDesc*) dd, 
					     data);
    return R_NilValue;
}

/****************************************************************
 * Some graphics engine transformations
 ****************************************************************
 */

double fromDeviceX(double value, GEUnit to, GEDevDesc *dd)
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

double toDeviceX(double value, GEUnit from, GEDevDesc *dd)
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

double fromDeviceY(double value, GEUnit to, GEDevDesc *dd)
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

double toDeviceY(double value, GEUnit from, GEDevDesc *dd)
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

double fromDeviceWidth(double value, GEUnit to, GEDevDesc *dd)
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

double toDeviceWidth(double value, GEUnit from, GEDevDesc *dd)
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

double fromDeviceHeight(double value, GEUnit to, GEDevDesc *dd)
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

double toDeviceHeight(double value, GEUnit from, GEDevDesc *dd)
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
 * Code to retrieve current clipping rect from device
 ****************************************************************
 */

static void getClipRect(double *x1, double *y1, double *x2, double *y2,
                        GEDevDesc *dd)
{
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
				GEDevDesc *dd)
{
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
void GESetClip(double x1, double y1, double x2, double y2, GEDevDesc *dd)
{
    dd->dev->clip(x1, x2, y1, y2, dd->dev);
    /* This needs to be uncommented once R 1.4 is out 
     * Also take the setting of these values out of devX11.c
     * The problem is that these settings are not happening in
     * devices other than X11
     */
    /*    dd->dev->clipLeft = fmin2(x1, x2);
     *    dd->dev->clipRight = fmax2(x1, x2);
     *    dd->dev->clipTop = fmax2(y1, y2);
     *    dd->dev->clipBottom = fmin2(y1, y2); 
     */
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
	   GEDevDesc *dd)
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
	 int toDevice, GEDevDesc *dd)
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
	    int col, double gamma, int lty, double lwd,
	    GEDevDesc *dd)
{
    Rboolean clip_ok;
    if (lty == LTY_BLANK) return;
    if (dd->dev->canClip) {
	clip_ok = clipLine(&x1, &y1, &x2, &y2, 1, dd);
    }
    else {
	clip_ok = clipLine(&x1, &y1, &x2, &y2, 0, dd);
    }
    if (clip_ok)
	dd->dev->line(x1, y1, x2, y2, col, gamma, lty, lwd, dd->dev);
}

/****************************************************************
 * R code for clipping polylines
 ****************************************************************
 */

static void CScliplines(int n, double *x, double *y,
			int col, double gamma, int lty, double lwd,
			int toDevice, GEDevDesc *dd)
{
    int ind1, ind2;
    /*int firstPoint = 1;*/
    int count = 0;
    int i = 0;
    double *xx, *yy;
    double x1, y1, x2, y2;
    cliprect cr;
    char *vmax = vmaxget();

    if (toDevice)
	getClipRectToDevice(&cr.xl, &cr.yb, &cr.xr, &cr.yt, dd);
    else
	getClipRect(&cr.xl, &cr.yb, &cr.xr, &cr.yt, dd);

    xx = (double *) R_alloc(n, sizeof(double));
    yy = (double *) R_alloc(n, sizeof(double));
    if (xx == NULL || yy == NULL)
	error("out of memory while clipping polyline");

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
		dd->dev->polyline(2, xx, yy, col, gamma, lty, lwd, dd->dev);
	    }
	    else if (ind1) {
		xx[0] = x1;
		yy[0] = y1;
		xx[1] = x2;
		yy[1] = y2;
		count = 2;
		if (i == n - 1)
		    dd->dev->polyline(count, xx, yy, col, gamma, 
				      lty, lwd, dd->dev);
	    }
	    else if (ind2) {
		xx[count] = x2;
		yy[count] = y2;
		count++;
		if (count > 1)
		    dd->dev->polyline(count, xx, yy, col, gamma, 
				      lty, lwd, dd->dev);
	    }
	    else {
		xx[count] = x2;
		yy[count] = y2;
		count++;
		if (i == n - 1 && count > 1)
		    dd->dev->polyline(count, xx, yy, col, gamma, 
				      lty, lwd, dd->dev);
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
			 int col, double gamma, int lty, double lwd,
			 int clipToDevice, GEDevDesc *dd)
{
    CScliplines(n, x, y, col, gamma, lty, lwd, clipToDevice, dd);
}

/* Draw a series of line segments. */
/* If the device canClip, R clips to the device extent and the device
   does all other clipping */
void GEPolyline(int n, double *x, double *y,
		int col, double gamma, int lty, double lwd,
		GEDevDesc *dd)
{
    if (lty == LTY_BLANK) return;
    if (dd->dev->canClip) {
	clipPolyline(n, x, y, col, gamma, 
		     lty, lwd, 1, dd);  /* clips to device extent
						  then draws */
    }
    else
	clipPolyline(n, x, y, col, gamma, lty, lwd, 0, dd);
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
    double ix, iy;

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
    double ix, iy;
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

int clipPoly(double *x, double *y, int n, int store, int toDevice,
	     double *xout, double *yout, GEDevDesc *dd)
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
			int col, int fill, double gamma, int lty, double lwd,
                        int toDevice, GEDevDesc *dd)
{
    double *xc = NULL, *yc = NULL;
    /* if bg not specified then draw as polyline rather than polygon
     * to avoid drawing line along border of clipping region */
    if (fill == NA_INTEGER) {
	int i;
	xc = (double*) R_alloc(n + 1, sizeof(double));
	yc = (double*) R_alloc(n + 1, sizeof(double));
	for (i=0; i<n; i++) {
	    xc[i] = x[i];
	    yc[i] = y[i];
	}
	xc[n] = x[0];
	yc[n] = y[0];
	GEPolyline(n+1, xc, yc, col, gamma, lty, lwd, dd);
    }
    else {
	int npts;
	xc = yc = 0;		/* -Wall */
	npts = clipPoly(x, y, n, 0, toDevice, xc, yc, dd);
	if (npts > 1) {
	    xc = (double*) R_alloc(npts, sizeof(double));
	    yc = (double*) R_alloc(npts, sizeof(double));
	    npts = clipPoly(x, y, n, 1, toDevice, xc, yc, dd);
	    dd->dev->polygon(npts, xc, yc, col, fill, gamma, 
			     lty, lwd, dd->dev);
	}
    }
}

/****************************************************************
 * GEPolygon
 ****************************************************************
 */
void GEPolygon(int n, double *x, double *y,
	       int col, int fill, double gamma, int lty, double lwd,
	       GEDevDesc *dd)
{
    if (lty == LTY_BLANK) return;
    if (dd->dev->canClip) {
	clipPolygon(n, x, y, col, fill, gamma, lty, lwd, 1, dd);
    }
    else
	clipPolygon(n, x, y, col, fill, gamma, lty, lwd, 0, dd);
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
			  int toDevice, GEDevDesc *dd)
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

	    result = (r <= 6) ? 10 : 2 * M_PI/acos(1 - 1/r) ;
	}
    }
    return result;
}

/****************************************************************
 * GECircle
 ****************************************************************
 */
void GECircle(double x, double y, double radius,
	     int col, int fill, double gamma, int lty, double lwd,
	     GEDevDesc *dd)
{
    char *vmax;
    double *xc, *yc;
    int result;

    result = clipCircleCode(x, y, radius, !dd->dev->canClip, dd);

    switch (result) {
    case -2: /* No clipping;  draw all of circle */
	dd->dev->circle(x, y, radius, col, fill, gamma, lty, lwd, dd->dev);
	break;
    case -1: /* Total clipping; draw nothing */
	break;
    default: /* Partial clipping; draw poly[line|gon] */
	result = clipCircleCode(x, y, radius, !dd->dev->canClip, dd);
	if (dd->dev->canClip && result == -2) {
	    dd->dev->circle(x, y, radius, col, fill, gamma, lty, lwd, dd->dev);
	}
	else {
	    vmax = vmaxget();
	    xc = (double*)R_alloc(result+1, sizeof(double));
	    yc = (double*)R_alloc(result+1, sizeof(double));
	    convertCircle(x, y, radius, result, xc, yc);
	    if (fill == NA_INTEGER) {
		GEPolyline(result+1, xc, yc, col, gamma, lty, lwd, dd);
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
		    dd->dev->polygon(npts, xcc, ycc, col, fill, gamma, 
				     lty, lwd, dd->dev);
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
			int toDevice, GEDevDesc *dd)
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
/* These may both be NA_INTEGER	 */
void GERect(double x0, double y0, double x1, double y1,
	    int col, int fill, double gamma, int lty, double lwd,
	    GEDevDesc *dd)
{
    char *vmax;
    double *xc, *yc;
    int result;

    result = clipRectCode(x0, y0, x1, y1, !dd->dev->canClip, dd);
    switch (result) {
    case 0:  /* rectangle totally clipped; draw nothing */
	break;
    case 1:  /* rectangle totally inside;  draw all */
	dd->dev->rect(x0, y0, x1, y1, col, fill, gamma, 
		      lty, lwd, dd->dev);
	break;
    case 2:  /* rectangle intersects clip region;  use polygon clipping */
	result = clipRectCode(x0, y0, x1, y1, !dd->dev->canClip, dd);
	if (result == 1)
	    dd->dev->rect(x0, y0, x1, y1, col, fill, gamma, lty, lwd, dd->dev);
	else {
	    vmax = vmaxget();
	    xc = (double*)R_alloc(5, sizeof(double));
	    yc = (double*)R_alloc(5, sizeof(double));
	    xc[0] = x0; yc[0] = y0;
	    xc[1] = x0; yc[1] = y1;
	    xc[2] = x1; yc[2] = y1;
	    xc[3] = x1; yc[3] = y0;
	    xc[4] = x0; yc[4] = y0;
	    if (fill == NA_INTEGER) {
		GEPolyline(5, xc, yc, col, gamma, lty, lwd, dd);
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
		    dd->dev->polygon(npts, xcc, ycc, col, fill, gamma, 
				     lty, lwd, dd->dev);
		}
	    }
	    vmaxset(vmax);
	}
    }
}

/****************************************************************
 * R code for clipping text
 ****************************************************************
 */

/* Return a code indicating how the text should be clipped
   NOTE that x, y indicate the bottom-left of the text
   NOTE also that x and y are in GE_INCHES
   NOTE also also that this is a bit crude because it actually uses
   a bounding box for the entire text to determine the clipping code.
   This will mean that in certain (very rare ?) cases, a piece of
   text will be characterised as intersecting with the clipping region
   when in fact it lies totally outside the clipping region.  But
   this is not a problem because the final output will still be correct.
   0 means totally outside clip region
   1 means totally inside clip region
   2 means intersects clip region */
static int clipTextCode(double x, double y, char *str,
			double rot, double hadj,
			int font, double cex, double ps,
			int toDevice, GEDevDesc *dd)
{
    double x0, x1, x2, x3, y0, y1, y2, y3, left, right, bottom, top;
    double angle = DEG2RAD * rot;
    double theta1 = M_PI/2 - angle;
    double width = fromDeviceWidth(GEStrWidth(str, font, cex, ps, dd),
				   GE_INCHES, dd);
    double height = fromDeviceHeight(GEStrHeight(str, font, cex, ps, dd),
				     GE_INCHES, dd);
#ifdef HAVE_HYPOT
    double length = hypot(width, height);
#else
    double length = pythag(width, height);
#endif
    double theta2 = angle + atan2(height, width);
    x -= hadj*width*cos(angle);
    y -= hadj*width*sin(angle);
    x0 = x + height*cos(theta1);
    x1 = x;
    x2 = x + length*cos(theta2);
    x3 = x + width*cos(angle);
    y0 = y + height*sin(theta1);
    y1 = y;
    y2 = y + length*sin(theta2);
    y3 = y + width*sin(angle);
    left = fmin2(fmin2(x0, x1), fmin2(x2, x3));
    right = fmax2(fmax2(x0, x1), fmax2(x2, x3));
    bottom = fmin2(fmin2(y0, y1), fmin2(y2, y3));
    top = fmax2(fmax2(y0, y1), fmax2(y2, y3));
    return clipRectCode(left, bottom, right, top, toDevice, dd);
}

static void clipText(double x, double y, char *str, double rot, double hadj,
		     int col, double gamma, int font, double cex, double ps,
		     int toDevice, GEDevDesc *dd)
{
    int result = clipTextCode(x, y, str, rot, hadj, font, cex, ps,
			      toDevice, dd);
    switch (result) {
    case 0:  /* text totally clipped; draw nothing */
	break;
    case 1:  /* text totally inside;  draw all */
	dd->dev->text(x, y, str, rot, hadj, col, gamma, 
		      font, cex, ps, dd->dev);
	break;
    case 2:  /* text intersects clip region
		act according to value of clipToDevice */
	if (toDevice) /* Device will do clipping */
	    dd->dev->text(x, y, str, rot, hadj, col, gamma, 
			  font, cex, ps, dd->dev);
	else /* don't draw anything; this could be made less crude :) */
	    ;
    }
}

/****************************************************************
 * GEText
 ****************************************************************
 */
/* If you want EXACT centering of text (e.g., like in GSymbol) */
/* then pass NA_REAL for xc and yc */
void GEText(double x, double y, char *str,
	   double xc, double yc, double rot,
	   int col, double gamma, int font, double cex, double ps,
	   GEDevDesc *dd)
{
    char *sbuf = NULL;
    if(str && *str) {
        char *s, *sb;
	int i, n;
	double xoff, yoff, hadj;
	double sin_rot, cos_rot;/* sin() & cos() of rot{ation} in radians */
	double xleft, ybottom;
	/* We work in GE_INCHES */
	x = fromDeviceX(x, GE_INCHES, dd);
	y = fromDeviceY(y, GE_INCHES, dd);
	/* Count the lines of text */
	n = 1;
        for(s = str; *s ; s++)
            if (*s == '\n')
		n += 1;
	/* Allocate a temporary buffer */
	sbuf = (char*) R_alloc(strlen(str) + 1, sizeof(char));
	sb = sbuf;
	i = 0;
	sin_rot = DEG2RAD * rot;
	cos_rot = cos(sin_rot);
	sin_rot = sin(sin_rot);
        for(s = str; ; s++) {
            if (*s == '\n' || *s == '\0') {
		*sb = '\0';
		if (n > 1) {
		    /* first determine location of THIS line */
		    if (!R_FINITE(xc))
			xc = 0.5;
		    if (!R_FINITE(yc))
			yc = 0.5;
		    yoff = (1 - yc)*(n - 1) - i;
		    yoff = fromDeviceHeight(yoff * cex * dd->dev->cra[1],
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
		if(xc != 0.0 || yc != 0) {
		    double width, height;
		    width = fromDeviceWidth(GEStrWidth(sbuf, font,
						       cex, ps, dd),
					    GE_INCHES, dd);
		    if (!R_FINITE(xc))
			xc = 0.5;
		    if (!R_FINITE(yc)) {
			/* "exact" vertical centering */
			/* If font metric info is available AND */
			/* there is only one line, use GMetricInfo & yc=0.5 */
			/* Otherwise use GEStrHeight and fiddle yc */
			double h, d, w;
			GEMetricInfo(0, font, cex, ps, &h, &d, &w, dd);
			if (n>1 || (h==0 && d==0 && w==0)) {
			    height = fromDeviceHeight(GEStrHeight(sbuf, font,
								  cex, ps, dd),
						      GE_INCHES, dd);
			    yc = dd->dev->yCharOffset;
			} else {
			    double maxHeight = 0.0;
			    double maxDepth = 0.0;
			    char *ss;
			    int charNum = 0;
			    h = fromDeviceHeight(h, GE_INCHES, dd);
			    d = fromDeviceHeight(d, GE_INCHES, dd);
			    for (ss=sbuf; *ss; ss++) {
				GEMetricInfo((unsigned char) *ss,
					     font, cex, ps, &h, &d, &w, dd);
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
			    height = maxHeight - maxDepth;
			    yc = 0.5;
			}
		    } else {
			height = fromDeviceHeight(GEStrHeight(sbuf, font,
							      cex, ps, dd),
						  GE_INCHES, dd);
		    }
		    if (dd->dev->canHAdj == 2) hadj = xc;
		    else if (dd->dev->canHAdj == 1) {
			hadj = 0.5 * floor(2*xc + 0.5);
			/* limit to 0, 0.5, 1 */
			hadj = (hadj > 1.0) ? 1.0 :((hadj < 0.0) ? 0.0 : hadj);
		    } else hadj = 0.0;
		    xleft = xoff - (xc-hadj)*width*cos_rot + yc*height*sin_rot;
		    ybottom= yoff - (xc-hadj)*width*sin_rot -
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
		if(dd->dev->canClip) {
		    clipText(xleft, ybottom, sbuf, rot, hadj,
			     col, gamma, font, cex, ps, 1, dd);
		} else
		    clipText(xleft, ybottom, sbuf, rot, hadj,
			     col, gamma, font, cex, ps, 0, dd);
		sb = sbuf;
		i += 1;
	    }
	    else *sb++ = *s;
	    if (!*s) break;
	}
    }
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
void GEMode(int mode, GEDevDesc *dd)
{
    if (NoDevices())
	error("No graphics device is active");
    dd->dev->mode(mode, dd->dev);
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
	      int col, int fill, double gamma, double lty, double lwd,
	      int font, double cex, double ps,
	      GEDevDesc *dd)
{
    double r, xc, yc;
    double xx[4], yy[4];
    char str[2];

    /* Special cases for plotting pch="." or pch=<character>
     */
    if(' ' <= pch && pch <= 255) {
	if (pch == '.') {
	    GERect(x-.5, y-.5, x+.5, y+.5, col, NA_INTEGER, gamma, 
		   lty, lwd, dd);
	} else {
	    str[0] = pch;
	    str[1] = '\0';
	    GEText(x, y, str, NA_REAL, NA_REAL, 0., col, gamma, 
		   font, cex, ps, dd);
	}
    }
    else {
	double GSTR_0 = fromDeviceWidth(size, GE_INCHES, dd);

	switch(pch) {

	case 0: /* S square */
	    xc = toDeviceWidth(RADIUS * GSTR_0, GE_INCHES, dd);
	    yc = toDeviceHeight(RADIUS * GSTR_0, GE_INCHES, dd);
	    GERect(x-xc, y-yc, x+xc, y+yc,
		   NA_INTEGER, col, gamma, lty, lwd, dd);
	    break;

	case 1: /* S octahedron ( circle) */
	    xc = RADIUS * size;
	    GECircle(x, y, xc, col, NA_INTEGER, gamma, lty, lwd, dd);
	    break;

	case 2:	/* S triangle - point up */
	    xc = RADIUS * GSTR_0;
	    r = toDeviceHeight(TRC0 * xc, GE_INCHES, dd);
	    yc = toDeviceHeight(TRC2 * xc, GE_INCHES, dd);
	    xc = toDeviceWidth(TRC1 * xc, GE_INCHES, dd);
	    xx[0] = x; yy[0] = y+r;
	    xx[1] = x+xc; yy[1] = y-yc;
	    xx[2] = x-xc; yy[2] = y-yc;
	    GEPolygon(3, xx, yy, col, NA_INTEGER, gamma, lty, lwd, dd);
	    break;

	case 3: /* S plus */
	    xc = toDeviceWidth(M_SQRT2*RADIUS*GSTR_0, GE_INCHES, dd);
	    yc = toDeviceHeight(M_SQRT2*RADIUS*GSTR_0, GE_INCHES, dd);
	    GELine(x-xc, y, x+xc, y, col, gamma, lty, lwd, dd);
	    GELine(x, y-yc, x, y+yc, col, gamma, lty, lwd, dd);
	    break;

	case 4: /* S times */
	    xc = toDeviceWidth(RADIUS * GSTR_0, GE_INCHES, dd);
	    yc = toDeviceHeight(RADIUS * GSTR_0, GE_INCHES, dd);
	    GELine(x-xc, y-yc, x+xc, y+yc, col, gamma, lty, lwd, dd);
	    GELine(x-xc, y+yc, x+xc, y-yc, col, gamma, lty, lwd, dd);
	    break;

	case 5: /* S diamond */
	    xc = toDeviceWidth(M_SQRT2 * RADIUS * GSTR_0, GE_INCHES, dd);
	    yc = toDeviceHeight(M_SQRT2 * RADIUS * GSTR_0, GE_INCHES, dd);
	    xx[0] = x-xc; yy[0] = y;
	    xx[1] = x; yy[1] = y+yc;
	    xx[2] = x+xc; yy[2] = y;
	    xx[3] = x; yy[3] = y-yc;
	    GEPolygon(4, xx, yy, col, NA_INTEGER, gamma, lty, lwd, dd);
	    break;

	case 6: /* S triangle - point down */
	    xc = RADIUS * GSTR_0;
	    r = toDeviceHeight(TRC0 * xc, GE_INCHES, dd);
	    yc = toDeviceHeight(TRC2 * xc, GE_INCHES, dd);
	    xc = toDeviceWidth(TRC1 * xc, GE_INCHES, dd);
	    xx[0] = x; yy[0] = y-r;
	    xx[1] = x+xc; yy[1] = y+yc;
	    xx[2] = x-xc; yy[2] = y+yc;
	    GEPolygon(3, xx, yy, col, NA_INTEGER, gamma, lty, lwd, dd);
	    break;

	case 7:	/* S square and times superimposed */
	    xc = toDeviceWidth(RADIUS * GSTR_0, GE_INCHES, dd);
	    yc = toDeviceHeight(RADIUS * GSTR_0, GE_INCHES, dd);
	    GERect(x-xc, y-yc, x+xc, y+yc,
		   col, NA_INTEGER, gamma, lty, lwd, dd);
	    GELine(x-xc, y-yc, x+xc, y+yc, col, gamma, lty, lwd, dd);
	    GELine(x-xc, y+yc, x+xc, y-yc, col, gamma, lty, lwd, dd);
	    break;

	case 8: /* S plus and times superimposed */
	    xc = toDeviceWidth(RADIUS * GSTR_0, GE_INCHES, dd);
	    yc = toDeviceHeight(RADIUS * GSTR_0, GE_INCHES, dd);
	    GELine(x-xc, y-yc, x+xc, y+yc, col, gamma, lty, lwd, dd);
	    GELine(x-xc, y+yc, x+xc, y-yc, col, gamma, lty, lwd, dd);
	    xc = toDeviceWidth(M_SQRT2*RADIUS*GSTR_0, GE_INCHES, dd);
	    yc = toDeviceHeight(M_SQRT2*RADIUS*GSTR_0, GE_INCHES, dd);
	    GELine(x-xc, y, x+xc, y, col, gamma, lty, lwd, dd);
	    GELine(x, y-yc, x, y+yc, col, gamma, lty, lwd, dd);
	    break;

	case 9: /* S diamond and plus superimposed */
	    xc = toDeviceWidth(M_SQRT2 * RADIUS * GSTR_0, GE_INCHES, dd);
	    yc = toDeviceHeight(M_SQRT2 * RADIUS * GSTR_0, GE_INCHES, dd);
	    GELine(x-xc, y, x+xc, y, col, gamma, lty, lwd, dd);
	    GELine(x, y-yc, x, y+yc, col, gamma, lty, lwd, dd);
	    xx[0] = x-xc; yy[0] = y;
	    xx[1] = x; yy[1] = y+yc;
	    xx[2] = x+xc; yy[2] = y;
	    xx[3] = x; yy[3] = y-yc;
	    GEPolygon(4, xx, yy, col, NA_INTEGER, gamma, lty, lwd, dd);
	    break;

	case 10: /* S hexagon (circle) and plus superimposed */
	    xc = RADIUS * size;
	    GECircle(x, y, xc, col, NA_INTEGER, gamma, lty, lwd, dd);
	    xc = toDeviceWidth(M_SQRT2 * RADIUS * GSTR_0, GE_INCHES, dd);
	    yc = toDeviceHeight(M_SQRT2 * RADIUS * GSTR_0, GE_INCHES, dd);
	    GELine(x-xc, y, x+xc, y, col, gamma, lty, lwd, dd);
	    GELine(x, y-yc, x, y+yc, col, gamma, lty, lwd, dd);
	    break;

	case 11: /* S superimposed triangles */
	    xc = RADIUS * GSTR_0;
	    r = toDeviceHeight(TRC0 * xc, GE_INCHES, dd);
	    yc = toDeviceHeight(TRC2 * xc, GE_INCHES, dd);
	    xc = toDeviceWidth(TRC1 * xc, GE_INCHES, dd);
	    xx[0] = x; yy[0] = y-r;
	    xx[1] = x+xc; yy[1] = y+yc;
	    xx[2] = x-xc; yy[2] = y+yc;
	    GEPolygon(3, xx, yy, col, NA_INTEGER, gamma, lty, lwd, dd);
	    xx[0] = x; yy[0] = y+r;
	    xx[1] = x+xc; yy[1] = y-yc;
	    xx[2] = x-xc; yy[2] = y-yc;
	    GEPolygon(3, xx, yy, col, NA_INTEGER, gamma, lty, lwd, dd);
	    break;

	case 12: /* S square and plus superimposed */
	    xc = toDeviceWidth(M_SQRT2*RADIUS*GSTR_0, GE_INCHES, dd);
	    yc = toDeviceHeight(M_SQRT2*RADIUS*GSTR_0, GE_INCHES, dd);
	    GELine(x-xc, y, x+xc, y, col, gamma, lty, lwd, dd);
	    GELine(x, y-yc, x, y+yc, col, gamma, lty, lwd, dd);
	    xc = toDeviceWidth(RADIUS * GSTR_0, GE_INCHES, dd);
	    yc = toDeviceHeight(RADIUS * GSTR_0, GE_INCHES, dd);
	    GERect(x-xc, y-yc, x+xc, y+yc,
		   col, NA_INTEGER, gamma, lty, lwd, dd);
	    break;

	case 13: /* S octagon (circle) and times superimposed */
	    xc = RADIUS * size;
	    GECircle(x, y, xc, col, NA_INTEGER, gamma, lty, lwd, dd);
	    xc = toDeviceWidth(RADIUS * GSTR_0, GE_INCHES, dd);
	    yc = toDeviceHeight(RADIUS * GSTR_0, GE_INCHES, dd);
	    GELine(x-xc, y-yc, x+xc, y+yc, col, gamma, lty, lwd, dd);
	    GELine(x-xc, y+yc, x+xc, y-yc, col, gamma, lty, lwd, dd);
	    break;

	case 14: /* S square and point-up triangle superimposed */
	    xc = toDeviceWidth(RADIUS * GSTR_0, GE_INCHES, dd);
	    yc = toDeviceHeight(RADIUS * GSTR_0, GE_INCHES, dd);
	    GERect(x-xc, y-yc, x+xc, y+yc,
		   col, NA_INTEGER, gamma, lty, lwd, dd);
	    xc = RADIUS * GSTR_0;
	    r = toDeviceHeight(TRC0 * xc, GE_INCHES, dd);
	    yc = toDeviceHeight(TRC2 * xc, GE_INCHES, dd);
	    xc = toDeviceWidth(TRC1 * xc, GE_INCHES, dd);
	    xx[0] = x; yy[0] = y+r;
	    xx[1] = x+xc; yy[1] = y-yc;
	    xx[2] = x-xc; yy[2] = y-yc;
	    GEPolygon(3, xx, yy, col, NA_INTEGER, gamma, lty, lwd, dd);
	    break;

	case 15: /* S filled square */
	    xc = toDeviceWidth(RADIUS * GSTR_0, GE_INCHES, dd);
	    yc = toDeviceHeight(RADIUS * GSTR_0, GE_INCHES, dd);
	    xx[0] = x-xc; yy[0] = y-yc;
	    xx[1] = x+xc; yy[1] = y-yc;
	    xx[2] = x+xc; yy[2] = y+yc;
	    xx[3] = x-xc; yy[3] = y+yc;
	    GEPolygon(4, xx, yy, NA_INTEGER, col, gamma, lty, lwd, dd);
	    break;

	case 16: /* S filled octagon (circle) */
	    xc = RADIUS * size;
	    GECircle(x, y, xc, col, col, gamma, lty, lwd, dd);
	    break;

	case 17: /* S filled point-up triangle */
	    xc = RADIUS * GSTR_0;
	    r = toDeviceHeight(TRC0 * xc, GE_INCHES, dd);
	    yc = toDeviceHeight(TRC2 * xc, GE_INCHES, dd);
	    xc = toDeviceWidth(TRC1 * xc, GE_INCHES, dd);
	    xx[0] = x; yy[0] = y+r;
	    xx[1] = x+xc; yy[1] = y-yc;
	    xx[2] = x-xc; yy[2] = y-yc;
	    GEPolygon(3, xx, yy, NA_INTEGER, col, gamma, lty, lwd, dd);
	    break;

	case 18: /* S filled diamond */
	    xc = toDeviceWidth(M_SQRT2 * RADIUS * GSTR_0, GE_INCHES, dd);
	    yc = toDeviceHeight(M_SQRT2 * RADIUS * GSTR_0, GE_INCHES, dd);
	    xx[0] = x-xc; yy[0] = y;
	    xx[1] = x; yy[1] = y+yc;
	    xx[2] = x+xc; yy[2] = y;
	    xx[3] = x; yy[3] = y-yc;
	    GEPolygon(4, xx, yy, NA_INTEGER, col, gamma, lty, lwd, dd);
	    break;

	case 19: /* R filled circle */
	    xc = RADIUS * size;
	    GECircle(x, y, xc, col, col, gamma, lty, lwd, dd);
	    break;


	case 20: /* R `Dot' (small circle) */
	    xc = SMALL * size;
	    GECircle(x, y, xc, col, col, gamma, lty, lwd, dd);
	    break;


	case 21: /* circles */
	    xc = RADIUS * size;
	    GECircle(x, y, xc, col, fill, gamma, lty, lwd, dd);
	    break;

	case  22: /* squares */
	    xc = toDeviceWidth(RADIUS * SQRC * GSTR_0, GE_INCHES, dd);
	    yc = toDeviceHeight(RADIUS * SQRC * GSTR_0, GE_INCHES, dd);
	    GERect(x-xc, y-yc, x+xc, y+yc, col, fill, gamma, lty, lwd, dd);
	    break;

	case 23: /* diamonds */
	    xc = toDeviceWidth(RADIUS * DMDC * GSTR_0, GE_INCHES, dd);
	    yc = toDeviceHeight(RADIUS * DMDC * GSTR_0, GE_INCHES, dd);
	    xx[0] = x	  ; yy[0] = y-yc;
	    xx[1] = x+xc; yy[1] = y;
	    xx[2] = x	  ; yy[2] = y+yc;
	    xx[3] = x-xc; yy[3] = y;
	    GEPolygon(4, xx, yy, col, fill, gamma, lty, lwd, dd);
	    break;

	case 24: /* triangle (point up) */
	    xc = RADIUS * GSTR_0;
	    r = toDeviceHeight(TRC0 * xc, GE_INCHES, dd);
	    yc = toDeviceHeight(TRC2 * xc, GE_INCHES, dd);
	    xc = toDeviceWidth(TRC1 * xc, GE_INCHES, dd);
	    xx[0] = x; yy[0] = y+r;
	    xx[1] = x+xc; yy[1] = y-yc;
	    xx[2] = x-xc; yy[2] = y-yc;
	    GEPolygon(3, xx, yy, col, fill, gamma, lty, lwd, dd);
	    break;

	case 25: /* triangle (point down) */
	    xc = RADIUS * GSTR_0;
	    r = toDeviceHeight(TRC0 * xc, GE_INCHES, dd);
	    yc = toDeviceHeight(TRC2 * xc, GE_INCHES, dd);
	    xc = toDeviceWidth(TRC1 * xc, GE_INCHES, dd);
	    xx[0] = x; yy[0] = y-r;
	    xx[1] = x+xc; yy[1] = y+yc;
	    xx[2] = x-xc; yy[2] = y+yc;
	    GEPolygon(3, xx, yy, col, fill, gamma, lty, lwd, dd);
	    break;
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
	error("invalid axis extents [GEPretty(.,.,n=%d)", *ndiv);
    if(*lo == R_PosInf || *up == R_PosInf ||
       *lo == R_NegInf || *up == R_NegInf ||
       !R_FINITE(*up - *lo)) {
	error("Infinite axis extents [GEPretty(%g,%g,%d)]", *lo, *up, *ndiv);
	return;/*-Wall*/
    }

    ns = *lo; nu = *up;
#ifdef DEBUG_PLOT
    x1 = ns; x2 = nu;
#endif
    unit = R_pretty0(&ns, &nu, ndiv, /* min_n = */ 1,
		     /* shrink_sml = */ 0.25,
		     high_u_fact,
		     2, /* do eps_correction in any case */
		     0 /* return (ns,nu) in  (lo,up) */);
    /* ==> ../appl/pretty.c */

    /* The following is ugly since it kind of happens already in Rpretty0(..):
     */
#define rounding_eps 1e-7
    if(nu >= ns + 1) {
	if(               ns * unit < *lo - rounding_eps*unit)
	    ns++;
	if(nu > ns + 1 && nu * unit > *up + rounding_eps*unit)
	    nu--;
	*ndiv = nu - ns;
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
	warning(" .. GEPretty(.): new *lo = %g < %g = x1", *lo, x1);
    if(*up > x2)
	warning(" .. GEPretty(.): new *up = %g > %g = x2", *up, x2);
#endif
}

/****************************************************************
 * GEMetricInfo
 ****************************************************************
 */
void GEMetricInfo(int c, int font, double cex, double ps,
		  double *ascent, double *descent, double *width,
		  GEDevDesc *dd)
{
    dd->dev->metricInfo(c & 0xFF, font, cex, ps, ascent, descent, width,
			dd->dev);
}

/****************************************************************
 * GEStrWidth
 ****************************************************************
 */
double GEStrWidth(char *str, int font, double cex, double ps, GEDevDesc *dd)
{
    double w;
    char *sbuf = NULL;
    w = 0;
    if(str && *str) {
        char *s, *sb;
	double wdash;
	sbuf = (char*) R_alloc(strlen(str) + 1, sizeof(char));
	sb = sbuf;
        for(s = str; ; s++) {
            if (*s == '\n' || *s == '\0') {
		*sb = '\0';
		wdash = dd->dev->strWidth(sbuf, font, cex, ps, dd->dev);
		if (wdash > w) w = wdash;
		sb = sbuf;
	    }
	    else *sb++ = *s;
	    if (!*s) break;
	}
    }
    return w;
}

/****************************************************************
 * GEStrHeight
 ****************************************************************
 */
double GEStrHeight(char *str, int font, double cex, double ps, GEDevDesc *dd)
{
    double h;
    char *s;
    double asc, dsc, wid;
    int n;
    /* Count the lines of text minus one */
    n = 0;
    for(s = str; *s ; s++)
	if (*s == '\n')
	    n++;
    h = n * cex * dd->dev->cra[1];
    /* Add in the ascent of the font, if available */
    GEMetricInfo('M', font, cex, ps, &asc, &dsc, &wid, dd);
    if ((asc == 0.0) && (dsc == 0.0) && (wid == 0.0))
	asc = cex * dd->dev->cra[1];
    h += asc;
    return h;
}

/****************************************************************
 * GENewPage
 ****************************************************************
 */

void GENewPage(int fill, double gamma, GEDevDesc *dd)
{
    dd->dev->newPage(fill, gamma, dd->dev);
}

/****************************************************************
 * GEinitDisplayList
 ****************************************************************
 */

void GEinitDisplayList(GEDevDesc *dd)
{
    int i;
    /* Get each graphics system to save state required for 
     * replaying the display list
     */
    for (i=0; i<numGraphicsSystems; i++)
	if (dd->gesd[i] != NULL)
	    (dd->gesd[i]->callback)(GE_SaveState, dd, R_NilValue);
    dd->dev->displayList = R_NilValue;
}

/****************************************************************
 * GEplayDisplayList
 ****************************************************************
 */

void GEplayDisplayList(GEDevDesc *dd)
{
    int i, savedDevice, plotok;
    SEXP theList;
    /* Get each graphics system to restore state required for 
     * replaying the display list
     */
    for (i=0; i<numGraphicsSystems; i++)
	if (dd->gesd[i] != NULL)
	    (dd->gesd[i]->callback)(GE_RestoreState, dd, R_NilValue);
    /* Play the display list
     */
    theList = dd->dev->displayList;
    plotok = 1;
    if (theList != R_NilValue) {
	savedDevice = curDevice();
	selectDevice(deviceNumber((DevDesc*) dd));
	while (theList != R_NilValue && plotok) {
	    SEXP theOperation = CAR(theList);
	    SEXP op = CAR(theOperation);
	    SEXP args = CDR(theOperation);
	    PRIMFUN(op) (R_NilValue, op, args, R_NilValue);
	    /* Check with each graphics system that the plotting went ok
	     */
	    for (i=0; i<numGraphicsSystems; i++)
		if (dd->gesd[i] != NULL)
		    if (!LOGICAL((dd->gesd[i]->callback)(GE_CheckPlot, dd, 
							 R_NilValue))[0])
			plotok = 0;
	    theList = CDR(theList);
	}
	selectDevice(savedDevice);
    }
}

/****************************************************************
 * GEcurrentDevice
 ****************************************************************
 */

GEDevDesc* GEcurrentDevice()
{
    return (GEDevDesc*) CurrentDevice();
}

/****************************************************************
 * GEcopyDisplayList
 ****************************************************************
 */

/* We assume that the device being copied TO is the "current" device
 */
/* We assume that BOTH from and to devices are GEDevDesc's
 * i.e., this will crash if you try to copy from or to an old DevDesc
 */
void GEcopyDisplayList(int fromDevice)
{
    GEDevDesc *dd = GEcurrentDevice();
    DevDesc* fromDev = GetDevice(fromDevice);
    int i;
    dd->dev->displayList = Rf_displayList(fromDev);
    /* Get each registered graphics system to copy system state
     * information from the "from" device to the current device
     */
    for (i=0; i<numGraphicsSystems; i++) 
	if (dd->gesd[i] != NULL)
	    (dd->gesd[i]->callback)(GE_CopyState, (GEDevDesc*) fromDev, 
				    R_NilValue);
    GEplayDisplayList(dd);
    if (!dd->dev->displayListOn)
	GEinitDisplayList(dd);
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

SEXP GEcreateSnapshot(GEDevDesc *dd)
{
    int i;
    SEXP snapshot;
    SEXP state;
    /* Create a list with one spot for the display list 
     * and one spot each for the registered graphics systems
     * to put their graphics state
     */
    PROTECT(snapshot = allocVector(VECSXP, 1 + numGraphicsSystems));
    /* The first element of the snapshot is the display list.
     */
    SET_VECTOR_ELT(snapshot, 0, dd->dev->displayList);
    /* For each registered system, obtain state information,
     * and store that in the snapshot.
     */
    for (i=0; i<numGraphicsSystems; i++)
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
 */

void GEplaySnapshot(SEXP snapshot, GEDevDesc* dd)
{
    /* Only have to set up information for as many graphics systems
     * as were registered when the snapshot was taken.
     */
    int i, numSystems = LENGTH(snapshot) - 1;
    /* Reset the snapshot state information in each registered 
     * graphics system
     */
    for (i=0; i<numSystems; i++) 
	if (dd->gesd[i] != NULL)
	    (dd->gesd[i]->callback)(GE_RestoreSnapshotState, dd, 
				    VECTOR_ELT(snapshot, i + 1));
    /* Replay the display list
     */
    dd->dev->displayList = VECTOR_ELT(snapshot, 0);
    GEplayDisplayList(dd);
    if (!dd->dev->displayListOn)
	GEinitDisplayList(dd);
}

