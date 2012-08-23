/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2004-11  The R Core Team
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

#include <Rinternals.h>
#define R_USE_PROTOTYPES 1
#include <R_ext/GraphicsEngine.h>

#include "grDevices.h"
#include <stdlib.h>

static Rboolean nullDeviceDriver(pDevDesc dev);

void GEnullDevice()
{
    pDevDesc dev = NULL;
    pGEDevDesc dd;

    R_GE_checkVersionOrDie(R_GE_version);
    R_CheckDeviceAvailable();
    BEGIN_SUSPEND_INTERRUPTS {
	if (!(dev = (pDevDesc ) calloc(1, sizeof(DevDesc))))
	    error(_("unable to start NULL device"));
	if (!nullDeviceDriver(dev)) {
	    free(dev);
	    error(_("unable to start NULL device"));
	}
	dd = GEcreateDevDesc(dev);
	GEaddDevice2(dd, "NULL");
    } END_SUSPEND_INTERRUPTS;
}
static void NULL_Circle(double x, double y, double r,
                        const pGEcontext gc,
                        pDevDesc dev) {
}
static void NULL_Line(double x1, double y1, double x2, double y2,
                      const pGEcontext gc,
                      pDevDesc dev) {
}
static void NULL_Polygon(int n, double *x, double *y,
                         const pGEcontext gc,
                         pDevDesc dev) {
}
static void NULL_Path(double *x, double *y,
                      int npoly, int *nper,
                      Rboolean winding,
                      const pGEcontext gc,
                      pDevDesc dev) {
}
static void NULL_Polyline(int n, double *x, double *y,
                          const pGEcontext gc,
                          pDevDesc dev) {
}
static void NULL_Rect(double x0, double y0, double x1, double y1,
                      const pGEcontext gc,
                      pDevDesc dev) {
}
static void NULL_Text(double x, double y, const char *str,
                      double rot, double hadj,
                      const pGEcontext gc,
                      pDevDesc dev) {
}
static void NULL_NewPage(const pGEcontext gc,
                         pDevDesc dev) {
}
static void NULL_Close(pDevDesc dev) {
}
static Rboolean NULL_Open(pDevDesc dev) {
    return TRUE;
}
static void NULL_Clip(double x0, double x1, double y0, double y1,
                      pDevDesc dev) {
}
static void NULL_MetricInfo(int c, const pGEcontext gc,
                            double* ascent, double* descent,
                            double* width, pDevDesc dev) 
{
    Rboolean Unicode = mbcslocale;

    *ascent = 0.0;
    *descent = 0.0;
    *width = 0.0;

    /* dummy, as a test of the headers */
    if (c < 0) { Unicode = TRUE; c = -c; }
    if(Unicode && gc->fontface != 5 && c >= 128) {
	/* Unicode case */ ;
    } else {
	/* single-byte case */ ;
    }
}
static void NULL_Size(double *left, double *right,
                      double *bottom, double *top,
                      pDevDesc dev) {
    *left = dev->left;
    *right = dev->right;
    *bottom = dev->bottom;
    *top = dev->top;
}
static double NULL_StrWidth(const char *str,
                            const pGEcontext gc,
                            pDevDesc dev) {
    return 0.0;
}

static Rboolean nullDeviceDriver(pDevDesc dev) {
    dev->deviceSpecific = NULL;
    /*
     * Device functions
     */
    dev->close = NULL_Close;
    dev->size = NULL_Size;
    dev->newPage = NULL_NewPage;
    dev->clip = NULL_Clip;
    dev->strWidth = NULL_StrWidth;
    dev->text = NULL_Text;
    dev->rect = NULL_Rect;
    dev->circle = NULL_Circle;
    dev->line = NULL_Line;
    dev->polyline = NULL_Polyline;
    dev->polygon = NULL_Polygon;
    dev->path = NULL_Path;
    dev->metricInfo = NULL_MetricInfo;
    dev->hasTextUTF8 = FALSE;
    dev->useRotatedTextInContour = FALSE;
    /*
     * Initial graphical settings
     */
    dev->startfont = 1;
    dev->startps = 10;
    dev->startcol = R_RGB(0, 0, 0);
    dev->startfill = R_TRANWHITE;
    dev->startlty = LTY_SOLID;
    dev->startgamma = 1;
    /*
     * Start device
     */
    if(!NULL_Open(dev)) {
        return FALSE;
    }
    /*
     * Device physical characteristics
     */
    dev->left = 0;
    dev->right = 1000;
    dev->bottom = 0;
    dev->top = 1000;
    dev->cra[0] = 9;
    dev->cra[1] = 12;
    dev->xCharOffset = 0.4900;
    dev->yCharOffset = 0.3333;
    dev->yLineBias = 0.2;
    dev->ipr[0] = 1.0/72;
    dev->ipr[1] = 1.0/72;
    /*
     * Device capabilities
     */
    dev->canClip = TRUE;
    dev->canHAdj = 2;
    dev->canChangeGamma = FALSE;
    dev->displayListOn = FALSE;

    dev->haveTransparency = 1;
    dev->haveTransparentBg = 2;

    return TRUE;
}

/* formerly in grid.c */
SEXP R_GD_nullDevice()
{
    warning("R_GD_nullDevice is deprecated and will be removed shortly");
    GEnullDevice();
    return R_NilValue;
}
