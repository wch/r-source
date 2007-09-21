/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2007  The R Development Core Team
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
 *
 *  This header file constitutes the (inofficial) API to the Quartz device.
 *  Being inofficial, the API may change at any point without warning.
 *
 *  Quartz is a general device-independent way of drawing in Mac OS X,
 *  therefore the Quartz device modularizes the actual drawing target
 *  implementation into separate modules (e.g. Carbon and Cocoa for
 *  on-screen display and Bitmap for off-screen drawing). The API
 *  below is used by the modules to talk to the Quartz device without
 *  having to know anything about R graphics device API.
 *
 *  Key functions are listed here:
 *  QuartzDevice_Create - creates a Quartz device
 *  QuartzDevice_ResetContext - should be called after the target context has
 *               been created to initialize it.
 *  QuartzDevice_Kill - closes the Quartz device (e.g. on window close)
 *  QuartzDevice_SetScaledSize - resize device (does not include re-painting,
 *               it should be followed by QuartzDevice_ReplayDisplayList)
 *  QuartzDevice_ReplayDisplayList - replays all plot commands
 *
 */

#ifndef R_EXT_QUARTZDEVICE_H_
#define R_EXT_QUARTZDEVICE_H_

#ifdef __cplusplus
extern "C" {
#endif   

/* for CGContextRef */
#include <ApplicationServices/ApplicationServices.h>

typedef void* QuartzDesc_t;

/* embedded Quartz support hook (defined in unix/aqua.c):
    dd = should be passed-through to QuartzDevice_Create
    type = name of the desired Quartz output type
    file = filename (optional)
    width, height = size (in inches)
    pointsize, family = initial text properties
    antialias, smooth, autorefresh = flags
    quartzpos = desired initial position specification
    bg = background color
    title = window title (optional)
    dpi = either NULL (auto-detect) or double[2] with x and y DPI
    */
extern Rboolean (*ptr_QuartzDeviceCreate)(void *dd,const char *type,
       const char *file, double width, double height, double pointsize,
       const char *family, Rboolean antialias, Rboolean smooth,
       Rboolean autorefresh,int quartzpos, int bg, const char *title,
       double *dpi);

/* all device implementations have to call this general Quartz device constructor at some point */
QuartzDesc_t QuartzDevice_Create(
	void *dd,
	double scalex, double scaley, double ps,
        double width,double height,
        int bg,int aa,int fs,
	CGContextRef (*getCGContext)(QuartzDesc_t dev,void*userInfo), 			/* Get the context for this device (mandatory) */
	int          (*locatePoint)(QuartzDesc_t dev,void*userInfo,double*x,double*y),	/* Locate a point on the device */
	void         (*close)(QuartzDesc_t dev,void*userInfo),				/* Close the device */
	void         (*newPage)(QuartzDesc_t dev,void*userInfo),			/* Start a new page */
	void         (*state)(QuartzDesc_t dev,void*userInfo, int state),		/* Change state (active/inactive) */
	void*        (*par)(QuartzDesc_t dev,void*userInfo, void*par),			/* Change graphics parameters */
	void         (*sync)(QuartzDesc_t dev,void*userInfo),				/* synchronize display with backing plane */
	void* userInfo);  /* pointer that will be available to all calls for backend-specific data */

int  QuartzDevice_DevNumber(QuartzDesc_t desc);       /* returns device number */
void QuartzDevice_Kill(QuartzDesc_t desc);            /* call to close the device */
void QuartzDevice_ResetContext(QuartzDesc_t desc);    /* notifies Q back-end that the implementation has created a new context */

double QuartzDevice_GetWidth(QuartzDesc_t desc);      /* get device width (in inches) */
void   QuartzDevice_SetWidth(QuartzDesc_t desc,double width); /* set device width (in inches) */

double QuartzDevice_GetHeight(QuartzDesc_t desc);     /* get device height (in inches) */
void   QuartzDevice_SetHeight(QuartzDesc_t desc,double height); /* get device height (in inches) */

double QuartzDevice_GetScaledWidth(QuartzDesc_t desc);/* get device width (in pixels) */
double QuartzDevice_GetScaledHeight(QuartzDesc_t desc);/* get device height (in pixels) */
void   QuartzDevice_SetScaledSize(QuartzDesc_t desc, double width, double height); /* set device size (in pixels) */

double QuartzDevice_GetXScale(QuartzDesc_t desc);     /* get x scale factor (px/pt ratio) */
double QuartzDevice_GetYScale(QuartzDesc_t desc);     /* get y scale factor (px/pt ratio) */
void   QuartzDevice_SetScale(QuartzDesc_t desc,double scalex, double scaley); /* sets both scale factors (px/pt ratio) */

void   QuartzDevice_SetTextScale(QuartzDesc_t desc,double scale); /* sets text scale factor */
double QuartzDevice_GetTextScale(QuartzDesc_t desc);  /* sets text scale factor */

void   QuartzDevice_SetPointSize(QuartzDesc_t desc,double ps); /* sets point size */
double QuartzDevice_GetPointSize(QuartzDesc_t desc);  /* gets point size */

int  QuartzDevice_GetDirty(QuartzDesc_t desc);        /* sets dirty flag */
void QuartzDevice_SetDirty(QuartzDesc_t desc,int dirty); /* gets dirty flag */

void  QuartzDevice_ReplayDisplayList(QuartzDesc_t desc); /* replay display list
                                 Note: it inhibits sync calls during repaint,
                                 the caller is responsible for calling sync if needed */
void* QuartzDevice_GetSnapshot(QuartzDesc_t desc);    /* create a (replayable) snapshot of the device contents */
void  QuartzDevice_RestoreSnapshot(QuartzDesc_t desc,void* snapshot); /* restore a snapshot */

int  QuartzDevice_GetAntialias(QuartzDesc_t desc);    /* get anti-alias flag */
void QuartzDevice_SetAntialias(QuartzDesc_t desc,int aa); /* set anti-alias flag */

int  QuartzDevice_GetFontSmooth(QuartzDesc_t desc);   /* get font smoothing flag */
void QuartzDevice_SetFontSmooth(QuartzDesc_t desc,int fs); /* get font smoothing flag */

int  QuartzDevice_GetBackground(QuartzDesc_t desc);   /* get background color */

double QuartzDevice_UserX(QuartzDesc_t desc,double x);
double QuartzDevice_UserY(QuartzDesc_t desc,double y);

/* this is an experimental interface that allows to start a Quartz device from C code instead of going through the Quartz R function */
typedef int (*quartz_create_fn_t)(void *dd,const char *type,const char *file,double width,double height,double pointsize,const char *family,
                                  int antialias,int smooth,int autorefresh,int quartzpos,int bg, const char *title, double *dpi);
/* C version of the Quartz call (experimental)
   returns 0 on success, error code on failure */
int Quartz_C(const char *type, const char *file, double width, double height, double ps,
             const char *family, int aa, int fsm, const char *title, int bg, double *dpi,
             quartz_create_fn_t q_create);
/* R supplied q_create values available: QuartzCocoa_DeviceCreate, QuartzCarbon_DeviceCreate, QuartzBitmap_DeviceCreate or the value of ptr_QuartzDeviceCreate */
#ifdef __cplusplus
}
#endif   
    
#endif
