/*
 *  RProxy: Connector implementation between application and R language
 *  Copyright (C) 2000--2001 Thomas Baier
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Library General Public
 *  License as published by the Free Software Foundation; either
 *  version 2 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public
 *  License along with this library; if not, write to the Free
 *  Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
 *  MA 02111-1307, USA
 *
 *  $Id: rproxy_dev.c,v 1.7 2003/11/26 20:50:15 murrell Exp $
 */

/* virtual device size */
#define DEV_X 500
#define DEV_Y 500

#ifdef HAVE_CONFIG_H
#include <Rconfig.h>
#endif

#include "Defn.h"
#include "Graphics.h"
#include <stdio.h>
#include <windows.h>

#include "SC_proxy.h"

extern SC_GraphicsDevice* __graphics_device;

static void R_Proxy_Graphics_Activate (NewDevDesc* pDD)
{
  if (__graphics_device)
    {
      __graphics_device->vtbl->activate (__graphics_device);
      return;
    }
}

#ifdef MessageBox
#undef MessageBox
#endif
#define MessageBox(a,b,c,d)

static void R_Proxy_Graphics_Activate(NewDevDesc *dd);
static void R_Proxy_Graphics_Circle(double x, double y, double r,
				    R_GE_gcontext *gc,
				    NewDevDesc *dd);
static void R_Proxy_Graphics_Clip(double x0, double x1, double y0, double y1,
		     NewDevDesc *dd);
static void R_Proxy_Graphics_Close(NewDevDesc *dd);
static void R_Proxy_Graphics_Deactivate(NewDevDesc *dd);
static void R_Proxy_Graphics_Hold(NewDevDesc *dd);
static Rboolean R_Proxy_Graphics_Locator(double *x, double *y, NewDevDesc *dd);
static void R_Proxy_Graphics_Line(double x1, double y1, double x2, double y2,
				  R_GE_gcontext *gc,
				  NewDevDesc *dd);
static void R_Proxy_Graphics_MetricInfo(int c, 
					R_GE_gcontext *gc,
					double* ascent, double* descent,
					double* width, NewDevDesc *dd);
static void R_Proxy_Graphics_Mode(int mode, NewDevDesc *dd);
static void R_Proxy_Graphics_NewPage(R_GE_gcontext *gc,
				     NewDevDesc *dd);
static void R_Proxy_Graphics_Polygon(int n, double *x, double *y,
				     R_GE_gcontext *gc,
				     NewDevDesc *dd);
static void R_Proxy_Graphics_Polyline(int n, double *x, double *y,
				      R_GE_gcontext *gc,
				      NewDevDesc *dd);
static void R_Proxy_Graphics_Rect(double x0, double y0, double x1, double y1,
				  R_GE_gcontext *gc,
				  NewDevDesc *dd);
static void R_Proxy_Graphics_Size(double *left, double *right,
				  double *bottom, double *top,
				  NewDevDesc *dd);
#ifdef UNUSED
static void R_Proxy_Graphics_Resize(NewDevDesc *dd);
#endif
static double R_Proxy_Graphics_StrWidth(char *str, 
					R_GE_gcontext *gc,
					NewDevDesc *dd);
static void R_Proxy_Graphics_Text(double x, double y, char *str,
				  double rot, double hadj,
				  R_GE_gcontext *gc,
				  NewDevDesc *dd);
static Rboolean R_Proxy_Graphics_Open (NewDevDesc* pDD,
				       void* pAXD,
				       char* pDisplay,
				       double pWidth,
				       double pHeight,
				       Rboolean pRecording,
				       int pResize);

/* 00-06-22 | baier | added line type and width */
static void R_Proxy_Graphics_Circle (double pX,
				     double pY,
				     double pRad,
				     R_GE_gcontext *gc,
				     NewDevDesc* pDD)
{
  if (__graphics_device)
    {
      __graphics_device->vtbl->circle (__graphics_device,
				       pX,
				       pY,
				       pRad,
				       gc->fill,
				       gc->col,
				       gc->lty,
				       gc->lwd);
      return;
    }

  MessageBox (GetDesktopWindow (),"Circle()","R_Proxy_Graphics",MB_OK);
}
static void R_Proxy_Graphics_Clip (double pX0,
				   double pX1,
				   double pY0,
				   double pY1,
				   NewDevDesc* pDD)
{
  if (__graphics_device)
    {
      __graphics_device->vtbl->clip (__graphics_device,
				     pX0,pX1,pY0,pY1);
      return;
    }

  MessageBox (GetDesktopWindow (),"Clip()","R_Proxy_Graphics",MB_OK);
}
static void R_Proxy_Graphics_Close (NewDevDesc* pDD)
{
  if (__graphics_device)
    {
      __graphics_device->vtbl->close (__graphics_device);
      return;
    }

  MessageBox (GetDesktopWindow (),"Close()","R_Proxy_Graphics",MB_OK);
}
static void R_Proxy_Graphics_Deactivate (NewDevDesc* pDD)
{
  if (__graphics_device)
    {
      __graphics_device->vtbl->deactivate (__graphics_device);
      return;
    }

  MessageBox (GetDesktopWindow (),"Deactivate()","R_Proxy_Graphics",MB_OK);
}
static void R_Proxy_Graphics_Hold (NewDevDesc* pDD)
{
  if (__graphics_device)
    {
      __graphics_device->vtbl->hold (__graphics_device);
      return;
    }

  MessageBox (GetDesktopWindow (),"Hold()","R_Proxy_Graphics",MB_OK);
}
/* 00-06-22 | baier | added color, line type and width */
static void R_Proxy_Graphics_Line (double pX0,
				   double pY0,
				   double pX1,
				   double pY1,
				   R_GE_gcontext *gc,
				   NewDevDesc* pDD)
{
  if (__graphics_device)
    {

      __graphics_device->vtbl->line (__graphics_device,
				     pX0,pY0,pX1,pY1,gc->col,
				     gc->lty,gc->lwd);
      return;
    }

  MessageBox (GetDesktopWindow (),"Line()","R_Proxy_Graphics",MB_OK);
}
/* 01-01-25 | changed return type */
static Rboolean R_Proxy_Graphics_Locator (double* pX,
					  double* pY,
					  NewDevDesc* pDD)
{
  if (__graphics_device)
    {
      return __graphics_device->vtbl->locator (__graphics_device,pX,pY);
    }

  MessageBox (GetDesktopWindow (),"Locator()","R_Proxy_Graphics",MB_OK);
  return 0;
}
static void R_Proxy_Graphics_Mode (int pMode, NewDevDesc *dd)
{
  if (__graphics_device)
    {
      __graphics_device->vtbl->mode (__graphics_device,pMode);
      return;
    }

  MessageBox (GetDesktopWindow (),"Mode()","R_Proxy_Graphics",MB_OK);
}
static void R_Proxy_Graphics_NewPage (R_GE_gcontext *gc,
				      NewDevDesc* pDD)
{
  if (__graphics_device)
    {
      __graphics_device->vtbl->newpage (__graphics_device);
      return;
    }

  MessageBox (GetDesktopWindow (),"NewPage()","R_Proxy_Graphics",MB_OK);
}
/* 01-01-25 | baier | added new parameters "recording", "resize" */
static Rboolean R_Proxy_Graphics_Open (NewDevDesc* pDD,
				       void* pAXD,
				       char* pDisplay,
				       double pWidth,
				       double pHeight,
				       Rboolean pRecording,
				       int pResize)
{
  if (__graphics_device)
    {
      return __graphics_device->vtbl->open (__graphics_device,
					    pDisplay,pWidth,pHeight,0.0,0,0);
    }

  MessageBox (GetDesktopWindow (),"Open()","R_Proxy_Graphics",MB_OK);
  return 1;
}

static void R_Proxy_Graphics_Polygon (int pCount,
				      double* pX,
				      double* pY,
				      R_GE_gcontext *gc,
				      NewDevDesc* pDD)
{
  if (__graphics_device)
    {
      /* convert coordinates here: don't use alloca() -> overflow? */
      double* lX = (double*) malloc (pCount * sizeof (double));
      double* lY = (double*) malloc (pCount * sizeof (double));
      int i = 0;

      /* could use memcpy() for speed */
      for (i = 0;i < pCount;i++)
	{
	  lX[i] = pX[i];
	  lY[i] = pY[i];

	}

      /*      sprintf (x,"device::Polygon: bg is %08x, fg is %08x\n",fill,col); */
      /*      OutputDebugString (x); */
      __graphics_device->vtbl->polygon (__graphics_device,
					pCount,lX,lY,gc->fill,gc->col);

      free (lX);
      free (lY);

      return;
    }

  MessageBox (GetDesktopWindow (),"Polygon()","R_Proxy_Graphics",MB_OK);
}
/* 01-01-23 | baier | added "col" parameter */
static void R_Proxy_Graphics_Polyline (int pCount,
				       double* pX,
				       double* pY,
				       R_GE_gcontext *gc,
				       NewDevDesc* pDD)
{
  if (__graphics_device)
    {
      /* convert coordinates here: don't use alloca() -> overflow? */
      double* lX = (double*) malloc (pCount * sizeof (double));
      double* lY = (double*) malloc (pCount * sizeof (double));
      int i = 0;

      /* could use memcpy() for speed */
      for (i = 0;i < pCount;i++)
	{
	  char x[1000];
	  lX[i] = pX[i];
	  lY[i] = pY[i];

	  sprintf (x,"Polyline: coord %d is %f/%f (was %f/%f)\n",
		   i,lX[i],lY[i],pX[i],pY[i]);
	  /*	  OutputDebugString (x); */

	}

      __graphics_device->vtbl->polyline (__graphics_device,
					 pCount,lX,lY,gc->col);

      free (lX);
      free (lY);

      return;
    }

  MessageBox (GetDesktopWindow (),"Polyline()","R_Proxy_Graphics",MB_OK);
}
/* 00-06-22 | baier | added line type and width */
static void R_Proxy_Graphics_Rect (double pX0,
				   double pY0,
				   double pX1,
				   double pY1,
				   R_GE_gcontext *gc,
				   NewDevDesc* pDD)
{
  if (__graphics_device)
    {

      __graphics_device->vtbl->rect (__graphics_device,
				     pX0,pY0,pX1,pY1,gc->fill,gc->col,
				     gc->lty,gc->lwd);
      return;
    }

  MessageBox (GetDesktopWindow (),"Rect()","R_Proxy_Graphics",MB_OK);
}
static void R_Proxy_Graphics_Size(double *left, double *right,
				  double *bottom, double *top,
				  NewDevDesc *pDD)
{
    *left = pDD->left;
    *right = pDD->right;
    *bottom = pDD->bottom;
    *top = pDD->top;
}

#ifdef UNUSED
static void R_Proxy_Graphics_Resize (NewDevDesc* pDD)
{
  if (__graphics_device)
    {
      __graphics_device->vtbl->resize (__graphics_device);
      return;
    }

  MessageBox (GetDesktopWindow (),"Resize()","R_Proxy_Graphics",MB_OK);
}
#endif

/* 00-06-22 | baier | added font and size parameters */
static double R_Proxy_Graphics_StrWidth (char* pString,
					 R_GE_gcontext *gc,
					 NewDevDesc* pDD)
{
  if (__graphics_device)
    {
      int lSize = gc->cex * gc->ps + 0.5;
      return __graphics_device->vtbl->strwidth (__graphics_device,
						pString,
						gc->fontface,
						lSize);
    }

  MessageBox (GetDesktopWindow (),"StrWidth()","R_Proxy_Graphics",MB_OK);
  return 0.0;
}

/* 00-06-22 | baier | added color, font and size */
static void R_Proxy_Graphics_Text (double pX,
				   double pY,
				   char* pString,
				   double pRot,
				   double pHadj,
				   R_GE_gcontext *gc,
				   NewDevDesc* pDD)
{
  if (__graphics_device)
    {
      int lSize = gc->cex * gc->ps + 0.5;

      __graphics_device->vtbl->text (__graphics_device,
				     pX,pY,pString,pRot,pHadj,
				     gc->col,gc->fontface,lSize);
      return;
    }

  MessageBox (GetDesktopWindow (),"Text()","R_Proxy_Graphics",MB_OK);
}

/* 00-06-22 | baier | added font and size parameters */
static void R_Proxy_Graphics_MetricInfo(int c, 
					R_GE_gcontext *gc,
					double* ascent, double* descent,
					double* width, NewDevDesc *dd);
static void R_Proxy_Graphics_MetricInfo (int pC,
					 R_GE_gcontext *gc,
					 double* pAscent,
					 double* pDescent,
					 double* pWidth,
					 NewDevDesc* pDD)
{
  if (__graphics_device)
    {
      int lSize = gc->cex * gc->ps + 0.5;
      __graphics_device->vtbl->metricinfo (__graphics_device,
					   pC,
					   pAscent,
					   pDescent,
					   pWidth,
					   gc->fontface,
					   lSize);
      return;
    }

  MessageBox (GetDesktopWindow (),"MetricInfo()","R_Proxy_Graphics",MB_OK);
}

/* 01-01-25 | baier | new paramters */
int R_Proxy_Graphics_Driver (NewDevDesc* pDD,
			     char* pDisplay,
			     double pWidth,
			     double pHeight,
			     double pPointSize,
			     Rboolean pRecording,
			     int pResize,
			     struct _SC_GraphicsDevice* pDevice)
{
  pDD->startfont = 1;
  pDD->startps = pPointSize;
  pDD->startcol = 0;
  pDD->startfill = NA_INTEGER;
  pDD->startlty = LTY_SOLID;
  pDD->startgamma = 1;

  /* init the device-specific functionality here */
  pDD->deviceSpecific = (void *) NULL;

  /* Start the Device Driver and Hardcopy.  */
  if (!R_Proxy_Graphics_Open (pDD,
			      NULL,
			      pDisplay,
			      pWidth,
			      pHeight,
			      pRecording,
			      pResize)) {
    return 0;
  }
  /* Set up Data Structures  */

  pDD->newDevStruct = 1;

  pDD->open = R_Proxy_Graphics_Open;
  pDD->close = R_Proxy_Graphics_Close;
  pDD->activate = R_Proxy_Graphics_Activate;
  pDD->deactivate = R_Proxy_Graphics_Deactivate;
  pDD->size = R_Proxy_Graphics_Size;
  pDD->newPage = R_Proxy_Graphics_NewPage;
  pDD->clip = R_Proxy_Graphics_Clip;
  pDD->strWidth = R_Proxy_Graphics_StrWidth;
  pDD->text = R_Proxy_Graphics_Text;
  pDD->rect = R_Proxy_Graphics_Rect;
  pDD->circle = R_Proxy_Graphics_Circle;
  pDD->line = R_Proxy_Graphics_Line;
  pDD->polyline = R_Proxy_Graphics_Polyline;
  pDD->polygon = R_Proxy_Graphics_Polygon;
  pDD->locator = R_Proxy_Graphics_Locator;
  pDD->mode = R_Proxy_Graphics_Mode;
  pDD->hold = R_Proxy_Graphics_Hold;
  pDD->metricInfo = R_Proxy_Graphics_MetricInfo;

    /* set graphics parameters that must be set by device driver */
    /* Window Dimensions in Pixels */
  pDD->left = 0;	/* left */
  pDD->right = DEV_X;	/* right */
  pDD->top = 0;	/* top */
  pDD->bottom = DEV_Y;	/* bottom */


  /* Nominal Character Sizes in Pixels */
  pDD->cra[0] = 10;
  pDD->cra[1] = 10;
  /* Character Addressing Offsets */
  /* These are used to plot a single plotting character */
  /* so that it is exactly over the plotting point */

  pDD->xCharOffset = 0.50;
  pDD->yCharOffset = 0.40;
  pDD->yLineBias = 0.1;

  /* Inches per raster unit */

  pDD->ipr[0] = 1.0 / 72.0;
  pDD->ipr[1] = 1.0 / 72.0;

    /* Device capabilities */
    /* Clipping is problematic for X11 */
    /* Graphics is clipped, text is not */

  pDD->canResizePlot = 1;
  pDD->canChangeFont = 0;
  pDD->canRotateText = 1;
  pDD->canResizeText = 1;
  pDD->canClip = 1;

  /* initialise x11 device description (most of the work */
  /* has been done in X11_Open) */

  pDD->displayListOn = 1;
  return 1;
}
