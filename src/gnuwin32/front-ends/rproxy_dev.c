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
 *  $Id: rproxy_dev.c,v 1.2 2001/04/05 09:42:35 ripley Exp $
 */

// virtual device size
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

static void R_Proxy_Graphics_Activate (DevDesc* pDD)
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

// 00-06-22 | baier | added line type and width
static void R_Proxy_Graphics_Circle (double pX,
				     double pY,
				     int pCoords,
				     double pRad,
				     int pColor,
				     int pBorder,
				     DevDesc* pDD)
{
  if (__graphics_device)
    {
      // convert coordinates here
      double lTmp = 0.0;

      GConvert (&pX,&pY,pCoords,DEVICE,pDD);
      GConvert (&pRad,&lTmp,pCoords,DEVICE,pDD);

      __graphics_device->vtbl->circle (__graphics_device,
				       pX,
				       pY,
				       pRad,
				       pColor,
				       pBorder,
				       pDD->gp.lty,
				       pDD->gp.lwd);
      return;
    }

  MessageBox (GetDesktopWindow (),"Circle()","R_Proxy_Graphics",MB_OK);
}
static void R_Proxy_Graphics_Clip (double pX0,
				   double pX1,
				   double pY0,
				   double pY1,
				   DevDesc* pDD)
{
  if (__graphics_device)
    {
      __graphics_device->vtbl->clip (__graphics_device,
				     pX0,pX1,pY0,pY1);
      return;
    }

  MessageBox (GetDesktopWindow (),"Clip()","R_Proxy_Graphics",MB_OK);
}
static void R_Proxy_Graphics_Close (DevDesc* pDD)
{
  if (__graphics_device)
    {
      __graphics_device->vtbl->close (__graphics_device);
      return;
    }

  MessageBox (GetDesktopWindow (),"Close()","R_Proxy_Graphics",MB_OK);
}
static void R_Proxy_Graphics_Deactivate (DevDesc* pDD)
{
  if (__graphics_device)
    {
      __graphics_device->vtbl->deactivate (__graphics_device);
      return;
    }

  MessageBox (GetDesktopWindow (),"Deactivate()","R_Proxy_Graphics",MB_OK);
}
static void R_Proxy_Graphics_Hold (DevDesc* pDD)
{
  if (__graphics_device)
    {
      __graphics_device->vtbl->hold (__graphics_device);
      return;
    }

  MessageBox (GetDesktopWindow (),"Hold()","R_Proxy_Graphics",MB_OK);
}
// 00-06-22 | baier | added color, line type and width
static void R_Proxy_Graphics_Line (double pX0,
				   double pY0,
				   double pX1,
				   double pY1,
				   int pCoords,
				   DevDesc* pDD)
{
  if (__graphics_device)
    {
      // convert coordinates here
      GConvert (&pX0,&pY0,pCoords,DEVICE,pDD);
      GConvert (&pX1,&pY1,pCoords,DEVICE,pDD);

      __graphics_device->vtbl->line (__graphics_device,
				     pX0,pY0,pX1,pY1,pDD->gp.col,
				     pDD->gp.lty,pDD->gp.lwd);
      return;
    }

  MessageBox (GetDesktopWindow (),"Line()","R_Proxy_Graphics",MB_OK);
}
// 01-01-25 | changed return type
static Rboolean R_Proxy_Graphics_Locator (double* pX,
					  double* pY,
					  DevDesc* pDD)
{
  if (__graphics_device)
    {
      return __graphics_device->vtbl->locator (__graphics_device,pX,pY);
    }

  MessageBox (GetDesktopWindow (),"Locator()","R_Proxy_Graphics",MB_OK);
  return 0;
}
static void R_Proxy_Graphics_Mode (int pMode)
{
  if (__graphics_device)
    {
      __graphics_device->vtbl->mode (__graphics_device,pMode);
      return;
    }

  MessageBox (GetDesktopWindow (),"Mode()","R_Proxy_Graphics",MB_OK);
}
static void R_Proxy_Graphics_NewPage (DevDesc* pDD)
{
  if (__graphics_device)
    {
      __graphics_device->vtbl->newpage (__graphics_device);
      return;
    }

  MessageBox (GetDesktopWindow (),"NewPage()","R_Proxy_Graphics",MB_OK);
}
// 01-01-25 | baier | added new parameters "recording", "resize"
static Rboolean R_Proxy_Graphics_Open (DevDesc* pDD,
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
				      int pCoords,
				      int pBG,
				      int pFG,
				      DevDesc* pDD)
{
  if (__graphics_device)
    {
      // convert coordinates here: don't use alloca() -> overflow?
      double* lX = (double*) malloc (pCount * sizeof (double));
      double* lY = (double*) malloc (pCount * sizeof (double));
      int i = 0;
      char x[1000];

      // could use memcpy() for speed
      for (i = 0;i < pCount;i++)
	{
	  lX[i] = pX[i];
	  lY[i] = pY[i];

	  GConvert (&(lX[i]),&(lY[i]),pCoords,DEVICE,pDD);
	}

      sprintf (x,"device::Polygon: bg is %08x, fg is %08x\n",pBG,pFG);
      OutputDebugString (x);
      __graphics_device->vtbl->polygon (__graphics_device,
					pCount,lX,lY,pBG,pFG);

      free (lX);
      free (lY);

      return;
    }

  MessageBox (GetDesktopWindow (),"Polygon()","R_Proxy_Graphics",MB_OK);
}
// 01-01-23 | baier | added "col" parameter
static void R_Proxy_Graphics_Polyline (int pCount,
				       double* pX,
				       double* pY,
				       int pCoords,
				       DevDesc* pDD)
{
  if (__graphics_device)
    {
      // convert coordinates here: don't use alloca() -> overflow?
      double* lX = (double*) malloc (pCount * sizeof (double));
      double* lY = (double*) malloc (pCount * sizeof (double));
      int i = 0;

      // could use memcpy() for speed
      for (i = 0;i < pCount;i++)
	{
	  char x[1000];
	  lX[i] = pX[i];
	  lY[i] = pY[i];

	  GConvert (&(lX[i]),&(lY[i]),pCoords,DEVICE,pDD);

	  sprintf (x,"Polyline: coord %d is %f/%f (was %f/%f)\n",
		   i,lX[i],lY[i],pX[i],pY[i]);
	  //	  OutputDebugString (x);

	}

      __graphics_device->vtbl->polyline (__graphics_device,
					 pCount,lX,lY,pDD->gp.col);

      free (lX);
      free (lY);

      return;
    }

  MessageBox (GetDesktopWindow (),"Polyline()","R_Proxy_Graphics",MB_OK);
}
// 00-06-22 | baier | added line type and width
static void R_Proxy_Graphics_Rect (double pX0,
				   double pY0,
				   double pX1,
				   double pY1,
				   int pCoords,
				   int pBG,
				   int pFG,
				   DevDesc* pDD)
{
  if (__graphics_device)
    {
      // convert to device coordinates
      GConvert (&pX0,&pY0,pCoords,DEVICE,pDD);
      GConvert (&pX1,&pY1,pCoords,DEVICE,pDD);

      __graphics_device->vtbl->rect (__graphics_device,
				     pX0,pY0,pX1,pY1,pBG,pFG,
				     pDD->dp.lty,pDD->dp.lwd);
      return;
    }

  MessageBox (GetDesktopWindow (),"Rect()","R_Proxy_Graphics",MB_OK);
}
static void R_Proxy_Graphics_Resize (DevDesc* pDD)
{
  if (__graphics_device)
    {
      __graphics_device->vtbl->resize (__graphics_device);
      return;
    }

  MessageBox (GetDesktopWindow (),"Resize()","R_Proxy_Graphics",MB_OK);
}
// 00-06-22 | baier | added font and size parameters
static double R_Proxy_Graphics_StrWidth (char* pString,DevDesc* pDD)
{
  if (__graphics_device)
    {
      int lSize = pDD->gp.cex * pDD->gp.ps + 0.5;
      return __graphics_device->vtbl->strwidth (__graphics_device,
						pString,
						pDD->gp.font,
						lSize);
    }

  MessageBox (GetDesktopWindow (),"StrWidth()","R_Proxy_Graphics",MB_OK);
  return 0.0;
}

// 00-06-22 | baier | added color, font and size
static void R_Proxy_Graphics_Text (double pX,
				   double pY,
				   int pCoords,
				   char* pString,
				   double pRot,
				   double pHadj,
				   DevDesc* pDD)
{
  if (__graphics_device)
    {
      int lSize = pDD->gp.cex * pDD->gp.ps + 0.5;

      // convert to device coordinates
      GConvert (&pX,&pY,pCoords,DEVICE,pDD);

      __graphics_device->vtbl->text (__graphics_device,
				     pX,pY,pString,pRot,pHadj,
				     pDD->gp.col,pDD->gp.font,lSize);
      return;
    }

  MessageBox (GetDesktopWindow (),"Text()","R_Proxy_Graphics",MB_OK);
}

// 00-06-22 | baier | added font and size parameters
static void R_Proxy_Graphics_MetricInfo (int pC,
					 double* pAscent,
					 double* pDescent,
					 double* pWidth,
					 DevDesc* pDD)
{
  if (__graphics_device)
    {
      int lSize = pDD->gp.cex * pDD->gp.ps + 0.5;
      __graphics_device->vtbl->metricinfo (__graphics_device,
					   pC,
					   pAscent,
					   pDescent,
					   pWidth,
					   pDD->gp.font,
					   lSize);
      return;
    }

  MessageBox (GetDesktopWindow (),"MetricInfo()","R_Proxy_Graphics",MB_OK);
}

// 01-01-25 | baier | new paramters
int R_Proxy_Graphics_Driver (DevDesc* pDD,
			     char* pDisplay,
			     double pWidth,
			     double pHeight,
			     double pPointSize,
			     Rboolean pRecording,
			     int pResize,
			     struct _SC_GraphicsDevice* pDevice)
{
  pDD->dp.font = 1;
  pDD->dp.ps = pPointSize;

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

  pDD->dp.open = R_Proxy_Graphics_Open;
  pDD->dp.close = R_Proxy_Graphics_Close;
  pDD->dp.activate = R_Proxy_Graphics_Activate;
  pDD->dp.deactivate = R_Proxy_Graphics_Deactivate;
  pDD->dp.resize = R_Proxy_Graphics_Resize;
  pDD->dp.newPage = R_Proxy_Graphics_NewPage;
  pDD->dp.clip = R_Proxy_Graphics_Clip;
  pDD->dp.strWidth = R_Proxy_Graphics_StrWidth;
  pDD->dp.text = R_Proxy_Graphics_Text;
  pDD->dp.rect = R_Proxy_Graphics_Rect;
  pDD->dp.circle = R_Proxy_Graphics_Circle;
  pDD->dp.line = R_Proxy_Graphics_Line;
  pDD->dp.polyline = R_Proxy_Graphics_Polyline;
  pDD->dp.polygon = R_Proxy_Graphics_Polygon;
  pDD->dp.locator = R_Proxy_Graphics_Locator;
  pDD->dp.mode = R_Proxy_Graphics_Mode;
  pDD->dp.hold = R_Proxy_Graphics_Hold;
  pDD->dp.metricInfo = R_Proxy_Graphics_MetricInfo;

    /* set graphics parameters that must be set by device driver */
    /* Window Dimensions in Pixels */
  pDD->dp.left = 0;	/* left */
  pDD->dp.right = DEV_X;	/* right */
  pDD->dp.top = 0;	/* top */
  pDD->dp.bottom = DEV_Y;	/* bottom */


  /* Nominal Character Sizes in Pixels */
  pDD->dp.cra[0] = 10;
  pDD->dp.cra[1] = 10;
  /* Character Addressing Offsets */
  /* These are used to plot a single plotting character */
  /* so that it is exactly over the plotting point */

  pDD->dp.xCharOffset = 0.50;
  pDD->dp.yCharOffset = 0.40;
  pDD->dp.yLineBias = 0.1;

  /* Inches per raster unit */

  pDD->dp.ipr[0] = 1.0 / 72.0;
  pDD->dp.ipr[1] = 1.0 / 72.0;

    /* Device capabilities */
    /* Clipping is problematic for X11 */
    /* Graphics is clipped, text is not */

  pDD->dp.canResizePlot = 1;
  pDD->dp.canChangeFont = 0;
  pDD->dp.canRotateText = 1;
  pDD->dp.canResizeText = 1;
  pDD->dp.canClip = 1;

  /* initialise x11 device description (most of the work */
  /* has been done in X11_Open) */

  pDD->displayListOn = 1;
  return 1;
}
