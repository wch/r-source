/*******************************************************************************
 *  RProxy: Connector implementation between application and R language
 *  Copyright (C) 2000--2006 Thomas Baier
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
 *  Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *  MA 02110-1301, USA.
 *
 ******************************************************************************/

/* virtual device size */
#define DEV_X 500
#define DEV_Y 500

#include <config.h>
#include <stdio.h>
#include <windows.h>

#include "SC_proxy.h"
#include "rproxy_impl.h"


#ifdef MessageBox
#undef MessageBox
#endif
#define MessageBox(a,b,c,d)

/******************************************************************************
 Callback Device (SC_Graphics_Device forwarder)
 ******************************************************************************/

/* 06-08-20 | baier | R_Proxy_Graphics_CB instead of NewDevDesc */
static void R_Proxy_Graphics_Activate_CB(R_Proxy_Graphics_CB* dd);
static void R_Proxy_Graphics_Circle_CB(double x, double y, double r,
				    R_GE_gcontext *gc,
				    R_Proxy_Graphics_CB* dd);
static void R_Proxy_Graphics_Clip_CB(double x0, double x1, double y0, double y1,
		     R_Proxy_Graphics_CB* dd);
static void R_Proxy_Graphics_Close_CB(R_Proxy_Graphics_CB* dd);
static void R_Proxy_Graphics_Deactivate_CB(R_Proxy_Graphics_CB* dd);
static void R_Proxy_Graphics_Hold_CB(R_Proxy_Graphics_CB* dd);
static Rboolean R_Proxy_Graphics_Locator_CB(double *x, double *y, R_Proxy_Graphics_CB* dd);
static void R_Proxy_Graphics_Line_CB(double x1, double y1, double x2, double y2,
				  R_GE_gcontext *gc,
				  R_Proxy_Graphics_CB* dd);
static void R_Proxy_Graphics_MetricInfo_CB(int c, 
					R_GE_gcontext *gc,
					double* ascent, double* descent,
					double* width, R_Proxy_Graphics_CB* dd);
static void R_Proxy_Graphics_Mode_CB(int mode, R_Proxy_Graphics_CB* dd);
static void R_Proxy_Graphics_NewPage_CB(R_GE_gcontext *gc,
				     R_Proxy_Graphics_CB* dd);
static void R_Proxy_Graphics_Polygon_CB(int n, double *x, double *y,
				     R_GE_gcontext *gc,
				     R_Proxy_Graphics_CB* dd);
static void R_Proxy_Graphics_Polyline_CB(int n, double *x, double *y,
				      R_GE_gcontext *gc,
				      R_Proxy_Graphics_CB* dd);
static void R_Proxy_Graphics_Rect_CB(double x0, double y0, double x1, double y1,
				  R_GE_gcontext *gc,
				  R_Proxy_Graphics_CB* dd);
static void R_Proxy_Graphics_Size_CB(double *left, double *right,
				  double *bottom, double *top,
				  R_Proxy_Graphics_CB* dd);
#ifdef UNUSED
static void R_Proxy_Graphics_Resize_CB(R_Proxy_Graphics_CB* dd);
#endif
static double R_Proxy_Graphics_StrWidth_CB(char *str, 
					R_GE_gcontext *gc,
					R_Proxy_Graphics_CB* dd);
static void R_Proxy_Graphics_Text_CB(double x, double y, char *str,
				  double rot, double hadj,
				  R_GE_gcontext *gc,
				  R_Proxy_Graphics_CB* dd);
static Rboolean R_Proxy_Graphics_Open_CB (R_Proxy_Graphics_CB* pDD,
				       void* pAXD,
				       char* pDisplay,
				       double pWidth,
				       double pHeight,
				       Rboolean pRecording,
				       int pResize);


/* 06-05-17 | baier | use GFX access macros */
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static void R_Proxy_Graphics_Activate_CB(R_Proxy_Graphics_CB* pDD)
{
  if(HASGFXDEV()) {
    GFXDEV()->vtbl->activate(GFXDEV());
    return;
  }
}
/* 00-06-22 | baier | added line type and width */
/* 06-05-17 | baier | use GFX access macros */
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static void R_Proxy_Graphics_Circle_CB(double pX,
				       double pY,
				       double pRad,
				       R_GE_gcontext *gc,
				       R_Proxy_Graphics_CB* pDD)
{
/*  OutputDebugString("R_Proxy_Graphics_Circle()\n"); */
  if(HASGFXDEV()) {
    GFXDEV()->vtbl->circle (GFXDEV(),
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


/* 06-05-17 | baier | use GFX access macros */
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static void R_Proxy_Graphics_Clip_CB(double pX0,
				     double pX1,
				     double pY0,
				     double pY1,
				     R_Proxy_Graphics_CB* pDD)
{
  if(HASGFXDEV()) {
    GFXDEV()->vtbl->clip (GFXDEV(),
			   pX0,pX1,pY0,pY1);
    return;
  }

  MessageBox (GetDesktopWindow (),"Clip()","R_Proxy_Graphics",MB_OK);
}

/* 06-05-17 | baier | use GFX access macros */
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static void R_Proxy_Graphics_Close_CB(R_Proxy_Graphics_CB* pDD)
{
  if(HASGFXDEV()) {
    GFXDEV()->vtbl->close (GFXDEV());
      return;
    }

  MessageBox (GetDesktopWindow (),"Close()","R_Proxy_Graphics",MB_OK);
}

/* 06-05-17 | baier | use GFX access macros */
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static void R_Proxy_Graphics_Deactivate_CB(R_Proxy_Graphics_CB* pDD)
{
  if(HASGFXDEV()) {
    GFXDEV()->vtbl->deactivate (GFXDEV());
    return;
  }

  MessageBox (GetDesktopWindow (),"Deactivate()","R_Proxy_Graphics",MB_OK);
}

/* 06-05-17 | baier | use GFX access macros */
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static void R_Proxy_Graphics_Hold_CB(R_Proxy_Graphics_CB* pDD)
{
  if(HASGFXDEV()) {
    GFXDEV()->vtbl->hold (GFXDEV());
    return;
  }

  MessageBox (GetDesktopWindow (),"Hold()","R_Proxy_Graphics",MB_OK);
}

/* 00-06-22 | baier | added color, line type and width */
/* 06-05-17 | baier | use GFX access macros */
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static void R_Proxy_Graphics_Line_CB(double pX0,
				     double pY0,
				     double pX1,
				     double pY1,
				     R_GE_gcontext *gc,
				     R_Proxy_Graphics_CB* pDD)
{
  if(HASGFXDEV()) { 
    GFXDEV()->vtbl->line (GFXDEV(),
			   pX0,pY0,pX1,pY1,gc->col,
			   gc->lty,gc->lwd);
    return;
  }

  MessageBox (GetDesktopWindow (),"Line()","R_Proxy_Graphics",MB_OK);
}

/* 01-01-25 | changed return type */
/* 06-05-17 | baier | use GFX access macros */
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static Rboolean R_Proxy_Graphics_Locator_CB(double* pX,
					    double* pY,
					    R_Proxy_Graphics_CB* pDD)
{
  if(HASGFXDEV()) {
    return GFXDEV()->vtbl->locator (GFXDEV(),pX,pY);
  }

  MessageBox (GetDesktopWindow (),"Locator()","R_Proxy_Graphics",MB_OK);
  return 0;
}

/* 06-05-17 | baier | use GFX access macros */
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static void R_Proxy_Graphics_Mode_CB(int pMode, R_Proxy_Graphics_CB* dd)
{
  if(HASGFXDEV()) {
    GFXDEV()->vtbl->mode (GFXDEV(),pMode);
    return;
  }

  MessageBox (GetDesktopWindow (),"Mode()","R_Proxy_Graphics",MB_OK);
}

/* 06-05-17 | baier | use GFX access macros */
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static void R_Proxy_Graphics_NewPage_CB(R_GE_gcontext *gc,
					R_Proxy_Graphics_CB* pDD)
{
  if(HASGFXDEV()) {
    GFXDEV()->vtbl->newpage (GFXDEV());
    return;
  }

  MessageBox (GetDesktopWindow (),"NewPage()","R_Proxy_Graphics",MB_OK);
}

/* 01-01-25 | baier | added new parameters "recording", "resize" */
/* 06-05-17 | baier | use GFX access macros */
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static Rboolean R_Proxy_Graphics_Open_CB(R_Proxy_Graphics_CB* pDD,
					 void* pAXD,
					 char* pDisplay,
					 double pWidth,
					 double pHeight,
					 Rboolean pRecording,
					 int pResize)
{
  if(HASGFXDEV()) {
    return GFXDEV()->vtbl->open (GFXDEV(),
				  pDisplay,pWidth,pHeight,0.0,0,0);
  }

  MessageBox (GetDesktopWindow (),"Open()","R_Proxy_Graphics",MB_OK);
  return 1;
}

/* 06-05-17 | baier | use GFX access macros */
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static void R_Proxy_Graphics_Polygon_CB(int pCount,
					double* pX,
					double* pY,
					R_GE_gcontext *gc,
					R_Proxy_Graphics_CB* pDD)
{
  if(HASGFXDEV()) {
    /* convert coordinates here: don't use alloca() -> overflow? */
    double* lX = (double*) malloc (pCount * sizeof (double));
    double* lY = (double*) malloc (pCount * sizeof (double));
    int i = 0;
    
    /* could use memcpy() for speed */
    for (i = 0;i < pCount;i++) {
      lX[i] = pX[i];
      lY[i] = pY[i];
      
    }
    
    /*      sprintf (x,"device::Polygon: bg is %08x, fg is %08x\n",fill,col); */
    /*      OutputDebugString (x); */
    GFXDEV()->vtbl->polygon (GFXDEV(),
			      pCount,lX,lY,gc->fill,gc->col);
    free (lX);
    free (lY);
    
    return;
  }

  MessageBox (GetDesktopWindow (),"Polygon()","R_Proxy_Graphics",MB_OK);
}

/* 01-01-23 | baier | added "col" parameter */
/* 06-05-16 | baier | added "lty" and "lwd" parameters */
/* 06-05-17 | baier | use GFX access macros */
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static void R_Proxy_Graphics_Polyline_CB(int pCount,
					 double* pX,
					 double* pY,
					 R_GE_gcontext *gc,
					 R_Proxy_Graphics_CB* pDD)
{
  if(HASGFXDEV()) {
    /* convert coordinates here: don't use alloca() -> overflow? */
    double* lX = (double*) malloc (pCount * sizeof (double));
    double* lY = (double*) malloc (pCount * sizeof (double));
    int i = 0;

    /* could use memcpy() for speed */
    for(i = 0;i < pCount;i++) {
      char x[1000];
      lX[i] = pX[i];
      lY[i] = pY[i];
      
      sprintf (x,"Polyline: coord %d is %f/%f (was %f/%f)\n",
	       i,lX[i],lY[i],pX[i],pY[i]);
      /*	  OutputDebugString (x); */
      
    }
    
    GFXDEV()->vtbl->polyline (GFXDEV(),
			       pCount,lX,lY,gc->col);
    
    free (lX);
    free (lY);
    
    return;
  }

  MessageBox (GetDesktopWindow (),"Polyline()","R_Proxy_Graphics",MB_OK);
}

/* 00-06-22 | baier | added line type and width */
/* 06-05-17 | baier | use GFX access macros */
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static void R_Proxy_Graphics_Rect_CB(double pX0,
				     double pY0,
				     double pX1,
				     double pY1,
				     R_GE_gcontext *gc,
				     R_Proxy_Graphics_CB* pDD)
{
  if(HASGFXDEV()) {
    GFXDEV()->vtbl->rect (GFXDEV(),
			   pX0,pY0,pX1,pY1,gc->fill,gc->col,
			   gc->lty,gc->lwd);
    return;
  }
  
  MessageBox (GetDesktopWindow (),"Rect()","R_Proxy_Graphics",MB_OK);
}
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static void R_Proxy_Graphics_Size_CB(double *left, double *right,
				     double *bottom, double *top,
				     R_Proxy_Graphics_CB* pDD)
{
    *left = DEVDESC(pDD)->left;
    *right = DEVDESC(pDD)->right;
    *bottom = DEVDESC(pDD)->bottom;
    *top = DEVDESC(pDD)->top;
}

#ifdef UNUSED
/* 06-05-17 | baier | use GFX access macros */
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static void R_Proxy_Graphics_Resize_CB(R_Proxy_Graphics_CB* pDD)
{
  if(HASGFXDEV()) {
    GFXDEV()->vtbl->resize (GFXDEV());
    return;
  }

  MessageBox (GetDesktopWindow (),"Resize()","R_Proxy_Graphics",MB_OK);
}
#endif

/* 00-06-22 | baier | added font and size parameters */
/* 06-05-17 | baier | use GFX access macros */
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static double R_Proxy_Graphics_StrWidth_CB(char* pString,
					 R_GE_gcontext *gc,
					 R_Proxy_Graphics_CB* pDD)
{
  if(HASGFXDEV()) {
    int lSize = gc->cex * gc->ps + 0.5;
    return GFXDEV()->vtbl->strwidth (GFXDEV(),
				      pString,
				      gc->fontface,
				      lSize);
  }
  
  MessageBox (GetDesktopWindow (),"StrWidth()","R_Proxy_Graphics",MB_OK);
  return 0.0;
}

/* 00-06-22 | baier | added color, font and size */
/* 06-05-17 | baier | use GFX access macros */
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static void R_Proxy_Graphics_Text_CB(double pX,
				     double pY,
				     char* pString,
				     double pRot,
				     double pHadj,
				     R_GE_gcontext *gc,
				     R_Proxy_Graphics_CB* pDD)
{
  if(HASGFXDEV()) {
    int lSize = gc->cex * gc->ps + 0.5;
    
    GFXDEV()->vtbl->text (GFXDEV(),
			   pX,pY,pString,pRot,pHadj,
			   gc->col,gc->fontface,lSize);
    return;
  }

  MessageBox (GetDesktopWindow (),"Text()","R_Proxy_Graphics",MB_OK);
}

/* 00-06-22 | baier | added font and size parameters */
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static void R_Proxy_Graphics_MetricInfo_CB(int c, 
					   R_GE_gcontext *gc,
					   double* ascent, double* descent,
					   double* width, R_Proxy_Graphics_CB* dd);

/* 06-05-17 | baier | use GFX access macros */
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static void R_Proxy_Graphics_MetricInfo_CB(int pC,
					   R_GE_gcontext *gc,
					   double* pAscent,
					   double* pDescent,
					   double* pWidth,
					   R_Proxy_Graphics_CB* pDD)
{
  if(HASGFXDEV()) {
    int lSize = gc->cex * gc->ps + 0.5;
    GFXDEV()->vtbl->metricinfo (GFXDEV(),
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
/* 04-09-27 | baier | startcol, startfill */
/* 06-08-20 | baier | restructured */
int R_Proxy_Graphics_Driver_CB(R_Proxy_Graphics_CB* pDD,
			       char* pDisplay,
			       double pWidth,
			       double pHeight,
			       double pPointSize,
			       Rboolean pRecording,
			       int pResize)
{
  DEVDESC(pDD)->startfont = 1;
  DEVDESC(pDD)->startps = pPointSize;
  DEVDESC(pDD)->startcol = R_RGB(0, 0, 0);
  DEVDESC(pDD)->startfill = R_TRANWHITE;
  DEVDESC(pDD)->startlty = LTY_SOLID;
  DEVDESC(pDD)->startgamma = 1;

  /* init the device-specific functionality here */
  DEVDESC(pDD)->deviceSpecific = (void *) NULL;

  /* Start the Device Driver and Hardcopy.  */
  if(!R_Proxy_Graphics_Open_CB (pDD,
				     NULL,
				     pDisplay,
				     pWidth,
				     pHeight,
				     pRecording,
				     pResize)) {
    return 0;
  }
  /* Set up Data Structures  */

  DEVDESC(pDD)->newDevStruct = 1;

  DEVDESC(pDD)->open = R_Proxy_Graphics_Open_CB;
  DEVDESC(pDD)->close = R_Proxy_Graphics_Close_CB;
  DEVDESC(pDD)->activate = R_Proxy_Graphics_Activate_CB;
  DEVDESC(pDD)->deactivate = R_Proxy_Graphics_Deactivate_CB;
  DEVDESC(pDD)->size = R_Proxy_Graphics_Size_CB;
  DEVDESC(pDD)->newPage = R_Proxy_Graphics_NewPage_CB;
  DEVDESC(pDD)->clip = R_Proxy_Graphics_Clip_CB;
  DEVDESC(pDD)->strWidth = R_Proxy_Graphics_StrWidth_CB;
  DEVDESC(pDD)->text = R_Proxy_Graphics_Text_CB;
  DEVDESC(pDD)->rect = R_Proxy_Graphics_Rect_CB;
  DEVDESC(pDD)->circle = R_Proxy_Graphics_Circle_CB;
  DEVDESC(pDD)->line = R_Proxy_Graphics_Line_CB;
  DEVDESC(pDD)->polyline = R_Proxy_Graphics_Polyline_CB;
  DEVDESC(pDD)->polygon = R_Proxy_Graphics_Polygon_CB;
  DEVDESC(pDD)->locator = R_Proxy_Graphics_Locator_CB;
  DEVDESC(pDD)->mode = R_Proxy_Graphics_Mode_CB;
  DEVDESC(pDD)->hold = R_Proxy_Graphics_Hold_CB;
  DEVDESC(pDD)->metricInfo = R_Proxy_Graphics_MetricInfo_CB;

    /* set graphics parameters that must be set by device driver */
    /* Window Dimensions in Pixels */
  DEVDESC(pDD)->left = 0;	/* left */
  DEVDESC(pDD)->right = DEV_X;	/* right */
  DEVDESC(pDD)->top = 0;	/* top */
  DEVDESC(pDD)->bottom = DEV_Y;	/* bottom */


  /* Nominal Character Sizes in Pixels */
  DEVDESC(pDD)->cra[0] = 10;
  DEVDESC(pDD)->cra[1] = 10;
  /* Character Addressing Offsets */
  /* These are used to plot a single plotting character */
  /* so that it is exactly over the plotting point */

  DEVDESC(pDD)->xCharOffset = 0.50;
  DEVDESC(pDD)->yCharOffset = 0.40;
  DEVDESC(pDD)->yLineBias = 0.1;

  /* Inches per raster unit */

  DEVDESC(pDD)->ipr[0] = 1.0 / 72.0;
  DEVDESC(pDD)->ipr[1] = 1.0 / 72.0;

    /* Device capabilities */
    /* Clipping is problematic for X11 */
    /* Graphics is clipped, text is not */

  DEVDESC(pDD)->canResizePlot = 1;
  DEVDESC(pDD)->canChangeFont = 1;
  DEVDESC(pDD)->canRotateText = 1;
  DEVDESC(pDD)->canResizeText = 1;
  DEVDESC(pDD)->canClip = 1;
  DEVDESC(pDD)->displayListOn = 1;
  DEVDESC(pDD)->canChangeGamma = 1;

  return 1;
}


/******************************************************************************
 Recorder Device
 ******************************************************************************/
static void R_Proxy_Graphics_Activate_Recorder(R_Proxy_Graphics_Recorder* dd);
static void R_Proxy_Graphics_Circle_Recorder(double x, double y, double r,
				    R_GE_gcontext *gc,
				    R_Proxy_Graphics_Recorder* dd);
static void R_Proxy_Graphics_Clip_Recorder(double x0, double x1, double y0, double y1,
		     R_Proxy_Graphics_Recorder* dd);
static void R_Proxy_Graphics_Close_Recorder(R_Proxy_Graphics_Recorder* dd);
static void R_Proxy_Graphics_Deactivate_Recorder(R_Proxy_Graphics_Recorder* dd);
static void R_Proxy_Graphics_Hold_Recorder(R_Proxy_Graphics_Recorder* dd);
static Rboolean R_Proxy_Graphics_Locator_Recorder(double *x, double *y, R_Proxy_Graphics_Recorder* dd);
static void R_Proxy_Graphics_Line_Recorder(double x1, double y1, double x2, double y2,
				  R_GE_gcontext *gc,
				  R_Proxy_Graphics_Recorder* dd);
static void R_Proxy_Graphics_MetricInfo_Recorder(int c, 
					R_GE_gcontext *gc,
					double* ascent, double* descent,
					double* width, R_Proxy_Graphics_Recorder* dd);
static void R_Proxy_Graphics_Mode_Recorder(int mode, R_Proxy_Graphics_Recorder* dd);
static void R_Proxy_Graphics_NewPage_Recorder(R_GE_gcontext *gc,
				     R_Proxy_Graphics_Recorder* dd);
static void R_Proxy_Graphics_Polygon_Recorder(int n, double *x, double *y,
				     R_GE_gcontext *gc,
				     R_Proxy_Graphics_Recorder* dd);
static void R_Proxy_Graphics_Polyline_Recorder(int n, double *x, double *y,
				      R_GE_gcontext *gc,
				      R_Proxy_Graphics_Recorder* dd);
static void R_Proxy_Graphics_Rect_Recorder(double x0, double y0, double x1, double y1,
				  R_GE_gcontext *gc,
				  R_Proxy_Graphics_Recorder* dd);
static void R_Proxy_Graphics_Size_Recorder(double *left, double *right,
				  double *bottom, double *top,
				  R_Proxy_Graphics_Recorder* dd);
#ifdef UNUSED
static void R_Proxy_Graphics_Resize_Recorder(R_Proxy_Graphics_Recorder* dd);
#endif
static double R_Proxy_Graphics_StrWidth_Recorder(char *str, 
					R_GE_gcontext *gc,
					R_Proxy_Graphics_Recorder* dd);
static void R_Proxy_Graphics_Text_Recorder(double x, double y, char *str,
				  double rot, double hadj,
				  R_GE_gcontext *gc,
				  R_Proxy_Graphics_Recorder* dd);
static Rboolean R_Proxy_Graphics_Open_Recorder (R_Proxy_Graphics_Recorder* pDD,
				       void* pAXD,
				       char* pDisplay,
				       double pWidth,
				       double pHeight,
				       Rboolean pRecording,
				       int pResize);


static void R_Proxy_Graphics_Activate_Recorder(R_Proxy_Graphics_Recorder* pDD)
{
}

static void R_Proxy_Graphics_Circle_Recorder(double pX,
					     double pY,
					     double pRad,
					     R_GE_gcontext *gc,
					     R_Proxy_Graphics_Recorder* pDD)
{
}


static void R_Proxy_Graphics_Clip_Recorder(double pX0,
					   double pX1,
					   double pY0,
					   double pY1,
					   R_Proxy_Graphics_Recorder* pDD)
{
}

/* 06-05-17 | baier | use GFX access macros */
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static void R_Proxy_Graphics_Close_Recorder(R_Proxy_Graphics_Recorder* pDD)
{
  if(HASGFXDEV()) {
    GFXDEV()->vtbl->close (GFXDEV());
      return;
    }

  MessageBox (GetDesktopWindow (),"Close()","R_Proxy_Graphics",MB_OK);
}

/* 06-05-17 | baier | use GFX access macros */
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static void R_Proxy_Graphics_Deactivate_Recorder(R_Proxy_Graphics_Recorder* pDD)
{
  if(HASGFXDEV()) {
    GFXDEV()->vtbl->deactivate (GFXDEV());
    return;
  }

  MessageBox (GetDesktopWindow (),"Deactivate()","R_Proxy_Graphics",MB_OK);
}

/* 06-05-17 | baier | use GFX access macros */
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static void R_Proxy_Graphics_Hold_Recorder(R_Proxy_Graphics_Recorder* pDD)
{
  if(HASGFXDEV()) {
    GFXDEV()->vtbl->hold (GFXDEV());
    return;
  }

  MessageBox (GetDesktopWindow (),"Hold()","R_Proxy_Graphics",MB_OK);
}

/* 00-06-22 | baier | added color, line type and width */
/* 06-05-17 | baier | use GFX access macros */
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static void R_Proxy_Graphics_Line_Recorder(double pX0,
					   double pY0,
					   double pX1,
					   double pY1,
					   R_GE_gcontext *gc,
					   R_Proxy_Graphics_Recorder* pDD)
{
  if(HASGFXDEV()) { 
    GFXDEV()->vtbl->line (GFXDEV(),
			   pX0,pY0,pX1,pY1,gc->col,
			   gc->lty,gc->lwd);
    return;
  }

  MessageBox (GetDesktopWindow (),"Line()","R_Proxy_Graphics",MB_OK);
}

/* 01-01-25 | changed return type */
/* 06-05-17 | baier | use GFX access macros */
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static Rboolean R_Proxy_Graphics_Locator_Recorder(double* pX,
						  double* pY,
						  R_Proxy_Graphics_Recorder* pDD)
{
  if(HASGFXDEV()) {
    return GFXDEV()->vtbl->locator (GFXDEV(),pX,pY);
  }

  MessageBox (GetDesktopWindow (),"Locator()","R_Proxy_Graphics",MB_OK);
  return 0;
}

/* 06-05-17 | baier | use GFX access macros */
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static void R_Proxy_Graphics_Mode_Recorder(int pMode, R_Proxy_Graphics_Recorder* dd)
{
  if(HASGFXDEV()) {
    GFXDEV()->vtbl->mode (GFXDEV(),pMode);
    return;
  }

  MessageBox (GetDesktopWindow (),"Mode()","R_Proxy_Graphics",MB_OK);
}

/* 06-05-17 | baier | use GFX access macros */
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static void R_Proxy_Graphics_NewPage_Recorder(R_GE_gcontext *gc,
					      R_Proxy_Graphics_Recorder* pDD)
{
  if(HASGFXDEV()) {
    GFXDEV()->vtbl->newpage (GFXDEV());
    return;
  }

  MessageBox (GetDesktopWindow (),"NewPage()","R_Proxy_Graphics",MB_OK);
}

/* 01-01-25 | baier | added new parameters "recording", "resize" */
/* 06-05-17 | baier | use GFX access macros */
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static Rboolean R_Proxy_Graphics_Open_Recorder(R_Proxy_Graphics_Recorder* pDD,
					       void* pAXD,
					       char* pDisplay,
					       double pWidth,
					       double pHeight,
					       Rboolean pRecording,
					       int pResize)
{
  if(HASGFXDEV()) {
    return GFXDEV()->vtbl->open (GFXDEV(),
				  pDisplay,pWidth,pHeight,0.0,0,0);
  }

  MessageBox (GetDesktopWindow (),"Open()","R_Proxy_Graphics",MB_OK);
  return 1;
}

/* 06-05-17 | baier | use GFX access macros */
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static void R_Proxy_Graphics_Polygon_Recorder(int pCount,
					      double* pX,
					      double* pY,
					      R_GE_gcontext *gc,
					      R_Proxy_Graphics_Recorder* pDD)
{
  if(HASGFXDEV()) {
    /* convert coordinates here: don't use alloca() -> overflow? */
    double* lX = (double*) malloc (pCount * sizeof (double));
    double* lY = (double*) malloc (pCount * sizeof (double));
    int i = 0;
    
    /* could use memcpy() for speed */
    for (i = 0;i < pCount;i++) {
      lX[i] = pX[i];
      lY[i] = pY[i];
      
    }
    
    /*      sprintf (x,"device::Polygon: bg is %08x, fg is %08x\n",fill,col); */
    /*      OutputDebugString (x); */
    GFXDEV()->vtbl->polygon (GFXDEV(),
			      pCount,lX,lY,gc->fill,gc->col);
    free (lX);
    free (lY);
    
    return;
  }

  MessageBox (GetDesktopWindow (),"Polygon()","R_Proxy_Graphics",MB_OK);
}

/* 01-01-23 | baier | added "col" parameter */
/* 06-05-16 | baier | added "lty" and "lwd" parameters */
/* 06-05-17 | baier | use GFX access macros */
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static void R_Proxy_Graphics_Polyline_Recorder(int pCount,
					       double* pX,
					       double* pY,
					       R_GE_gcontext *gc,
					       R_Proxy_Graphics_Recorder* pDD)
{
  if(HASGFXDEV()) {
    /* convert coordinates here: don't use alloca() -> overflow? */
    double* lX = (double*) malloc (pCount * sizeof (double));
    double* lY = (double*) malloc (pCount * sizeof (double));
    int i = 0;

    /* could use memcpy() for speed */
    for(i = 0;i < pCount;i++) {
      char x[1000];
      lX[i] = pX[i];
      lY[i] = pY[i];
      
      sprintf (x,"Polyline: coord %d is %f/%f (was %f/%f)\n",
	       i,lX[i],lY[i],pX[i],pY[i]);
      /*	  OutputDebugString (x); */
      
    }
    
    GFXDEV()->vtbl->polyline (GFXDEV(),
			       pCount,lX,lY,gc->col);
    
    free (lX);
    free (lY);
    
    return;
  }

  MessageBox (GetDesktopWindow (),"Polyline()","R_Proxy_Graphics",MB_OK);
}

/* 00-06-22 | baier | added line type and width */
/* 06-05-17 | baier | use GFX access macros */
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static void R_Proxy_Graphics_Rect_Recorder(double pX0,
					   double pY0,
					   double pX1,
					   double pY1,
					   R_GE_gcontext *gc,
					   R_Proxy_Graphics_Recorder* pDD)
{
  if(HASGFXDEV()) {
    GFXDEV()->vtbl->rect (GFXDEV(),
			   pX0,pY0,pX1,pY1,gc->fill,gc->col,
			   gc->lty,gc->lwd);
    return;
  }
  
  MessageBox (GetDesktopWindow (),"Rect()","R_Proxy_Graphics",MB_OK);
}
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static void R_Proxy_Graphics_Size_Recorder(double *left, double *right,
					   double *bottom, double *top,
					   R_Proxy_Graphics_Recorder* pDD)
{
    *left = DEVDESC(pDD)->left;
    *right = DEVDESC(pDD)->right;
    *bottom = DEVDESC(pDD)->bottom;
    *top = DEVDESC(pDD)->top;
}

#ifdef UNUSED
/* 06-05-17 | baier | use GFX access macros */
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static void R_Proxy_Graphics_Resize_Recorder(R_Proxy_Graphics_Recorder* pDD)
{
  if(HASGFXDEV()) {
    GFXDEV()->vtbl->resize (GFXDEV());
    return;
  }

  MessageBox (GetDesktopWindow (),"Resize()","R_Proxy_Graphics",MB_OK);
}
#endif

/* 00-06-22 | baier | added font and size parameters */
/* 06-05-17 | baier | use GFX access macros */
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static double R_Proxy_Graphics_StrWidth_Recorder(char* pString,
						 R_GE_gcontext *gc,
						 R_Proxy_Graphics_Recorder* pDD)
{
#if 0
  if(HASGFXDEV()) {
    int lSize = gc->cex * gc->ps + 0.5;
    return GFXDEV()->vtbl->strwidth (GFXDEV(),
				      pString,
				      gc->fontface,
				      lSize);
  }
  
  MessageBox (GetDesktopWindow (),"StrWidth()","R_Proxy_Graphics",MB_OK);
#endif
  return 0.0;
}

/* 00-06-22 | baier | added color, font and size */
/* 06-05-17 | baier | use GFX access macros */
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static void R_Proxy_Graphics_Text_Recorder(double pX,
					   double pY,
					   char* pString,
					   double pRot,
					   double pHadj,
					   R_GE_gcontext *gc,
					   R_Proxy_Graphics_Recorder* pDD)
{
  if(HASGFXDEV()) {
    int lSize = gc->cex * gc->ps + 0.5;
    
    GFXDEV()->vtbl->text (GFXDEV(),
			   pX,pY,pString,pRot,pHadj,
			   gc->col,gc->fontface,lSize);
    return;
  }

  MessageBox (GetDesktopWindow (),"Text()","R_Proxy_Graphics",MB_OK);
}

/* 00-06-22 | baier | added font and size parameters */
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static void R_Proxy_Graphics_MetricInfo_Recorder(int c, 
						 R_GE_gcontext *gc,
						 double* ascent, double* descent,
						 double* width, R_Proxy_Graphics_Recorder* dd);

/* 06-05-17 | baier | use GFX access macros */
/* 06-08-20 | baier | R_Proxy_Graphics instead of NewDevDesc, Recorder device */
static void R_Proxy_Graphics_MetricInfo_Recorder(int pC,
						 R_GE_gcontext *gc,
						 double* pAscent,
						 double* pDescent,
						 double* pWidth,
						 R_Proxy_Graphics_Recorder* pDD)
{
#if 1
  *pAscent = 0.0;
  *pDescent = 0.0;
  *pWidth = 0.0;
#else
  if(HASGFXDEV()) {
    int lSize = gc->cex * gc->ps + 0.5;
    GFXDEV()->vtbl->metricinfo (GFXDEV(),
				 pC,
				 pAscent,
				 pDescent,
				 pWidth,
				 gc->fontface,
				 lSize);
    return;
  }

  MessageBox (GetDesktopWindow (),"MetricInfo()","R_Proxy_Graphics",MB_OK);
#endif
}

/* 06-08-20 | baier | restructured */
int R_Proxy_Graphics_Driver_Recorder(R_Proxy_Graphics_Recorder* pDD,
				     char* pDisplay,
				     double pWidth,
				     double pHeight,
				     double pPointSize,
				     Rboolean pRecording,
				     int pResize)
{
  DEVDESC(pDD)->startfont = 1;
  DEVDESC(pDD)->startps = pPointSize;
  DEVDESC(pDD)->startcol = R_RGB(0, 0, 0);
  DEVDESC(pDD)->startfill = R_TRANWHITE;
  DEVDESC(pDD)->startlty = LTY_SOLID;
  DEVDESC(pDD)->startgamma = 1;

  /* init the device-specific functionality here */
  DEVDESC(pDD)->deviceSpecific = (void *) NULL;

  /* Start the Device Driver and Hardcopy.  */
  if(!R_Proxy_Graphics_Open_Recorder(pDD,
				     NULL,
				     pDisplay,
				     pWidth,
				     pHeight,
				     pRecording,
				     pResize)) {
    return 0;
  }
  /* Set up Data Structures  */

  DEVDESC(pDD)->newDevStruct = 1;

  DEVDESC(pDD)->open = R_Proxy_Graphics_Open_Recorder;
  DEVDESC(pDD)->close = R_Proxy_Graphics_Close_Recorder;
  DEVDESC(pDD)->activate = R_Proxy_Graphics_Activate_Recorder;
  DEVDESC(pDD)->deactivate = R_Proxy_Graphics_Deactivate_Recorder;
  DEVDESC(pDD)->size = R_Proxy_Graphics_Size_Recorder;
  DEVDESC(pDD)->newPage = R_Proxy_Graphics_NewPage_Recorder;
  DEVDESC(pDD)->clip = R_Proxy_Graphics_Clip_Recorder;
  DEVDESC(pDD)->strWidth = R_Proxy_Graphics_StrWidth_Recorder;
  DEVDESC(pDD)->text = R_Proxy_Graphics_Text_Recorder;
  DEVDESC(pDD)->rect = R_Proxy_Graphics_Rect_Recorder;
  DEVDESC(pDD)->circle = R_Proxy_Graphics_Circle_Recorder;
  DEVDESC(pDD)->line = R_Proxy_Graphics_Line_Recorder;
  DEVDESC(pDD)->polyline = R_Proxy_Graphics_Polyline_Recorder;
  DEVDESC(pDD)->polygon = R_Proxy_Graphics_Polygon_Recorder;
  DEVDESC(pDD)->locator = R_Proxy_Graphics_Locator_Recorder;
  DEVDESC(pDD)->mode = R_Proxy_Graphics_Mode_Recorder;
  DEVDESC(pDD)->hold = R_Proxy_Graphics_Hold_Recorder;
  DEVDESC(pDD)->metricInfo = R_Proxy_Graphics_MetricInfo_Recorder;

    /* set graphics parameters that must be set by device driver */
    /* Window Dimensions in Pixels */
  DEVDESC(pDD)->left = 0;	/* left */
  DEVDESC(pDD)->right = DEV_X;	/* right */
  DEVDESC(pDD)->top = 0;	/* top */
  DEVDESC(pDD)->bottom = DEV_Y;	/* bottom */


  /* Nominal Character Sizes in Pixels */
  DEVDESC(pDD)->cra[0] = 10;
  DEVDESC(pDD)->cra[1] = 10;
  /* Character Addressing Offsets */
  /* These are used to plot a single plotting character */
  /* so that it is exactly over the plotting point */

  DEVDESC(pDD)->xCharOffset = 0.50;
  DEVDESC(pDD)->yCharOffset = 0.40;
  DEVDESC(pDD)->yLineBias = 0.1;

  /* Inches per raster unit */

  DEVDESC(pDD)->ipr[0] = 1.0 / 72.0;
  DEVDESC(pDD)->ipr[1] = 1.0 / 72.0;

    /* Device capabilities */
    /* Clipping is problematic for X11 */
    /* Graphics is clipped, text is not */

  DEVDESC(pDD)->canResizePlot = 1;
  DEVDESC(pDD)->canChangeFont = 1;
  DEVDESC(pDD)->canRotateText = 1;
  DEVDESC(pDD)->canResizeText = 1;
  DEVDESC(pDD)->canClip = 1;
  DEVDESC(pDD)->displayListOn = 1;
  DEVDESC(pDD)->canChangeGamma = 1;

  return 1;
}
