 /*
 *  R : A Computer Language for Statistical Data Analysis
 *  file devQuartz.c
 *  Copyright (C) 2002-2003  Stefano M. Iacus and the R core team
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

#ifndef __QUARTZ_DEVICE__
#define __QUARTZ_DEVICE__

 
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <Defn.h>
#include <Graphics.h>
#include <Rdevices.h>


#if defined(__APPLE_CC__) && defined(HAVE_AQUA)

#include <Carbon/Carbon.h>
#include <CoreFoundation/CoreFoundation.h>
#include <ApplicationServices/ApplicationServices.h>


#define R_RED(col)	(((col)	   )&255)
#define R_GREEN(col)	(((col)>> 8)&255)
#define R_BLUE(col)	(((col)>>16)&255)
#define kRAppSignature '0FFF'

extern  DL_FUNC ptr_GetQuartzParameters;


void GetQuartzParameters(double *width, double *height, double *ps, char *family, Rboolean *antialias, Rboolean *autorefresh) {ptr_GetQuartzParameters(width, height, ps, family, antialias, autorefresh);}


   /***************************************************************************/
   /* Each driver can have its own device-specic graphical                    */
   /* parameters and resources.  these should be wrapped                      */
   /* in a structure (like the x11Desc structure below)                       */
   /* and attached to the overall device description via                      */
   /* the dd->deviceSpecific pointer                                          */
   /* NOTE that there are generic graphical parameters                        */
   /* which must be set by the device driver, but are                         */
   /* common to all device types (see Graphics.h)                             */
   /* so go in the GPar structure rather than this device-                    */
   /* specific structure                                                      */
   /***************************************************************************/

typedef struct {
    int cex;
    int windowWidth;
    int windowHeight;
    Boolean resize;
    int Text_Font;          /* 0 is system font and 4 is monaco */
    int fontface;           /* Typeface */
    int fontsize;           /* Size in points */
    int usefixed;
	int color;		        /* color */
	int fill;	        	/* fill color */
    WindowPtr window;
    int	lineType;
    int lineWidth;
    Boolean Antialias;		/* Use Antialiasing */
    Boolean Autorefresh;
    char	*family;
    CGContextRef context;  /* This is the Contetx used by Quartz */
    double	xscale;
    double	yscale;
}
QuartzDesc;

extern OSStatus DoCloseHandler(EventHandlerCallRef inCallRef, EventRef inEvent, void* inUserData);
static const EventTypeSpec	RCloseWinEvent[] =
{
        { kEventClassWindow, kEventWindowClose }
};

Rboolean innerQuartzDeviceDriver(NewDevDesc *dd, char *display,
			 double width, double height, double pointsize,
			 char *family, Rboolean antialias, Rboolean autorefresh);

Rboolean QuartzDeviceDriver(DevDesc *dd, char *display,
			 double width, double height, double pointsize,
			 char *family, Rboolean antialias, Rboolean autorefresh);

OSStatus SetCGContext(QuartzDesc *xd);


/* Device primitives */

static Rboolean	Quartz_Open(NewDevDesc *, QuartzDesc *, char *,double, double);
static void 	Quartz_Close(NewDevDesc *dd);
static void 	Quartz_Activate(NewDevDesc *dd);
static void 	Quartz_Deactivate(NewDevDesc *dd);
static void 	Quartz_Size(double *left, double *right,
		     	 double *bottom, double *top, NewDevDesc *dd);
static void 	Quartz_NewPage(int fill, double gamma, NewDevDesc *dd);
static void 	Quartz_Clip(double x0, double x1, double y0, double y1,
		     	NewDevDesc *dd);
static double 	Quartz_StrWidth(char *str, int font,
			     double cex, double ps, NewDevDesc *dd);
static void 	Quartz_Text(double x, double y, char *str,
		     	 double rot, double hadj, int col, double gamma, int font,
		     	 double cex, double ps, NewDevDesc *dd);
static void 	Quartz_Rect(double x0, double y0, double x1, double y1,
		     	 int col, int fill, double gamma, int lty, double lwd,
		     	 NewDevDesc *dd);
static void 	Quartz_Circle(double x, double y, double r, int col,
				 int fill, double gamma, int lty, double lwd, NewDevDesc *dd);
static void 	Quartz_Line(double x1, double y1, double x2, double y2,
		     	 int col, double gamma, int lty, double lwd, NewDevDesc *dd);
static void 	Quartz_Polyline(int n, double *x, double *y, int col,
				 double gamma, int lty, double lwd, NewDevDesc *dd);
static void 	Quartz_Polygon(int n, double *x, double *y, int col, int fill,
				 double gamma, int lty, double lwd, NewDevDesc *dd);
static Rboolean Quartz_Locator(double *x, double *y, NewDevDesc *dd);
static void 	Quartz_Mode(int mode, NewDevDesc *dd);
static void 	Quartz_Hold(NewDevDesc *dd);
static void 	Quartz_MetricInfo(int c, int font, double cex, double ps,
			     double* ascent, double* descent, double* width,
			     NewDevDesc *dd);


static void Quartz_SetFill(int fill, double gamma,  NewDevDesc *dd);
static void Quartz_SetStroke(int color, double gamma,  NewDevDesc *dd);
static void Quartz_SetLineDash(int lty, NewDevDesc *dd);
static void Quartz_SetLineWidth(double lwd,  NewDevDesc *dd);
static void Quartz_SetFont(int font,  double cex, double ps,  NewDevDesc *dd);



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

/* int QuartzCount = 1; */

void HaveFlush(CGContextRef c, Rboolean flush);

void HaveFlush(CGContextRef c, Rboolean flush){

  if(flush)
   CGContextFlush(c);

/*  Flushing graphics at every graphic call
    is really slow. It should be something
    like that...but there should be a
    better way to do that!

  if ((QuartzCount++ % 10) == 0) {
    CGContextFlush(c);
	QuartzCount = 1 ;
    }
*/
}

/*  Quartz Device Driver Parameters:
 *  -----------------		cf with ../unix/X11/devX11.c
 *  display	= display
 *  width	= width in inches
 *  height	= height in inches
 *  ps		= pointsize
 *  family  = Postscript fon family name
 *  Antialias = wheter to make antialiasing
 */


SEXP do_Quartz(SEXP call, SEXP op, SEXP args, SEXP env)
{
    NewDevDesc *dev = NULL;
    GEDevDesc *dd;
    char *display, *vmax, *family=NULL;
    char fontfamily[255];
    double height, width, ps;
    Rboolean  antialias, autorefresh;
    gcall = call;
    vmax = vmaxget();
    display = SaveString(CAR(args), 0);
    args = CDR(args);
    width = asReal(CAR(args));	args = CDR(args);
    height = asReal(CAR(args)); args = CDR(args);
    if (width <= 0 || height <= 0)
	errorcall(call, "invalid width or height");
    ps = asReal(CAR(args));  args = CDR(args);
    family = SaveString(CAR(args), 0);    args = CDR(args);
    antialias = asLogical(CAR(args));   args = CDR(args);
    autorefresh = asLogical(CAR(args));



     R_CheckDeviceAvailable();
    /* Allocate and initialize the device driver data */
     BEGIN_SUSPEND_INTERRUPTS {
      if (!(dev = (NewDevDesc *) calloc(1, sizeof(NewDevDesc))))
	   return 0;
    /* Do this for early redraw attempts */
    dev->displayList = R_NilValue;
    /* Make sure that this is initialised before a GC can occur.
     * This (and displayList) get protected during GC
     */
    dev->savedSnapshot = R_NilValue;

    strcpy(fontfamily, family);
    GetQuartzParameters(&width, &height, &ps, fontfamily, &antialias, &autorefresh);

    if (!QuartzDeviceDriver((DevDesc *)dev, display, width, height, ps,
       fontfamily, antialias, autorefresh)) {
	 free(dev);
	 errorcall(call, "unable to start device Quartz\n");
    }
    gsetVar(install(".Device"), mkString("quartz"), R_NilValue);
    dd = GEcreateDevDesc(dev);
    addDevice((DevDesc*)dd);
    GEinitDisplayList(dd);
    } END_SUSPEND_INTERRUPTS;
    vmaxset(vmax);
    return R_NilValue;
}





Rboolean QuartzDeviceDriver(DevDesc *dd, char *display,
			 double width, double height, double pointsize,
			 char *family, Rboolean antialias, Rboolean autorefresh)
{
return innerQuartzDeviceDriver((NewDevDesc *)dd, display,
			 width,  height,  pointsize, family, antialias, autorefresh);
}


Rboolean innerQuartzDeviceDriver(NewDevDesc *dd, char *display,
			 double width, double height, double pointsize,
			 char *family, Rboolean antialias, Rboolean autorefresh)
{
    QuartzDesc *xd;
    int ps;
    Rect rect;
    OSStatus err;


    if (!(xd = (QuartzDesc *)malloc(sizeof(QuartzDesc))))
	return 0;

    if(!Quartz_Open(dd, xd, display, width, height))
     return(FALSE);

    ps = pointsize;
    if (ps < 6 || ps > 24) ps = 10;
    ps = 2 * (ps / 2);
    dd->startps = ps;
    dd->startfont = 1;
    dd->startlty = LTY_SOLID;
    dd->startgamma = 1;

    dd->newDevStruct = 1;

    dd->open       = Quartz_Open;
    dd->close      = Quartz_Close;
    dd->activate   = Quartz_Activate;
    dd->deactivate = Quartz_Deactivate;
    dd->size       = Quartz_Size;
    dd->newPage    = Quartz_NewPage;
    dd->clip       = Quartz_Clip;
    dd->strWidth   = Quartz_StrWidth;
    dd->text       = Quartz_Text;
    dd->rect       = Quartz_Rect;
    dd->circle     = Quartz_Circle;
    dd->line       = Quartz_Line;
    dd->polyline   = Quartz_Polyline;
    dd->polygon    = Quartz_Polygon;
    dd->locator    = Quartz_Locator;
    dd->mode       = Quartz_Mode;
    dd->hold       = Quartz_Hold;

    dd->metricInfo = Quartz_MetricInfo;

    dd->left        = 0;
    dd->right       =  xd->windowWidth;
    dd->bottom      =  xd->windowHeight;
    dd->top         = 0;

    dd->xCharOffset = 0.4900;
    dd->yCharOffset = 0.3333;
    dd->yLineBias = 0.1;

    dd->cra[0] = ps / 2;
    dd->cra[1] = ps;

    dd->ipr[0] = 1.0 / 72;
    dd->ipr[1] = 1.0 / 72;

    dd->canResizePlot = TRUE;
    dd->canChangeFont = TRUE;
    dd->canRotateText = TRUE;
    dd->canResizeText = TRUE;
    dd->canClip       = FALSE;
    dd->canHAdj = 0;
    dd->canChangeGamma = FALSE;


    /* It is used to set the font that you will be used on the postscript and
       drawing.
    */

    /* There is the place for you to set the default value of the MAC Devices */
    xd->cex = 1.0;
    xd->resize = 0;
    xd->Text_Font = 4; /* initial is monaco */
    xd->fontface = 0;  /* initial is plain text */
    xd->fontsize = 12; /* initial is 12 size */
    xd->Antialias = antialias; /* by default Antialias if on */
    xd->Autorefresh = autorefresh; /* by default it is on */

    if(family){
     xd->family = malloc(sizeof(family)+1);
     strcpy(xd->family,family);
    }
    else
     xd->family = NULL;

    err = SetCGContext(xd);

/* This scale factor is needed in MetricInfo */
    xd->xscale = width/72.0;
    xd->yscale = height/72.0;

    dd->deviceSpecific = (void *) xd;
    dd->displayListOn = TRUE;

    return 1;
}

OSStatus SetCGContext(QuartzDesc *xd)
{
    Rect rect;
    OSStatus	err = noErr;
    CGRect    cgRect;

    if(xd->context)
        CGContextRelease(xd->context);

    err = CreateCGContextForPort(GetWindowPort(xd->window), &xd->context);

    /*  Translate to QuickDraw coordinate system */

    GetPortBounds(GetWindowPort(xd->window), &rect);
    CGContextTranslateCTM(xd->context,0, (float)(rect.bottom - rect.top));

/* Be aware that by performing a negative scale in the following line of
   code, your text will also be flipped
*/
    CGContextScaleCTM(xd->context, 1, -1);

  /* We apply here Antialiasing if necessary */
    CGContextSetShouldAntialias(xd->context, xd->Antialias);

   return err;
}

static Rboolean	Quartz_Open(NewDevDesc *dd, QuartzDesc *xd, char *dsp,
		    double wid, double hgt)
{

	OSStatus	err;
	WindowRef 	devWindow =  NULL;
	Rect		devBounds;
        Str255		Title;
	char		buffer[250];
	int 		devnum = devNumber((DevDesc *)dd);

    xd->windowWidth = wid*72;
    xd->windowHeight = hgt*72;
    xd->window = NULL;
    xd->context = NULL;
    dd->startfill = R_RGB(255, 255, 255);
    dd->startcol = R_RGB(0, 0, 0);
    /* Create a new window with the specified size */


	SetRect(&devBounds, 400, 400, 400 + xd->windowWidth, 400 + xd->windowHeight ) ;

    err = CreateNewWindow( kDocumentWindowClass, kWindowStandardHandlerAttribute|kWindowVerticalZoomAttribute | kWindowCollapseBoxAttribute|kWindowResizableAttribute | kWindowCloseBoxAttribute ,
		& devBounds, & devWindow);


	sprintf(buffer,"Quartz (%d) - Active",devnum+1);
	CopyCStringToPascal(buffer,Title);
        SetWTitle(devWindow, Title);

	ShowWindow(devWindow);
	err = InstallWindowEventHandler( devWindow, NewEventHandlerUPP(DoCloseHandler),
                                          GetEventTypeCount(RCloseWinEvent),
                                          RCloseWinEvent, (void *)devWindow, NULL);
       
    if(err != noErr)
     return(0);

    xd->window = devWindow;
    xd->color = xd->fill = NA_INTEGER;
    xd->resize = false;
    xd->lineType = 0;
    xd->lineWidth = 1;
    return TRUE;
}

static void 	Quartz_Close(NewDevDesc *dd)
{
  QuartzDesc *xd = (QuartzDesc *) dd->deviceSpecific;

  if(xd->window)
   DisposeWindow(xd->window);

  if(xd->family)
   free(xd->family);

  if(xd->context)
   CGContextRelease(xd->context);

  free(xd);
}

static void 	Quartz_Activate(NewDevDesc *dd)
{
	Str255	Title;
	char	buffer[250];
	QuartzDesc *xd = (QuartzDesc*)dd->deviceSpecific;
	int devnum = devNumber((DevDesc *)dd);
        OSStatus err;

	sprintf(buffer,"Quartz (%d) - Active",devnum+1);
	CopyCStringToPascal(buffer,Title);
	SetWTitle(xd->window,Title);

/*
   We add a property to the Window each time we activate it.
   We should only make this the first time we open the device.
*/
        err = SetWindowProperty(xd->window,kRAppSignature,1,sizeof(int),&devnum);

	ShowWindow(xd->window);

}



static void 	Quartz_Deactivate(NewDevDesc *dd)
{
	Str255	Title;
	char	buffer[250];
	QuartzDesc *xd = (QuartzDesc*)dd->deviceSpecific;
	int devnum = devNumber((DevDesc *)dd);

	sprintf(buffer,"Quartz (%d) - Not Active",devnum+1);
	CopyCStringToPascal(buffer,Title);
	SetWTitle(xd->window,Title);
	ShowWindow(xd->window);
}


static void 	Quartz_Size(double *left, double *right,
		     	 double *bottom, double *top, NewDevDesc *dd)
{
    QuartzDesc *xd = (QuartzDesc*)dd->deviceSpecific;
    Rect portRect;

    GetWindowPortBounds ( xd->window, & portRect ) ;

    *left = 0.0;
    *right = portRect.right;
    *bottom = portRect.bottom;
    *top = 0.0;

    xd->windowWidth = *right - *left;
    xd->windowHeight = *bottom - *top;
    SetCGContext(xd);

    return;
}

void Quartz_ReSizeWin(NewDevDesc *dd)
{

}

static void 	Quartz_NewPage(int fill, double gamma, NewDevDesc *dd)
{
    QuartzDesc *xd = (QuartzDesc*)dd->deviceSpecific;
    CGPoint origin = {0.0, 0.0};
    CGSize  size;
    CGRect area;

    size.width = xd->windowWidth;
    size.height = xd->windowHeight;

    area.origin = origin;
    area.size = size;

    Quartz_SetFill(fill, gamma, dd);

    CGContextFillRect(xd->context, area);

    HaveFlush( xd->context, xd->Autorefresh );
}

static void 	Quartz_Clip(double x0, double x1, double y0, double y1,
		     	NewDevDesc *dd)
{
 return;
}

static double 	Quartz_StrWidth(char *str, int font,
			     double cex, double ps, NewDevDesc *dd)
{
    QuartzDesc *xd = (QuartzDesc*)dd->deviceSpecific;
    CGPoint position;

    CGContextSaveGState( xd->context );
    CGContextTranslateCTM( xd->context, 0, 0 );

    CGContextScaleCTM( xd->context, -1, 1);

    CGContextRotateCTM( xd->context, -1.0 * 3.1416);

    CGContextSetTextDrawingMode( xd->context, kCGTextInvisible );

    Quartz_SetFont(font, cex,  ps, dd);

    CGContextShowTextAtPoint( xd->context, 0, 0, str, strlen(str) );

    position = CGContextGetTextPosition(xd->context);

    CGContextRestoreGState( xd->context );

    return(position.x);
}


static void Quartz_SetFont(int font,  double cex, double ps, NewDevDesc *dd)
{
 	QuartzDesc *xd = (QuartzDesc*)dd->deviceSpecific;
    int size = cex * ps + 0.5;

	switch(font){

     case 5:
      CGContextSelectFont( xd->context, "Symbol", size, kCGEncodingMacRoman);
     break;

     default:
      if(xd->family)
       CGContextSelectFont( xd->context, xd->family, size, kCGEncodingMacRoman);
      else
       CGContextSelectFont( xd->context, "Helvetica", size, kCGEncodingMacRoman);
     break;
    }

}

static void 	Quartz_Text(double x, double y, char *str,
		     	 double rot, double hadj, int col, double gamma, int font,
		     	 double cex, double ps, NewDevDesc *dd)
{

    QuartzDesc *xd = (QuartzDesc*)dd->deviceSpecific;

    CGContextSaveGState( xd->context );
    CGContextTranslateCTM( xd->context, x, y );

    CGContextScaleCTM(xd->context, -1, 1);

    CGContextRotateCTM( xd->context, (-1.0  + 2*rot/360)  * 3.1416);

    Quartz_SetStroke( col, gamma, dd);

    CGContextSetTextDrawingMode( xd->context, kCGTextFill );
    Quartz_SetFill(col, gamma, dd);
    Quartz_SetFont(font, cex,  ps, dd);

    CGContextShowTextAtPoint( xd->context, 0, 0, str, strlen(str) );

    HaveFlush( xd->context, xd->Autorefresh );

    CGContextRestoreGState( xd->context );
}


static void 	Quartz_Rect(double x0, double y0, double x1, double y1,
		     	 int col, int fill, double gamma, int lty, double lwd,
		     	 NewDevDesc *dd)
{
    QuartzDesc *xd = (QuartzDesc*)dd->deviceSpecific;
	CGRect rect;
    CGPoint origin;
    CGSize  size;

    origin.x = x0;
    origin.y = y0;

    size.width = x1-x0;
    size.height = y1-y0;

    rect.size = size;
    rect.origin = origin;

    CGContextSaveGState( xd->context );

   	Quartz_SetLineWidth(lwd, dd);
    Quartz_SetLineDash(lty, dd);

    Quartz_SetFill( fill, gamma, dd);
    CGContextFillRect(xd->context, rect);

	Quartz_SetStroke( col, gamma, dd);
    CGContextStrokeRect(xd->context, rect);


    HaveFlush( xd->context, xd->Autorefresh );
    CGContextRestoreGState( xd->context );


}

static void 	Quartz_Circle(double x, double y, double r, int col,
				 int fill, double gamma, int lty, double lwd, NewDevDesc *dd)
{
    QuartzDesc *xd = (QuartzDesc*)dd->deviceSpecific;

    CGContextSaveGState( xd->context );


    CGContextBeginPath( xd->context );

    Quartz_SetLineWidth(lwd, dd);
    Quartz_SetLineDash(lty, dd);

	CGContextAddArc( xd->context, (float)x , (float)y, (float)r, 3.141592654 * 2.0, 0.0, 0);
 	Quartz_SetFill( fill, gamma, dd);
	CGContextFillPath( xd->context );

    Quartz_SetStroke( col, gamma, dd);
	CGContextAddArc( xd->context, (float)x , (float)y, (float)r, 3.141592654 * 2.0, 0.0, 0);
    CGContextStrokePath( xd->context );


    HaveFlush( xd->context, xd->Autorefresh );

    CGContextRestoreGState( xd->context );

}


static void 	Quartz_Line(double x1, double y1, double x2, double y2,
		     	 int col, double gamma, int lty, double lwd, NewDevDesc *dd)
{
    QuartzDesc *xd = (QuartzDesc*)dd->deviceSpecific;
    CGPoint lines[ 2 ];
    Rect rect;

    CGContextSaveGState( xd->context );


    CGContextBeginPath( xd->context );

    lines[0].x = (float)x1;
    lines[0].y = (float)y1;
    lines[1].x = (float)x2;
    lines[1].y = (float)y2;


 	Quartz_SetLineWidth(lwd,  dd);
    Quartz_SetLineDash(lty, dd);

 	CGContextAddLines( xd->context, &lines[0], 2 );

 	Quartz_SetStroke( col, gamma,  dd);

    CGContextStrokePath( xd->context);

    HaveFlush( xd->context, xd->Autorefresh );

    CGContextRestoreGState( xd->context );

}


static void 	Quartz_Polyline(int n, double *x, double *y, int col,
				 double gamma, int lty, double lwd, NewDevDesc *dd)
{
  	CGPoint *lines;
    int	i;
    CGrafPtr savedPort, port;
    QuartzDesc *xd = (QuartzDesc*)dd->deviceSpecific;

    lines = (CGPoint *)malloc(sizeof(CGPoint)*n);

    if(lines == NULL)
     return;

    for (i = 0; i < n; i++) {
	  lines[i].x = (float)x[i];
	  lines[i].y = (float)y[i];
	 }


     CGContextSaveGState( xd->context );

  	 Quartz_SetLineWidth(lwd,  dd);
     Quartz_SetLineDash(lty,  dd);

	 CGContextBeginPath( xd->context );
	 CGContextAddLines( xd->context, &lines[0], n );
	 Quartz_SetStroke( col, gamma, dd);
     CGContextStrokePath( xd->context );

     HaveFlush( xd->context, xd->Autorefresh );
     CGContextRestoreGState( xd->context );

}


#define MAX_DASH 6
static float Dash1[] = {1.0, 0.0};
static float Dash2[] = {1.0, 1.0};
static float Dash3[] = {2.0, 2.0};
static float Dash4[] = {4.0, 2.0};
static float Dash5[] = {4.0, 2.0};
static float Dash6[] = {6.0, 2.0, 2.0, 2.0};

float *dash[MAX_DASH] = {Dash1, Dash2, Dash3, Dash4, Dash5, Dash6 };

size_t dashn[MAX_DASH] = {2, 2, 2, 2, 2, 4};



static void Quartz_SetLineDash(int lty, NewDevDesc *dd)
{
    QuartzDesc *xd = (QuartzDesc*)dd->deviceSpecific;


	if(lty < 1)
	 lty = 1;


	if(lty > MAX_DASH) lty = MAX_DASH;

	xd->lineType = lty;

	if(lty <2)
	 return;

    CGContextSetLineDash(xd->context, 0, dash[lty-1], dashn[lty-1] );

}

static void Quartz_SetLineWidth(double lwd, NewDevDesc *dd)
{
 	QuartzDesc *xd = (QuartzDesc*)dd->deviceSpecific;

	if(lwd < 1)
	 lwd=1;

 	xd->lineWidth = lwd;


    CGContextSetLineWidth( xd->context, lwd );

}



static void Quartz_SetStroke(int color, double gamma, NewDevDesc *dd)
{
    QuartzDesc *xd = (QuartzDesc*)dd->deviceSpecific;
    float alpha = 1.0;

 	xd->color = color;

	if(color == NA_INTEGER)
	 alpha = 0.0;

    CGContextSetRGBStrokeColor(xd->context, (float)R_RED(color)/255.0, (float)R_GREEN(color)/255.0, (float)R_BLUE(color)/255.0, alpha);

}

static void Quartz_SetFill(int fill, double gamma, NewDevDesc *dd)
{
    QuartzDesc *xd = (QuartzDesc*)dd->deviceSpecific;
    float alpha = 1.0;

    xd->fill = fill;

 	if(fill == NA_INTEGER)
 	 alpha = 0.0;

    CGContextSetRGBFillColor( xd->context, (float)R_RED(fill)/255.0, (float)R_GREEN(fill)/255.0, (float)R_BLUE(fill)/255.0, alpha);

}

static void 	Quartz_Polygon(int n, double *x, double *y, int col, int fill,
				 double gamma, int lty, double lwd, NewDevDesc *dd)
{
   int	i;
   QuartzDesc *xd = (QuartzDesc*)dd->deviceSpecific;
   CGPoint *lines;


   CGContextSaveGState( xd->context );


   CGContextBeginPath( xd->context );
   Quartz_SetLineWidth(lwd, dd);
   Quartz_SetLineDash(lty,  dd);


    lines = (CGPoint *)malloc(sizeof(CGPoint)*(n+1));

    if(lines == NULL)
     return;

    for (i = 0; i < n; i++) {
	  lines[i].x = (float)x[i];
	  lines[i].y = (float)y[i];
	 }
	lines[n].x = (float)x[0];
	lines[n].y = (float)y[0];

  	CGContextAddLines( xd->context, &lines[0], n+1 );
 	Quartz_SetFill( fill, gamma, dd);
	CGContextFillPath( xd->context );

	CGContextAddLines( xd->context, &lines[0], n+1 );
    Quartz_SetStroke( col, gamma,  dd);
    CGContextStrokePath( xd->context);

    HaveFlush( xd->context, xd->Autorefresh );

    CGContextRestoreGState( xd->context );

}

static Rboolean Quartz_Locator(double *x, double *y, NewDevDesc *dd)
{
 return(1);
}

static void 	Quartz_Mode(int mode, NewDevDesc *dd)
{
 return;
}

static void 	Quartz_Hold(NewDevDesc *dd)
{
 return;
}

#define FixedToFloat(a)	((float)(a) / fixed1)
#define FloatToFixed(a)	((Fixed)((float) (a) * fixed1))

static void 	Quartz_MetricInfo(int c, int font, double cex, double ps,
			     double* ascent, double* descent, double* width,
			     NewDevDesc *dd)
{
 	FMetricRec myFMetric;
 	QuartzDesc *xd = (QuartzDesc *) dd-> deviceSpecific;
    char testo[2];
 	CGrafPtr savedPort;

    testo[0] = c;
    testo[1] = '\0';

    GetPort(&savedPort);

    *width = xd->xscale * floor(cex * ps + 0.5) * Quartz_StrWidth(testo, font, cex, ps, dd);

    SetPort(GetWindowPort(xd->window));


    FontMetrics(&myFMetric);

    *ascent = xd->yscale *floor(cex * ps + 0.5) * FixedToFloat(myFMetric.ascent);
    *descent = xd->yscale*floor(cex * ps + 0.5) * FixedToFloat(myFMetric.descent);

    SetPort(savedPort);


 return;
}


#else
SEXP do_Quartz(SEXP call, SEXP op, SEXP args, SEXP env)
{
	warning("Quartz device not available on this platform\n");
    return R_NilValue;
}
#endif  /* __APPLE_CC__  && HAVE_AQUA*/

#endif /* __QUARTZ_DEVICE__ */
