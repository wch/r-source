/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file macGraphic.c
 *  Copyright (C) 1998-1999  Ross Ihaka
 *                2000-2001  Stefano M. Iacus and the R core team
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

#include <stdio.h>

#include <fp.h> /* Jago */
#include <Quickdraw.h>
#include <ToolUtils.h>
#include <Menus.h>
#include <Scrap.h>
#include "Defn.h"
#include "Graphics.h"
#include "RIntf.h"
#include <Rdevices.h>

#include <ATSUnicode.h>
#include <TextEncodingConverter.h>

/* #define MAC_TEXT */
#define SETFONT

#define MM_PER_INCH  		25.4    /* mm -> inch conversion */
#define inches      		72		/* This is the constant that I use for WinWidth */
#define kAllowHistorySize	2
#define eMemoryPro			12

static int BuildingPict = 0;

#define FixedToFloat(a)	((float)(a) / fixed1)
#define FloatToFixed(a)	((Fixed)((float) (a) * fixed1))

extern char *mac_getenv(const char *name);
extern int GetScreenRes(void);

   /* Device Driver Actions */

static void		Mac_Activate(DevDesc *);
static void		Mac_Circle(double, double, int, double, int, int, DevDesc*);
static void		Mac_Clip(double, double, double, double, DevDesc*);
static void		Mac_Close(DevDesc*);
static void		Mac_Deactivate(DevDesc *);
static void		Mac_Hold(DevDesc*);
static void		Mac_Line(double, double, double, double, int, DevDesc*);
static int		Mac_Locator(double*, double*, DevDesc*);
static void		Mac_Mode(int, DevDesc*);
static void		Mac_NewPage(DevDesc*);
static void		Mac_Polygon(int, double*, double*, int, int, int, DevDesc*);
static void		Mac_Polyline(int, double*, double*, int, DevDesc*);
static void		Mac_Rect(double, double, double, double, int, int, int, DevDesc*);
static double	Mac_StrWidth(char*, DevDesc*);
static void		Mac_Text(double, double, int, char*, double, double, DevDesc*);
static void		Mac_MetricInfo(int, double*, double*, double*, DevDesc*);
static void		Mac_Resize(DevDesc* dd);



extern CGrafPtr	printerPort;
extern Boolean	WeArePrinting, WeArePasting;
extern SInt32	systemVersion ;



   /*****************************************************************************/
   /* end of list of required device driver actions                             */
   /*****************************************************************************/

   /* Support Routines */

static void	FreeColors(void);
static double	pixelHeight(void);
static double	pixelWidth(void);
static int		SetColor(int, int, DevDesc*);
static void		SetFont(int, int, DevDesc*);
static void		SetLinetype(int, double, DevDesc*);
static int		SetBaseFont(MacDesc *xd);
OSErr 			NewRasterTextRotation(char *str, int face, int size, int color, int xx, 
					int yy, double rot,  WindowPtr window);
void			startRecord(WindowPtr window);
void			stopRecord(WindowPtr window);
void			CleanUpWindow(WindowPtr window);
extern void		doActivate(EventRecord*);
void			DrawLineType(int xx1, int yy1, int xx2, int yy2, DevDesc *dd);

   /*****************************************************************************/
   /* Global or external variables                                              */
   /*****************************************************************************/

SInt16				gExpose = false;
PicHandle			gPicHandle;
Graphic_Ref			gGReference[MAX_NUM_G_WIN + 1];
Boolean				gWrite = false;
PicHandle			TempPicture;
Boolean				defaultPort = true;
static GrafPtr		storeport;
Str255				PostFont;
Str255				UserFont;
Str255				MacSymbolFont;
extern WindowPtr	Console_Window;
extern WindowPtr	Working_Window;
extern SInt16		Current_Window;
extern int			gScreenRes;





   /*****************************************************************************/
   /* Mac_Resize : You will set the contants of the corresponding MAC devices   */
   /*              (Window) in here, and then then let the Event loop to detect */
   /*              the change  (Indirect method)                                */
   /*****************************************************************************/

static void Mac_Resize(DevDesc* dd)
{
    MacDesc	*xd = (MacDesc *)dd->deviceSpecific;
    SInt16 WinIndex;
    Rect offRect,portRect;

    if(WeArePrinting || WeArePasting){
    	GetPortBounds ( gGReference[isGraphicWindow(xd->window)].activePort, & portRect ) ;
    	SetRect(&offRect, 0, 0, portRect.right, portRect.bottom);
    }
    else{
    	GetWindowPortBounds ( xd->window, & portRect ) ;
    	SetRect(&offRect, 0, 0, portRect.right, portRect.bottom);
    }
 
    if (xd->resize) {
 		dd->dp.left = dd->gp.left = 0.0;
		dd->dp.right = dd->gp.right = portRect.right;
		dd->dp.bottom = dd->gp.bottom = portRect.bottom;
		dd->dp.top = dd->gp.top = 0.0;
		xd->resize = 0 ;
    }
   
}




   /*****************************************************************************/
   /* Mac_Open : Open the Window, setup the the MAC devices record              */
   /*****************************************************************************/

static int Mac_Open(DevDesc *dd, MacDesc *xd, char *dsp,
		    double wid, double hgt)
{
    SInt16 WinIndex;
    if (!SetBaseFont(xd)) {
	Rprintf("can't find Macintosh font\n");
	return 0;
    }
    
    gScreenRes = GetScreenRes();
    
    xd->windowWidth = wid;
    xd->windowHeight = hgt;
    dd->dp.bg = R_RGB(255, 255, 255);
    dd->dp.fg = R_RGB(0, 0, 0);
    /* Create a new window with the specified size */
    CreateGraphicWindow(gScreenRes * wid, gScreenRes * hgt);
    xd->window = Working_Window;

    SetPortWindowPort(xd->window);

    WinIndex = isGraphicWindow(Working_Window);
    gGReference[WinIndex].devdesc = (Ptr)dd;
    gGReference[WinIndex].colorPort = nil;
    gGReference[WinIndex].colorDevice = nil;
    gGReference[WinIndex].printPort = nil;
    gGReference[WinIndex].activePort = nil;
    gGReference[WinIndex].MenuIndex = 0;
    xd->col[1] = xd->col[0] = NA_INTEGER;
    dd->dp.col = R_RGB(0, 0, 0);
    xd->resize = false;
    xd->lineType = 0;
    return 1;
}

void Mac_Dev_Kill(WindowPtr window)
{
    SInt16 WinIndex;
    WinIndex = isGraphicWindow(window);
    
    if ((WinIndex) && (gGReference[WinIndex].devdesc != nil)){
		if ((gGReference[WinIndex].colorDevice != nil)
	    	&& (gGReference[WinIndex].colorPort != nil)) {
	
		gGReference[WinIndex].colorPort = nil;
	    gGReference[WinIndex].colorDevice = nil;
	    gGReference[WinIndex].savedPort = nil;
	    gGReference[WinIndex].savedDevice = nil;
	    gGReference[WinIndex].printPort = nil;
	    gGReference[WinIndex].activePort = nil;
	    
		}
	KillDevice((DevDesc *)gGReference[WinIndex].devdesc);
    }
}

   /**********************************************************************/
   /* device_StrWidth should return the width of the given               */
   /* string in DEVICE units (GStrWidth is responsible for               */
   /* converting from DEVICE to whatever units the user                  */
   /* asked for                                                          */
   /**********************************************************************/

/* It will return the StrWidth of the current font in the current GrafPort */

static double Mac_StrWidth(char *str, DevDesc *dd)
{
    CGrafPtr savedPort,port;
    int width;
    int Stringlen = strlen(str);
    MacDesc *xd = (MacDesc*) dd->deviceSpecific;
    int size = dd->gp.cex * dd->gp.ps + 0.5;

    GetPort(&savedPort);
  
    if(WeArePrinting || WeArePasting){
	 port = gGReference[isGraphicWindow(xd->window)].activePort;
    }
    else
     port = GetWindowPort(xd->window);
 
    SetPortWindowPort(port);
    SetFont(dd->gp.font, size, dd);
    width = TextWidth(str, 0, Stringlen);

    SetPort(savedPort);
    return width;
}


   /**********************************************************************/
   /* device_MetricInfo should return height, depth, and                 */
   /* width information for the given character in DEVICE                */
   /* units (GMetricInfo does the necessary conversions)                 */
   /* This is used for formatting mathematical expressions               */
   /*                                                                    */
   /* Mac port: Jago Dec 2000 (Stefano M. Iacus). This is still not      */
   /* a good port and not definitive. Metric info is availble on Mac     */
   /* but actually, this routine takes metric info on the current        */
   /* selected graphic port                                              */
   /**********************************************************************/

static void Mac_MetricInfo(int c, double* ascent, double* descent,
            double* width, DevDesc *dd)
{
    FMetricRec myFMetric;
    MacDesc *xd = (MacDesc *) dd-> deviceSpecific;
    char testo[2];
 
   // if(c==0) return;
    
    testo[0] = c;
    testo[1] = '\0';
    
    FontMetrics(&myFMetric);

    *ascent = FixedToFloat(myFMetric.ascent);
    *descent = FixedToFloat(myFMetric.descent);
    *width = (double)CharWidth(c);//, 0,1) ;
}



   /**********************************************************************/
   /* device_Clip is given the left, right, bottom, and                  */
   /* top of a rectangle (in DEVICE coordinates).  it                    */
   /* should have the side-effect that subsequent output                 */
   /* is clipped to the given rectangle                                  */
   /**********************************************************************/

static void Mac_Clip(double x0, double x1, double y0, double y1, DevDesc *dd)
{

}



static void Mac_NewPage(DevDesc *dd)
{
    MacDesc *xd = (MacDesc *)dd->deviceSpecific;
    Rect 	portRect;		/* Window bounds */
    CGrafPtr savedPort,port;		/* Pointer to save current graphic port */

    GetPort(&savedPort);
    
    
    if(WeArePrinting || WeArePasting)
	 port = gGReference[isGraphicWindow(xd->window)].activePort;
    else
	 port = GetWindowPort(xd->window);
    
    SetPort(port);
  
    GetPortBounds ( port, & portRect ) ;
 
    SetColor(dd->dp.bg, 0, dd);
    PaintRect(&portRect);


    SetColor(dd->dp.fg, 1, dd);

    SetPort(savedPort);
    
}



   /**********************************************************************/
   /* device_Close is called when the device is killed                   */
   /* this function is responsible for destroying any                    */
   /* device-specific resources that were created in                     */
   /* device_Open and for FREEing the device-specific                    */
   /* parameters structure                                               */
   /**********************************************************************/
/* We need a way to tell the internal R that the window had been closed  */
/* by the GUI interface's control                                        */

static void Mac_Close(DevDesc *dd)
{
    MacDesc *xd = (MacDesc *) dd->deviceSpecific;
    SInt16 WinIndex;
    Str255 Cur_Title;

    if( (WinIndex = isGraphicWindow(xd->window)) == 0)
     return;
     
    RemWinMenuItem();
    changeGWinPtr(xd->window, Cur_Title);
    DestroyWindow(xd->window);
    free(xd);
}

   /**********************************************************************/
   /* device_Activate is called when a device becomes the                */
   /* active device.  in this case it is used to change the              */
   /* title of a window to indicate the active status of                 */
   /* the device to the user.  not all device types will                 */
   /* do anything                                                        */
   /* Routine also adjust menus items in the Windows menu of R           */
   /* Updated, Stefano M.Iacus Jan, 2001                                 */
   /**********************************************************************/

static void Mac_Activate(DevDesc *dd)
{
    unsigned char titledString[256], curString[256];
    MenuHandle windowsMenu,my_menu;
    int i;
    Boolean EqString = FALSE;
    MacDesc *xd = (MacDesc *) dd->deviceSpecific;
    SInt16 WinIndex = isGraphicWindow(xd->window);
    int devNum = deviceNumber(dd);

    sprintf((char*)&titledString[1], "Graphics Window %d [Inactive]",
	    devNum +1);
    titledString[0] = strlen((char*)&titledString[1]);

    windowsMenu = GetMenuHandle(kMenuWindows);
    for(i = 1; i <= CountMenuItems(windowsMenu); i++){
	GetMenuItemText(windowsMenu, i , curString);
	EqString = EqualNumString(titledString, curString, titledString[0]);
	if (EqString) {
	    DeleteMenuItem(windowsMenu, i);
	    sprintf((char*)&titledString[1], "Graphics Window %d [Active]",
		    devNum + 1);
	    titledString[0] = strlen((char*)&titledString[1]);
	    InsertMenuItem(windowsMenu, titledString,i);
	    break;
	}
    }

    if(EqString==0)
    {
	sprintf((char*)&titledString[1], "Graphics Window %d [Active]",
		devNum + 1);
	titledString[0] = strlen((char*)&titledString[1]);
	AppendMenu(windowsMenu, titledString);
   }
   SetWTitle(xd->window, titledString) ;
}

   /**********************************************************************/
   /* device_Deactivate is called when a device becomes                  */
   /* inactive.  in this case it is used to change the                   */
   /* title of a window to indicate the inactive status of               */
   /* the device to the user.  not all device types will                 */
   /* do anything                                                        */
   /* Routine also adjust menus items in the Windows menu of R           */
   /* Updated, Stefano M.Iacus Jan, 2001                                 */
   /**********************************************************************/

static void Mac_Deactivate(DevDesc *dd)
{
    unsigned char titledString[256],curString[256];
    int i;
    Boolean EqString;
    MenuHandle windowsMenu;
    MacDesc *xd = (MacDesc*)dd->deviceSpecific;
    int devNum = deviceNumber(dd);
    sprintf((char*)&titledString[1], "Graphics Window %d [Active]",
	    devNum + 1);
    titledString[0] = strlen((char*)&titledString[1]);

    windowsMenu = GetMenuHandle(kMenuWindows);

    for(i = 1; i <= CountMenuItems(windowsMenu); i++){
	GetMenuItemText(windowsMenu, i , curString);
	EqString = EqualNumString(titledString, curString, titledString[0]);
	if (EqString) {
	    DeleteMenuItem(windowsMenu, i);
	    sprintf((char*)&titledString[1], "Graphics Window %d [Inactive]",
		    devNum + 1);
	    titledString[0] = strlen((char*)&titledString[1]);
	    InsertMenuItem(windowsMenu, titledString,i);
	    break;
	}
    }

    sprintf((char*)&titledString[1], "Graphics Window %d [Inactive]",
	    devNum + 1);
    titledString[0] = strlen((char*)&titledString[1]);
    SetWTitle (xd->window, titledString) ;
}

   /**********************************************************************/
   /* device_Rect should have the side-effect that a                     */
   /* rectangle is drawn with the given locations for its                */
   /* opposite corners.  the border of the rectangle                     */
   /* should be in the given "fg" colour and the rectangle               */
   /* should be filled with the given "bg" colour                        */
   /* if "fg" is NA_INTEGER then no border should be drawn               */
   /* if "bg" is NA_INTEGER then the rectangle should not                */
   /* be filled                                                          */
   /* the locations are in an arbitrary coordinate system                */
   /* and this function is responsible for converting the                */
   /* locations to DEVICE coordinates using GConvert                     */
  /**********************************************************************/

static void Mac_Rect(double x0, double y0, double x1, double y1,
		     int coords, int bg, int fg, DevDesc *dd)
{
    int tmp;
    Rect myRect;
    MacDesc *xd = (MacDesc*)dd->deviceSpecific;
    SInt16 WinIndex;
    CGrafPtr savedPort,port;

    GetPort(&savedPort);

    GConvert(&x0, &y0, coords, DEVICE, dd);
    GConvert(&x1, &y1, coords, DEVICE, dd);
    /* FIXME -- redundancy here */
    /* put the values directly in myRect */
    if (x0 > x1) {
	tmp = x0;
	x0 = x1;
	x1 = tmp;
    }
    if (y0 > y1){
	tmp = y0;
	y0 = y1;
	y1 = tmp;
    }
    myRect.left = (short)x0;
    myRect.top = (short)y0;
    myRect.right = (short)x1 + 1;
    myRect.bottom = (short)y1 + 1;

   if(WeArePrinting || WeArePasting)
	 port = gGReference[isGraphicWindow(xd->window)].activePort;
    else
	 port = GetWindowPort(xd->window);

    SetPort(port);

    if (bg != NA_INTEGER){
	SetColor(bg, 0, dd);
	PaintRect(&myRect);
    }
    if (fg != NA_INTEGER){
	SetColor(fg, 0, dd);
	FrameRect(&myRect);
    }
    /* (2) Draw the rectangle into the backing pixmap */
    if (bg != NA_INTEGER){
	SetColor(bg, 1, dd);
	PaintRect(&myRect);
    }
    if (fg != NA_INTEGER){
	SetColor(fg, 1, dd);
	FrameRect(&myRect);
    }
    SetPort(savedPort);
}

   /**********************************************************************/
   /* device_Circle should have the side-effect that a                   */
   /* circle is drawn, centred at the given location, with               */
   /* the given radius.  the border of the circle should be              */
   /* drawn in the given "col", and the circle should be                 */
   /* filled with the given "border" colour.                             */
   /* if "col" is NA_INTEGER then no border should be drawn              */
   /* if "border" is NA_INTEGER then the circle should not               */
   /* be filled                                                          */
   /* the location is in arbitrary coordinates and the                   */
   /* function is responsible for converting this to                     */
   /* DEVICE coordinates.  the radius is given in DEVICE                 */
   /* coordinates                                                        */
   /**********************************************************************/

static void Mac_Circle(double x, double y, int coords,
		       double r, int col, int border, DevDesc *dd)
{
    int ir, ix, iy;
    Rect myRect;
    MacDesc *xd = (MacDesc *) dd->deviceSpecific;
    SInt16 WinIndex;
    CGrafPtr savedPort,port;
    GDHandle savedDevice;

    GetPort(&savedPort);
 
    ir = floor(r + 0.5);
    if (ir < 2) ir = 2;
    GConvert(&x, &y, coords, DEVICE, dd);
    ix = (int)x;
    iy = (int)y;
    myRect.top = iy - ir;
    myRect.left = ix - ir;
    myRect.right = ix + ir;
    myRect.bottom = iy + ir;



    if(WeArePrinting || WeArePasting)
	 port = gGReference[isGraphicWindow(xd->window)].activePort;
    else
	 port = GetWindowPort(xd->window);
    
    SetPort(port);
  
    if (col != NA_INTEGER){
	SetColor(col, 0, dd);
	PaintArc(&myRect, 0, 360);
    }
    if (border != NA_INTEGER){
	SetColor(border, 0, dd);
	FrameArc(&myRect, 0, 360);
    }

    /* Update the backing pixmap */
    /* Only do this if it makes sense */

    if (col != NA_INTEGER){
	SetColor(col, 1, dd);
	PaintArc(&myRect, 0, 360);
    }
    if (border != NA_INTEGER){
	SetColor(border, 1, dd);
	FrameArc(&myRect, 0, 360);
    }
    SetPort(savedPort);
}

   /**********************************************************************/
   /* device_Line should have the side-effect that a single              */
   /* line is drawn (from x1,y1 to x2,y2)                                */
   /* x1, y1, x2, and y2 are in arbitrary coordinates and                */
   /* the function is responsible for converting them to                 */
   /* DEVICE coordinates using GConvert                                  */
   /**********************************************************************/

static void Mac_Line(double x1, double y1, double x2, double y2,
		     int coords, DevDesc *dd)
{
    int		xx1, yy1, xx2, yy2;
    short	dx, dy;
    short	absdx, absdy;
    short	startx, starty;
    short	numPixelsToDraw;
    double	ratio;
    short	dashLength;
    short	fullDashLength;
    short	delta;
    short	xoffset, yoffset;
    short	xoffset2, yoffset2;
    double	newx, newy;
    double	newx2, newy2;
    Boolean notFirst = false;
    MacDesc *xd = (MacDesc*)dd->deviceSpecific;
    SInt16 WinIndex;
    CGrafPtr	savedPort,port;

    GetPort(&savedPort);

    GConvert(&x1, &y1, coords, DEVICE, dd);
    GConvert(&x2, &y2, coords, DEVICE, dd);
    xx1 = (int)x1;
    yy1 = (int)y1;
    xx2 = (int)x2;
    yy2 = (int)y2;
    
    WinIndex = isGraphicWindow(xd->window);
  
  
   if(WeArePrinting || WeArePasting)
	 port = gGReference[isGraphicWindow(xd->window)].activePort;
    else
	 port = GetWindowPort(xd->window);
    
    SetPort(port);
   
    SetColor(dd->gp.col, 0, dd);
    SetLinetype(dd->gp.lty, 1, dd);
    /* For some reason SetLineType does not work ! */
    /* so we have fixed dd->gp.lwd to 1            */
    /* It was:                                     */
    /*  SetLinetype(dd->gp.lty, dd->gp.lwd, dd);   */
       
    if (xd->lineType == 0) {
	MoveTo(xx1, yy1);
	LineTo(xx2, yy2);
    }
    else {
	DrawLineType(xx1, yy1, xx2, yy2, dd);
    }

   	SetPort(port);
 
    SetColor(dd->gp.col, 1, dd);
    if(xd->lineType == 0){
	MoveTo(x1, y1);
	LineTo(x2, y2);
    }
    else {
	DrawLineType(xx1, yy1, xx2, yy2, dd);
    }

    SetPort(savedPort);  
}

   /**********************************************************************/
   /* device_Polyline should have the side-effect that a                 */
   /* series of line segments are drawn using the given x                */
   /* and y values                                                       */
   /* the x and y values are in arbitrary coordinates and                */
   /* the function is responsible for converting them to                 */
   /* DEVICE coordinates using GConvert                                  */
   /**********************************************************************/

static void Mac_Polyline(int n, double *x, double *y, int coords, DevDesc *dd)
{
    int i, startXX, startYY;
    double startX, startY;
    MacDesc *xd = (MacDesc*)dd->deviceSpecific;
    SInt16 WinIndex;
    CGrafPtr savedPort,port;		/* Pointer to the saved graphics environment */
    RGBColor aColor;

    GetPort(&savedPort);
      
    if(WeArePrinting || WeArePasting)
	 port = gGReference[isGraphicWindow(xd->window)].activePort;
	 port = GetWindowPort(xd->window);

     SetPort(port);
 
    for (i = 1; i < n; i++) 
	 Mac_Line(x[i - 1], y[i - 1], x[i], y[i], coords, dd);

    SetPort(savedPort);
}

   /**********************************************************************/
   /* device_Polygon should have the side-effect that a                  */
   /* polygon is drawn using the given x and y values                    */
   /* the polygon border should be drawn in the "fg"                     */
   /* colour and filled with the "bg" colour                             */
   /* if "fg" is NA_INTEGER don't draw the border                        */
   /* if "bg" is NA_INTEGER don't fill the polygon                       */
   /* the x and y values are in arbitrary coordinates and                */
   /* the function is responsible for converting them to                 */
   /* DEVICE coordinates using GConvert                                  */
   /**********************************************************************/

static void Mac_Polygon(int n, double *x, double *y, int coords,
			int bg, int fg, DevDesc *dd)
{
    int i;
    double startX, startY, startXX, startYY;
    PolyHandle  myPolygon;
    SInt16 WinIndex;
    MacDesc *xd = (MacDesc*)dd->deviceSpecific;
    CGrafPtr savedPort,port;
    GDHandle savedDevice;

    GetPort(&savedPort);
    
    if(WeArePrinting || WeArePasting)
	 port = gGReference[isGraphicWindow(xd->window)].activePort;
    else
	 port = GetWindowPort(xd->window);

    SetPort(port);

    myPolygon = OpenPoly();
    startX = x[0];
    startY = y[0];
    GConvert(&startX, &startY, coords, DEVICE, dd);
    startXX = (int)startX;
    startYY = (int)startY;
    MoveTo(startXX, startYY);
    for (i = 0; i < n; i++) {
	startX = x[i];
	startY = y[i];
	GConvert(&startX, &startY, coords, DEVICE, dd);
	startXX = (int)startX;
	startYY = (int)startY;
	LineTo(startXX, startYY);
    }
    startX = x[0];
    startY = y[0];
    GConvert(&startX, &startY, coords, DEVICE, dd);
    startXX = (int)startX;
    startYY = (int)startY;
    LineTo(startXX, startYY);
    ClosePoly();

    if (bg != NA_INTEGER){
	SetColor(bg, 0, dd);
	PaintPoly(myPolygon);
    }
    if (fg != NA_INTEGER){
	SetColor(fg, 0, dd);
	FramePoly(myPolygon);
    }
    if (bg != NA_INTEGER){
	SetColor(bg, 1, dd);
	PaintPoly(myPolygon);
    }
    if (fg != NA_INTEGER){
	SetColor(fg, 1, dd);
	FramePoly(myPolygon);
    }
    KillPoly(myPolygon);
    SetPort(savedPort);
}


   /***********************************************************************/
   /* device_Text should have the side-effect that the                    */
   /* given text is drawn at the given location                           */
   /* the text should be justified according to "xc" and                  */
   /* "yc" (0 = left, 0.5 = centre, 1 = right)                            */
   /* and rotated according to rot (degrees)                              */
   /* the location is in an arbitrary coordinate system                   */
   /* and this function is responsible for converting the                 */
   /* location to DEVICE coordinates using GConvert                       */
   /***********************************************************************/

double deg2rad = 0.01745329251994329576;

/* 	Updated version of previous Mac_Text. A bug of double writing of text 
	has been removed. Text is rotated as needed taking into accounts the 
	different rotation rules used by CarbonLib (clockwise/counter-clockwise) 
	on different versions of OSes.
   	Jago, April 2001, Stefano M. Iacus
*/

static void Mac_Text(double x, double y, int coords,
		     char *str, double rot, double hadj, DevDesc *dd)
{
    int Stringlen;
    int xx, yy, x1, y1;
    int size;
    int realFace, face;
    MacDesc *xd = (MacDesc *) dd-> deviceSpecific;
    RGBColor aColor;
    Rect zeroRect,rgnRect;
    SInt16  WinIndex;
    RgnHandle oldClip;
    CGrafPtr savedPort;
    GDHandle savedDevice;
    double xc, yc;
    FMFontFamily postFontId;


    xc=0;
    yc=0;

    GetPort( &savedPort );
  
    if(WeArePrinting || WeArePasting){
       SetPort(gGReference[isGraphicWindow(xd->window)].activePort);
       
       if(WeArePrinting && systemVersion >= 0x10008000)
        rot = - rot; /* On System X for some reason rotation has reverse clockwising */
       }
    else
     SetPortWindowPort(xd->window);

    size = dd->gp.cex * dd->gp.ps + 0.5;
    SetFont(dd->gp.font, size, dd);
    Stringlen = strlen(str);
    GConvert(&x, &y, coords, DEVICE, dd);
    if (xc != 0.0 || yc != 0.0){
	x1 = TextWidth(str, 0, Stringlen);
	y1 = GConvertYUnits(1, CHARS, DEVICE, dd);
	x += -xc * x1 * cos(toRadian(rot)) + yc * y1 * sin(toRadian(rot));
	y -= -xc * x1 * sin(toRadian(rot)) - yc * y1 * cos(toRadian(rot));
    }
    xx = (int)x;
    yy = (int)y;

    MoveTo(xx, yy);
 
    SetColor(dd->gp.col, 0, dd);
   
    size = dd->gp.cex * dd->gp.ps + 0.5;
    SetFont(dd->gp.font, size, dd);

    face = dd->gp.font;           /* Typeface */

    NewRasterTextRotation(str, face, size, dd->gp.col, xx, yy, rot, xd->window);
    
    SetColor(dd->gp.col, 1, dd);

    SetPort(savedPort);

}

   /**********************************************************************/
   /* device_Locator should return the location of the next              */
   /* mouse click (in DEVICE coordinates; GLocator is                    */
   /* responsible for any conversions)                                   */
   /* not all devices will do anythin (e.g., postscript)                 */
   /**********************************************************************/

static int Mac_Locator(double *x, double *y, DevDesc *dd)
{
    EventRecord event;
    SInt16 key;
    Boolean gotEvent,SIOUXDidEvent;
    Boolean mouseClick = false;
    Point myPoint;
    WindowPtr window;
    SInt16 partCode;
    GrafPtr savePort;
    Cursor		arrow ;
    MacDesc *xd = (MacDesc*)dd->deviceSpecific;

    GetPort(&savePort);

#if TARGET_API_MAC_CARBON
    SetPortWindowPort(xd->window);
    SetCursor( GetQDGlobalsArrow ( & arrow ) ) ;
#else
    SetPort(xd->window);
    SetCursor(&qd.arrow);
#endif

    while(!mouseClick) {
	gotEvent = WaitNextEvent( everyEvent, &event, 0, nil);

	if (event.what == mouseDown) {
	    partCode = FindWindow(event.where, &window);
	    if ((window == (xd->window)) && (partCode == inContent)) {
		myPoint = event.where;
		GlobalToLocal(&myPoint);
		*x = (double)(myPoint.h);
		*y = (double)(myPoint.v);
		mouseClick = true;
	    }
	    if (partCode == inContent) {
		if (DoContent(event.where, &event, window)) {
		    SelectWindow (window);
		}
	    }
	    if (partCode == inDrag)
		DoDrag (event.where, window);
	}
	if (event.what == updateEvt){
	    DoWindowEvent( &event );
	}
	if (event.what == activateEvt) {
	    window = (WindowPtr)(&event)->message;
	    if (isGraphicWindow(window)) {
		doActivate(&event);
	    }
	    DoWindowEvent(&event);
	}
	if (event.what == keyDown) {
	    key = (event.message & charCodeMask);
	    if (key == kReturn){
		SetPort(savePort);
		return 0;
	    }
	}
    }

    SetPort(savePort);
    return 1;
}

   /**********************************************************************/
   /* device_Mode is called whenever the graphics engine                 */
   /* starts drawing (mode=1) or stops drawing (mode=1)                  */
   /* the device is not required to do anything                          */
   /**********************************************************************/

/* This routine is called before and after each part of the plot being drawn */
static void Mac_Mode(int mode, DevDesc *dd)
{
    MacDesc *xd = (MacDesc*)dd->deviceSpecific;
    if (mode) {
	GetPort(&storeport);
#if TARGET_API_MAC_CARBON
    SetPortWindowPort(xd->window);
#else
    SetPort(xd->window);
#endif
    }
    else {
	SetPort(storeport);
    }
}

   /**********************************************************************/
   /* i don't know what this is for and i can't find it                  */
   /* being used anywhere, but i'm loath to kill it in                   */
   /* case i'm missing something important                               */
   /**********************************************************************/

/* Hold the Picture Onscreen - not needed for Mac                        */
static void Mac_Hold(DevDesc *dd)
{
}

   /**********************************************************************/
   /* the device-driver entry point is given a device                    */
   /* description structure that it must set up.  this                   */
   /* involves several important jobs ...                                */
   /* (1) it must ALLOCATE a new device-specific parameters              */
   /* structure and FREE that structure if anything goes                 */
   /* wrong (i.e., it won't report a successful setup to                 */
   /* the graphics engine (the graphics engine is NOT                    */
   /* responsible for allocating or freeing device-specific              */
   /* resources or parameters)                                           */
   /* (2) it must initialise the device-specific resources               */
   /* and parameters (mostly done by calling device_Open)                */
   /* (3) it must initialise the generic graphical                       */
   /* parameters that are not initialised by GInit (because              */
   /* only the device knows what values they should have)                */
   /* see Graphics.h for the official list of these                      */
   /* (4) it may reset generic graphics parameters that                  */
   /* have already been initialised by GInit (although you               */
   /* should know what you are doing if you do this)                     */
   /* (5) it must attach the device-specific parameters                  */
   /* structure to the device description structure                      */
   /* e.g., dd->deviceSpecfic = (void *) xd;                             */
   /* (6) it must FREE the overall device description if                 */
   /* it wants to bail out to the top-level                              */
   /* the graphics engine is responsible for allocating                  */
   /* the device description and freeing it in most cases                */
   /* but if the device driver freaks out it needs to do                 */
   /* the clean-up itself                                                */
   /**********************************************************************/


   /*  Mac Device Driver Arguments                                       */

   /*  cpars[0] = display name                                           */
   /*  cpars[1] = paper type (a4, letter, none)                          */

   /*  npars[0] = width (inches)                                         */
   /*  npars[1] = height (inches)                                        */
   /*  npars[2] = base pointsize                                         */
   /*  npars[3] = paper orientation                                      */
   /*        1 - portrait                                                */
   /*        2 - landscape                                               */
   /*        3 - flexible                                                */

Rboolean MacDeviceDriver(DevDesc *dd, char *display,
			 double width, double height, double pointsize)
{
    MacDesc *xd;
    int ps;
    
    if (!(xd = (MacDesc *)malloc(sizeof(MacDesc))))
	return 0;

    Mac_Open(dd, xd, display, width, height);

    gScreenRes = GetScreenRes();
    
    ps = pointsize;
    if (ps < 6 || ps > 24) ps = 10;
    ps = 2 * (ps / 2);
    dd->dp.ps = ps;
    dd->dp.open       = Mac_Open;
    dd->dp.close      = Mac_Close;
    dd->dp.activate   = Mac_Activate;
    dd->dp.deactivate = Mac_Deactivate;
    dd->dp.resize     = Mac_Resize;
    dd->dp.newPage    = Mac_NewPage;
    dd->dp.clip       = Mac_Clip;
    dd->dp.strWidth   = Mac_StrWidth;
    dd->dp.text       = Mac_Text;
    dd->dp.rect       = Mac_Rect;
    dd->dp.circle     = Mac_Circle;
    dd->dp.line       = Mac_Line;
    dd->dp.polyline   = Mac_Polyline;
    dd->dp.polygon    = Mac_Polygon;
    dd->dp.locator    = Mac_Locator;
    dd->dp.mode       = Mac_Mode;
    dd->dp.hold       = Mac_Hold;

    dd->dp.metricInfo = Mac_MetricInfo;

    dd->dp.left        = 0;
    dd->dp.right       = gScreenRes * xd->windowWidth;
    dd->dp.bottom      = gScreenRes * xd->windowHeight;
    dd->dp.top         = 0;

    dd->dp.xCharOffset = 0.4900;
    dd->dp.yCharOffset = 0.3333;
    dd->dp.yLineBias = 0.1;

    dd->dp.cra[0] = ps / 2;
    dd->dp.cra[1] = ps;

    dd->dp.ipr[0] = 1.0 / gScreenRes;
    dd->dp.ipr[1] = 1.0 / gScreenRes;

    dd->dp.canResizePlot = 1;
    dd->dp.canChangeFont = 0;
    dd->dp.canRotateText = 1;
    dd->dp.canResizeText = 1;
   	dd->dp.canClip       = FALSE;

    /* It is used to set the font that you will be used on the postscript and
       drawing.
    */
        
    /* There is the place for you to set the default value of the MAC Devices */
    xd->cex = 1.0;
    xd->resize = 0;
    xd->Text_Font = 4; /* initial is monaco */
    xd->fontface = 0;  /* initial is plain text */
    xd->fontsize = 12; /* initial is 12 size */
    dd->deviceSpecific = (void *) xd;
    dd->displayListOn = 1;
    SelectWindow (Console_Window);
    return 1;
}


/* I can't find where this method is being used. I assume                   */
/* the method is not sueful in Macintosh()                                  */
int MacConnectionNumber()
{
}



/* New_G_History
 */
/* I write this routine because I guess I need to deal with resize event. */
/* Now, I recognize that this will be handled by internal R. If after some */
/* Testing and this didn't happen any error. This can be deleted          */
/* Also, You can update the Struct of Graphic_Ref                         */

void New_G_History(SInt16 WinIndex)
{
/*
    gGReference[WinIndex].WPicHandle = 0;
    gGReference[WinIndex].HavePic = false;
    If the journal handle is nil then no journalling will be performed
    and printing and resizing will still work - just not as well.
*/
}


/* Kill_G_History
 */
/* I write this routine because I guess I need to deal with resize event. */
/* Now, I recognize that this will be handled by internal R. If after some */
/* Testing and this didn't happen any error. This can be deleted          */

void Kill_G_History(SInt16 WinIndex)
{
}

 
/* GraResize :
   Resize event will only happen after you release the mouse.
   Thus, you can simply re-calculate the window size and redraw
   the content easy!!!
   The most important thing is to understand what para R ro to here,
   and how to convert it into local coordinate.
 */

void GraResize(WindowPtr window)
{
    SInt16 WinIndex;
    Rect    portRect;
    GrafPtr	savePort;
    DevDesc *dd;
    MacDesc *xd; /* = (MacDesc *) dd-> deviceSpecific; */

    WinIndex = isGraphicWindow(window);

    if (WinIndex && (gGReference[WinIndex].devdesc != nil)) {

	GetPort(&savePort);

#if TARGET_API_MAC_CARBON
    SetPortWindowPort(window);
    GetWindowPortBounds(window,&portRect);
#else
	SetPort(window);
#endif

	SetPort(savePort);
	dd = (DevDesc*)gGReference[WinIndex].devdesc;
	xd = (MacDesc*)dd->deviceSpecific;
	xd->resize = true;
	gExpose = WinIndex;
    }
}


/* toRadian : Transfer angle in degree to Radian
 */
double toRadian(int angle)
{
    int InAngle;
    InAngle = angle;
    while (InAngle >360){
	InAngle = InAngle - 360;
    }
    return ((double)InAngle/180) * 3.141592654;
}



/* startRecord :
   This routine will be called to start the record of the picture on the
   window specified in the Mac_Mode routine.
 */
void startRecord(WindowPtr window)
{
}


/* stopRecord :
   This routine will be called to end the record of the picture on the
   window specified in the Mac_Mode routine.
 */
void stopRecord(WindowPtr window)
{
}



/* CleanUpWindow :
   Try to clean up the drawing window
 */
void CleanUpWindow(WindowPtr window)
{
}

/* GraphicCopy makes a copy of the current device window into
   the clipboard. This is a very quick and clean routine
   completely rewritten wrt the original one.
   Jago, April 2001, Stefano M. Iacus
*/   

void GraphicCopy(WindowPtr window)
{
    Size dataLength;
    SInt32 errorCode;
    SInt16 WinIndex;
    DevDesc *dd;
    PicHandle WPicHandle=NULL;
    CGrafPtr savePort, tempPort;
    Rect portRect;
    ScrapRef scrap;
    
    WinIndex = isGraphicWindow(window);
    dd = (DevDesc*)gGReference[WinIndex].devdesc;
    GetPort(&savePort);
    
    GetWindowPortBounds(window,&portRect);
	tempPort = CreateNewPort();
    
    WPicHandle = OpenPicture(&portRect);
    ClipRect(&portRect);
 	 
 	gGReference[WinIndex].activePort = tempPort;
 	WeArePasting = true;
 	playDisplayList(dd);
 	WeArePasting = false;
 		
	ClosePicture();

    DisposePort(tempPort);
    
    if (ClearCurrentScrap() == noErr) {
	dataLength = GetHandleSize((Handle) WPicHandle);
	HLock((Handle)WPicHandle);
    errorCode = GetCurrentScrap(&scrap);
    errorCode = PutScrapFlavor (scrap, 'PICT', 0, 
    GetHandleSize((Handle) WPicHandle), *WPicHandle);
	HUnlock((Handle)WPicHandle);
    }
    
    KillPicture(WPicHandle);

    SetPort(savePort);
}


/* If you call this routine, the default value about TextSize and
   TextFace of  the corresponding Mac Devices will be used
 */
/* ( not sure where R use this function)                                       */
static int SetBaseFont(MacDesc *xd)
{
    xd->fontface = 4;    /* if you want monaco to be the default font.   */
                         /* for system font, it is 0 instead of 4        */
    xd->fontsize = 12;
                         /* We can use a fix font in graphic window?     */
    xd->usefixed = 1;
    return 1;
}

/* Pixel Dimensions (Inches)
   Not useful in Mac, cause there have no way to queue the system.
   We using a contant inches to control the Window Size
 */
static double pixelWidth(void)
{
}

/* Not useful in Mac, cause there have no way to queue the system.
   We using a contant inches to control the Window Size
 */
static double pixelHeight(void)
{
}


/* SetFont: This routine is used to set the Font face and size.
   However, even you using the System font, there will still exist
   some chance that you saw nothing display on the Screen.
   In this moment, the only thing we can do is reinitialize the GrafPort.
 */

static void SetFont(int face, int size, DevDesc *dd)
{
    int realFace;
#if TARGET_API_MAC_CARBON
    FMFontFamily postFontId;
#else
    short postFontId;
#endif
    GrafPtr savePort;
    /* you can not chnage the picture which is drawed directly, you can only */
    /* see the effect for the next call */
    MacDesc *xd = (MacDesc *) dd-> deviceSpecific;

    GetPort(&savePort);
    SetPortWindowPort(xd->window);
    xd -> Text_Font = 4;		/* Initial font is monaco (default) */
    xd -> fontface = face;		/* Typeface */
    if (size < 6) size = 6;
    xd -> fontsize = size;		/* Size in points */
    realFace = 0;
    if (face == 1) realFace = 0;	/* normal */
    if (face == 2) realFace = 1;	/* bold */
    if (face == 3) realFace = 2;	/* italic */
    if (face == 4) realFace = 3;	/* bold & italic */
#if TARGET_API_MAC_CARBON
    if(systemVersion > kMinSystemVersion)
    postFontId = FMGetFontFamilyFromName(PostFont);
    else
     GetFNum(PostFont, &postFontId);
#else
    GetFNum(PostFont, &postFontId);
#endif
    if (face == 5){ realFace = 0;	/* plain symbol */
#if TARGET_API_MAC_CARBON
    if(systemVersion > kMinSystemVersion)
    postFontId = FMGetFontFamilyFromName(MacSymbolFont);
    else
     GetFNum(MacSymbolFont, &postFontId);
#else
    GetFNum(MacSymbolFont, &postFontId);
#endif
    }
    TextFont(postFontId);
    TextFace(realFace);
    TextSize(size);
    SetPort(savePort);

}

static int SetColor(int color, int which, DevDesc *dd)
{
    MacDesc *xd = (MacDesc*)dd->deviceSpecific;
    
    if (color != xd->col[which]) { 
	xd->rgb[which].red = R_RED(color) * 255;
	xd->rgb[which].green = R_GREEN(color) * 255;
	xd->rgb[which].blue = R_BLUE(color) * 255;
    RGBForeColor(&(xd->rgb[which]));
	return 1;
    }
    return 0;
}

/* I am not sure either we need to use it in Mac */
static void FreeColors()
{
}

/*
 * Some Notes on Line Textures
 *
 * Line textures are stored as an array of 4-bit integers within
 * a single 32-bit word.  These integers contain the lengths of
 * lines to be drawn with the pen alternately down and then up.
 * The device should try to arrange that these values are measured
 * in points if possible, although pixels is ok on most displays.
 *
 * If newlty contains a line texture description it is decoded
 * as follows:
 *
 *    ndash = 0;
 *    for(i=0 ; i<8 && newlty&15 ; i++) {
 *       dashlist[ndash++] = newlty&15;
 *       newlty = newlty>>4;
 *    }
 *    dashlist[0] = length of pen-down segment
 *    dashlist[1] = length of pen-up segment
 *    etc
 *
 * An integer containing a zero terminates the pattern.  Hence
 * ndash in this code fragment gives the length of the texture
 * description.  If a description contains an odd number of
 * elements it is replicated to create a pattern with an
 * even number of elements.  (If this is a pain, do something
 * different its not crucial).
 *
 * 27/5/98 Paul - change to allow lty and lwd to interact:
 * the line texture is now scaled by the line width so that,
 * for example, a wide (lwd=2) dotted line (lty=2) has bigger
 * dots which are more widely spaced.  Previously, such a line
 * would have "dots" which were wide, but not long, nor widely
 * spaced.
 */

/* Have not implement yet, you can choose pen pattern in Mac, however, it*/
/* seem to be not the things you want in here                            */

static void SetLinetype(int newlty, double nlwd, DevDesc *dd)
{
    /* Much of this code has been lifted straight from the devX11.c source file */

    int i;
    int end;
    short numDashes;
    MacDesc *xd = (MacDesc *) dd-> deviceSpecific;
    /* If the linetype is 0 then we want to go back to standard, solid lines
       otherwise set up the appropriate variables for the Mac_LineTo routine.
       We keep track of several things so that the Mac_LineTo routine knows
       what it should do.  These include:
       sLineType: 	the current line type
       sDashList:	the dash lengths in a digested form the current line type.
       sNumDashes:	the number of segments in the current line type.
    */

    if( newlty != xd->lineType ) {
	xd->lineType = newlty;
	xd->numDashes = 0;
	xd->currentDash = 0;
	if(newlty != 0) {
	    xd->lineType = newlty;
	    for (i = 0; i < 8 && newlty & 15; i++ ) {
		xd->dashList[xd->numDashes++] = newlty & 15;
		newlty = newlty >> 4;
	    }


	    /* Deal with odd length dash patterns by doubling the length of
	       the sequence. */

	    numDashes = xd->numDashes;
	    if (numDashes & 1 == 1) {
		for(i = 0; i < numDashes; i++)
		    xd->dashList[ numDashes+i ] = xd->dashList[i];
		xd->numDashes = numDashes * 2;
	    }
	}
    }
    PenSize(nlwd,nlwd);
}

void DrawLineType(int xx1, int yy1, int xx2, int yy2, DevDesc *dd)
{
    short dx, dy;
    short absdx, absdy;
    short startx, starty;
    short numPixelsToDraw;
    double ratio;
    short dashLength;
    short fullDashLength;
    short delta;
    short xoffset, yoffset;
    short xoffset2, yoffset2;
    double newx, newy;
    double newx2, newy2;
    Boolean notFirst = false;
    MacDesc *xd = (MacDesc *) dd->deviceSpecific;

    xd->dashStart_x = xx1;
    xd->dashStart_y = yy1;
    startx = xd->dashStart_x;
    starty = xd->dashStart_y;
    /* startx = x1;
       starty = y1;
    */
    dx = xx2 - startx;
    dy = yy2 - starty;
    absdx = abs(dx);
    absdy = abs(dy);

    if( absdx > absdy) {
	ratio = (double)absdy / absdx;
	numPixelsToDraw = absdx;
    }
    else if( absdy > absdx ) {
	ratio = (double)absdx / absdy;
	numPixelsToDraw = absdy;
    }
    else {	/* Don't perform division here in case dx and dy are both 0. */
	ratio = 1;
	numPixelsToDraw = absdy;
    }

    /* MoveTo(x1, y1);*/
    /* Do the line drawing here... */
    /* this is foolish, the complier turn off the while loop at the first run. */
    /* which implies that I need to do something to make the loop execute one more time */
    /* I hope this will be figures out why later on.    */
    /* By experiment, 4 is a reasonable number to add in here. */

    while(numPixelsToDraw + 4 >= xd->dashList[xd->currentDash]
	  + xd->dashList[xd->currentDash+1]) {
	dashLength = xd->dashList[xd->currentDash];
	fullDashLength = dashLength + xd->dashList[xd->currentDash+1];
	/* Calculate the end-point for this segment */
	if( absdx > absdy ) {
	    delta = absdx - numPixelsToDraw + dashLength - 1;
	    xoffset = delta;
	    yoffset = (double)delta * ratio;
	    delta = absdx - numPixelsToDraw + fullDashLength;
	    xoffset2 = delta;
	    yoffset2 = (double)delta * ratio;
	}
	else {
	    delta = absdy - numPixelsToDraw + dashLength - 1;
	    yoffset = delta;
	    xoffset = (double)delta * ratio;
	    delta = absdy - numPixelsToDraw + fullDashLength;
	    yoffset2 = delta;
	    xoffset2 = (double)delta * ratio;
	}
	if(dx > 0) {
	    newx = startx + xoffset;
	    newx2 = startx + xoffset2;
	}
	else {
	    newx = startx - xoffset;
	    newx2 = startx - xoffset2;
	}
	if( dy > 0 ) {
	    newy = starty + yoffset;
	    newy2 = starty + yoffset2;
	}
	else {
	    newy = starty - yoffset;
	    newy2 = starty - yoffset2;
	}
	MoveTo( xd->dashStart_x, xd->dashStart_y );
	LineTo( newx, newy);
	numPixelsToDraw -= fullDashLength;
	xd->currentDash += 2;
	if(xd->currentDash >= xd->numDashes)
	    xd->currentDash = 0;
	xd->dashStart_x = newx2;
	xd->dashStart_y = newy2;
	notFirst = true;
    }
}



