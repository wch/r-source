/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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

#include "Defn.h"
#include "Graphics.h"
#include <stdio.h>

#include "wincons.h"
#include "wingdi.h"
#include "winuser.h"

extern void doGraphicsMenu(HWND, WPARAM, LPARAM);


        /********************************************************/
        /* This device driver has been documented so that it be */
        /* used as a template for new drivers                   */
        /* This is the Windows hack based on the X11 driver     */
        /********************************************************/

#define MM_PER_INCH     25.4                    /* mm -> inch conversion */

#define G_PI          3.141592653589793238462643383276

        /********************************************************/
        /* Each driver can have its own device-specic graphical */
        /* parameters and resources.  these should be wrapped   */
        /* in a structure (like the winDesc structure below)    */
        /* and attached to the overall device description via   */
        /* the dd->deviceSpecific pointer                       */
        /* NOTE that there are generic graphical parameters     */
        /* which must be set by the device driver, but are      */
        /* common to all device types (see Graphics.h)          */
        /* so go in the GPar structure rather than this device- */
        /* specific structure                                   */
        /********************************************************/

                        /* R Graphics Parameters */
                        /* local device copy so that we can detect */
                        /* when parameter changes */
typedef struct {
        double cex;                             /* Character expansion */
        double srt;                             /* String rotation */
        int lty;                                /* Line type */
        double lwd;
        int col;                                /* Color */
        int fg;                                 /* Foreground */
        int bg;                                 /* Background */
        int fontface;                           /* Typeface */
        int fontsize;                           /* Size in points */

                        /* Windows Driver Specific */
                        /* parameters with copy per win device */

        int resize;                             /* Window resized */
        HWND window;                            /* Graphics Window */
        COLORREF fgcolor;                       /* Foreground color */
        COLORREF bgcolor;                       /* Background color */
        RECT    clip;                           /* The clipping rectangle */
        RECT    graph;                          /* graphics plotting area */
        HPEN cpen;                              /* The current pen */
        HBRUSH cbrush;                          /* The current brush */
        int pcol;                               /* Pen color */
        int bcol;                               /* Brush color */
        int tcol;                               /* Text color */
        int fontindex;                          /* Which font */
        int usefixed;
        HDC     Dhdc;
} winDesc;

        /********************************************************/
        /* If there are resources that are shared by all devices*/
        /* of this type, you may wish to make them globals      */
        /* rather than including them in the device-specific    */
        /* parameters structure (especially if they are large !)*/
        /********************************************************/

                        /* Windows Driver Specific */
                        /* parameters with only one copy for all win devices */

static int numWinDevices = 0;
static int LocatorDone = 0;
static POINTS MousePoint;
HMENU RMenuGraph, RMenuGraphWin;



        /********************************************************/
        /* There must be an entry point for the device driver   */
        /* which will create device-specific resources,         */
        /* initialise the device-specific parameters structure  */
        /* and return whether the setup succeeded               */
        /* This is called by the graphics engine when the user  */
        /* creates a new device of this type                    */
        /********************************************************/

        /* Device Driver Entry Point */

int WinDeviceDriver(DevDesc*,  double, double, double);
extern void playDisplayList(DevDesc*);

        /********************************************************/
        /* There are a number of actions that every device      */
        /* driver is expected to perform (even if, in some      */
        /* cases it does nothing - just so long as it doesn't   */
        /* crash !).  this is how the graphics engine interacts */
        /* with each device. ecah action will be documented     */
        /* individually.                                        */
        /* hooks for these actions must be set up when the      */
        /* device is first created                              */
        /********************************************************/
        
        /* Device Driver Actions */

static void   Win_Activate(DevDesc *);
static void   Win_Circle(double, double, int, double, int, int, DevDesc*);
static void   Win_Clip(double, double, double, double, DevDesc*);
static void   Win_Close(DevDesc*);
static void   Win_Deactivate(DevDesc *);
static void   Win_Hold(DevDesc*);
static void   Win_Line(double, double, double, double, int, DevDesc*);
static int    Win_Locator(double*, double*, DevDesc*);
static void   Win_Mode(int);
static void   Win_NewPage(DevDesc*);
static int    Win_Open(DevDesc*, winDesc*, double, double);
static void   Win_Polygon(int, double*, double*, int, int, int, DevDesc*);
static void   Win_Polyline(int, double*, double*, int, DevDesc*);
static void   Win_Rect(double, double, double, double, int, int, int, DevDesc*);
static void   Win_Resize(DevDesc*);
static double Win_StrWidth(char*, DevDesc*);
static void   Win_Text(double, double, int, char*, double, double, double, 
                       DevDesc*);
static void   Win_MetricInfo(int, double*, double*, double*, DevDesc*);

        /********************************************************/
        /* end of list of required device driver actions        */
        /********************************************************/

        /* Support Routines */

static void SetColor(int, int, DevDesc*);
static void Win_RGSetFont(int, int, double, DevDesc*);
static void SetLineType(int, double, DevDesc*);

static int fontindex = 2;
static char *Rfontname[] = {"Courier New", "Times New", "Arial"};
static char *Rfacename[] = {"","Bold","Italic","Bold Italic","Symbol"};
static LOGFONT RGraphLF;

#define SMALLEST 8
#define LARGEST 24

/*
        face - whether the type face is normal or bold or ...

        size - the size of the font desired in points

        rot  - the desired rotation; this can be passed as a
                negative value to force a new font (ie. for the
                meta file)
*/

static void Win_RGSetFont(int face, int size, double rot, DevDesc *dd)
{
        HDC     Ghdc;
        HANDLE  cFont;
        char    fname[30];
        double  trot;
        winDesc *wd = (winDesc *) dd->deviceSpecific;


        face--;
        if( face < 0 || face > 4 ) face = 0;
        size = 2* size/2;
        if(size < SMALLEST) size = SMALLEST;
        if(size > LARGEST) size = LARGEST;
        trot = (double) RGraphLF.lfEscapement/10;

        if(size != wd->fontsize || face != wd->fontface || rot != trot) { 
                if( rot >= 0 )
                        RGraphLF.lfEscapement= (LONG) rot*10;
                if( wd->Dhdc == NULL )
                        Ghdc=GetDC(wd->window);
                else
                        Ghdc = wd->Dhdc;
                switch( face) {
                    case 0:
                        sprintf(fname,"%s", Rfontname[fontindex]);
                        break;
                    case 4:
                        sprintf(fname,"%s", Rfacename[face]);
                        break;
                    default:
                        sprintf(fname,"%s %s",Rfontname[fontindex],Rfacename[face]);
                }
                RGraphLF.lfHeight= -size*GetDeviceCaps(Ghdc,LOGPIXELSY)/72;
                strcpy(RGraphLF.lfFaceName, fname);
                cFont= CreateFontIndirect(&RGraphLF);

                if( cFont == NULL )
                        warning("could not select the requested font");
                DeleteObject(SelectObject(Ghdc, cFont));
                if( wd->Dhdc == NULL )
                        ReleaseDC(wd->window, Ghdc);
                wd->fontsize = size;
                wd->fontface = face+1;
        }
}


static void SetColor(unsigned col, int object, DevDesc *dd)
{
        HDC Ghdc;
        COLORREF fcol, fixColor();
        winDesc *wd = (winDesc *) dd->deviceSpecific;

        fcol = RGB(R_RED(col),R_GREEN(col), R_BLUE(col));
        if( wd->Dhdc == NULL )  
                Ghdc = GetDC(wd->window);
        else
                Ghdc = wd->Dhdc;
                
        switch(object) {
                case 1:         /*pen */
                        if(fcol != wd->pcol) {
                        wd->cpen = CreatePen(wd->lty, 1, fcol);
                        DeleteObject( SelectObject(Ghdc, wd->cpen));
                        wd->pcol = fcol;
                        }
                        break;
                case 2:         /*brush */
                        if(fcol != wd->bcol ) {
                                wd->cbrush = CreateSolidBrush(fcol);
                                DeleteObject( SelectObject(Ghdc, wd->cbrush ));
                                wd->bcol = fcol;
                        }
                        break;
                case 3:         /* text */
                        if( fcol != wd->tcol ) {
                        SetTextColor(Ghdc, fcol);
                        wd->tcol = fcol;
                        }
                break;
                default:
                        error("SetColor invalid object specified\n");
    }
    if ( wd->Dhdc == NULL )
        ReleaseDC(wd->window, Ghdc);
}

/*
 *      Some Notes on Line Textures
 *
 *      Line textures are stored as an array of 4-bit integers within
 *      a single 32-bit word.  These integers contain the lengths of
 *      lines to be drawn with the pen alternately down and then up.
 *      The device should try to arrange that these values are measured
 *      in points if possible, although pixels is ok on most displays.
 *
 *      If newlty contains a line texture description it is decoded
 *      as follows:
 *
 *              ndash = 0;
 *              for(i=0 ; i<8 && newlty&15 ; i++) {
 *                      dashlist[ndash++] = newlty&15;
 *                      newlty = newlty>>4;
 *              }
 *              dashlist[0] = length of pen-down segment
 *              dashlist[1] = length of pen-up segment
 *              etc
 *
 *      An integer containing a zero terminates the pattern.  Hence
 *      ndash in this code fragment gives the length of the texture
 *      description.  If a description contains an odd number of
 *      elements it is replicated to create a pattern with an
 *      even number of elements.  (If this is a pain, do something
 *      different its not crucial).
 */
 /*
        Windows line types are a mess. Without performing significant
        gyrations you cannot have line tipes with lines wider than 1
        Petzold provides some examples of how this can be done for the
        adventerous among you.
*/

static void SetLineType(int newlty, double nlwd, DevDesc *dd)
{
        HDC Ghdc;
        winDesc *wd = (winDesc *) dd->deviceSpecific;

        if( (int) nlwd > 1 ) {
                if (newlty != PS_SOLID && newlty != PS_NULL) { 
                        warning("can not have line types with wide pens\n");
                        newlty = 1;
                }
        }

        if(newlty != wd->lty || nlwd != wd->lwd ) {
                wd->cpen = CreatePen(newlty, nlwd, wd->pcol);
        if( wd->cpen == NULL )
                warning("could not create requested line type\n");
        else {
                wd->lty = newlty;
                wd->lwd = nlwd;
                if ( wd->Dhdc == NULL )
                        Ghdc = GetDC(wd->window);
                else
                        Ghdc = wd->Dhdc;
                        
                DeleteObject( SelectObject(Ghdc, wd->cpen) );
                if ( wd->Dhdc == NULL )
                        ReleaseDC(wd->window, Ghdc);
          }
      }
}


        /********************************************************/
        /* device_Open is not usually called directly by the    */
        /* graphics engine;  it is usually only called from     */
        /* the device-driver entry point.                       */
        /* this function should set up all of the device-       */
        /* specific resources for a new device                  */
        /* this function is given a new structure for device-   */
        /* specific information AND it must FREE the structure  */
        /* if anything goes seriously wrong                     */
        /* NOTE that it is perfectly acceptable for this        */
        /* function to set generic graphics parameters too      */
        /* (i.e., override the generic parameter settings       */
        /* which GInit sets up) all at the author's own risk    */
        /* of course :)                                         */
        /********************************************************/

static int Win_Open(DevDesc *dd, winDesc *wd, double width, double height)
{
                /* if have to bail out with "error" then must */
                /* free(dd) and free(xd) */

        MDICREATESTRUCT mdicreate;
        RECT r;
        HDC Chdc;
        int iw, ih;

        wd->bg =  dd->dp.bg  = R_RGB(255,255,255);
        wd->fg =  dd->dp.fg  = R_RGB(0,0,0);
        wd->col = dd->dp.col = wd->fg;

        GetClientRect(RClient, (LPRECT) &r);
        
        if (wd->Dhdc == NULL )
                Chdc = GetDC(RClient);
        else
                Chdc = wd->Dhdc;
        iw = ((int)width)*GetDeviceCaps(Chdc, LOGPIXELSX);
        ih = ((int) height) * GetDeviceCaps(Chdc, LOGPIXELSY);

        if ( wd->Dhdc == NULL )
                ReleaseDC(RClient, Chdc);

        if( iw > (r.right-r.left) || ih > (r.bottom-r.top) )
                iw = ih = min(r.right-r.left,r.bottom-r.top);
                
        mdicreate.szClass = RGraphClass;
        mdicreate.szTitle = "R Graphics";
        mdicreate.hOwner =(HINSTANCE) RInst;
        mdicreate.x = r.right-iw-5;
        mdicreate.y = r.top;
        mdicreate.cx = iw-5;
        mdicreate.cy = ih-5;
        mdicreate.style = 0;
        mdicreate.lParam=NULL;

        wd->window = (HWND) (UINT) SendMessage(RClient, WM_MDICREATE,0,
                (LONG) (LPMDICREATESTRUCT) &mdicreate);
        if( wd->window == NULL )
                return 0;

        SetWindowLong((HWND) wd->window, GWL_USERDATA, (LONG) dd);

        /* initialize the Graphics Logfont */
        wd->fontindex = 2; /* Arial */
        wd->fontface = 1;  /* Normal Text */

        /* a magic incantation to make angles rotate the same on printers 
                and the screen */
        RGraphLF.lfClipPrecision = CLIP_LH_ANGLES;
        
        Win_RGSetFont(wd->fontface, 8, -1, dd);

        ShowWindow(wd->window, SW_SHOW);
        GetClientRect(wd->window, (LPRECT) &(wd->graph));

        SetFocus(RConsoleFrame);

        wd->lty = -1;
        wd->lwd = -1;

        numWinDevices++;
        return 1;
}


        /********************************************************/
        /* device_StrWidth should return the width of the given */
        /* string in DEVICE units (GStrWidth is responsible for */
        /* converting from DEVICE to whatever units the user    */
        /* asked for                                            */
        /********************************************************/

static double Win_StrWidth(char *str, DevDesc *dd)
{
        HDC Ghdc;
        SIZE ext;
        int size;
        winDesc *wd = (winDesc *) dd->deviceSpecific;

        size = dd->gp.cex * dd->gp.ps +0.5;
        Win_RGSetFont(dd->gp.font, size, -1, dd);
        if ( wd->Dhdc == NULL )
                Ghdc=GetDC(wd->window);
        else
                Ghdc = wd->Dhdc;
        if( R_WinVersion >= 4.0 )
                GetTextExtentPoint32(Ghdc, str, strlen(str), &ext);
        else
                GetTextExtentPoint(Ghdc, str, strlen(str), &ext);

        if ( wd->Dhdc == NULL )
                ReleaseDC(wd->window, Ghdc);
        return (double) ext.cx;
}


        /********************************************************/
        /* device_MetricInfo should return height, depth, and   */
        /* width information for the given character in DEVICE  */
        /* units (GMetricInfo does the necessary conversions)   */
        /* This is used for formatting mathematical expressions */
        /********************************************************/
        
        /* Character Metric Information */
        /* Passing c == 0 gets font information */

static void Win_MetricInfo(int c, double* ascent, double* descent, 
                           double* width, DevDesc *dd)
{
        int first, last;
        int size = dd->gp.cex * dd->gp.ps + 0.5;
        winDesc *wd = (winDesc *) dd->deviceSpecific;
        TEXTMETRIC Itm;
        HDC Ghdc;
        SIZE sz;
        char xx[1];

        Win_RGSetFont(dd->gp.font, size, -1, dd);

        if( wd->Dhdc == NULL )
                Ghdc = GetDC(wd->window);
        else
                Ghdc = wd->Dhdc;

        GetTextMetrics(Ghdc, &Itm);
        
        first = Itm.tmFirstChar;
        last = Itm.tmLastChar;
        *ascent = Itm.tmAscent;
        *descent = Itm.tmDescent;

        if(c == 0) 
                *width = Itm.tmMaxCharWidth;
        else if(first <= c && c <= last) {
                xx[0]=c;
                GetTextExtentPoint32(Ghdc, xx, 1, &sz);
                *width = sz.cx-Itm.tmOverhang;
        }
        else {
                *ascent = 0;
                *descent = 0;
                *width = 0;
        }
}

        /********************************************************/
        /* device_Clip is given the left, right, bottom, and    */
        /* top of a rectangle (in DEVICE coordinates).  it      */
        /* should have the side-effect that subsequent output   */
        /* is clipped to the given rectangle                    */
        /********************************************************/

static void Win_Clip(double x0, double x1, double y0, double y1, DevDesc *dd)
{
        HDC Ghdc;
        winDesc *wd = (winDesc *) dd->deviceSpecific;

        if(x0 < x1) {
                wd->clip.left = (int) x0;
                wd->clip.right = (int) x1;
        }
        else {
                wd->clip.left = (int) x1;
                wd->clip.right = (int) x0;
        }
        if(y0 > y1) {
                wd->clip.bottom = (int) y0;
                wd->clip.top = (int) y1;
        }
        else {
                wd->clip.bottom = (int) y1;
                wd->clip.top = (int) y0;
        }

        if( wd->Dhdc == NULL )
                Ghdc = GetDC(wd->window);
        else
                Ghdc = wd->Dhdc;
                
        SelectClipRgn(Ghdc, NULL);
        IntersectClipRect(Ghdc, wd->clip.left, wd->clip.top, wd->clip.right,
                wd->clip.bottom);
                
        if ( wd->Dhdc == NULL )
                ReleaseDC(wd->window,Ghdc);
}

        
        /********************************************************/
        /* device_Resize is called whenever the device is       */
        /* resized.  the function must update the GPar          */
        /* parameters (left, right, bottom, and top) for the    */
        /* new device size                                      */
        /* this is not usually called directly by the graphics  */
        /* engine because the detection of device resizes       */
        /* (e.g., a window resize) are usually detected by      */
        /* device-specific code (see ProcessEvents in this file)*/
        /********************************************************/

static void Win_Resize(DevDesc *dd)
{
        HDC Ghdc;
        winDesc *wd = (winDesc *) dd->deviceSpecific;

        
        if (wd->resize) {
                dd->dp.left = dd->gp.left = 0.0;
                dd->dp.right = dd->gp.right =  wd->graph.right;
                dd->dp.bottom = dd->gp.bottom = wd->graph.bottom;
                dd->dp.top = dd->gp.top = 0.0;
                wd->resize = 0;
                Ghdc = GetDC(wd->window);
                SelectClipRgn(Ghdc, NULL);
                FillRect(Ghdc, &(wd->graph), GetStockObject(WHITE_BRUSH));
                ReleaseDC(wd->window, Ghdc);
                /*playDisplayList(dd);*/
        }
}

        /********************************************************/
        /* device_NewPage is called whenever a new plot requires*/
        /* a new page.  a new page might mean just clearing the */
        /* device (as in this case) or moving to a new page     */
        /* (e.g., postscript)                                   */
        /********************************************************/
        
static void Win_NewPage(DevDesc *dd)
{
        HDC Ghdc;
        winDesc *wd = (winDesc *) dd->deviceSpecific;

        if(wd->bg != dd->dp.bg) {
                wd->bg = dd->dp.bg;
/*
                wd->bgcolor.red   = R_RED(wd->bg)   * 257;
                wd->bgcolor.green = R_GREEN(wd->bg) * 257;
                wd->bgcolor.blue  = R_BLUE(wd->bg)  * 257;
*/
        }
        if (wd->Dhdc == NULL )
                Ghdc = GetDC(wd->window);
        else
                Ghdc = wd->Dhdc;
                
        SelectClipRgn(Ghdc, NULL);
        FillRect(Ghdc, &(wd->graph), GetStockObject(WHITE_BRUSH));

        if( wd->Dhdc == NULL )
                ReleaseDC(wd->window, Ghdc);
}

        /********************************************************/
        /* device_Close is called when the device is killed     */
        /* this function is responsible for destroying any      */
        /* device-specific resources that were created in       */
        /* device_Open and for FREEing the device-specific      */
        /* parameters structure                                 */
        /********************************************************/

static void Win_Close(DevDesc *dd)
{
        HDC Ghdc;
        winDesc *wd = (winDesc *) dd->deviceSpecific;

        Ghdc = GetDC(wd->window);
        DeleteObject( SelectObject(Ghdc, GetStockObject(BLACK_PEN)) );
        DeleteObject( SelectObject(Ghdc, GetStockObject(WHITE_BRUSH)) );
        ReleaseDC(wd->window, Ghdc);

        SendMessage(RClient, WM_MDIDESTROY, (WPARAM) (HWND) wd->window, 0);
        numWinDevices--;
        SetFocus(RConsoleFrame);
}

        /********************************************************/
        /* device_Activate is called when a device becomes the  */
        /* active device.  In this case it is used to change the*/
        /* title of a window to indicate the active status of   */
        /* the device to the user.  Not all device types will   */
        /* do anything                                          */
        /********************************************************/

static unsigned char title[11] = "R Graphics";

static void Win_Activate(DevDesc *dd)
{
        char t[50];
        char num[3];
        winDesc *wd = (winDesc *) dd->deviceSpecific;

        strcpy(t, title);
        strcat(t, ": Device ");
        sprintf(num, "%i", deviceNumber(dd)+1);
        strcat(t, num);
        strcat(t, "(ACTIVE)");
        SetWindowText(wd->window, t);
}

        /********************************************************/
        /* device_Deactivate is called when a device becomes    */
        /* inactive.  in this case it is used to change the     */
        /* title of a window to indicate the inactive status of */
        /* the device to the user.  not all device types will   */
        /* do anything                                          */
        /********************************************************/

static void Win_Deactivate(DevDesc *dd)
{
        char t[50];
        char num[3];
        winDesc *wd = (winDesc *) dd->deviceSpecific;

        strcpy(t, title);
        strcat(t, ": Device ");
        sprintf(num, "%i", deviceNumber(dd)+1);
        strcat(t, num);
        strcat(t, " (inactive)");
        SetWindowText(wd->window, t);
}
        
        /********************************************************/
        /* device_Rect should have the side-effect that a       */
        /* rectangle is drawn with the given locations for its  */
        /* opposite corners.  the border of the rectangle       */
        /* should be in the given "fg" colour and the rectangle */
        /* should be filled with the given "bg" colour          */
        /* if "fg" is NA_INTEGER then no border should be drawn */
        /* if "bg" is NA_INTEGER then the rectangle should not  */
        /* be filled                                            */
        /* the locations are in an arbitrary coordinate system  */
        /* and this function is responsible for converting the  */
        /* locations to DEVICE coordinates using GConvert       */
        /********************************************************/

static void Recthelper(HDC ihdc, int x0, int y0, int x1, int y1, int bg, int fg)
{
    HBRUSH tBrush;
    HPEN tPen;

        if( bg == NA_INTEGER ) {
                tBrush = SelectObject(ihdc, GetStockObject(NULL_BRUSH));
                if( fg == NA_INTEGER )
                        tPen = SelectObject(ihdc, GetStockObject(NULL_PEN));
        }

        Rectangle(ihdc, x0, y0, x1, y1);
        if( bg == NA_INTEGER ) {
                SelectObject(ihdc, tBrush);
                if( fg == NA_INTEGER )
                        SelectObject(ihdc, tPen);
        }
}

/* Draw a Filled Rectangle */
static void Win_Rect(double x0, double y0, double x1, double y1, 
                        int coords, int bg, int fg, DevDesc *dd)
{
        double tmp;
        HDC Ghdc;
        winDesc *wd = (winDesc *) dd->deviceSpecific;

        GConvert(&x0, &y0, coords, DEVICE, dd);
        GConvert(&x1, &y1, coords, DEVICE, dd);

        if( x0 > x1 ) {
                tmp = x0;
                x0 = x1;
                x1 = tmp;
        }
        if( y0 > y1 ) {
            tmp = y0;
            y0 = y1;
            y1 = tmp;
        }

        if( bg != NA_INTEGER ) {
                SetColor(bg, 2, dd);
                if( fg == NA_INTEGER )
                        SetColor(bg, 1, dd);
        }
        if( fg != NA_INTEGER )
                SetColor(fg, 1, dd);

        if( wd->Dhdc == NULL )
                Ghdc = GetDC(wd->window);
        else
                Ghdc = wd->Dhdc;
                
        Recthelper(Ghdc, (int) x0, (int) y0, (int) x1, (int) y1, bg, fg);
        if( wd->Dhdc == NULL )
                ReleaseDC(wd->window, Ghdc);

}


        /********************************************************/
        /* device_Circle should have the side-effect that a     */
        /* circle is drawn, centred at the given location, with */
        /* the given radius.  the border of the circle should be*/
        /* drawn in the given "col", and the circle should be   */
        /* filled with the given "border" colour.               */
        /* if "col" is NA_INTEGER then no border should be drawn*/
        /* if "border" is NA_INTEGER then the circle should not */
        /* be filled                                            */
        /* the location is in arbitrary coordinates and the     */
        /* function is responsible for converting this to       */
        /* DEVICE coordinates.  the radius is given in DEVICE   */
        /* coordinates                                          */
        /********************************************************/

static void RCirclehelper(HDC ihdc, int ix, int iy, int ir, int bg, int fg)
{
    HBRUSH tBrush;
    HPEN tPen;

    if( bg == NA_INTEGER ) {
        tBrush = SelectObject(ihdc, GetStockObject(NULL_BRUSH));
        if( fg == NA_INTEGER )
                tPen = SelectObject(ihdc, GetStockObject(NULL_PEN));
    }

    Ellipse(ihdc,ix-ir, iy+ir, ix+ir, iy-ir);

    if( bg == NA_INTEGER ) {
        SelectObject(ihdc, tBrush);
        if( fg == NA_INTEGER )
                SelectObject(ihdc,tPen);
    }
}


static void Win_Circle(double x, double y, int coords,
                        double r, int col, int border, DevDesc *dd)
{
    int ir, ix, iy;
    HDC Ghdc;
    winDesc *wd = (winDesc *) dd->deviceSpecific;
  
    ir= floor(r+0.5);
    GConvert(&x, &y, coords, DEVICE, dd);
    ix=(int) x;
    iy=(int) y;

    if( col != NA_INTEGER )
        SetColor(col, 1, dd);
    if( border != NA_INTEGER ) {
        SetColor(border, 2, dd);
        if( col == NA_INTEGER )
                SetColor(border, 1, dd);
    }

    if( wd->Dhdc == NULL ) {
        Ghdc=GetDC(wd->window);        
        RCirclehelper(Ghdc, ix, iy, ir, wd->bg, wd->fg);
        ReleaseDC(wd->window, Ghdc);
    }
    else
        RCirclehelper(wd->Dhdc, ix, iy, ir, wd->bg, wd->fg);

}

        /********************************************************/
        /* device_Line should have the side-effect that a single*/
        /* line is drawn (from x1,y1 to x2,y2)                  */
        /* x1, y1, x2, and y2 are in arbitrary coordinates and  */
        /* the function is responsible for converting them to   */
        /* DEVICE coordinates using GConvert                    */
        /********************************************************/

static void Win_Line(double x1, double y1, double x2, double y2,
                        int coords, DevDesc *dd)
{
        int xx1, yy1, xx2, yy2;
        HDC Ghdc;
        POINT lp;
        winDesc *wd = (winDesc *) dd->deviceSpecific;

        GConvert(&x1, &y1, coords, DEVICE, dd);
        GConvert(&x2, &y2, coords, DEVICE, dd);

        xx1 = (int) x1;
        yy1 = (int) y1;
        xx2 = (int) x2;
        yy2 = (int) y2;

        if( wd->Dhdc == NULL )
                Ghdc=GetDC(wd->window);
        else
                Ghdc = wd->Dhdc;

        MoveToEx(Ghdc, xx1, yy1, &lp);

        SetColor(dd->gp.col, 1, dd);
        SetLineType(dd->gp.lty, dd->gp.lwd, dd);

        LineTo(Ghdc, xx2, yy2);
        if (wd->Dhdc == NULL )
                ReleaseDC(wd->window, Ghdc);
}

        /********************************************************/
        /* device_Polyline should have the side-effect that a   */
        /* series of line segments are drawn using the given x  */
        /* and y values                                         */
        /* the x and y values are in arbitrary coordinates and  */
        /* the function is responsible for converting them to   */
        /* DEVICE coordinates using GConvert                    */
        /********************************************************/

static void Win_Polyline(int n, double *x, double *y, int coords, DevDesc *dd)
{
        double devx, devy;
        int i;
        POINT lp;
        HDC Ghdc;
        winDesc *wd = (winDesc *) dd->deviceSpecific;

        SetColor(dd->gp.col, 1, dd);
        SetLineType(dd->gp.lty, dd->gp.lwd, dd);

        if( wd->Dhdc == NULL )
                Ghdc = GetDC( wd->window);
        else
                Ghdc = wd->Dhdc;

        devx = x[0];  devy = y[0];
        GConvert(&devx, &devy, coords, DEVICE, dd);
        MoveToEx(Ghdc, (int) devx, (int) devy, &lp);

        for(i=1 ; i<n ; i++) {
                devx = x[i];  devy = y[i];
                GConvert(&devx, &devy, coords, DEVICE, dd);
                LineTo(Ghdc, (int) devx, (int) devy);
                MoveToEx(Ghdc, (int) devx, (int) devy, &lp);
        }
        if ( wd->Dhdc == NULL )
                ReleaseDC(wd->window, Ghdc);
}

        /********************************************************/
        /* device_Polygon should have the side-effect that a    */
        /* polygon is drawn using the given x and y values      */
        /* the polygon border should be drawn in the "fg"       */
        /* colour and filled with the "bg" colour               */
        /* if "fg" is NA_INTEGER don't draw the border          */
        /* if "bg" is NA_INTEGER don't fill the polygon         */
        /* the x and y values are in arbitrary coordinates and  */
        /* the function is responsible for converting them to   */
        /* DEVICE coordinates using GConvert                    */
        /********************************************************/
static void RPolyhelper(HDC ihdc, POINT *pt, int n, int bg, int fg)
{
    HBRUSH tBrush;
    HPEN tPen;

    if( bg == NA_INTEGER ) {
        tBrush = SelectObject(ihdc, GetStockObject(NULL_BRUSH));
        if( fg == NA_INTEGER )
                tPen = SelectObject(ihdc, GetStockObject(NULL_PEN));
    }
    Polygon(ihdc, pt, n);

    if( bg == NA_INTEGER ) {
        SelectObject(ihdc, tBrush);
        if( fg == NA_INTEGER )
                SelectObject(ihdc, tPen);
    }
}

static void Win_Polygon(int n, double *x, double *y, int coords, int bg, int fg, DevDesc *dd)
{
        int i;
        POINT *pt;
        double devx, devy;
        HDC Ghdc;
        winDesc *wd = (winDesc *) dd->deviceSpecific;

        pt = (POINT*) C_alloc(n,sizeof(POINT));

        for(i=0; i<n ; i++) {
            devx = x[i]; devy = y[i];
            GConvert(&devx, &devy, coords, DEVICE, dd);
            pt[i].x = (int) (devx);
            pt[i].y = (int) (devy);
        }
        if( fg != NA_INTEGER )
                SetColor(fg, 1, dd);
        if( bg != NA_INTEGER ) {
                SetColor(bg, 2, dd);
                if( fg == NA_INTEGER )
                        SetColor(bg, 1, dd);
        }

        if( wd->Dhdc == NULL ) {
                Ghdc = GetDC(wd->window);
                RPolyhelper(Ghdc, pt, n, bg, fg);
                ReleaseDC(wd->window, Ghdc);
        }
        else
                RPolyhelper(wd->Dhdc, pt, n, bg, fg);
        
        C_free((char *) pt);
}


        /********************************************************/
        /* device_Text should have the side-effect that the     */
        /* given text is drawn at the given location            */
        /* the text should be justified according to "xc" and   */
        /* "yc" (0 = left, 0.5 = centre, 1 = right)             */
        /* and rotated according to rot (degrees)               */
        /* the location is in an arbitrary coordinate system    */
        /* and this function is responsible for converting the  */
        /* location to DEVICE coordinates using GConvert        */
        /********************************************************/

/* Rotated Text
   xc and yc indicate text justification
   0 means left justified,
   1 means right justified
   0.5 means centered

  Under Win32 only TrueType Fonts can be rotated. This means
  that the user will have to select one of these for us to have
  rotated text.

  For some reason windows default is to have alignment by the top
  left of the character string.

*/

static void Win_Text(double x, double y, int coords, char *str, 
                        double xc, double yc, double rot, DevDesc *dd)
{
        HDC Ghdc;
        int fsize, nstr;
        double rotrad, xl, yl;
        SIZE lpSize;
        winDesc *wd = (winDesc *) dd->deviceSpecific;


        fsize = dd->gp.cex * dd->gp.ps +0.5;
        Win_RGSetFont(dd->gp.font, fsize, rot, dd);
        SetColor(dd->gp.col, 3, dd);
        nstr = strlen(str);

        GConvert(&x, &y, coords, DEVICE, dd);

        if( wd->Dhdc == NULL )
                Ghdc=GetDC(wd->window);
        else
                Ghdc = wd->Dhdc;
                
        if( xc != 0 || yc != 1 ) {
                if( R_WinVersion >= 4.0 )
                        GetTextExtentPoint32(Ghdc,str,nstr,&lpSize);
                else
                        GetTextExtentPoint(Ghdc,str,nstr,&lpSize);
                xl = (double) lpSize.cx;
                yl = (double) lpSize.cy;
                rotrad=rot*(2*G_PI)/360;
                x += -xc * xl * cos(rotrad) - (1-yc) * yl * sin(rotrad);
                y -= - xc * xl * sin(rotrad) + (1 - yc) * yl * cos(rotrad);

        }
        TextOut(Ghdc,(int) x,(int) y,str, nstr);
        if( wd->Dhdc == NULL )
                ReleaseDC(wd->window, Ghdc);
}


        /********************************************************/
        /* device_Locator should return the location of the next*/
        /* mouse click (in DEVICE coordinates;  GLocator is     */
        /* responsible for any conversions)                     */
        /* not all devices will do anything (e.g., postscript)  */
        /********************************************************/

static int Win_Locator(double *x, double *y, DevDesc *dd)
{
        winDesc *wd = (winDesc *) dd->deviceSpecific;
        
        LocatorDone=0;
        SendMessage(RClient, WM_MDIACTIVATE, (WPARAM)(HWND) wd->window, 0L);
        while (LocatorDone == 0 )
                EventLoop();
        if( LocatorDone == 1 ) {
                *x=(double) MousePoint.x;
                *y=(double) MousePoint.y;
                LocatorDone=-1;
                return 1;
        }
        else {
                LocatorDone=-1;
                return 0;
        }
}


        /********************************************************/
        /* device_Mode is called whenever the graphics engine   */
        /* starts drawing (mode=1) or stops drawing (mode=1)    */
        /* the device is not required to do anything            */
        /********************************************************/

/* Set Graphics mode - not needed for Windows */
static void Win_Mode(int mode)
{
        return;
}


        /********************************************************/
        /* i don't know what this is for and i can't find it    */
        /* being used anywhere, but i'm loath to kill it in     */
        /* case i'm missing something important                 */
        /********************************************************/
        
/* Hold the Picture Onscreen - not needed for Windows */
static void Win_Hold(DevDesc *dd)
{
}

static HDC Win_SetMetaDC(HDC Ihdc, RECT graphicsRect)
{
        RECT rr;
        float iMMPerPelX, iMMPerPelY;
        float i,j,k,l;

        i = (float) GetDeviceCaps(Ihdc, HORZSIZE);
        j = (float) GetDeviceCaps(Ihdc, HORZRES);
        k = (float) GetDeviceCaps(Ihdc, VERTSIZE);
        l = (float) GetDeviceCaps(Ihdc, VERTRES);
        iMMPerPelX=(i*100.0)/j; 
        iMMPerPelY=(k*100.0)/l; 
        rr.left= floor(graphicsRect.left* iMMPerPelX);
        rr.top = floor(graphicsRect.top* iMMPerPelY);
        rr.right = floor(graphicsRect.right* iMMPerPelX);
        rr.bottom = floor(graphicsRect.bottom* iMMPerPelY);
        return( CreateEnhMetaFile(Ihdc, NULL, &rr, "R metafile"));
}


static void CopyGraphToScrap(DevDesc *dd)
{
        HDC Ghdc, hdcMem, metaHdc;
        HBITMAP tbm=NULL;
        HANDLE ehmf;
        winDesc *wd = (winDesc *) dd->deviceSpecific;

        Ghdc=GetDC(wd->window);

        tbm=CreateCompatibleBitmap(Ghdc, wd->graph.right,wd->graph.bottom);
        if ( tbm == NULL ) {
            MessageBox( RFrame, "Cannot create bitmap","R Application",
                MB_ICONEXCLAMATION | MB_OK);
            ReleaseDC(wd->window, Ghdc);                
            return;
        }                          
        hdcMem = CreateCompatibleDC(Ghdc);
        SelectObject(hdcMem, wd->cpen);
        SelectObject(hdcMem, wd->cbrush);
        SetTextColor(hdcMem, wd->tcol);
        
        ReleaseDC(wd->window, Ghdc);        
        SelectObject(hdcMem, tbm);
        wd->Dhdc = hdcMem;
        playDisplayList(dd);

        if ( R_WinVersion >= 4.0 ) {
                metaHdc = Win_SetMetaDC(Ghdc, wd->graph);
                SelectObject(metaHdc, wd->cpen);
                SelectObject(metaHdc, wd->cbrush);
                SetTextColor(metaHdc, wd->tcol);
                wd->Dhdc = metaHdc;
                playDisplayList(dd);
                ehmf = CloseEnhMetaFile(metaHdc);
        }
 
        if( OpenClipboard(RFrame) ) {
            EmptyClipboard();
            if( tbm != NULL )
                SetClipboardData(CF_BITMAP, tbm);
            if( ehmf != NULL ) {
                SetClipboardData(CF_ENHMETAFILE, CopyEnhMetaFile(ehmf, NULL));
                DeleteEnhMetaFile(ehmf);
            }
            CloseClipboard();
        }
        else
            MessageBox( RFrame, "Cannot open the clipboard","R Application",
                MB_ICONEXCLAMATION | MB_OK);
                
        wd->Dhdc = NULL;        
        DeleteDC(hdcMem);
}

        /********************************************************/
        /* the device-driver entry point is given a device      */
        /* description structure that it must set up.  this     */
        /* involves several important jobs ...                  */
        /* (1) it must ALLOCATE a new device-specific parameters*/
        /* structure and FREE that structure if anything goes   */
        /* wrong (i.e., it won't report a successful setup to   */
        /* the graphics engine (the graphics engine is NOT      */
        /* responsible for allocating or freeing device-specific*/
        /* resources or parameters)                             */
        /* (2) it must initialise the device-specific resources */
        /* and parameters (mostly done by calling device_Open)  */
        /* (3) it must initialise the generic graphical         */
        /* parameters that are not initialised by GInit (because*/
        /* only the device knows what values they should have)  */
        /* see Graphics.h for the official list of these        */
        /* (4) it may reset generic graphics parameters that    */
        /* have already been initialised by GInit (although you */
        /* should know what you are doing if you do this)       */
        /* (5) it must attach the device-specific parameters    */
        /* structure to the device description structure        */
        /* e.g., dd->deviceSpecfic = (void *) xd;               */
        /* (6) it must FREE the overall device description if   */
        /* it wants to bail out to the top-level                */
        /* the graphics engine is responsible for allocating    */
        /* the device description and freeing it in most cases  */
        /* but if the device driver freaks out it needs to do   */
        /* the clean-up itself                                  */
        /********************************************************/
        

int WinDeviceDriver(DevDesc *dd,double width, double height, double pointsize)
{
        /* if need to bail out with some sort of "error" then */
        /* must free(dd) */

        HDC Ghdc;
        TEXTMETRIC Itm;
        int ps;
        winDesc *wd;

        /* allocate new device description */
        if (!(wd = (winDesc *) malloc(sizeof(winDesc)))) 
                return 0;


        /* from here on, if need to bail out with "error", must also */
        /* free(wd) */

        /*  Font will load at first use  */

        ps = pointsize;
        if(ps < 6 || ps > 24) ps = 10;
        ps = 2*(ps/2);

        wd->fontface = -1;
        wd->fontsize = -1;
        wd->pcol = 0;
        wd->bcol = R_RGB(255,255,255);
        wd->tcol = 0;
		wd->Dhdc = NULL;

        dd->dp.font = 1;
        dd->dp.ps = ps;
        dd->deviceSpecific = (void *) wd;


        /*  Start the Device Driver and Hardcopy.  */

        if (!Win_Open(dd, wd, width, height)) {
                free(wd);
                return 0;
        }

        /*  Set up Data Structures  */

        dd->dp.open = Win_Open;
        dd->dp.close = Win_Close;
        dd->dp.activate = Win_Activate;
        dd->dp.deactivate = Win_Deactivate;
        dd->dp.resize = Win_Resize;
        dd->dp.newPage = Win_NewPage;
        dd->dp.clip = Win_Clip;
        dd->dp.strWidth = Win_StrWidth;
        dd->dp.text = Win_Text;
        dd->dp.rect = Win_Rect;
        dd->dp.circle = Win_Circle;
        dd->dp.line = Win_Line;
        dd->dp.polyline = Win_Polyline;
        dd->dp.polygon = Win_Polygon;
        dd->dp.locator = Win_Locator;
        dd->dp.mode = Win_Mode;
        dd->dp.hold = Win_Hold;
        dd->dp.metricInfo = Win_MetricInfo;

        /* set graphics parameters that must be set by device driver */
        /* Window Dimensions in Pixels */

        dd->dp.left = 0;                        /* left */
        dd->dp.right = wd->graph.right;          /* right */
        dd->dp.bottom = wd->graph.bottom;       /* bottom */
        dd->dp.top = 0;                         /* top */

        Ghdc = GetDC(wd->window);

        GetTextMetrics(Ghdc, &Itm);

        /* Nominal Character Sizes in Pixels */

        dd->dp.cra[0] = Itm.tmMaxCharWidth;
        dd->dp.cra[1] = Itm.tmHeight;

        /* Character Addressing Offsets */
        /* These are used to plot a single plotting character */
        /* so that it is exactly over the plotting point */

        dd->dp.xCharOffset = 0.5;
        dd->dp.yCharOffset = 0.5;
        dd->dp.yLineBias = 0.1;

        /* Inches per raster unit */

        dd->dp.ipr[0] = 1/(double)GetDeviceCaps(Ghdc, LOGPIXELSX);
        dd->dp.ipr[1] = 1/(double)GetDeviceCaps(Ghdc, LOGPIXELSY);

        /* Device capabilities */

        dd->dp.canResizePlot = 1;
        dd->dp.canChangeFont = 1;
        dd->dp.canRotateText = 1;
        dd->dp.canResizeText = 1;
        dd->dp.canClip = 1; 

        /* initialise win device description (most of the work */
        /* has been done in Win_Open) */

        wd->cex = 1.0;
        wd->srt = 0.0;
        wd->lty = 0;
        wd->resize = 0;
        
        wd->Dhdc = NULL;

        dd->displayListOn = 1;

        SetBkMode(Ghdc, TRANSPARENT);
        /* remove the stock objects from the display context */
        wd->cpen = CreatePen(PS_SOLID, 1, 0);
        wd->cbrush = CreateSolidBrush(RGB(255,255,255));
        wd->lty = PS_SOLID;
        SelectObject(Ghdc, wd->cpen);
        SelectObject(Ghdc, wd->cbrush);
        SetTextColor(Ghdc, wd->tcol);

        ReleaseDC(wd->window, Ghdc);

        return 1;
}

/*
 *  there cannot be any references to RGraphWnd in this function
 *  as it is called before RGraphWnd is assigned
*/
LRESULT FAR PASCAL GraphWndProc(HWND hWnd, UINT message, WPARAM wParam,
        LPARAM lParam)
{
        PAINTSTRUCT ps;
        DevDesc *dd;
        winDesc *wd;

        dd = (DevDesc *) GetWindowLong(hWnd, GWL_USERDATA);

        switch(message) {
             case WM_CREATE:
                 break;
             case WM_SIZE:
                 if( wParam != SIZE_MINIMIZED  ) {                         
                         if( dd != NULL ) { 
                                wd = (winDesc *) dd->deviceSpecific;
                                if( HIWORD(lParam) != wd->graph.bottom
                                     || LOWORD(lParam) != wd->graph.right ) {
                                         wd->graph.bottom = HIWORD(lParam);
                                         wd->graph.right  = LOWORD(lParam);
                                         wd->resize = 1;
                                }
                         }
                 }
                 break;
             case WM_LBUTTONDOWN:
                 if(LocatorDone==0) {
                    MousePoint = MAKEPOINTS(lParam);
                    LocatorDone=1;
                    return 0;
                  }
                  break;
             case WM_RBUTTONDOWN:
                 if(LocatorDone==0) {
                       LocatorDone=2;
                       return 0;
                 }
                 break;
              case WM_COMMAND:    
                  doGraphicsMenu(hWnd, wParam, lParam);
                  break;
              case WM_PAINT:
                   if ( dd != NULL ) {
                        Win_Resize(dd);
                        playDisplayList(dd);
                   }
                   BeginPaint(hWnd, &ps);
                   SelectClipRgn(ps.hdc, NULL);
                   EndPaint(hWnd, &ps);
                   return 0;
              case WM_SETFOCUS:
                   SetFocus(hWnd);
                   break;
              case WM_MDIACTIVATE:
                   if((HWND) lParam == hWnd ) {
                       SendMessage(RClient, WM_MDISETMENU,
                             (WPARAM) RMenuGraph, (LPARAM) RMenuGraphWin);
                       DrawMenuBar(RFrame);
                       InvalidateRect(hWnd,&(wd->graph), TRUE);

                   }
                   return(0);
              case WM_CLOSE:
                   ShowWindow(hWnd, SW_MINIMIZE);
                   KillDevice(dd);
                   Win_Close(dd);
                   return(0);
              case WM_DESTROY:
                   return(0);
      }
  return(DefMDIChildProc(hWnd, message, wParam, lParam));
}

void doGraphicsMenu(HWND GWnd, WPARAM wParam, LPARAM lParam)
{
        DevDesc *dd;
        HBITMAP tbm=NULL;
        HANDLE ehmf;
        winDesc *wd;
        HDC Ghdc, metaHdc, hdcMem;
        BITMAPINFOHEADER bi, *lpbi;
        BITMAP bmp;
        HANDLE hDIB;
        int i;
        WORD cClrBits;
        LPBYTE lpBits;
               
        dd = (DevDesc *) GetWindowLong(GWnd, GWL_USERDATA);
        wd = (winDesc *) dd->deviceSpecific;
        switch (wParam) {
                case RRR_SETUP:
                        SysBeep();
                        break;
                case RRR_PRINT:
                        Ghdc=GetDC(wd->window);
                        if( R_WinVersion >= 4.0 ) {
                             metaHdc = Win_SetMetaDC(Ghdc, wd->graph);
                             SelectObject(metaHdc, wd->cpen);
                             SelectObject(metaHdc, wd->cbrush);
                             SetTextColor(metaHdc, wd->tcol);
                             wd->Dhdc = metaHdc;
                             playDisplayList(dd);
                             ehmf = CloseEnhMetaFile(metaHdc);
                             wd->Dhdc = NULL;
                             RPrintGraph(RConsoleFrame, CopyEnhMetaFile(ehmf,NULL));
                             DeleteEnhMetaFile(ehmf);
                        }
                        else {  /* one of the reasons I really hate Windows */
                             tbm=CreateCompatibleBitmap(Ghdc, wd->graph.right,wd->graph.bottom);
                             if ( tbm == NULL ) {
                                     MessageBox( RFrame, "Cannot create bitmap","R Application",
                                             MB_ICONEXCLAMATION | MB_OK);
                                     ReleaseDC(wd->window, Ghdc);                
                                     return;
                             }                          
                             hdcMem = CreateCompatibleDC(Ghdc);
                             SelectObject(hdcMem, wd->cpen);
                             SelectObject(hdcMem, wd->cbrush);
                             SetTextColor(hdcMem, wd->tcol);        
                             ReleaseDC(wd->window, Ghdc);        
                             SelectObject(hdcMem, tbm);
                             wd->Dhdc = hdcMem;
                             playDisplayList(dd);
                             GetObject(tbm, sizeof(BITMAP), &bmp);
                             bi.biSize = sizeof(BITMAPINFOHEADER);
                             bi.biWidth = bmp.bmWidth;
                             bi.biHeight = bmp.bmHeight;
                             bi.biPlanes = bmp.bmPlanes;
                             bi.biBitCount = bmp.bmBitsPixel;
                             bi.biCompression = BI_RGB;
                             bi.biClrUsed = 0;
                             bi.biXPelsPerMeter = 0;
                             bi.biYPelsPerMeter = 0;
                             bi.biClrImportant = 0;

                             cClrBits = (WORD) (bmp.bmPlanes * bmp.bmBitsPixel);
                             if( cClrBits == 1)
                                cClrBits = 1;
                             else if( cClrBits <= 4)
                                cClrBits = 4;
                             else if( cClrBits <= 8)
                                cClrBits = 8;
                             else if( cClrBits <=16)
                                cClrBits = 16;
                             else if (cClrBits <= 24)
                                cClrBits = 24;
                             else
                                cClrBits = 32;

                             bi.biSizeImage = (bmp.bmWidth +7)/8 * bmp.bmHeight * cClrBits;
                             lpBits = (LPBYTE) GlobalAlloc(GMEM_FIXED, bi.biSizeImage);

                             hDIB = GlobalAlloc(GHND, sizeof(BITMAPINFOHEADER)+
                                        16*sizeof(RGBQUAD));
                             lpbi =(BITMAPINFOHEADER *) GlobalLock(hDIB);
                             *lpbi = bi;
                             Ghdc = GetDC(wd->window);
                             i = GetDIBits(Ghdc, tbm, 0, bmp.bmHeight, lpBits,
                                    (LPBITMAPINFO) lpbi, DIB_RGB_COLORS);
                             ReleaseDC(wd->window, Ghdc);

                             RPrintBitMap((LPBITMAPINFO) lpbi, (LPBYTE) lpBits);
                             GlobalFree(hDIB);
                             GlobalFree(lpBits);
                        }
                        break;
                case RRR_COPY:
                        CopyGraphToScrap(dd);
                        break;
                }
}

