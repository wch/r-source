 /*  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995  Robert Gentleman and Ross Ihaka
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
#include <string.h>
#include <math.h>
#include "WINcons.h"
#include "Graphics.h"
#include "wingdi.h"
#include "winuser.h"

#define STRICT


/* OUTSTANDING ISSUES                   */

/* 1) line types for fat lines via LineDDA       */

/* Only True Type Fonts can be rotated so we need to choose one
    -GetRasterizerCaps indicates whether TrueType is available on
                the machine
    -EnumFontFamilies can be called to enumerate available fonts
        and you can check these to see if they are TrueType
    -CHOOSEFONT (the interactive dialog) has a flag that allows
        only TrueType fonts to be enumerated (should we allow
        only TrueType??)

        MetaFiles:
                -a means of enabling print (the easy way is bitmaps
                but I doubt all printers will support this).
                -currently every graphics command must be executed twice; once to
                plot to the screen and once to store it in the meta file (be careful
                to only use commands that are allowable in metafiles...)
                -clipping is a real problem with metafiles; the clipping info
                is in device coords (so when you go to print on a laser printer
                you need to transform all the clips)

*/


#define G_PI          3.141592653589793238462643383276

static LOGFONT RGraphLF;
static HANDLE pFont;
HWND    RGraphWnd;
/* static HANDLE RGMhmf=NULL; */
static HANDLE RGMhmf = NULL;
static HDC RGMhdc=NULL;
static HDC RGBhdc=NULL;         /*memory device context for the bitmap */
static HBITMAP RGBitMap=NULL;   /* graphics bitmap */

HMENU RMenuGraph, RMenuGraphWin;

static POINTS MousePoint;
static int LocatorDone;

LRESULT CALLBACK _export GraphWndProc(HWND,UINT,WPARAM,LPARAM);

/* Basic Win Objects */
RECT                    graphicsRect;
int                     fontw, fonth;

static int Win_Open(void);
static void CopyGraphToScrap(void);
static void Win_NewPlot(void);
static void Win_Close(void);
static void doGraphicsMenu(HWND, WPARAM, LPARAM);
static void Win_SetMetaDC(HDC);

static void SetLineType(unsigned);

/* R Graphics Parameters */
static double cex = 1.0;
static int lty = -1;
static COLORREF RPenCol;
static COLORREF RBrushCol;
static COLORREF RTextCol;
static HPEN RCurrentPen;
static HBRUSH RCurrentBrush;
static int RCurrentLty;
static int fontface = -1;
static int fontsize = -1;


/* Windows Specific Parameters */
static int fontindex = 2;
static char *Rfontname[] = {"Courier New", "Times New", "Arial"};
static char *Rfacename[] = {"","Bold","Italic","Bold Italic"};


#define SMALLEST 8
#define LARGEST 24

/*
        face - whether the type face is normal or bold or ...
 
        size - the size of the font desired in points

        rot  - the desired rotation; this can be passed as a
                negative value to force a new font (ie. for the
                meta file)
*/

static void Win_RGSetFont(int face, int size, double rot)
{
        HDC     Ihdc;
        HANDLE  cFont, sFont;
        char    fname[30];
        double  trot;


        face--;  
        if( face < 0 || face > 3 ) face = 0;
        size = 2* size/2;
        if(size < SMALLEST) size = SMALLEST;
        if(size > LARGEST) size = LARGEST;
        trot = (double) RGraphLF.lfEscapement/10;
                
        if(size != fontsize || face != fontface || rot != trot) { /*we create a new font */
                if( rot >= 0 )
                        RGraphLF.lfEscapement= (LONG) rot*10;
                        
                Ihdc=GetDC(RGraphWnd);
                if( face )
                        sprintf(fname,"%s %s",Rfontname[fontindex],Rfacename[face]);
                else
                        sprintf(fname,"%s", Rfontname[fontindex]);
                        
                RGraphLF.lfHeight= -size*GetDeviceCaps(Ihdc,LOGPIXELSY)/72;
                strcpy(RGraphLF.lfFaceName, fname);
                cFont= CreateFontIndirect(&RGraphLF);
                
                if( cFont == NULL )
                        warning("could not select the requested font");
                sFont=SelectObject(Ihdc, cFont);
                if( RGMhdc != NULL )
                        SelectObject(RGMhdc, cFont);
                if( RGBhdc != NULL )
                        SelectObject(RGBhdc, cFont);
                if( pFont == sFont )
                        DeleteObject(sFont);
                pFont=cFont;
                ReleaseDC(RGraphWnd, Ihdc);
                fontsize = size;
                fontface = face+1;
        }
}


static void SetLineType(int newlty)
{
    HDC Ihdc;

      if(newlty != RCurrentLty) {
          RCurrentPen = CreatePen(newlty, 1, RPenCol);
          if( RCurrentPen == NULL )
                warning("could not create requested line type\n");
          else {
                RCurrentLty=newlty;
                if( RGMhdc != NULL )
                        SelectObject(RGMhdc, RCurrentPen);
                if( RGBhdc != NULL )
                        SelectObject(RGBhdc, RCurrentPen);
                Ihdc = GetDC(RGraphWnd);
                DeleteObject( SelectObject(Ihdc, RCurrentPen) );
                ReleaseDC(RGraphWnd, Ihdc);
          }
      }           
}

static void SetColor(unsigned col, int object)
{
    HDC Ihdc;
    COLORREF fcol, fixColor();
    
    fcol = RGB(R_RED(col),R_GREEN(col), R_BLUE(col));   /* to account for the different bit patterns in R and Windoze */
    Ihdc = GetDC(RGraphWnd);
    
    switch(object) {
        case 1:         /*pen */
                if(fcol != RPenCol) {
                        RCurrentPen = CreatePen(RCurrentLty,1,fcol);
                        if( RGMhdc != NULL )
                                SelectObject(RGMhdc, RCurrentPen);
                        if( RGBhdc != NULL )
                                SelectObject(RGBhdc, RCurrentPen);
                        DeleteObject( SelectObject(Ihdc, RCurrentPen));                
                        RPenCol = fcol;
                }
                break;
        case 2:         /*brush */
                if(fcol != RBrushCol ) {
                        RCurrentBrush = CreateSolidBrush(fcol);
                        if( RGMhdc != NULL )
                                SelectObject(RGMhdc, RCurrentBrush);
                        if( RGBhdc != NULL )
                                SelectObject(RGBhdc, RCurrentBrush);
                        DeleteObject( SelectObject(Ihdc, RCurrentBrush));
                        RBrushCol = fcol;
                }
                break;
        case 3:         /* text */
              if( fcol != RTextCol ) {
                if( RGMhdc != NULL )
                        SetTextColor(RGMhdc, fcol);
                if( RGBhdc != NULL )
                        SetTextColor(RGBhdc, fcol);
                SetTextColor(Ihdc, fcol);
                RTextCol = fcol;
              }
              break;
        default:
                error("SetColor invalid object specified\n");
    }
    ReleaseDC(RGraphWnd, Ihdc);
}


static double Win_StrWidth(char *str)
{
    HDC Ihdc;
    SIZE ext;
    int size;

    size = GP->cex * GP->ps +0.5;
    Win_RGSetFont(GP->font, size, -1);
    Ihdc=GetDC(RGraphWnd);
    if( R_WinVersion >= 4.0 )
        GetTextExtentPoint32(Ihdc, str, strlen(str), &ext);
    else
        GetTextExtentPoint(Ihdc, str, strlen(str), &ext);
    ReleaseDC(RGraphWnd, Ihdc);
    return (double) ext.cx;
}

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

static void Win_Circle(double x, double y, double r, int bg, int fg)
{
    int ir, ix, iy;
    HDC thdc;
   
    ir=(int) (r+0.5);
    ix=(int) x;
    iy=(int) y;

    if( fg != NA_INTEGER )
        SetColor(fg, 1);
    if( bg != NA_INTEGER ) {
        SetColor(bg, 2);
        if( fg == NA_INTEGER )
                SetColor(bg, 1);
    }
                
    thdc=GetDC(RGraphWnd);
    RCirclehelper(thdc, ix, iy, ir, bg, fg);
    ReleaseDC(RGraphWnd, thdc);
        
    if( RGMhdc != NULL )
          RCirclehelper(RGMhdc, ix, iy, ir, bg, fg);
    if( RGBhdc != NULL )
          RCirclehelper(RGBhdc, ix, iy, ir, bg, fg);        

}
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
           
static void Win_Polygon(int n, double *x, double *y, int bg, int fg)
{
        char *vmax, *vmaxget();
        int i;
        POINT *pt;
        HDC thdc;

        vmax = vmaxget();
        if((pt=(POINT*)R_alloc(n,sizeof(POINT)))==NULL)
                error("out of memory while drawing ploygon\n");
        for(i=0; i<n ; i++) {
            pt[i].x = (int) (x[i]);
            pt[i].y = (int) (y[i]);
        }
        if( fg != NA_INTEGER )
                SetColor(fg,1);
        if( bg != NA_INTEGER ) {
                SetColor(bg,2);
                if( fg == NA_INTEGER )
                        SetColor(bg, 1);
        }

        thdc = GetDC(RGraphWnd);
        RPolyhelper(thdc, pt, n, bg, fg);
        ReleaseDC(RGraphWnd, thdc);
        
        if( RGMhdc != NULL )
                RPolyhelper(RGMhdc, pt, n, bg, fg);
        if( RGBhdc != NULL )
                RPolyhelper(RGBhdc, pt, n, bg, fg);

        vmaxset(vmax);
}

static void Win_SavePlot(char *name)
{
    /* print the plot to the named file??? */
}

static void Win_PrintPlot(char *name)
{
    /* ???? */
}



    
static void Win_StartPath()
{
    SetColor(GP->col,1);
    SetLineType(GP->lty);
}

static void Win_EndPath()
{
}

int CALLBACK PaintMetafile(HDC hdc, HANDLETABLE* lpHTable, ENHMETARECORD* lpEMFR, int nObj,
LPVOID lpData)
{
    double x0, x1, y0, y1;
    char *x;
    
    if( lpEMFR->iType == EMR_GDICOMMENT ) {
        x = (char *) &(lpEMFR->dParm[1]);
        if( x[0]=='R' && x[1]=='_' && x[2]=='C') {
                sscanf(x+3,"%lf %lf %lf %lf",&x0,&x1,&y0,&y1);
                Win_Clip(x0,x1,y0,y1);
        }
    }
    else
        PlayEnhMetaFileRecord(hdc, lpHTable, lpEMFR, nObj);
    return(1);
}

/*
 *  there cannot be any references to RGraphWnd in this function
 *  as it is called before RGraphWnd is assigned
*/
LRESULT FAR PASCAL GraphWndProc(HWND hWnd, UINT message, WPARAM wParam,
        LPARAM lParam)
{
        PAINTSTRUCT ps;
        HDC thdc;
        RECT r1;

        switch(message) {
             case WM_CREATE:
                 break;
             case WM_SIZE:
                 if( wParam != SIZE_MINIMIZED  ) { /* we must start a new plot because the clipping
                     cannot be scaled properly */
                         GetClientRect(hWnd, &r1);
                         if( r1.bottom != graphicsRect.bottom
                            || r1.right != graphicsRect.right ) {
                             graphicsRect = r1;
                             if( IsWindow(RGraphWnd) )
                                Win_Resize();
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
              case WM_COMMAND:    /* probably a problem with returning if doMenu does soemthing */
                  doGraphicsMenu(hWnd, wParam, lParam);
                  break;
              case WM_PAINT:
                   BeginPaint(hWnd, &ps);
                   SelectClipRgn(ps.hdc, NULL);                  
                   EndPaint(hWnd, &ps);
                   /* RgraphWnd should be ok since RGBhdc gets created from it */    
                   if ( RGBhdc != NULL ) {
                       thdc = GetDC(RGraphWnd);
                       BitBlt(thdc, 0, 0, graphicsRect.right,graphicsRect.bottom,
                        RGBhdc, 0, 0, SRCCOPY);
                       ReleaseDC(RGraphWnd, thdc);
                   }
                   return 0;
              case WM_SETFOCUS:
                   SetFocus(hWnd);
                   break;
              case WM_MDIACTIVATE:
                   if((HWND) lParam == hWnd ) {
                       SendMessage(RClient, WM_MDISETMENU,
                             (WPARAM) RMenuGraph, (LPARAM) RMenuGraphWin);
                       DrawMenuBar(RFrame);
                       InvalidateRect(hWnd,&graphicsRect, TRUE);
                                                        
                   }
                   return(0);
              case WM_CLOSE:
                   ShowWindow(hWnd, SW_MINIMIZE);
                   Win_Close();
                   return(0);
              case WM_DESTROY:
                   return(0);
      }
  return(DefMDIChildProc(hWnd, message, wParam, lParam));
}

void doGraphicsMenu(HWND GWnd, WPARAM wParam, LPARAM lParam)
{
        HDC hdc;
        BITMAPINFOHEADER bi, *lpbi;
        BITMAP bmp;
        HANDLE hDIB;
        int i;
        WORD cClrBits;
        LPBYTE lpBits;
        
        
        switch (wParam) {
                case RRR_SETUP:
                        SysBeep();
                        /*
                        SetupPrinter(); */
                        break;
                case RRR_PRINT:
                        if ( RGMhdc != NULL ) {
                                if( RGMhmf != NULL )
                                        if( R_WinVersion >= 4.0 )
                                                DeleteEnhMetaFile(RGMhmf);
                                        else
                                                DeleteMetaFile(RGMhmf);
                                RGMhmf=CloseEnhMetaFile(RGMhdc);
                        }
                        if( RGMhmf != NULL && R_WinVersion >= 4.0 ) {
                                RPrintGraph(RConsoleFrame, CopyEnhMetaFile(RGMhmf,NULL));
                                hdc = GetDC(RGraphWnd);
                                Win_SetMetaDC(hdc);
                                ReleaseDC(RGraphWnd, hdc);
                                PlayEnhMetaFile(RGMhdc, RGMhmf, &graphicsRect);
                                DeleteEnhMetaFile(RGMhmf);
                                RGMhmf=NULL;
                        }
                        else {  /* one of the reasons I really hate Windows */
                            GetObject(RGBitMap, sizeof(BITMAP), &bmp);
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
                            hdc = GetDC(RGraphWnd);
                            i = GetDIBits(hdc, RGBitMap, 0, bmp.bmHeight, lpBits,
                                    (LPBITMAPINFO) lpbi, DIB_RGB_COLORS);
                            ReleaseDC(RGraphWnd, hdc);                            
                            RPrintBitMap((LPBITMAPINFO) lpbi, (LPBYTE) lpBits);
                            GlobalFree(hDIB);
                            GlobalFree(lpBits);                          
                        }
                        break;
                case RRR_COPY:          
                        CopyGraphToScrap();
                        break;
                }
}

static void Win_SetMetaDC(HDC Ihdc)
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
        RGMhdc = CreateEnhMetaFile(Ihdc, NULL, &rr, "R metafile");
}

/* Last Point Coordinates */
static double xlast = 0;
static double ylast = 0;

/* Open a Graphics Window and set Graphics State */
static int Win_Open()
{
        MDICREATESTRUCT mdicreate;
        HDC Nhdc;
        RECT r;
        int i;


        GetClientRect(RClient, (LPRECT) &r);
        i= min(r.right-r.left,r.bottom-r.top);
        mdicreate.szClass = RGraphClass;
        mdicreate.szTitle = "R Graphics";
        mdicreate.hOwner =(HINSTANCE) RInst;
        mdicreate.x = r.right-i-5;
        mdicreate.y = r.top;
        mdicreate.cx = i-5;
        mdicreate.cy = i-5;
        mdicreate.style = 0;
        mdicreate.lParam=NULL;
        RGraphWnd = (HWND) (UINT) SendMessage(RClient, WM_MDICREATE,0,
                (LONG) (LPMDICREATESTRUCT) &mdicreate);
        if( RGraphWnd == NULL )
                return 0;
        
        pFont=NULL;

        /* initialize the Graphics Logfont */
        fontindex = 2; /* Arial */
        fontface = 1;  /* Normal Text */
        
        /* a magic incantation to make angles rotate the same on printers and the screen */
        RGraphLF.lfClipPrecision = CLIP_LH_ANGLES;
 
        Win_RGSetFont(fontface, 8, -1);

        ShowWindow(RGraphWnd, SW_SHOW);

        /* set up the bitmap and get ready to use it for the graphics */
                
        Nhdc = GetDC(RGraphWnd);
        RGBhdc = CreateCompatibleDC(Nhdc);
        SetMapMode(RGBhdc, GetMapMode(Nhdc));
        RGBitMap= CreateCompatibleBitmap(Nhdc,graphicsRect.right-graphicsRect.left,
                graphicsRect.bottom-graphicsRect.top);
        if( RGBitMap == NULL ) {
                MessageBox( RFrame, "couldn't alloc bitmap","R Graphics",
                MB_ICONEXCLAMATION | MB_OK);
                return 0;
        }

        SelectObject(RGBhdc, RGBitMap);
        FillRect(RGBhdc, &graphicsRect, GetStockObject(WHITE_BRUSH));
        BitBlt(Nhdc,0,0,graphicsRect.right,graphicsRect.bottom,RGBhdc,0,0,SRCCOPY);

        ReleaseDC(RGraphWnd, Nhdc);
        SetFocus(RConsoleFrame);
        DevInit = 1;
        return 1;
}

/* Current Clipping Rectangle */
static double Clipxl, Clipxr, Clipyb, Clipyt;

/* Set the Clipping Rectangle */
static void Win_Clip(double x0, double x1, double y0, double y1)
{ 
    HDC devHdc;

       if(x0 < x1) {
                Clipxl = x0;
                Clipxr = x1;
        }
        else {
                Clipxl = x1;
                Clipxr = x0;
        }
        if(y0 > y1) {
                Clipyb = y0;
                Clipyt = y1;
        }
        else {
                Clipyb = y1;
                Clipyt = y0;
        }

     devHdc = GetDC(RGraphWnd);
     SelectClipRgn(devHdc, NULL);
     IntersectClipRect(devHdc, (int) Clipxl, (int) Clipyt,(int) Clipxr,(int) Clipyb);
     ReleaseDC(RGraphWnd,devHdc);
     
     if( RGMhdc != NULL ) {
         ExtSelectClipRgn(RGMhdc, NULL, RGN_COPY);
         IntersectClipRect(RGMhdc, (int) Clipxl, (int) Clipyt,(int) Clipxr,(int) Clipyb);
     }

     if( RGBhdc != NULL ) {
         SelectClipRgn(RGBhdc, NULL);
         IntersectClipRect(RGBhdc, (int) Clipxl, (int) Clipyt,(int) Clipxr,(int) Clipyb);
     }
}


/* Actions on Window Resize */
static void Win_Resize()
{
    HDC Nhdc;

    if( DP->right != graphicsRect.right || DP->bottom != graphicsRect.bottom ) {
    
        DP->right = graphicsRect.right;
        DP->bottom = graphicsRect.bottom;

        Nhdc = GetDC(RGraphWnd);
        if( RGBitMap != NULL ) 
                DeleteObject(RGBitMap);
        RGBitMap= CreateCompatibleBitmap(Nhdc,graphicsRect.right-graphicsRect.left,
                graphicsRect.bottom-graphicsRect.top);
        if( RGBitMap == NULL ) {
            MessageBox( RFrame, "couldn't alloc bitmap","R Graphics",
            MB_ICONEXCLAMATION | MB_OK);
            Win_Close();
        }

        SelectObject(RGBhdc, RGBitMap);
        FillRect(RGBhdc, &graphicsRect, GetStockObject(WHITE_BRUSH));
        BitBlt(Nhdc,0,0,graphicsRect.right,graphicsRect.bottom,RGBhdc,0,0,SRCCOPY);

        ReleaseDC(RGraphWnd, Nhdc);
    }                    
}

/* Begin a New Plot; under Win32 just clear the graphics rectangle. */

void Win_NewPlot()
{
        HDC Nhdc;
        int i;
        DWORD err;

        Nhdc=GetDC(RGraphWnd);
        SelectClipRgn(Nhdc, NULL);
        SelectClipRgn(RGBhdc, NULL);
        
        i = FillRect(RGBhdc, &graphicsRect, GetStockObject(WHITE_BRUSH));
        err = GetLastError();
        i = BitBlt(Nhdc,0,0,graphicsRect.right,graphicsRect.bottom,RGBhdc,0,0,SRCCOPY);

        if( R_WinVersion >= 4.0 ) {
                if( RGMhdc != NULL )
                        DeleteEnhMetaFile(CloseEnhMetaFile(RGMhdc));
                if( RGMhmf != NULL )
                        DeleteEnhMetaFile(RGMhmf);
                Win_SetMetaDC(Nhdc);
                FillRect(RGMhdc, &graphicsRect, GetStockObject(WHITE_BRUSH));
        }
        
        ReleaseDC(RGraphWnd, Nhdc);
}

/* Close the Graphics Window */

static void Win_Close()
{
    HDC hdc;

        /* delete the pen and brush we created */
       
        hdc = GetDC(RGraphWnd);
        DeleteObject( SelectObject(hdc, GetStockObject(BLACK_PEN)) );
        DeleteObject( SelectObject(hdc, GetStockObject(WHITE_BRUSH)) );
        ReleaseDC(RGraphWnd, hdc);
        
        DevInit=0;
        SendMessage(RClient, WM_MDIDESTROY, (WPARAM) (HWND) RGraphWnd, 0);
        RGraphWnd = NULL;
        if( R_WinVersion > 4.0 ) {
                if( RGMhdc != NULL )
                   CloseEnhMetaFile(RGMhdc);
                RGraphWnd=NULL;
                RGMhmf=NULL;
        }
        if( RGBhdc != NULL ) {
                DeleteDC(RGBhdc);
                RGBhdc = NULL;
        }
        if( RGBitMap != NULL ) {
            DeleteObject(RGBitMap);
            RGBitMap = NULL;
        }
        SetFocus(RConsoleFrame);
}

/* MoveTo */
static void Win_MoveTo(double x, double y)
{
        HDC devHdc;
        POINT lp;
        
        xlast = x;
        ylast = y;
        devHdc=GetDC(RGraphWnd);
        MoveToEx(devHdc,(int) x,(int) y,&lp);
        if( RGMhdc != NULL )
                MoveToEx(RGMhdc,(int) x,(int) y,&lp);
        if( RGBhdc != NULL )
                MoveToEx(RGBhdc, (int) x, (int) y, &lp);
                
        ReleaseDC(RGraphWnd,devHdc);
}

/* Dot */
/* LineTo */
static void Win_LineTo(double x, double y)
{
        HDC devHdc;

        devHdc=GetDC(RGraphWnd);
        LineTo(devHdc,(int) x, (int) y);
        if( RGMhdc != NULL )
          LineTo(RGMhdc,(int) x, (int) y);
        if( RGBhdc != NULL )
          LineTo(RGBhdc, (int) x, (int) y);
        ReleaseDC(RGraphWnd,devHdc);
        xlast = x;
        ylast = y;
}

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
static void Win_Rect(double x0, double y0, double x1, double y1, int bg, int fg)
{
        double tmp;
        HDC thdc;
        
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
                SetColor(bg,2);
                if( fg == NA_INTEGER )
                        SetColor(bg, 1);
        }
        if( fg != NA_INTEGER )
                SetColor(fg, 1);
                
        thdc = GetDC(RGraphWnd);
        Recthelper(thdc, (int) x0, (int) y0, (int) x1, (int) y1, bg, fg);
        ReleaseDC(RGraphWnd, thdc);
                
        if (RGMhdc != NULL )
                Recthelper(RGMhdc, (int) x0, (int) y0, (int) x1, (int) y1, bg, fg);
        if (RGBhdc != NULL )
                Recthelper(RGBhdc, (int) x0, (int) y0, (int) x1, (int) y1, bg, fg);
}

/* Rotated Text   
        xlast and ylast are meant to be the lower left corner of the box
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

static void Win_Text(double x, double y, char *str, double xc, double yc, double rot)
{
        HDC devHdc;
        int fsize, nstr;
        double rotrad, xl, yl;
        SIZE lpSize;

 
        fsize = GP->cex * GP->ps +0.5;
        Win_RGSetFont(GP->font, fsize, rot);
        SetColor(GP->col, 3);
        nstr = strlen(str);
        
        xlast = x;
        ylast = y;

        devHdc=GetDC(RGraphWnd);
        if( xc != 0 || yc != 1 ) {
                if( R_WinVersion >= 4.0 )
                        GetTextExtentPoint32(devHdc,str,nstr,&lpSize);
                else
                        GetTextExtentPoint(devHdc,str,nstr,&lpSize);
                xl = (double) lpSize.cx;
                yl = (double) lpSize.cy;
                rotrad=rot*(2*G_PI)/360;
                x += -xc * xl * cos(rotrad) - (1-yc) * yl * sin(rotrad);
                y -= - xc * xl * sin(rotrad) + (1 - yc) * yl * cos(rotrad);        
        }
        TextOut(devHdc,(int) x,(int) y,str, nstr);
        if( RGMhdc != NULL )
                TextOut(RGMhdc,(int) x,(int) y,str,nstr);
        if( RGBhdc != NULL )
                TextOut(RGBhdc,(int) x,(int) y,str,nstr);
        ReleaseDC(RGraphWnd,devHdc);
}

/* Return the Pointer Location */
static int Win_Locator(double *x, double *y)
{
        LocatorDone=0;
        SendMessage(RClient, WM_MDIACTIVATE, (WPARAM)(HWND) RGraphWnd,0L);
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


/* Set the Graphics Mode */
static void Win_Mode(int mode)
{
        return ;
/*
        if( mode == 1 || mode == 2)
                if( RGMhmf != NULL )  { // we're adding to the current picture
                        Win_SetMetaDC();
                        PlayMetaFile(RGMhdc, RGMhmf);
                        DeleteMetaFile(RGMhmf);
                        RGMhmf=NULL;
                }
        if( mode == 0 )
                if( RGMhdc != NULL )  {
                        if( RGMhmf != NULL )
                                DeleteMetaFile(RGMhmf);
                        RGMhmf=CloseMetaFile(RGMhdc);
                        RGMhdc=NULL;
                }
*/
}

/* Keep the Graphics Window in Front */
static void Win_Hold(void)
{
}

static void CopyGraphToScrap(void)
{
        HDC hdc, hdcMem;
        HBITMAP tbm=NULL;

        hdc = GetDC(RGraphWnd);
        if( RGBhdc != NULL ) {
            tbm=CreateCompatibleBitmap(hdc, graphicsRect.right,graphicsRect.bottom);
            hdcMem = CreateCompatibleDC(hdc);
            SelectObject(hdcMem, tbm);
            BitBlt(hdcMem,0,0,graphicsRect.right,graphicsRect.bottom,hdc, 0, 0, SRCCOPY);
        }
        
        if( RGMhdc != NULL ) {
            if( RGMhmf != NULL )
                DeleteEnhMetaFile(RGMhmf);
            RGMhmf = CloseEnhMetaFile(RGMhdc);
        }  

        if( OpenClipboard(RFrame) ) {
            EmptyClipboard();
            if( RGMhmf != NULL )
                SetClipboardData(CF_ENHMETAFILE, CopyEnhMetaFile(RGMhmf, NULL));
            if( tbm != NULL )
                SetClipboardData(CF_BITMAP, tbm);
            CloseClipboard();
        }
        else
            MessageBox( RFrame, "Cannot open the clipboard","R Application",
                MB_ICONEXCLAMATION | MB_OK);
                

        if( RGMhmf != NULL )  { 
                PlayEnhMetaFile(RGMhdc, RGMhmf, &graphicsRect);
                DeleteEnhMetaFile(RGMhmf);
                RGMhmf=NULL;  
        }
}

/* Device Driver */
int WinDeviceDriver()
{
        HDC DDhdc;
        TEXTMETRIC      Itm;

        if( RGraphWnd != NULL )
                return 1;
        RPenCol = 0;
        RBrushCol = R_RGB(255,255,255);
        RTextCol = 0;
        
        DevInit = 0;
        if( ! Win_Open() ) return 0;

        DevOpen = Win_Open;
        DevClose = Win_Close;
        DevResize = Win_Resize;
        DevNewPlot = Win_NewPlot;
        DevClip = Win_Clip;
        DevStartPath = Win_StartPath;
        DevEndPath = Win_EndPath;
        DevMoveTo = Win_MoveTo;
        DevLineTo = Win_LineTo;
        DevStrWidth = Win_StrWidth;
        DevText = Win_Text;
        DevCircle = Win_Circle;
        DevRect = Win_Rect;
        DevPolygon = Win_Polygon;
        DevLocator = Win_Locator;
        DevMode = Win_Mode;
        DevHold = Win_Hold;
        DevSavePlot = Win_SavePlot;
        DevPrintPlot = Win_PrintPlot;

        /* window dimensions in pixels */
        
        GP->left = 0;
        GP->right = graphicsRect.right - graphicsRect.left;
        GP->bottom = graphicsRect.bottom - graphicsRect.top;
        GP->top = 0;

        DDhdc=GetDC(RGraphWnd);
        
        /* character size in raster */
        
        GetTextMetrics(DDhdc, &Itm);
        GP->cra[0] = Itm.tmMaxCharWidth;
        GP->cra[1] = Itm.tmHeight;
        
        /*character addressing offsets; just guesses */
        /* these are used to centre when plotting a single character */
          
        GP->xCharOffset = 0.5;
        GP->yCharOffset = 0.5;
        GP->yLineBias = 0.1;

        /* inches per raster */
        
        GP->ipr[0] = 1/(double)GetDeviceCaps(DDhdc, LOGPIXELSX);
        GP->ipr[1] = 1/(double)GetDeviceCaps(DDhdc, LOGPIXELSY);

        GP->canResizePlot = 1;
        GP->canChangeFont = 1;
        GP->canRotateText = 1;
        GP->canResizeText = 1;
        GP->canClip = 1;

        LocatorDone=-1;

        DevInit = 1;
        cex = 1.0;
        lty = 0;
        xlast = 250;
        ylast = 250;

        SetBkMode(DDhdc, TRANSPARENT);
        
        /* remove the  stock objects from the display context so we
         * don't accidentally delete one of them
         */
        RCurrentPen = CreatePen(PS_SOLID, 1, 0);
        RCurrentBrush = CreateSolidBrush(RGB(255,255,255));
        RCurrentLty = PS_SOLID;
        SelectObject(DDhdc, RCurrentPen);
        SelectObject(DDhdc, RCurrentBrush);
        SetTextColor(DDhdc, RTextCol);
        if( RGMhdc != NULL ) {
                SelectObject(RGMhdc, RCurrentPen);
                SelectObject(RGMhdc, RCurrentBrush);
                SetTextColor(RGMhdc, RTextCol);
        }
        
        ReleaseDC(RGraphWnd, DDhdc);

        return 1;
}
