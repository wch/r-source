/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2002  Robert Gentleman, Ross Ihaka
 *			      and the R Development Core Team
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

/* See system.txt for a description of functions
 */
#ifndef __RAQUA_H__
#define __RAQUA_H__

#ifndef max
#define max(a, b) (((a)>(b))?(a):(b))
#endif
#ifndef min
#define min(a, b) (((a)<(b))?(a):(b))
#endif


#define kRAppSignature '0FFF'
#define kRVersionInfoID 132
#define kRCopyrightID 133
#define kRAquaAuthorsID 134
#define kRAquaThanksToID 135
#define kRImageID 136



/* RPreferences structure */
typedef struct
{
	long		prefsTypeVers;
	int			RFontFace;
	int			RFontSize;
	int			RTabSize;
	RGBColor	FGInputColor;
	RGBColor	BGInputColor;
	RGBColor	FGOutputColor;
	RGBColor	BGOutputColor;
	char		DeviceFontName[255];
	int			DevicePointSize;
	double		DeviceWidth;
	double		DeviceHeight;
	int			AntiAlias;
	int			AutoRefresh;
	int			OverrideRDefaults;
	char		CRANmirror[255];
	char		BIOCmirror[255];
	int			GlobalPackages;
	int			GrabStdout;
	int			GrabStderr;
	int			SaveConsolePos;
	int			QuartzPos;
	int			SetConsoleWidthOnResize;
	char		WorkingDirectory[255];
}  RAquaPrefs, *RAquaPrefsPointer, **RAquaPrefsHandle;


#ifdef __AQUA_PREFS__
const char *RFontFaces[] = {"Andale Mono", "Courier", "Courier New", "Monaco", "VT100" };
const int  RFontSizes[] = {8, 9, 10, 11, 12, 14, 18, 24, 36, 48, 64, 72};
#else
extern char *RFontFaces[];
extern int  RFontSizes[];
#endif

#define kQuartzTopRight		1
#define kQuartzBottomRight  2
#define kQuartzBottomLeft   3
#define kQuartzTopLeft		4
#define kQuartzCenter		5


#define kOnScreen 	0
#define kOnFilePDF 	1
#define kOnFilePICT	2
#define kOnClipboard 	3
#define kOnPrinter	4


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
    CGContextRef context;     /* This is the context used by Quartz for OnScreen drawings */
    CGContextRef auxcontext;  /* Additional context used for: cliboard, printer, file     */
    double	xscale;
    double	yscale;
    int		where;
	int		QuartzPos;		 /* Window Pos: TopRight=1, BottomRight, BottomLeft, TopLeft=4, Center = 5 */
}
QuartzDesc;

#endif /* __RAQUA_H__ */
